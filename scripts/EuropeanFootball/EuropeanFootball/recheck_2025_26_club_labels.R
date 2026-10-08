# Offline comparison labels only. Does not add aliases or import fixtures.
recheck_target_labels <- function() {
  suppressPackageStartupMessages({library(data.table);library(stringi)})
  base<-"EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26"
  out<-file.path(base,"club_label_recheck");dir.create(out,recursive=TRUE,showWarnings=FALSE)
  w<-fread(file.path(base,"score_date_matching/wikipedia_results.csv"),encoding="UTF-8")
  d<-fread(file.path(base,"cached_source_review/dated_fixture_candidates.csv"),encoding="UTF-8")
  d<-d[!grepl("Gozo",CompetitionLabel,ignore.case=TRUE)&Country %in% w$Country]
  aliases<-fread("EuropeanFootball/pipeline_data/Reference/team_aliases.csv",encoding="UTF-8")
  source("scripts/EuropeanFootball/club_identity_resolution.R",local=TRUE)
  lm<-setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  rm<-setNames(character(),character())
  clean<-function(x,country) {
    x<-tolower(stri_trans_general(resolve_alias_chain(x,country,lm,rm),"Latin-ASCII"))
    trimws(gsub(" +"," ",gsub("[^a-z0-9]"," ",x)))
  }
  tokens<-function(x) {
    words<-strsplit(x," ",fixed=TRUE)[[1]]
    setdiff(words,c("fc","cf","afc","fk","kf","sk","sc","ac","club","clube","football","futebol","futbol",
      "esporte","esportiva","sociedade","sporting","regatas","de","da","do","e","the"))
  }
  proposed<-list();review<-list();wiki_out<-list();summaries<-list()
  countries<-sort(unique(w$Country))
  cat("[1/2] Testing unique club labels against season rosters and score evidence...\n")
  for(i in seq_along(countries)) {
    country<-countries[i];dc<-copy(d[Country==country]);wc<-copy(w[Country==country])
    sn<-unique(c(dc$Home,dc$Away));wn<-unique(c(wc$Home,wc$Away))
    sk<-clean(sn,rep(country,length(sn)));wk<-clean(wn,rep(country,length(wn)))
    edges<-list()
    for(a in seq_along(sn))for(b in seq_along(wn)) {
      st<-tokens(sk[a]);wt<-tokens(wk[b])
      exact<-sk[a]==wk[b]
      contained<-length(st)>0L&&length(wt)>0L&&
        nchar(paste(wt,collapse=""))>=4L&&nchar(paste(st,collapse=""))>=4L&&
        (all(wt %in% st)||all(st %in% wt))
      if(exact||contained)edges[[length(edges)+1L]]<-data.table(SourceName=sn[a],WikiName=wn[b],
        Method=if(exact)"EXISTING_ALIAS_OR_NORMALISED_EXACT"else "UNIQUE_ROSTER_WORD_CONTAINMENT")
    }
    edges<-if(length(edges))rbindlist(edges)else data.table(SourceName=character(),WikiName=character(),Method=character())
    # Neither side may have multiple candidates; no automatic tie-breaking.
    edges<-edges[!SourceName %in% edges[,.N,by=SourceName][N>1L,SourceName]&
      !WikiName %in% edges[,.N,by=WikiName][N>1L,WikiName]]
    if(!nrow(edges)) {
      dc[,Decision:="CLUB_LABEL_REVIEW"]
      review[[length(review)+1L]]<-dc
      summaries[[length(summaries)+1L]]<-dc[,.N,by=.(Country,Decision)]
      next
    }
    map<-setNames(edges$WikiName,edges$SourceName)
    dc[,`:=`(MatchedHome=unname(map[Home]),MatchedAway=unname(map[Away]))]
    wc[,K:=paste(Home,Away,Score,sep="|")]
    dc[,K:=paste(MatchedHome,MatchedAway,Score,sep="|")]
    good<-dc[!is.na(MatchedHome)&!is.na(MatchedAway)&K %chin% wc$K]
    app<-rbind(good[,.(SourceName=Home,Opponent=MatchedAway,MatchID)],good[,.(SourceName=Away,Opponent=MatchedHome,MatchID)])
    evidence<-app[,.(ScoreAgreements=uniqueN(MatchID),DistinctOpponents=uniqueN(Opponent)),by=SourceName]
    edges<-merge(edges,evidence,by="SourceName",all.x=TRUE)
    edges[,Decision:=fifelse(Method=="EXISTING_ALIAS_OR_NORMALISED_EXACT"|(!is.na(ScoreAgreements)&ScoreAgreements>=4L&DistinctOpponents>=2L),
      "COMPARISON_LABEL_SUPPORTED","INSUFFICIENT_FIXTURE_EVIDENCE")]
    edges[,Country:=country];proposed[[length(proposed)+1L]]<-edges
    accepted<-edges[Decision=="COMPARISON_LABEL_SUPPORTED",SourceName]
    dc[!Home %in% accepted,MatchedHome:=NA_character_];dc[!Away %in% accepted,MatchedAway:=NA_character_]
    dc[,K:=paste(MatchedHome,MatchedAway,Score,sep="|")]
    counts<-wc[,. (WikiN=.N,WikiStage=paste(unique(Stage),collapse="; ")),by=K]
    dc<-merge(dc,counts,by="K",all.x=TRUE)
    dc[,DatedN:=.N,by=K]
    dc[,Decision:=fifelse(is.na(MatchedHome)|is.na(MatchedAway),"CLUB_LABEL_REVIEW",
      fifelse(is.na(WikiN),"SCORE_OR_STAGE_REVIEW",fifelse(WikiN!=1L|DatedN!=1L,"REPEATED_FIXTURE_STAGE_REVIEW","UNIQUE_SCORE_DATE_MATCH_REVIEW")))]
    review[[length(review)+1L]]<-dc;wiki_out[[length(wiki_out)+1L]]<-wc
    summaries[[length(summaries)+1L]]<-dc[,.N,by=.(Country,Decision)]
    if(i%%10L==0L||i==length(countries))cat("  Compared",i,"/",length(countries),"countries\n")
  }
  cat("[2/2] Writing comparison evidence and remaining holds...\n")
  fwrite(rbindlist(proposed,fill=TRUE),file.path(out,"label_evidence.csv"))
  r<-rbindlist(review,fill=TRUE);fwrite(r,file.path(out,"fixture_review.csv"))
  fwrite(rbindlist(summaries,fill=TRUE),file.path(out,"country_summary.csv"))
  print(r[,.N,by=Decision])
  cat("No aliases applied or fixtures imported. Comparison labels still need identity and stage review.\n")
  cat("Report:",normalizePath(out,winslash="/"),"\n")
}
recheck_target_labels()
