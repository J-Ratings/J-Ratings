# Audit only. No imports, new aliases, ratings or JSON writes.
match_target_wikipedia_dates <- function() {
  suppressPackageStartupMessages({library(data.table);library(xml2);library(rvest);library(stringi)})
  started<-Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  base<-"EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26"
  out<-file.path(base,"score_date_matching");cache<-file.path(out,"cache")
  dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  candidates<-fread(file.path(base,"wikipedia_discovery/season_page_candidates.csv"),encoding="UTF-8")
  selected<-copy(candidates[Rank==1L])
  corrections<-c(Fiji="2025 Fiji Premier League",Iceland="2025 Besta deild karla",
    Indonesia="2025\u201326 Super League (Indonesia)",Japan="2025 J1 League",
    "San Marino"="2025\u201326 Campionato Sammarinese di Calcio")
  for(country in names(corrections)) {
    row<-candidates[Country==country&Title==corrections[[country]]]
    if(nrow(row)!=1L)stop("Missing explicit page choice for ",country)
    selected<-rbind(selected[Country!=country],row)
  }
  # These searches did not identify a correct target-season domestic page.
  holds<-c("Comoros","Ethiopia","Libya","Oman","Panama","Peru","Romania")
  selected[,PageDecision:=fifelse(Country %in% holds|!TitleContainsTargetSeason,
    "HOLD_PAGE_SCOPE","SELECTED_SEASON_PAGE_CONTENT_REVIEW")]
  selected[,Reason:=fifelse(Country %in% holds,"Search did not identify a correct target-season league page","")]
  setorder(selected,Country)
  fwrite(selected,file.path(out,"selected_pages.csv"))
  if(Sys.getenv("SEASON_MATCH_PREPARE_ONLY","0")=="1") {
    print(selected[,.N,by=PageDecision]);cat("Prepared page selection; no downloads.\n");return(invisible(selected))
  }
  needed<-c("clean_text","clean_team_name","team_key","capture","empty_games","wiki_games","wiki_link_identities")
  engine<-new.env(parent=environment())
  for(expr in parse("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R"))
    if(is.call(expr)&&identical(expr[[1]],as.name("<-"))&&as.character(expr[[2]])[1]%in%needed)eval(expr,engine)
  aliases<-fread("EuropeanFootball/pipeline_data/Reference/team_aliases.csv",encoding="UTF-8")
  source("scripts/EuropeanFootball/club_identity_resolution.R",local=TRUE)
  local_map<-setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  regional_map<-setNames(character(),character())
  norm<-function(x,country)gsub("[^a-z0-9]","",tolower(stri_trans_general(resolve_alias_chain(x,country,local_map,regional_map),"Latin-ASCII")))
  dated<-fread(file.path(base,"cached_source_review/dated_fixture_candidates.csv"),encoding="UTF-8")
  dated[,Pair:=paste(norm(Home,Country),norm(Away,Country),sep="|")]
  dated[,EvidenceKey:=paste(Pair,Score,sep="|")]
  ready<-selected[PageDecision!="HOLD_PAGE_SCOPE"]
  filter<-trimws(Sys.getenv("SEASON_MATCH_COUNTRIES",""))
  if(nzchar(filter))ready<-ready[Country %chin% trimws(strsplit(filter,",",fixed=TRUE)[[1]])]
  if(!nrow(ready))stop("No selected countries to match")
  summary<-list();matched<-list();wiki_all<-list();held<-list();stop_network<-FALSE
  cat("Matching",nrow(ready),"selected season pages;",sum(selected$PageDecision=="HOLD_PAGE_SCOPE"),"page selections held.\n")
  for(i in seq_len(nrow(ready))) {
    c<-ready[i];path<-file.path(cache,paste0(gsub("[^A-Za-z0-9]","_",c$Country),".html"))
    cat(sprintf("  [%d/%d] %s: %s\n",i,nrow(ready),c$Country,c$Title))
    warnings_seen<-character()
    tryCatch({
      if(!file.exists(path)) {
        if(Sys.getenv("SEASON_MATCH_CACHE_ONLY","0")=="1")stop("Page not cached; offline run holds this country")
        # Reuse the original successful Wales pilot cache.
        pilot<-file.path(base,"Wales_test/sources/wiki.html")
        if(c$Country=="Wales"&&file.exists(pilot))file.copy(pilot,path) else {
          Sys.sleep(10);old<-options(timeout=40)
          tryCatch(withCallingHandlers(download.file(c$URL,paste0(path,".part"),mode="wb",quiet=TRUE,method="libcurl",
            headers=c("User-Agent"="J-Ratings-Season-Audit/1.0 (local football data research)")),
            warning=function(w){warnings_seen<<-c(warnings_seen,conditionMessage(w));invokeRestart("muffleWarning")}),finally=options(old))
          doc<-read_html(paste0(path,".part"))
          title<-xml_text(xml_find_first(doc,"//h1"))
          if(is.na(title)||!nzchar(title))stop("No article heading; response held")
          if(!file.rename(paste0(path,".part"),path))stop("Cannot save article cache")
        }
      }
      doc<-read_html(path)
      heading<-xml_text(xml_find_first(doc,"//h1"))
      if(is.na(heading)||trimws(heading)!=c$Title)stop("Article heading differs from selected season page; redirect held")
      w<-engine$wiki_games(doc)$games
      if(!nrow(w))stop("No Wikipedia result tables extracted")
      w[,`:=`(Country=c$Country,PageTitle=c$Title,PageURL=c$URL)]
      w[,Pair:=paste(norm(Home,Country),norm(Away,Country),sep="|")]
      w[,EvidenceKey:=paste(Pair,Score,sep="|")]
      # Same home/away/score repeated across stages is explicitly ambiguous.
      # Do not choose a date by row order or silently collapse repetitions.
      dc<-dated[Country==c$Country&ScopeDecision!="REGIONAL_LEAGUE_SCOPE_REVIEW"]
      wc<-w[,. (WikiN=.N,WikiHome=Home[1],WikiAway=Away[1],WikiStage=paste(unique(Stage),collapse="; ")),by=EvidenceKey]
      counts<-dc[,. (DatedN=.N),by=EvidenceKey]
      review<-merge(dc,wc,by="EvidenceKey",all.x=TRUE)
      review<-merge(review,counts,by="EvidenceKey",all.x=TRUE)
      review[,Decision:=fifelse(is.na(WikiN),"NO_EXACT_SCORE_NAME_MATCH",
        fifelse(WikiN!=1L|DatedN!=1L,"REPEATED_FIXTURE_STAGE_REVIEW","UNIQUE_SCORE_DATE_MATCH_REVIEW"))]
      review[,WikipediaURL:=c$URL]
      matched[[length(matched)+1L]]<-review
      wiki_all[[length(wiki_all)+1L]]<-w
      summary[[length(summary)+1L]]<-data.table(Country=c$Country,Page=c$Title,WikiResults=nrow(w),DatedResults=nrow(dc),
        UniqueMatches=sum(review$Decision=="UNIQUE_SCORE_DATE_MATCH_REVIEW"),Ambiguous=sum(review$Decision=="REPEATED_FIXTURE_STAGE_REVIEW"),
        UnmatchedDated=sum(review$Decision=="NO_EXACT_SCORE_NAME_MATCH"),WikiWithoutDatedEvidence=sum(!w$EvidenceKey %chin% dc$EvidenceKey),Error="")
    },error=function(e){
      detail<-paste(c(conditionMessage(e),warnings_seen),collapse=" | ")
      held[[length(held)+1L]]<<-data.table(Country=c$Country,Page=c$Title,Error=detail)
      cat("    Held:",detail,"\n")
      if(grepl("403|429",detail))stop_network<<-TRUE
      if(file.exists(paste0(path,".part")))unlink(paste0(path,".part"))
    })
    if(length(summary))fwrite(rbindlist(summary),file.path(out,"country_summary.csv"))
    if(length(held))fwrite(rbindlist(held),file.path(out,"page_errors.csv"))
    if(length(matched))fwrite(rbindlist(matched,fill=TRUE),file.path(out,"fixture_match_review.csv"))
    if(length(wiki_all))fwrite(rbindlist(wiki_all,fill=TRUE),file.path(out,"wikipedia_results.csv"))
    if(stop_network){cat("Access restriction: stopping downloads; review progress saved.\n");break}
  }
  if(length(matched))print(rbindlist(matched,fill=TRUE)[,.N,by=Decision])
  if(!length(held))fwrite(data.table(Country=character(),Page=character(),Error=character()),file.path(out,"page_errors.csv"))
  cat("Matches remain review candidates: full-season and stage completeness are not assumed.\n")
  cat("No imports, new aliases or ratings changes. Report:",normalizePath(out,winslash="/"),"\n")
}
match_target_wikipedia_dates()
