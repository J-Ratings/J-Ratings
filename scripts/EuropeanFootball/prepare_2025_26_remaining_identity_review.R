# Offline, ranked review of names actually blocking the current recovery.
# No downloads, aliases, imports or rating changes.
prepare_remaining_identity_review <- function() {
  suppressPackageStartupMessages({library(data.table);library(stringi)})
  started<-Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  base<-"EuropeanFootball/pipeline_data"
  batch<-file.path(base,"Manual_Sources/Season_2025_26")
  out<-file.path(batch,"remaining_identity_review")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Reading held fixtures and current league identities...\n")
  held<-fread(file.path(batch,"import_validation/held_or_present.csv"),encoding="UTF-8")
  held<-held[Validation=="UNKNOWN_OR_CHANGED_CLUB_IDENTITY_REVIEW"]
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),
    select=c("Country","CompetitionType","Home","Away"),encoding="UTF-8",showProgress=FALSE)
  a<-fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
  source("scripts/EuropeanFootball/club_identity_resolution.R",local=TRUE)
  lm<-setNames(a$CanonicalName,paste(a$Country,a$SourceName,sep="\r"))
  rm<-setNames(character(),character())
  norm<-function(z)gsub("[^a-z0-9]","",tolower(stri_trans_general(z,"Latin-ASCII")))
  words<-function(z)trimws(gsub(" +"," ",gsub("[^a-z0-9]"," ",tolower(stri_trans_general(z,"Latin-ASCII")))))
  clubs<-unique(rbind(m[CompetitionType=="league",.(Country,Name=Home)],m[CompetitionType=="league",.(Country,Name=Away)]))
  clubs[,Name:=resolve_alias_chain(Name,Country,lm,rm)]
  clubs<-unique(clubs);clubs[,NK:=norm(Name)]
  cat("[2/4] Separating unknown clubs from their already-known opponents...\n")
  apps<-rbind(held[,.(Country,Season,CandidateID,SourceName=Home,WikiName=MatchedHome,CanonicalName=CanonicalHome)],
    held[,.(Country,Season,CandidateID,SourceName=Away,WikiName=MatchedAway,CanonicalName=CanonicalAway)])
  known<-paste(clubs$Country,clubs$NK,sep="|")
  apps[,IsKnown:=paste(Country,norm(CanonicalName),sep="|") %chin% known]
  unknown<-apps[IsKnown==FALSE]
  review<-unknown[,.(BlockedMatches=uniqueN(CandidateID),Seasons=paste(sort(unique(Season)),collapse="; ")),
    by=.(Country,SourceName,WikiName)]
  evidence_path<-file.path(batch,"club_article_evidence/club_article_review.csv")
  if(file.exists(evidence_path)) {
    evidence<-fread(evidence_path,encoding="UTF-8")
    evidence<-unique(evidence[,.(Country,WikiName,ArticleURL,EvidenceDecision,LeadHistoricalNames,LeadExcerpt,Error)],by=c("Country","WikiName"))
    review<-merge(review,evidence,by=c("Country","WikiName"),all.x=TRUE)
  }
  cat("[3/4] Ranking historical spelling and abbreviation candidates...\n")
  # Suggestions only: short names, initials and shared words never authorize a merge.
  generic<-c("fc","cf","afc","fk","kf","sc","ac","club","clube","football","futbol","futebol","de","da","do","the")
  tokens<-function(z)setdiff(strsplit(words(z)," ",fixed=TRUE)[[1]],generic)
  suggestions<-vector("list",nrow(review))
  for(i in seq_len(nrow(review))) {
    if(i==1L||i%%25L==0L||i==nrow(review))cat(sprintf("  Club %d/%d\n",i,nrow(review)))
    r<-review[i];candidates<-clubs[Country==r$Country]
    wt<-tokens(r$WikiName);st<-tokens(r$SourceName)
    scores<-vapply(candidates$Name,function(n){
      ct<-tokens(n);unionwords<-unique(c(wt,ct))
      wordscore<-if(length(unionwords))length(intersect(wt,ct))/length(unionwords) else 0
      distance<-as.numeric(adist(norm(r$WikiName),norm(n)))/max(1L,nchar(norm(r$WikiName)),nchar(norm(n)))
      max(wordscore,1-distance)
    },numeric(1))
    orderids<-head(order(scores,decreasing=TRUE),3L)
    if(length(orderids))suggestions[[i]]<-data.table(Country=r$Country,SourceName=r$SourceName,WikiName=r$WikiName,
      CandidateName=candidates$Name[orderids],Similarity=round(scores[orderids],3),Rank=seq_along(orderids))
  }
  ranked<-rbindlist(suggestions,fill=TRUE)
  setorder(review,-BlockedMatches,Country,WikiName)
  review[,`:=`(ReviewedCanonicalName="",Decision="",EvidenceURL="",ReviewNotes="")]
  fwrite(review,file.path(out,"club_review.csv"),na="")
  fwrite(ranked,file.path(out,"historical_name_suggestions.csv"),na="")
  fixturecounts<-unknown[,.(UnknownClubs=uniqueN(paste(SourceName,WikiName,sep="|"))),by=CandidateID]
  detail<-merge(held,fixturecounts,by="CandidateID",all.x=TRUE)
  fwrite(detail,file.path(out,"held_fixture_evidence.csv"),na="")
  summary<-detail[,.(HeldMatches=.N,UnknownNames=uniqueN(c(MatchedHome,MatchedAway)),
    OneUnknownClub=sum(UnknownClubs==1L),TwoUnknownClubs=sum(UnknownClubs==2L)),by=.(Country,Season)]
  setorder(summary,-HeldMatches)
  fwrite(summary,file.path(out,"league_priorities.csv"))
  cat("[4/4] Largest identity bottlenecks:\n")
  print(head(review[,.(Country,WikiName,BlockedMatches)],25L))
  cat("Distinct held matches:",nrow(held),"; unresolved club labels:",nrow(review),"\n")
  cat("Counts by club overlap: a game can involve two unknown clubs.\n")
  cat("Suggestions are not approved mappings; new clubs require identity evidence.\n")
  cat("No downloads or production changes. Report:",normalizePath(out,winslash="/"),"\n")
}
prepare_remaining_identity_review()
