# Resolve source-label bridges and inspect genuinely new roster clubs offline.
# Writes batch-scoped evidence only; never changes global aliases or the master.
review_target_identities <- function() {
  suppressPackageStartupMessages({library(data.table);library(stringi)})
  base<-"EuropeanFootball/pipeline_data"
  out<-file.path(base,"Manual_Sources/Season_2025_26/identity_hold_review")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/3] Reading held identities and existing domestic clubs...\n")
  h<-fread(file.path(base,"Manual_Sources/Season_2025_26/import_validation/held_or_present.csv"),encoding="UTF-8")
  h<-h[Validation=="UNKNOWN_OR_CHANGED_CLUB_IDENTITY_REVIEW"]
  labels<-fread(file.path(base,"Manual_Sources/Season_2025_26/club_label_recheck/label_evidence.csv"),encoding="UTF-8")
  a<-fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),select=c("Country","CompetitionType","Home","Away"),encoding="UTF-8")
  source("scripts/EuropeanFootball/club_identity_resolution.R",local=TRUE)
  lm<-setNames(a$CanonicalName,paste(a$Country,a$SourceName,sep="\r"));rm<-setNames(character(),character())
  canon<-function(n,c)resolve_alias_chain(n,c,lm,rm)
  norm<-function(n)gsub("[^a-z0-9]","",tolower(stri_trans_general(n,"Latin-ASCII")))
  clubs<-unique(rbind(m[CompetitionType=="league",.(Country,Name=Home)],m[CompetitionType=="league",.(Country,Name=Away)]))
  clubs[,Name:=canon(Name,Country)];clubs<-unique(clubs);clubs[,NK:=norm(Name)]
  unknown<-unique(rbind(h[,.(Country,SourceName=Home,WikiName=MatchedHome)],h[,.(Country,SourceName=Away,WikiName=MatchedAway)]))
  unknown<-merge(unknown,labels[,.(Country,SourceName,WikiName,ScoreAgreements,DistinctOpponents,LabelDecision=Decision)],by=c("Country","SourceName","WikiName"),all.x=TRUE)
  cat("[2/3] Testing exact source-label bridges and historical-name collisions...\n")
  results<-list()
  for(i in seq_len(nrow(unknown))) {
    x<-unknown[i];country<-x$Country
    sk<-norm(canon(x$SourceName,country));wk<-norm(canon(x$WikiName,country))
    known<-clubs[Country==country]
    sr<-unique(known[NK==sk,Name]);wr<-unique(known[NK==wk,Name])
    target<-"";decision<-"HISTORICAL_IDENTITY_REVIEW";reason<-""
    if(length(wr)==1L) {target<-wr;decision<-"ALREADY_KNOWN_WIKI_IDENTITY"}
    else if(length(sr)==1L&&x$LabelDecision=="COMPARISON_LABEL_SUPPORTED") {
      target<-sr;decision<-"EXACT_EXISTING_SOURCE_LABEL_BRIDGE"
    } else if(!length(sr)&&!length(wr)&&x$LabelDecision=="COMPARISON_LABEL_SUPPORTED"&&
      !is.na(x$ScoreAgreements)&&x$ScoreAgreements>=4L&&x$DistinctOpponents>=2L) {
      # New club names remain REVIEW candidates. Report close historical names
      # instead of admitting all promoted clubs or inventing a global alias.
      keys<-unique(known$NK)
      distances<-if(length(keys))as.numeric(adist(wk,keys))else numeric()
      close<-keys[distances<=max(2L,floor(nchar(wk)*0.15))]
      reason<-paste(known[NK %in% close,Name],collapse="; ")
      decision<-if(length(close))"POSSIBLE_HISTORICAL_VARIANT"else "TWO_SOURCE_NEW_ROSTER_CLUB_REVIEW"
    }
    results[[i]]<-data.table(x,CanonicalName=target,IdentityDecision=decision,NearbyHistoricalNames=reason)
  }
  r<-rbindlist(results,fill=TRUE)
  # No bridge may redirect a name that is already a distinct canonical club.
  r[IdentityDecision=="EXACT_EXISTING_SOURCE_LABEL_BRIDGE"&
    paste(Country,norm(canon(WikiName,Country)),sep="|") %chin% paste(clubs$Country,clubs$NK,sep="|"),
    IdentityDecision:="CONFLICTING_KNOWN_IDENTITIES_REVIEW"]
  fwrite(r,file.path(out,"identity_evidence.csv"))
  fwrite(r[IdentityDecision=="EXACT_EXISTING_SOURCE_LABEL_BRIDGE"],file.path(out,"supported_source_bridges.csv"))
  fwrite(r[IdentityDecision=="TWO_SOURCE_NEW_ROSTER_CLUB_REVIEW"],file.path(out,"new_roster_clubs.csv"))
  cat("[3/3] Identity evidence overview:\n");print(r[,.N,by=IdentityDecision])
  cat("No identities applied. Report:",normalizePath(out,winslash="/"),"\n")
}
review_target_identities()
