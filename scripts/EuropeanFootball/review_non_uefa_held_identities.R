# Diagnose saved identity holds against current domestic AND continental clubs.
# Similar names are review suggestions, never automatic aliases or imports.
run_non_uefa_identity_review <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE))
    try(beepr::beep(),silent=TRUE),add=TRUE)
  base<-file.path(normalizePath(getwd(),winslash="/",mustWork=TRUE),"EuropeanFootball/pipeline_data")
  saved<-file.path(base,"Manual_Sources/RSSSF_World_Refresh_Review")
  out<-file.path(saved,"identity_review")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Reading identity holds and current club names...\n")
  held<-fread(file.path(saved,"abbreviation_recheck/held_fixture_review.csv"),encoding="UTF-8")
  held<-held[Decision=="IDENTITY_REVIEW"]
  master<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),encoding="UTF-8",showProgress=FALSE)
  aliases<-fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
  cmap<-c("Hongkong"="Hong Kong","Macao"="Macau","East Timor"="Timor-Leste",
    "Congo-Brazzaville"="Congo","Congo-Kinshasa"="DR Congo","Guinea Bissau"="Guinea-Bissau",
    "French Guyana"="French Guiana","US Virgin Islands"="United States Virgin Islands",
    "Surinam"="Suriname","Fiji (clubs)"="Fiji","Fiji (districts)"="Fiji","Fiji (national)"="Fiji",
    "Vanuatu (PVFL)"="Vanuatu","Vanuatu (VFFCL)"="Vanuatu")
  country<-function(x) { y<-unname(cmap[x]); x[!is.na(y)]<-y[!is.na(y)]; x }
  norm<-function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  aliases[,Country:=country(Country)]
  aliases<-aliases[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,by=.(Country,SourceName)]
  amap<-setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  canon<-function(c,x) {
    for(i in 1:10) { y<-unname(amap[paste(c,x,sep="\r")]); use<-!is.na(y)&y!=x
      if(!any(use)) break
      x[use]<-y[use]
    }; x
  }
  master[,`:=`(Country=country(Country),Date=as.IDate(Date))]
  held[,`:=`(Country=country(Country),Date=as.IDate(Date))]
  # Continental competitions have a competition region in Country. Attribute
  # each endpoint to its recorded association, never to that competition region.
  appearance<-function(d,home) {
    label<-if(home) d$Home else d$Away
    association<-if(home) d$HomeAssociation else d$AwayAssociation
    fallback<-is.na(association)|!nzchar(association)
    association[fallback & d$CompetitionType=="league"]<-d$Country[fallback & d$CompetitionType=="league"]
    data.table(Country=country(association),Name=label,Date=d$Date,Type=d$CompetitionType)
  }
  known<-rbindlist(list(appearance(master,TRUE),appearance(master,FALSE)))
  known<-known[!is.na(Country)&nzchar(Country)&!is.na(Name)&nzchar(Name)]
  known[,Name:=canon(Country,Name)]
  known[,Key:=norm(Name)]
  inventory<-known[,.(CanonicalName=Name[1],FirstRecorded=min(Date,na.rm=TRUE),
    LastRecorded=max(Date,na.rm=TRUE),Domestic=any(Type=="league")),by=.(Country,Key)]
  alias_known<-aliases[,.(Country,Key=norm(canon(Country,CanonicalName)))]
  known_keys<-unique(c(paste(inventory$Country,inventory$Key,sep="\r"),
    paste(alias_known$Country,alias_known$Key,sep="\r")))
  held[,`:=`(Home=canon(Country,Home),Away=canon(Country,Away))]
  held[,`:=`(H=norm(Home),A=norm(Away))]
  held[,`:=`(HomeKnown=paste(Country,H,sep="\r") %chin% known_keys,
             AwayKnown=paste(Country,A,sep="\r") %chin% known_keys)]
  endpoints<-rbind(held[HomeKnown==FALSE,.(Country,SourceName=Home,Key=H,Season,Date,StructurePass)],
    held[AwayKnown==FALSE,.(Country,SourceName=Away,Key=A,Season,Date,StructurePass)])
  unresolved<-endpoints[,.(SourceName=SourceName[1],Appearances=.N,Seasons=uniqueN(Season),
    FirstDate=min(Date),LastDate=max(Date),StructurePassedAppearances=sum(StructurePass)),by=.(Country,Key)]
  cat("[2/4] Testing repeated date, score and known-opponent evidence...\n")
  dom<-master[CompetitionType=="league" & Country %chin% held$Country]
  dom[,`:=`(H=norm(canon(Country,Home)),A=norm(canon(Country,Away)))]
  old<-unique(rbind(dom[,.(Country,Date,Score,Side="home",Opponent=A,Target=H)],
    dom[,.(Country,Date,Score,Side="away",Opponent=H,Target=A)]))
  source<-unique(rbind(held[HomeKnown==FALSE & AwayKnown==TRUE,
    .(Country,Date,Score,Side="home",Opponent=A,Key=H)],
    held[AwayKnown==FALSE & HomeKnown==TRUE,
    .(Country,Date,Score,Side="away",Opponent=H,Key=A)]))
  votes<-merge(source,old,by=c("Country","Date","Score","Side","Opponent"),allow.cartesian=TRUE)
  # A matching fixture with multiple target clubs is ambiguous, not evidence.
  votes[,TargetCount:=uniqueN(Target),by=.(Country,Key,Date)]
  votes<-unique(votes[TargetCount==1L,.(Country,Key,Date,Target)])
  evidence<-votes[,.(SupportDates=uniqueN(Date)),by=.(Country,Key,Target)]
  evidence[,TotalSupport:=sum(SupportDates),by=.(Country,Key)]
  evidence<-merge(evidence,inventory[,.(Country,Target=Key,TargetName=CanonicalName)],by=c("Country","Target"),all.x=TRUE)
  fwrite(evidence,file.path(out,"repeated_fixture_evidence.csv"),na="")
  cat("[3/4] Ranking possible aliases within each association...\n")
  suggestions<-list()
  for(i in seq_len(nrow(unresolved))) {
    ct<-unresolved$Country[i]; key<-unresolved$Key[i]
    candidates<-inventory[Country==ct]
    if(nrow(candidates)) {
      distance<-as.integer(adist(key,candidates$Key))
      similarity<-1-distance/pmax(nchar(key),nchar(candidates$Key))
      take<-head(order(-similarity,distance,candidates$CanonicalName),3)
      suggestions[[length(suggestions)+1L]]<-data.table(Country=ct,Key=key,
        CandidateRank=seq_along(take),CandidateName=candidates$CanonicalName[take],
        Similarity=round(similarity[take],3),EditDistance=distance[take])
    }
    if(i==1L||i%%25L==0L||i==nrow(unresolved)) cat("  Compared ",i,"/",nrow(unresolved)," unresolved names\n",sep="")
  }
  suggestion<-if(length(suggestions)) rbindlist(suggestions) else
    data.table(Country=character(),Key=character(),CandidateRank=integer(),CandidateName=character(),Similarity=numeric(),EditDistance=integer())
  review<-merge(unresolved,suggestion[CandidateRank==1L],by=c("Country","Key"),all.x=TRUE)
  setorder(evidence,Country,Key,-SupportDates)
  top<-evidence[,head(.SD,1L),by=.(Country,Key)]
  review<-merge(review,top,by=c("Country","Key"),all.x=TRUE)
  review[,ReviewReason:=fcase(!is.na(SupportDates)&SupportDates>=3L&SupportDates==TotalSupport,
    "REPEATED_FIXTURE_ALIAS_EVIDENCE",!is.na(Similarity)&Similarity>=0.85,
    "SIMILAR_EXISTING_NAME_REVIEW",default="NO_CLOSE_NAME_NEW_CLUB_OR_ALIAS_REVIEW")]
  setorder(review,-Appearances,Country,SourceName)
  fwrite(review,file.path(out,"unresolved_club_review.csv"),na="")
  fwrite(merge(unresolved,suggestion,by=c("Country","Key")),file.path(out,"name_suggestions.csv"),na="")
  fwrite(held,file.path(out,"held_fixture_identity_status.csv"),na="")
  overview<-held[,.(HeldFixtures=.N,NowKnown=sum(HomeKnown & AwayKnown),
    StillUnknown=sum(!HomeKnown | !AwayKnown)),by=Country][order(-HeldFixtures)]
  fwrite(overview,file.path(out,"country_summary.csv"))
  cat("[4/4] Identity review overview:\n")
  print(review[,.(Names=.N,Appearances=sum(Appearances)),by=ReviewReason])
  print(head(review[,.(Country,SourceName,Appearances,CandidateName,Similarity,ReviewReason)],20))
  cat("Previously held fixtures now recognised: ",sum(held$HomeKnown & held$AwayKnown),"\n",sep="")
  cat("No aliases applied, imports, downloads or production changes. Report: ",out,"\n",sep="")
}
run_non_uefa_identity_review()
