# Validate only grouped stages that passed the saved stage-structure review.
# No downloads, new aliases, or production writes.
run_grouped_recovery_validation <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE))
    try(beepr::beep(),silent=TRUE),add=TRUE)
  base<-file.path(normalizePath(getwd(),winslash="/",mustWork=TRUE),"EuropeanFootball/pipeline_data")
  saved<-file.path(base,"Manual_Sources/RSSSF_World_Refresh_Review")
  input<-file.path(saved,"grouped_stage_review")
  out<-file.path(saved,"grouped_stage_validation")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Reading plausible stages and current master...\n")
  stages<-fread(file.path(input,"stage_summary.csv"))
  stages<-stages[Decision=="STAGE_STRUCTURE_PLAUSIBLE" & Aggregate==FALSE]
  g<-fread(file.path(input,"located_stage_fixtures.csv"),encoding="UTF-8")
  g<-merge(g,stages[,.(Country,Season,ReviewTable=Table)],by=c("Country","Season","ReviewTable"))
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),encoding="UTF-8",showProgress=FALSE)
  a<-fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
  a<-a[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,by=.(Country,SourceName)]
  amap<-setNames(a$CanonicalName,paste(a$Country,a$SourceName,sep="\r"))
  canon<-function(c,x) { for(i in 1:10) { y<-unname(amap[paste(c,x,sep="\r")]); use<-!is.na(y)&y!=x
    if(!any(use)) break
    x[use]<-y[use]
  };x }
  norm<-function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  g[,`:=`(Home=canon(Country,Home),Away=canon(Country,Away),Date=as.IDate(Date))]
  g[,`:=`(H=norm(Home),A=norm(Away))]
  dom<-m[CompetitionType=="league" & Country %chin% g$Country]
  dom[,`:=`(Date=as.IDate(Date),Home=canon(Country,Home),Away=canon(Country,Away))]
  dom[,`:=`(H=norm(Home),A=norm(Away))]
  key<-function(d) paste(d$Country,d$Date,d$H,d$A,d$Score,sep="\r")
  g[,Present:=key(g) %chin% key(dom)]
  g[,Duplicate:=duplicated(key(g))]
  cat("[2/4] Checking identities, stage schedules and played totals...\n")
  known<-unique(c(paste(dom$Country,dom$H,sep="\r"),paste(dom$Country,dom$A,sep="\r"),
    paste(a$Country,norm(canon(a$Country,a$CanonicalName)),sep="\r")))
  admission<-fread(file.path(saved,"new_roster_club_recheck/new_roster_club_evidence.csv"))
  permitted<-paste(admission[RosterAdmissionEligible==TRUE,Country],
    admission[RosterAdmissionEligible==TRUE,Key],sep="\r")
  g[,IdentityOK:=paste(Country,H,sep="\r") %chin% c(known,permitted) &
    paste(Country,A,sep="\r") %chin% c(known,permitted)]
  schedule<-function(d) unique(c(paste(d$Country,d$Date,d$H,sep="\r"),paste(d$Country,d$Date,d$A,sep="\r")))
  booked<-schedule(dom)
  g[,Conflict:=!Present & (paste(Country,Date,H,sep="\r") %chin% booked |
    paste(Country,Date,A,sep="\r") %chin% booked)]
  appearances<-rbind(g[,.(Country,Season,ReviewTable,Club=RH,Date)],
    g[,.(Country,Season,ReviewTable,Club=RA,Date)])
  roster<-fread(file.path(input,"stage_rosters.csv"))
  totals<-appearances[,.(Appearances=.N),by=.(Country,Season,ReviewTable,Club)]
  totals<-merge(totals,roster[,.(Country,Season,ReviewTable=Table,Club=Key,P)],
    by=c("Country","Season","ReviewTable","Club"),all.x=TRUE)
  bad_stages<-unique(totals[is.na(P)|Appearances>P,.(Country,Season,ReviewTable)])
  stage_key<-function(d) paste(d$Country,d$Season,d$ReviewTable,sep="\r")
  g[,StageCountBad:=stage_key(g) %chin% stage_key(bad_stages)]
  # Different source names must not identify the same roster club within a stage.
  name_map<-rbind(g[,.(Country,Season,ReviewTable,Club=RH,Name=H)],g[,.(Country,Season,ReviewTable,Club=RA,Name=A)])
  variants<-name_map[,.(Names=uniqueN(Name)),by=.(Country,Season,ReviewTable,Club)][Names>1L]
  g[,StageNameAmbiguous:=stage_key(g) %chin% stage_key(variants)]
  clashes<-appearances[,.(Rows=.N),by=.(Country,Date,Club)][Rows>1L]
  g[,StageScheduleBad:=paste(Country,Date,RH,sep="\r") %chin% paste(clashes$Country,clashes$Date,clashes$Club,sep="\r") |
    paste(Country,Date,RA,sep="\r") %chin% paste(clashes$Country,clashes$Date,clashes$Club,sep="\r")]
  sp<-tstrsplit(g$Score,"-",fixed=TRUE)
  valid_score<-grepl("^[0-9]+-[0-9]+$",g$Score)
  expected<-fifelse(as.integer(sp[[1]])>as.integer(sp[[2]]),"1-0",
    fifelse(as.integer(sp[[1]])<as.integer(sp[[2]]),"0-1","0.5-0.5"))
  g[,Invalid:=is.na(Date)|H==A|!valid_score|is.na(expected)|Result!=expected |
    as.integer(format(Date,"%Y"))<as.integer(StartYear) |
    as.integer(format(Date,"%Y"))>as.integer(StartYear)+1L]
  g[,Decision:=fcase(Present,"ALREADY_PRESENT",Duplicate,"DUPLICATE_REVIEW",Invalid,"INVALID_DATE_OR_SCORE",
    StageCountBad|StageNameAmbiguous|StageScheduleBad,"STAGE_CONFLICT_REVIEW",
    Conflict,"CLUB_DATE_CONFLICT_REVIEW",!IdentityOK,"IDENTITY_REVIEW",default="VALIDATED_IMPORT_PREVIEW")]
  add<-g[Decision=="VALIDATED_IMPORT_PREVIEW"]
  metadata<-dom[Tier==1L,.SD[which.max(as.integer(Date))],by=Country,.SDcols=c("Competition","League")]
  for(ct in unique(add$Country)) {
    meta<-metadata[Country==ct]
    if(nrow(meta)!=1L) stop("Missing league metadata: ",ct)
    add[Country==ct,`:=`(Competition=meta$Competition[1],League=meta$League[1])]
  }
  add[,`:=`(Tier=1L,CompetitionType="league",Source="rsssf",DateApprox=FALSE,
    HomeAssociation=Country,AwayAssociation=Country)]
  cat("[3/4] Writing import preview and held evidence...\n")
  summary<-add[,.(ValidatedAdditions=.N),by=.(Country,Season)]
  fwrite(summary,file.path(out,"batch_summary.csv"))
  fwrite(add,file.path(out,"validated_import_preview.csv"),na="")
  fwrite(g[Decision!="VALIDATED_IMPORT_PREVIEW"],file.path(out,"held_review.csv"),na="")
  fwrite(totals,file.path(out,"club_played_checks.csv"),na="")
  cat("[4/4] Validation result:\n")
  print(g[,.(Rows=.N),by=Decision]);print(summary)
  cat("Validated additions: ",nrow(add),". No production changes or downloads.\nReport: ",out,"\n",sep="")
}
run_grouped_recovery_validation()
