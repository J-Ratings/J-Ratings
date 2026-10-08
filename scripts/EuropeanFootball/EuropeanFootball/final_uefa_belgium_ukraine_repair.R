# Final bounded UEFA repair. Preview by default; APPLY_FINAL_UEFA_REPAIR=1 writes.
# Only Belgium 2016/17-2018/19 and Ukraine 2023/24-2024/25 are eligible.
# Uses saved extraction. No downloads, HTML parsing, or inferred alias writes.
run_final_uefa_repair <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE))
    try(beepr::beep(),silent=TRUE),add=TRUE)
  root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
  base <- file.path(root,"EuropeanFootball/pipeline_data")
  input <- file.path(base,"Manual_Sources/UEFA/full_modern_recovery")
  out <- file.path(input,"final_repair")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  master_path <- file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv")
  targets <- data.table(Country=c(rep("Belgium",3),rep("Ukraine",2)),
    Season=c("2016/17","2017/18","2018/19","2023/24","2024/25"))
  cat("[1/5] Reading the five saved seasons and master...\n")
  master <- fread(master_path,showProgress=FALSE)
  original_columns <- names(master)
  master[,MasterRow:=.I]
  m <- merge(master[CompetitionType=="league" & Tier==1L],targets,by=c("Country","Season"))
  m[,Date:=as.IDate(Date)]
  g <- merge(fread(file.path(input,"parser/all_dated_rsssf_games.csv")),targets,by=c("Country","Season"))
  g[,Date:=as.IDate(Date)]
  g <- g[!is.na(Date) & !(tolower(as.character(Annotated)) %chin% c("true","t","1"))]
  aliases <- fread(file.path(base,"Reference/team_aliases.csv"))
  aliases <- aliases[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,
                     by=.(Country,SourceName)]
  amap <- setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  canonical <- function(c,x) {
    for(i in 1:10) { h<-unname(amap[paste(c,x,sep="\r")]); use<-!is.na(h)&h!=x
      if(!any(use)) break; x[use]<-h[use] }; x
  }
  norm <- function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  for(d in list(g,m)) d[,`:=`(H=norm(canonical(Country,Home)),A=norm(canonical(Country,Away)),
                             MonthDay=format(Date,"%m-%d"))]
  cat("[2/5] Resolving repeated fixture evidence within each season...\n")
  # Month/day signatures allow a one-year error to be detected without matching
  # merely on the score. Only unique signatures and repeated consistent evidence
  # are used. Inferred names are comparison keys, never new production aliases.
  sg <- unique(g[,.(Country,Season,MonthDay,Score,H,A)])
  sm <- unique(m[,.(Country,Season,MonthDay,Score,H,A)])
  sg <- sg[,if(.N==1L) .SD else NULL,by=.(Country,Season,MonthDay,Score)]
  sm <- sm[,if(.N==1L) .SD else NULL,by=.(Country,Season,MonthDay,Score)]
  paired <- merge(sg,sm,by=c("Country","Season","MonthDay","Score"),suffixes=c("Source","Target"))
  votes <- unique(rbind(paired[,.(Country,Season,MonthDay,Source=HSource,Target=HTarget)],
                        paired[,.(Country,Season,MonthDay,Source=ASource,Target=ATarget)]))
  votes <- votes[,.(Support=uniqueN(MonthDay)),by=.(Country,Season,Source,Target)]
  votes[,Total:=sum(Support),by=.(Country,Season,Source)]
  setorder(votes,Country,Season,Source,-Support)
  names_review <- votes[, .SD[1L],by=.(Country,Season,Source)]
  names_review[,AcceptedForComparison:=Support>=5L & Support/Total>=0.95]
  fwrite(names_review,file.path(out,"fixture_name_evidence.csv"))
  accepted <- names_review[AcceptedForComparison==TRUE]
  nmap <- setNames(accepted$Target,paste(accepted$Country,accepted$Season,accepted$Source,sep="\r"))
  apply_names <- function(c,s,x) { h<-unname(nmap[paste(c,s,x,sep="\r")]); x[!is.na(h)]<-h[!is.na(h)]; x }
  g[,`:=`(H=apply_names(Country,Season,H),A=apply_names(Country,Season,A))]
  # Recover display names only from existing production names for that season.
  labels <- unique(rbind(m[,.(Country,Season,Name=H,Label=canonical(Country,Home))],
                         m[,.(Country,Season,Name=A,Label=canonical(Country,Away))]))
  labels <- labels[, .(Label=sort(unique(Label))[1L]),by=.(Country,Season,Name)]
  lmap <- setNames(labels$Label,paste(labels$Country,labels$Season,labels$Name,sep="\r"))
  g[,`:=`(ImportHome=unname(lmap[paste(Country,Season,H,sep="\r")]),
           ImportAway=unname(lmap[paste(Country,Season,A,sep="\r")]))]
  key <- function(d,day=TRUE) paste(d$Country,d$Season,d$H,d$A,d$Score,
                                   if(day) d$Date else d$MonthDay,sep="\r")
  g[,ExactPresent:=key(g) %chin% key(m)]
  g[,Duplicate:=duplicated(key(g))]
  m_md <- key(m,FALSE)
  unique_md <- !duplicated(m_md) & !duplicated(m_md,fromLast=TRUE)
  idx <- match(key(g,FALSE),m_md)
  idx[!is.na(idx) & !unique_md[idx]] <- NA_integer_
  g[,MatchedMasterRow:=m$MasterRow[idx]]
  g[,MasterDate:=m$Date[idx]]
  g[,YearCorrection:=Country=="Ukraine" & !ExactPresent & !is.na(MasterDate) &
      MonthDay==format(MasterDate,"%m-%d") & abs(as.integer(format(Date,"%Y"))-as.integer(format(MasterDate,"%Y")))==1L]
  start <- as.integer(substr(g$Season,1,4))
  in_season <- g$Date>=as.IDate(paste0(start,"-07-01")) & g$Date<=as.IDate(paste0(start+1L,"-06-30"))
  g[!in_season,YearCorrection:=FALSE]
  cat("[3/5] Checking Belgian playoff additions and date conflicts...\n")
  g[,RegularClubPair:=FALSE]
  for(i in seq_len(nrow(targets))) {
    ct<-targets$Country[i]; ss<-targets$Season[i]
    regular<-g[Country==ct & Season==ss & RSSSFPhase=="regular" & grepl("^Round [0-9]+",RSSSFStage)]
    clubs<-unique(c(regular$H,regular$A))
    g[Country==ct & Season==ss,RegularClubPair:=H %chin% clubs & A %chin% clubs]
  }
  date_lookup <- split(as.integer(m$Date),paste(m$Country,m$Season,m$H,m$A,sep="\r"))
  pairkey <- paste(g$Country,g$Season,g$H,g$A,sep="\r")
  g[,NearbyPair:=vapply(seq_len(.N),function(i) {
    d<-date_lookup[[pairkey[i]]]; length(d)>0L && any(abs(as.integer(Date[i])-d)<=1L)
  },logical(1))]
  appearances <- unique(c(paste(m$Country,m$Date,m$H,sep="\r"),paste(m$Country,m$Date,m$A,sep="\r")))
  g[,ClubDateConflict:=paste(Country,Date,H,sep="\r") %chin% appearances |
                         paste(Country,Date,A,sep="\r") %chin% appearances]
  g[,Action:=fcase(ExactPresent,"ALREADY_PRESENT",Duplicate,"DUPLICATE_REVIEW",
    YearCorrection,"CORRECT_UKRAINE_YEAR",NearbyPair,"NEARBY_FIXTURE_REVIEW",
    !RegularClubPair,"MIXED_DIVISION_REVIEW",is.na(ImportHome)|is.na(ImportAway),"IDENTITY_REVIEW",
    ClubDateConflict,"CLUB_DATE_CONFLICT_REVIEW",
    Country=="Belgium" & RSSSFPhase=="championship" & grepl("^Round [0-9]+",RSSSFStage),"ADD_BELGIUM_PLAYOFF",
    default="LEAVE_DOCUMENTED")]
  corrections <- unique(g[Action=="CORRECT_UKRAINE_YEAR",.(MasterRow=MatchedMasterRow,OldDate=MasterDate,NewDate=Date,
                              Country,Season,Home, Away,Score)],by="MasterRow")
  adds <- g[Action=="ADD_BELGIUM_PLAYOFF"]
  # Persist only existing approved canonical names; no guessed identities.
  adds[,`:=`(Home=ImportHome,Away=ImportAway)]
  metadata <- m[, .SD[1L],by=Country,.SDcols=c("Competition","League")]
  for(ct in unique(adds$Country)) {
    meta<-metadata[Country==ct]
    adds[Country==ct,`:=`(Competition=meta$Competition[1],League=meta$League[1])]
  }
  if(nrow(adds)) adds[,`:=`(Tier=1L,CompetitionType="league",Source="rsssf",DateApprox=FALSE,
                            HomeAssociation=Country,AwayAssociation=Country)]
  for(col in setdiff(original_columns,names(adds))) adds[,(col):=NA]
  adds <- adds[,..original_columns]
  cat("[4/5] Validating the complete proposed repair...\n")
  proposed <- copy(master)
  if(nrow(corrections)) proposed[corrections$MasterRow,Date:=corrections$NewDate]
  proposed <- proposed[,..original_columns]
  proposed <- rbindlist(list(proposed,adds),use.names=TRUE,fill=FALSE)
  # Validate all changed fixtures against the rest of the domestic master.
  changed <- rbindlist(list(proposed[corrections$MasterRow],adds),use.names=TRUE)
  unaffected <- master[!MasterRow %in% corrections$MasterRow & CompetitionType=="league" & Tier==1L]
  booking <- function(d) c(paste(d$Country,d$Date,norm(canonical(d$Country,d$Home)),sep="\r"),
                          paste(d$Country,d$Date,norm(canonical(d$Country,d$Away)),sep="\r"))
  if(anyDuplicated(booking(changed)) || any(booking(changed) %chin% booking(unaffected)))
    stop("Proposed repair double-books a club. No master write performed.")
  if(any(changed$Home==changed$Away) || anyNA(as.IDate(changed$Date))) stop("Invalid proposed fixtures.")
  fwrite(g[Action!="ALREADY_PRESENT"],file.path(out,"decisions.csv"),na="")
  fwrite(corrections,file.path(out,"verified_year_corrections.csv"),na="")
  fwrite(adds,file.path(out,"verified_belgium_additions.csv"),na="")
  summary <- g[,.(Rows=.N),by=.(Country,Season,Action)]
  fwrite(summary,file.path(out,"repair_summary.csv"))
  print(summary[Action!="ALREADY_PRESENT"])
  cat("Verified date corrections: ",nrow(corrections),"; Belgian additions: ",nrow(adds),".\n",sep="")
  cat("[5/5] ")
  if(identical(Sys.getenv("APPLY_FINAL_UEFA_REPAIR"),"1") && (nrow(corrections)+nrow(adds)>0L)) {
    stamp<-format(Sys.time(),"%Y%m%d_%H%M%S")
    backup<-sub("[.]csv$",paste0("_before_final_uefa_repair_",stamp,".csv"),master_path)
    if(!file.copy(master_path,backup)) stop("Backup failed.")
    tmp<-paste0(master_path,".final_repair.tmp")
    fwrite(proposed,tmp,na="")
    if(nrow(fread(tmp,select="Country",showProgress=FALSE))!=nrow(proposed)) stop("Temporary master failed validation.")
    if(!file.copy(tmp,master_path,overwrite=TRUE)) stop("Could not install validated master.")
    unlink(tmp)
    cat("Master updated: ",nrow(master)," -> ",nrow(proposed),".\nBackup: ",backup,"\n",sep="")
  } else cat("Preview/no additional changes. Set APPLY_FINAL_UEFA_REPAIR=1 to apply verified repairs.\n")
  cat("Remaining discrepancies are documented; no further general UEFA parsing pass is proposed.\nReport: ",out,"\n",sep="")
}
run_final_uefa_repair()
