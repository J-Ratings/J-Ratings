# Final bounded review of six material UEFA gaps. Uses saved extraction only.
# No web requests, HTML parsing, alias writes, imports, or Elo calculations.
run_final_uefa_review <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE))
    try(beepr::beep(),silent=TRUE),add=TRUE)
  root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
  base <- file.path(root,"EuropeanFootball/pipeline_data")
  audit <- file.path(base,"Manual_Sources/UEFA/full_modern_recovery")
  out <- file.path(audit,"final_material_review")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  targets <- data.table(Country=c(rep("Belgium",3),rep("Ukraine",2),"Switzerland"),
    Season=c("2016/17","2017/18","2018/19","2023/24","2024/25","2024/25"))
  cat("[1/4] Reading saved fixtures for six seasons...\n")
  g <- fread(file.path(audit,"parser/all_dated_rsssf_games.csv"))
  g <- merge(g,targets,by=c("Country","Season"))
  g <- g[!(tolower(as.character(Annotated)) %chin% c("true","t","1"))]
  g[,Date:=as.IDate(Date)]
  m <- fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),showProgress=FALSE)
  m <- m[Country %in% targets$Country & CompetitionType=="league" & Tier==1L]
  m[,Date:=as.IDate(Date)]
  aliases <- fread(file.path(base,"Reference/team_aliases.csv"))
  aliases <- aliases[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,
                     by=.(Country,SourceName)]
  amap <- setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  canonical <- function(c,x) {
    for(i in 1:10) { h<-unname(amap[paste(c,x,sep="\r")]); use<-!is.na(h)&h!=x
      if(!any(use)) break; x[use]<-h[use] }; x
  }
  norm <- function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  g[,`:=`(H=norm(canonical(Country,Home)),A=norm(canonical(Country,Away)))]
  m[,`:=`(H=norm(canonical(Country,Home)),A=norm(canonical(Country,Away)))]
  evidence <- fread(file.path(audit,"reconciliation/name_evidence.csv"))
  maps <- evidence[AcceptedForComparison==TRUE & Country %in% targets$Country]
  map <- setNames(maps$Target,paste(maps$Country,maps$Source,sep="\r"))
  map_names <- function(c,x) { h<-unname(map[paste(c,x,sep="\r")]); x[!is.na(h)]<-h[!is.na(h)]; x }
  g[,`:=`(H=map_names(Country,H),A=map_names(Country,A))]
  cat("[2/4] Testing extra name evidence with a known opponent...\n")
  # An exact date, score, home/away role and known opponent supplies stronger
  # evidence than a score alone. Ambiguous signatures contribute no vote.
  anchored <- function(home) {
    src <- g[,.(Country,Date,Score,Opponent=if(home) A else H,Source=if(home) H else A)]
    dst <- m[,.(Country,Date,Score,Opponent=if(home) A else H,Target=if(home) H else A)]
    src <- unique(src); dst <- unique(dst)
    dst <- dst[,if(.N==1L) .SD else NULL,by=.(Country,Date,Score,Opponent)]
    merge(src,dst,by=c("Country","Date","Score","Opponent"))[,.(Country,Date,Source,Target)]
  }
  votes <- unique(rbind(anchored(TRUE),anchored(FALSE)))
  votes <- votes[,.(Support=uniqueN(Date)),by=.(Country,Source,Target)]
  votes[,Total:=sum(Support),by=.(Country,Source)]
  setorder(votes,Country,Source,-Support)
  proposed <- votes[, .SD[1L],by=.(Country,Source)]
  proposed[,AcceptedForComparison:=Support>=3L & Support/Total>=0.95]
  known <- unique(rbind(m[,.(Country,Name=H)],m[,.(Country,Name=A)]))
  proposed[paste(Country,Source,sep="\r") %chin% paste(known$Country,known$Name,sep="\r") & Source!=Target,
           AcceptedForComparison:=FALSE]
  fwrite(proposed[Source!=Target],file.path(out,"additional_name_evidence.csv"))
  extra <- proposed[AcceptedForComparison==TRUE & Source!=Target]
  map <- setNames(extra$Target,paste(extra$Country,extra$Source,sep="\r"))
  g[,`:=`(H=map_names(Country,H),A=map_names(Country,A))]
  pair <- function(d) paste(d$Country,d$H,d$A,d$Score,sep="\r")
  key <- function(d) paste(pair(d),d$Date,sep="\r")
  g[,AlreadyPresent:=key(g) %chin% key(m)]
  dates <- split(as.integer(m$Date),pair(m))
  gp <- pair(g)
  g[,NearbyDateMatch:=vapply(seq_len(.N),function(i) {
    d<-dates[[gp[i]]]; length(d)>0L && any(abs(as.integer(Date[i])-d)<=1L)
  },logical(1))]
  cat("[3/4] Checking playoff membership and club schedules...\n")
  g[,NumberedRound:=grepl("^Round [0-9]+(?:$|[[:space:]]|\\[)",RSSSFStage,perl=TRUE)]
  g[,RegularClubPair:=FALSE]
  for(i in seq_len(nrow(targets))) {
    ct<-targets$Country[i]; ss<-targets$Season[i]
    regular <- g[Country==ct & Season==ss & RSSSFPhase=="regular" & NumberedRound==TRUE]
    clubs <- unique(c(regular$H,regular$A))
    g[Country==ct & Season==ss,RegularClubPair:=H %chin% clubs & A %chin% clubs]
    cat("  ",i,"/",nrow(targets),": ",ct," ",ss,"; ",length(clubs)," regular-stage names\n",sep="")
  }
  appearances <- unique(c(paste(m$Country,m$Date,m$H,sep="\r"),paste(m$Country,m$Date,m$A,sep="\r")))
  g[,ClubDateConflict:=paste(Country,Date,H,sep="\r") %chin% appearances |
                         paste(Country,Date,A,sep="\r") %chin% appearances]
  g[,DuplicateRow:=duplicated(key(g))]
  g[,Decision:=fcase(AlreadyPresent,"PRESENT_AFTER_NAME_RECONCILIATION",
    NearbyDateMatch,"SAME_FIXTURE_WITHIN_ONE_DAY",DuplicateRow,"DUPLICATE_ROW",
    !RegularClubPair,"MIXED_DIVISION_OR_IDENTITY_REVIEW",
    !NumberedRound,"NON_NUMBERED_PLAYOFF_REVIEW",
    ClubDateConflict,"NAME_DATE_OR_SCORE_CONFLICT",
    default="MATERIAL_RECOVERY_CANDIDATE")]
  summary <- g[,.(ParsedDated=.N,Present=sum(AlreadyPresent),NearbyDate=sum(!AlreadyPresent & NearbyDateMatch),
    RecoveryCandidates=sum(Decision=="MATERIAL_RECOVERY_CANDIDATE"),
    RemainingReview=sum(!Decision %chin% c("PRESENT_AFTER_NAME_RECONCILIATION","SAME_FIXTURE_WITHIN_ONE_DAY","MATERIAL_RECOVERY_CANDIDATE"))),
    by=.(Country,Season)]
  summary[,Recommendation:=fifelse(RecoveryCandidates>=20L,"CHECK_MATERIAL_BATCH_ONCE",
    fifelse(RecoveryCandidates>=5L,"OPTIONAL_SMALL_BATCH","STOP_GENERAL_PARSER_WORK"))]
  fwrite(summary,file.path(out,"final_season_summary.csv"))
  fwrite(g[AlreadyPresent==FALSE],file.path(out,"final_fixture_review.csv"))
  fwrite(g[Decision=="MATERIAL_RECOVERY_CANDIDATE"],file.path(out,"material_recovery_candidates.csv"))
  cat("[4/4] Final targeted review:\n"); print(summary)
  cat("Stop rule: focus on batches of at least 20 plausible missing league games;\n")
  cat("document smaller residuals and stop general UEFA parser changes.\n")
  cat("These are review candidates, not approved imports. Inferred names were used only for comparison.\n")
  cat("No downloads, reparsing, alias changes or master writes.\nOutput: ",out,"\n",sep="")
}
run_final_uefa_review()
