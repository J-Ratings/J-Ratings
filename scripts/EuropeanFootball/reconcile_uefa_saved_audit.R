# Reconcile saved UEFA extraction with master fixtures. Review only; no parsing,
# web requests, alias writes, or production writes.
run_saved_uefa_reconciliation <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE)) try(beepr::beep(),silent=TRUE),add=TRUE)
  root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
  base <- file.path(root,"EuropeanFootball/pipeline_data")
  out <- file.path(base,"Manual_Sources/UEFA/full_modern_recovery/reconciliation")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Reading saved extraction and master...\n")
  g <- fread(file.path(dirname(out),"parser/all_dated_rsssf_games.csv"))
  g <- g[!is.na(Date) & !(tolower(as.character(Annotated)) %chin% c("true","t","1"))]
  g[,Date:=as.IDate(Date)]
  g[,RowId:=.I]
  m <- fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),showProgress=FALSE)
  m <- m[CompetitionType=="league" & Tier==1L & Country %in% unique(g$Country)]
  m[,Date:=as.IDate(Date)]
  a <- fread(file.path(base,"Reference/team_aliases.csv"))
  a <- a[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,by=.(Country,SourceName)]
  amap <- setNames(a$CanonicalName,paste(a$Country,a$SourceName,sep="\r"))
  canon <- function(c,x) {
    for(i in 1:10) { h<-unname(amap[paste(c,x,sep="\r")]); use<-!is.na(h)&h!=x
      if(!any(use)) break; x[use]<-h[use] }
    x
  }
  norm <- function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  g[,`:=`(H=norm(canon(Country,Home)),A=norm(canon(Country,Away)))]
  m[,`:=`(H=norm(canon(Country,Home)),A=norm(canon(Country,Away)))]
  fixture <- function(d) paste(d$Country,d$Date,d$H,d$A,d$Score,sep="\r")
  g[,MatchedExisting:=fixture(g) %chin% fixture(m)]
  cat("[2/4] Comparing repeated fixture evidence for naming differences...\n")
  # Unique country/date/score signatures supply independent pairing evidence.
  # One collision is insufficient: require repeated, overwhelmingly consistent
  # team correspondences before treating a name mismatch as explained.
  sig_g <- unique(g[,.(Country,Date,Score,H,A)],by=c("Country","Date","Score","H","A"))
  sig_m <- unique(m[,.(Country,Date,Score,H,A)],by=c("Country","Date","Score","H","A"))
  sig_g <- sig_g[,if(.N==1L) .SD else NULL,by=.(Country,Date,Score)]
  sig_m <- sig_m[,if(.N==1L) .SD else NULL,by=.(Country,Date,Score)]
  pairs <- merge(sig_g,sig_m,by=c("Country","Date","Score"),suffixes=c("Source","Master"))
  evidence <- rbind(pairs[,.(Country,Date,Source=HSource,Target=HMaster)],
                    pairs[,.(Country,Date,Source=ASource,Target=AMaster)])
  evidence <- unique(evidence)
  votes <- evidence[,.(Support=uniqueN(Date)),by=.(Country,Source,Target)]
  votes[,TotalEvidence:=sum(Support),by=.(Country,Source)]
  votes[,Share:=Support/TotalEvidence]
  setorder(votes,Country,Source,-Support)
  suggestions <- votes[, .SD[1],by=.(Country,Source)]
  suggestions[,StemRelated:=mapply(function(s,t) nchar(s)>=5L && nchar(t)>=5L &&
    (grepl(s,t,fixed=TRUE)||grepl(t,s,fixed=TRUE)),Source,Target)]
  suggestions[,AcceptedForComparison:=Support>=5L & Share>=0.95 & (StemRelated | Support>=8L)]
  # A source name already used for a separate master team is never remapped.
  master_names <- unique(rbind(m[,.(Country,Name=H)],m[,.(Country,Name=A)]))
  suggestions[paste(Country,Source,sep="\r") %chin% paste(master_names$Country,master_names$Name,sep="\r") & Source!=Target,
              AcceptedForComparison:=FALSE]
  fwrite(suggestions[Source!=Target],file.path(out,"name_evidence.csv"))
  maps <- suggestions[AcceptedForComparison==TRUE & Source!=Target]
  inferred <- setNames(maps$Target,paste(maps$Country,maps$Source,sep="\r"))
  apply_inferred <- function(c,x) { h<-unname(inferred[paste(c,x,sep="\r")]); x[!is.na(h)]<-h[!is.na(h)]; x }
  g[,`:=`(H=apply_inferred(Country,H),A=apply_inferred(Country,A))]
  g[,MatchedAfterEvidence:=fixture(g) %chin% fixture(m)]
  cat("[3/4] Flagging non-league blocks, conflicts and remaining gaps...\n")
  # Exclude explicitly identified foreign competition blocks, but retain league
  # championship/relegation phases. Other non-numbered rows remain review-only.
  g[,ExplicitCup:=grepl("cup|copa|kubok|karikas|kupa|coupe|pokal|beker",RSSSFSection,ignore.case=TRUE)]
  g[,NumberedLeagueRound:=grepl("^Round [0-9]+(?:$|[[:space:]]|\\[)",RSSSFStage,perl=TRUE)]
  g[,DuplicateRow:=duplicated(fixture(g))]
  m_pair <- paste(m$Country,m$Date,m$H,m$A,sep="\r")
  g_pair <- paste(g$Country,g$Date,g$H,g$A,sep="\r")
  hit <- match(g_pair,m_pair)
  g[,ConflictingScore:=!is.na(hit) & Score!=m$Score[hit]]
  appearances <- unique(c(paste(m$Country,m$Date,m$H,sep="\r"),paste(m$Country,m$Date,m$A,sep="\r")))
  g[,TeamAlreadyPlayed:=paste(Country,Date,H,sep="\r") %chin% appearances |
       paste(Country,Date,A,sep="\r") %chin% appearances]
  g[,Classification:=fcase(MatchedExisting,"ALREADY_PRESENT",MatchedAfterEvidence,"EXPLAINED_BY_NAME_EVIDENCE",
    DuplicateRow,"DUPLICATE_EXTRACTED_ROW",ExplicitCup,"CUP_BLOCK",!NumberedLeagueRound,"NON_NUMBERED_BLOCK_REVIEW",
    ConflictingScore,"SCORE_CONFLICT",TeamAlreadyPlayed,"NAME_OR_DATE_CONFLICT_REVIEW",
    default="POTENTIAL_MISSING_LEAGUE_FIXTURE")]
  fwrite(g[MatchedAfterEvidence==FALSE],file.path(out,"remaining_fixture_review.csv"))
  summary <- g[,.(ExtractedDated=.N,AlreadyPresent=sum(MatchedExisting),
    ExplainedByNames=sum(!MatchedExisting & MatchedAfterEvidence),
    PotentialMissing=sum(Classification=="POTENTIAL_MISSING_LEAGUE_FIXTURE"),
    OtherReview=sum(!MatchedAfterEvidence & Classification!="POTENTIAL_MISSING_LEAGUE_FIXTURE")),by=.(Country,Season)]
  summary[,Priority:=fifelse(PotentialMissing>=20L,"SUBSTANTIAL_GAP",fifelse(PotentialMissing>=5L,"SMALL_GAP","STOP_OR_LOW_PRIORITY"))]
  setorder(summary,-PotentialMissing,Country,Season)
  fwrite(summary,file.path(out,"season_gap_summary.csv"))
  cat("[4/4] Saved reconciliation summary:\n")
  print(g[,.(Rows=.N),by=Classification][order(-Rows)])
  print(summary[PotentialMissing>=20L])
  cat("Comparison-only inferred name mappings: ",nrow(maps),"\n",sep="")
  cat("No aliases or production files changed. No parsing or downloads.\nReport: ",out,"\n",sep="")
}
run_saved_uefa_reconciliation()
