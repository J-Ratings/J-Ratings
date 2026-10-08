# Consolidated recovery status. Counts are evidence inventories, not completeness percentages.
audit_remaining_league_gaps <- function() {
  suppressPackageStartupMessages(library(data.table))
  started<-Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  base<-"EuropeanFootball/pipeline_data"
  batch<-file.path(base,"Manual_Sources/Season_2025_26")
  out<-file.path(batch,"remaining_league_gaps");dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Reading league queue, scores, date matches and held decisions...\n")
  q<-fread(file.path(batch,"source_discovery/recovery_queue.csv"),encoding="UTF-8")
  w<-fread(file.path(batch,"score_date_matching/wikipedia_results.csv"),encoding="UTF-8")
  f<-fread(file.path(batch,"club_label_recheck/fixture_review.csv"),encoding="UTF-8")
  d<-fread(file.path(batch,"import_validation/all_decisions.csv"),encoding="UTF-8")
  selected<-fread(file.path(batch,"score_date_matching/selected_pages.csv"),encoding="UTF-8")
  cat("[2/4] Checking whether the latest preview was imported...\n")
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),
    select=c("Country","Date","Score","SourcePage","Source"),encoding="UTF-8",showProgress=FALSE)
  mk<-paste(m$Country,m$Date,m$Score,m$SourcePage,sep="|")
  d[,CurrentStatus:=Validation]
  pending<-d[Validation=="VALIDATED_IMPORT_PREVIEW"]
  imported<-pending[ paste(Country,Date,Score,SourceURL,sep="|") %chin% mk,CandidateID]
  d[CandidateID %in% imported,CurrentStatus:="IMPORTED_LATEST_PREVIEW"]
  cat("  Latest preview imported:",length(imported),"/",nrow(pending),"\n")
  cat("[3/4] Separating missing dates, identities and scope problems...\n")
  scores<-w[,.(ExtractedWikipediaScores=.N),by=Country]
  dates<-f[,.(SavedDatedCandidates=.N,UniqueScoreDateCandidates=sum(Decision=="UNIQUE_SCORE_DATE_MATCH_REVIEW"),
    UnmatchedClubLabels=sum(Decision=="CLUB_LABEL_REVIEW"),
    RepeatedFixtureStageCases=sum(Decision=="REPEATED_FIXTURE_STAGE_REVIEW"),
    ScoreOrStageCases=sum(Decision=="SCORE_OR_STAGE_REVIEW")),by=Country]
  statuses<-d[,.(PresentOrImported=sum(CurrentStatus %chin% c("ALREADY_PRESENT","IMPORTED_LATEST_PREVIEW")),
    ValidatedAwaitingImport=sum(CurrentStatus=="VALIDATED_IMPORT_PREVIEW"),
    IdentityHeld=sum(CurrentStatus %chin% c("UNKNOWN_OR_CHANGED_CLUB_IDENTITY_REVIEW","COLLIDING_EXISTING_IDENTITIES_REVIEW")),
    OtherHeld=sum(!CurrentStatus %chin% c("ALREADY_PRESENT","IMPORTED_LATEST_PREVIEW","VALIDATED_IMPORT_PREVIEW",
      "UNKNOWN_OR_CHANGED_CLUB_IDENTITY_REVIEW","COLLIDING_EXISTING_IDENTITIES_REVIEW"))),by=Country]
  report<-merge(q,scores,by="Country",all.x=TRUE)
  report<-merge(report,dates,by="Country",all.x=TRUE)
  report<-merge(report,statuses,by="Country",all.x=TRUE)
  report<-merge(report,unique(selected[,.(Country,WikipediaPage=Title,PageDecision)]),by="Country",all.x=TRUE,allow.cartesian=TRUE)
  report[,Recommendation:=fcase(
    Tier!=1L,"SEPARATE_LOWER_TIER_REVIEW",
    is.na(ExtractedWikipediaScores),"MISSING_SCORE_SOURCE_OR_EXTRACTION_REVIEW",
    is.na(SavedDatedCandidates),"MISSING_DATED_SOURCE_REVIEW",
    ValidatedAwaitingImport>0L,"IMPORT_VALIDATED_BATCH_FIRST",
    IdentityHeld>=20L,"MATERIAL_IDENTITY_BATCH",
    UnmatchedClubLabels>=20L,"MATERIAL_SOURCE_LABEL_BATCH",
    OtherHeld>=20L,"MATERIAL_DATE_OR_SCOPE_BATCH",
    default="CHECK_SEASON_COMPLETENESS_BEFORE_CHECKPOINT")]
  report[,CheckpointStatus:="NOT_CERTIFIED_COMPLETE"]
  # Country aggregates must not be mistaken for tier-specific evidence.
  report[Tier!=1L,c("ExtractedWikipediaScores","SavedDatedCandidates","UniqueScoreDateCandidates",
    "UnmatchedClubLabels","RepeatedFixtureStageCases","ScoreOrStageCases","PresentOrImported",
    "ValidatedAwaitingImport","IdentityHeld","OtherHeld"):=NA_integer_]
  setorder(report,Recommendation,Country,Tier)
  fwrite(report,file.path(out,"league_gap_report.csv"),na="")
  fwrite(d[!CurrentStatus %chin% c("ALREADY_PRESENT","IMPORTED_LATEST_PREVIEW")],file.path(out,"remaining_matched_fixtures.csv"),na="")
  fwrite(f[Decision!="UNIQUE_SCORE_DATE_MATCH_REVIEW"],file.path(out,"unmatched_date_source_fixtures.csv"),na="")
  cat("[4/4] League recovery priorities:\n")
  print(report[Tier==1L,.N,by=Recommendation])
  print(head(report[Tier==1L & !is.na(IdentityHeld)][order(-IdentityHeld),
    .(Country,TargetSeason,PresentOrImported,IdentityHeld,UnmatchedClubLabels,OtherHeld)],25L))
  cat("Missing from a saved extraction does not prove absent online. No league is certified complete by these counts.\n")
  cat("No downloads or production changes. Report:",normalizePath(out,winslash="/"),"\n")
}
audit_remaining_league_gaps()
