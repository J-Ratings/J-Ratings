# Build the recovery queue from the saved season-presence audit. No master writes.
prepare_target_recovery <- function() {
  suppressPackageStartupMessages(library(data.table))
  base <- "EuropeanFootball/pipeline_data/Manual_Sources"
  input <- file.path(base,"Global/season_2025_26_presence")
  out <- file.path(base,"Season_2025_26/source_discovery")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/3] Reading every covered league, including partial seasons...\n")
  x <- fread(file.path(input,"league_status.csv"))
  recent <- fread(file.path(input,"recent_season_counts.csv"))
  recent[,StartYear:=suppressWarnings(as.integer(substr(Season,1,4)))]
  previous <- recent[StartYear %in% c(2023L,2024L),
    .(LargestRecentRecordedSeason=max(CompletedMatches)),by=.(Country,Tier)]
  x <- merge(x,previous,by=c("Country","Tier"),all.x=TRUE)
  x[,Priority:=fifelse(Status=="SEASON_CONVENTION_REVIEW","CONFIRM_TARGET_SEASON",
    fifelse(TargetCompletedMatches==0L,"FIND_MISSING_SEASON",
      fifelse(!is.na(LargestRecentRecordedSeason)&TargetCompletedMatches<LargestRecentRecordedSeason,
        "CHECK_POSSIBLY_PARTIAL_SEASON","VERIFY_PRESENT_SEASON")))]
  x[,`:=`(SourceStatus="NOT_CHECKED", SourceURL="", ScopeNote=fifelse(Tier==1L,
    "Country/tier unit; regional or split stages require separate validation",
    "Existing lower-tier unit; source competition requires explicit tier confirmation"))]
  x[Country=="Wales"&Tier==1L,`:=`(SourceStatus="PILOT_192_SCORES_MATCHED",
    SourceURL="https://www.transfermarkt.co.uk/cymru-premier/gesamtspielplan/wettbewerb/WAL1/saison_id/2025")]
  setorder(x,Confederation,Country,Tier)
  cat("[2/3] Writing recovery queue and season-convention holds...\n")
  fwrite(x,file.path(out,"recovery_queue.csv"),na="")
  fwrite(x[Priority=="CONFIRM_TARGET_SEASON"],file.path(out,"season_convention_holds.csv"),na="")
  fwrite(x[,.(Units=.N),by=.(Confederation,Priority)],file.path(out,"queue_summary.csv"))
  writeLines(c("Scope: calendar 2025 or split 2025/26 only; exclude 2026 and 2026/27.",
    "All 203 saved country/tier units are retained, including present and uncertain seasons.",
    "LargestRecentRecordedSeason is a warning baseline, NOT an expected-games denominator or coverage percentage.",
    "A smaller count can be legitimate (format change, cancellation); a larger count does not prove completeness.",
    "Source discovery confirms season labels and whether dated played results can be extracted; it does not approve imports.",
    "No fuzzy club merges, production master changes or Elo/JSON runs.",
    "Run check_2025_26_dated_source_coverage.R for the longer cached, sequential source check."),file.path(out,"READ_ME.txt"))
  cat("[3/3] Queue overview:\n"); print(x[,.N,by=Priority])
  cat("Report:",normalizePath(out,winslash="/"),"\nProduction unchanged.\n")
}
prepare_target_recovery()
