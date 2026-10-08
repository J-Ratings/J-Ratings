# Replace the historical domestic Tier 1 data for the audited UEFA countries
# with the RSSSF-only rebuild. The 2025/26 (or calendar 2025) season and all
# continental matches are deliberately left untouched.
#
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/import_rsssf_uefa_rebuild.R")
#
# Optional preview:
# Sys.setenv(RSSSF_UEFA_IMPORT_DRY_RUN="1")

required <- c("data.table", "tictoc", "beepr")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install these packages first: ", paste(missing_packages, collapse = ", "))
}

library(data.table)

import_rsssf_uefa_rebuild <- function() {
  tictoc::tic("Import RSSSF-only UEFA rebuild")
  on.exit({
    tictoc::toc()
    try(suppressWarnings(beepr::beep()), silent = TRUE)
  }, add = TRUE)

  root <- normalizePath(
    Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
    winslash = "/", mustWork = TRUE
  )
  rebuild_dir <- file.path(
    root,
    "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_UEFA_Rebuild"
  )
  candidate_path <- file.path(rebuild_dir, "dated_candidate.csv")
  audit_path <- file.path(rebuild_dir, "season_audit.csv")
  master_path <- file.path(
    root,
    "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv"
  )
  stopifnot(file.exists(candidate_path), file.exists(audit_path), file.exists(master_path))

  candidates <- fread(candidate_path, encoding = "UTF-8")
  audit <- fread(audit_path, encoding = "UTF-8")
  master <- fread(master_path, encoding = "UTF-8")
  master_columns <- names(master)

  required_candidate <- c(
    "Season", "Country", "Competition", "CompetitionType", "Tier", "League",
    "Date", "Home", "Away", "Result", "Score", "SourcePage", "Stage",
    "DateApprox", "SourceFile"
  )
  if (!all(required_candidate %in% names(candidates))) {
    stop("The RSSSF candidate has an unexpected schema.")
  }
  required_master <- c(
    "Season", "Country", "CompetitionType", "Tier", "Date", "Home", "Away",
    "Result", "Score", "Source"
  )
  if (!all(required_master %in% master_columns)) stop("The production master has an unexpected schema.")

  rebuild_countries <- sort(unique(audit$Country))
  if (length(rebuild_countries) != 38L) {
    stop("Expected 38 audited countries, found ", length(rebuild_countries), ".")
  }

  candidates[, StartYear := suppressWarnings(as.integer(substr(Season, 1L, 4L)))]
  candidates[, Date := as.IDate(Date)]
  candidates[, Tier := as.integer(Tier)]
  candidates <- candidates[
    Country %chin% rebuild_countries &
      CompetitionType == "league" & Tier == 1L &
      !is.na(StartYear) & StartYear <= 2024L
  ]
  if (!nrow(candidates)) stop("No eligible RSSSF candidate rows were found.")
  if (anyNA(candidates$Date)) stop("An RSSSF candidate has no date.")
  if (any(!candidates$Result %chin% c("1-0", "0.5-0.5", "0-1"))) {
    stop("An RSSSF candidate has an invalid Result value.")
  }
  if (any(candidates$Home == candidates$Away)) stop("An RSSSF candidate has identical home and away teams.")

  # Exact duplicated source lines occur twice in two cached RSSSF pages. They
  # are the same fixture, so retain one copy. A same-day fixture with conflicting
  # scores is never silently resolved.
  fixture_columns <- c("Season", "Country", "Date", "Home", "Away", "Score")
  conflict_columns <- c("Season", "Country", "Date", "Home", "Away")
  conflicts <- candidates[, .(Scores = uniqueN(Score)), by = conflict_columns][Scores > 1L]
  if (nrow(conflicts)) stop("RSSSF candidate contains same-day score conflicts.")
  candidate_rows_before_dedup <- nrow(candidates)
  candidates <- unique(candidates, by = fixture_columns)

  new_rows <- data.table(
    Season = candidates$Season,
    Country = candidates$Country,
    Competition = candidates$Competition,
    CompetitionType = "league",
    Tier = 1L,
    League = candidates$League,
    Date = candidates$Date,
    Home = candidates$Home,
    Away = candidates$Away,
    Result = candidates$Result,
    Score = candidates$Score,
    Source = "rsssf",
    SourcePage = candidates$SourcePage,
    Stage = candidates$Stage,
    DateApprox = as.logical(candidates$DateApprox),
    SourceFile = candidates$SourceFile
  )
  for (column in setdiff(master_columns, names(new_rows))) new_rows[, (column) := NA]
  new_rows <- new_rows[, ..master_columns]

  master[, ReplacementStartYear := suppressWarnings(as.integer(substr(Season, 1L, 4L)))]
  replace_row <- master$Country %chin% rebuild_countries &
    master$CompetitionType == "league" &
    suppressWarnings(as.integer(master$Tier)) == 1L &
    !is.na(master$ReplacementStartYear) & master$ReplacementStartYear <= 2024L

  removed <- master[replace_row]
  retained <- master[!replace_row]
  retained[, ReplacementStartYear := NULL]
  removed[, ReplacementStartYear := NULL]

  result <- rbindlist(list(retained, new_rows), use.names = TRUE, fill = FALSE)
  setorder(result, Date, Country, Tier, Home, Away)

  result_keys <- do.call(paste, c(result[, ..fixture_columns], sep = "\r"))
  if (anyDuplicated(result_keys)) stop("The proposed master contains duplicated fixture keys.")

  replacement_summary <- merge(
    removed[, .(RemovedRows = .N), by = Country],
    new_rows[, .(
      AddedRows = .N,
      FirstSeason = Season[which.min(Date)],
      LastSeason = Season[which.max(Date)],
      FirstDate = min(Date),
      LastDate = max(Date)
    ), by = Country],
    by = "Country", all = TRUE
  )
  setorder(replacement_summary, Country)
  replacement_summary[is.na(RemovedRows), RemovedRows := 0L]
  replacement_summary[is.na(AddedRows), AddedRows := 0L]

  preserved_current <- retained[
    Country %chin% rebuild_countries & CompetitionType == "league" &
      suppressWarnings(as.integer(Tier)) == 1L &
      suppressWarnings(as.integer(substr(Season, 1L, 4L))) >= 2025L,
    .N
  ]

  print(replacement_summary)
  message(
    "Would replace ", nrow(removed), " historical rows with ", nrow(new_rows),
    " RSSSF rows across ", uniqueN(new_rows$Country), " countries. " ,
    "Preserved current-season rows: ", preserved_current, "."
  )
  if (candidate_rows_before_dedup != nrow(candidates)) {
    message("Removed ", candidate_rows_before_dedup - nrow(candidates),
            " exact duplicate RSSSF rows.")
  }

  if (identical(Sys.getenv("RSSSF_UEFA_IMPORT_DRY_RUN"), "1")) {
    message("Dry run: production master unchanged.")
    return(invisible(replacement_summary))
  }

  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- sub(
    "[.]csv$", paste0("_before_rsssf_uefa_rebuild_", stamp, ".csv"), master_path
  )
  temporary_path <- paste0(master_path, ".tmp")
  log_path <- file.path(rebuild_dir, paste0("master_import_", stamp, ".csv"))

  if (!file.copy(master_path, backup_path, overwrite = FALSE)) {
    stop("Could not create the production-master backup.")
  }
  fwrite(result, temporary_path, na = "")
  check <- fread(temporary_path, select = fixture_columns)
  if (nrow(check) != nrow(result)) stop("Temporary master verification failed.")
  if (anyDuplicated(do.call(paste, c(check, sep = "\r")))) {
    stop("Temporary master contains duplicated fixture keys.")
  }
  if (!file.copy(temporary_path, master_path, overwrite = TRUE)) {
    stop("Could not replace the production master. Backup: ", backup_path)
  }
  unlink(temporary_path)
  fwrite(replacement_summary, log_path, na = "")

  message("Master rows: ", nrow(master), " -> ", nrow(result))
  message("Backup: ", backup_path)
  message("Import log: ", log_path)
  invisible(replacement_summary)
}

import_rsssf_uefa_rebuild()
