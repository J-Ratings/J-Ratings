# Import the 24 audited UEFA top divisions that are not yet in the production master.
#
# Results come from Wikipedia and dates from RSSSF. Only rows whose DateStatus is
# "matched" enter the master. The starting seasons below are the earliest suffixes
# that satisfy the agreed rule after unresolved team identities are excluded:
# no run of more than two consecutive seasons below 95% coverage.

required <- c("data.table", "tictoc", "beepr")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install these packages first: ", paste(missing_packages, collapse = ", "))
}

library(data.table)
import_remaining_uefa_leagues <- function() {
tictoc::tic("Import remaining UEFA leagues")
on.exit({
  tictoc::toc()
  try(suppressWarnings(beepr::beep()), silent = TRUE)
}, add = TRUE)

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/", mustWork = TRUE
)

audit_dir <- file.path(
  root,
  "EuropeanFootball/pipeline_data/Manual_Sources/Wikipedia_RSSSF_Alias_Audit"
)
candidate_path <- file.path(audit_dir, "dated_candidate.csv")
coverage_path <- file.path(audit_dir, "parser_coverage_by_season.csv")
master_path <- file.path(
  root,
  "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv"
)

stopifnot(file.exists(candidate_path), file.exists(coverage_path), file.exists(master_path))

selection <- data.table(
  Country = c(
    "Albania", "Andorra", "Armenia", "Azerbaijan", "Belarus", "Croatia",
    "Cyprus", "Estonia", "Faroe Islands", "Finland", "Georgia", "Gibraltar",
    "Kazakhstan", "Kosovo", "Latvia", "Lithuania", "Moldova", "Montenegro",
    "North Macedonia", "Northern Ireland", "Republic of Ireland", "Slovakia",
    "Slovenia", "Wales"
  ),
  StartYear = c(
    2010L, 2021L, 2010L, 2010L, 2010L, 2010L,
    2010L, 2010L, 2011L, 2010L, 2010L, 2011L,
    2010L, 2012L, 2015L, 2011L, 2010L, 2010L,
    2013L, 2010L, 2010L, 2010L, 2010L, 2011L
  ),
  EndYear = 2024L
)

coverage <- fread(coverage_path)
coverage[, StartYear := as.integer(StartYear)]
coverage[, MeetsAdjusted95 :=
  !is.na(CoverageExcludingUnresolvedIdentities) &
  CoverageExcludingUnresolvedIdentities >= 95 &
  WikipediaGames > 0]

# Verify that the saved audit still supports every configured range.
rule_check <- coverage[selection, on = "Country", nomatch = 0L][
  StartYear >= i.StartYear & StartYear <= EndYear
][order(Country, StartYear), {
  bad_runs <- rle(!MeetsAdjusted95)
  .(
    FirstSeason = first(Season),
    LastSeason = last(Season),
    Seasons = .N,
    SeasonsBelow95 = sum(!MeetsAdjusted95),
    LongestBelow95Run = if (any(bad_runs$values))
      max(bad_runs$lengths[bad_runs$values]) else 0L
  )
}, by = Country]

if (nrow(rule_check) != nrow(selection) || any(rule_check$LongestBelow95Run > 2L)) {
  print(rule_check)
  stop("The saved audit no longer supports the configured import ranges.")
}

candidates <- fread(candidate_path)
candidates[, StartYear := as.integer(substr(Season, 1L, 4L))]
import <- candidates[selection, on = "Country", nomatch = 0L][
  StartYear >= i.StartYear & StartYear <= EndYear & DateStatus == "matched"
]

if (!nrow(import)) stop("No eligible dated candidate rows were found.")
if (anyNA(import$Date)) stop("A matched candidate has no date.")
if (!setequal(unique(import$Country), selection$Country)) {
  stop("At least one selected country has no dated candidate rows.")
}

master <- fread(master_path)
master_columns <- names(master)

new_rows <- data.table(
  Season = import$Season,
  Country = import$Country,
  Competition = import$Competition,
  CompetitionType = "league",
  Tier = 1L,
  League = import$League,
  Date = as.IDate(import$Date),
  Home = import$Home,
  Away = import$Away,
  Result = import$Result,
  Score = import$Score,
  Source = import$Source,
  SourcePage = import$SourcePage,
  Stage = import$Stage,
  DateApprox = FALSE,
  SourceFile = import$DateSourcePage
)

for (column in setdiff(master_columns, names(new_rows))) new_rows[, (column) := NA]
new_rows <- new_rows[, ..master_columns]

# Exact fixture keys make the script safe to rerun without duplicating games.
fixture_columns <- c("Season", "Country", "Date", "Home", "Away", "Score")
new_rows <- unique(new_rows, by = fixture_columns)
existing_keys <- do.call(paste, c(master[, ..fixture_columns], sep = "\r"))
new_keys <- do.call(paste, c(new_rows[, ..fixture_columns], sep = "\r"))
new_rows <- new_rows[!new_keys %chin% existing_keys]

if (!nrow(new_rows)) {
  message("Nothing to add: all selected dated fixtures are already in the master.")
  print(rule_check)
  return(invisible(new_rows))
}

result <- rbindlist(list(master, new_rows), use.names = TRUE, fill = FALSE)
setorder(result, Date, Country, Tier, Home, Away)

if (identical(Sys.getenv("UEFA_IMPORT_DRY_RUN"), "1")) {
  print(new_rows[, .(
    AddedRows = .N,
    FirstSeason = Season[which.min(Date)],
    LastSeason = Season[which.max(Date)]
  ), by = Country][order(Country)])
  message("Dry run: would add ", nrow(new_rows), " dated matches across ",
          uniqueN(new_rows$Country), " countries. Master unchanged.")
  return(invisible(new_rows))
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- sub("[.]csv$", paste0("_before_remaining_uefa_", stamp, ".csv"), master_path)
temporary_path <- paste0(master_path, ".tmp")

if (!file.copy(master_path, backup_path, overwrite = FALSE)) {
  stop("Could not create the master backup: ", backup_path)
}
fwrite(result, temporary_path, na = "")
check <- fread(temporary_path, select = fixture_columns)
if (nrow(check) != nrow(result)) stop("Temporary master verification failed.")
if (!file.copy(temporary_path, master_path, overwrite = TRUE)) {
  stop("Could not replace the production master. Backup: ", backup_path)
}
unlink(temporary_path)

import_summary <- new_rows[, .(
  AddedRows = .N,
  FirstSeason = Season[which.min(Date)],
  LastSeason = Season[which.max(Date)],
  FirstDate = min(Date),
  LastDate = max(Date)
), by = Country][order(Country)]

print(import_summary)
message("Added ", nrow(new_rows), " dated matches across ",
        uniqueN(new_rows$Country), " countries.")
message("Master rows: ", nrow(master), " -> ", nrow(result))
message("Backup: ", backup_path)

invisible(new_rows)
}

import_remaining_uefa_leagues()
