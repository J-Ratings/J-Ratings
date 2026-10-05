# Admit substantial, well-dated historical top-flight pages that were held by
# the worldwide structural review because they had no existing master rows.
# Tiny fragments, unaffiliated territories, mixed identities, and modern pages
# outside the federation reference remain excluded.
#
# Preview (default):
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/admit_world_historical_structural_pages.R")
#
# Apply:
# Sys.setenv(APPLY_WORLD_HISTORICAL_ADMISSIONS = "1")
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/admit_world_historical_structural_pages.R")

suppressPackageStartupMessages({
  library(data.table)
  library(tictoc)
  library(beepr)
})

tictoc::tic("Admit world historical RSSSF pages")
on.exit({ tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE) }, add = TRUE)

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/", mustWork = TRUE
)
audit_dir <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit")
held_path <- file.path(audit_dir, "structural_recheck_replacement", "held_page_replacements.csv")
games_path <- file.path(audit_dir, "all_dated_rsssf_games.csv")
seed_path <- file.path(root, "EuropeanFootball/pipeline_data/Reference/non_uefa_country_seeds.csv")
master_path <- file.path(root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
required_paths <- c(held_path, games_path, seed_path, master_path)
if (any(!file.exists(required_paths))) stop("Missing input(s):\n", paste(required_paths[!file.exists(required_paths)], collapse = "\n"))

country_alias <- c(
  "Hongkong" = "Hong Kong", "Macao" = "Macau", "East Timor" = "Timor-Leste",
  "Congo-Brazzaville" = "Congo", "Congo-Kinshasa" = "DR Congo",
  "Guinea Bissau" = "Guinea-Bissau", "French Guyana" = "French Guiana",
  "US Virgin Islands" = "United States Virgin Islands", "Surinam" = "Suriname",
  "Central African Republic (Bangui)" = "Central African Republic"
)

held <- fread(held_path, encoding = "UTF-8")
held <- held[ReplacementEligibility == "not_in_master_report_only"]
held[, CanonicalCountry := Country]
held[Country %chin% names(country_alias), CanonicalCountry := unname(country_alias[Country])]
held[, StartYear := suppressWarnings(as.integer(substr(Season, 1L, 4L)))]

seeds <- fread(seed_path, encoding = "UTF-8")
members <- unique(seeds[suppressWarnings(as.integer(Tier)) == 1L & Confederation != "UEFA",
                        .(CanonicalCountry = Country, SeedConfederation = Confederation)])

# These rules deliberately favour evidence over volume. A source page must be
# an affiliated association, contain at least 20 dated league results, and date
# at least 95% of the played results that the parser found.
targets <- merge(held, members, by = "CanonicalCountry", all = FALSE)
targets <- targets[
  IdentityMatchedRows >= 20L &
    DatedShareOfRSSSFPercent >= 95 &
    StartYear < 2010L
]
targets <- unique(targets[, .(
  SourceFile, SourceCountry = Country, CanonicalCountry, TargetSeason = Season,
  Confederation = SeedConfederation, ExpectedRows = IdentityMatchedRows,
  DatedShareOfRSSSFPercent
)])
if (!nrow(targets)) stop("No historical pages passed the admission rules.")

games <- fread(games_path, encoding = "UTF-8")
required_games <- c("SourceFile", "Country", "Season", "Date", "Home", "Away", "Score", "Result", "CompetitionType")
if (!all(required_games %in% names(games))) stop("Worldwide games output has an unexpected schema.")
if (!"RSSSFPhase" %in% names(games)) games[, RSSSFPhase := ""]
games <- merge(games, targets, by = "SourceFile", all = FALSE)
games <- games[
  Country == SourceCountry & Season == TargetSeason & CompetitionType == "league" &
    !is.na(Date) & nzchar(Home) & nzchar(Away) & Home != Away &
    Result %chin% c("1-0", "0.5-0.5", "0-1") &
    !tolower(trimws(RSSSFPhase)) %chin% c("promotion", "relegation")
]
if (!nrow(games)) stop("No valid dated fixtures remained for the admitted pages.")
games[, `:=`(Country = CanonicalCountry, Season = TargetSeason)]

master <- fread(master_path, encoding = "UTF-8")
master_columns <- names(master)
required_master <- c("Country", "Competition", "CompetitionType", "Tier", "League", "Date", "Home", "Away", "Score", "Source")
if (!all(required_master %in% master_columns)) stop("Production master has an unexpected schema.")

# Reuse each country's established competition identifiers and display name.
metadata <- master[
  Country %chin% targets$CanonicalCountry & CompetitionType == "league" &
    suppressWarnings(as.integer(Tier)) == 1L,
  .N, by = .(Country, Competition, League)
][order(Country, -N)][, .SD[1L], by = Country]
if (metadata[, uniqueN(Country)] != targets[, uniqueN(CanonicalCountry)]) {
  stop("One or more admitted countries have no established top-flight metadata in the master.")
}
games <- metadata[games, on = "Country"]
games[, `:=`(CompetitionType = "league", Tier = 1L, Source = "rsssf")]
games[, c("i.Competition", "i.League") := NULL]

conflict_key <- c("Season", "Country", "Date", "Home", "Away")
conflicts <- games[, .(Scores = uniqueN(Score)), by = conflict_key][Scores > 1L]
if (nrow(conflicts)) stop("Historical admission contains same-day score conflicts.")
games <- unique(games, by = c(conflict_key, "Score"))

new_rows <- copy(games)
for (column in setdiff(master_columns, names(new_rows))) new_rows[, (column) := NA]
new_rows <- new_rows[, ..master_columns]
fixture_columns <- c("Date", "Home", "Away", "Score", "CompetitionType")
key_of <- function(x) do.call(paste, c(x[, ..fixture_columns], sep = "\r"))
existing_keys <- key_of(master)
new_rows <- new_rows[!key_of(new_rows) %chin% existing_keys]
if (anyDuplicated(key_of(new_rows))) stop("Historical admission contains duplicate fixture keys.")
if (!nrow(new_rows)) stop("All proposed historical fixtures are already in the master.")

result <- rbindlist(list(master, new_rows), use.names = TRUE, fill = FALSE)
setorder(result, Date, Country, CompetitionType, Home, Away)
summary <- new_rows[, .(
  AddedRows = .N, FirstDate = min(as.IDate(Date)), LastDate = max(as.IDate(Date)),
  Teams = uniqueN(c(Home, Away))
), by = .(Country, Season, SourceFile)][order(Country, Season)]

out_dir <- file.path(audit_dir, "historical_structural_admissions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(summary, file.path(out_dir, "admission_summary.csv"), na = "")
cat("\nHistorical worldwide admissions:\n")
print(summary)
message("Would add ", nrow(new_rows), " matches from ", nrow(summary), " season pages.")

if (!identical(Sys.getenv("APPLY_WORLD_HISTORICAL_ADMISSIONS"), "1")) {
  message("Preview only. Production master unchanged.")
  quit(save = "no")
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- sub("[.]csv$", paste0("_before_world_historical_admissions_", stamp, ".csv"), master_path)
temporary_path <- paste0(master_path, ".tmp")
if (!file.copy(master_path, backup_path, overwrite = FALSE)) stop("Could not create production-master backup.")
fwrite(result, temporary_path, na = "")
check <- fread(temporary_path, select = fixture_columns)
if (nrow(check) != nrow(result)) stop("Temporary master verification failed.")
old_path <- paste0(master_path, ".replace-old")
if (file.exists(old_path)) stop("Stale replacement file exists: ", old_path)
if (!file.rename(master_path, old_path)) stop("Could not move the existing production master aside.")
if (!file.rename(temporary_path, master_path)) {
  file.rename(old_path, master_path)
  stop("Could not install the new production master; the original was restored.")
}
unlink(old_path)
message("Production master updated: ", nrow(master), " -> ", nrow(result), " rows.")
message("Backup: ", backup_path)
message("Admission summary: ", file.path(out_dir, "admission_summary.csv"))
