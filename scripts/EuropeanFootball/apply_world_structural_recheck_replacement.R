# Replace, rather than append, the RSSSF rows from non-UEFA source pages that
# passed preview_world_structural_recheck_replacement.R.  This limits the
# change to pages already represented in the production master.
#
# Preview (default):
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/apply_world_structural_recheck_replacement.R")
#
# Apply after checking the preview:
# Sys.setenv(APPLY_WORLD_STRUCTURAL_RECHECK = "1")
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/apply_world_structural_recheck_replacement.R")

suppressPackageStartupMessages({
  library(data.table)
  library(tictoc)
  library(beepr)
})

tictoc::tic("Replace world structural RSSSF pages")
on.exit({ tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE) }, add = TRUE)

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/", mustWork = TRUE
)
audit_dir <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit")
preview_path <- file.path(audit_dir, "structural_recheck_replacement", "eligible_page_replacements.csv")
parsed_path <- file.path(audit_dir, "all_dated_rsssf_games.csv")
master_path <- file.path(root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
if (any(!file.exists(c(preview_path, parsed_path, master_path)))) {
  stop("Missing replacement preview or audit output. Run preview_world_structural_recheck_replacement.R first.")
}

pages <- fread(preview_path, encoding = "UTF-8")
required_pages <- c("SourceFile", "Country", "Season", "ReplacementEligibility")
if (!all(required_pages %in% names(pages))) stop("Replacement preview has an unexpected schema.")
pages <- unique(pages[ReplacementEligibility == "eligible_for_page_replacement",
                      .(SourceFile, TargetCountry = Country, TargetSeason = Season)])
if (!nrow(pages)) stop("The replacement preview contains no eligible pages.")

master <- fread(master_path, encoding = "UTF-8")
master_columns <- names(master)
required_master <- c("Season", "Country", "Competition", "CompetitionType", "Tier", "League",
                     "Date", "Home", "Away", "Result", "Score", "Source", "SourceFile")
if (!all(required_master %in% master_columns)) stop("Production master has an unexpected schema.")

fresh <- fread(parsed_path, encoding = "UTF-8")
required_fresh <- c("Date", "Home", "Away", "Score", "Result", "CompetitionType", "SourceFile", "Country", "Season")
if (!all(required_fresh %in% names(fresh))) stop("World audit games file has an unexpected schema.")
if (!"RSSSFPhase" %in% names(fresh)) fresh[, RSSSFPhase := ""]
fresh <- merge(fresh, pages, by = "SourceFile", all = FALSE)
fresh <- fresh[
  CompetitionType == "league" &
    Country == TargetCountry & Season == TargetSeason &
    !is.na(Date) & nzchar(Home) & nzchar(Away) & Home != Away &
    Result %chin% c("1-0", "0.5-0.5", "0-1") &
    !tolower(trimws(RSSSFPhase)) %chin% c("promotion", "relegation")
]
if (!nrow(fresh)) stop("No valid replacement rows were found for the eligible pages.")

# Keep exactly one copy of an identical source fixture, but never resolve a
# same-day contradictory score silently.
conflict_key <- c("Season", "Country", "Date", "Home", "Away")
conflicts <- fresh[, .(Scores = uniqueN(Score)), by = conflict_key][Scores > 1L]
if (nrow(conflicts)) stop("Replacement pages contain same-day score conflicts.")
fresh <- unique(fresh, by = c(conflict_key, "Score"))

new_rows <- copy(fresh)
for (column in setdiff(master_columns, names(new_rows))) new_rows[, (column) := NA]
new_rows <- new_rows[, ..master_columns]

remove_row <- master$Source == "rsssf" & master$SourceFile %chin% pages$SourceFile
removed <- master[remove_row]
retained <- master[!remove_row]

# Do not create a duplicate if the same dated fixture is preserved from a
# different source.  Keep the retained source in that rare situation.
fixture_key_columns <- c("Date", "Home", "Away", "Score", "CompetitionType")
key_of <- function(x) do.call(paste, c(x[, ..fixture_key_columns], sep = "\r"))
retained_keys <- key_of(retained)
new_rows <- new_rows[!key_of(new_rows) %chin% retained_keys]
if (anyDuplicated(key_of(new_rows))) {
  stop("Replacement batch contains duplicated fixture keys.")
}
retained_duplicate_keys <- sum(duplicated(retained_keys))
result <- rbindlist(list(retained, new_rows), use.names = TRUE, fill = FALSE)
setorder(result, Date, Country, CompetitionType, Home, Away)

replacement_summary <- merge(
  removed[, .(RemovedRows = .N), by = SourceFile],
  new_rows[, .(AddedRows = .N, FirstDate = min(as.IDate(Date)), LastDate = max(as.IDate(Date))), by = SourceFile],
  by = "SourceFile", all = TRUE
)
replacement_summary <- merge(pages, replacement_summary, by = "SourceFile", all.x = TRUE)
for (nm in c("RemovedRows", "AddedRows")) replacement_summary[is.na(get(nm)), (nm) := 0L]
replacement_summary[, NetRows := AddedRows - RemovedRows]
setorder(replacement_summary, TargetCountry, TargetSeason)

cat("\nWorld structural page replacement:\n")
print(replacement_summary[, .(
  Pages = .N, RemovedRows = sum(RemovedRows), AddedRows = sum(AddedRows), NetRows = sum(NetRows)
)])
print(replacement_summary[, .(Country = TargetCountry, Season = TargetSeason, RemovedRows, AddedRows, NetRows, SourceFile)])

out_dir <- file.path(audit_dir, "structural_recheck_replacement")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(replacement_summary, file.path(out_dir, "applied_page_replacement_summary.csv"), na = "")

if (!identical(Sys.getenv("APPLY_WORLD_STRUCTURAL_RECHECK"), "1")) {
  message("Preview only. Production master unchanged.")
  message("To apply this exact replacement, set APPLY_WORLD_STRUCTURAL_RECHECK = '1' and run again.")
  quit(save = "no")
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- sub("[.]csv$", paste0("_before_world_structural_recheck_", stamp, ".csv"), master_path)
temporary_path <- paste0(master_path, ".tmp")
if (!file.copy(master_path, backup_path, overwrite = FALSE)) stop("Could not create production-master backup.")
fwrite(result, temporary_path, na = "")
check <- fread(temporary_path, select = fixture_key_columns)
if (nrow(check) != nrow(result)) stop("Temporary master verification failed.")
if (sum(duplicated(key_of(check))) != retained_duplicate_keys) {
  stop("Temporary master changed the count of pre-existing duplicate fixture keys.")
}
if (!file.copy(temporary_path, master_path, overwrite = TRUE)) stop("Could not replace the production master.")
unlink(temporary_path)

message("Production master updated: ", nrow(master), " -> ", nrow(result), " rows.")
message("Backup: ", backup_path)
message("Replacement summary: ", file.path(out_dir, "applied_page_replacement_summary.csv"))
