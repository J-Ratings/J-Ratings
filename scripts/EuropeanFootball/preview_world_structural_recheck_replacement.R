# Compare the newly reparsed non-UEFA RSSSF structural-review pages against
# the rows in the production master.  This is deliberately read-only: it
# prepares evidence for a page-by-page replacement import rather than
# appending a second copy of the same pages.
#
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/preview_world_structural_recheck_replacement.R")
#
# No master, aliases, or site files are changed.

suppressPackageStartupMessages({
  library(data.table)
  library(tictoc)
  library(beepr)
})

tictoc::tic("Preview world structural RSSSF replacements")
on.exit({ tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE) }, add = TRUE)

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/", mustWork = TRUE
)
audit_dir <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit")
audit_path <- file.path(audit_dir, "season_audit.csv")
parsed_path <- file.path(audit_dir, "all_dated_rsssf_games.csv")
master_path <- file.path(root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
if (any(!file.exists(c(audit_path, parsed_path, master_path)))) {
  stop("Missing the world audit output or production master. Run rsssf_world_audit.R first.")
}

audit <- fread(audit_path, encoding = "UTF-8")
if (!"ReferenceGames" %in% names(audit)) audit[, ReferenceGames := NA_integer_]
if (!"RSSSFPlayedResults" %in% names(audit) && "RSSSFResults" %in% names(audit)) {
  audit[, RSSSFPlayedResults := RSSSFResults]
}
targets <- unique(audit[grepl("structural_review", QualityStatus) & !is.na(SourceFile),
  .(Confederation, Country, Season, SourceFile, QualityStatus,
    ReferenceGames, RSSSFPlayedResults, RSSSFDatedResults, DatedShareOfRSSSFPercent)
])
if (!nrow(targets)) stop("No structural-review source pages were found in season_audit.csv.")

master <- fread(master_path, encoding = "UTF-8")
required_master <- c("Date", "Home", "Away", "Score", "CompetitionType", "Source", "SourceFile")
if (!all(required_master %in% names(master))) stop("Production master has an unexpected schema.")

# Only pages that already have identifiable RSSSF rows in the master can be
# safely replaced automatically.  Pages without that footprint are reported
# as possible future additions, never silently imported.
old <- master[Source == "rsssf" & SourceFile %chin% targets$SourceFile]
replace_sources <- unique(old$SourceFile)

fresh <- fread(parsed_path, encoding = "UTF-8")
required_fresh <- c("Date", "Home", "Away", "Score", "Result", "CompetitionType", "SourceFile", "Country", "Season")
if (!all(required_fresh %in% names(fresh))) stop("World audit games file has an unexpected schema.")
if (!"RSSSFPhase" %in% names(fresh)) fresh[, RSSSFPhase := ""]
fresh <- fresh[
  SourceFile %chin% targets$SourceFile &
    CompetitionType == "league" &
    !is.na(Date) & nzchar(Home) & nzchar(Away) & Home != Away &
    Result %chin% c("1-0", "0.5-0.5", "0-1") &
    !tolower(trimws(RSSSFPhase)) %chin% c("promotion", "relegation")
]

# A page normally represents the season and country recorded by its manifest.
# Reject anything that would escape that source's declared identity.
fresh <- merge(
  fresh,
  targets[, .(SourceFile, TargetCountry = Country, TargetSeason = Season)],
  by = "SourceFile", all = FALSE
)
fresh[, identity_ok := Country == TargetCountry & Season == TargetSeason]

old_summary <- old[, .(
  MasterRows = .N,
  MasterFirstDate = min(as.IDate(Date), na.rm = TRUE),
  MasterLastDate = max(as.IDate(Date), na.rm = TRUE)
), by = SourceFile]
fresh_summary <- fresh[, .(
  FreshRows = .N,
  IdentityMatchedRows = sum(identity_ok),
  FreshFirstDate = min(as.IDate(Date), na.rm = TRUE),
  FreshLastDate = max(as.IDate(Date), na.rm = TRUE),
  SelfMatches = sum(Home == Away)
), by = SourceFile]

preview <- merge(targets, old_summary, by = "SourceFile", all.x = TRUE)
preview <- merge(preview, fresh_summary, by = "SourceFile", all.x = TRUE)
for (nm in c("MasterRows", "FreshRows", "IdentityMatchedRows", "SelfMatches")) {
  preview[is.na(get(nm)), (nm) := 0L]
}
preview[, ReplacementEligibility := fcase(
  MasterRows == 0L, "not_in_master_report_only",
  IdentityMatchedRows == 0L, "identity_mismatch_do_not_replace",
  IdentityMatchedRows != FreshRows, "mixed_identity_do_not_replace",
  default = "eligible_for_page_replacement"
)]
preview[, NetRowsIfReplaced := IdentityMatchedRows - MasterRows]
setorder(preview, ReplacementEligibility, Confederation, Country, Season)

out_dir <- file.path(audit_dir, "structural_recheck_replacement")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(preview, file.path(out_dir, "page_replacement_preview.csv"), na = "")
fwrite(preview[ReplacementEligibility == "eligible_for_page_replacement"],
       file.path(out_dir, "eligible_page_replacements.csv"), na = "")
fwrite(preview[ReplacementEligibility != "eligible_for_page_replacement"],
       file.path(out_dir, "held_page_replacements.csv"), na = "")

cat("\nWorld structural recheck replacement preview:\n")
print(preview[, .(
  Pages = .N,
  ExistingMasterRows = sum(MasterRows),
  FreshRows = sum(FreshRows),
  NetRowsIfReplaced = sum(NetRowsIfReplaced)
), by = ReplacementEligibility][order(ReplacementEligibility)])
cat("\nLargest eligible page changes:\n")
print(preview[ReplacementEligibility == "eligible_for_page_replacement"][order(-abs(NetRowsIfReplaced))][1:min(.N, 25L),
  .(Confederation, Country, Season, MasterRows, IdentityMatchedRows, NetRowsIfReplaced,
    RSSSFDatedResults, DatedShareOfRSSSFPercent, SourceFile)
])
message("Review: ", out_dir)
message("No production files changed.")
