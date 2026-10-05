suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = TRUE
)
workbook_path <- Sys.getenv(
  "HISTORICAL_ALIAS_REVIEW_XLSX",
  "C:/Users/stjuk/Downloads/rsssf_wikipedia_historical_mapping_review.xlsx"
)
csv_path <- Sys.getenv(
  "HISTORICAL_ALIAS_ADDITIONS_CSV",
  "C:/Users/stjuk/Downloads/rsssf_wikipedia_historical_alias_additions.csv"
)
alias_path <- file.path(root, "EuropeanFootball/pipeline_data/Reference/team_aliases.csv")
audit_dir <- file.path(
  root,
  "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_UEFA_Rebuild/team_identity_inventory"
)
apply_changes <- identical(Sys.getenv("APPLY_HISTORICAL_ALIAS_REVIEW", "0"), "1")

for (path in c(workbook_path, csv_path, alias_path)) {
  if (!file.exists(path)) stop("Required file not found: ", path)
}

review <- as.data.table(read_excel(workbook_path, sheet = "Historical_Review"))
review[, `:=`(
  Country = trimws(as.character(Country)),
  RSSSFName = trimws(as.character(RSSSFName)),
  WikipediaName = trimws(as.character(WikipediaName)),
  ProposedAction = trimws(as.character(ProposedAction)),
  Confidence = trimws(as.character(Confidence))
)]

candidates <- review[
  ProposedAction == "add_alias_candidate" &
    !is.na(Country) & Country != "" &
    !is.na(RSSSFName) & RSSSFName != "" &
    !is.na(WikipediaName) & WikipediaName != "",
  .(
    Country,
    SourceName = RSSSFName,
    CanonicalName = WikipediaName,
    Confidence,
    AnchorYears,
    Games,
    Seasons
  )
]
candidates <- unique(candidates, by = c("Country", "SourceName", "CanonicalName"))

candidate_conflicts <- candidates[
  , .(CanonicalNames = uniqueN(CanonicalName)),
  by = .(Country, SourceName)
][CanonicalNames > 1L]
if (nrow(candidate_conflicts)) {
  stop("Country-aware candidate mappings contain conflicts; nothing was changed.")
}

workbook_pairs <- as.data.table(read_excel(workbook_path, sheet = "Alias_Additions"))
setnames(workbook_pairs, c("RSSSFName", "WikipediaName"), c("SourceName", "CanonicalName"))
external_pairs <- fread(csv_path, encoding = "UTF-8")
setnames(external_pairs, c("RSSSFName", "WikipediaName"), c("SourceName", "CanonicalName"))

clean_pairs <- function(x) {
  x[, `:=`(
    SourceName = trimws(as.character(SourceName)),
    CanonicalName = trimws(as.character(CanonicalName))
  )]
  unique(x[SourceName != "" & CanonicalName != "", .(SourceName, CanonicalName)])
}
workbook_pairs <- clean_pairs(workbook_pairs)
external_pairs <- clean_pairs(external_pairs)
derived_pairs <- unique(candidates[, .(SourceName, CanonicalName)])

pair_key <- function(x) paste(x$SourceName, x$CanonicalName, sep = "\r")
if (!setequal(pair_key(workbook_pairs), pair_key(external_pairs))) {
  stop("The workbook Alias_Additions sheet and supplied CSV do not agree.")
}
if (!setequal(pair_key(workbook_pairs), pair_key(derived_pairs))) {
  stop("The two-column additions do not agree with country-aware approved rows.")
}

existing <- fread(alias_path, encoding = "UTF-8", na.strings = c("", "NA"))
existing[, `:=`(
  Country = trimws(as.character(Country)),
  SourceName = trimws(as.character(SourceName)),
  CanonicalName = trimws(as.character(CanonicalName))
)]

comparison <- merge(
  candidates,
  existing,
  by = c("Country", "SourceName"),
  all.x = TRUE,
  suffixes = c("_Review", "_Existing")
)
comparison[, Decision := fifelse(
  is.na(CanonicalName_Existing),
  "add",
  fifelse(CanonicalName_Review == CanonicalName_Existing, "already_present", "conflict")
)]

cat("Historical rows reviewed:", nrow(review), "\n")
cat("Approved country-aware alias candidates:", nrow(candidates), "\n")
cat("Workbook/CSV pair lists agree:", nrow(workbook_pairs), "pairs\n")
print(comparison[, .N, by = Decision][order(Decision)])
cat("Candidate games represented:", sum(candidates$Games, na.rm = TRUE), "\n")

conflicts <- comparison[Decision == "conflict"]
if (nrow(conflicts)) {
  cat("\nConflicts with existing aliases (not applied):\n")
  print(conflicts[, .(
    Country,
    SourceName,
    Existing = CanonicalName_Existing,
    Review = CanonicalName_Review,
    Confidence,
    AnchorYears
  )])
}

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
audit_path <- file.path(audit_dir, "historical_alias_review_comparison.csv")
fwrite(comparison, audit_path, bom = TRUE)
cat("Audit:", audit_path, "\n")

if (!apply_changes) {
  cat("Dry run only. Set APPLY_HISTORICAL_ALIAS_REVIEW=1 to add non-conflicting mappings.\n")
  quit(save = "no", status = 0)
}

to_add <- comparison[Decision == "add", .(
  Country,
  SourceName,
  CanonicalName = CanonicalName_Review
)]
if (!nrow(to_add)) {
  cat("No new aliases to add.\n")
  quit(save = "no", status = 0)
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- file.path(
  dirname(alias_path),
  paste0("team_aliases_before_historical_review_", stamp, ".csv")
)
if (!file.copy(alias_path, backup_path, overwrite = FALSE)) {
  stop("Could not create alias backup; nothing was changed.")
}

updated <- rbindlist(list(
  existing[, .(Country, SourceName, CanonicalName)],
  to_add
), use.names = TRUE)
updated <- unique(updated, by = c("Country", "SourceName"))
setorder(updated, Country, SourceName)

post_conflicts <- updated[
  , .(CanonicalNames = uniqueN(CanonicalName)),
  by = .(Country, SourceName)
][CanonicalNames > 1L]
if (nrow(post_conflicts)) stop("Post-merge conflict detected; original file is unchanged.")

fwrite(updated, alias_path, bom = TRUE)
cat("Added historical aliases:", nrow(to_add), "\n")
cat("Updated alias file:", alias_path, "\n")
cat("Backup:", backup_path, "\n")
