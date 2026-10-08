suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

repo_root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = TRUE
)

review_path <- Sys.getenv(
  "TEAM_ALIAS_REVIEW_XLSX",
  "C:/Users/stjuk/Downloads/rsssf_wikipedia_2025_26_mapping_review.xlsx"
)
alias_path <- file.path(
  repo_root,
  "EuropeanFootball/pipeline_data/Reference/team_aliases.csv"
)
audit_dir <- file.path(
  repo_root,
  "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_UEFA_Rebuild/team_identity_inventory"
)
apply_changes <- identical(Sys.getenv("APPLY_TEAM_ALIAS_REVIEW", "0"), "1")

if (!file.exists(review_path)) stop("Review workbook not found: ", review_path)
if (!file.exists(alias_path)) stop("Alias file not found: ", alias_path)

review <- as.data.table(read_excel(review_path, sheet = "Review"))
setnames(review, trimws(names(review)))
required <- c("Country", "RSSSFName", "WikipediaName", "Method")
missing <- setdiff(required, names(review))
if (length(missing)) stop("Review sheet is missing: ", paste(missing, collapse = ", "))

review <- review[
  !is.na(Country) & trimws(Country) != "" &
    !is.na(RSSSFName) & trimws(RSSSFName) != "" &
    !is.na(WikipediaName) & trimws(WikipediaName) != ""
]
review[, `:=`(
  Country = trimws(as.character(Country)),
  SourceName = trimws(as.character(RSSSFName)),
  CanonicalName = trimws(as.character(WikipediaName)),
  Method = trimws(as.character(Method))
)]

review_conflicts <- review[
  , .(CanonicalNames = uniqueN(CanonicalName)),
  by = .(Country, SourceName)
][CanonicalNames > 1L]
if (nrow(review_conflicts)) {
  stop("The workbook contains conflicting country/source mappings; nothing was changed.")
}

candidates <- unique(
  review[, .(Country, SourceName, CanonicalName, Method)],
  by = c("Country", "SourceName")
)
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

cat("Workbook mappings:", nrow(candidates), "\n")
cat("  normalised exact:", candidates[Method == "normalised_exact", .N], "\n")
cat("  manual current-season:", candidates[Method == "manual_current_season", .N], "\n")
print(comparison[, .N, by = Decision][order(Decision)])

conflicts <- comparison[Decision == "conflict"]
if (nrow(conflicts)) {
  cat("\nConflicts with existing aliases (not applied):\n")
  print(conflicts[, .(
    Country,
    SourceName,
    Existing = CanonicalName_Existing,
    Review = CanonicalName_Review,
    Method
  )])
}

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
audit_path <- file.path(audit_dir, "2025_26_alias_review_comparison.csv")
fwrite(comparison, audit_path, bom = TRUE)
cat("\nAudit:", audit_path, "\n")

if (!apply_changes) {
  cat("Dry run only. Set APPLY_TEAM_ALIAS_REVIEW=1 to add non-conflicting mappings.\n")
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
  paste0("team_aliases_before_2025_26_review_", stamp, ".csv")
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
cat("Added aliases:", nrow(to_add), "\n")
cat("Updated alias file:", alias_path, "\n")
cat("Backup:", backup_path, "\n")
