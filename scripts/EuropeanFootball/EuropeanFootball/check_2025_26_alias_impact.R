suppressPackageStartupMessages(library(data.table))

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = TRUE
)
alias_path <- file.path(root, "EuropeanFootball/pipeline_data/Reference/team_aliases.csv")
master_path <- file.path(root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
review_audit_path <- file.path(
  root,
  "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_UEFA_Rebuild/team_identity_inventory/2025_26_alias_review_comparison.csv"
)

aliases <- fread(alias_path, encoding = "UTF-8")
review <- fread(review_audit_path, encoding = "UTF-8")
review <- review[, .(
  Country,
  SourceName,
  CanonicalName = CanonicalName_Review
)]

master <- fread(master_path, select = c(
  "Country", "CompetitionType", "Source", "Home", "Away"
))
long <- rbindlist(list(
  master[, .(Country, CompetitionType, Source, Team = trimws(as.character(Home)))],
  master[, .(Country, CompetitionType, Source, Team = trimws(as.character(Away)))]
))

impact <- merge(
  long,
  review,
  by.x = c("Country", "Team"),
  by.y = c("Country", "SourceName"),
  all = FALSE
)
impact[, ChangesName := Team != CanonicalName]

cat("Master team appearances covered by workbook mappings:", nrow(impact), "\n")
cat("Appearances whose displayed identity changes:", impact[ChangesName == TRUE, .N], "\n")
cat("Distinct raw names changed:", impact[ChangesName == TRUE, uniqueN(paste(Country, Team))], "\n")
cat("Distinct canonical clubs receiving changed names:", impact[ChangesName == TRUE, uniqueN(CanonicalName)], "\n")

cat("\nChanged appearances by competition type:\n")
print(impact[ChangesName == TRUE, .N, by = CompetitionType][order(-N)])

europe_names <- unique(long[Country == "Europe", Team])
connected <- unique(impact[ChangesName == TRUE, .(Country, Team, CanonicalName)])[
  CanonicalName %in% europe_names
]
cat("\nChanged domestic identities whose canonical name already occurs in Europe:", nrow(connected), "\n")
cat("Their domestic team appearances:", impact[
  ChangesName == TRUE & CanonicalName %in% europe_names,
  .N
], "\n")

cat("\nSerbia / Red Star check:\n")
print(review[Country == "Serbia" & grepl("Crvena|Red Star", paste(SourceName, CanonicalName), ignore.case = TRUE)])
print(long[Country %in% c("Serbia", "Europe") & Team %in% c("Crvena zvezda", "Crvena zvzeda", "Red Star Belgrade"), .N, by = .(Country, Team)])

alias_conflicts <- aliases[
  , .(CanonicalNames = uniqueN(CanonicalName)),
  by = .(Country, SourceName)
][CanonicalNames > 1L]
cat("\nCountry/source conflicts after import:", nrow(alias_conflicts), "\n")
