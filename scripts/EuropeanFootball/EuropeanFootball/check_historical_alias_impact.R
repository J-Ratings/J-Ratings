suppressPackageStartupMessages(library(data.table))

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = TRUE
)
alias_path <- file.path(root, "EuropeanFootball/pipeline_data/Reference/team_aliases.csv")
master_path <- file.path(root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
audit_path <- file.path(
  root,
  "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_UEFA_Rebuild/team_identity_inventory/historical_alias_review_comparison.csv"
)

aliases <- fread(alias_path, encoding = "UTF-8")
review <- fread(audit_path, encoding = "UTF-8")[
  Decision %in% c("add", "already_present"),
  .(Country, SourceName, CanonicalName = CanonicalName_Review)
]
master <- fread(master_path, select = c("Country", "CompetitionType", "Home", "Away"))
long <- rbindlist(list(
  master[, .(Country, CompetitionType, Team = trimws(as.character(Home)))],
  master[, .(Country, CompetitionType, Team = trimws(as.character(Away)))]
))
impact <- merge(
  long,
  review,
  by.x = c("Country", "Team"),
  by.y = c("Country", "SourceName"),
  all = FALSE
)[Team != CanonicalName]
europe_names <- unique(long[Country == "Europe", Team])

cat("Historical raw team appearances changed:", nrow(impact), "\n")
cat("Distinct historical source identities changed:", uniqueN(paste(impact$Country, impact$Team)), "\n")
cat("Canonical clubs receiving those identities:", uniqueN(impact$CanonicalName), "\n")
cat("Mapped identities joining a canonical name already used in Europe:",
    uniqueN(impact[CanonicalName %in% europe_names, paste(Country, Team)]), "\n")
cat("Their domestic appearances:", impact[CanonicalName %in% europe_names, .N], "\n")

conflicts <- aliases[
  , .(CanonicalNames = uniqueN(CanonicalName)),
  by = .(Country, SourceName)
][CanonicalNames > 1L]
cat("Country/source conflicts after import:", nrow(conflicts), "\n")
