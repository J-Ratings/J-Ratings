# Worldwide top-flight presence report.
# A season is counted as present when the master contains at least one completed,
# dated Tier 1 league fixture for that country.  This deliberately does not use a
# fixture-coverage threshold: it is a map of where the dataset reaches at all.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
master_path <- file.path(root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
seeds_path <- file.path(root, "EuropeanFootball/pipeline_data/Reference/non_uefa_country_seeds.csv")
uefa_path <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/UEFA/top_flight_span_overlap/country_coverage.csv")
out_dir <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/Global/top_flight_span_overview")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(master_path), file.exists(seeds_path))

country_key <- function(x) {
  x <- trimws(as.character(x))
  x[x == "Türkiye"] <- "Turkey"
  x[x == "Hongkong"] <- "Hong Kong"
  x[x == "Surinam"] <- "Suriname"
  x
}

season_year <- function(season, date) {
  y <- suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", as.character(season))))
  y[is.na(y)] <- as.integer(format(as.IDate(date[is.na(y)]), "%Y"))
  y
}

cat("Reading the production master...\n")
m <- fread(master_path, showProgress = FALSE)
needed <- c("Season", "Country", "Competition", "CompetitionType", "Tier", "League", "Date", "Home", "Away", "Result")
stopifnot(all(needed %in% names(m)))

m[, Date := as.IDate(Date)]
m[, CountryKey := country_key(Country)]
m[, StartYear := season_year(Season, Date)]

# Completed domestic top-flight matches only.  A dated result is the minimum
# evidence needed to say that the season appears in the dataset.
top <- m[
  CompetitionType == "league" & Tier == 1L &
    !is.na(Date) & nzchar(Home) & nzchar(Away) & nzchar(Result) &
    !is.na(StartYear) & !is.na(CountryKey)
]

presence <- top[, .(
  Matches = .N,
  FirstDate = min(Date),
  LastDate = max(Date),
  Competitions = paste(sort(unique(League[nzchar(League)])), collapse = " | "),
  CompetitionKeys = paste(sort(unique(Competition[nzchar(Competition)])), collapse = " | ")
), by = .(Country = CountryKey, Season, StartYear)]
setorder(presence, Country, StartYear, Season)

seeds <- unique(fread(seeds_path, showProgress = FALSE)[Tier == 1L, .(
  Country = country_key(Country), Confederation
)])
setkey(seeds, Country)

observed <- presence[, {
  years <- sort(unique(StartYear))
  first <- min(years); last <- max(years)
  gaps <- setdiff(seq.int(first, last), years)
  # Show all country/league labels seen, so historic renamed competitions remain visible.
  all_rows <- .SD
  .(
    FirstCoveredYear = first,
    LastCoveredYear = last,
    CoveredYears = length(years),
    ObservedSpanYears = last - first + 1L,
    InternalGapYears = length(gaps),
    InternalGapList = if (length(gaps)) paste(gaps, collapse = ", ") else "",
    DatedMatches = sum(Matches),
    FirstMatchDate = min(FirstDate),
    LastMatchDate = max(LastDate),
    LeaguesSeen = paste(sort(unique(unlist(strsplit(Competitions, " \\| ", fixed = FALSE)))), collapse = " | "),
    LeagueKeysSeen = paste(sort(unique(unlist(strsplit(CompetitionKeys, " \\| ", fixed = FALSE)))), collapse = " | ")
  )
}, by = Country]

overview <- merge(seeds, observed, by = "Country", all = TRUE)
overview[is.na(Confederation), Confederation := "Unmapped"]
overview[, ReferenceSpanAvailable := FALSE]
overview[, `:=`(ReferenceStartYear = NA_integer_, ReferenceEndYear = NA_integer_,
                ReferenceSeasons = NA_integer_, ReferenceCoveredYears = NA_integer_,
                ReferenceCoveragePercent = NA_real_)]

# The UEFA reference list supplies a true historical denominator.  Other
# confederations are explicitly labelled observed-only rather than implying that
# their first recorded season was the league's first season.
if (file.exists(uefa_path)) {
  uefa <- fread(uefa_path, showProgress = FALSE)
  uefa[, Country := country_key(Country)]
  uefa <- unique(uefa[, .(
    Country,
    ReferenceStartYear = as.integer(SpanStartYear),
    ReferenceEndYear = as.integer(SpanEndYear),
    ReferenceSeasons = as.integer(ExpectedSeasons),
    ReferenceCoveredYears = as.integer(MasterPresentSeasons),
    ReferenceCoveragePercent = as.numeric(MasterPresencePercent)
  )])
  setkey(uefa, Country); setkey(overview, Country)
  overview[uefa, `:=`(
    ReferenceSpanAvailable = TRUE,
    ReferenceStartYear = i.ReferenceStartYear,
    ReferenceEndYear = i.ReferenceEndYear,
    ReferenceSeasons = i.ReferenceSeasons,
    ReferenceCoveredYears = i.ReferenceCoveredYears,
    ReferenceCoveragePercent = i.ReferenceCoveragePercent
  )]
}

overview[, PresenceStatus := fifelse(is.na(CoveredYears), "no recorded top-flight matches",
  fifelse(InternalGapYears == 0L, "continuous within observed span", "has internal year gaps"))]
setcolorder(overview, c("Confederation", "Country", "PresenceStatus", "FirstCoveredYear", "LastCoveredYear",
  "CoveredYears", "ObservedSpanYears", "InternalGapYears", "InternalGapList", "DatedMatches",
  "FirstMatchDate", "LastMatchDate", "LeaguesSeen", "LeagueKeysSeen", "ReferenceSpanAvailable",
  "ReferenceStartYear", "ReferenceEndYear", "ReferenceSeasons", "ReferenceCoveredYears", "ReferenceCoveragePercent"))
setorder(overview, Confederation, Country)

gaps <- presence[, {
  years <- sort(unique(StartYear)); missing <- setdiff(seq.int(min(years), max(years)), years)
  if (length(missing)) data.table(MissingYear = missing) else NULL
}, by = Country]
gaps <- merge(gaps, overview[, .(Country, Confederation)], by = "Country", all.x = TRUE)
setorder(gaps, Confederation, Country, MissingYear)

conf_summary <- overview[, .(
  Countries = .N,
  CountriesWithRecordedTopFlight = sum(!is.na(CoveredYears)),
  CountriesWithoutRecordedTopFlight = sum(is.na(CoveredYears)),
  FirstRecordedYear = suppressWarnings(min(FirstCoveredYear, na.rm = TRUE)),
  LastRecordedYear = suppressWarnings(max(LastCoveredYear, na.rm = TRUE)),
  RecordedSeasonRows = sum(CoveredYears, na.rm = TRUE),
  InternalGapYears = sum(InternalGapYears, na.rm = TRUE)
), by = Confederation]
conf_summary[is.infinite(FirstRecordedYear), `:=`(FirstRecordedYear = NA_integer_, LastRecordedYear = NA_integer_)]
setorder(conf_summary, Confederation)

fwrite(overview, file.path(out_dir, "country_top_flight_span_overview.csv"))
fwrite(presence, file.path(out_dir, "season_presence.csv"))
fwrite(gaps, file.path(out_dir, "internal_gap_years.csv"))
fwrite(conf_summary, file.path(out_dir, "confederation_summary.csv"))

cat("\nWorldwide top-flight span overview:\n")
print(conf_summary)
cat(sprintf("\nWrote %s\n", out_dir))
cat("A season is present when it contains at least one dated Tier 1 league match; no 90% threshold was used.\n")
