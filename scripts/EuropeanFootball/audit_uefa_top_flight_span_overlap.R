# Historical top-flight overlap audit.
# A season is counted as covered as soon as the master contains any dated,
# played top-flight match. This measures span overlap, not data completeness.
# The RSSSF comparison is retained only as a diagnostic for later quality work.

required <- c("data.table")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required package(s): ", paste(missing, collapse = ", "))
library(data.table)

repo_dir <- normalizePath(Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"), winslash = "/", mustWork = TRUE)
span_file <- file.path(repo_dir, "EuropeanFootball", "pipeline_data", "Reference", "uefa_top_flight_spans.tsv")
master_file <- file.path(repo_dir, "EuropeanFootball", "pipeline_data", "Matches_Clean_Combined", "european_football_all_matches.csv")
audit_file <- file.path(repo_dir, "EuropeanFootball", "pipeline_data", "Manual_Sources", "RSSSF_UEFA_Rebuild", "season_audit.csv")
out_dir <- file.path(repo_dir, "EuropeanFootball", "pipeline_data", "Manual_Sources", "UEFA", "top_flight_span_overlap")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 2024 is the latest start year that is complete across both calendar-year and
# autumn-to-spring leagues in the supplied historical list.
coverage_end_year <- 2024L
coverage_threshold <- 90
country_name_map <- c("Türkiye" = "Turkey")
trusted_complete_sources <- c("schochastics", "openfootball", "weltfussball_manual")

spans_raw <- fread(span_file, sep = "\t", encoding = "UTF-8")
setnames(spans_raw, c("Association", "Top-flight competition span"), c("Association", "SpanText"))

parse_segments <- function(country, span_text) {
  parts <- trimws(unlist(strsplit(span_text, ";", fixed = TRUE)))
  out <- vector("list", length(parts))
  for (i in seq_along(parts)) {
    part <- gsub(intToUtf8(0x2013), "-", parts[[i]], fixed = TRUE)
    title <- trimws(sub(":.*$", "", part))
    years <- regmatches(part, gregexpr("[0-9]{4}(?:-(?:[0-9]{4}|Present))?", part, perl = TRUE))[[1]]
    rows <- lapply(years, function(token) {
      bits <- strsplit(token, "-", fixed = TRUE)[[1]]
      start <- as.integer(bits[[1]])
      end <- if (length(bits) == 1L) start else if (bits[[2]] == "Present") coverage_end_year else as.integer(bits[[2]])
      data.table(Association = country, Era = title, Span = token, StartYear = start, EndYear = min(end, coverage_end_year))
    })
    out[[i]] <- rbindlist(rows, fill = TRUE)
  }
  rbindlist(out, fill = TRUE)
}
segments <- rbindlist(Map(parse_segments, spans_raw$Association, spans_raw$SpanText), fill = TRUE)
segments <- segments[!is.na(StartYear) & StartYear <= EndYear]
segments[, Country := fifelse(Association %chin% names(country_name_map), country_name_map[Association], Association)]
# Keep the supplied association label intact while matching the master-data
# country spelling. This ASCII match also remains robust to Windows encoding.
segments[grepl("rkiye", Association, ignore.case = TRUE), Country := "Turkey"]
segments[, EraKey := sprintf("%s: %s (%s)", Association, Era, Span)]

master <- fread(master_file, encoding = "UTF-8", select = c("Season", "Country", "CompetitionType", "Tier", "Date", "Result", "Source"))
master[, StartYear := suppressWarnings(as.integer(substr(Season, 1L, 4L)))]
master <- master[
  CompetitionType == "league" & Tier == 1L & !is.na(StartYear) & StartYear <= coverage_end_year &
    !is.na(Date) & nzchar(Date) & !is.na(Result) & nzchar(Result)
]
master <- master[, .(
  MasterGames = .N,
  Sources = paste(sort(unique(Source)), collapse = ";"),
  TrustedOnly = all(Source %chin% trusted_complete_sources)
), by = .(Country, StartYear)]

audit <- fread(audit_file, encoding = "UTF-8", select = c("Country", "StartYear", "ExpectedGames"))
audit <- audit[!is.na(ExpectedGames) & ExpectedGames > 0 & StartYear <= coverage_end_year,
  .(ExpectedGames = max(ExpectedGames)), by = .(Country, StartYear)]

season_universe <- segments[, .(Association, Country, Era, Span, StartYear = seq.int(StartYear, EndYear)), by = .(EraKey)]
season_universe <- merge(season_universe, master, by = c("Country", "StartYear"), all.x = TRUE)
season_universe <- merge(season_universe, audit, by = c("Country", "StartYear"), all.x = TRUE)
season_universe[is.na(MasterGames), `:=`(MasterGames = 0L, Sources = "", TrustedOnly = FALSE)]
season_universe[, MeasuredCoveragePercent := fifelse(!is.na(ExpectedGames), 100 * MasterGames / ExpectedGames, NA_real_)]
season_universe[, RSSSFQualityBasis := fcase(
  !is.na(ExpectedGames) & MeasuredCoveragePercent >= coverage_threshold, "measured_90_plus",
  !is.na(ExpectedGames), "measured_below_90",
  MasterGames > 0L & TrustedOnly, "trusted_source_present",
  MasterGames > 0L, "present_unverified",
  default = "no_master_data"
)]
season_universe[, CoverageBasis := fifelse(
  MasterGames > 0L,
  "master_games_present",
  "no_master_data"
)]
season_universe[, Covered := MasterGames > 0L]

league_coverage <- season_universe[, .(
  SpanStartYear = min(StartYear),
  SpanEndYear = max(StartYear),
  ExpectedSeasons = .N,
  CoveredSeasons = sum(Covered),
  CoveragePercent = round(100 * mean(Covered), 1),
  Measured90Seasons = sum(RSSSFQualityBasis == "measured_90_plus"),
  TrustedSourceSeasons = sum(RSSSFQualityBasis == "trusted_source_present"),
  MeasuredBelow90 = sum(RSSSFQualityBasis == "measured_below_90"),
  PresentUnverified = sum(RSSSFQualityBasis == "present_unverified"),
  NoMasterData = sum(CoverageBasis == "no_master_data"),
  MasterPresentSeasons = sum(MasterGames > 0L),
  MasterPresencePercent = round(100 * mean(MasterGames > 0L), 1)
), by = .(Association, Country, Era, Span, EraKey)][order(Association, SpanStartYear)]

country_seasons <- unique(season_universe[, .(
  Association, Country, StartYear, Covered, CoverageBasis, RSSSFQualityBasis, MasterGames
)])
country_coverage <- country_seasons[, .(
  SpanStartYear = min(StartYear),
  SpanEndYear = max(StartYear),
  ExpectedSeasons = .N,
  CoveredSeasons = sum(Covered),
  CoveragePercent = round(100 * mean(Covered), 1),
  Measured90Seasons = sum(RSSSFQualityBasis == "measured_90_plus"),
  TrustedSourceSeasons = sum(RSSSFQualityBasis == "trusted_source_present"),
  MeasuredBelow90 = sum(RSSSFQualityBasis == "measured_below_90"),
  PresentUnverified = sum(RSSSFQualityBasis == "present_unverified"),
  NoMasterData = sum(CoverageBasis == "no_master_data"),
  MasterPresentSeasons = sum(MasterGames > 0L),
  MasterPresencePercent = round(100 * mean(MasterGames > 0L), 1)
), by = .(Association, Country)][order(Association)]

fwrite(segments, file.path(out_dir, "top_flight_era_reference.csv"))
fwrite(season_universe[order(Association, StartYear, Era)], file.path(out_dir, "season_coverage_detail.csv"))
fwrite(league_coverage, file.path(out_dir, "league_era_coverage.csv"))
fwrite(country_coverage, file.path(out_dir, "country_coverage.csv"))

cat("UEFA top-flight historical overlap through", coverage_end_year, "\n")
print(country_coverage[, .(Association, ExpectedSeasons, CoveredSeasons, CoveragePercent, MasterPresencePercent)])
cat("\nOutput:", out_dir, "\n")
