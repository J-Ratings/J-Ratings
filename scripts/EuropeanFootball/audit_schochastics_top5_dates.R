# audit_schochastics_all15_dates.R
#
# Structural audit of the Schochastics football-data historical top-flight
# results for all 15 countries currently covered by J-Ratings.
#
# It DOES NOT modify J-Ratings, the combined match CSV, aliases or Elo.
#
# Source:
# https://github.com/schochastics/football-data
# Current repository licence: Open Data Commons Attribution License.
#
# Package:
#   install.packages("nanoparquet")
# if neither nanoparquet nor arrow is already installed.

library(tictoc)
options(stringsAsFactors = FALSE)


tic()
repo_dir <- normalizePath(
  Sys.getenv(
    "J_RATINGS_REPO",
    "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"
  ),
  winslash = "/",
  mustWork = FALSE
)

SOURCE_PARQUET <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "schochastics",
  "games.parquet"
)

OUT_DIR <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Audit",
  "schochastics_all15_dates"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

AUDIT_CSV <- file.path(
  OUT_DIR,
  "schochastics_all15_date_audit.csv"
)

SUMMARY_CSV <- file.path(
  OUT_DIR,
  "schochastics_all15_country_summary.csv"
)

FLAGGED_CSV <- file.path(
  OUT_DIR,
  "schochastics_all15_flagged_seasons.csv"
)

SAMPLE_CSV <- file.path(
  OUT_DIR,
  "schochastics_all15_earliest_samples.csv"
)

if (!file.exists(SOURCE_PARQUET)) {
  stop(
    "Schochastics parquet is not cached at:\n",
    SOURCE_PARQUET,
    "\nRun 00_download_openfootball_current.R first."
  )
}

# ------------------------------------------------------------
# Read parquet
# ------------------------------------------------------------

if (requireNamespace("nanoparquet", quietly = TRUE)) {
  games <- as.data.frame(nanoparquet::read_parquet(SOURCE_PARQUET))
} else if (requireNamespace("arrow", quietly = TRUE)) {
  games <- as.data.frame(arrow::read_parquet(SOURCE_PARQUET))
} else {
  stop(
    'Need package "nanoparquet" or "arrow". ',
    'Run install.packages("nanoparquet") and source this script again.'
  )
}

needed <- c(
  "home", "away", "date", "gh", "ga",
  "competition", "level"
)

missing_cols <- setdiff(needed, names(games))
if (length(missing_cols)) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

games$date <- as.Date(games$date)

normalise_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

games$competition_key <- normalise_key(games$competition)
games$level_key <- normalise_key(games$level)

# ------------------------------------------------------------
# J-Ratings top-flight configuration
#
# FirstSeasonStartYear is the beginning of the national top-flight lineage
# we want to consider. CutoverStartYear is the first season handled by the
# existing J-Ratings source, so Schoch is audited only before that handover.
#
# For unusual short inaugural calendar-year seasons, FirstSeasonStartYear
# follows the Jul-Jun inference used below; season_label() fixes the display.
# ------------------------------------------------------------

cfg <- data.frame(
  country_key = c(
    "england", "spain", "italy", "germany", "france",
    "portugal", "netherlands", "belgium", "austria", "turkey",
    "scotland", "switzerland", "greece", "czechia", "ukraine"
  ),
  Country = c(
    "England", "Spain", "Italy", "Germany", "France",
    "Portugal", "Netherlands", "Belgium", "Austria", "Turkey",
    "Scotland", "Switzerland", "Greece", "Czechia", "Ukraine"
  ),
  FirstSeasonStartYear = c(
    1888L, 1928L, 1929L, 1963L, 1932L,
    1934L, 1956L, 1895L, 1911L, 1958L,
    1890L, 1897L, 1959L, 1993L, 1991L
  ),
  CutoverStartYear = c(
    1992L, 2012L, 2013L, 2010L, 2014L,
    2018L, 2018L, 2018L, 2010L, 2018L,
    2018L, 2014L, 2018L, 2018L, 2023L
  ),
  stringsAsFactors = FALSE
)

# Common naming variants.
key_aliases <- list(
  england = c("england"),
  spain = c("spain"),
  italy = c("italy"),
  germany = c("germany"),
  france = c("france"),
  portugal = c("portugal"),
  netherlands = c("netherlands", "holland"),
  belgium = c("belgium"),
  austria = c("austria"),
  turkey = c("turkey", "turkiye"),
  scotland = c("scotland"),
  switzerland = c("switzerland"),
  greece = c("greece"),
  czechia = c("czechia", "czech republic"),
  ukraine = c("ukraine")
)

# ------------------------------------------------------------
# Infer season
# ------------------------------------------------------------

yr <- as.integer(format(games$date, "%Y"))
mo <- as.integer(format(games$date, "%m"))

games$SeasonStartYear <- ifelse(
  !is.na(mo) & mo >= 7L,
  yr,
  yr - 1L
)

season_label <- function(country_key, sy) {
  if (country_key == "spain" && sy == 1928L) return("1929")
  if (country_key == "turkey" && sy == 1958L) return("1959")
  if (country_key == "ukraine" && sy == 1991L) return("1992")
  
  paste0(
    sy,
    "-",
    sprintf("%02d", (sy + 1L) %% 100L)
  )
}

# ------------------------------------------------------------
# Audit one country-season
# ------------------------------------------------------------

empty_audit_row <- function(country, key, sy) {
  data.frame(
    Country = country,
    CountryKey = key,
    Season = season_label(key, sy),
    SeasonStartYear = sy,
    Matches = 0L,
    Teams = 0L,
    FirstDate = as.Date(NA),
    LastDate = as.Date(NA),
    UniqueDates = 0L,
    MaxMatchesOnOneDate = 0L,
    MaxShareOneDatePct = NA_real_,
    MissingDateRows = 0L,
    MissingTeamRows = 0L,
    MissingScoreRows = 0L,
    SelfMatches = 0L,
    DuplicateFixtureDateKeys = 0L,
    ConflictingFixtureDateKeys = 0L,
    TeamDoubleBookedDates = 0L,
    Status = "MISSING",
    Reason = "NO_ROWS",
    stringsAsFactors = FALSE
  )
}

audit_one <- function(crow, sy) {
  aliases <- key_aliases[[crow$country_key]]
  
  z <- games[
    games$level_key == "national" &
      games$competition_key %in% aliases &
      games$SeasonStartYear == sy,
    ,
    drop = FALSE
  ]
  
  if (!nrow(z)) {
    return(empty_audit_row(crow$Country, crow$country_key, sy))
  }
  
  home <- trimws(as.character(z$home))
  away <- trimws(as.character(z$away))
  
  missing_date <- is.na(z$date)
  missing_team <- is.na(home) | is.na(away) | home == "" | away == ""
  missing_score <- is.na(z$gh) | is.na(z$ga)
  
  self_match <- !missing_team &
    normalise_key(home) == normalise_key(away)
  
  # Fixture/date duplicate checks.
  fixture_key <- paste(
    z$date,
    normalise_key(home),
    normalise_key(away),
    sep = "|||"
  )
  valid_fixture_key <- !missing_date & !missing_team
  
  fixture_tab <- table(fixture_key[valid_fixture_key])
  duplicate_fixture_keys <- sum(fixture_tab > 1L)
  
  score_txt <- ifelse(
    missing_score,
    NA_character_,
    paste(z$gh, z$ga, sep = "-")
  )
  
  conflicting_keys <- 0L
  if (any(valid_fixture_key)) {
    idx <- split(
      seq_len(nrow(z))[valid_fixture_key],
      fixture_key[valid_fixture_key]
    )
    
    conflicting_keys <- sum(vapply(
      idx,
      function(ii) {
        scores <- unique(score_txt[ii][!is.na(score_txt[ii])])
        length(scores) > 1L
      },
      logical(1)
    ))
  }
  
  # Date-distribution checks.
  valid_dates <- z$date[!missing_date]
  date_tab <- table(valid_dates)
  max_on_date <- if (length(date_tab)) max(date_tab) else 0L
  share <- if (nrow(z)) 100 * max_on_date / nrow(z) else NA_real_
  
  # A club should not normally play two top-flight league matches on the
  # same calendar date. Count unique team/date combinations occurring >1.
  team_dates <- rbind(
    data.frame(date = z$date, team = normalise_key(home)),
    data.frame(date = z$date, team = normalise_key(away))
  )
  
  team_dates <- team_dates[
    !is.na(team_dates$date) &
      !is.na(team_dates$team) &
      team_dates$team != "",
    ,
    drop = FALSE
  ]
  
  td_key <- paste(team_dates$date, team_dates$team, sep = "|||")
  td_tab <- table(td_key)
  double_booked <- sum(td_tab > 1L)
  
  reasons <- character()
  
  if (sum(missing_date) > 0L) {
    reasons <- c(reasons, "MISSING_DATES")
  }
  if (sum(missing_team) > 0L) {
    reasons <- c(reasons, "MISSING_TEAMS")
  }
  if (sum(missing_score) > 0L) {
    reasons <- c(reasons, "MISSING_SCORES")
  }
  if (sum(self_match) > 0L) {
    reasons <- c(reasons, "SELF_MATCH")
  }
  if (duplicate_fixture_keys > 0L) {
    reasons <- c(reasons, "DUPLICATE_FIXTURE_DATE")
  }
  if (conflicting_keys > 0L) {
    reasons <- c(reasons, "CONFLICTING_SCORE")
  }
  if (double_booked > 0L) {
    reasons <- c(reasons, "TEAM_DOUBLE_BOOKED")
  }
  if (nrow(z) >= 50L && length(unique(valid_dates)) < 5L) {
    reasons <- c(reasons, "TOO_FEW_UNIQUE_DATES")
  }
  if (nrow(z) >= 50L && !is.na(share) && share >= 20) {
    reasons <- c(reasons, "PLACEHOLDER_DATE_CONCENTRATION")
  }
  
  status <- if (length(reasons)) "CHECK" else "PASS"
  
  data.frame(
    Country = crow$Country,
    CountryKey = crow$country_key,
    Season = season_label(crow$country_key, sy),
    SeasonStartYear = sy,
    Matches = nrow(z),
    Teams = length(unique(c(
      home[!is.na(home) & home != ""],
      away[!is.na(away) & away != ""]
    ))),
    FirstDate = if (length(valid_dates)) min(valid_dates) else as.Date(NA),
    LastDate = if (length(valid_dates)) max(valid_dates) else as.Date(NA),
    UniqueDates = length(unique(valid_dates)),
    MaxMatchesOnOneDate = max_on_date,
    MaxShareOneDatePct = if (is.na(share)) NA_real_ else round(share, 1),
    MissingDateRows = sum(missing_date),
    MissingTeamRows = sum(missing_team),
    MissingScoreRows = sum(missing_score),
    SelfMatches = sum(self_match, na.rm = TRUE),
    DuplicateFixtureDateKeys = duplicate_fixture_keys,
    ConflictingFixtureDateKeys = conflicting_keys,
    TeamDoubleBookedDates = double_booked,
    Status = status,
    Reason = if (length(reasons)) paste(unique(reasons), collapse = ";") else "",
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Run all 15
#
# We deliberately test EVERY nominal season-start year between inception and
# the existing-source cutover. Therefore wartime/non-played years can appear
# as MISSING. We will distinguish genuine gaps from seasons that simply did
# not exist when deciding the final whitelist for 01.
# ------------------------------------------------------------

out <- list()
k <- 1L

for (i in seq_len(nrow(cfg))) {
  crow <- cfg[i, ]
  
  for (sy in crow$FirstSeasonStartYear:(crow$CutoverStartYear - 1L)) {
    out[[k]] <- audit_one(crow, sy)
    k <- k + 1L
  }
}

audit <- do.call(rbind, out)
audit <- audit[
  order(
    match(audit$CountryKey, cfg$country_key),
    audit$SeasonStartYear
  ),
  ,
  drop = FALSE
]

write.csv(audit, AUDIT_CSV, row.names = FALSE, na = "")

flagged <- audit[audit$Status != "PASS", , drop = FALSE]
write.csv(flagged, FLAGGED_CSV, row.names = FALSE, na = "")

# ------------------------------------------------------------
# Country summary
# ------------------------------------------------------------

summary_rows <- lapply(seq_len(nrow(cfg)), function(i) {
  crow <- cfg[i, ]
  a <- audit[audit$CountryKey == crow$country_key, , drop = FALSE]
  present <- a[a$Matches > 0L, , drop = FALSE]
  
  data.frame(
    Country = crow$Country,
    FirstTargetSeason = season_label(crow$country_key, crow$FirstSeasonStartYear),
    LastSchochTargetSeason = season_label(
      crow$country_key,
      crow$CutoverStartYear - 1L
    ),
    ExistingSourceFrom = season_label(
      crow$country_key,
      crow$CutoverStartYear
    ),
    NominalYearsChecked = nrow(a),
    PresentSeasons = sum(a$Matches > 0L),
    PassSeasons = sum(a$Status == "PASS"),
    CheckSeasons = sum(a$Status == "CHECK"),
    MissingYears = sum(a$Status == "MISSING"),
    EarliestPresent = if (nrow(present)) present$Season[1] else "",
    LatestPresent = if (nrow(present)) present$Season[nrow(present)] else "",
    stringsAsFactors = FALSE
  )
})

country_summary <- do.call(rbind, summary_rows)
write.csv(country_summary, SUMMARY_CSV, row.names = FALSE, na = "")

# ------------------------------------------------------------
# Earliest samples
# ------------------------------------------------------------

samples <- list()
s <- 1L

for (i in seq_len(nrow(cfg))) {
  crow <- cfg[i, ]
  aliases <- key_aliases[[crow$country_key]]
  
  z <- games[
    games$level_key == "national" &
      games$competition_key %in% aliases &
      games$SeasonStartYear >= crow$FirstSeasonStartYear &
      games$SeasonStartYear < crow$CutoverStartYear,
    needed,
    drop = FALSE
  ]
  
  z <- z[order(z$date, z$home, z$away), , drop = FALSE]
  
  if (nrow(z)) {
    z$Country <- crow$Country
    samples[[s]] <- head(z, 12)
    s <- s + 1L
  }
}

if (length(samples)) {
  sample_df <- do.call(rbind, samples)
  write.csv(sample_df, SAMPLE_CSV, row.names = FALSE, na = "")
}

# ------------------------------------------------------------
# Console summary
# ------------------------------------------------------------

cat("\nSCHOCHASTICS ALL-15 HISTORICAL TOP-FLIGHT AUDIT\n")
cat("===============================================\n")

for (i in seq_len(nrow(cfg))) {
  crow <- cfg[i, ]
  a <- audit[audit$CountryKey == crow$country_key, , drop = FALSE]
  
  cat("\n", toupper(crow$Country), "\n", sep = "")
  cat("  Nominal years checked: ", nrow(a), "\n", sep = "")
  cat("  Seasons/years present: ", sum(a$Matches > 0L), "\n", sep = "")
  cat("  PASS: ", sum(a$Status == "PASS"), "\n", sep = "")
  cat("  CHECK: ", sum(a$Status == "CHECK"), "\n", sep = "")
  cat("  MISSING years: ", sum(a$Status == "MISSING"), "\n", sep = "")
  
  present <- a[a$Matches > 0L, , drop = FALSE]
  if (nrow(present)) {
    cat(
      "  Earliest present: ",
      present$Season[1],
      " (",
      as.character(present$FirstDate[1]),
      " to ",
      as.character(present$LastDate[1]),
      ")\n",
      sep = ""
    )
  }
  
  bad <- a[a$Status != "PASS", , drop = FALSE]
  
  if (nrow(bad)) {
    cat("  First flagged/missing years:\n")
    print(
      utils::head(
        bad[, c(
          "Season", "Matches", "Teams", "FirstDate", "LastDate",
          "UniqueDates", "MaxShareOneDatePct", "SelfMatches",
          "DuplicateFixtureDateKeys", "ConflictingFixtureDateKeys",
          "TeamDoubleBookedDates", "Status", "Reason"
        )],
        15
      ),
      row.names = FALSE
    )
  }
}

cat("\nCountry summary CSV:\n", SUMMARY_CSV, "\n", sep = "")
cat("\nFull season audit CSV:\n", AUDIT_CSV, "\n", sep = "")
cat("\nFlagged/missing seasons CSV:\n", FLAGGED_CSV, "\n", sep = "")
cat("\nEarliest-match samples:\n", SAMPLE_CSV, "\n", sep = "")

cat(
  "\nInterpretation:\n",
  "- PASS = no structural problem found by these checks.\n",
  "- CHECK = data exists but at least one structural issue needs review.\n",
  "- MISSING = no Schoch rows for that nominal season-start year.\n",
  "- MISSING does NOT automatically mean a data gap; wartime and other years in which no league season was played are deliberately left visible here.\n",
  "- This is a structural audit, not proof that every historical score is correct.\n",
  sep = ""
)

toc()
