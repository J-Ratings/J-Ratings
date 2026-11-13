# install.packages(c("httr2","jsonlite","dplyr","readr","purrr","stringr","lubridate","tibble"))

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(tibble)

options(stringsAsFactors = FALSE)

# ============================================================
# World Rugby Union match updater
#
# Purpose:
#   - Run entirely inside the J-Ratings Git repo
#   - Read existing Rugby Union master match CSV
#   - Discover international rugby competitions and fetch their schedules
#   - Merge completed API matches into the master file
#   - Export upcoming fixtures to RugbyUnion/pipeline_data/source/results.csv
#   - Prefer scored API rows over older score-less rows
#
# Main output:
#   RugbyUnion/pipeline_data/Matches/rugby_union_results_master.csv
#
# Optional seed input, used only if master does not exist:
#   RugbyUnion/pipeline_data/Matches/rugby_union_results_deduped.csv
#
# GitHub Actions:
#   - Works from GITHUB_WORKSPACE
#   - API key should be supplied by repository secret:
#       THESPORTSDB_API_KEY
#   - If the env var is missing, falls back to "123"
#     because that is the current value in your local script.
# ============================================================

# -----------------------------
# Resolve repo root
# -----------------------------
get_repo_dir <- function() {
  github_workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  
  if (nzchar(github_workspace) && dir.exists(github_workspace)) {
    return(normalizePath(github_workspace, winslash = "/", mustWork = TRUE))
  }
  
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  
  if (
    dir.exists(file.path(wd, "RugbyUnion")) &&
    dir.exists(file.path(wd, "scripts", "world_rugby"))
  ) {
    return(wd)
  }
  
  stop(
    "Could not find repo root. Run this script from the J-Ratings repo root, ",
    "or set GITHUB_WORKSPACE."
  )
}

repo_dir <- get_repo_dir()

# -----------------------------
# File paths
# -----------------------------
match_dir <- file.path(
  repo_dir,
  "RugbyUnion", "pipeline_data", "Matches"
)

dir.create(match_dir, recursive = TRUE, showWarnings = FALSE)

historical_csv <- file.path(
  match_dir,
  "rugby_union_results_deduped.csv"
)

out_file <- file.path(
  match_dir,
  "rugby_union_results_master.csv"
)

fixtures_file <- file.path(repo_dir, "RugbyUnion", "pipeline_data", "source", "results.csv")
aliases_file <- file.path(repo_dir, "RugbyUnion", "config", "international_teams.tsv")

cat("Repo dir: ", repo_dir, "\n", sep = "")
cat("Match data dir: ", match_dir, "\n", sep = "")
cat("Master output: ", out_file, "\n", sep = "")

# -----------------------------
# API settings
# -----------------------------
api_key <- Sys.getenv("THESPORTSDB_API_KEY", unset = "")

if (!nzchar(api_key)) {
  api_key <- "123"
  warning(
    "THESPORTSDB_API_KEY is not set. Falling back to API key '123'. ",
    "For GitHub Actions, add a repository secret named THESPORTSDB_API_KEY."
  )
}

base_url <- paste0("https://www.thesportsdb.com/api/v1/json/", api_key)

# Keep known competition names stable across updates.
leagues <- tribble(
  ~idLeague, ~competition,
  4714, "Six Nations Championship",
  4986, "Rugby Championship",
  4985, "Pacific Nations Cup",
  4574, "Rugby World Cup",
  4983, "Rugby Europe Championship",
  5479, "Rugby Union International Friendlies",
  5512, "British and Irish Lions Tours",
  5852, "Nations Championship",
  5933, "World Rugby Nations Cup"
)

today <- Sys.Date()
current_year <- year(today)
lookback_date <- today %m-% months(6)
fixtures_until <- today + 45L

# Override the season bounds for a backfill or to check farther ahead.
start_override <- Sys.getenv("RUGBY_API_START_YEAR")
start_year <- suppressWarnings(
  as.integer(Sys.getenv("RUGBY_API_START_YEAR", unset = as.character(year(lookback_date))))
)
if (is.na(start_year)) {
  stop("RUGBY_API_START_YEAR must be an integer year if supplied.")
}
if (nzchar(start_override)) lookback_date <- as.Date(sprintf("%04d-01-01", start_year))

end_year <- suppressWarnings(
  as.integer(Sys.getenv("RUGBY_API_END_YEAR", unset = as.character(year(fixtures_until))))
)

if (is.na(end_year)) {
  stop("RUGBY_API_END_YEAR must be an integer year if supplied.")
}

if (start_year > end_year) {
  stop("Start year is later than end year. start_year=", start_year, ", end_year=", end_year)
}

seasons <- as.character(start_year:end_year)

cat("Today's date: ", as.character(today), "\n", sep = "")
cat("Seasons to query: ", paste(seasons, collapse = ", "), "\n", sep = "")

# -----------------------------
# Helpers
# -----------------------------
normalise_date_text <- function(x) {
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  
  ymd_parsed <- suppressWarnings(ymd(x))
  dmy_parsed <- suppressWarnings(dmy(x))
  
  parsed <- coalesce(ymd_parsed, dmy_parsed)
  format(parsed, "%Y-%m-%d")
}

clean_team_name <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_remove(" Rugby$")
}

safe_int <- function(x) {
  suppressWarnings(as.integer(x))
}

read_results_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing results file: ", path)
  }
  
  df <- read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
  
  # Create missing columns if this is an older file layout
  if (!"home_score" %in% names(df)) df$home_score <- NA_character_
  if (!"away_score" %in% names(df)) df$away_score <- NA_character_
  
  required_cols <- c(
    "date", "home_team", "away_team",
    "home_score", "away_score",
    "result", "competition"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns in file ", path, ": ", paste(missing_cols, collapse = ", "))
  }
  
  df %>%
    transmute(
      date        = normalise_date_text(date),
      home_team   = clean_team_name(home_team),
      away_team   = clean_team_name(away_team),
      home_score  = safe_int(home_score),
      away_score  = safe_int(away_score),
      result      = str_squish(as.character(result)),
      competition = str_squish(as.character(competition))
    ) %>%
    filter(!is.na(date), home_team != "", away_team != "", competition != "")
}

fetch_api <- function(endpoint) {
  dat <- tryCatch({
    resp <- request(paste0(base_url, "/", endpoint)) |>
      req_retry(max_tries = 5) |>
      req_timeout(seconds = 60) |>
      req_perform()
    fromJSON(resp_body_string(resp), flatten = TRUE)
  }, error = function(e) {
    stop("TheSportsDB request failed for ", endpoint, ". Existing data was kept.", call. = FALSE)
  })
  Sys.sleep(2.5 + runif(1, min = -0.5, max = 0.5))
  dat
}

discover_leagues <- function() {
  dat <- fetch_api("all_leagues.php")
  required <- c("idLeague", "strLeague", "strSport")
  if (!all(required %in% names(dat$leagues))) {
    stop("TheSportsDB returned an invalid league catalogue.")
  }
  as_tibble(dat$leagues) %>%
    filter(strSport == "Rugby") %>%
    transmute(idLeague = safe_int(idLeague), competition = str_squish(strLeague)) %>%
    filter(!is.na(idLeague), !is.na(competition), competition != "")
}

aliases <- read_tsv(aliases_file, col_types = cols(.default = col_character()))
team_key <- function(x) str_to_lower(clean_team_name(x))
team_names <- setNames(aliases$canonical, team_key(aliases$alias))

fetch_season <- function(id_league, season_label, competition_name) {
  endpoint <- paste0("eventsseason.php?id=", id_league, "&s=", season_label)
  dat <- fetch_api(endpoint)
  if (!"events" %in% names(dat)) stop("Invalid TheSportsDB response for ", endpoint)
  if (season_label == as.character(current_year) && NROW(dat$events) == 15L) {
    warning(
      competition_name, " returned 15 season events, the documented free-tier limit. ",
      "Checking the separate recent and upcoming endpoints; coverage may still be incomplete."
    )
    for (route in c("eventspastleague.php", "eventsnextleague.php")) {
      extra <- fetch_api(paste0(route, "?id=", id_league))
      if (!"events" %in% names(extra)) stop("Invalid TheSportsDB response for ", route)
      if (NROW(extra$events)) dat$events <- bind_rows(extra$events, dat$events)
    }
    if (id_league %in% c(5852L, 5933L)) {
      # Query each competition separately so other rugby games do not use
      # its daily allowance. Use only dates in this season's fixture window.
      scan_until <- min(fixtures_until, as.Date(paste0(season_label, "-12-31")))
      dates <- format(seq(today, scan_until, by = "day"), "%Y-%m-%d")
      cat("Checking ", length(dates), " fixture dates for ", competition_name, "\n", sep = "")
      daily_events <- map_dfr(dates, function(day) {
        daily <- fetch_api(paste0("eventsday.php?d=", day, "&l=", id_league))
        if (!"events" %in% names(daily)) stop("Invalid TheSportsDB day response for ", day)
        if (NROW(daily$events) == 3L) {
          warning(competition_name, " on ", day, " reached the documented free daily limit. More fixtures may exist.")
        }
        as_tibble(daily$events)
      })
      dat$events <- bind_rows(daily_events, dat$events)
    }
    if ("idEvent" %in% names(dat$events)) {
      dat$events <- as_tibble(dat$events) %>% distinct(idEvent, .keep_all = TRUE)
    }
  }
  if (length(dat$events) == 0L) {
    return(tibble())
  }
  
  events <- as_tibble(dat$events)
  
  required_api_cols <- c(
    "dateEvent", "strHomeTeam", "strAwayTeam",
    "intHomeScore", "intAwayScore"
  )
  
  missing_api_cols <- setdiff(required_api_cols, names(events))
  
  if (length(missing_api_cols) > 0) {
    stop(
      "API result missing expected columns for league ", id_league,
      ", season ", season_label,
      ": ", paste(missing_api_cols, collapse = ", ")
    )
  }

  if (!"strStatus" %in% names(events)) events$strStatus <- NA_character_

  events %>%
    transmute(
      date        = normalise_date_text(dateEvent),
      home_team   = unname(team_names[team_key(strHomeTeam)]),
      away_team   = unname(team_names[team_key(strAwayTeam)]),
      home_score  = safe_int(intHomeScore),
      away_score  = safe_int(intAwayScore),
      status      = str_to_lower(str_squish(coalesce(strStatus, ""))),
      competition = competition_name
    ) %>%
    filter(!is.na(date), !is.na(home_team), !is.na(away_team), home_team != away_team) %>%
    mutate(
      result = case_when(
        as.Date(date) > today | is.na(home_score) | is.na(away_score) ~ NA_character_,
        !status %in% c("ft", "aet", "finished", "match finished", "after extra time") ~ NA_character_,
        home_score > away_score ~ home_team,
        away_score > home_score ~ away_team,
        home_score == away_score ~ "Draw"
      )
    ) %>%
    select(date, home_team, away_team, home_score, away_score, result, competition, status)
}

# -----------------------------
# Load existing data
#
# Priority:
#   1. Existing repo master file
#   2. Historical seed file
# -----------------------------
if (file.exists(out_file)) {
  cat("Loading existing master dataset\n")
  historical <- read_results_file(out_file)
} else if (file.exists(historical_csv)) {
  cat("Master dataset not found. Loading historical seed dataset\n")
  historical <- read_results_file(historical_csv) %>%
    filter(!is.na(date), date <= "2024-12-31")
} else {
  stop(
    "No master or seed dataset found. Expected one of:\n",
    out_file, "\n",
    historical_csv
  )
}

if (nrow(historical) == 0 || all(is.na(historical$date))) {
  stop("No valid dates found in the existing dataset.")
}

last_date <- max(historical$date, na.rm = TRUE)

cat("Latest match date in existing dataset: ", last_date, "\n", sep = "")
cat("Existing rows: ", nrow(historical), "\n", sep = "")

# -----------------------------
# Download API results
# -----------------------------
# The catalogue includes club competitions. The existing international-team
# allow-list in fetch_season keeps club fixtures out of the Elo pipeline.
leagues <- bind_rows(leagues, discover_leagues()) %>%
  distinct(idLeague, .keep_all = TRUE)
api_grid <- expand.grid(
  idLeague = leagues$idLeague,
  season   = seasons,
  stringsAsFactors = FALSE
)

api_events <- pmap_dfr(
  api_grid,
  function(idLeague, season) {
    comp_name <- leagues$competition[match(idLeague, leagues$idLeague)]
    
    cat("Fetching league ", idLeague, " season ", season, " - ", comp_name, "\n", sep = "")
    
    fetch_season(
      id_league = idLeague,
      season_label = season,
      competition_name = comp_name
    )
  }
)

if (!nrow(api_events)) stop("TheSportsDB returned no international fixtures. Existing data was kept.")

historical_keys_by_team <- historical %>%
  mutate(
    home_key = unname(team_names[team_key(home_team)]),
    away_key = unname(team_names[team_key(away_team)])
  ) %>%
  distinct(date, home_key, away_key, .keep_all = TRUE) %>%
  select(date, home_key, away_key, home_team, away_team, competition)

api_results <- api_events %>%
  filter(as.Date(date) >= lookback_date, !is.na(result)) %>%
  select(-status) %>%
  # Preserve the existing competition label if the feed uses a different
  # competition for this fixture. Prefer a tournament over a generic friendly.
  left_join(
    historical_keys_by_team,
    by = c("date", "home_team" = "home_key", "away_team" = "away_key"),
    suffix = c("", "_existing")
  ) %>%
  arrange(competition == "Rugby Union International Friendlies") %>%
  mutate(
    result = case_when(
      result == home_team ~ coalesce(home_team_existing, home_team),
      result == away_team ~ coalesce(away_team_existing, away_team),
      TRUE ~ result
    ),
    home_team = coalesce(home_team_existing, home_team),
    away_team = coalesce(away_team_existing, away_team),
    competition = coalesce(competition_existing, competition)
  ) %>%
  select(-ends_with("_existing")) %>%
  distinct(date, home_team, away_team, .keep_all = TRUE)

fixtures <- api_events %>%
  filter(
    as.Date(date) >= today,
    as.Date(date) <= fixtures_until,
    status %in% c("", "ns", "not started", "scheduled", "tbd"),
    is.na(home_score), is.na(away_score)
  ) %>%
  select(-status) %>%
  anti_join(bind_rows(historical, api_results), by = c("date", "home_team", "away_team")) %>%
  distinct(date, home_team, away_team, .keep_all = TRUE) %>%
  arrange(date, competition, home_team, away_team)

cat("Completed API rows fetched: ", nrow(api_results), "\n", sep = "")

# -----------------------------
# Merge logic
#
# Match key:
#   date + home_team + away_team + competition
#
# Rules:
#   - Keep existing rows
#   - Append API rows
#   - Prefer rows with real scores
#   - If both have scores, prefer API row
#   - Do not delete older rows first
# -----------------------------
master <- bind_rows(
  api_results %>% mutate(source_priority = 1L),
  historical  %>% mutate(source_priority = 2L)
) %>%
  mutate(
    has_scores = !is.na(home_score) & !is.na(away_score),
    sort_date  = suppressWarnings(ymd(date))
  ) %>%
  arrange(
    date,
    home_team,
    away_team,
    competition,
    desc(has_scores),
    source_priority
  ) %>%
  distinct(date, home_team, away_team, competition, .keep_all = TRUE) %>%
  arrange(sort_date, home_team, away_team, competition) %>%
  select(date, home_team, away_team, home_score, away_score, result, competition)

# -----------------------------
# Save updated dataset
# -----------------------------
write_csv(master, out_file, na = "")
dir.create(dirname(fixtures_file), recursive = TRUE, showWarnings = FALSE)
write_csv(fixtures, fixtures_file, na = "")
cat("Upcoming fixtures saved: ", nrow(fixtures), "\n", sep = "")

# -----------------------------
# Summary
# -----------------------------
old_n <- nrow(historical)

api_new_keys <- api_results %>%
  distinct(date, home_team, away_team, competition)

historical_keys <- historical %>%
  distinct(date, home_team, away_team, competition)

new_match_count <- anti_join(
  api_new_keys,
  historical_keys,
  by = c("date", "home_team", "away_team", "competition")
) %>%
  nrow()

score_filled_count <- historical %>%
  transmute(
    date,
    home_team,
    away_team,
    competition,
    old_has_scores = !is.na(home_score) & !is.na(away_score)
  ) %>%
  left_join(
    api_results %>%
      transmute(
        date,
        home_team,
        away_team,
        competition,
        new_has_scores = !is.na(home_score) & !is.na(away_score)
      ) %>%
      distinct(),
    by = c("date", "home_team", "away_team", "competition")
  ) %>%
  filter(!old_has_scores, new_has_scores %in% TRUE) %>%
  nrow()

duplicate_check <- master %>%
  count(date, home_team, away_team, competition, name = "n") %>%
  filter(n > 1)

cat("\nPrevious rows: ", old_n, sep = "")
cat("\nAPI completed rows fetched: ", nrow(api_results), sep = "")
cat("\nBrand new match rows added: ", new_match_count, sep = "")
cat("\nExisting match rows enriched with scores: ", score_filled_count, sep = "")
cat("\nTotal rows: ", nrow(master), sep = "")
cat("\nDuplicate rows after merge: ", nrow(duplicate_check), "\n\n", sep = "")

if (nrow(duplicate_check) > 0) {
  cat("Duplicate keys found:\n")
  print(duplicate_check, n = 100)
  stop("Stopped because duplicate match keys remain after merge.")
}

cat("Latest rows in master:\n")
print(tail(master, 20))
