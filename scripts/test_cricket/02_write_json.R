# install.packages(c("dplyr", "readr", "jsonlite", "stringi", "stringr", "tibble", "data.table"))

library(dplyr)
library(readr)
library(jsonlite)
library(stringi)
library(stringr)
library(tibble)
library(data.table)

options(stringsAsFactors = FALSE)

# ============================================================
# Export Test Cricket ratings + history to JSON for website
#
# Inputs:
#   Cricket/Test Cricket/pipeline_data/Elo/test_cricket_elo_final_ratings.csv
#   Cricket/Test Cricket/pipeline_data/Elo/test_cricket_elo_game_history.csv
#   team_flag_lookup.csv
#
# Outputs:
#   Cricket/Test Cricket/data/teams.json
#   Cricket/Test Cricket/data/era_starts.json
#   Cricket/Test Cricket/data/meta.json
#   Cricket/Test Cricket/data/history/<team_id>.json
#   Cricket/Test Cricket/data/games/<team_id>.json
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
    dir.exists(file.path(wd, "Cricket", "Test Cricket")) &&
    dir.exists(file.path(wd, "scripts", "test_cricket"))
  ) {
    return(wd)
  }
  
  local_repo <- "C:/Users/stjuk/Documents/GitHub/J-Ratings"
  
  if (dir.exists(local_repo)) {
    return(normalizePath(local_repo, winslash = "/", mustWork = TRUE))
  }
  
  stop(
    "Could not find repo root. Run this script from the J-Ratings repo root, ",
    "or set GITHUB_WORKSPACE."
  )
}

repo_dir <- get_repo_dir()

# -----------------------------
# Paths
# -----------------------------

elo_dir <- file.path(
  repo_dir,
  "Cricket",
  "pipeline_data",
  "Elo"
)

final_csv <- file.path(
  elo_dir,
  "test_cricket_elo_final_ratings.csv"
)

hist_csv <- file.path(
  elo_dir,
  "test_cricket_elo_game_history.csv"
)

flag_lookup_csv <- file.path(
  repo_dir,
  "team_flag_lookup.csv"
)

base_data_dir <- file.path(
  repo_dir,
  "Cricket",
  "data"
)

history_out <- file.path(base_data_dir, "history")
games_out <- file.path(base_data_dir, "games")

dir.create(base_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(history_out, recursive = TRUE, showWarnings = FALSE)
dir.create(games_out, recursive = TRUE, showWarnings = FALSE)

cat("Repo dir: ", repo_dir, "\n", sep = "")
cat("Elo dir: ", elo_dir, "\n", sep = "")
cat("Data output dir: ", base_data_dir, "\n", sep = "")

if (!file.exists(final_csv)) {
  stop("Missing final ratings CSV: ", final_csv)
}

if (!file.exists(hist_csv)) {
  stop("Missing game history CSV: ", hist_csv)
}

if (!file.exists(flag_lookup_csv)) {
  stop("Missing flag lookup CSV: ", flag_lookup_csv)
}

# -----------------------------
# Settings
# -----------------------------

INACTIVE_YEARS <- suppressWarnings(
  as.numeric(Sys.getenv("CRICKET_INACTIVE_YEARS", unset = "6"))
)

if (is.na(INACTIVE_YEARS) || INACTIVE_YEARS <= 0) {
  stop("CRICKET_INACTIVE_YEARS must be a positive number if supplied.")
}

USE_DATA_MAX_AS_LATEST_DATE <- TRUE
MANUAL_LATEST_DATE <- as.Date("2026-12-31")

# -----------------------------
# Helpers
# -----------------------------

slug <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

era_label <- function(d) {
  format(as.Date(d), "%Y")
}

norm_name <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
}

safe_int <- function(x) {
  suppressWarnings(as.integer(x))
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

clean_flag <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x[x %in% c("", "na", "null")] <- NA_character_
  x
}

write_json_compact <- function(x, path, na = "null") {
  write_json(
    x,
    path,
    auto_unbox = TRUE,
    pretty = FALSE,
    na = na
  )
}

elo_expected <- function(team_elo, opponent_elo) {
  1 / (1 + 10 ^ ((opponent_elo - team_elo) / 400))
}

clamp <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))
}

draw_rate_from_gap_and_year <- function(abs_gap, year) {
  abs_gap <- as.numeric(abs_gap)
  year <- as.numeric(year)
  
  # Test cricket draw model:
  # - Draws were more common in the 2000s than in recent Tests.
  # - Evenly matched teams have the highest draw probability.
  # - Larger Elo gaps reduce the draw probability.
  #
  # Era baseline for evenly matched teams:
  #   ~30% in the early 2000s
  #   ~19% around 2018
  #   ~12% by 2025
  era_max_draw <- 0.10 + 0.20 / (1 + exp((year - 2017) / 3.5))
  
  # Elo-gap multiplier:
  #   0 gap   => 100% of era baseline
  #   600 gap => about 37% of era baseline
  gap_multiplier <- exp(-abs_gap / 600)
  
  draw_rate <- era_max_draw * gap_multiplier
  
  # Safety bounds to avoid impossible or silly values in the W/D/L bar.
  pmax(0.015, pmin(0.350, draw_rate))
}

make_score <- function(result, margin, margin_type, margin_value) {
  result <- trimws(as.character(result))
  margin <- trimws(as.character(margin))
  margin_type <- trimws(as.character(margin_type))
  margin_value <- suppressWarnings(as.integer(margin_value))
  
  if (!is.na(margin) && margin != "") {
    return(margin)
  }
  
  if (is.na(result) || result == "") {
    return(NA_character_)
  }
  
  if (result == "Draw") {
    return("Draw")
  }
  
  if (result == "Not finished") {
    return("Not finished")
  }
  
  if (!is.na(margin_type) && margin_type == "innings_runs" && !is.na(margin_value)) {
    return(paste0(result, " won by an innings and ", margin_value, " runs"))
  }
  
  if (!is.na(margin_type) && margin_type == "runs" && !is.na(margin_value)) {
    return(paste0(result, " won by ", margin_value, " runs"))
  }
  
  if (!is.na(margin_type) && margin_type == "wickets" && !is.na(margin_value)) {
    return(paste0(result, " won by ", margin_value, " wickets"))
  }
  
  result
}

# -----------------------------
# Load CSVs
# -----------------------------

final <- read_csv(
  final_csv,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

ghist <- read_csv(
  hist_csv,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

flag_lookup <- read_csv(
  flag_lookup_csv,
  show_col_types = FALSE,
  locale = locale(encoding = "Windows-1252")
)

# -----------------------------
# Validate columns
# -----------------------------

required_final <- c("Team", "Rating", "Games")

required_hist <- c(
  "date",
  "competition",
  "home_team",
  "away_team",
  "result",
  "HomeRating_Before",
  "AwayRating_Before",
  "HomeRating_After",
  "AwayRating_After"
)

required_flags <- c("Team", "Flag")

miss_final <- setdiff(required_final, names(final))
miss_hist <- setdiff(required_hist, names(ghist))
miss_flags <- setdiff(required_flags, names(flag_lookup))

if (length(miss_final) > 0) {
  stop("Missing columns in final CSV: ", paste(miss_final, collapse = ", "))
}

if (length(miss_hist) > 0) {
  stop("Missing columns in history CSV: ", paste(miss_hist, collapse = ", "))
}

if (length(miss_flags) > 0) {
  stop("Missing columns in flag lookup CSV: ", paste(miss_flags, collapse = ", "))
}

optional_hist_cols <- c(
  "result_type",
  "event_name",
  "season",
  "city",
  "venue",
  "margin",
  "margin_type",
  "margin_value",
  "match_type",
  "match_type_number",
  "source_file"
)

for (col in optional_hist_cols) {
  if (!col %in% names(ghist)) {
    ghist[[col]] <- NA
  }
}

# -----------------------------
# Prepare flag lookup
# -----------------------------

flag_lookup_tbl <- flag_lookup %>%
  transmute(
    lookup_name = trimws(as.character(Team)),
    lookup_key = norm_name(Team),
    flag = clean_flag(Flag)
  ) %>%
  filter(lookup_name != "") %>%
  group_by(lookup_key) %>%
  slice_head(n = 1) %>%
  ungroup()

flag_aliases <- tibble(
  cricket_name = c(
    "England",
    "Ireland",
    "West Indies",
    "United States",
    "USA"
  ),
  lookup_name = c(
    "England",
    "INCT",
    "West Indies",
    "United States",
    "United States"
  )
) %>%
  transmute(
    cricket_key = norm_name(cricket_name),
    lookup_key = norm_name(lookup_name)
  )

get_flag_for_team <- function(team_names) {
  team_tbl <- tibble(
    team = as.character(team_names),
    team_key = norm_name(team_names)
  )
  
  out <- team_tbl %>%
    left_join(flag_aliases, by = c("team_key" = "cricket_key")) %>%
    mutate(resolved_key = if_else(!is.na(lookup_key), lookup_key, team_key)) %>%
    select(team, resolved_key) %>%
    left_join(
      flag_lookup_tbl %>% select(lookup_key, flag),
      by = c("resolved_key" = "lookup_key")
    ) %>%
    mutate(flag = if_else(is.na(flag), "", flag))
  
  out$flag
}

# -----------------------------
# Clean final ratings
# -----------------------------

final <- final %>%
  mutate(
    Team = trimws(as.character(Team)),
    Rating = safe_num(Rating),
    Games = safe_int(Games)
  ) %>%
  filter(
    Team != "",
    !is.na(Rating),
    !is.na(Games)
  )

if (nrow(final) == 0) {
  stop("No usable rows in final ratings.")
}

# -----------------------------
# Clean game history
# -----------------------------

ghist <- ghist %>%
  mutate(
    date = as.Date(date),
    competition = trimws(as.character(competition)),
    event_name = trimws(as.character(event_name)),
    season = trimws(as.character(season)),
    city = trimws(as.character(city)),
    venue = trimws(as.character(venue)),
    home_team = trimws(as.character(home_team)),
    away_team = trimws(as.character(away_team)),
    result = trimws(as.character(result)),
    result = if_else(tolower(result) %in% c("draw", "tie"), "Draw", result),
    result_type = trimws(as.character(result_type)),
    margin = trimws(as.character(margin)),
    margin_type = trimws(as.character(margin_type)),
    margin_value = safe_int(margin_value),
    match_type = trimws(as.character(match_type)),
    match_type_number = safe_int(match_type_number),
    source_file = trimws(as.character(source_file)),
    HomeRating_Before = safe_num(HomeRating_Before),
    AwayRating_Before = safe_num(AwayRating_Before),
    HomeRating_After = safe_num(HomeRating_After),
    AwayRating_After = safe_num(AwayRating_After)
  ) %>%
  filter(
    !is.na(date),
    home_team != "",
    away_team != "",
    competition != "",
    !is.na(HomeRating_Before),
    !is.na(AwayRating_Before),
    !is.na(HomeRating_After),
    !is.na(AwayRating_After)
  )

if (nrow(ghist) == 0) {
  stop("No usable rows in game history.")
}

# -----------------------------
# Latest game date
# -----------------------------

latest_game_date <- if (USE_DATA_MAX_AS_LATEST_DATE) {
  max(ghist$date, na.rm = TRUE)
} else {
  MANUAL_LATEST_DATE
}

if (is.na(latest_game_date)) {
  stop("Could not determine latest game date.")
}

# -----------------------------
# Last played per team
# -----------------------------

last_played_tbl <- bind_rows(
  ghist %>% transmute(team = home_team, last_played = date),
  ghist %>% transmute(team = away_team, last_played = date)
) %>%
  filter(team != "", !is.na(last_played)) %>%
  group_by(team) %>%
  summarise(last_played = max(last_played), .groups = "drop")

inactive_cutoff_days <- INACTIVE_YEARS * 365.25

team_status_tbl <- last_played_tbl %>%
  mutate(
    days_inactive = as.numeric(latest_game_date - last_played),
    is_active = days_inactive <= inactive_cutoff_days
  )

active_teams <- team_status_tbl %>%
  filter(is_active) %>%
  pull(team)

cat("Latest game date: ", format(latest_game_date, "%Y-%m-%d"), "\n", sep = "")
cat("Inactive threshold: ", INACTIVE_YEARS, " years\n", sep = "")
cat("Active teams: ", length(active_teams), "\n", sep = "")
cat("Inactive teams: ", nrow(team_status_tbl) - length(active_teams), "\n", sep = "")

# -----------------------------
# teams.json
# -----------------------------

teams_tbl <- final %>%
  transmute(
    name = Team,
    rating = as.integer(round(Rating)),
    games = as.integer(Games)
  ) %>%
  filter(name %in% active_teams) %>%
  mutate(
    id = slug(name),
    flag = get_flag_for_team(name)
  ) %>%
  select(id, name, flag, rating, games) %>%
  arrange(desc(rating), name)

if (nrow(teams_tbl) == 0) {
  stop("No active teams found for teams.json.")
}

if (anyDuplicated(teams_tbl$id)) {
  teams_tbl <- teams_tbl %>%
    group_by(id) %>%
    mutate(
      n = row_number(),
      id = if_else(n == 1L, id, paste0(id, "-", n - 1L))
    ) %>%
    ungroup() %>%
    select(-n)
}

write_json_compact(
  teams_tbl,
  file.path(base_data_dir, "teams.json")
)

cat("Wrote teams.json, n = ", nrow(teams_tbl), "\n", sep = "")

name_to_id <- setNames(teams_tbl$id, teams_tbl$name)
name_to_flag <- setNames(teams_tbl$flag, teams_tbl$name)
name_to_rating <- setNames(teams_tbl$rating, teams_tbl$name)

# -----------------------------
# Long rating history
# -----------------------------

hist_long_all <- bind_rows(
  ghist %>%
    transmute(
      team = home_team,
      date = date,
      era = era_label(date),
      rating = HomeRating_After,
      competition = competition
    ),
  ghist %>%
    transmute(
      team = away_team,
      date = date,
      era = era_label(date),
      rating = AwayRating_After,
      competition = competition
    )
) %>%
  filter(
    !is.na(rating),
    team != "",
    !is.na(date)
  ) %>%
  arrange(team, date) %>%
  group_by(team, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

if (nrow(hist_long_all) == 0) {
  stop("No usable rating history rows.")
}

# -----------------------------
# Historical world rank
# -----------------------------

cat("Building historical world ranks...\n")

rank_dt <- as.data.table(hist_long_all)
rank_dt[, date := as.Date(date)]
setorder(rank_dt, team, date)

rank_dates <- sort(unique(rank_dt$date))
inactive_days_int <- as.integer(round(INACTIVE_YEARS * 365.25))

rank_rows <- vector("list", length(rank_dates))

for (i in seq_along(rank_dates)) {
  d <- rank_dates[i]
  cutoff <- d - inactive_days_int
  
  current_ratings <- rank_dt[
    date <= d & date >= cutoff,
    .SD[.N],
    by = team
  ]
  
  if (nrow(current_ratings) > 0) {
    setorder(current_ratings, -rating, team)
    current_ratings[, rank := frank(-rating, ties.method = "min")]
    current_ratings[, rank_date := d]
    
    rank_rows[[i]] <- current_ratings[, .(
      team,
      rank_date,
      rank = as.integer(rank)
    )]
  } else {
    rank_rows[[i]] <- data.table(
      team = character(),
      rank_date = as.Date(character()),
      rank = integer()
    )
  }
  
  if (i %% 250L == 0L) {
    cat("Rank dates processed: ", i, " of ", length(rank_dates), "\n", sep = "")
  }
}

rank_tbl <- bind_rows(rank_rows) %>%
  mutate(rank_date = as.Date(rank_date))

hist_long <- hist_long_all %>%
  left_join(
    rank_tbl,
    by = c("team" = "team", "date" = "rank_date")
  ) %>%
  filter(team %in% active_teams)

cat("Historical rank rows: ", nrow(rank_tbl), "\n", sep = "")

# -----------------------------
# era_starts.json
# -----------------------------

era_starts_long <- bind_rows(
  ghist %>%
    transmute(
      team = home_team,
      date = date,
      era = era_label(date),
      elo = HomeRating_Before,
      competition = competition
    ),
  ghist %>%
    transmute(
      team = away_team,
      date = date,
      era = era_label(date),
      elo = AwayRating_Before,
      competition = competition
    )
) %>%
  filter(
    team != "",
    !is.na(date),
    !is.na(elo),
    !is.na(era)
  ) %>%
  filter(team %in% active_teams) %>%
  arrange(team, era, date) %>%
  group_by(team, era) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    id = unname(name_to_id[team]),
    flag = unname(name_to_flag[team]),
    elo = as.integer(round(elo))
  ) %>%
  filter(!is.na(id)) %>%
  mutate(flag = if_else(is.na(flag), "", flag)) %>%
  transmute(
    era = as.character(era),
    id = as.character(id),
    name = as.character(team),
    flag = as.character(flag),
    elo = as.integer(elo),
    competition = as.character(competition)
  ) %>%
  arrange(desc(era), desc(elo), name)

write_json_compact(
  era_starts_long,
  file.path(base_data_dir, "era_starts.json")
)

cat("Wrote era_starts.json, rows = ", nrow(era_starts_long), "\n", sep = "")

# -----------------------------
# meta.json
# -----------------------------

flagged_teams <- sum(teams_tbl$flag != "", na.rm = TRUE)
unflagged_teams <- sum(teams_tbl$flag == "", na.rm = TRUE)

meta <- list(
  sport = "Test Cricket",
  latest_game_date = format(latest_game_date, "%Y-%m-%d"),
  asof = format(latest_game_date, "%Y-%m-%d"),
  games = nrow(ghist),
  inactive_years = INACTIVE_YEARS,
  inactive_cutoff_days = unname(inactive_cutoff_days),
  total_teams_seen = nrow(team_status_tbl),
  active_teams = nrow(teams_tbl),
  inactive_teams = nrow(team_status_tbl) - nrow(teams_tbl),
  flagged_teams = flagged_teams,
  unflagged_teams = unflagged_teams,
  history_has_world_rank = TRUE,
  games_have_world_rank = TRUE,
  games_have_expected_wdl = TRUE,
  source = "Cricsheet men's Test match JSON",
  excluded_teams = list(
    "ICC World XI"
  ),
  rank_method = paste0(
    "Ranked by latest known Elo on each match date. ",
    "Teams inactive for more than ", INACTIVE_YEARS,
    " years at that date are excluded."
  ),
  expected_wdl_method = paste0(
    "Elo expected score converted into win/draw/loss using a Test cricket draw-rate curve. ",
    "Draw probability depends on both match year and Elo gap: the era baseline is higher in the 2000s ",
    "and lower in recent Tests, while larger Elo gaps reduce the draw probability."
  ),
  score_method = "Score field uses Cricsheet outcome margin text rather than innings-by-innings scorecards. Not finished matches are included in team game logs but carry no rating delta.",
  input_files = list(
    final_ratings = basename(final_csv),
    game_history = basename(hist_csv),
    flag_lookup = basename(flag_lookup_csv)
  )
)

write_json(
  meta,
  file.path(base_data_dir, "meta.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

cat("Wrote meta.json\n")
cat("Teams with flags: ", flagged_teams, "\n", sep = "")
cat("Teams without flags: ", unflagged_teams, "\n", sep = "")

# -----------------------------
# Per-team rating history JSON
#
# Output:
#   [{date, era, rating, rank, competition}]
# -----------------------------

n_hist_written <- 0L

for (tm in names(name_to_id)) {
  id <- name_to_id[[tm]]
  
  df <- hist_long %>%
    filter(team == tm) %>%
    arrange(date) %>%
    transmute(
      date = format(date, "%Y-%m-%d"),
      era = as.character(era),
      rating = as.integer(round(rating)),
      rank = as.integer(rank),
      competition = as.character(competition)
    )
  
  if (nrow(df) > 0) {
    write_json_compact(
      df,
      file.path(history_out, paste0(id, ".json")),
      na = "null"
    )
    
    n_hist_written <- n_hist_written + 1L
  }
}

cat("Wrote rating history files: ", n_hist_written, "\n", sep = "")

# -----------------------------
# Per-team games JSON
#
# Output:
#   [{
#     date, era, competition, tournament,
#     home, homeElo,
#     away, awayElo,
#     result, score, delta,
#     homeWinPct, drawPct, awayWinPct,
#     rank, status,
#     venue, city, season
#   }]
# -----------------------------

team_rank_lookup <- hist_long %>%
  select(team, date, rank) %>%
  filter(!is.na(rank)) %>%
  arrange(team, date) %>%
  group_by(team, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

n_games_written <- 0L

for (tm in names(name_to_id)) {
  id <- name_to_id[[tm]]
  
  df <- ghist %>%
    filter(home_team == tm | away_team == tm) %>%
    arrange(desc(date)) %>%
    mutate(
      era = era_label(date),
      
      is_finished = case_when(
        result == "Not finished" ~ FALSE,
        result_type == "N" ~ FALSE,
        TRUE ~ TRUE
      ),
      
      delta_num = case_when(
        !is_finished ~ NA_real_,
        home_team == tm ~ HomeRating_After - HomeRating_Before,
        away_team == tm ~ AwayRating_After - AwayRating_Before,
        TRUE ~ NA_real_
      ),
      delta = ifelse(
        !is.na(delta_num),
        sprintf("%+0.1f", round(delta_num, 1)),
        NA_character_
      ),
      
      home_expected = elo_expected(HomeRating_Before, AwayRating_Before),
      away_expected = 1 - home_expected,
      
      match_year = lubridate::year(date),
      abs_elo_gap = abs(HomeRating_Before - AwayRating_Before),
      draw_prob = draw_rate_from_gap_and_year(abs_elo_gap, match_year),
      
      home_win_prob = home_expected - draw_prob / 2,
      away_win_prob = away_expected - draw_prob / 2,
      
      home_win_prob = clamp(home_win_prob, 0, 1 - draw_prob),
      away_win_prob = 1 - draw_prob - home_win_prob,
      
      score = mapply(
        function(result, margin, margin_type, margin_value, is_finished) {
          if (is.na(is_finished) || !is_finished) {
            return("Not finished")
          }
          
          make_score(
            result = result,
            margin = margin,
            margin_type = margin_type,
            margin_value = margin_value
          )
        },
        result,
        margin,
        margin_type,
        margin_value,
        is_finished,
        USE.NAMES = FALSE
      )
    ) %>%
    left_join(
      team_rank_lookup %>%
        filter(team == tm) %>%
        select(date, rank),
      by = "date"
    ) %>%
    transmute(
      date = format(date, "%Y-%m-%d"),
      era = as.character(era),
      competition = as.character(competition),
      tournament = as.character(if_else(
        !is.na(event_name) & event_name != "",
        event_name,
        competition
      )),
      
      home = as.character(home_team),
      homeElo = as.integer(round(HomeRating_Before)),
      
      away = as.character(away_team),
      awayElo = as.integer(round(AwayRating_Before)),
      
      result = as.character(result),
      score = as.character(score),
      delta = as.character(delta),
      
      homeWinPct = as.integer(round(100 * home_win_prob)),
      drawPct = as.integer(round(100 * draw_prob)),
      awayWinPct = as.integer(round(100 * away_win_prob)),
      
      rank = as.integer(rank),
      status = if_else(is_finished, "completed", "not_finished"),
      
      venue = as.character(venue),
      city = as.character(city),
      season = as.character(season),
      matchTypeNumber = as.integer(match_type_number)
    )
  
  if (nrow(df) > 0) {
    write_json_compact(
      df,
      file.path(games_out, paste0(id, ".json")),
      na = "null"
    )
    
    n_games_written <- n_games_written + 1L
  }
}

cat("Wrote games files: ", n_games_written, "\n", sep = "")

# -----------------------------
# Final checks
# -----------------------------

expected_outputs <- c(
  file.path(base_data_dir, "meta.json"),
  file.path(base_data_dir, "teams.json"),
  file.path(base_data_dir, "era_starts.json")
)

missing_outputs <- expected_outputs[!file.exists(expected_outputs)]

if (length(missing_outputs) > 0) {
  stop("Missing expected JSON outputs: ", paste(missing_outputs, collapse = ", "))
}

if (n_hist_written == 0) {
  stop("No history JSON files were written.")
}

if (n_games_written == 0) {
  stop("No games JSON files were written.")
}

cat("Done.\n")
cat("JSON output directory: ", base_data_dir, "\n", sep = "")