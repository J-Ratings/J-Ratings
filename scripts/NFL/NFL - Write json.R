# ============================================================
# Export NFL ratings + history to JSON for website
# Single global Elo pool (NFL)
#
# Inputs:
#   - nfl_elo_final_ratings.csv
#   - nfl_elo_game_history.csv
#
# Outputs (under J-Ratings/sports/NFL/data):
#   - teams.json                 (current rating)
#   - era_starts.json            (team Elo at start of each season)
#   - history/<team_id>.json     (date, era, rating, tournament)
#   - games/<team_id>.json       (per-game list with delta + tournament)
#
# Notes:
#   - "era" = NFL season
#   - history rating = post-match Elo on that date
#   - era_starts elo = pre-match Elo from team's first match in that season
# ============================================================

library(dplyr)
library(readr)
library(jsonlite)
library(stringi)
library(stringr)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
repo_dir <- "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"
src_dir  <- file.path(repo_dir, "NFL", "outputs")

base_data_dir <- file.path(repo_dir, "NFL", "data")
history_out   <- file.path(base_data_dir, "history")
games_out     <- file.path(base_data_dir, "games")

dir.create(base_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(history_out,   recursive = TRUE, showWarnings = FALSE)
dir.create(games_out,     recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Helpers
# -----------------------------
slug <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

# NFL uses season as the era bucket
era_label <- function(season_value) {
  as.character(season_value)
}

# Optional: map abbreviations to display names for nicer website output.
# If you prefer abbreviations everywhere, leave as-is.
display_team_name <- function(x) {
  x0 <- trimws(as.character(x))
  
  team_map <- c(
    "ARI" = "Arizona Cardinals",
    "ATL" = "Atlanta Falcons",
    "BAL" = "Baltimore Ravens",
    "BUF" = "Buffalo Bills",
    "CAR" = "Carolina Panthers",
    "CHI" = "Chicago Bears",
    "CIN" = "Cincinnati Bengals",
    "CLE" = "Cleveland Browns",
    "DAL" = "Dallas Cowboys",
    "DEN" = "Denver Broncos",
    "DET" = "Detroit Lions",
    "GB"  = "Green Bay Packers",
    "HOU" = "Houston Texans",
    "IND" = "Indianapolis Colts",
    "JAX" = "Jacksonville Jaguars",
    "KC"  = "Kansas City Chiefs",
    "LV"  = "Las Vegas Raiders",
    "LAC" = "Los Angeles Chargers",
    "LA"  = "Los Angeles Rams",   # nflverse often uses LA for Rams
    "MIA" = "Miami Dolphins",
    "MIN" = "Minnesota Vikings",
    "NE"  = "New England Patriots",
    "NO"  = "New Orleans Saints",
    "NYG" = "New York Giants",
    "NYJ" = "New York Jets",
    "PHI" = "Philadelphia Eagles",
    "PIT" = "Pittsburgh Steelers",
    "SEA" = "Seattle Seahawks",
    "SF"  = "San Francisco 49ers",
    "TB"  = "Tampa Bay Buccaneers",
    "TEN" = "Tennessee Titans",
    "WAS" = "Washington Commanders",
    
    # Historical aliases (leave distinct if you want historical identity preserved)
    "OAK" = "Oakland Raiders",
    "SD"  = "San Diego Chargers",
    "STL" = "St. Louis Rams",
    "WSH" = "Washington"
  )
  
  ifelse(x0 %in% names(team_map), unname(team_map[x0]), x0)
}

# -----------------------------
# Load CSVs
# -----------------------------
final_csv <- file.path(src_dir, "nfl_elo_final_ratings.csv")
hist_csv  <- file.path(src_dir, "nfl_elo_game_history.csv")

if (!file.exists(final_csv)) stop("Missing file: ", final_csv)
if (!file.exists(hist_csv))  stop("Missing file: ", hist_csv)

final <- read_csv(final_csv, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
ghist <- read_csv(hist_csv,  show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# -----------------------------
# Basic checks
# -----------------------------
required_final <- c("Team", "Rating", "Games")
required_hist  <- c(
  "date", "season", "tournament", "home_team", "away_team", "result",
  "HomeRating_Before", "AwayRating_Before", "HomeRating_After", "AwayRating_After"
)

miss_final <- setdiff(required_final, names(final))
miss_hist  <- setdiff(required_hist, names(ghist))

if (length(miss_final) > 0) stop("Missing columns in final CSV: ", paste(miss_final, collapse = ", "))
if (length(miss_hist) > 0)  stop("Missing columns in history CSV: ", paste(miss_hist, collapse = ", "))

ghist <- ghist %>%
  mutate(
    date       = as.Date(date),
    season     = as.integer(season),
    home_team  = trimws(as.character(home_team)),
    away_team  = trimws(as.character(away_team)),
    tournament = trimws(as.character(tournament)),
    result     = trimws(as.character(result)),
    game_type  = if ("game_type" %in% names(.)) trimws(as.character(game_type)) else NA_character_
  ) %>%
  filter(!is.na(date), !is.na(season), home_team != "", away_team != "")

if (nrow(ghist) == 0L) stop("No rows in game history after cleaning.")

# -----------------------------
# meta.json (for Home page "Updated to")
# -----------------------------
asof <- max(ghist$date, na.rm = TRUE)

write_json(
  list(asof = format(asof, "%Y-%m-%d")),
  file.path(base_data_dir, "meta.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote meta.json (asof =", format(asof, "%Y-%m-%d"), ")\n")

# -----------------------------
# teams.json (current rating)
# -----------------------------
teams_tbl <- final %>%
  transmute(
    code   = trimws(as.character(Team)),
    name   = display_team_name(Team),
    rating = as.integer(round(Rating)),
    games  = as.integer(Games)
  ) %>%
  mutate(
    id = slug(code)  # use code for stable ids (e.g. "ne", "gb", "dal")
  ) %>%
  select(id, code, name, rating, games) %>%
  arrange(desc(rating), name)

# De-duplicate IDs if needed (rare)
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

write_json(
  teams_tbl,
  file.path(base_data_dir, "teams.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote teams.json (n =", nrow(teams_tbl), ")\n")

code_to_id   <- setNames(teams_tbl$id, teams_tbl$code)
code_to_name <- setNames(teams_tbl$name, teams_tbl$code)

# -----------------------------
# Long rating history
# history uses AFTER-match rating
# Output fields later: date, era, rating, tournament
# -----------------------------
hist_long <- bind_rows(
  ghist %>%
    transmute(
      team       = home_team,
      date       = date,
      season     = season,
      era        = era_label(season),
      rating     = HomeRating_After,
      tournament = tournament
    ),
  ghist %>%
    transmute(
      team       = away_team,
      date       = date,
      season     = season,
      era        = era_label(season),
      rating     = AwayRating_After,
      tournament = tournament
    )
) %>%
  filter(!is.na(rating), team != "", !is.na(season)) %>%
  arrange(team, date, season) %>%
  group_by(team, date) %>%
  slice_tail(n = 1) %>%   # keep last rating that day if multiple games (rare)
  ungroup()

# -----------------------------
# era_starts.json
# Elo at START of season = pre-match Elo in team's first game that season
# -----------------------------
era_starts_long <- bind_rows(
  ghist %>%
    transmute(
      team       = home_team,
      date       = date,
      season     = season,
      era        = era_label(season),
      elo        = HomeRating_Before,
      tournament = tournament
    ),
  ghist %>%
    transmute(
      team       = away_team,
      date       = date,
      season     = season,
      era        = era_label(season),
      elo        = AwayRating_Before,
      tournament = tournament
    )
) %>%
  filter(team != "", !is.na(date), !is.na(season), !is.na(elo), !is.na(era)) %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    id        = unname(code_to_id[team]),
    name      = unname(code_to_name[team]),
    elo       = as.integer(round(elo)),
    season    = as.integer(season)
  ) %>%
  filter(!is.na(id)) %>%
  transmute(
    era        = as.character(era),
    season     = season,
    id         = as.character(id),
    code       = as.character(team),
    name       = as.character(name),
    elo        = as.integer(elo),
    tournament = as.character(tournament)
  ) %>%
  arrange(desc(season), desc(elo), name)

write_json(
  era_starts_long,
  file.path(base_data_dir, "era_starts.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote era_starts.json (rows =", nrow(era_starts_long), ")\n")

# -----------------------------
# Per-team rating history JSON
# Output: [{date, era, rating, tournament}]
# -----------------------------
n_hist_written <- 0L

for (tm in names(code_to_id)) {
  id <- code_to_id[[tm]]
  
  df <- hist_long %>%
    filter(team == tm) %>%
    arrange(date) %>%
    transmute(
      date       = format(date, "%Y-%m-%d"),
      era        = as.character(era),
      rating     = as.integer(round(rating)),
      tournament = as.character(tournament)
    )
  
  if (nrow(df) > 0) {
    write_json(
      df,
      file.path(history_out, paste0(id, ".json")),
      auto_unbox = TRUE,
      pretty = FALSE
    )
    n_hist_written <- n_hist_written + 1L
  }
}

cat("Wrote rating history files:", n_hist_written, "\n")

# -----------------------------
# Per-team games JSON
# Output:
# [{date, era, tournament, home, homeElo, away, awayElo, result, delta}]
#
# delta is from the perspective of the team whose file it is
# -----------------------------
n_games_written <- 0L

for (tm in names(code_to_id)) {
  id <- code_to_id[[tm]]
  
  df <- ghist %>%
    filter(home_team == tm | away_team == tm) %>%
    arrange(desc(date)) %>%
    mutate(
      era = era_label(season),
      delta_num = case_when(
        home_team == tm ~ HomeRating_After - HomeRating_Before,
        away_team == tm ~ AwayRating_After - AwayRating_Before,
        TRUE ~ NA_real_
      ),
      delta = ifelse(
        !is.na(delta_num),
        sprintf("%+0.1f", round(delta_num, 1)),
        NA_character_
      ),
      home_name = display_team_name(home_team),
      away_name = display_team_name(away_team)
    ) %>%
    transmute(
      date       = format(date, "%Y-%m-%d"),
      era        = as.character(era),
      season     = as.integer(season),
      tournament = as.character(tournament),
      home       = as.character(home_name),
      homeCode   = as.character(home_team),
      homeElo    = as.integer(round(HomeRating_Before)),
      away       = as.character(away_name),
      awayCode   = as.character(away_team),
      awayElo    = as.integer(round(AwayRating_Before)),
      result     = as.character(result),
      delta      = as.character(delta)
    )
  
  if (nrow(df) > 0) {
    write_json(
      df,
      file.path(games_out, paste0(id, ".json")),
      auto_unbox = TRUE,
      pretty = FALSE
    )
    n_games_written <- n_games_written + 1L
  }
}

cat("Wrote games files:", n_games_written, "\n")
cat("Done.\n")