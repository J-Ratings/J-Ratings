library(dplyr)
library(readr)
library(jsonlite)
library(stringi)
library(stringr)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------

repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

src_dir <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "Elo"
)

base_data_dir <- file.path(
  repo_dir,
  "InternationalFootball",
  "data"
)

history_out <- file.path(base_data_dir, "history")
games_out <- file.path(base_data_dir, "games")

dir.create(base_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(history_out, recursive = TRUE, showWarnings = FALSE)
dir.create(games_out, recursive = TRUE, showWarnings = FALSE)

final_csv <- file.path(src_dir, "world_football_elo_final_ratings.csv")
hist_csv <- file.path(src_dir, "world_football_elo_game_history.csv")

if (!file.exists(final_csv)) {
  stop("Missing file: ", final_csv)
}

if (!file.exists(hist_csv)) {
  stop("Missing file: ", hist_csv)
}

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

clean_flag <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x[x %in% c("", "na", "null")] <- NA_character_
  x
}

elo_expected <- function(team_elo, opponent_elo) {
  1 / (1 + 10 ^ ((opponent_elo - team_elo) / 400))
}

clamp <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))
}

draw_rate_from_gap <- function(abs_gap) {
  abs_gap <- as.numeric(abs_gap)
  
  # Calibrated from International Football match data, last 50 years.
  # Buckets are absolute pre-match Elo gap bands.
  gap_mid <- c(
    12, 37, 62, 87, 112, 137, 162, 187, 212, 237,
    262, 287, 312, 337, 362, 387, 412, 437, 462, 487,
    512, 537, 562, 587, 625
  )
  
  draw_rate <- c(
    0.265, 0.262, 0.261, 0.267, 0.252,
    0.252, 0.241, 0.230, 0.225, 0.202,
    0.193, 0.156, 0.183, 0.171, 0.170,
    0.134, 0.112, 0.087, 0.077, 0.060,
    0.051, 0.074, 0.053, 0.055, 0.014
  )
  
  out <- approx(
    x = gap_mid,
    y = draw_rate,
    xout = abs_gap,
    rule = 2
  )$y
  
  clamp(out, 0.014, 0.267)
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

# -----------------------------
# Validate columns
# -----------------------------

required_final <- c("Team", "Flag", "Rating", "Games")

required_hist <- c(
  "date",
  "tournament",
  "home_team",
  "HomeFlag",
  "away_team",
  "AwayFlag",
  "result",
  "score",
  "HomeRating_Before",
  "AwayRating_Before",
  "HomeRating_After",
  "AwayRating_After"
)

miss_final <- setdiff(required_final, names(final))
miss_hist <- setdiff(required_hist, names(ghist))

if (length(miss_final) > 0) {
  stop("Missing columns in final CSV: ", paste(miss_final, collapse = ", "))
}

if (length(miss_hist) > 0) {
  stop("Missing columns in history CSV: ", paste(miss_hist, collapse = ", "))
}

# -----------------------------
# Prepare data
# -----------------------------

final <- final %>%
  mutate(
    Team = trimws(as.character(Team)),
    Flag = clean_flag(Flag),
    Rating = as.numeric(Rating),
    Games = as.integer(Games)
  ) %>%
  filter(
    Team != "",
    !is.na(Rating),
    !is.na(Games)
  )

ghist <- ghist %>%
  mutate(
    date = as.Date(date),
    home_team = trimws(as.character(home_team)),
    away_team = trimws(as.character(away_team)),
    HomeFlag = clean_flag(HomeFlag),
    AwayFlag = clean_flag(AwayFlag),
    tournament = trimws(as.character(tournament)),
    result = trimws(as.character(result)),
    score = trimws(as.character(score)),
    HomeRating_Before = as.numeric(HomeRating_Before),
    AwayRating_Before = as.numeric(AwayRating_Before),
    HomeRating_After = as.numeric(HomeRating_After),
    AwayRating_After = as.numeric(AwayRating_After)
  ) %>%
  filter(
    !is.na(date),
    home_team != "",
    away_team != ""
  )

if (nrow(final) == 0) {
  stop("Final ratings CSV has no usable rows.")
}

if (nrow(ghist) == 0) {
  stop("Game history CSV has no usable rows.")
}

# -----------------------------
# Last played per team
# -----------------------------

team_last <- bind_rows(
  ghist %>% transmute(team = home_team, date = date),
  ghist %>% transmute(team = away_team, date = date)
) %>%
  filter(!is.na(date), team != "") %>%
  group_by(team) %>%
  summarise(last_played = max(date), .groups = "drop") %>%
  transmute(
    name = team,
    last_played = as.Date(last_played)
  )

# -----------------------------
# meta.json
# -----------------------------

asof_date <- max(ghist$date, na.rm = TRUE)

meta <- list(
  asof = format(asof_date, "%Y-%m-%d"),
  games = nrow(ghist)
)

write_json(
  meta,
  file.path(base_data_dir, "meta.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote meta.json (asof =", meta$asof, ")\n")

# -----------------------------
# teams.json
# -----------------------------

teams_all <- final %>%
  transmute(
    name = trimws(as.character(Team)),
    flag = clean_flag(Flag),
    rating = as.integer(round(Rating)),
    games = as.integer(Games)
  ) %>%
  left_join(team_last, by = "name") %>%
  mutate(
    id = slug(name)
  ) %>%
  select(id, name, flag, rating, games, last_played) %>%
  arrange(desc(rating), name)

if (anyDuplicated(teams_all$id)) {
  teams_all <- teams_all %>%
    group_by(id) %>%
    mutate(
      n = row_number(),
      id = if_else(n == 1L, id, paste0(id, "-", n - 1L))
    ) %>%
    ungroup() %>%
    select(-n)
}

cutoff_date <- as.Date(asof_date) - 365L * 5L

teams_tbl <- teams_all %>%
  filter(!is.na(last_played) & last_played >= cutoff_date) %>%
  select(id, name, flag, rating, games) %>%
  arrange(desc(rating), name)

write_json(
  teams_tbl,
  file.path(base_data_dir, "teams.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat(
  "Wrote teams.json (n =", nrow(teams_tbl),
  ", cutoff =", format(cutoff_date, "%Y-%m-%d"), ")\n"
)

name_to_id <- setNames(teams_all$id, teams_all$name)
name_to_flag <- setNames(teams_all$flag, teams_all$name)

# -----------------------------
# Long rating history
# -----------------------------

hist_long <- bind_rows(
  ghist %>%
    transmute(
      team = home_team,
      date = date,
      era = era_label(date),
      rating = HomeRating_After,
      tournament = tournament
    ),
  ghist %>%
    transmute(
      team = away_team,
      date = date,
      era = era_label(date),
      rating = AwayRating_After,
      tournament = tournament
    )
) %>%
  filter(
    !is.na(rating),
    team != ""
  ) %>%
  arrange(team, date) %>%
  group_by(team, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# -----------------------------
# era_starts.json
# -----------------------------

era_starts_long <- bind_rows(
  ghist %>%
    transmute(
      team = home_team,
      flag = HomeFlag,
      date = date,
      era = era_label(date),
      elo = HomeRating_Before,
      tournament = tournament
    ),
  ghist %>%
    transmute(
      team = away_team,
      flag = AwayFlag,
      date = date,
      era = era_label(date),
      elo = AwayRating_Before,
      tournament = tournament
    )
) %>%
  filter(
    team != "",
    !is.na(date),
    !is.na(elo),
    !is.na(era)
  ) %>%
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
  transmute(
    era = as.character(era),
    id = as.character(id),
    name = as.character(team),
    flag = as.character(flag),
    elo = as.integer(elo),
    tournament = as.character(tournament)
  ) %>%
  arrange(desc(era), desc(elo), name)

write_json(
  era_starts_long,
  file.path(base_data_dir, "era_starts.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote era_starts.json (rows =", nrow(era_starts_long), ")\n")

# -----------------------------
# Per-team rating history JSON
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
# -----------------------------

n_games_written <- 0L

for (tm in names(name_to_id)) {
  id <- name_to_id[[tm]]
  
  df <- ghist %>%
    filter(home_team == tm | away_team == tm) %>%
    arrange(desc(date)) %>%
    mutate(
      era = era_label(date),
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
      
      home_expected = elo_expected(HomeRating_Before, AwayRating_Before),
      away_expected = 1 - home_expected,
      
      abs_elo_gap = abs(HomeRating_Before - AwayRating_Before),
      draw_prob = draw_rate_from_gap(abs_elo_gap),
      
      home_win_prob = home_expected - draw_prob / 2,
      away_win_prob = away_expected - draw_prob / 2,
      
      home_win_prob = clamp(home_win_prob, 0, 1 - draw_prob),
      away_win_prob = 1 - draw_prob - home_win_prob
    ) %>%
    transmute(
      date = format(date, "%Y-%m-%d"),
      era = as.character(era),
      tournament = as.character(tournament),
      
      home = as.character(home_team),
      homeElo = as.integer(round(HomeRating_Before)),
      
      away = as.character(away_team),
      awayElo = as.integer(round(AwayRating_Before)),
      
      result = as.character(result),
      score = as.character(score),
      delta = as.character(delta),
      
      homeWinPct = as.integer(round(100 * home_win_prob)),
      drawPct = as.integer(round(100 * draw_prob)),
      awayWinPct = as.integer(round(100 * away_win_prob))
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

cat("Done.\n")
cat("JSON output directory:", base_data_dir, "\n")