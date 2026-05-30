```r
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

source_results_csv <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "source",
  "all_matches.csv"
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

normalise_team_name <- function(x) {
  x0 <- trimws(as.character(x))
  
  team_map <- c(
    "Åland" = "Åland Islands"
  )
  
  ifelse(x0 %in% names(team_map), unname(team_map[x0]), x0)
}

era_label <- function(d) {
  format(as.Date(d), "%Y")
}

clean_flag <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x[x %in% c("", "na", "null")] <- NA_character_
  x
}

parse_mixed_date <- function(x) {
  x_chr <- trimws(as.character(x))
  
  out <- rep(as.Date(NA), length(x_chr))
  
  is_blank <- is.na(x_chr) | x_chr == ""
  is_excel_num <- grepl("^[0-9]+$", x_chr)
  is_dmy <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x_chr)
  is_ymd <- grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", x_chr)
  
  out[!is_blank & is_excel_num] <- as.Date(
    as.numeric(x_chr[!is_blank & is_excel_num]),
    origin = "1899-12-30"
  )
  
  out[!is_blank & is_dmy] <- as.Date(
    x_chr[!is_blank & is_dmy],
    format = "%d/%m/%Y"
  )
  
  out[!is_blank & is_ymd] <- as.Date(
    x_chr[!is_blank & is_ymd],
    format = "%Y-%m-%d"
  )
  
  out
}

clean_score_num <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null", "na")] <- NA_character_
  suppressWarnings(as.integer(x_chr))
}

elo_expected <- function(team_elo, opponent_elo) {
  1 / (1 + 10 ^ ((opponent_elo - team_elo) / 400))
}

clamp <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))
}

draw_rate_from_gap <- function(abs_gap) {
  abs_gap <- as.numeric(abs_gap)
  
  max_draw <- 0.266
  midpoint <- 295.472
  scale <- 68.851
  
  max_draw / (1 + exp((abs_gap - midpoint) / scale))
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
    Team = normalise_team_name(Team),
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
    home_team = normalise_team_name(home_team),
    away_team = normalise_team_name(away_team),
    HomeFlag = clean_flag(HomeFlag),
    AwayFlag = clean_flag(AwayFlag),
    tournament = trimws(as.character(tournament)),
    result = trimws(as.character(result)),
    result = if_else(tolower(result) == "draw", "Draw", normalise_team_name(result)),
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
name_to_rating <- setNames(teams_all$rating, teams_all$name)

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
# Future fixtures
# -----------------------------
# The new source usually contains completed matches only.
# This section is kept so the website still supports scheduled fixtures
# if all_matches.csv ever includes future rows with blank scores.

future_fixtures <- tibble()

if (file.exists(source_results_csv)) {
  source_results <- read_csv(
    source_results_csv,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  )
  
  required_source_cols <- c(
    "date",
    "home_team",
    "away_team",
    "home_score",
    "away_score",
    "tournament"
  )
  
  missing_source_cols <- setdiff(required_source_cols, names(source_results))
  
  if (length(missing_source_cols) > 0) {
    warning(
      "Skipping future fixtures because all_matches.csv is missing columns: ",
      paste(missing_source_cols, collapse = ", ")
    )
  } else {
    future_fixtures <- source_results %>%
      mutate(
        date = parse_mixed_date(date),
        home_team = normalise_team_name(home_team),
        away_team = normalise_team_name(away_team),
        home_score = clean_score_num(home_score),
        away_score = clean_score_num(away_score),
        tournament = trimws(as.character(tournament))
      ) %>%
      filter(
        !is.na(date),
        date > asof_date,
        home_team != "",
        away_team != "",
        is.na(home_score),
        is.na(away_score)
      ) %>%
      mutate(
        HomeRating_Before = as.numeric(unname(name_to_rating[home_team])),
        AwayRating_Before = as.numeric(unname(name_to_rating[away_team])),
        home_id = unname(name_to_id[home_team]),
        away_id = unname(name_to_id[away_team])
      ) %>%
      filter(
        is.finite(HomeRating_Before),
        is.finite(AwayRating_Before),
        !is.na(home_id),
        !is.na(away_id)
      ) %>%
      mutate(
        era = era_label(date),
        
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
        
        result = NA_character_,
        score = NA_character_,
        delta = NA_character_,
        
        homeWinPct = as.integer(round(100 * home_win_prob)),
        drawPct = as.integer(round(100 * draw_prob)),
        awayWinPct = as.integer(round(100 * away_win_prob)),
        
        status = "scheduled"
      )
  }
} else {
  warning("No source all_matches.csv found, so no future fixtures were added: ", source_results_csv)
}

cat("Future fixtures loaded:", nrow(future_fixtures), "\n")

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
      awayWinPct = as.integer(round(100 * away_win_prob)),
      
      status = "completed"
    )
  
  scheduled_df <- future_fixtures %>%
    filter(home == tm | away == tm)
  
  df <- bind_rows(df, scheduled_df) %>%
    arrange(desc(as.Date(date)))
  
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
```
