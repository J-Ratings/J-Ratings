library(dplyr)
library(readr)
library(jsonlite)
library(stringi)
library(stringr)
library(tibble)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------

repo_dir <- normalizePath(
  Sys.getenv(
    "J_RATINGS_REPO",
    "C:/Users/stjuk/Documents/GitHub/J-Ratings"
  ),
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

manual_games_csv <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "source",
  "manual_games.csv"
)

tournament_registry_file <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "reference",
  "tournament_registry.csv"
)

tournament_override_file <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "reference",
  "tournament_edition_overrides.csv"
)

base_data_dir <- file.path(
  repo_dir,
  "InternationalFootball",
  "data"
)

history_out <- file.path(base_data_dir, "history")
games_out <- file.path(base_data_dir, "games")
tournaments_out <- file.path(base_data_dir, "tournaments")

dir.create(base_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(history_out, recursive = TRUE, showWarnings = FALSE)
dir.create(games_out, recursive = TRUE, showWarnings = FALSE)
dir.create(tournaments_out, recursive = TRUE, showWarnings = FALSE)

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
    "Åland" = "Åland Islands",
    "Türkiye" = "Turkey",
    "Turkiye" = "Turkey",
    "Curaçao" = "Curacao",
    "Côte d'Ivoire" = "Ivory Coast",
    "Cote d'Ivoire" = "Ivory Coast",
    "Congo DR" = "DR Congo",
    "Democratic Republic of the Congo" = "DR Congo",
    "USA" = "United States",
    "USMNT" = "United States",
    "United States of America" = "United States",
    "Korea Republic" = "South Korea",
    "Czech Republic" = "Czechia"
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

fixture_key_any_order <- function(date, team_a, team_b, tournament) {
  d <- format(as.Date(date), "%Y-%m-%d")
  a <- normalise_team_name(team_a)
  b <- normalise_team_name(team_b)
  t <- trimws(as.character(tournament))
  
  team_1 <- pmin(a, b)
  team_2 <- pmax(a, b)
  
  paste(d, team_1, team_2, t, sep = "||")
}


load_tournament_registry <- function() {
  if (!file.exists(tournament_registry_file)) {
    stop("Missing tournament registry: ", tournament_registry_file)
  }
  
  read_csv(
    tournament_registry_file,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    transmute(
      source_tournament = trimws(as.character(source_tournament)),
      tournament_id = trimws(as.character(tournament_id)),
      display_name = trimws(as.character(display_name)),
      edition_gap_days = suppressWarnings(as.integer(edition_gap_days)),
      enabled = case_when(
        is.logical(enabled) ~ enabled,
        tolower(trimws(as.character(enabled))) %in% c("true", "1", "yes", "y") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(
      enabled,
      source_tournament != "",
      tournament_id != "",
      display_name != ""
    )
}

load_tournament_overrides <- function() {
  if (!file.exists(tournament_override_file)) {
    return(tibble(
      tournament_id = character(),
      detected_edition_id = character(),
      edition_id_override = character(),
      event_display_override = character()
    ))
  }
  
  read_csv(
    tournament_override_file,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    transmute(
      tournament_id = trimws(as.character(tournament_id)),
      detected_edition_id = trimws(as.character(detected_edition_id)),
      edition_id_override = trimws(as.character(edition_id)),
      event_display_override = trimws(as.character(event_display))
    ) %>%
    filter(
      tournament_id != "",
      detected_edition_id != "",
      edition_id_override != ""
    )
}

recognise_future_tournaments <- function(df, registry, overrides) {
  if (nrow(df) == 0L) {
    df$tournament_id <- character()
    df$edition_id <- character()
    df$event <- character()
    return(df)
  }
  
  df %>%
    left_join(
      registry %>% select(source_tournament, tournament_id, display_name),
      by = c("tournament" = "source_tournament")
    ) %>%
    mutate(
      detected_edition_id = if_else(
        !is.na(tournament_id),
        format(as.Date(date), "%Y"),
        NA_character_
      )
    ) %>%
    left_join(
      overrides,
      by = c("tournament_id", "detected_edition_id")
    ) %>%
    mutate(
      edition_id = case_when(
        is.na(tournament_id) ~ NA_character_,
        !is.na(edition_id_override) & edition_id_override != "" ~ edition_id_override,
        TRUE ~ detected_edition_id
      ),
      event = case_when(
        is.na(tournament_id) ~ NA_character_,
        !is.na(event_display_override) & event_display_override != "" ~ event_display_override,
        TRUE ~ paste(display_name, edition_id)
      )
    ) %>%
    select(-display_name, -detected_edition_id, -edition_id_override, -event_display_override)
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
  "tournament_id",
  "edition_id",
  "event",
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
    tournament_id = trimws(as.character(tournament_id)),
    edition_id = trimws(as.character(edition_id)),
    event = trimws(as.character(event)),
    result = trimws(as.character(result)),
    result = if_else(
      tolower(result) == "draw",
      "Draw",
      normalise_team_name(result)
    ),
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

ghist <- ghist %>%
  mutate(
    tournament_id = na_if(tournament_id, ""),
    edition_id = na_if(edition_id, ""),
    event = na_if(event, "")
  )

tournament_registry <- load_tournament_registry()
tournament_overrides <- load_tournament_overrides()

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
      tournament = tournament,
      tournament_id = tournament_id,
      edition_id = edition_id,
      event = event
    ),
  ghist %>%
    transmute(
      team = away_team,
      date = date,
      era = era_label(date),
      rating = AwayRating_After,
      tournament = tournament,
      tournament_id = tournament_id,
      edition_id = edition_id,
      event = event
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
      tournament = as.character(tournament),
      tournamentId = as.character(tournament_id),
      editionId = as.character(edition_id),
      event = as.character(event)
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
# Combines:
# 1. future rows from all_matches.csv, if that source ever contains them
# 2. future rows from manual_games.csv
#
# Manual rows are removed automatically once the completed match appears
# in world_football_elo_game_history.csv.
#
# Duplicate check uses:
# same date + same tournament + same two teams, ignoring home/away order.

future_fixtures_raw <- tibble()

completed_fixture_keys <- fixture_key_any_order(
  ghist$date,
  ghist$home_team,
  ghist$away_team,
  ghist$tournament
)

completed_fixture_keys <- unique(completed_fixture_keys)

# -----------------------------
# Future fixtures from all_matches.csv
# -----------------------------

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
      "Skipping future fixtures from all_matches.csv because it is missing columns: ",
      paste(missing_source_cols, collapse = ", ")
    )
  } else {
    source_future <- source_results %>%
      mutate(
        date = parse_mixed_date(date),
        home_team = normalise_team_name(home_team),
        away_team = normalise_team_name(away_team),
        home_score = clean_score_num(home_score),
        away_score = clean_score_num(away_score),
        tournament = trimws(as.character(tournament)),
        source = "source_all_matches",
        fixture_key = fixture_key_any_order(date, home_team, away_team, tournament)
      ) %>%
      filter(
        !is.na(date),
        date > asof_date,
        home_team != "",
        away_team != "",
        is.na(home_score),
        is.na(away_score),
        !(fixture_key %in% completed_fixture_keys)
      ) %>%
      transmute(
        date,
        tournament,
        home_team,
        away_team,
        source,
        fixture_key
      )
    
    source_future <- recognise_future_tournaments(
      source_future,
      tournament_registry,
      tournament_overrides
    )
    
    future_fixtures_raw <- bind_rows(future_fixtures_raw, source_future)
  }
} else {
  warning(
    "No source all_matches.csv found, so future fixtures will only come from manual_games.csv: ",
    source_results_csv
  )
}

# -----------------------------
# Future fixtures from manual_games.csv
# -----------------------------

if (file.exists(manual_games_csv)) {
  manual_games <- read_csv(
    manual_games_csv,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  )
  
  required_manual_cols <- c(
    "date",
    "tournament",
    "home_team",
    "away_team",
    "source"
  )
  
  missing_manual_cols <- setdiff(required_manual_cols, names(manual_games))
  
  if (length(missing_manual_cols) > 0) {
    warning(
      "Skipping manual_games.csv because it is missing columns: ",
      paste(missing_manual_cols, collapse = ", ")
    )
  } else {
    manual_future <- manual_games %>%
      mutate(
        date = parse_mixed_date(date),
        home_team = normalise_team_name(home_team),
        away_team = normalise_team_name(away_team),
        tournament = trimws(as.character(tournament)),
        source = trimws(as.character(source)),
        fixture_key = fixture_key_any_order(date, home_team, away_team, tournament)
      ) %>%
      filter(
        !is.na(date),
        date > asof_date,
        home_team != "",
        away_team != "",
        !(fixture_key %in% completed_fixture_keys)
      ) %>%
      transmute(
        date,
        tournament,
        home_team,
        away_team,
        source,
        fixture_key
      )
    
    manual_future <- recognise_future_tournaments(
      manual_future,
      tournament_registry,
      tournament_overrides
    )
    
    future_fixtures_raw <- bind_rows(future_fixtures_raw, manual_future)
  }
} else {
  warning("No manual_games.csv found: ", manual_games_csv)
}

# -----------------------------
# De-duplicate future fixtures
# -----------------------------
# If all_matches.csv and manual_games.csv both contain the same future match,
# prefer all_matches.csv over manual_games.csv.

future_fixtures_raw <- future_fixtures_raw %>%
  mutate(
    source_priority = case_when(
      source == "source_all_matches" ~ 1L,
      TRUE ~ 2L
    )
  ) %>%
  arrange(date, tournament, fixture_key, source_priority) %>%
  distinct(fixture_key, .keep_all = TRUE) %>%
  select(-fixture_key, -source_priority)

# -----------------------------
# Add Elo probabilities
# -----------------------------

future_fixtures_with_ratings <- future_fixtures_raw %>%
  mutate(
    HomeRating_Before = as.numeric(unname(name_to_rating[home_team])),
    AwayRating_Before = as.numeric(unname(name_to_rating[away_team])),
    home_id = unname(name_to_id[home_team]),
    away_id = unname(name_to_id[away_team])
  )

missing_future_ratings <- future_fixtures_with_ratings %>%
  filter(
    !is.finite(HomeRating_Before) |
      !is.finite(AwayRating_Before) |
      is.na(home_id) |
      is.na(away_id)
  ) %>%
  transmute(
    date,
    tournament,
    home_team,
    away_team,
    source,
    HomeRating_Before,
    AwayRating_Before,
    home_id,
    away_id
  )

if (nrow(missing_future_ratings) > 0) {
  warning(
    "Some future fixtures were skipped because one or both teams were not found in the ratings table:\n",
    paste(
      paste(
        missing_future_ratings$date,
        missing_future_ratings$home_team,
        "v",
        missing_future_ratings$away_team,
        sep = " "
      ),
      collapse = "\n"
    )
  )
}

future_fixtures <- future_fixtures_with_ratings %>%
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
    tournamentId = as.character(tournament_id),
    editionId = as.character(edition_id),
    event = as.character(event),
    
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
    
    status = "scheduled",
    source = as.character(source)
  )

cat("Future fixtures loaded:", nrow(future_fixtures), "\n")

# -----------------------------
# Tournament edition JSON
# -----------------------------
#
# No tournament format is assumed here. These files only establish:
#   tournament family -> edition -> teams/matches.
# Format/stage/group information can be added later without changing the IDs.

score_parts <- stringr::str_match(
  as.character(ghist$score),
  "^(\\d+)\\s*-\\s*(\\d+)"
)

completed_tournament_matches <- ghist %>%
  mutate(
    homeScore = suppressWarnings(as.integer(score_parts[, 2])),
    awayScore = suppressWarnings(as.integer(score_parts[, 3])),
    home_id = unname(name_to_id[home_team]),
    away_id = unname(name_to_id[away_team])
  ) %>%
  filter(
    !is.na(tournament_id), tournament_id != "",
    !is.na(edition_id), edition_id != "",
    !is.na(event), event != "",
    !is.na(home_id), !is.na(away_id)
  ) %>%
  transmute(
    tournament_id,
    edition_id,
    event,
    date = as.Date(date),
    home_name = home_team,
    away_name = away_team,
    home_id,
    away_id,
    homeScore,
    awayScore,
    homeElo = as.integer(round(HomeRating_Before)),
    awayElo = as.integer(round(AwayRating_Before)),
    played = TRUE,
    status = "completed"
  )

future_tournament_matches <- future_fixtures %>%
  mutate(
    tournament_id = na_if(as.character(tournamentId), ""),
    edition_id = na_if(as.character(editionId), ""),
    event = na_if(as.character(event), ""),
    home_id = unname(name_to_id[home]),
    away_id = unname(name_to_id[away])
  ) %>%
  filter(
    !is.na(tournament_id),
    !is.na(edition_id),
    !is.na(event),
    !is.na(home_id), !is.na(away_id)
  ) %>%
  transmute(
    tournament_id,
    edition_id,
    event,
    date = as.Date(date),
    home_name = home,
    away_name = away,
    home_id,
    away_id,
    homeScore = NA_integer_,
    awayScore = NA_integer_,
    homeElo = as.integer(homeElo),
    awayElo = as.integer(awayElo),
    played = FALSE,
    status = "scheduled"
  )

tournament_matches <- bind_rows(
  completed_tournament_matches,
  future_tournament_matches
) %>%
  arrange(tournament_id, edition_id, date, home_name, away_name)

family_index <- list()
family_index_n <- 0L

if (nrow(tournament_matches) > 0L) {
  tournament_families <- tournament_matches %>%
    distinct(tournament_id) %>%
    arrange(tournament_id)
  
  for (family_id in tournament_families$tournament_id) {
    family_rows <- tournament_matches %>%
      filter(tournament_id == family_id)
    
    family_name <- tournament_registry %>%
      filter(tournament_id == family_id) %>%
      pull(display_name) %>%
      unique()
    
    family_name <- if (length(family_name) > 0L) family_name[1] else family_id
    
    family_dir <- file.path(tournaments_out, family_id)
    dir.create(family_dir, recursive = TRUE, showWarnings = FALSE)
    
    edition_ids <- unique(family_rows$edition_id)
    edition_manifest <- list()
    edition_manifest_n <- 0L
    
    for (edition_id_value in edition_ids) {
      edition_rows <- family_rows %>%
        filter(edition_id == edition_id_value) %>%
        arrange(date, home_name, away_name)
      
      event_name <- edition_rows$event[which(!is.na(edition_rows$event) & edition_rows$event != "")[1]]
      if (length(event_name) == 0L || is.na(event_name)) {
        event_name <- paste(family_name, edition_id_value)
      }
      
      edition_team_names <- sort(unique(c(edition_rows$home_name, edition_rows$away_name)))
      edition_teams <- tibble(
        id = unname(name_to_id[edition_team_names]),
        name = edition_team_names,
        flag = unname(name_to_flag[edition_team_names])
      ) %>%
        filter(!is.na(id))
      
      match_rows <- edition_rows %>%
        transmute(
          date = format(date, "%Y-%m-%d"),
          home = as.character(home_id),
          away = as.character(away_id),
          homeScore = as.integer(homeScore),
          awayScore = as.integer(awayScore),
          homeElo = as.integer(homeElo),
          awayElo = as.integer(awayElo),
          played = as.logical(played),
          status = as.character(status)
        )
      
      played_n <- sum(edition_rows$played, na.rm = TRUE)
      total_n <- nrow(edition_rows)
      edition_status <- if (played_n == total_n) {
        "complete"
      } else if (played_n > 0L) {
        "active"
      } else {
        "scheduled"
      }
      
      tournament_obj <- list(
        tournamentId = family_id,
        name = family_name,
        editionId = as.character(edition_id_value),
        event = as.character(event_name),
        status = edition_status,
        teams = edition_teams,
        matches = match_rows,
        summary = list(
          teams = as.integer(nrow(edition_teams)),
          matches = as.integer(total_n),
          playedMatches = as.integer(played_n),
          firstDate = format(min(edition_rows$date), "%Y-%m-%d"),
          lastDate = format(max(edition_rows$date), "%Y-%m-%d")
        )
      )
      
      write_json(
        tournament_obj,
        file.path(family_dir, paste0(edition_id_value, ".json")),
        auto_unbox = TRUE,
        pretty = FALSE,
        na = "null"
      )
      
      edition_manifest_n <- edition_manifest_n + 1L
      edition_manifest[[edition_manifest_n]] <- list(
        id = as.character(edition_id_value),
        label = as.character(event_name),
        status = edition_status,
        firstDate = format(min(edition_rows$date), "%Y-%m-%d"),
        lastDate = format(max(edition_rows$date), "%Y-%m-%d"),
        teams = as.integer(nrow(edition_teams)),
        matches = as.integer(total_n),
        playedMatches = as.integer(played_n)
      )
    }
    
    if (length(edition_manifest) > 0L) {
      edition_year <- suppressWarnings(as.integer(vapply(edition_manifest, function(x) x$id, character(1))))
      ord <- order(ifelse(is.na(edition_year), -Inf, edition_year), decreasing = TRUE)
      edition_manifest <- edition_manifest[ord]
    }
    
    write_json(
      edition_manifest,
      file.path(family_dir, "editions.json"),
      auto_unbox = TRUE,
      pretty = FALSE,
      na = "null"
    )
    
    family_index_n <- family_index_n + 1L
    family_index[[family_index_n]] <- list(
      id = family_id,
      name = family_name,
      editions = as.integer(length(edition_manifest))
    )
  }
}

write_json(
  family_index,
  file.path(tournaments_out, "index.json"),
  auto_unbox = TRUE,
  pretty = FALSE,
  na = "null"
)

cat("Wrote tournament families:", length(family_index), "\n")
cat("Tournament JSON directory:", tournaments_out, "\n")

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
      tournamentId = as.character(tournament_id),
      editionId = as.character(edition_id),
      event = as.character(event),
      
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
      
      status = "completed",
      source = NA_character_
    )
  
  scheduled_df <- future_fixtures %>%
    filter(home == tm | away == tm)
  
  df <- bind_rows(df, scheduled_df) %>%
    arrange(desc(as.Date(date)), desc(status == "scheduled"))
  
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
  file.path(base_data_dir, "era_starts.json"),
  file.path(tournaments_out, "index.json")
)

missing_outputs <- expected_outputs[!file.exists(expected_outputs)]

if (length(missing_outputs) > 0) {
  stop("Missing expected JSON outputs: ", paste(missing_outputs, collapse = ", "))
}

cat("Done.\n")
cat("JSON output directory:", base_data_dir, "\n")

