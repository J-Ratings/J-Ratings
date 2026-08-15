# scripts/EuropeanFootball/03_write_json.R

library(dplyr)
library(readr)
library(jsonlite)
library(stringi)
library(stringr)
library(tictoc)

options(stringsAsFactors = FALSE)


tic()
# -----------------------------
# Paths
# -----------------------------

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

src_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Elo"
)

base_data_dir <- file.path(repo_dir, "EuropeanFootball", "data")
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

season_label <- function(d) {
  d <- as.Date(d)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  
  start_year <- ifelse(m >= 7L, y, y - 1L)
  end_short <- substr(as.character(start_year + 1L), 3, 4)
  
  paste0(start_year, "-", end_short)
}

elo_expected <- function(team_elo, opponent_elo) {
  1 / (1 + 10 ^ ((opponent_elo - team_elo) / 400))
}

clamp <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))
}

draw_rate_from_tier_gap <- function(tier, abs_gap) {
  tier <- as.integer(tier)
  
  # Calibrated from English football match data, 2005 onwards.
  # The model is intentionally simple:
  # draw_rate = base + slope * abs_elo_gap, clamped by tier.
  base <- dplyr::case_when(
    tier == 1L ~ 0.302,
    tier == 2L ~ 0.291,
    tier == 3L ~ 0.282,
    tier == 4L ~ 0.284,
    tier == 5L ~ 0.274,
    TRUE       ~ 0.282
  )
  
  slope <- dplyr::case_when(
    tier == 1L ~ -0.00048,
    tier == 2L ~ -0.00022,
    tier == 3L ~ -0.00018,
    tier == 4L ~ -0.00018,
    tier == 5L ~ -0.00025,
    TRUE       ~ -0.00018
  )
  
  min_draw <- dplyr::case_when(
    tier == 1L ~ 0.13,
    tier == 2L ~ 0.18,
    tier == 3L ~ 0.17,
    tier == 4L ~ 0.17,
    tier == 5L ~ 0.16,
    TRUE       ~ 0.17
  )
  
  max_draw <- dplyr::case_when(
    tier == 1L ~ 0.31,
    tier == 2L ~ 0.30,
    tier == 3L ~ 0.29,
    tier == 4L ~ 0.29,
    tier == 5L ~ 0.28,
    TRUE       ~ 0.29
  )
  
  clamp(base + slope * abs_gap, min_draw, max_draw)
}

# -----------------------------
# Load CSVs
# -----------------------------

final_csv    <- file.path(src_dir, "football_elo_final_ratings.csv")
hist_csv     <- file.path(src_dir, "football_elo_game_history.csv")
fixtures_csv <- file.path(src_dir, "football_upcoming_fixtures.csv")

if (!file.exists(final_csv))    stop("Missing file: ", final_csv)
if (!file.exists(hist_csv))     stop("Missing file: ", hist_csv)
if (!file.exists(fixtures_csv)) stop("Missing file: ", fixtures_csv)

final    <- read_csv(final_csv, show_col_types = FALSE)
ghist    <- read_csv(hist_csv, show_col_types = FALSE)
fixtures <- read_csv(fixtures_csv, show_col_types = FALSE)


# -----------------------------
# Validate columns
# -----------------------------

required_final <- c("Team", "Rating", "Games")

required_hist <- c(
  "Country", "Competition", "CompetitionType", "League", "Tier", "Date", "Home", "Away", "Result", "Score",
  "HomeRating_Before", "AwayRating_Before",
  "HomeRating_After", "AwayRating_After"
)

miss_final <- setdiff(required_final, names(final))
miss_hist  <- setdiff(required_hist, names(ghist))

if (length(miss_final) > 0) {
  stop("Missing columns in final CSV: ", paste(miss_final, collapse = ", "))
}

if (length(miss_hist) > 0) {
  stop("Missing columns in history CSV: ", paste(miss_hist, collapse = ", "))
}

# -----------------------------
# Prepare history
# -----------------------------

ghist <- ghist %>%
  mutate(
    Date   = as.Date(Date),
    Home            = trimws(as.character(Home)),
    Away            = trimws(as.character(Away)),
    Country         = trimws(as.character(Country)),
    Competition     = trimws(as.character(Competition)),
    CompetitionType = trimws(as.character(CompetitionType)),
    League          = trimws(as.character(League)),
    Source          = if ("Source" %in% names(ghist)) trimws(as.character(Source)) else NA_character_,
    Score           = trimws(as.character(Score)),
    Tier            = as.integer(Tier)
  ) %>%
  filter(!is.na(Date), Home != "", Away != "")

fixtures <- fixtures %>%
  mutate(
    Date   = as.Date(Date),
    Home            = trimws(as.character(Home)),
    Away            = trimws(as.character(Away)),
    Country         = trimws(as.character(Country)),
    Competition     = trimws(as.character(Competition)),
    CompetitionType = trimws(as.character(CompetitionType)),
    League          = trimws(as.character(League)),
    Source          = if ("Source" %in% names(fixtures)) trimws(as.character(Source)) else NA_character_,
    Tier            = as.integer(Tier)
  ) %>%
  filter(!is.na(Date), Home != "", Away != "")

asof_date <- max(ghist$Date, na.rm = TRUE)

# -----------------------------
# meta.json
# -----------------------------

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
# Current tier/division per team
# -----------------------------

team_last <- bind_rows(
  ghist %>% transmute(
    team = Home, Date, Country, Competition, CompetitionType, Tier, League
  ),
  ghist %>% transmute(
    team = Away, Date, Country, Competition, CompetitionType, Tier, League
  )
) %>%
  filter(
    !is.na(Date),
    team != "",
    CompetitionType == "league"
  ) %>%
  arrange(team, Date) %>%
  group_by(team) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    name = team,
    last_played = as.Date(Date),
    country = as.character(Country),
    competition = as.character(Competition),
    tier = as.integer(Tier),
    division = as.character(League)
  )

# -----------------------------
# teams.json
# -----------------------------

teams_all <- final %>%
  transmute(
    name = trimws(as.character(Team)),
    rating = as.integer(round(Rating)),
    games = as.integer(Games)
  ) %>%
  left_join(team_last, by = "name") %>%
  mutate(
    id = slug(name)
  ) %>%
  select(id, name, rating, games, last_played, country, competition, tier, division) %>%
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

cutoff_date <- as.Date(asof_date) - 365L

teams_tbl <- teams_all %>%
  filter(!is.na(last_played) & last_played >= cutoff_date) %>%
  select(id, name, rating, games, country, competition, tier, division) %>%
  arrange(desc(rating), name)

write_json(
  teams_tbl,
  file.path(base_data_dir, "teams.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat(
  "Wrote teams.json (n =",
  nrow(teams_tbl),
  ", cutoff =",
  format(cutoff_date, "%Y-%m-%d"),
  ")\n"
)

name_to_id <- setNames(teams_all$id, teams_all$name)

rating_lookup <- setNames(
  as.numeric(final$Rating),
  trimws(as.character(final$Team))
)

# -----------------------------
# Long rating history
# -----------------------------
# -----------------------------
# Long rating history
# -----------------------------

hist_long <- bind_rows(
  ghist %>%
    transmute(
      team = Home,
      date = Date,
      season = season_label(Date),
      rating = HomeRating_After,
      country = as.character(Country),
      competition = as.character(Competition),
      competition_type = as.character(CompetitionType),
      tier = as.integer(Tier),
      division = if_else(CompetitionType == "league", as.character(League), NA_character_)
    ),
  ghist %>%
    transmute(
      team = Away,
      date = Date,
      season = season_label(Date),
      rating = AwayRating_After,
      country = as.character(Country),
      competition = as.character(Competition),
      competition_type = as.character(CompetitionType),
      tier = as.integer(Tier),
      division = if_else(CompetitionType == "league", as.character(League), NA_character_)
    )
) %>%
  filter(!is.na(rating), team != "") %>%
  arrange(team, date) %>%
  group_by(team, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# -----------------------------
# season_starts.json
# -----------------------------

season_starts_long <- bind_rows(
  ghist %>%
    transmute(
      team = Home,
      date = Date,
      season = season_label(Date),
      elo = HomeRating_Before,
      country = as.character(Country),
      competition = as.character(Competition),
      competition_type = as.character(CompetitionType),
      tier = as.integer(Tier),
      division = if_else(CompetitionType == "league", as.character(League), NA_character_)
    ),
  ghist %>%
    transmute(
      team = Away,
      date = Date,
      season = season_label(Date),
      elo = AwayRating_Before,
      country = as.character(Country),
      competition = as.character(Competition),
      competition_type = as.character(CompetitionType),
      tier = as.integer(Tier),
      division = if_else(CompetitionType == "league", as.character(League), NA_character_)
    )
) %>%
  filter(team != "", !is.na(date), !is.na(elo), !is.na(season)) %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    id = unname(name_to_id[team]),
    elo = as.integer(round(elo))
  ) %>%
  filter(!is.na(id)) %>%
  transmute(
    season = as.character(season),
    id = as.character(id),
    name = as.character(team),
    elo = as.integer(elo),
    country = as.character(country),
    competition = as.character(competition),
    competition_type = as.character(competition_type),
    tier = as.integer(tier),
    division = as.character(division)
  ) %>%
  arrange(desc(season), desc(elo), name)

write_json(
  season_starts_long,
  file.path(base_data_dir, "season_starts.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote season_starts.json (rows =", nrow(season_starts_long), ")\n")

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
      season = as.character(season),
      rating = as.integer(round(rating)),
      country = as.character(country),
      competition = as.character(competition),
      competitionType = as.character(competition_type),
      tier = as.integer(tier),
      division = as.character(division)
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
  
  # -----------------------------
  # Next five fixtures
  # -----------------------------
  
  future_df <- fixtures %>%
    filter(
      Home == tm | Away == tm,
      Date > asof_date
    ) %>%
    arrange(Date) %>%
    slice_head(n = 5) %>%
    mutate(
      season = season_label(Date),
      division = if_else(CompetitionType == "league", as.character(League), NA_character_),
      
      home_elo = unname(rating_lookup[Home]),
      away_elo = unname(rating_lookup[Away]),
      
      home_expected = elo_expected(home_elo, away_elo),
      away_expected = 1 - home_expected,
      
      abs_elo_gap = abs(home_elo - away_elo),
      draw_prob = draw_rate_from_tier_gap(
        as.integer(Tier),
        abs_elo_gap
      ),
      
      home_win_prob = home_expected - draw_prob / 2,
      away_win_prob = away_expected - draw_prob / 2,
      
      home_win_prob = clamp(
        home_win_prob,
        0,
        1 - draw_prob
      ),
      
      away_win_prob = 1 - draw_prob - home_win_prob
    ) %>%
    transmute(
      date = format(Date, "%Y-%m-%d"),
      season = as.character(season),
      country = as.character(Country),
      competition = as.character(Competition),
      competitionType = as.character(CompetitionType),
      league = as.character(League),
      tier = as.integer(Tier),
      division = as.character(division),
      source = as.character(Source),
      
      home = as.character(Home),
      homeElo = as.integer(round(home_elo)),
      
      away = as.character(Away),
      awayElo = as.integer(round(away_elo)),
      
      result = NA_character_,
      score = NA_character_,
      delta = NA_character_,
      
      homeWinPct = as.integer(round(100 * home_win_prob)),
      drawPct = as.integer(round(100 * draw_prob)),
      awayWinPct = as.integer(round(100 * away_win_prob)),
      
      upcoming = TRUE
    )
  
  # -----------------------------
  # Completed matches
  # -----------------------------
  
  past_df <- ghist %>%
    filter(Home == tm | Away == tm) %>%
    arrange(desc(Date)) %>%
    mutate(
      season = season_label(Date),
      
      delta_num = case_when(
        Home == tm ~ HomeRating_After - HomeRating_Before,
        Away == tm ~ AwayRating_After - AwayRating_Before,
        TRUE ~ NA_real_
      ),
      
      delta = ifelse(
        !is.na(delta_num),
        sprintf("%+0.1f", round(delta_num, 1)),
        NA_character_
      ),
      
      division = if_else(CompetitionType == "league", as.character(League), NA_character_),
      
      home_expected = elo_expected(
        HomeRating_Before,
        AwayRating_Before
      ),
      
      away_expected = 1 - home_expected,
      
      abs_elo_gap = abs(
        HomeRating_Before - AwayRating_Before
      ),
      
      draw_prob = draw_rate_from_tier_gap(
        as.integer(Tier),
        abs_elo_gap
      ),
      
      home_win_prob = home_expected - draw_prob / 2,
      away_win_prob = away_expected - draw_prob / 2,
      
      home_win_prob = clamp(
        home_win_prob,
        0,
        1 - draw_prob
      ),
      
      away_win_prob = 1 - draw_prob - home_win_prob
    ) %>%
    transmute(
      date = format(Date, "%Y-%m-%d"),
      season = as.character(season),
      country = as.character(Country),
      competition = as.character(Competition),
      competitionType = as.character(CompetitionType),
      league = as.character(League),
      tier = as.integer(Tier),
      division = as.character(division),
      source = as.character(Source),
      
      home = as.character(Home),
      homeElo = as.integer(round(HomeRating_Before)),
      
      away = as.character(Away),
      awayElo = as.integer(round(AwayRating_Before)),
      
      result = as.character(Result),
      score = as.character(Score),
      delta,
      
      homeWinPct = as.integer(round(100 * home_win_prob)),
      drawPct = as.integer(round(100 * draw_prob)),
      awayWinPct = as.integer(round(100 * away_win_prob)),
      
      upcoming = FALSE
    )
  
  # Upcoming games first, followed by historical games
  df <- bind_rows(
    future_df,
    past_df
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


# -----------------------------
# Simulation tournament files
# -----------------------------
#
# Build Premier League season files directly from the normal pipeline data.
# This replaces the old one-off tournament-build test script.
#
# Historical seasons come from completed Elo history. For the current season,
# upcoming Premier League fixtures are appended from football_upcoming_fixtures.csv.
#
# Output:
#   EuropeanFootball/data/simulations/premier-league/seasons.json
#   EuropeanFootball/data/simulations/premier-league/YYYY-YY.json

simulation_root <- file.path(
  base_data_dir,
  "simulations",
  "premier-league"
)

dir.create(
  simulation_root,
  recursive = TRUE,
  showWarnings = FALSE
)

pl_past <- ghist %>%
  filter(
    Country == "England",
    Competition == "premier_league",
    CompetitionType == "league",
    Tier == 1L
  ) %>%
  mutate(
    season = season_label(Date),
    homeScore = suppressWarnings(
      as.integer(
        stringr::str_match(
          as.character(Score),
          "^(\\d+)\\s*-\\s*(\\d+)"
        )[, 2]
      )
    ),
    awayScore = suppressWarnings(
      as.integer(
        stringr::str_match(
          as.character(Score),
          "^(\\d+)\\s*-\\s*(\\d+)"
        )[, 3]
      )
    )
  ) %>%
  transmute(
    season = as.character(season),
    date = as.Date(Date),
    home = as.character(Home),
    away = as.character(Away),
    homeScore = as.integer(homeScore),
    awayScore = as.integer(awayScore),
    played = TRUE
  )

pl_future <- fixtures %>%
  filter(
    Country == "England",
    Competition == "premier_league",
    CompetitionType == "league",
    Tier == 1L
  ) %>%
  mutate(
    season = season_label(Date)
  ) %>%
  transmute(
    season = as.character(season),
    date = as.Date(Date),
    home = as.character(Home),
    away = as.character(Away),
    homeScore = NA_integer_,
    awayScore = NA_integer_,
    played = FALSE
  )

pl_all <- bind_rows(
  pl_past,
  pl_future
) %>%
  filter(
    !is.na(date),
    home != "",
    away != ""
  ) %>%
  arrange(
    season,
    date,
    home,
    away
  )

pl_seasons <- sort(
  unique(pl_all$season),
  decreasing = TRUE
)

season_manifest <- list()

for (season_id in pl_seasons) {
  season_matches <- pl_all %>%
    filter(
      season == season_id
    ) %>%
    arrange(
      date,
      home,
      away
    )
  
  # Avoid publishing a malformed/incomplete historical season.
  # The current season may legitimately contain unplayed fixtures.
  if (nrow(season_matches) == 0) next
  
  season_teams <- sort(
    unique(
      c(
        season_matches$home,
        season_matches$away
      )
    )
  )
  
  team_tbl <- tibble(
    id = slug(season_teams),
    name = season_teams
  )
  
  if (anyDuplicated(team_tbl$id)) {
    stop(
      "Team slug collision in Premier League season ",
      season_id
    )
  }
  
  name_to_id <- setNames(
    team_tbl$id,
    team_tbl$name
  )
  
  matches_tbl <- season_matches %>%
    transmute(
      date = format(date, "%Y-%m-%d"),
      home = unname(name_to_id[home]),
      away = unname(name_to_id[away]),
      homeScore = as.integer(homeScore),
      awayScore = as.integer(awayScore),
      played = as.logical(played)
    )
  
  first_date <- min(
    season_matches$date,
    na.rm = TRUE
  )
  
  last_date <- max(
    season_matches$date,
    na.rm = TRUE
  )
  
  tournament <- list(
    id = paste0(
      "premier-league-",
      season_id
    ),
    competition = "premier-league",
    name = "Premier League",
    country = "England",
    season = season_id,
    format = "league",
    rules = list(
      winPoints = 3L,
      drawPoints = 1L,
      lossPoints = 0L,
      relegationPlaces = 3L
    ),
    summary = list(
      teams = as.integer(
        length(season_teams)
      ),
      matches = as.integer(
        nrow(season_matches)
      ),
      playedMatches = as.integer(
        sum(season_matches$played)
      ),
      firstDate = format(
        first_date,
        "%Y-%m-%d"
      ),
      lastDate = format(
        last_date,
        "%Y-%m-%d"
      )
    ),
    teams = team_tbl,
    matches = matches_tbl
  )
  
  write_json(
    tournament,
    file.path(
      simulation_root,
      paste0(
        season_id,
        ".json"
      )
    ),
    auto_unbox = TRUE,
    pretty = FALSE,
    na = "null"
  )
  
  season_manifest[[length(season_manifest) + 1L]] <- list(
    id = season_id,
    label = gsub(
      "-",
      "/",
      season_id,
      fixed = TRUE
    ),
    firstDate = format(
      first_date,
      "%Y-%m-%d"
    ),
    lastDate = format(
      last_date,
      "%Y-%m-%d"
    ),
    teams = as.integer(
      length(season_teams)
    ),
    matches = as.integer(
      nrow(season_matches)
    ),
    playedMatches = as.integer(
      sum(season_matches$played)
    )
  )
}

write_json(
  season_manifest,
  file.path(
    simulation_root,
    "seasons.json"
  ),
  auto_unbox = TRUE,
  pretty = FALSE,
  na = "null"
)

cat(
  "Wrote Premier League simulation seasons:",
  length(season_manifest),
  "\n"
)


cat("Wrote games files:", n_games_written, "\n")
cat("Done.\n")



toc()



beep()