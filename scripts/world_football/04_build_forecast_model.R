library(dplyr)
library(readr)
library(jsonlite)
library(stringi)
library(tibble)

options(stringsAsFactors = FALSE)

# ============================================================
# J-Ratings football forecast model - Advanced v1 (actual-score fix)
#
# IMPORTANT:
# This is a NEW step 04 script.
# It is NOT 03_write_json.R.
#
# It builds attack/defence forecast parameters for tournament
# simulation and does not alter Elo ratings or Elo mechanics.
#
# For each tournament team:
#   use the LAST 100 matches STRICTLY BEFORE tournament start.
# ============================================================

# Find the repository root robustly, whether this script is run from the
# repository root, from scripts/world_football, or sourced from another
# working directory inside the repository.
find_repo_dir <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  
  repeat {
    if (
      dir.exists(file.path(p, "InternationalFootball")) &&
      dir.exists(file.path(p, "scripts", "world_football"))
    ) {
      return(p)
    }
    
    parent <- dirname(p)
    
    if (identical(parent, p)) {
      stop(
        "Could not find the J-Ratings repository root from working directory: ",
        start
      )
    }
    
    p <- parent
  }
}

repo_dir <- find_repo_dir()

hist_csv <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "Elo",
  "world_football_elo_game_history.csv"
)

tournament_json <- file.path(
  repo_dir,
  "InternationalFootball",
  "data",
  "tournaments",
  "world-cup",
  "2022.json"
)

structure_json <- file.path(
  repo_dir,
  "InternationalFootball",
  "data",
  "tournament-structure",
  "world-cup",
  "2022.json"
)

forecast_dir <- file.path(
  repo_dir,
  "InternationalFootball",
  "data",
  "forecast",
  "tournaments",
  "world-cup"
)

forecast_json <- file.path(
  forecast_dir,
  "2022.json"
)

dir.create(forecast_dir, recursive = TRUE, showWarnings = FALSE)

for (p in c(hist_csv, tournament_json, structure_json)) {
  if (!file.exists(p)) stop("Missing file: ", p)
}

MAX_TEAM_GAMES <- 100L
RECENCY_HALF_LIFE_GAMES <- 35
SHRINKAGE_GAMES <- 20
BASELINE_LOOKBACK_YEARS <- 8

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

safe_num <- function(x) suppressWarnings(as.numeric(x))


# IMPORTANT:
# In the Elo game-history file, HomeScore/AwayScore are Elo outcome values
# (1 / 0.5 / 0), NOT football goals. Actual football goals are parsed from
# the "score" field, e.g. "3-1".
parse_actual_score <- function(x) {
  s <- trimws(as.character(x))
  
  # Accept the normal pipeline format N-N, allowing surrounding text only
  # after the score if a future source ever adds annotations.
  m <- stringr::str_match(s, "^\\s*([0-9]+)\\s*[-–—]\\s*([0-9]+)")
  
  tibble(
    actual_home_goals = suppressWarnings(as.numeric(m[, 2])),
    actual_away_goals = suppressWarnings(as.numeric(m[, 3]))
  )
}

ghist <- read_csv(
  hist_csv,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

required_hist <- c(
  "date",
  "home_team",
  "away_team",
  "score",
  "HomeRating_Before",
  "AwayRating_Before",
  "HomeRating_After",
  "AwayRating_After"
)

missing_hist <- setdiff(required_hist, names(ghist))

if (length(missing_hist) > 0) {
  stop(
    "world_football_elo_game_history.csv is missing: ",
    paste(missing_hist, collapse = ", ")
  )
}

tournament <- fromJSON(tournament_json, simplifyVector = FALSE)
structure <- fromJSON(structure_json, simplifyVector = FALSE)

if (is.null(structure$groupStage$startDate)) {
  stop("Tournament structure has no groupStage.startDate.")
}

cutoff <- as.Date(structure$groupStage$startDate)

team_tbl <- tibble(
  id = vapply(tournament$teams, function(x) as.character(x$id), character(1)),
  name = vapply(tournament$teams, function(x) as.character(x$name), character(1))
) %>%
  mutate(
    name_norm = normalise_team_name(name)
  )

score_parts <- parse_actual_score(ghist$score)

ghist <- ghist %>%
  mutate(
    home_goals_actual = score_parts$actual_home_goals,
    away_goals_actual = score_parts$actual_away_goals
  ) %>%
  transmute(
    date = as.Date(date),
    home_team = normalise_team_name(home_team),
    away_team = normalise_team_name(away_team),
    home_goals = home_goals_actual,
    away_goals = away_goals_actual,
    home_elo_before = safe_num(HomeRating_Before),
    away_elo_before = safe_num(AwayRating_Before),
    home_elo_after = safe_num(HomeRating_After),
    away_elo_after = safe_num(AwayRating_After)
  ) %>%
  filter(
    !is.na(date),
    date < cutoff,
    !is.na(home_goals),
    !is.na(away_goals),
    !is.na(home_elo_before),
    !is.na(away_elo_before),
    home_team != "",
    away_team != ""
  ) %>%
  arrange(date)

if (nrow(ghist) == 0) {
  stop("No usable pre-tournament matches found.")
}


# Score sanity checks. International football should not have a baseline
# anywhere near the 0-1 Elo outcome coding.
raw_goals_per_team <- mean(c(ghist$home_goals, ghist$away_goals), na.rm = TRUE)

if (!is.finite(raw_goals_per_team) || raw_goals_per_team < 0.70) {
  stop(
    "Actual-score sanity check failed: mean goals per team = ",
    round(raw_goals_per_team, 3),
    ". Check parsing of the score column."
  )
}

# ------------------------------------------------------------
# Global baseline goal model.
# This estimates the normal scoring environment and how Elo
# difference relates to goal production.
# ------------------------------------------------------------

baseline_start <- cutoff - round(365.25 * BASELINE_LOOKBACK_YEARS)

baseline_matches <- ghist %>%
  filter(date >= baseline_start)

baseline_long <- bind_rows(
  baseline_matches %>%
    transmute(
      goals = home_goals,
      elo_diff = (home_elo_before - away_elo_before) / 400,
      venue_code = 0.5
    ),
  baseline_matches %>%
    transmute(
      goals = away_goals,
      elo_diff = (away_elo_before - home_elo_before) / 400,
      venue_code = -0.5
    )
) %>%
  filter(
    is.finite(goals),
    is.finite(elo_diff),
    is.finite(venue_code)
  )

if (nrow(baseline_long) < 500) {
  stop("Not enough matches to fit the global goal baseline.")
}

goal_glm <- glm(
  goals ~ elo_diff + venue_code,
  family = poisson(link = "log"),
  data = baseline_long
)

beta <- coef(goal_glm)

if (any(!is.finite(beta))) {
  stop("Global goal model returned invalid coefficients.")
}

neutral_base_goals <- unname(exp(beta[["(Intercept)"]]))
elo_goal_slope <- unname(beta[["elo_diff"]])
home_goal_effect <- unname(beta[["venue_code"]])


if (!is.finite(neutral_base_goals) || neutral_base_goals < 0.70 || neutral_base_goals > 3.00) {
  stop(
    "Goal-model sanity check failed: neutral baseline goals/team = ",
    round(neutral_base_goals, 3),
    ". Expected a football-like value, roughly around 1-2."
  )
}

predict_goal_rate <- function(team_elo, opp_elo, venue_code) {
  exp(
    beta[["(Intercept)"]] +
      beta[["elo_diff"]] * ((team_elo - opp_elo) / 400) +
      beta[["venue_code"]] * venue_code
  )
}

# ------------------------------------------------------------
# Team-perspective match table.
# ------------------------------------------------------------

team_games_long <- bind_rows(
  ghist %>%
    transmute(
      date = date,
      team = home_team,
      opponent = away_team,
      goals_for = home_goals,
      goals_against = away_goals,
      team_elo = home_elo_before,
      opponent_elo = away_elo_before,
      rating_after = home_elo_after,
      venue_code = 0.5
    ),
  ghist %>%
    transmute(
      date = date,
      team = away_team,
      opponent = home_team,
      goals_for = away_goals,
      goals_against = home_goals,
      team_elo = away_elo_before,
      opponent_elo = home_elo_before,
      rating_after = away_elo_after,
      venue_code = -0.5
    )
) %>%
  arrange(team, date)

build_team_profile <- function(team_name, team_id) {
  
  # THIS IS THE KEY RULE:
  # last 100 matches strictly before tournament start
  x <- team_games_long %>%
    filter(
      team == team_name,
      date < cutoff
    ) %>%
    arrange(desc(date)) %>%
    slice_head(n = MAX_TEAM_GAMES) %>%
    mutate(
      game_index = row_number(),
      weight = 0.5 ^ ((game_index - 1) / RECENCY_HALF_LIFE_GAMES),
      expected_for = predict_goal_rate(
        team_elo,
        opponent_elo,
        venue_code
      ),
      expected_against = predict_goal_rate(
        opponent_elo,
        team_elo,
        -venue_code
      )
    )
  
  if (nrow(x) == 0) {
    return(tibble(
      id = team_id,
      name = team_name,
      anchorElo = NA_real_,
      gamesUsed = 0L,
      newestMatch = NA_character_,
      oldestMatch = NA_character_,
      attackMultiplier = 1,
      defenceStrength = 1,
      goalsForPerGame = NA_real_,
      goalsAgainstPerGame = NA_real_,
      expectedGoalsForPerGame = NA_real_,
      expectedGoalsAgainstPerGame = NA_real_,
      weightedActualGoalsFor = NA_real_,
      weightedExpectedGoalsFor = NA_real_,
      weightedActualGoalsAgainst = NA_real_,
      weightedExpectedGoalsAgainst = NA_real_,
      rawAttackRatio = NA_real_,
      rawDefenceStrength = NA_real_,
      auditMatches = list(tibble())
    ))
  }
  
  w <- x$weight
  
  weighted_obs_for <- sum(w * x$goals_for)
  weighted_exp_for <- sum(w * x$expected_for)
  
  weighted_obs_against <- sum(w * x$goals_against)
  weighted_exp_against <- sum(w * x$expected_against)
  
  mean_exp_for <- weighted_exp_for / sum(w)
  mean_exp_against <- weighted_exp_against / sum(w)
  
  # Residual attack performance, shrunk towards 1.
  attack_multiplier <- (
    weighted_obs_for + SHRINKAGE_GAMES * mean_exp_for
  ) / (
    weighted_exp_for + SHRINKAGE_GAMES * mean_exp_for
  )
  
  # Residual goals conceded, inverted so higher = stronger defence.
  defence_leak_multiplier <- (
    weighted_obs_against + SHRINKAGE_GAMES * mean_exp_against
  ) / (
    weighted_exp_against + SHRINKAGE_GAMES * mean_exp_against
  )
  
  defence_strength <- 1 / max(0.20, defence_leak_multiplier)
  
  # Conservative defaults.
  attack_multiplier <- min(2.00, max(0.50, attack_multiplier))
  defence_strength <- min(2.00, max(0.50, defence_strength))
  
  anchor_elo <- x %>%
    arrange(desc(date)) %>%
    slice_head(n = 1) %>%
    pull(rating_after)
  
  audit_matches <- x %>%
    transmute(
      date = format(date, "%Y-%m-%d"),
      opponent = opponent,
      venue = case_when(
        venue_code > 0 ~ "Home",
        venue_code < 0 ~ "Away",
        TRUE ~ "Neutral"
      ),
      teamElo = round(team_elo),
      opponentElo = round(opponent_elo),
      goalsFor = as.integer(goals_for),
      goalsAgainst = as.integer(goals_against),
      expectedGoalsFor = round(expected_for, 4),
      expectedGoalsAgainst = round(expected_against, 4),
      weight = round(weight, 6)
    )
  
  tibble(
    id = team_id,
    name = team_name,
    anchorElo = as.numeric(anchor_elo),
    gamesUsed = nrow(x),
    newestMatch = format(max(x$date), "%Y-%m-%d"),
    oldestMatch = format(min(x$date), "%Y-%m-%d"),
    attackMultiplier = attack_multiplier,
    defenceStrength = defence_strength,
    goalsForPerGame = weighted.mean(x$goals_for, w),
    goalsAgainstPerGame = weighted.mean(x$goals_against, w),
    expectedGoalsForPerGame = weighted.mean(x$expected_for, w),
    expectedGoalsAgainstPerGame = weighted.mean(x$expected_against, w),
    weightedActualGoalsFor = weighted_obs_for,
    weightedExpectedGoalsFor = weighted_exp_for,
    weightedActualGoalsAgainst = weighted_obs_against,
    weightedExpectedGoalsAgainst = weighted_exp_against,
    rawAttackRatio = weighted_obs_for / weighted_exp_for,
    rawDefenceStrength = weighted_exp_against / weighted_obs_against,
    auditMatches = list(audit_matches)
  )
}

profiles <- bind_rows(
  lapply(seq_len(nrow(team_tbl)), function(i) {
    build_team_profile(
      team_tbl$name_norm[[i]],
      team_tbl$id[[i]]
    ) %>%
      mutate(name = team_tbl$name[[i]])
  })
) %>%
  arrange(id)

missing_profiles <- profiles %>%
  filter(gamesUsed == 0 | is.na(anchorElo))

if (nrow(missing_profiles) > 0) {
  print(missing_profiles)
  stop("One or more tournament teams have no usable pre-tournament profile.")
}

profile_list <- lapply(seq_len(nrow(profiles)), function(i) {
  p <- profiles[i, ]
  
  list(
    id = as.character(p$id),
    name = as.character(p$name),
    anchorElo = as.integer(round(p$anchorElo)),
    gamesUsed = as.integer(p$gamesUsed),
    newestMatch = as.character(p$newestMatch),
    oldestMatch = as.character(p$oldestMatch),
    attackMultiplier = round(as.numeric(p$attackMultiplier), 6),
    defenceStrength = round(as.numeric(p$defenceStrength), 6),
    goalsForPerGame = round(as.numeric(p$goalsForPerGame), 4),
    goalsAgainstPerGame = round(as.numeric(p$goalsAgainstPerGame), 4),
    expectedGoalsForPerGame = round(as.numeric(p$expectedGoalsForPerGame), 4),
    expectedGoalsAgainstPerGame = round(as.numeric(p$expectedGoalsAgainstPerGame), 4),
    calculation = list(
      weightedActualGoalsFor = round(as.numeric(p$weightedActualGoalsFor), 4),
      weightedExpectedGoalsFor = round(as.numeric(p$weightedExpectedGoalsFor), 4),
      weightedActualGoalsAgainst = round(as.numeric(p$weightedActualGoalsAgainst), 4),
      weightedExpectedGoalsAgainst = round(as.numeric(p$weightedExpectedGoalsAgainst), 4),
      rawAttackRatio = round(as.numeric(p$rawAttackRatio), 6),
      rawDefenceStrength = round(as.numeric(p$rawDefenceStrength), 6),
      shrinkageGames = SHRINKAGE_GAMES,
      explanation = "Attack compares weighted actual goals scored with Elo/venue-adjusted expected goals. Defence compares expected goals conceded with weighted actual goals conceded. Both are shrunk towards 1.00."
    ),
    matches = lapply(seq_len(nrow(p$auditMatches[[1]])), function(j) {
      m <- p$auditMatches[[1]][j, ]
      list(
        date = as.character(m$date),
        opponent = as.character(m$opponent),
        venue = as.character(m$venue),
        teamElo = as.integer(m$teamElo),
        opponentElo = as.integer(m$opponentElo),
        goalsFor = as.integer(m$goalsFor),
        goalsAgainst = as.integer(m$goalsAgainst),
        expectedGoalsFor = as.numeric(m$expectedGoalsFor),
        expectedGoalsAgainst = as.numeric(m$expectedGoalsAgainst),
        weight = as.numeric(m$weight)
      )
    })
  )
})

out <- list(
  model = "j-ratings-football-advanced-v1",
  tournamentId = "world-cup",
  editionId = "2022",
  cutoffDate = format(cutoff, "%Y-%m-%d"),
  teamWindow = list(
    maxGames = MAX_TEAM_GAMES,
    rule = "last matches strictly before tournament start",
    recencyHalfLifeGames = RECENCY_HALF_LIFE_GAMES,
    shrinkageGames = SHRINKAGE_GAMES
  ),
  baseline = list(
    lookbackYears = BASELINE_LOOKBACK_YEARS,
    matches = nrow(baseline_matches),
    neutralBaseGoalsPerTeam = round(neutral_base_goals, 8),
    eloGoalSlope = round(elo_goal_slope, 8),
    homeGoalEffect = round(home_goal_effect, 8)
  ),
  eloAnchor = list(
    enabledByDefault = TRUE,
    rule = "adjust paired scoring rates so win + 0.5*draw equals Elo expected score"
  ),
  teams = profile_list
)

write_json(
  out,
  forecast_json,
  auto_unbox = TRUE,
  pretty = FALSE,
  digits = NA
)

cat("\n============================================================\n")
cat("ADVANCED FOOTBALL FORECAST MODEL COMPLETE\n")
cat("============================================================\n")
cat("Repository root:", repo_dir, "\n")
cat("Output:", forecast_json, "\n")
cat("Tournament cutoff:", format(cutoff, "%Y-%m-%d"), "\n")
cat("Rule: LAST", MAX_TEAM_GAMES, "matches STRICTLY BEFORE cutoff\n")
cat("Recency half-life:", RECENCY_HALF_LIFE_GAMES, "games\n")
cat("Shrinkage:", SHRINKAGE_GAMES, "games\n")
cat("Raw historical goals/team:", round(raw_goals_per_team, 4), "\n")
cat("Neutral baseline goals/team:", round(neutral_base_goals, 4), "\n")
cat("Tournament teams:", nrow(profiles), "\n")
cat("Min games used:", min(profiles$gamesUsed), "\n")
cat("Max games used:", max(profiles$gamesUsed), "\n")
cat("Audit rows written:", sum(profiles$gamesUsed), "\n\n")

cat("Sample profiles:\n")
print(
  profiles %>%
    select(
      name,
      anchorElo,
      gamesUsed,
      newestMatch,
      oldestMatch,
      attackMultiplier,
      defenceStrength
    ) %>%
    arrange(desc(anchorElo)) %>%
    slice_head(n = 8)
)

cat("\nDone.\n")
