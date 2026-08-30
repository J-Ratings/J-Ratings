# scripts/EuropeanFootball/04_simulate_premier_league_test.R

library(dplyr)
library(readr)
library(jsonlite)

options(stringsAsFactors = FALSE)


# -----------------------------
# Test settings
# -----------------------------

TEST_SEASON <- "2024-25"
N_SIMS      <- 10000L
RANDOM_SEED <- 20240816L
K_FACTOR    <- 20

# -----------------------------
# Paths
# -----------------------------

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

elo_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Elo"
)

hist_csv <- file.path(
  elo_dir,
  "football_elo_game_history.csv"
)

tournament_file <- file.path(
  repo_dir,
  "EuropeanFootball",
  "data",
  "simulations",
  "premier-league",
  paste0(TEST_SEASON, ".json")
)

out_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "data",
  "simulations",
  "premier-league",
  "tests"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(
  out_dir,
  paste0(TEST_SEASON, "_dynamic_full_season.json")
)

# -----------------------------
# Helpers
# -----------------------------

clamp <- function(x, lo, hi) {
  pmax(lo, pmin(hi, x))
}

# -----------------------------
# Load tournament
# -----------------------------

if (!file.exists(tournament_file)) {
  stop(
    "Missing tournament JSON:\n",
    tournament_file,
    "\nThe tournament JSON must already exist."
  )
}

tournament <- fromJSON(
  tournament_file,
  simplifyDataFrame = TRUE
)

teams <- as.character(tournament$teams$name)
n_teams <- length(teams)

team_index <- setNames(
  seq_along(teams),
  teams
)

id_to_name <- setNames(
  tournament$teams$name,
  tournament$teams$id
)

fixtures <- tournament$matches %>%
  transmute(
    date = as.Date(date),
    home = unname(id_to_name[as.character(home)]),
    away = unname(id_to_name[as.character(away)])
  ) %>%
  arrange(date, home, away)

if (any(is.na(fixtures$home)) || any(is.na(fixtures$away))) {
  stop("Tournament JSON contains a team ID that could not be resolved.")
}

if (nrow(fixtures) != 380L) {
  stop("Expected 380 Premier League fixtures, found ", nrow(fixtures), ".")
}

tournament_start <- min(fixtures$date)

home_idx <- unname(team_index[fixtures$home])
away_idx <- unname(team_index[fixtures$away])

if (any(is.na(home_idx)) || any(is.na(away_idx))) {
  stop("Failed to convert one or more fixture teams to integer indices.")
}

n_fixtures <- length(home_idx)

# -----------------------------
# Load Elo history
# -----------------------------

if (!file.exists(hist_csv)) {
  stop("Missing Elo history CSV: ", hist_csv)
}

ghist <- read_csv(
  hist_csv,
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date),
    Home = trimws(as.character(Home)),
    Away = trimws(as.character(Away)),
    HomeRating_After = as.numeric(HomeRating_After),
    AwayRating_After = as.numeric(AwayRating_After)
  )

# -----------------------------
# Starting ratings
# -----------------------------

starting_ratings <- bind_rows(
  ghist %>%
    transmute(
      team = Home,
      date = Date,
      rating = HomeRating_After
    ),
  ghist %>%
    transmute(
      team = Away,
      date = Date,
      rating = AwayRating_After
    )
) %>%
  filter(
    team %in% teams,
    !is.na(date),
    date < tournament_start,
    is.finite(rating)
  ) %>%
  arrange(team, date) %>%
  group_by(team) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(
    team,
    rating_date = date,
    rating
  )

missing_rating_teams <- setdiff(
  teams,
  starting_ratings$team
)

if (length(missing_rating_teams) > 0) {
  stop(
    "Missing pre-tournament Elo rating for: ",
    paste(missing_rating_teams, collapse = ", ")
  )
}

starting_elo <- starting_ratings$rating[
  match(teams, starting_ratings$team)
]

if (any(!is.finite(starting_elo))) {
  stop("Starting Elo vector contains missing/non-finite values.")
}

# -----------------------------
# Vectorised Monte Carlo simulation
# -----------------------------
#
# IMPORTANT:
# Dynamic Elo is still updated after EVERY simulated match.
#
# The optimisation is that all N_SIMS seasons are processed in parallel
# for each fixture. Therefore R loops over 380 fixtures rather than
# 10,000 x 380 individual matches.

set.seed(RANDOM_SEED)

# Matrices:
# rows    = simulations
# columns = teams
ratings <- matrix(
  rep(starting_elo, each = N_SIMS),
  nrow = N_SIMS,
  ncol = n_teams
)

points <- matrix(
  0,
  nrow = N_SIMS,
  ncol = n_teams
)

sim_rows <- seq_len(N_SIMS)

for (i in seq_len(n_fixtures)) {
  
  h <- home_idx[i]
  a <- away_idx[i]
  
  home_elo <- ratings[, h]
  away_elo <- ratings[, a]
  
  # Same Elo expectation as before.
  expected_home <- 1 / (
    1 + 10 ^ ((away_elo - home_elo) / 400)
  )
  
  # Same Premier League draw model as before.
  abs_gap <- abs(home_elo - away_elo)
  
  draw_prob <- clamp(
    0.302 - 0.00048 * abs_gap,
    0.13,
    0.31
  )
  
  home_win_prob <- clamp(
    expected_home - draw_prob / 2,
    0,
    1 - draw_prob
  )
  
  u <- runif(N_SIMS)
  
  home_win <- u < home_win_prob
  draw <- !home_win & u < (home_win_prob + draw_prob)
  away_win <- !(home_win | draw)
  
  # League points.
  points[, h] <- points[, h] +
    3 * home_win +
    1 * draw
  
  points[, a] <- points[, a] +
    3 * away_win +
    1 * draw
  
  # Elo actual scores.
  home_score <- as.numeric(home_win) +
    0.5 * as.numeric(draw)
  
  away_score <- as.numeric(away_win) +
    0.5 * as.numeric(draw)
  
  # Dynamic Elo update after EVERY simulated match.
  ratings[, h] <-
    home_elo +
    K_FACTOR * (home_score - expected_home)
  
  ratings[, a] <-
    away_elo +
    K_FACTOR * (away_score - (1 - expected_home))
  
  if (i %% 50L == 0L || i == n_fixtures) {
    cat(
      "Processed fixture ",
      i,
      " / ",
      n_fixtures,
      " across ",
      N_SIMS,
      " simulations\n",
      sep = ""
    )
  }
}

# -----------------------------
# Final league positions
# -----------------------------
#
# Exact future scores are still not simulated.
# Equal-points teams therefore receive tiny random jitter only for
# ordering purposes, exactly as in the previous MVP.

tie_jitter <- matrix(
  runif(N_SIMS * n_teams, min = 0, max = 1e-6),
  nrow = N_SIMS,
  ncol = n_teams
)

ranking_scores <- points + tie_jitter

finish_counts <- matrix(
  0L,
  nrow = n_teams,
  ncol = n_teams,
  dimnames = list(
    teams,
    as.character(seq_len(n_teams))
  )
)

for (sim in sim_rows) {
  
  order_idx <- order(
    ranking_scores[sim, ],
    decreasing = TRUE
  )
  
  finish_counts[
    cbind(
      order_idx,
      seq_len(n_teams)
    )
  ] <- finish_counts[
    cbind(
      order_idx,
      seq_len(n_teams)
    )
  ] + 1L
}

# -----------------------------
# Results
# -----------------------------

finish_prob <- finish_counts / N_SIMS

expected_points <- colMeans(points)
expected_final_elo <- colMeans(ratings)

results <- tibble(
  team = teams,
  startingElo = starting_elo,
  titlePct = 100 * finish_prob[, 1],
  top4Pct = 100 * rowSums(
    finish_prob[, 1:4, drop = FALSE]
  ),
  relegationPct = 100 * rowSums(
    finish_prob[, 18:20, drop = FALSE]
  ),
  expectedPoints = expected_points,
  expectedFinalElo = expected_final_elo
)

for (position in seq_len(n_teams)) {
  results[[paste0("p", position, "Pct")]] <-
    100 * finish_prob[, position]
}

results <- results %>%
  arrange(
    desc(titlePct),
    desc(top4Pct),
    desc(expectedPoints)
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

starting_ratings_out <- starting_ratings %>%
  arrange(desc(rating), team) %>%
  mutate(
    rating_date = format(rating_date, "%Y-%m-%d"),
    rating = round(rating, 1)
  )

output <- list(
  competition = "Premier League",
  season = TEST_SEASON,
  tournamentStart = format(tournament_start, "%Y-%m-%d"),
  simulations = as.integer(N_SIMS),
  model = list(
    ratingMode = "dynamic",
    startingRatings = "latest J-Ratings Elo before tournament start",
    ratingUpdate = "after every simulated match",
    kFactor = as.integer(K_FACTOR),
    resultMode = "win_draw_loss",
    tiebreakMode = "random_for_equal_points",
    implementation = "vectorised_across_simulations"
  ),
  startingRatings = starting_ratings_out,
  results = results
)

write_json(
  output,
  out_file,
  auto_unbox = TRUE,
  pretty = TRUE,
  na = "null"
)

# -----------------------------
# Console summary
# -----------------------------

cat("\n")
cat("Premier League dynamic simulation test\n")
cat("--------------------------------------\n")
cat("Season:             ", TEST_SEASON, "\n", sep = "")
cat("Tournament start:   ", format(tournament_start, "%Y-%m-%d"), "\n", sep = "")
cat("Fixtures simulated: ", n_fixtures, "\n", sep = "")
cat("Simulations:        ", N_SIMS, "\n", sep = "")
cat("Rating mode:        dynamic\n")
cat("Implementation:     vectorised across simulations\n")
cat("K-factor:           ", K_FACTOR, "\n", sep = "")
cat("Points-tie breaker: random (temporary MVP)\n")
cat("Output:             ", out_file, "\n", sep = "")

cat("\nStarting ratings:\n")
print(starting_ratings_out, n = Inf)

cat("\nSimulation headline probabilities:\n")
print(
  results %>%
    select(
      team,
      startingElo,
      titlePct,
      top4Pct,
      relegationPct,
      expectedPoints,
      expectedFinalElo
    ),
  n = Inf
)

