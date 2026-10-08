# ============================================================
# NFL Elo (single global pool) - TWO PASS VERSION
#
# Pass 1:
#   - Time-varying entry rating (your current method)
#
# Pass 2:
#   - Re-run whole history
#   - Each team's entry rating = Pass 1 rating after first RETRO_GAMES_N games
#     (or last available rating if team has fewer than RETRO_GAMES_N games)
# ============================================================

library(data.table)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
INPUT_CSV <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/NFL/games.csv"

OUT_DIR <- "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings/NFL/outputs"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Pass 1 debug outputs
OUTPUT_GAME_HISTORY_CSV_PASS1 <- file.path(OUT_DIR, "nfl_elo_game_history_pass1.csv")
OUTPUT_FINAL_RATINGS_CSV_PASS1 <- file.path(OUT_DIR, "nfl_elo_final_ratings_pass1.csv")

# Final outputs (Pass 2)
OUTPUT_GAME_HISTORY_CSV <- file.path(OUT_DIR, "nfl_elo_game_history.csv")
OUTPUT_FINAL_RATINGS_CSV <- file.path(OUT_DIR, "nfl_elo_final_ratings.csv")

# -----------------------------
# Elo settings
# -----------------------------
BASELINE_START_RATING <- 2600
K_VALUE <- 20

# Provisional K for new teams
PROVISIONAL_GAMES <- 32L
PROVISIONAL_K <- 20

# Time-varying entry rating settings (Pass 1)
# NFL history is modern enough that a tighter range is sensible
ENTRY_RATING_START <- 2600
ENTRY_RATING_END <- 2600
ENTRY_DATE_START <- as.Date("1920-01-01")
ENTRY_DATE_END <- as.Date("2028-01-01")

# Retro-start settings (used to build Pass 2 from Pass 1)
RETRO_GAMES_N <- 50L

# Filter settings
INCLUDE_PLAYOFFS <- FALSE   # set TRUE if you want postseason games included

# -----------------------------
# Helpers
# -----------------------------
expected_score <- function(Ra, Rb) {
  1 / (1 + 10 ^ ((Rb - Ra) / 400))
}

normalise_team_name <- function(x) {
  x0 <- trimws(as.character(x))
  
  # Keep abbreviations as-is (NFL dataset is already clean), but allow aliases if needed later.
  team_map <- c(
    "WSH" = "WAS"  # only if your data version mixes WSH/WAS; otherwise harmless
  )
  
  ifelse(x0 %in% names(team_map), unname(team_map[x0]), x0)
}

entry_rating_for_date <- function(d) {
  if (is.na(d)) return(BASELINE_START_RATING)
  
  if (d <= ENTRY_DATE_START) return(ENTRY_RATING_START)
  if (d >= ENTRY_DATE_END) return(ENTRY_RATING_END)
  
  frac <- as.numeric(d - ENTRY_DATE_START) / as.numeric(ENTRY_DATE_END - ENTRY_DATE_START)
  ENTRY_RATING_START + frac * (ENTRY_RATING_END - ENTRY_RATING_START)
}

# -----------------------------
# Load and prepare data
# -----------------------------
dt <- fread(INPUT_CSV, encoding = "UTF-8")

required_cols <- c("gameday", "away_team", "away_score", "home_team", "home_score", "game_type")
missing_cols <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Clean and normalise
dt[, gameday_raw := trimws(as.character(gameday))]
dt[, home_team := normalise_team_name(home_team)]
dt[, away_team := normalise_team_name(away_team)]
dt[, game_type := trimws(as.character(game_type))]

# Parse date
# nflverse games.csv usually has gameday already as YYYY-MM-DD (often IDate)
dt[, Date := as.Date(gameday)]

# Fallback if needed
dt[is.na(Date), Date := as.Date(gameday_raw, format = "%Y-%m-%d")]

# Drop unusable rows
dt <- dt[
  !is.na(Date) &
    home_team != "" & away_team != "" &
    !is.na(home_score) & !is.na(away_score)
]

# Filter game types
if (INCLUDE_PLAYOFFS) {
  dt <- dt[game_type %in% c("REG", "WC", "DIV", "CON", "SB")]
} else {
  dt <- dt[game_type == "REG"]
}

# Add a tournament/competition label to match your existing output structure
dt[, tournament := fifelse(game_type == "REG", "NFL Regular Season", "NFL Playoffs")]

# If neutral_site is missing in some file versions, create it
if (!("neutral_site" %in% names(dt))) {
  dt[, neutral_site := 0L]
}

# -----------------------------
# Convert scores to Elo match outcomes
# -----------------------------
# NFL ties are rare but do happen, so handle them properly.
dt[, ResultType := fcase(
  home_score > away_score, "H",
  home_score < away_score, "A",
  home_score == away_score, "D",
  default = NA_character_
)]

bad_rows <- dt[is.na(ResultType)]
if (nrow(bad_rows) > 0) {
  stop("Found rows with invalid score/result data.")
}

dt[, HomeScore := fcase(
  ResultType == "H", 1,
  ResultType == "A", 0,
  ResultType == "D", 0.5
)]
dt[, AwayScore := 1 - HomeScore]

# Create a result label (for output compatibility with your football format)
dt[, result := fcase(
  ResultType == "H", home_team,
  ResultType == "A", away_team,
  ResultType == "D", "Draw"
)]

# Stable order
# week is useful but not always sufficient alone; game_id keeps ordering stable within a day
sort_cols <- c("Date")
if ("season" %in% names(dt)) sort_cols <- c(sort_cols, "season")
if ("week" %in% names(dt)) sort_cols <- c(sort_cols, "week")
if ("game_id" %in% names(dt)) sort_cols <- c(sort_cols, "game_id")
setorderv(dt, sort_cols)

# -----------------------------
# Generic Elo runner
# -----------------------------
run_elo <- function(dt_input,
                    entry_mode = c("time", "retro"),
                    retro_start_map = NULL,
                    pass_label = "pass") {
  
  entry_mode <- match.arg(entry_mode)
  
  n <- nrow(dt_input)
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Running ", pass_label, "\n", sep = "")
  cat("Matches to process:", n, "\n")
  cat("Date range:", as.character(min(dt_input$Date)), "to", as.character(max(dt_input$Date)), "\n")
  
  ratings_env <- new.env(hash = TRUE, parent = emptyenv())
  games_env <- new.env(hash = TRUE, parent = emptyenv())
  first_date_env <- new.env(hash = TRUE, parent = emptyenv())
  start_rating_env <- new.env(hash = TRUE, parent = emptyenv())
  
  # Pull columns into vectors once
  Home <- dt_input$home_team
  Away <- dt_input$away_team
  HomeScoreVec <- dt_input$HomeScore
  AwayScoreVec <- dt_input$AwayScore
  DateVec <- dt_input$Date
  
  # Preallocate outputs
  HomeFirstAppearance <- logical(n)
  AwayFirstAppearance <- logical(n)
  
  HomeGamesBefore <- integer(n)
  AwayGamesBefore <- integer(n)
  HomeRating_Before <- numeric(n)
  AwayRating_Before <- numeric(n)
  
  HomeStartRating <- numeric(n)
  AwayStartRating <- numeric(n)
  
  ExpectedHome <- numeric(n)
  ExpectedAway <- numeric(n)
  KHome <- numeric(n)
  KAway <- numeric(n)
  
  HomeRating_After <- numeric(n)
  AwayRating_After <- numeric(n)
  HomeGamesAfter <- integer(n)
  AwayGamesAfter <- integer(n)
  
  for (i in seq_len(n)) {
    home <- Home[i]
    away <- Away[i]
    match_date <- DateVec[i]
    
    home_exists <- exists(home, envir = ratings_env, inherits = FALSE)
    away_exists <- exists(away, envir = ratings_env, inherits = FALSE)
    
    home_first <- !home_exists
    away_first <- !away_exists
    
    # Assign entry ratings when teams first appear
    if (home_first) {
      home_entry <- if (entry_mode == "time") {
        entry_rating_for_date(match_date)
      } else {
        if (!is.null(retro_start_map) && home %in% names(retro_start_map)) {
          retro_start_map[[home]]
        } else {
          entry_rating_for_date(match_date)
        }
      }
      
      assign(home, home_entry, envir = ratings_env)
      assign(home, match_date, envir = first_date_env)
      assign(home, home_entry, envir = start_rating_env)
      assign(home, 0L, envir = games_env)
    }
    
    if (away_first) {
      away_entry <- if (entry_mode == "time") {
        entry_rating_for_date(match_date)
      } else {
        if (!is.null(retro_start_map) && away %in% names(retro_start_map)) {
          retro_start_map[[away]]
        } else {
          entry_rating_for_date(match_date)
        }
      }
      
      assign(away, away_entry, envir = ratings_env)
      assign(away, match_date, envir = first_date_env)
      assign(away, away_entry, envir = start_rating_env)
      assign(away, 0L, envir = games_env)
    }
    
    Rh <- get(home, envir = ratings_env, inherits = FALSE)
    Ra <- get(away, envir = ratings_env, inherits = FALSE)
    
    Gh <- get(home, envir = games_env, inherits = FALSE)
    Ga <- get(away, envir = games_env, inherits = FALSE)
    
    home_entry_assigned <- get(home, envir = start_rating_env, inherits = FALSE)
    away_entry_assigned <- get(away, envir = start_rating_env, inherits = FALSE)
    
    Eh <- expected_score(Rh, Ra)
    Ea <- 1 - Eh
    
    Sh <- HomeScoreVec[i]
    Sa <- AwayScoreVec[i]
    
    # Provisional K
    Kh <- if (Gh < PROVISIONAL_GAMES) PROVISIONAL_K else K_VALUE
    Ka <- if (Ga < PROVISIONAL_GAMES) PROVISIONAL_K else K_VALUE
    
    Rh_new <- Rh + Kh * (Sh - Eh)
    Ra_new <- Ra + Ka * (Sa - Ea)
    
    Gh_new <- Gh + 1L
    Ga_new <- Ga + 1L
    
    assign(home, Rh_new, envir = ratings_env)
    assign(away, Ra_new, envir = ratings_env)
    assign(home, Gh_new, envir = games_env)
    assign(away, Ga_new, envir = games_env)
    
    HomeFirstAppearance[i] <- home_first
    AwayFirstAppearance[i] <- away_first
    
    HomeGamesBefore[i] <- Gh
    AwayGamesBefore[i] <- Ga
    HomeRating_Before[i] <- Rh
    AwayRating_Before[i] <- Ra
    
    HomeStartRating[i] <- home_entry_assigned
    AwayStartRating[i] <- away_entry_assigned
    
    ExpectedHome[i] <- Eh
    ExpectedAway[i] <- Ea
    KHome[i] <- Kh
    KAway[i] <- Ka
    
    HomeRating_After[i] <- Rh_new
    AwayRating_After[i] <- Ra_new
    HomeGamesAfter[i] <- Gh_new
    AwayGamesAfter[i] <- Ga_new
    
    if (i %% 5000L == 0L) {
      cat("Processed", i, "matches (", round(100 * i / n, 1), "%)\n")
      flush.console()
    }
  }
  
  dt_out <- copy(dt_input)
  dt_out[, `:=`(
    HomeFirstAppearance = HomeFirstAppearance,
    AwayFirstAppearance = AwayFirstAppearance,
    
    HomeGamesBefore = HomeGamesBefore,
    AwayGamesBefore = AwayGamesBefore,
    HomeRating_Before = HomeRating_Before,
    AwayRating_Before = AwayRating_Before,
    
    HomeStartRating = HomeStartRating,
    AwayStartRating = AwayStartRating,
    
    ExpectedHome = ExpectedHome,
    ExpectedAway = ExpectedAway,
    KHome = KHome,
    KAway = KAway,
    
    HomeRating_After = HomeRating_After,
    AwayRating_After = AwayRating_After,
    HomeGamesAfter = HomeGamesAfter,
    AwayGamesAfter = AwayGamesAfter
  )]
  
  teams <- ls(ratings_env, all.names = TRUE)
  
  final_ratings <- data.table(
    Team = teams,
    Rating = as.numeric(mget(teams, envir = ratings_env)),
    Games = as.integer(unlist(mget(teams, envir = games_env))),
    FirstMatchDate = as.Date(unlist(mget(teams, envir = first_date_env)), origin = "1970-01-01"),
    EntryRating = as.numeric(unlist(mget(teams, envir = start_rating_env)))
  )
  
  setorder(final_ratings, -Rating, Team)
  final_ratings[, Rating := round(Rating, 0)]
  final_ratings[, EntryRating := round(EntryRating, 0)]
  final_ratings[, FirstMatchDate := format(FirstMatchDate, "%Y-%m-%d")]
  
  list(
    dt = dt_out,
    final = final_ratings
  )
}

# -----------------------------
# Build retro start map from Pass 1
# Start = rating after RETRO_GAMES_N games (or last if fewer)
# -----------------------------
build_perf_start_map <- function(pass1_dt, n_games = 50L, fallback_center = BASELINE_START_RATING) {
  
  expected_vs <- function(R, Ro) 1 / (1 + 10 ^ ((Ro - R) / 400))
  
  solve_perf <- function(opp_ratings, scores, lower = 0, upper = 4000) {
    
    opp_ratings <- as.numeric(opp_ratings)
    scores <- as.numeric(scores)
    
    ok <- is.finite(opp_ratings) & is.finite(scores)
    opp_ratings <- opp_ratings[ok]
    scores <- scores[ok]
    
    n <- length(scores)
    if (n == 0) return(fallback_center)
    
    s <- sum(scores)
    
    # ties exist, so s can be fractional; handle 0 and n extremes
    if (s <= 0) return(max(lower, min(upper, min(opp_ratings) - 800)))
    if (s >= n) return(max(lower, min(upper, max(opp_ratings) + 800)))
    
    f <- function(R) sum(expected_vs(R, opp_ratings)) - s
    
    flo <- f(lower)
    fhi <- f(upper)
    
    # If it doesn't bracket (rare), do a safe approximation around mean opponent
    if (!is.finite(flo) || !is.finite(fhi) || flo * fhi > 0) {
      avg_opp <- mean(opp_ratings)
      p <- s / n
      p <- min(0.999, max(0.001, p))
      R0 <- avg_opp + 400 * log10(p / (1 - p))
      return(max(lower, min(upper, R0)))
    }
    
    uniroot(f, lower = lower, upper = upper, tol = 1e-8)$root
  }
  
  # One row per team-match with opponent pre-game rating + actual score
  home_long <- pass1_dt[, .(
    Team = home_team,
    OppRating = AwayRating_Before,
    Score = HomeScore,
    Date = Date
  )]
  
  away_long <- pass1_dt[, .(
    Team = away_team,
    OppRating = HomeRating_Before,
    Score = AwayScore,
    Date = Date
  )]
  
  team_games <- rbindlist(list(home_long, away_long), use.names = TRUE, fill = TRUE)
  team_games <- team_games[is.finite(OppRating) & is.finite(Score)]
  setorder(team_games, Team, Date)
  
  # First N games per team
  team_games[, GameIndex := seq_len(.N), by = Team]
  team_games <- team_games[GameIndex <= n_games]
  
  perf_dt <- team_games[, .(
    GamesUsed  = .N,
    RetroStart = round(solve_perf(OppRating, Score), 0)  # <- nearest integer
  ), by = Team]
  
  # Only teams with at least n_games get a retro start
  perf_dt <- perf_dt[!is.na(Team) & GamesUsed >= n_games & is.finite(RetroStart)]
  setNames(as.list(perf_dt$RetroStart), perf_dt$Team)
}

# -----------------------------
# Pass 1 (time-based entry)
# -----------------------------
pass1 <- run_elo(
  dt_input = dt,
  entry_mode = "time",
  retro_start_map = NULL,
  pass_label = "Pass 1 (time-based entry)"
)

# Write Pass 1 game history
game_history_out_pass1 <- pass1$dt[, .(
  date = format(Date, "%Y-%m-%d"),
  season = if ("season" %in% names(pass1$dt)) season else NA_integer_,
  week = if ("week" %in% names(pass1$dt)) week else NA_integer_,
  game_type,
  tournament,
  home_team,
  away_team,
  away_score_raw = away_score,
  home_score_raw = home_score,
  result,
  HomeScore,
  AwayScore,
  
  HomeFirstAppearance,
  AwayFirstAppearance,
  
  HomeGamesBefore,
  AwayGamesBefore,
  HomeRating_Before,
  AwayRating_Before,
  HomeStartRating,
  AwayStartRating,
  ExpectedHome,
  ExpectedAway,
  KHome,
  KAway,
  HomeRating_After,
  AwayRating_After,
  HomeGamesAfter,
  AwayGamesAfter
)]
fwrite(game_history_out_pass1, OUTPUT_GAME_HISTORY_CSV_PASS1)

# Write Pass 1 final ratings
final_pass1 <- copy(pass1$final)
final_pass1[, `:=`(
  Pass = "Pass1_TimeEntry",
  BaseK = K_VALUE,
  ProvisionalK = PROVISIONAL_K,
  ProvisionalGames = PROVISIONAL_GAMES,
  EntryMode = "TimeLinear",
  EntryRatingStart = ENTRY_RATING_START,
  EntryRatingEnd = ENTRY_RATING_END,
  EntryDateStart = format(ENTRY_DATE_START, "%Y-%m-%d"),
  EntryDateEnd = format(ENTRY_DATE_END, "%Y-%m-%d"),
  BaselineStartRating = BASELINE_START_RATING,
  RetroGamesN = RETRO_GAMES_N,
  IncludePlayoffs = INCLUDE_PLAYOFFS
)]
fwrite(final_pass1, OUTPUT_FINAL_RATINGS_CSV_PASS1)

# -----------------------------
# Build retro starts from Pass 1
# -----------------------------
retro_start_map <- build_perf_start_map(pass1$dt, n_games = RETRO_GAMES_N)

cat("\nBuilt retro start ratings from Pass 1 using first", RETRO_GAMES_N, "games.\n")
cat("Teams in retro map:", length(retro_start_map), "\n")

# -----------------------------
# Pass 2 (retro starts)
# -----------------------------
pass2 <- run_elo(
  dt_input = dt,
  entry_mode = "retro",
  retro_start_map = retro_start_map,
  pass_label = "Pass 2 (retro starts from Pass 1)"
)

# -----------------------------
# Write Pass 2 outputs (final)
# -----------------------------
game_history_out <- pass2$dt[, .(
  date = format(Date, "%Y-%m-%d"),
  season = if ("season" %in% names(pass2$dt)) season else NA_integer_,
  week = if ("week" %in% names(pass2$dt)) week else NA_integer_,
  game_type,
  tournament,
  home_team,
  away_team,
  away_score_raw = away_score,
  home_score_raw = home_score,
  result,
  HomeScore,
  AwayScore,
  
  HomeFirstAppearance,
  AwayFirstAppearance,
  
  HomeGamesBefore,
  AwayGamesBefore,
  HomeRating_Before,
  AwayRating_Before,
  HomeStartRating,
  AwayStartRating,
  ExpectedHome,
  ExpectedAway,
  KHome,
  KAway,
  HomeRating_After,
  AwayRating_After,
  HomeGamesAfter,
  AwayGamesAfter
)]
fwrite(game_history_out, OUTPUT_GAME_HISTORY_CSV)

final_ratings <- copy(pass2$final)
final_ratings[, `:=`(
  Pass = "Pass2_RetroStart",
  BaseK = K_VALUE,
  ProvisionalK = PROVISIONAL_K,
  ProvisionalGames = PROVISIONAL_GAMES,
  EntryMode = "RetroFromPass1FirstN",
  EntryRatingStart = ENTRY_RATING_START,
  EntryRatingEnd = ENTRY_RATING_END,
  EntryDateStart = format(ENTRY_DATE_START, "%Y-%m-%d"),
  EntryDateEnd = format(ENTRY_DATE_END, "%Y-%m-%d"),
  BaselineStartRating = BASELINE_START_RATING,
  RetroGamesN = RETRO_GAMES_N,
  IncludePlayoffs = INCLUDE_PLAYOFFS
)]
fwrite(final_ratings, OUTPUT_FINAL_RATINGS_CSV)

cat("\nDone.\n")
cat("Pass 1 game history:", OUTPUT_GAME_HISTORY_CSV_PASS1, "\n")
cat("Pass 1 final ratings:", OUTPUT_FINAL_RATINGS_CSV_PASS1, "\n")
cat("Pass 2 game history (final):", OUTPUT_GAME_HISTORY_CSV, "\n")
cat("Pass 2 final ratings (final):", OUTPUT_FINAL_RATINGS_CSV, "\n")