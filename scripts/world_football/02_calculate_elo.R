library(data.table)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

INPUT_CSV <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "processed",
  "results_with_winner.csv"
)

TEAM_LOOKUP_FILE <- file.path(
  repo_dir,
  "team_flag_lookup.csv"
)

OUT_DIR <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "Elo"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTPUT_GAME_HISTORY_CSV_PASS1 <- file.path(OUT_DIR, "world_football_elo_game_history_pass1.csv")
OUTPUT_FINAL_RATINGS_CSV_PASS1 <- file.path(OUT_DIR, "world_football_elo_final_ratings_pass1.csv")

OUTPUT_GAME_HISTORY_CSV_PASS2 <- file.path(OUT_DIR, "world_football_elo_game_history_pass2.csv")
OUTPUT_FINAL_RATINGS_CSV_PASS2 <- file.path(OUT_DIR, "world_football_elo_final_ratings_pass2.csv")

OUTPUT_GAME_HISTORY_CSV <- file.path(OUT_DIR, "world_football_elo_game_history.csv")
OUTPUT_FINAL_RATINGS_CSV <- file.path(OUT_DIR, "world_football_elo_final_ratings.csv")

OUTPUT_GAME_HISTORY_CSV_PASS3 <- OUTPUT_GAME_HISTORY_CSV
OUTPUT_FINAL_RATINGS_CSV_PASS3 <- OUTPUT_FINAL_RATINGS_CSV

# -----------------------------
# Elo settings
# -----------------------------
BASELINE_START_RATING <- 2400
K_VALUE <- 40

PROVISIONAL_GAMES <- 50L
PROVISIONAL_K <- 40

ENTRY_RATING_START <- 2600
ENTRY_RATING_END <- 2100
ENTRY_DATE_START <- as.Date("1870-01-01")
ENTRY_DATE_END <- as.Date("2020-01-01")

RETRO_GAMES_N <- 50L

# -----------------------------
# Helpers
# -----------------------------
expected_score <- function(Ra, Rb) {
  1 / (1 + 10 ^ ((Rb - Ra) / 400))
}

normalise_team_name <- function(x) {
  x0 <- trimws(as.character(x))
  
  team_map <- c(
    "Åland" = "Åland Islands"
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
# Input checks
# -----------------------------
if (!file.exists(INPUT_CSV)) {
  stop("Input CSV not found: ", INPUT_CSV)
}

if (!file.exists(TEAM_LOOKUP_FILE)) {
  stop("Team lookup file not found: ", TEAM_LOOKUP_FILE)
}

# -----------------------------
# Load team flag lookup
# -----------------------------
team_lookup <- fread(TEAM_LOOKUP_FILE, encoding = "unknown")

required_lookup_cols <- c("Team", "Flag")
missing_lookup_cols <- setdiff(required_lookup_cols, names(team_lookup))

if (length(missing_lookup_cols) > 0) {
  stop(
    "Team lookup file is missing required columns: ",
    paste(missing_lookup_cols, collapse = ", ")
  )
}

team_lookup[, Team := iconv(as.character(Team), from = "", to = "UTF-8", sub = "")]
team_lookup[, Flag := iconv(as.character(Flag), from = "", to = "UTF-8", sub = "")]

team_lookup[, Team := normalise_team_name(Team)]
team_lookup[, Flag := trimws(tolower(as.character(Flag)))]
team_lookup[Flag %in% c("", "NA", "NULL"), Flag := NA_character_]

setorder(team_lookup, Team)
team_lookup <- team_lookup[!duplicated(Team), .(Team, Flag)]

cat("Team lookup rows:", nrow(team_lookup), "\n")

# -----------------------------
# Load and prepare match data
# -----------------------------
dt <- fread(INPUT_CSV, encoding = "UTF-8")

required_cols <- c(
  "date",
  "home_team",
  "away_team",
  "result",
  "score",
  "tournament"
)

missing_cols <- setdiff(required_cols, names(dt))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dt[, date_raw := trimws(as.character(date))]
dt[, home_team := normalise_team_name(home_team)]
dt[, away_team := normalise_team_name(away_team)]
dt[, result := trimws(as.character(result))]
dt[, score := trimws(as.character(score))]
dt[, tournament := trimws(as.character(tournament))]

dt[, result := ifelse(tolower(result) == "draw", "Draw", normalise_team_name(result))]

EXCLUDE_TEAMS <- c(
  "Ynys Môn",
  "Isle of Wight",
  "Shetland",
  "Western Isles",
  "Yorkshire",
  "Surrey",
  "Hitra",
  "Gozo",
  "Seborga",
  "Sealand",
  "Republic of St. Pauli",
  "Cascadia",
  "Sápmi",
  "Tamil Eelam",
  "Panjab",
  "United Koreans in Japan",
  "Biafra",
  "Darfur",
  "Romani people"
)

dt <- dt[
  !(home_team %in% EXCLUDE_TEAMS | away_team %in% EXCLUDE_TEAMS)
]

dt[, Date := as.Date(date_raw, format = "%Y-%m-%d")]

dt <- dt[
  !is.na(Date) &
    home_team != "" &
    away_team != "" &
    result != ""
]

dt[, ResultType := fcase(
  result == home_team, "H",
  result == away_team, "A",
  result == "Draw", "D",
  default = NA_character_
)]

bad_labels <- dt[is.na(ResultType), .N, by = .(result)][order(-N, result)]

if (nrow(bad_labels) > 0) {
  cat("\nUnmatched result labels. Add these to normalise_team_name if needed:\n")
  print(bad_labels, nrows = 500)
  
  cat("\nExample rows:\n")
  print(dt[is.na(ResultType), .(date_raw, home_team, away_team, result)][1:min(100, .N)])
  
  stop("Stopped because some result labels do not match home/away team names.")
}

dt[, HomeScore := fcase(
  ResultType == "H", 1,
  ResultType == "A", 0,
  ResultType == "D", 0.5
)]

dt[, AwayScore := 1 - HomeScore]

setorder(dt, Date, tournament, home_team, away_team, result)

cat("Loaded matches:", nrow(dt), "\n")
cat("Date range:", as.character(min(dt$Date)), "to", as.character(max(dt$Date)), "\n")

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
  
  Home <- dt_input$home_team
  Away <- dt_input$away_team
  HomeScoreVec <- dt_input$HomeScore
  AwayScoreVec <- dt_input$AwayScore
  DateVec <- dt_input$Date
  
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
    
    if (i %% 50000L == 0L) {
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
  final_ratings[, EntryRating := round(EntryRating, 1)]
  final_ratings[, FirstMatchDate := format(FirstMatchDate, "%Y-%m-%d")]
  
  list(
    dt = dt_out,
    final = final_ratings
  )
}

# -----------------------------
# Build retro start map
# -----------------------------
build_perf_start_map <- function(pass_dt,
                                 n_games = 50L,
                                 fallback_center = BASELINE_START_RATING) {
  
  expected_vs <- function(R, Ro) {
    1 / (1 + 10 ^ ((Ro - R) / 400))
  }
  
  solve_perf <- function(opp_ratings, scores, lower = 0, upper = 4000) {
    opp_ratings <- as.numeric(opp_ratings)
    scores <- as.numeric(scores)
    
    ok <- is.finite(opp_ratings) & is.finite(scores)
    opp_ratings <- opp_ratings[ok]
    scores <- scores[ok]
    
    n <- length(scores)
    
    if (n == 0) {
      return(fallback_center)
    }
    
    s <- sum(scores)
    
    if (s <= 0) {
      return(max(lower, min(upper, min(opp_ratings) - 800)))
    }
    
    if (s >= n) {
      return(max(lower, min(upper, max(opp_ratings) + 800)))
    }
    
    f <- function(R) {
      sum(expected_vs(R, opp_ratings)) - s
    }
    
    flo <- f(lower)
    fhi <- f(upper)
    
    if (!is.finite(flo) || !is.finite(fhi) || flo * fhi > 0) {
      avg_opp <- mean(opp_ratings)
      p <- s / n
      p <- min(0.999, max(0.001, p))
      R0 <- avg_opp + 400 * log10(p / (1 - p))
      return(max(lower, min(upper, R0)))
    }
    
    uniroot(f, lower = lower, upper = upper, tol = 1e-8)$root
  }
  
  home_long <- pass_dt[, .(
    Team = home_team,
    OppRating = AwayRating_Before,
    Score = HomeScore,
    Date = Date
  )]
  
  away_long <- pass_dt[, .(
    Team = away_team,
    OppRating = HomeRating_Before,
    Score = AwayScore,
    Date = Date
  )]
  
  team_games <- rbindlist(
    list(home_long, away_long),
    use.names = TRUE
  )
  
  team_games <- team_games[
    is.finite(OppRating) &
      is.finite(Score)
  ]
  
  setorder(team_games, Team, Date)
  
  team_games[, GameIndex := seq_len(.N), by = Team]
  team_games <- team_games[GameIndex <= n_games]
  
  perf <- team_games[, .(
    GamesUsed = .N,
    RetroStart = solve_perf(OppRating, Score)
  ), by = Team]
  
  perf <- perf[
    GamesUsed >= n_games &
      is.finite(RetroStart)
  ]
  
  setNames(as.list(perf$RetroStart), perf$Team)
}

# -----------------------------
# Standard output writers
# -----------------------------
write_pass_outputs <- function(pass_obj,
                               game_history_path,
                               final_ratings_path,
                               pass_name,
                               entry_mode_label) {
  
  game_history_out <- copy(pass_obj$dt[, .(
    date = format(Date, "%Y-%m-%d"),
    tournament,
    home_team,
    away_team,
    result,
    score,
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
  )])
  
  game_history_out[team_lookup, on = .(home_team = Team), HomeFlag := i.Flag]
  game_history_out[team_lookup, on = .(away_team = Team), AwayFlag := i.Flag]
  game_history_out[team_lookup, on = .(result = Team), ResultFlag := i.Flag]
  
  setcolorder(game_history_out, c(
    "date",
    "tournament",
    "home_team", "HomeFlag",
    "away_team", "AwayFlag",
    "result", "ResultFlag",
    "score",
    "HomeScore", "AwayScore",
    "HomeFirstAppearance", "AwayFirstAppearance",
    "HomeGamesBefore", "AwayGamesBefore",
    "HomeRating_Before", "AwayRating_Before",
    "HomeStartRating", "AwayStartRating",
    "ExpectedHome", "ExpectedAway",
    "KHome", "KAway",
    "HomeRating_After", "AwayRating_After",
    "HomeGamesAfter", "AwayGamesAfter"
  ))
  
  fwrite(game_history_out, game_history_path)
  
  final_out <- copy(pass_obj$final)
  
  final_out[team_lookup, on = .(Team), Flag := i.Flag]
  
  final_out[, `:=`(
    Pass = pass_name,
    BaseK = K_VALUE,
    ProvisionalK = PROVISIONAL_K,
    ProvisionalGames = PROVISIONAL_GAMES,
    EntryMode = entry_mode_label,
    EntryRatingStart = ENTRY_RATING_START,
    EntryRatingEnd = ENTRY_RATING_END,
    EntryDateStart = format(ENTRY_DATE_START, "%Y-%m-%d"),
    EntryDateEnd = format(ENTRY_DATE_END, "%Y-%m-%d"),
    BaselineStartRating = BASELINE_START_RATING,
    RetroGamesN = RETRO_GAMES_N
  )]
  
  setcolorder(final_out, c(
    "Team", "Flag", "Rating", "Games", "FirstMatchDate", "EntryRating",
    "Pass", "BaseK", "ProvisionalK", "ProvisionalGames",
    "EntryMode", "EntryRatingStart", "EntryRatingEnd",
    "EntryDateStart", "EntryDateEnd", "BaselineStartRating", "RetroGamesN"
  ))
  
  fwrite(final_out, final_ratings_path)
  
  cat("Wrote game history:", game_history_path, "\n")
  cat("Wrote final ratings:", final_ratings_path, "\n")
}

# -----------------------------
# Pass 1
# -----------------------------
pass1 <- run_elo(
  dt_input = dt,
  entry_mode = "time",
  retro_start_map = NULL,
  pass_label = "Pass 1 (time-based entry)"
)

write_pass_outputs(
  pass_obj = pass1,
  game_history_path = OUTPUT_GAME_HISTORY_CSV_PASS1,
  final_ratings_path = OUTPUT_FINAL_RATINGS_CSV_PASS1,
  pass_name = "Pass1_TimeEntry",
  entry_mode_label = "TimeLinear"
)

retro_start_map_pass2 <- build_perf_start_map(
  pass1$dt,
  n_games = RETRO_GAMES_N
)

retro_start_map_pass2[["Scotland"]] <- 2620
retro_start_map_pass2[["England"]] <- 2600

cat("\nBuilt retro start ratings for Pass 2 from Pass 1 using first", RETRO_GAMES_N, "games.\n")
cat("Teams in retro map (Pass 2):", length(retro_start_map_pass2), "\n")

# -----------------------------
# Pass 2
# -----------------------------
pass2 <- run_elo(
  dt_input = dt,
  entry_mode = "retro",
  retro_start_map = retro_start_map_pass2,
  pass_label = "Pass 2 (retro starts from Pass 1)"
)

write_pass_outputs(
  pass_obj = pass2,
  game_history_path = OUTPUT_GAME_HISTORY_CSV_PASS2,
  final_ratings_path = OUTPUT_FINAL_RATINGS_CSV_PASS2,
  pass_name = "Pass2_RetroStartFromPass1",
  entry_mode_label = "RetroFromPass1FirstN"
)

# -----------------------------
# Pass 3
# -----------------------------
retro_start_map_pass3 <- build_perf_start_map(
  pass2$dt,
  n_games = RETRO_GAMES_N
)

retro_start_map_pass3[["Scotland"]] <- 2550
retro_start_map_pass3[["England"]] <- 2530

cat("\nBuilt retro start ratings for Pass 3 from Pass 2 using first", RETRO_GAMES_N, "games.\n")
cat("Teams in retro map (Pass 3):", length(retro_start_map_pass3), "\n")

pass3 <- run_elo(
  dt_input = dt,
  entry_mode = "retro",
  retro_start_map = retro_start_map_pass3,
  pass_label = "Pass 3 (retro starts from Pass 2) [FINAL]"
)

write_pass_outputs(
  pass_obj = pass3,
  game_history_path = OUTPUT_GAME_HISTORY_CSV_PASS3,
  final_ratings_path = OUTPUT_FINAL_RATINGS_CSV_PASS3,
  pass_name = "Pass3_RetroStartFromPass2",
  entry_mode_label = "RetroFromPass2FirstN"
)

# -----------------------------
# Final checks
# -----------------------------
final_history <- fread(OUTPUT_GAME_HISTORY_CSV, encoding = "UTF-8")
final_ratings <- fread(OUTPUT_FINAL_RATINGS_CSV, encoding = "UTF-8")

cat("\nDone.\n")
cat("Pass 1 game history:", OUTPUT_GAME_HISTORY_CSV_PASS1, "\n")
cat("Pass 1 final ratings:", OUTPUT_FINAL_RATINGS_CSV_PASS1, "\n")
cat("Pass 2 game history:", OUTPUT_GAME_HISTORY_CSV_PASS2, "\n")
cat("Pass 2 final ratings:", OUTPUT_FINAL_RATINGS_CSV_PASS2, "\n")
cat("Pass 3 game history (final):", OUTPUT_GAME_HISTORY_CSV, "\n")
cat("Pass 3 final ratings (final):", OUTPUT_FINAL_RATINGS_CSV, "\n")
cat("Final history rows:", nrow(final_history), "\n")
cat("Final ratings rows:", nrow(final_ratings), "\n")
cat("Latest match date:", max(final_history$date, na.rm = TRUE), "\n")