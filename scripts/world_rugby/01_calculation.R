library(data.table)

options(stringsAsFactors = FALSE)

# ============================================================
# World Rugby Union Elo - three-pass calculation
#
# Pass 1:
#   - Time-varying entry rating
#
# Pass 2:
#   - Re-run whole history
#   - Each team's entry rating = Pass 1 performance estimate
#     from first RETRO_GAMES_N games
#
# Pass 3:
#   - Re-run whole history again
#   - Each team's entry rating = Pass 2 performance estimate
#     from first RETRO_GAMES_N games
#
# Only final Pass 3 outputs are written.
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
    dir.exists(file.path(wd, "RugbyUnion")) &&
    dir.exists(file.path(wd, "scripts", "world_rugby"))
  ) {
    return(wd)
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
INPUT_CSV <- file.path(
  repo_dir,
  "RugbyUnion", "pipeline_data", "Matches",
  "rugby_union_results_master.csv"
)

TEAM_ALIASES_TSV <- file.path(
  repo_dir,
  "RugbyUnion", "config",
  "international_teams.tsv"
)

OUT_DIR <- file.path(
  repo_dir,
  "RugbyUnion", "pipeline_data", "Elo"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTPUT_GAME_HISTORY_CSV <- file.path(
  OUT_DIR,
  "world_rugby_union_elo_game_history.csv"
)

OUTPUT_FINAL_RATINGS_CSV <- file.path(
  OUT_DIR,
  "world_rugby_union_elo_final_ratings.csv"
)

if (!file.exists(INPUT_CSV)) {
  stop("Missing input CSV: ", INPUT_CSV)
}

if (!file.exists(TEAM_ALIASES_TSV)) {
  stop("Missing international-team allow-list: ", TEAM_ALIASES_TSV)
}

cat("Repo dir: ", repo_dir, "\n", sep = "")
cat("Input CSV: ", INPUT_CSV, "\n", sep = "")
cat("International team aliases: ", TEAM_ALIASES_TSV, "\n", sep = "")
cat("Output dir: ", OUT_DIR, "\n", sep = "")

# -----------------------------
# Elo settings
# -----------------------------
BASELINE_START_RATING <- 2250
K_VALUE <- 40

# Provisional K for new teams
PROVISIONAL_GAMES <- 50L
PROVISIONAL_K <- 40

# Time-varying entry rating settings for Pass 1
ENTRY_RATING_START <- 2450
ENTRY_RATING_END   <- 1950
ENTRY_DATE_START   <- as.Date("1870-01-01")
ENTRY_DATE_END     <- as.Date("2020-01-01")

# Retro-start settings
RETRO_GAMES_N <- 50L

# -----------------------------
# Helpers
# -----------------------------
expected_score <- function(Ra, Rb) {
  1 / (1 + 10 ^ ((Rb - Ra) / 400))
}

normalise_alias_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- sub("[[:space:]]+Rugby$", "", x, ignore.case = TRUE)
  tolower(x)
}

team_aliases <- fread(
  TEAM_ALIASES_TSV,
  sep = "\t",
  encoding = "UTF-8",
  colClasses = "character",
  na.strings = c("", "NA")
)

required_alias_cols <- c("alias", "canonical")
missing_alias_cols <- setdiff(required_alias_cols, names(team_aliases))

if (length(missing_alias_cols) > 0) {
  stop(
    "international_teams.tsv is missing columns: ",
    paste(missing_alias_cols, collapse = ", ")
  )
}

team_aliases <- team_aliases[
  !is.na(alias) & !is.na(canonical) &
    trimws(alias) != "" & trimws(canonical) != ""
]

team_aliases[, alias := trimws(alias)]
team_aliases[, canonical := trimws(canonical)]
team_aliases[, alias_key := normalise_alias_key(alias)]

alias_conflicts <- team_aliases[
  ,
  .(canonical_count = uniqueN(canonical), canonical_values = paste(sort(unique(canonical)), collapse = " | ")),
  by = alias_key
][canonical_count > 1]

if (nrow(alias_conflicts) > 0) {
  cat("\nConflicting international-team aliases:\n")
  print(alias_conflicts, nrows = 500)
  stop("Stopped because some aliases map to more than one canonical team.")
}

team_aliases <- unique(team_aliases[, .(alias_key, canonical)], by = "alias_key")
alias_map <- setNames(team_aliases$canonical, team_aliases$alias_key)

canonicalise_team_name <- function(x) {
  keys <- normalise_alias_key(x)
  out <- unname(alias_map[keys])
  out[is.na(out)] <- NA_character_
  out
}

entry_rating_for_date <- function(d) {
  if (is.na(d)) return(BASELINE_START_RATING)
  
  if (d <= ENTRY_DATE_START) return(ENTRY_RATING_START)
  if (d >= ENTRY_DATE_END)   return(ENTRY_RATING_END)
  
  frac <- as.numeric(d - ENTRY_DATE_START) / as.numeric(ENTRY_DATE_END - ENTRY_DATE_START)
  ENTRY_RATING_START + frac * (ENTRY_RATING_END - ENTRY_RATING_START)
}

# -----------------------------
# Load and prepare data
# -----------------------------
dt <- fread(INPUT_CSV, encoding = "UTF-8")

required_cols <- c(
  "date", "home_team", "away_team",
  "home_score", "away_score",
  "result", "competition"
)

missing_cols <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dt[, date_raw       := trimws(as.character(date))]
dt[, home_team_raw  := trimws(as.character(home_team))]
dt[, away_team_raw  := trimws(as.character(away_team))]
dt[, result_raw     := trimws(as.character(result))]
dt[, home_score     := suppressWarnings(as.integer(home_score))]
dt[, away_score     := suppressWarnings(as.integer(away_score))]
dt[, competition    := trimws(as.character(competition))]

# Canonicalise through the external international-team allow-list.
# Any side not present in international_teams.tsv is excluded from Elo.
dt[, home_team := canonicalise_team_name(home_team_raw)]
dt[, away_team := canonicalise_team_name(away_team_raw)]

unknown_home <- dt[is.na(home_team) & home_team_raw != "", .N, by = .(team = home_team_raw)]
unknown_away <- dt[is.na(away_team) & away_team_raw != "", .N, by = .(team = away_team_raw)]

unknown_teams <- rbindlist(
  list(unknown_home, unknown_away),
  use.names = TRUE,
  fill = TRUE
)[
  ,
  .(appearances = sum(N)),
  by = team
][order(-appearances, team)]

excluded_match_count <- dt[is.na(home_team) | is.na(away_team), .N]

if (nrow(unknown_teams) > 0) {
  cat("\nUnknown/non-international teams excluded by international_teams.tsv:\n")
  print(unknown_teams, nrows = 500)
  cat("Matches excluded because at least one side is not approved: ",
      excluded_match_count, "\n", sep = "")
} else {
  cat("\nNo unknown/non-international teams found.\n")
}

# Keep only approved international-v-international matches.
dt <- dt[!is.na(home_team) & !is.na(away_team)]

# Keep Draw as Draw; canonicalise winner labels through the same alias file.
dt[, result := fifelse(
  tolower(result_raw) == "draw",
  "Draw",
  canonicalise_team_name(result_raw)
)]

# Parse date. Master is expected to be ISO: YYYY-MM-DD
dt[, Date := as.Date(date_raw, format = "%Y-%m-%d")]

# Remove rows with empty raw date before validation
dt <- dt[!is.na(date_raw) & date_raw != ""]

bad_dates <- dt[is.na(Date), unique(date_raw)]
if (length(bad_dates) > 0) {
  cat("\nUnparsed date values:\n")
  print(bad_dates)
  stop("Stopped because some dates are not in YYYY-MM-DD format.")
}

# Drop unusable rows
dt <- dt[
  !is.na(Date) &
    home_team != "" &
    away_team != "" &
    result != "" &
    competition != ""
]

if (nrow(dt) == 0) {
  stop("No usable rows after cleaning.")
}

# -----------------------------
# Validate result labels and convert to Elo scores
# Elo uses W/D/L, not points margin
# -----------------------------
dt[, ResultType := fcase(
  result == home_team, "H",
  result == away_team, "A",
  result == "Draw", "D",
  default = NA_character_
)]

bad_labels <- dt[is.na(ResultType), .N, by = .(result)][order(-N, result)]

if (nrow(bad_labels) > 0) {
  cat("\nUnmatched result labels. Check international_teams.tsv aliases:\n")
  print(bad_labels, nrows = 500)
  
  cat("\nExample rows:\n")
  print(dt[is.na(ResultType), .(date_raw, home_team, away_team, result)][1:min(100, .N)])
  
  stop("Stopped because some result labels do not match home/away team names.")
}

# Optional consistency check where numeric scores exist
score_mismatch <- dt[
  !is.na(home_score) & !is.na(away_score) &
    (
      (home_score > away_score & ResultType != "H") |
        (home_score < away_score & ResultType != "A") |
        (home_score == away_score & ResultType != "D")
    )
]

if (nrow(score_mismatch) > 0) {
  cat("\nRows where numeric scores disagree with result label:\n")
  print(score_mismatch[, .(
    date_raw, home_team, away_team, home_score, away_score, result, competition
  )][1:min(100, .N)])
  
  stop("Stopped because some numeric scores disagree with result labels.")
}

dt[, HomeScore := fcase(
  ResultType == "H", 1,
  ResultType == "A", 0,
  ResultType == "D", 0.5
)]

dt[, AwayScore := 1 - HomeScore]

# Stable order
setorder(dt, Date, competition, home_team, away_team, result, home_score, away_score)

cat("Usable matches: ", nrow(dt), "\n", sep = "")
cat("Date range: ", as.character(min(dt$Date)), " to ", as.character(max(dt$Date)), "\n", sep = "")

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
  cat("Matches to process: ", n, "\n", sep = "")
  cat("Date range: ", as.character(min(dt_input$Date)), " to ", as.character(max(dt_input$Date)), "\n", sep = "")
  
  ratings_env      <- new.env(hash = TRUE, parent = emptyenv())
  games_env        <- new.env(hash = TRUE, parent = emptyenv())
  first_date_env   <- new.env(hash = TRUE, parent = emptyenv())
  start_rating_env <- new.env(hash = TRUE, parent = emptyenv())
  
  Home         <- dt_input$home_team
  Away         <- dt_input$away_team
  HomeScoreVec <- dt_input$HomeScore
  AwayScoreVec <- dt_input$AwayScore
  DateVec      <- dt_input$Date
  
  HomeFirstAppearance <- logical(n)
  AwayFirstAppearance <- logical(n)
  
  HomeGamesBefore   <- integer(n)
  AwayGamesBefore   <- integer(n)
  HomeRating_Before <- numeric(n)
  AwayRating_Before <- numeric(n)
  
  HomeStartRating <- numeric(n)
  AwayStartRating <- numeric(n)
  
  ExpectedHome <- numeric(n)
  ExpectedAway <- numeric(n)
  KHome        <- numeric(n)
  KAway        <- numeric(n)
  
  HomeRating_After <- numeric(n)
  AwayRating_After <- numeric(n)
  HomeGamesAfter   <- integer(n)
  AwayGamesAfter   <- integer(n)
  
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
    
    HomeGamesBefore[i]   <- Gh
    AwayGamesBefore[i]   <- Ga
    HomeRating_Before[i] <- Rh
    AwayRating_Before[i] <- Ra
    
    HomeStartRating[i] <- home_entry_assigned
    AwayStartRating[i] <- away_entry_assigned
    
    ExpectedHome[i] <- Eh
    ExpectedAway[i] <- Ea
    KHome[i]        <- Kh
    KAway[i]        <- Ka
    
    HomeRating_After[i] <- Rh_new
    AwayRating_After[i] <- Ra_new
    HomeGamesAfter[i]   <- Gh_new
    AwayGamesAfter[i]   <- Ga_new
    
    if (i %% 50000L == 0L) {
      cat("Processed ", i, " matches (", round(100 * i / n, 1), "%)\n", sep = "")
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
    Rating = as.numeric(unlist(mget(teams, envir = ratings_env))),
    Games = as.integer(unlist(mget(teams, envir = games_env))),
    FirstMatchDate = as.Date(
      unlist(mget(teams, envir = first_date_env)),
      origin = "1970-01-01"
    ),
    EntryRating = as.numeric(unlist(mget(teams, envir = start_rating_env)))
  )
  
  setorder(final_ratings, -Rating, Team)
  
  final_ratings[, Rating := round(Rating, 0)]
  final_ratings[, EntryRating := round(EntryRating, 1)]
  final_ratings[, FirstMatchDate := format(FirstMatchDate, "%Y-%m-%d")]
  
  list(dt = dt_out, final = final_ratings)
}

# -----------------------------
# Build retro start map from a pass output
# Uses first N games against pass pre-game opponent ratings
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
    if (n == 0) return(fallback_center)
    
    s <- sum(scores)
    
    if (s <= 0) {
      return(max(lower, min(upper, min(opp_ratings) - 800)))
    }
    
    if (s >= n) {
      return(max(lower, min(upper, max(opp_ratings) + 800)))
    }
    
    f <- function(R) sum(expected_vs(R, opp_ratings)) - s
    
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
  
  team_games <- rbindlist(list(home_long, away_long), use.names = TRUE, fill = TRUE)
  team_games <- team_games[is.finite(OppRating) & is.finite(Score)]
  setorder(team_games, Team, Date)
  
  team_games[, GameIndex := seq_len(.N), by = Team]
  team_games <- team_games[GameIndex <= n_games]
  
  perf_dt <- team_games[, .(
    GamesUsed = .N,
    RetroStart = solve_perf(OppRating, Score)
  ), by = Team]
  
  perf_dt <- perf_dt[!is.na(Team) & GamesUsed >= n_games & is.finite(RetroStart)]
  
  setNames(as.list(perf_dt$RetroStart), perf_dt$Team)
}

# -----------------------------
# Final output writer
# -----------------------------
write_final_outputs <- function(pass_obj,
                                game_history_path,
                                final_ratings_path,
                                pass_name,
                                entry_mode_label) {
  
  game_history_out <- pass_obj$dt[, .(
    date = format(Date, "%Y-%m-%d"),
    competition,
    home_team,
    away_team,
    home_score,
    away_score,
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
  
  fwrite(game_history_out, game_history_path)
  
  final_out <- copy(pass_obj$final)
  
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
  
  fwrite(final_out, final_ratings_path)
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

retro_start_map_pass2 <- build_perf_start_map(
  pass1$dt,
  n_games = RETRO_GAMES_N
)

cat("\nBuilt retro start ratings for Pass 2 from Pass 1 using first ",
    RETRO_GAMES_N, " games.\n", sep = "")
cat("Teams in retro map for Pass 2: ", length(retro_start_map_pass2), "\n", sep = "")

# -----------------------------
# Pass 2
# -----------------------------
pass2 <- run_elo(
  dt_input = dt,
  entry_mode = "retro",
  retro_start_map = retro_start_map_pass2,
  pass_label = "Pass 2 (retro starts from Pass 1)"
)

retro_start_map_pass3 <- build_perf_start_map(
  pass2$dt,
  n_games = RETRO_GAMES_N
)

cat("\nBuilt retro start ratings for Pass 3 from Pass 2 using first ",
    RETRO_GAMES_N, " games.\n", sep = "")
cat("Teams in retro map for Pass 3: ", length(retro_start_map_pass3), "\n", sep = "")

# -----------------------------
# Pass 3 - final
# -----------------------------
pass3 <- run_elo(
  dt_input = dt,
  entry_mode = "retro",
  retro_start_map = retro_start_map_pass3,
  pass_label = "Pass 3 (retro starts from Pass 2) [FINAL]"
)

write_final_outputs(
  pass_obj = pass3,
  game_history_path = OUTPUT_GAME_HISTORY_CSV,
  final_ratings_path = OUTPUT_FINAL_RATINGS_CSV,
  pass_name = "Pass3_RetroStartFromPass2",
  entry_mode_label = "RetroFromPass2FirstN"
)

cat("\nDone.\n")
cat("Final game history: ", OUTPUT_GAME_HISTORY_CSV, "\n", sep = "")
cat("Final ratings: ", OUTPUT_FINAL_RATINGS_CSV, "\n", sep = "")