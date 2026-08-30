# ============================================================
# Pro Go Elo ratings - TWO PASS VERSION (full rebuild)
#
# Pass 1:
#   - All players start at START_R (3000)
#
# Pass 2:
#   - Re-run whole history
#   - Each player's entry rating = Pass 1 rating after first RETRO_GAMES_N games
#     (or last available rating if fewer)
#
# Notes:
#   - Full rebuild (not incremental)
#   - Checkpoint logic removed because Pass 2 requires rerunning everything
# ============================================================

library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
library(tibble)

options(stringsAsFactors = FALSE)

# -----------------------------
# Date window (inclusive)
# -----------------------------
RUN_START_DATE <- as.Date("1900-01-01")
RUN_END_DATE   <- as.Date("2026-12-31")

# -----------------------------
# Settings
# -----------------------------
K_NORMAL <- 20
K_NEW    <- 40
K_NEW_GAMES <- 100L       
START_R  <- 3100         
RETRO_GAMES_N <- 50L    

# -----------------------------
# Paths
# -----------------------------
input_file <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Go/Attempt 2/go_games_from_sgf.csv"
name_fixes_file <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Go/Attempt 2/name_fixes.txt"

# NEW repo structure: no /sports/ and renamed folder go -> Go
repo_go_dir <- "C:/Users/stjuk/Documents/GitHub/J-Ratings/Go"
out_dir <- file.path(repo_go_dir, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Pass 1 debug outputs
history_file_pass1 <- file.path(out_dir, "game_history_pass1.csv")
final_file_pass1   <- file.path(out_dir, "final_ratings_pass1.csv")

# Final outputs (Pass 2)
history_file <- file.path(out_dir, "game_history.csv")
final_file   <- file.path(out_dir, "final_ratings.csv")

# -----------------------------
# Helpers
# -----------------------------
expected_score <- function(Ra, Rb) {
  1 / (1 + 10 ^ ((Rb - Ra) / 400))
}

opponent_weight <- function(n, cap = 50L, floor = 0.05) {
  n <- pmin(as.numeric(n), cap)
  floor + (1 - floor) * (n / cap)
}

update_elo <- function(Ra, Rb, result, k) {
  Ea <- expected_score(Ra, Rb)
  Ra + k * (result - Ea)
}

normalise_player <- function(x) {
  x <- str_trim(x)
  x <- str_remove(x, "\\s+[0-9]p$")
  str_trim(x)
}

apply_name_fixes <- function(x, fixes_tbl) {
  x <- as.character(x)
  if (nrow(fixes_tbl) == 0) return(x)
  idx <- match(x, fixes_tbl$from_name)
  ifelse(!is.na(idx), fixes_tbl$to_name[idx], x)
}

has_space_in_name <- function(x) {
  x <- clean_name(x)
  !is.na(x) & str_detect(x, "\\s")
}

# -----------------------------
# Load, clean, dedupe
# -----------------------------
games_raw <- read_csv(input_file, show_col_types = FALSE)

name_fixes <- tibble::tibble(
  from_name = character(),
  to_name = character()
)

if (file.exists(name_fixes_file)) {
  name_fixes <- read_csv(
    name_fixes_file,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    transmute(
      from_name = str_trim(as.character(from_name)),
      to_name   = str_trim(as.character(to_name))
    ) %>%
    filter(from_name != "", to_name != "") %>%
    distinct(from_name, .keep_all = TRUE)
}

cat("Name fixes loaded:", nrow(name_fixes), "\n")


games_clean <- games_raw %>%
  mutate(
    Date = as.Date(Date),
    Black = normalise_player(Black),
    White = normalise_player(White),
    Black = apply_name_fixes(Black, name_fixes),
    White = apply_name_fixes(White, name_fixes),
    ResultCode = str_trim(ResultCode),
    Event = if ("Event" %in% names(.)) str_trim(as.character(Event)) else NA_character_
  ) %>%
  filter(
    has_space_in_name(Black),
    has_space_in_name(White)
  ) %>%
  filter(
    !str_detect(Black, regex("Alpha|AlphaGo|Golaxy|Fine Art|Leela|Leela Zero|Lc0|KataGo|ELF OpenGo|CGI| NR", ignore_case = TRUE)),
    !str_detect(White, regex("Alpha|AlphaGo|Golaxy|Fine Art|Leela|Leela Zero|Lc0|KataGo|ELF OpenGo|CGI| NR", ignore_case = TRUE)),
    !str_detect(Black, "&"),
    !str_detect(White, "&"),
    !str_detect(Black, ","),
    !str_detect(White, ",")
  ) %>%
  filter(!is.na(Date), Black != "", White != "", !is.na(ResultCode), ResultCode != "") %>%
  filter(Date >= RUN_START_DATE, Date <= RUN_END_DATE) %>%
  mutate(
    GameKey = paste(Date, Black, White, ResultCode, coalesce(Event, ""), sep = "|")
  ) %>%
  arrange(Date, Black, White, ResultCode, Event) %>%
  group_by(GameKey) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(Date, Black, White, ResultCode, Event)

cat("Total games after clean/dedupe:", nrow(games_clean), "\n")
cat("Date range:", as.character(min(games_clean$Date)), "to", as.character(max(games_clean$Date)), "\n")

# -----------------------------
# Generic Elo runner
# -----------------------------
run_elo <- function(games_df,
                    entry_mode = c("flat", "retro"),
                    retro_start_map = NULL,
                    pass_label = "pass") {
  
  entry_mode <- match.arg(entry_mode)
  
  n <- nrow(games_df)
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Running ", pass_label, "\n", sep = "")
  cat("Games to process:", n, "\n")
  
  # State
  ratings <- list()       # player -> rating
  games_cnt <- list()     # player -> games played
  first_date <- list()    # player -> first date
  entry_rating <- list()  # player -> assigned entry rating
  
  # Preallocate outputs (faster than bind_rows in loop)
  out_Date <- as.Date(rep(NA_character_, n))
  out_Black <- character(n)
  out_White <- character(n)
  out_ResultCode <- character(n)
  out_Event <- character(n)
  out_GameKey <- character(n)
  
  out_BlackFirst <- logical(n)
  out_WhiteFirst <- logical(n)
  
  out_BlackEntry <- numeric(n)
  out_WhiteEntry <- numeric(n)
  
  out_Kb <- numeric(n)
  out_Kw <- numeric(n)
  
  out_Rb_Before <- numeric(n)
  out_Rw_Before <- numeric(n)
  out_Rb_After  <- numeric(n)
  out_Rw_After  <- numeric(n)
  
  out_Gb_Before <- integer(n)
  out_Gw_Before <- integer(n)
  out_Gb_After  <- integer(n)
  out_Gw_After  <- integer(n)
  
  out_Eb <- numeric(n)
  out_Ew <- numeric(n)
  
  valid_row <- logical(n)
  
  for (i in seq_len(n)) {
    g <- games_df[i, ]
    
    res <- g$ResultCode
    if (startsWith(res, "B")) {
      result_black <- 1
      result_white <- 0
    } else if (startsWith(res, "W")) {
      result_black <- 0
      result_white <- 1
    } else {
      # skip non-standard results
      next
    }
    
    b <- g$Black
    w <- g$White
    d <- g$Date
    
    black_first <- is.null(ratings[[b]])
    white_first <- is.null(ratings[[w]])
    
    # Assign entry ratings on first appearance
    if (black_first) {
      b_entry <- if (entry_mode == "flat") {
        START_R
      } else {
        if (!is.null(retro_start_map) && b %in% names(retro_start_map)) {
          as.numeric(retro_start_map[[b]])
        } else {
          START_R
        }
      }
      ratings[[b]] <- b_entry
      games_cnt[[b]] <- 0L
      first_date[[b]] <- d
      entry_rating[[b]] <- b_entry
    }
    
    if (white_first) {
      w_entry <- if (entry_mode == "flat") {
        START_R
      } else {
        if (!is.null(retro_start_map) && w %in% names(retro_start_map)) {
          as.numeric(retro_start_map[[w]])
        } else {
          START_R
        }
      }
      ratings[[w]] <- w_entry
      games_cnt[[w]] <- 0L
      first_date[[w]] <- d
      entry_rating[[w]] <- w_entry
    }
    
    Rb <- as.numeric(ratings[[b]])
    Rw <- as.numeric(ratings[[w]])
    gb <- as.integer(games_cnt[[b]])
    gw <- as.integer(games_cnt[[w]])
    
    Kb_base <- if (gb < K_NEW_GAMES) K_NEW else K_NORMAL
    Kw_base <- if (gw < K_NEW_GAMES) K_NEW else K_NORMAL
    
    wb <- opponent_weight(gw, 50L, 0.05)   # black faces white
    ww <- opponent_weight(gb, 50L, 0.05)   # white faces black
    
    Kb <- Kb_base * wb
    Kw <- Kw_base * ww
    
    Eb <- expected_score(Rb, Rw)
    Ew <- 1 - Eb
    
    Rb_new <- update_elo(Rb, Rw, result_black, Kb)
    Rw_new <- update_elo(Rw, Rb, result_white, Kw)
    
    gb_new <- gb + 1L
    gw_new <- gw + 1L
    
    ratings[[b]] <- Rb_new
    ratings[[w]] <- Rw_new
    games_cnt[[b]] <- gb_new
    games_cnt[[w]] <- gw_new
    
    # Save row
    valid_row[i] <- TRUE
    
    out_Date[i] <- d
    out_Black[i] <- b
    out_White[i] <- w
    out_ResultCode[i] <- res
    out_Event[i] <- if ("Event" %in% names(g)) as.character(g$Event) else NA_character_
    out_GameKey[i] <- g$GameKey    
    out_BlackFirst[i] <- black_first
    out_WhiteFirst[i] <- white_first
    
    out_BlackEntry[i] <- as.numeric(entry_rating[[b]])
    out_WhiteEntry[i] <- as.numeric(entry_rating[[w]])
    
    out_Kb[i] <- Kb
    out_Kw[i] <- Kw
    
    out_Rb_Before[i] <- Rb
    out_Rw_Before[i] <- Rw
    out_Rb_After[i] <- Rb_new
    out_Rw_After[i] <- Rw_new
    
    out_Gb_Before[i] <- gb
    out_Gw_Before[i] <- gw
    out_Gb_After[i] <- gb_new
    out_Gw_After[i] <- gw_new
    
    out_Eb[i] <- Eb
    out_Ew[i] <- Ew
    
    if (i %% 20000L == 0L) {
      cat("Processed", i, "games (", round(100 * i / n, 1), "%)\n")
      flush.console()
    }
  }
  
  # History output
  game_history <- tibble(
    Date = out_Date,
    Black = out_Black,
    White = out_White,
    ResultCode = out_ResultCode,
    Event = out_Event,
    GameKey = out_GameKey,
    
    BlackFirstAppearance = out_BlackFirst,
    WhiteFirstAppearance = out_WhiteFirst,
    BlackStartRating = out_BlackEntry,
    WhiteStartRating = out_WhiteEntry,
    
    Gb_Before = out_Gb_Before,
    Gw_Before = out_Gw_Before,
    Kb = out_Kb,
    Kw = out_Kw,
    Rb_Before = out_Rb_Before,
    Rw_Before = out_Rw_Before,
    ExpectedBlack = out_Eb,
    ExpectedWhite = out_Ew,
    Rb_After = out_Rb_After,
    Rw_After = out_Rw_After,
    Gb_After = out_Gb_After,
    Gw_After = out_Gw_After
  ) %>%
    filter(valid_row)
  
  # Final ratings output
  players <- names(ratings)
  
  final_table <- tibble(
    name = players,
    rating = round(as.numeric(unlist(ratings[players])), 0),
    games = as.integer(unlist(games_cnt[players])),
    first_date = as.Date(as.numeric(unlist(first_date[players])), origin = "1970-01-01"),
    entry_rating = round(as.numeric(unlist(entry_rating[players])), 1)
  ) %>%
    mutate(
      is_seed = games >= 20
    ) %>%
    arrange(desc(rating), name)
  
  list(
    history = game_history,
    final = final_table
  )
}

# -----------------------------
# Build retro start map from Pass 1
# Start = rating after RETRO_GAMES_N games (or last if fewer)
# -----------------------------
build_perf_start_map <- function(pass1_history,
                                 n_games = RETRO_GAMES_N,
                                 min_opp_games = 50L,
                                 min_valid_opponents = 25L,
                                 fallback_center = START_R) {
  
  expected_vs <- function(R, Ro) {
    1 / (1 + 10 ^ ((Ro - R) / 400))
  }
  
  solve_perf <- function(opp_ratings, scores, lower = 0, upper = 6000) {
    
    opp_ratings <- as.numeric(opp_ratings)
    scores <- as.numeric(scores)
    
    ok <- is.finite(opp_ratings) & is.finite(scores)
    opp_ratings <- opp_ratings[ok]
    scores <- scores[ok]
    
    n <- length(scores)
    if (n == 0) return(fallback_center)
    
    s <- sum(scores)
    
    if (s <= 0) return(max(lower, min(upper, min(opp_ratings) - 800)))
    if (s >= n) return(max(lower, min(upper, max(opp_ratings) + 800)))
    
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
  
  black_long <- pass1_history %>%
    transmute(
      Player = Black,
      OppRating = Rw_Before,
      OppGames = Gw_Before,
      Score = if_else(str_starts(ResultCode, "B"), 1, 0),
      Date = Date
    )
  
  white_long <- pass1_history %>%
    transmute(
      Player = White,
      OppRating = Rb_Before,
      OppGames = Gb_Before,
      Score = if_else(str_starts(ResultCode, "W"), 1, 0),
      Date = Date
    )
  
  team_games <- bind_rows(black_long, white_long) %>%
    filter(is.finite(OppRating), is.finite(Score), !is.na(OppGames)) %>%
    arrange(Player, Date) %>%
    group_by(Player) %>%
    mutate(GameIndex = row_number()) %>%
    ungroup() %>%
    filter(GameIndex <= n_games)
  
  valid_games <- team_games %>%
    filter(OppGames >= min_opp_games)
  
  perf <- valid_games %>%
    group_by(Player) %>%
    summarise(
      GamesUsed = n(),
      RetroStart = solve_perf(OppRating, Score),
      .groups = "drop"
    ) %>%
    filter(GamesUsed >= min_valid_opponents, is.finite(RetroStart))
  
  setNames(as.list(perf$RetroStart), perf$Player)
}

# -----------------------------
# Pass 1 (flat 3000 starts)
# -----------------------------
pass1 <- run_elo(
  games_df = games_clean,
  entry_mode = "flat",
  retro_start_map = NULL,
  pass_label = "Pass 1 (all start at 3000)"
)

write_csv(pass1$history, history_file_pass1)

final_pass1 <- pass1$final %>%
  mutate(
    Pass = "Pass1_FlatStart",
    EntryMode = "Flat3000",
    StartR = START_R,
    KNormal = K_NORMAL,
    KNew = K_NEW,
    KNewGames = K_NEW_GAMES,
    RetroGamesN = RETRO_GAMES_N
  )

write_csv(final_pass1, final_file_pass1)

# -----------------------------
# Build retro starts (first 25 games)
# -----------------------------
retro_start_map <- build_perf_start_map(pass1$history, n_games = RETRO_GAMES_N)

cat("\nBuilt retro starts from Pass 1 using first", RETRO_GAMES_N, "games.\n")
cat("Players in retro map:", length(retro_start_map), "\n")

# -----------------------------
# Pass 2 (retro starts)
# -----------------------------
pass2 <- run_elo(
  games_df = games_clean,
  entry_mode = "retro",
  retro_start_map = retro_start_map,
  pass_label = "Pass 2 (retro starts from Pass 1)"
)

write_csv(pass2$history, history_file)

final_table <- pass2$final %>%
#final_table <- pass2$final %>%
  mutate(
    Pass = "Pass2_RetroStart",
    EntryMode = "RetroFromPass1FirstN",
    StartR = START_R,
    KNormal = K_NORMAL,
    KNew = K_NEW,
    KNewGames = K_NEW_GAMES,
    RetroGamesN = RETRO_GAMES_N
  )

write_csv(final_table, final_file)

cat("\nDone.\n")
cat("Pass 1 history:", history_file_pass1, "\n")
cat("Pass 1 final:", final_file_pass1, "\n")
cat("Pass 2 history (final):", history_file, "\n")
cat("Pass 2 final (final):", final_file, "\n")

# ============================================================
# Plot (same style as your world football plot)
# - all player lines + yearly average (black)
# - optional player subset
# ============================================================

read_game_history <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(Date = as.Date(Date))
}

plot_player_history_retro_style <- function(game_history,
                                            players = NULL,
                                            use_player_filter = FALSE,
                                            entry_from = as.Date("1900-01-01"),
                                            entry_to   = as.Date("2100-12-31")) {
  
  black_long <- game_history %>%
    transmute(Date, Player = Black, Rating = Rb_After)
  
  white_long <- game_history %>%
    transmute(Date, Player = White, Rating = Rw_After)
  
  ratings_long <- bind_rows(black_long, white_long) %>%
    arrange(Player, Date) %>%
    group_by(Player, Date) %>%
    slice_tail(n = 1) %>%   # one point per player per day
    ungroup()
  
  # First appearance date per player
  first_dates <- ratings_long %>%
    group_by(Player) %>%
    summarise(FirstDate = min(Date), .groups = "drop")
  
  players_in_window <- first_dates %>%
    filter(FirstDate >= entry_from, FirstDate <= entry_to) %>%
    pull(Player)
  
  plot_dt <- ratings_long %>%
    filter(Player %in% players_in_window)
  
  if (use_player_filter && !is.null(players) && length(players) > 0) {
    plot_dt <- plot_dt %>% filter(Player %in% players)
  }
  
  # Yearly average of plotted players
  yearly_avg <- plot_dt %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    summarise(AvgRating = mean(Rating, na.rm = TRUE), .groups = "drop") %>%
    mutate(YearDate = as.Date(paste0(Year, "-07-01")))
  
  ggplot() +
    geom_line(
      data = plot_dt,
      aes(x = Date, y = Rating, group = Player, colour = Player),
      linewidth = 0.35,
      alpha = 0.65
    ) +
    geom_line(
      data = yearly_avg,
      aes(x = YearDate, y = AvgRating),
      linewidth = 1.3,
      colour = "black"
    ) +
    labs(
      title = "Pro Go Elo Ratings",
      subtitle = paste0("Pass 2 retro starts from Pass 1 first ", RETRO_GAMES_N, " games"),
      x = NULL,
      y = "Elo Rating",
      colour = "Player"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = if (use_player_filter) "right" else "none"
    )
}
