# ============================================================
# Continue Go Elo from the 2025 checkpoint using GoRatings
#
# Established players:
#   - start from their exact 31 December 2025 rating
#   - retain their historical games-played count
#
# New/unmatched players:
#   - Pass 1 starts at START_R
#   - Pass 2 uses a retrospective performance start where
#     sufficient evidence exists
#
# Duplicate GoRatings IDs with the same normalised display name
# are merged upstream and share one canonical live Elo identity.
#
# Historical GoGoD outputs are not overwritten.
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tibble)

options(stringsAsFactors = FALSE)

# -----------------------------
# Rating settings
# -----------------------------
START_R <- 3100

K_NORMAL <- 20
K_NEW <- 40
K_NEW_GAMES <- 100L

RETRO_GAMES_N <- 50L
RETRO_MIN_OPP_GAMES <- 50L
RETRO_MIN_VALID_OPPONENTS <- 25L

LIVE_START_DATE <- as.Date("2026-01-01")

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

checkpoint_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "checkpoint",
  "ratings_2025-12-31.csv"
)

historical_final_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "Elo",
  "final_ratings.csv"
)

games_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed",
  "goratings_games_2026.csv"
)

player_map_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed",
  "goratings_player_map.csv"
)

live_elo_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "live_Elo"
)

dir.create(
  live_elo_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Pass 1 diagnostic outputs
history_pass1_file <- file.path(
  live_elo_dir,
  "game_history_2026_pass1.csv"
)

ratings_pass1_file <- file.path(
  live_elo_dir,
  "final_ratings_2026_pass1.csv"
)

# Final Pass 2 outputs
history_output_file <- file.path(
  live_elo_dir,
  "game_history_2026.csv"
)

ratings_output_file <- file.path(
  live_elo_dir,
  "final_ratings_2026.csv"
)

# -----------------------------
# Input checks
# -----------------------------
required_files <- c(
  checkpoint_file,
  historical_final_file,
  games_file,
  player_map_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required files:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}

check_columns <- function(
    data,
    required,
    label
) {
  missing <- setdiff(
    required,
    names(data)
  )
  
  if (length(missing) > 0) {
    stop(
      label,
      " is missing columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
}

# -----------------------------
# Read inputs
# -----------------------------
checkpoint <- read_csv(
  checkpoint_file,
  show_col_types = FALSE
)

historical_final <- read_csv(
  historical_final_file,
  show_col_types = FALSE
)

games <- read_csv(
  games_file,
  show_col_types = FALSE,
  col_types = cols(
    BlackID = col_character(),
    WhiteID = col_character(),
    WinnerID = col_character(),
    .default = col_guess()
  )
) %>%
  mutate(
    Date = as.Date(Date),
    BlackID = as.character(BlackID),
    WhiteID = as.character(WhiteID),
    WinnerID = as.character(WinnerID)
  ) %>%
  filter(
    !is.na(Date),
    Date >= LIVE_START_DATE
  ) %>%
  arrange(
    Date,
    BlackID,
    WhiteID,
    ResultCode,
    SameSignatureSequence,
    GameKey
  )

player_map <- read_csv(
  player_map_file,
  show_col_types = FALSE,
  col_types = cols(
    goratings_id = col_character(),
    canonical_goratings_id =
      col_character(),
    .default = col_guess()
  )
) %>%
  mutate(
    goratings_id = as.character(
      goratings_id
    ),
    last_historical_game = as.Date(
      last_historical_game
    )
  )

# -----------------------------
# Validate columns
# -----------------------------
check_columns(
  checkpoint,
  c(
    "name",
    "rating",
    "games",
    "last_historical_game"
  ),
  "Checkpoint"
)

check_columns(
  historical_final,
  c(
    "name",
    "first_date",
    "entry_rating"
  ),
  "Historical final ratings"
)

check_columns(
  games,
  c(
    "Date",
    "BlackID",
    "Black",
    "WhiteID",
    "White",
    "ResultCode",
    "Event",
    "GameKey"
  ),
  "GoRatings games"
)

check_columns(
  player_map,
  c(
    "goratings_id",
    "goratings_name",
    "canonical_goratings_id",
    "historical_name",
    "match_status",
    "checkpoint_rating",
    "checkpoint_games"
  ),
  "Player map"
)

# -----------------------------
# Build player starting-state table
# -----------------------------
historical_metadata <- historical_final %>%
  transmute(
    historical_name = as.character(name),
    historical_first_date = as.Date(
      first_date
    ),
    historical_entry_rating = as.numeric(
      entry_rating
    )
  ) %>%
  distinct(
    historical_name,
    .keep_all = TRUE
  )

# Raw GoRatings IDs can be aliases of one canonical player.
raw_to_canonical_id <- setNames(
  as.character(
    player_map$canonical_goratings_id
  ),
  as.character(
    player_map$goratings_id
  )
)

canonicalise_live_id <- function(x) {
  x <- as.character(x)
  mapped <- unname(
    raw_to_canonical_id[x]
  )
  
  ifelse(
    !is.na(mapped) & mapped != "",
    mapped,
    x
  )
}

# One starting-state row per canonical live player.
player_state <- player_map %>%
  left_join(
    historical_metadata,
    by = "historical_name"
  ) %>%
  mutate(
    canonical_goratings_id =
      as.character(
        canonical_goratings_id
      ),
    canonical_name = if_else(
      match_status == "unmatched_or_new",
      goratings_name,
      historical_name
    ),
    is_new_player =
      match_status == "unmatched_or_new",
    checkpoint_rating = if_else(
      is_new_player,
      NA_real_,
      as.numeric(checkpoint_rating)
    ),
    checkpoint_games = if_else(
      is_new_player,
      NA_integer_,
      as.integer(checkpoint_games)
    )
  ) %>%
  arrange(
    canonical_goratings_id,
    desc(!is_new_player),
    goratings_id
  ) %>%
  group_by(
    canonical_goratings_id
  ) %>%
  summarise(
    goratings_id =
      first(canonical_goratings_id),
    goratings_name =
      first(goratings_name),
    canonical_name =
      first(canonical_name),
    historical_name =
      first(historical_name),
    match_status =
      first(match_status),
    is_new_player =
      first(is_new_player),
    checkpoint_rating =
      first(checkpoint_rating),
    checkpoint_games =
      first(checkpoint_games),
    historical_first_date =
      first(historical_first_date),
    historical_entry_rating =
      first(historical_entry_rating),
    alias_count = n(),
    .groups = "drop"
  )

if (
  anyDuplicated(
    player_state$goratings_id
  ) > 0
) {
  stop(
    "Player-state table contains duplicate canonical GoRatings IDs."
  )
}

# Convert every game to canonical IDs before Elo is calculated.
# Keep the source IDs so the original feed remains auditable.
games <- games %>%
  mutate(
    BlackSourceID = BlackID,
    WhiteSourceID = WhiteID,
    WinnerSourceID = WinnerID,
    BlackID =
      canonicalise_live_id(BlackID),
    WhiteID =
      canonicalise_live_id(WhiteID),
    WinnerID =
      canonicalise_live_id(WinnerID)
  )

missing_game_ids <- setdiff(
  unique(
    c(
      games$BlackID,
      games$WhiteID
    )
  ),
  player_state$goratings_id
)

if (length(missing_game_ids) > 0) {
  stop(
    "Games contain canonical player IDs absent from the player map: ",
    paste(
      missing_game_ids,
      collapse = ", "
    )
  )
}

# If the same physical game arrived under two alias IDs, collapse
# it after canonicalisation. SameSignatureSequence preserves
# genuine repeated games with the same players/date/result.
games_before_alias_dedupe <- nrow(games)

games <- games %>%
  distinct(
    Date,
    BlackID,
    WhiteID,
    ResultCode,
    Event,
    SameSignatureSequence,
    .keep_all = TRUE
  ) %>%
  arrange(
    Date,
    BlackID,
    WhiteID,
    ResultCode,
    SameSignatureSequence,
    GameKey
  )

alias_game_duplicates_removed <-
  games_before_alias_dedupe -
  nrow(games)

if (alias_game_duplicates_removed > 0L) {
  cat(
    "Alias duplicate game rows removed:",
    alias_game_duplicates_removed,
    "\n"
  )
}

# Named lookup vectors
canonical_name_map <- setNames(
  player_state$canonical_name,
  player_state$goratings_id
)

new_player_map <- setNames(
  player_state$is_new_player,
  player_state$goratings_id
)

checkpoint_rating_map <- setNames(
  player_state$checkpoint_rating,
  player_state$goratings_id
)

checkpoint_games_map <- setNames(
  player_state$checkpoint_games,
  player_state$goratings_id
)

historical_first_date_map <- setNames(
  player_state$historical_first_date,
  player_state$goratings_id
)

historical_entry_rating_map <- setNames(
  player_state$historical_entry_rating,
  player_state$goratings_id
)

# Replace source names with canonical website names
games <- games %>%
  mutate(
    BlackSourceName = Black,
    WhiteSourceName = White,
    Black = unname(
      canonical_name_map[BlackID]
    ),
    White = unname(
      canonical_name_map[WhiteID]
    )
  )

# -----------------------------
# Basic validation report
# -----------------------------
cat(
  "Checkpoint players:",
  nrow(checkpoint),
  "\n"
)

cat(
  "Raw GoRatings IDs:",
  nrow(player_map),
  "\n"
)

cat(
  "Canonical GoRatings players:",
  nrow(player_state),
  "\n"
)

cat(
  "Live games:",
  nrow(games),
  "\n"
)

cat(
  "Earliest live game:",
  format(
    min(games$Date),
    "%Y-%m-%d"
  ),
  "\n"
)

cat(
  "Latest live game:",
  format(
    max(games$Date),
    "%Y-%m-%d"
  ),
  "\n"
)

cat(
  "Historical players matched:",
  sum(!player_state$is_new_player),
  "\n"
)

cat(
  "Players starting as new:",
  sum(player_state$is_new_player),
  "\n"
)

# -----------------------------
# Elo helpers
# -----------------------------
expected_score <- function(
    rating_a,
    rating_b
) {
  1 / (
    1 +
      10 ^ (
        (rating_b - rating_a) /
          400
      )
  )
}

opponent_weight <- function(
    games_played,
    cap = 50L,
    floor = 0.05
) {
  games_played <- pmin(
    as.numeric(games_played),
    cap
  )
  
  floor +
    (1 - floor) *
    (games_played / cap)
}

update_elo <- function(
    rating,
    opponent_rating,
    score,
    k_value
) {
  expected <- expected_score(
    rating,
    opponent_rating
  )
  
  rating +
    k_value *
    (score - expected)
}

# -----------------------------
# Run one live Elo pass
# -----------------------------
run_live_elo <- function(
    games_df,
    retro_start_map = NULL,
    pass_label = "Live pass"
) {
  n <- nrow(games_df)
  
  cat(
    "\n",
    strrep("=", 60),
    "\n",
    sep = ""
  )
  
  cat(
    "Running ",
    pass_label,
    "\n",
    sep = ""
  )
  
  cat(
    "Games to process:",
    n,
    "\n"
  )
  
  # State is keyed by GoRatings ID, not by player name.
  ratings_env <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )
  
  games_env <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )
  
  first_date_env <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )
  
  entry_rating_env <- new.env(
    hash = TRUE,
    parent = emptyenv()
  )
  
  # Preallocate outputs
  out_Date <- as.Date(
    rep(
      NA_character_,
      n
    )
  )
  
  out_BlackID <- character(n)
  out_WhiteID <- character(n)
  
  out_Black <- character(n)
  out_White <- character(n)
  
  out_ResultCode <- character(n)
  out_Event <- character(n)
  out_GameKey <- character(n)
  
  out_BlackFirst <- logical(n)
  out_WhiteFirst <- logical(n)
  
  out_BlackStart <- numeric(n)
  out_WhiteStart <- numeric(n)
  
  out_Gb_Before <- integer(n)
  out_Gw_Before <- integer(n)
  
  out_Kb <- numeric(n)
  out_Kw <- numeric(n)
  
  out_Rb_Before <- numeric(n)
  out_Rw_Before <- numeric(n)
  
  out_ExpectedBlack <- numeric(n)
  out_ExpectedWhite <- numeric(n)
  
  out_Rb_After <- numeric(n)
  out_Rw_After <- numeric(n)
  
  out_Gb_After <- integer(n)
  out_Gw_After <- integer(n)
  
  valid_row <- logical(n)
  
  create_player_state <- function(
    player_id,
    game_date
  ) {
    is_new <- isTRUE(
      new_player_map[[player_id]]
    )
    
    if (!is_new) {
      starting_rating <- as.numeric(
        checkpoint_rating_map[[player_id]]
      )
      
      starting_games <- as.integer(
        checkpoint_games_map[[player_id]]
      )
      
      first_date <- historical_first_date_map[[player_id]]
      
      original_entry <- historical_entry_rating_map[[player_id]]
      
      if (!is.finite(starting_rating)) {
        stop(
          "Missing checkpoint rating for established player ID ",
          player_id
        )
      }
      
      if (is.na(starting_games)) {
        stop(
          "Missing checkpoint game count for established player ID ",
          player_id
        )
      }
      
      if (
        is.na(first_date)
      ) {
        first_date <- game_date
      }
      
      if (
        !is.finite(original_entry)
      ) {
        original_entry <- starting_rating
      }
    } else {
      if (
        !is.null(retro_start_map) &&
        player_id %in%
        names(retro_start_map)
      ) {
        starting_rating <- as.numeric(
          retro_start_map[[player_id]]
        )
      } else {
        starting_rating <- START_R
      }
      
      starting_games <- 0L
      first_date <- game_date
      original_entry <- starting_rating
    }
    
    assign(
      player_id,
      starting_rating,
      envir = ratings_env
    )
    
    assign(
      player_id,
      starting_games,
      envir = games_env
    )
    
    assign(
      player_id,
      first_date,
      envir = first_date_env
    )
    
    assign(
      player_id,
      original_entry,
      envir = entry_rating_env
    )
  }
  
  for (i in seq_len(n)) {
    game <- games_df[i, ]
    
    result_code <- as.character(
      game$ResultCode
    )
    
    if (
      str_starts(
        result_code,
        "B"
      )
    ) {
      black_score <- 1
      white_score <- 0
    } else if (
      str_starts(
        result_code,
        "W"
      )
    ) {
      black_score <- 0
      white_score <- 1
    } else {
      next
    }
    
    black_id <- as.character(
      game$BlackID
    )
    
    white_id <- as.character(
      game$WhiteID
    )
    
    game_date <- game$Date
    
    black_exists <- exists(
      black_id,
      envir = ratings_env,
      inherits = FALSE
    )
    
    white_exists <- exists(
      white_id,
      envir = ratings_env,
      inherits = FALSE
    )
    
    if (!black_exists) {
      create_player_state(
        black_id,
        game_date
      )
    }
    
    if (!white_exists) {
      create_player_state(
        white_id,
        game_date
      )
    }
    
    black_rating <- get(
      black_id,
      envir = ratings_env,
      inherits = FALSE
    )
    
    white_rating <- get(
      white_id,
      envir = ratings_env,
      inherits = FALSE
    )
    
    black_games <- get(
      black_id,
      envir = games_env,
      inherits = FALSE
    )
    
    white_games <- get(
      white_id,
      envir = games_env,
      inherits = FALSE
    )
    
    black_base_k <- if (
      black_games < K_NEW_GAMES
    ) {
      K_NEW
    } else {
      K_NORMAL
    }
    
    white_base_k <- if (
      white_games < K_NEW_GAMES
    ) {
      K_NEW
    } else {
      K_NORMAL
    }
    
    black_k <- black_base_k *
      opponent_weight(
        white_games,
        cap = 50L,
        floor = 0.05
      )
    
    white_k <- white_base_k *
      opponent_weight(
        black_games,
        cap = 50L,
        floor = 0.05
      )
    
    expected_black <- expected_score(
      black_rating,
      white_rating
    )
    
    expected_white <- 1 -
      expected_black
    
    black_rating_after <- update_elo(
      black_rating,
      white_rating,
      black_score,
      black_k
    )
    
    white_rating_after <- update_elo(
      white_rating,
      black_rating,
      white_score,
      white_k
    )
    
    black_games_after <-
      black_games + 1L
    
    white_games_after <-
      white_games + 1L
    
    assign(
      black_id,
      black_rating_after,
      envir = ratings_env
    )
    
    assign(
      white_id,
      white_rating_after,
      envir = ratings_env
    )
    
    assign(
      black_id,
      black_games_after,
      envir = games_env
    )
    
    assign(
      white_id,
      white_games_after,
      envir = games_env
    )
    
    valid_row[i] <- TRUE
    
    out_Date[i] <- game_date
    
    out_BlackID[i] <- black_id
    out_WhiteID[i] <- white_id
    
    out_Black[i] <- as.character(
      game$Black
    )
    
    out_White[i] <- as.character(
      game$White
    )
    
    out_ResultCode[i] <- result_code
    out_Event[i] <- as.character(
      game$Event
    )
    
    out_GameKey[i] <- as.character(
      game$GameKey
    )
    
    # This means first career appearance in the Elo system,
    # rather than merely first appearance after the checkpoint.
    out_BlackFirst[i] <-
      !black_exists &&
      isTRUE(
        new_player_map[[black_id]]
      )
    
    out_WhiteFirst[i] <-
      !white_exists &&
      isTRUE(
        new_player_map[[white_id]]
      )
    
    out_BlackStart[i] <- get(
      black_id,
      envir = entry_rating_env,
      inherits = FALSE
    )
    
    out_WhiteStart[i] <- get(
      white_id,
      envir = entry_rating_env,
      inherits = FALSE
    )
    
    out_Gb_Before[i] <- black_games
    out_Gw_Before[i] <- white_games
    
    out_Kb[i] <- black_k
    out_Kw[i] <- white_k
    
    out_Rb_Before[i] <- black_rating
    out_Rw_Before[i] <- white_rating
    
    out_ExpectedBlack[i] <-
      expected_black
    
    out_ExpectedWhite[i] <-
      expected_white
    
    out_Rb_After[i] <-
      black_rating_after
    
    out_Rw_After[i] <-
      white_rating_after
    
    out_Gb_After[i] <-
      black_games_after
    
    out_Gw_After[i] <-
      white_games_after
  }
  
  history <- tibble(
    Date = out_Date,
    BlackID = out_BlackID,
    WhiteID = out_WhiteID,
    Black = out_Black,
    White = out_White,
    ResultCode = out_ResultCode,
    Event = out_Event,
    GameKey = out_GameKey,
    
    BlackFirstAppearance =
      out_BlackFirst,
    WhiteFirstAppearance =
      out_WhiteFirst,
    
    BlackStartRating =
      out_BlackStart,
    WhiteStartRating =
      out_WhiteStart,
    
    Gb_Before = out_Gb_Before,
    Gw_Before = out_Gw_Before,
    
    Kb = out_Kb,
    Kw = out_Kw,
    
    Rb_Before = out_Rb_Before,
    Rw_Before = out_Rw_Before,
    
    ExpectedBlack =
      out_ExpectedBlack,
    ExpectedWhite =
      out_ExpectedWhite,
    
    Rb_After = out_Rb_After,
    Rw_After = out_Rw_After,
    
    Gb_After = out_Gb_After,
    Gw_After = out_Gw_After
  ) %>%
    filter(valid_row)
  
  player_ids <- ls(
    envir = ratings_env,
    all.names = TRUE
  )
  
  final_ratings <- tibble(
    goratings_id = player_ids,
    name = unname(
      canonical_name_map[player_ids]
    ),
    rating_exact = vapply(
      player_ids,
      function(player_id) {
        as.numeric(
          get(
            player_id,
            envir = ratings_env,
            inherits = FALSE
          )
        )
      },
      numeric(1)
    ),
    games = vapply(
      player_ids,
      function(player_id) {
        as.integer(
          get(
            player_id,
            envir = games_env,
            inherits = FALSE
          )
        )
      },
      integer(1)
    ),
    first_date = as.Date(
      vapply(
        player_ids,
        function(player_id) {
          as.numeric(
            get(
              player_id,
              envir = first_date_env,
              inherits = FALSE
            )
          )
        },
        numeric(1)
      ),
      origin = "1970-01-01"
    ),
    entry_rating = vapply(
      player_ids,
      function(player_id) {
        as.numeric(
          get(
            player_id,
            envir = entry_rating_env,
            inherits = FALSE
          )
        )
      },
      numeric(1)
    )
  ) %>%
    left_join(
      player_state %>%
        select(
          goratings_id,
          goratings_name,
          historical_name,
          match_status,
          is_new_player
        ),
      by = "goratings_id"
    ) %>%
    mutate(
      rating = round(
        rating_exact,
        0
      ),
      entry_rating = round(
        entry_rating,
        1
      ),
      is_seed = games >= 20
    ) %>%
    arrange(
      desc(rating_exact),
      name
    )
  
  list(
    history = history,
    final = final_ratings
  )
}

# -----------------------------
# Build retrospective starts for new players only
# -----------------------------
build_new_player_retro_map <- function(
    pass1_history,
    new_player_ids,
    n_games = RETRO_GAMES_N,
    min_opp_games =
      RETRO_MIN_OPP_GAMES,
    min_valid_opponents =
      RETRO_MIN_VALID_OPPONENTS,
    fallback_center = START_R
) {
  expected_vs <- function(
    rating,
    opponent_rating
  ) {
    1 / (
      1 +
        10 ^ (
          (
            opponent_rating -
              rating
          ) /
            400
        )
    )
  }
  
  solve_performance <- function(
    opponent_ratings,
    scores,
    lower = 0,
    upper = 6000
  ) {
    opponent_ratings <-
      as.numeric(
        opponent_ratings
      )
    
    scores <- as.numeric(scores)
    
    valid <- is.finite(
      opponent_ratings
    ) &
      is.finite(scores)
    
    opponent_ratings <-
      opponent_ratings[valid]
    
    scores <- scores[valid]
    
    game_count <- length(scores)
    
    if (game_count == 0) {
      return(fallback_center)
    }
    
    score_sum <- sum(scores)
    
    if (score_sum <= 0) {
      return(
        max(
          lower,
          min(
            upper,
            min(opponent_ratings) -
              800
          )
        )
      )
    }
    
    if (score_sum >= game_count) {
      return(
        max(
          lower,
          min(
            upper,
            max(opponent_ratings) +
              800
          )
        )
      )
    }
    
    objective <- function(rating) {
      sum(
        expected_vs(
          rating,
          opponent_ratings
        )
      ) -
        score_sum
    }
    
    lower_value <- objective(lower)
    upper_value <- objective(upper)
    
    if (
      !is.finite(lower_value) ||
      !is.finite(upper_value) ||
      lower_value * upper_value > 0
    ) {
      average_opponent <-
        mean(opponent_ratings)
      
      score_rate <-
        score_sum / game_count
      
      score_rate <- min(
        0.999,
        max(
          0.001,
          score_rate
        )
      )
      
      estimated_rating <-
        average_opponent +
        400 *
        log10(
          score_rate /
            (1 - score_rate)
        )
      
      return(
        max(
          lower,
          min(
            upper,
            estimated_rating
          )
        )
      )
    }
    
    uniroot(
      objective,
      lower = lower,
      upper = upper,
      tol = 1e-8
    )$root
  }
  
  black_rows <- pass1_history %>%
    transmute(
      PlayerID = BlackID,
      OppRating = Rw_Before,
      OppGames = Gw_Before,
      Score = if_else(
        str_starts(
          ResultCode,
          "B"
        ),
        1,
        0
      ),
      Date,
      GameKey
    )
  
  white_rows <- pass1_history %>%
    transmute(
      PlayerID = WhiteID,
      OppRating = Rb_Before,
      OppGames = Gb_Before,
      Score = if_else(
        str_starts(
          ResultCode,
          "W"
        ),
        1,
        0
      ),
      Date,
      GameKey
    )
  
  player_games <- bind_rows(
    black_rows,
    white_rows
  ) %>%
    filter(
      PlayerID %in%
        new_player_ids,
      is.finite(OppRating),
      is.finite(Score),
      !is.na(OppGames)
    ) %>%
    arrange(
      PlayerID,
      Date,
      GameKey
    ) %>%
    group_by(PlayerID) %>%
    mutate(
      GameIndex = row_number()
    ) %>%
    ungroup() %>%
    filter(
      GameIndex <= n_games
    )
  
  valid_games <- player_games %>%
    filter(
      OppGames >= min_opp_games
    )
  
  performances <- valid_games %>%
    group_by(PlayerID) %>%
    summarise(
      GamesUsed = n(),
      RetroStart = solve_performance(
        OppRating,
        Score
      ),
      .groups = "drop"
    ) %>%
    filter(
      GamesUsed >=
        min_valid_opponents,
      is.finite(RetroStart)
    )
  
  setNames(
    as.list(
      performances$RetroStart
    ),
    performances$PlayerID
  )
}

# -----------------------------
# Pass 1
#
# Established players use their checkpoint.
# New players begin at 3100.
# -----------------------------
pass1 <- run_live_elo(
  games_df = games,
  retro_start_map = NULL,
  pass_label =
    "Live Pass 1 — flat starts for new players"
)

write_csv(
  pass1$history,
  history_pass1_file
)

write_csv(
  pass1$final %>%
    mutate(
      Pass = "LivePass1",
      EntryMode =
        "CheckpointOrFlatStart",
      StartR = START_R,
      KNormal = K_NORMAL,
      KNew = K_NEW,
      KNewGames = K_NEW_GAMES,
      RetroGamesN =
        RETRO_GAMES_N
    ),
  ratings_pass1_file
)

# -----------------------------
# Build retrospective start map
# -----------------------------
new_player_ids <- player_state %>%
  filter(is_new_player) %>%
  pull(goratings_id)

retro_start_map <-
  build_new_player_retro_map(
    pass1_history = pass1$history,
    new_player_ids =
      new_player_ids
  )

cat(
  "\nBuilt retrospective starts for",
  length(retro_start_map),
  "new players.\n"
)

# -----------------------------
# Pass 2 — final live result
# -----------------------------
pass2 <- run_live_elo(
  games_df = games,
  retro_start_map =
    retro_start_map,
  pass_label =
    "Live Pass 2 — retrospective starts for eligible new players"
)

write_csv(
  pass2$history,
  history_output_file
)

write_csv(
  pass2$final %>%
    mutate(
      Pass = "LivePass2",
      EntryMode =
        "CheckpointOrRetroStart",
      StartR = START_R,
      KNormal = K_NORMAL,
      KNew = K_NEW,
      KNewGames = K_NEW_GAMES,
      RetroGamesN =
        RETRO_GAMES_N
    ),
  ratings_output_file
)

# -----------------------------
# Final checks
# -----------------------------
if (
  nrow(pass2$history) !=
  nrow(games)
) {
  stop(
    "Final live history does not contain exactly one row per input game."
  )
}

if (
  anyDuplicated(
    pass2$history$GameKey
  ) > 0
) {
  stop(
    "Final live history contains duplicate GameKey values."
  )
}

cat("\nDone.\n")

cat(
  "Final live history rows:",
  nrow(pass2$history),
  "\n"
)

cat(
  "Final live players:",
  nrow(pass2$final),
  "\n"
)

cat(
  "Latest live game:",
  format(
    max(pass2$history$Date),
    "%Y-%m-%d"
  ),
  "\n"
)

cat(
  "Pass 1 history:",
  history_pass1_file,
  "\n"
)

cat(
  "Pass 1 ratings:",
  ratings_pass1_file,
  "\n"
)

cat(
  "Final history:",
  history_output_file,
  "\n"
)

cat(
  "Final ratings:",
  ratings_output_file,
  "\n"
)