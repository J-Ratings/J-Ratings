# ============================================================
# Build one combined Go ratings table
#
# Includes:
#   - every historical player from the 2025 checkpoint
#   - updated ratings for historical players active in 2026
#   - new GoRatings players first seen from 2026 onwards
#
# This script does not overwrite the historical Elo files.
# ============================================================

library(dplyr)
library(readr)
library(tibble)

options(stringsAsFactors = FALSE)

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

live_ratings_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "live_Elo",
  "final_ratings_2026.csv"
)

live_history_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "live_Elo",
  "game_history_2026.csv"
)

output_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "live_Elo",
  "combined_final_ratings.csv"
)

# -----------------------------
# Input checks
# -----------------------------
required_files <- c(
  checkpoint_file,
  historical_final_file,
  live_ratings_file,
  live_history_file
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
cat("Reading rating inputs...\n")

checkpoint <- read_csv(
  checkpoint_file,
  show_col_types = FALSE
) %>%
  mutate(
    name = as.character(name),
    rating = as.numeric(rating),
    games = as.integer(games),
    last_historical_game = as.Date(
      last_historical_game
    )
  )

historical_final <- read_csv(
  historical_final_file,
  show_col_types = FALSE
) %>%
  mutate(
    name = as.character(name),
    first_date = as.Date(first_date),
    entry_rating = as.numeric(entry_rating)
  )

live_ratings <- read_csv(
  live_ratings_file,
  show_col_types = FALSE
) %>%
  mutate(
    goratings_id = as.character(
      goratings_id
    ),
    historical_name = as.character(
      historical_name
    ),
    rating_exact = as.numeric(
      rating_exact
    ),
    rating = as.numeric(rating),
    games = as.integer(games),
    first_date = as.Date(first_date),
    entry_rating = as.numeric(
      entry_rating
    ),
    is_new_player = as.logical(
      is_new_player
    )
  )

live_history <- read_csv(
  live_history_file,
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date),
    BlackID = as.character(BlackID),
    WhiteID = as.character(WhiteID)
  )

# -----------------------------
# Validate input columns
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
  live_ratings,
  c(
    "goratings_id",
    "name",
    "historical_name",
    "match_status",
    "is_new_player",
    "rating_exact",
    "rating",
    "games",
    "first_date",
    "entry_rating"
  ),
  "Live ratings"
)

check_columns(
  live_history,
  c(
    "Date",
    "BlackID",
    "WhiteID"
  ),
  "Live game history"
)

# -----------------------------
# Validate source uniqueness
# -----------------------------
if (anyDuplicated(checkpoint$name) > 0) {
  stop(
    "Checkpoint contains duplicate player names."
  )
}

if (anyDuplicated(live_ratings$goratings_id) > 0) {
  stop(
    "Live ratings contain duplicate GoRatings IDs."
  )
}

matched_live <- live_ratings %>%
  filter(!is_new_player)

if (
  any(is.na(matched_live$historical_name)) ||
  any(matched_live$historical_name == "")
) {
  stop(
    "Some matched live players have no historical name."
  )
}

if (
  anyDuplicated(
    matched_live$historical_name
  ) > 0
) {
  stop(
    "More than one live player maps to the same historical player."
  )
}

missing_checkpoint_matches <- setdiff(
  matched_live$historical_name,
  checkpoint$name
)

if (length(missing_checkpoint_matches) > 0) {
  stop(
    "Matched historical players absent from checkpoint: ",
    paste(
      missing_checkpoint_matches,
      collapse = ", "
    )
  )
}

# -----------------------------
# Find each live player's latest game
# -----------------------------
black_last_games <- live_history %>%
  transmute(
    goratings_id = BlackID,
    live_last_game = Date
  )

white_last_games <- live_history %>%
  transmute(
    goratings_id = WhiteID,
    live_last_game = Date
  )

live_last_games <- bind_rows(
  black_last_games,
  white_last_games
) %>%
  group_by(goratings_id) %>%
  summarise(
    live_last_game = max(
      live_last_game,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

live_ratings <- live_ratings %>%
  left_join(
    live_last_games,
    by = "goratings_id"
  )

# -----------------------------
# Historical metadata
# -----------------------------
historical_metadata <- historical_final %>%
  select(
    name,
    first_date,
    entry_rating
  ) %>%
  distinct(
    name,
    .keep_all = TRUE
  )

historical_base <- checkpoint %>%
  left_join(
    historical_metadata,
    by = "name"
  ) %>%
  transmute(
    name,
    rating_exact = rating,
    rating = round(rating),
    games,
    first_date,
    last_game = last_historical_game,
    entry_rating,
    is_seed = games >= 20,
    player_status = "historical_inactive_2026",
    goratings_id = NA_character_,
    goratings_name = NA_character_,
    historical_name = name,
    match_status = "historical_checkpoint"
  )

# -----------------------------
# Prepare live updates for matched historical players
# -----------------------------
historical_updates <- live_ratings %>%
  filter(!is_new_player) %>%
  transmute(
    historical_name,
    updated_rating_exact = rating_exact,
    updated_rating = rating,
    updated_games = games,
    updated_last_game = live_last_game,
    updated_goratings_id = goratings_id,
    updated_goratings_name = goratings_name,
    updated_match_status = match_status
  )

combined_historical <- historical_base %>%
  left_join(
    historical_updates,
    by = c(
      "name" = "historical_name"
    )
  ) %>%
  mutate(
    was_active_2026 = !is.na(
      updated_rating_exact
    ),
    
    rating_exact = if_else(
      was_active_2026,
      updated_rating_exact,
      rating_exact
    ),
    
    rating = if_else(
      was_active_2026,
      updated_rating,
      rating
    ),
    
    games = if_else(
      was_active_2026,
      updated_games,
      games
    ),
    
    last_game = if_else(
      was_active_2026,
      updated_last_game,
      last_game
    ),
    
    is_seed = games >= 20,
    
    player_status = if_else(
      was_active_2026,
      "historical_active_2026",
      "historical_inactive_2026"
    ),
    
    goratings_id = if_else(
      was_active_2026,
      updated_goratings_id,
      NA_character_
    ),
    
    goratings_name = if_else(
      was_active_2026,
      updated_goratings_name,
      NA_character_
    ),
    
    historical_name = name,
    
    match_status = if_else(
      was_active_2026,
      updated_match_status,
      "historical_checkpoint"
    )
  ) %>%
  select(
    name,
    rating_exact,
    rating,
    games,
    first_date,
    last_game,
    entry_rating,
    is_seed,
    player_status,
    goratings_id,
    goratings_name,
    historical_name,
    match_status
  )

# -----------------------------
# Add genuinely new GoRatings players
# -----------------------------
new_players <- live_ratings %>%
  filter(is_new_player) %>%
  transmute(
    name,
    rating_exact,
    rating,
    games,
    first_date,
    last_game = live_last_game,
    entry_rating,
    is_seed = games >= 20,
    player_status = "new_from_2026",
    goratings_id,
    goratings_name,
    historical_name = NA_character_,
    match_status
  )

# -----------------------------
# Combine all players
# -----------------------------
combined_ratings <- bind_rows(
  combined_historical,
  new_players
) %>%
  arrange(
    desc(rating_exact),
    name
  ) %>%
  mutate(
    rank_all = row_number(),
    rank_seeded = if_else(
      is_seed,
      cumsum(is_seed),
      NA_integer_
    )
  ) %>%
  select(
    rank_all,
    rank_seeded,
    name,
    rating,
    rating_exact,
    games,
    first_date,
    last_game,
    entry_rating,
    is_seed,
    player_status,
    goratings_id,
    goratings_name,
    historical_name,
    match_status
  )

# -----------------------------
# Final validation
# -----------------------------
expected_total_players <-
  nrow(checkpoint) +
  nrow(new_players)

if (
  nrow(combined_ratings) !=
  expected_total_players
) {
  stop(
    "Combined player count is incorrect. Expected ",
    expected_total_players,
    " but produced ",
    nrow(combined_ratings),
    "."
  )
}

if (
  anyDuplicated(
    combined_ratings$name
  ) > 0
) {
  duplicate_names <- combined_ratings %>%
    count(
      name,
      name = "n"
    ) %>%
    filter(n > 1)
  
  print(duplicate_names)
  
  stop(
    "Combined ratings contain duplicate display names."
  )
}

if (
  any(!is.finite(
    combined_ratings$rating_exact
  ))
) {
  stop(
    "Combined ratings contain missing or invalid ratings."
  )
}

if (
  any(is.na(
    combined_ratings$games
  ))
) {
  stop(
    "Combined ratings contain missing game counts."
  )
}

# -----------------------------
# Write output
# -----------------------------
write_csv(
  combined_ratings,
  output_file
)

cat("\nDone.\n")

cat(
  "Historical checkpoint players:",
  nrow(checkpoint),
  "\n"
)

cat(
  "Historical players updated in 2026:",
  nrow(historical_updates),
  "\n"
)

cat(
  "Historical players unchanged:",
  nrow(checkpoint) -
    nrow(historical_updates),
  "\n"
)

cat(
  "New players added:",
  nrow(new_players),
  "\n"
)

cat(
  "Combined players:",
  nrow(combined_ratings),
  "\n"
)

cat(
  "Seeded players:",
  sum(combined_ratings$is_seed),
  "\n"
)

cat(
  "Latest game:",
  format(
    max(
      combined_ratings$last_game,
      na.rm = TRUE
    ),
    "%Y-%m-%d"
  ),
  "\n"
)

cat(
  "Wrote:",
  output_file,
  "\n"
)