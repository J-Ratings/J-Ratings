# ============================================================
# Build canonical GoRatings games
#
# Input:
#   Go/pipeline_data/goratings/
#     goratings_games_2026_observations.csv
#
# Outputs:
#   Go/pipeline_data/processed/
#     goratings_games_2026.csv
#     goratings_games_2026_issues.csv
#
# Also preserves the existing matching-history file:
#   Go/pipeline_data/goratings/
#     goratings_games_2015_2025.csv
#
# Each GoRatings game normally appears on both players'
# profile pages. This script merges those two observations
# into one game while preserving repeated games between the
# same two players on the same date.
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(purrr)

options(stringsAsFactors = FALSE)

# -----------------------------
# Settings
# -----------------------------
LIVE_START_DATE <- as.Date("2026-01-01")

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

goratings_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "goratings"
)

processed_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed"
)

dir.create(
  goratings_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

observations_file <- file.path(
  goratings_dir,
  "goratings_games_2026_observations.csv"
)

clean_games_file <- file.path(
  processed_dir,
  "goratings_games_2026.csv"
)

issues_file <- file.path(
  processed_dir,
  "goratings_games_2026_issues.csv"
)

# -----------------------------
# Helpers
# -----------------------------
clean_text <- function(x) {
  x <- as.character(x)
  
  x <- str_replace_all(
    x,
    "\u00a0",
    " "
  )
  
  x <- str_squish(x)
  
  x[x == ""] <- NA_character_
  
  x
}

clean_id <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  x
}

normalise_colour <- function(x) {
  x <- str_to_lower(
    clean_text(x)
  )
  
  case_when(
    x %in%
      c(
        "black",
        "b"
      ) ~
      "Black",
    
    x %in%
      c(
        "white",
        "w"
      ) ~
      "White",
    
    TRUE ~
      NA_character_
  )
}

normalise_result <- function(x) {
  x <- str_to_lower(
    clean_text(x)
  )
  
  case_when(
    x %in%
      c(
        "win",
        "won",
        "w"
      ) ~
      "Win",
    
    x %in%
      c(
        "loss",
        "lose",
        "lost",
        "l"
      ) ~
      "Loss",
    
    TRUE ~
      NA_character_
  )
}

combine_text_values <- function(x) {
  x <- clean_text(x)
  
  x <- x[
    !is.na(x) &
      x != ""
  ]
  
  if (length(x) == 0L) {
    return(
      NA_character_
    )
  }
  
  paste(
    unique(x),
    collapse = " | "
  )
}

first_non_missing_character <- function(x) {
  x <- clean_text(x)
  
  x <- x[
    !is.na(x) &
      x != ""
  ]
  
  if (length(x) == 0L) {
    return(
      NA_character_
    )
  }
  
  x[[1]]
}

first_non_missing_integer <- function(x) {
  x <- suppressWarnings(
    as.integer(x)
  )
  
  x <- x[
    !is.na(x)
  ]
  
  if (length(x) == 0L) {
    return(
      NA_integer_
    )
  }
  
  x[[1]]
}

make_base_key <- function(
    date,
    black_id,
    white_id,
    result_code
) {
  paste(
    format(
      as.Date(date),
      "%Y-%m-%d"
    ),
    black_id,
    white_id,
    result_code,
    sep = "|"
  )
}

# -----------------------------
# Input check
# -----------------------------
if (!file.exists(observations_file)) {
  stop(
    "Missing persistent GoRatings observation file: ",
    observations_file,
    "\nRun scripts/go/04_scrape_goratings.R first."
  )
}

observations_raw <- read_csv(
  observations_file,
  show_col_types = FALSE,
  locale = locale(
    encoding = "UTF-8"
  ),
  col_types = cols(
    player_id = col_character(),
    opponent_id = col_character(),
    .default = col_guess()
  )
)

required_columns <- c(
  "player_id",
  "player_name",
  "date",
  "player_rating",
  "colour",
  "result",
  "opponent_id",
  "opponent_name",
  "opponent_rating",
  "kifu_urls",
  "profile_url"
)

missing_columns <- setdiff(
  required_columns,
  names(observations_raw)
)

if (length(missing_columns) > 0L) {
  stop(
    "Observation file is missing columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

cat(
  "Raw 2026 observations:",
  nrow(observations_raw),
  "\n"
)

# -----------------------------
# Clean observations
# -----------------------------
observations <- observations_raw %>%
  transmute(
    player_id =
      clean_id(player_id),
    player_name =
      clean_text(player_name),
    date =
      as.Date(date),
    player_rating =
      suppressWarnings(
        as.integer(player_rating)
      ),
    colour =
      normalise_colour(colour),
    result =
      normalise_result(result),
    opponent_id =
      clean_id(opponent_id),
    opponent_name =
      clean_text(opponent_name),
    opponent_rating =
      suppressWarnings(
        as.integer(opponent_rating)
      ),
    opponent_sex = if (
      "opponent_sex" %in%
      names(observations_raw)
    ) {
      clean_text(opponent_sex)
    } else {
      NA_character_
    },
    kifu_urls =
      clean_text(kifu_urls),
    profile_url =
      clean_text(profile_url),
    scraped_at = if (
      "scraped_at" %in%
      names(observations_raw)
    ) {
      clean_text(scraped_at)
    } else {
      NA_character_
    }
  ) %>%
  filter(
    !is.na(player_id),
    !is.na(opponent_id),
    player_id != opponent_id,
    !is.na(date),
    date >= LIVE_START_DATE
  )

# -----------------------------
# Convert every profile row into
# Black/White orientation
# -----------------------------
oriented <- observations %>%
  mutate(
    black_id = case_when(
      colour == "Black" ~
        player_id,
      colour == "White" ~
        opponent_id,
      TRUE ~
        NA_character_
    ),
    
    white_id = case_when(
      colour == "Black" ~
        opponent_id,
      colour == "White" ~
        player_id,
      TRUE ~
        NA_character_
    ),
    
    black_name = case_when(
      colour == "Black" ~
        player_name,
      colour == "White" ~
        opponent_name,
      TRUE ~
        NA_character_
    ),
    
    white_name = case_when(
      colour == "Black" ~
        opponent_name,
      colour == "White" ~
        player_name,
      TRUE ~
        NA_character_
    ),
    
    black_rating = case_when(
      colour == "Black" ~
        player_rating,
      colour == "White" ~
        opponent_rating,
      TRUE ~
        NA_integer_
    ),
    
    white_rating = case_when(
      colour == "Black" ~
        opponent_rating,
      colour == "White" ~
        player_rating,
      TRUE ~
        NA_integer_
    ),
    
    result_code = case_when(
      colour == "Black" &
        result == "Win" ~
        "B",
      
      colour == "Black" &
        result == "Loss" ~
        "W",
      
      colour == "White" &
        result == "Win" ~
        "W",
      
      colour == "White" &
        result == "Loss" ~
        "B",
      
      TRUE ~
        NA_character_
    ),
    
    perspective_player_id =
      player_id
  )

# -----------------------------
# Invalid observations
# -----------------------------
invalid_observations <- oriented %>%
  filter(
    is.na(colour) |
      is.na(result) |
      is.na(black_id) |
      is.na(white_id) |
      is.na(result_code)
  ) %>%
  mutate(
    issue_type =
      "invalid observation"
  )

valid_observations <- oriented %>%
  filter(
    !is.na(colour),
    !is.na(result),
    !is.na(black_id),
    !is.na(white_id),
    !is.na(result_code)
  )

cat(
  "Valid oriented observations:",
  nrow(valid_observations),
  "\n"
)

cat(
  "Invalid observations:",
  nrow(invalid_observations),
  "\n"
)

# -----------------------------
# Remove exact duplicate rows
#
# A profile should only contribute one copy of
# the same displayed row. Kifu URLs are included
# where available because they can distinguish
# otherwise identical repeated games.
# -----------------------------
valid_observations <- valid_observations %>%
  arrange(
    date,
    black_id,
    white_id,
    result_code,
    perspective_player_id,
    kifu_urls,
    scraped_at
  ) %>%
  distinct(
    date,
    black_id,
    white_id,
    result_code,
    perspective_player_id,
    player_rating,
    opponent_rating,
    kifu_urls,
    .keep_all = TRUE
  )

# -----------------------------
# Assign occurrence numbers
#
# This preserves two games where the same players
# play more than once on the same date with the
# same result.
#
# Each player's profile receives its own sequence.
# Corresponding first, second, third observations
# are then combined.
# -----------------------------
numbered <- valid_observations %>%
  mutate(
    base_key = make_base_key(
      date,
      black_id,
      white_id,
      result_code
    ),
    
    kifu_sort =
      coalesce(
        kifu_urls,
        ""
      )
  ) %>%
  arrange(
    base_key,
    perspective_player_id,
    kifu_sort,
    player_rating,
    opponent_rating,
    profile_url
  ) %>%
  group_by(
    base_key,
    perspective_player_id
  ) %>%
  mutate(
    occurrence =
      row_number()
  ) %>%
  ungroup()

# -----------------------------
# Combine reciprocal observations
# into one canonical game
# -----------------------------
canonical_games <- numbered %>%
  group_by(
    base_key,
    occurrence,
    date,
    black_id,
    white_id,
    result_code
  ) %>%
  summarise(
    black_name =
      first_non_missing_character(
        black_name
      ),
    
    white_name =
      first_non_missing_character(
        white_name
      ),
    
    black_rating =
      first_non_missing_integer(
        black_rating
      ),
    
    white_rating =
      first_non_missing_integer(
        white_rating
      ),
    
    kifu_urls =
      combine_text_values(
        kifu_urls
      ),
    
    profile_urls =
      combine_text_values(
        profile_url
      ),
    
    observation_count =
      n(),
    
    perspective_count =
      n_distinct(
        perspective_player_id
      ),
    
    .groups = "drop"
  ) %>%
  mutate(
    GameKey = paste0(
      base_key,
      "|",
      occurrence
    ),
    
    Event = "GoRatings",
    
    source =
      "GoRatings",
    
    result = case_when(
      result_code == "B" ~
        "Black",
      result_code == "W" ~
        "White",
      TRUE ~
        NA_character_
    )
  ) %>%
  arrange(
    date,
    GameKey
  )

# -----------------------------
# Flag potentially conflicting rows
# -----------------------------
conflicting_groups <- numbered %>%
  group_by(
    base_key,
    occurrence
  ) %>%
  summarise(
    black_names =
      n_distinct(
        black_name[
          !is.na(black_name)
        ]
      ),
    
    white_names =
      n_distinct(
        white_name[
          !is.na(white_name)
        ]
      ),
    
    black_ratings =
      n_distinct(
        black_rating[
          !is.na(black_rating)
        ]
      ),
    
    white_ratings =
      n_distinct(
        white_rating[
          !is.na(white_rating)
        ]
      ),
    
    perspective_count =
      n_distinct(
        perspective_player_id
      ),
    
    .groups = "drop"
  ) %>%
  filter(
    black_names > 1L |
      white_names > 1L |
      black_ratings > 1L |
      white_ratings > 1L
  ) %>%
  mutate(
    issue_type =
      "conflicting reciprocal observations"
  )

single_perspective <- canonical_games %>%
  filter(
    perspective_count < 2L
  ) %>%
  transmute(
    base_key,
    occurrence,
    issue_type =
      "only one player profile observed"
  )

issues <- bind_rows(
  invalid_observations %>%
    transmute(
      base_key = NA_character_,
      occurrence = NA_integer_,
      issue_type,
      date,
      player_id,
      player_name,
      opponent_id,
      opponent_name,
      colour,
      result,
      profile_url
    ),
  
  conflicting_groups %>%
    transmute(
      base_key,
      occurrence,
      issue_type,
      date =
        as.Date(NA),
      player_id =
        NA_character_,
      player_name =
        NA_character_,
      opponent_id =
        NA_character_,
      opponent_name =
        NA_character_,
      colour =
        NA_character_,
      result =
        NA_character_,
      profile_url =
        NA_character_
    ),
  
  single_perspective %>%
    transmute(
      base_key,
      occurrence,
      issue_type,
      date =
        as.Date(NA),
      player_id =
        NA_character_,
      player_name =
        NA_character_,
      opponent_id =
        NA_character_,
      opponent_name =
        NA_character_,
      colour =
        NA_character_,
      result =
        NA_character_,
      profile_url =
        NA_character_
    )
)

# -----------------------------
# Final downstream schema
#
# Both lowercase and legacy title-case
# fields are retained so scripts 06 and 07
# can continue using their existing names.
# -----------------------------
games_out <- canonical_games %>%
  transmute(
    date =
      as.Date(date),
    
    Date =
      as.Date(date),
    
    black_id =
      as.character(black_id),
    
    white_id =
      as.character(white_id),
    
    WinnerID = case_when(
      result_code == "B" ~
        as.character(black_id),
      
      result_code == "W" ~
        as.character(white_id),
      
      TRUE ~
        NA_character_
    ),
    
    black_goratings_id =
      as.character(black_id),
    
    white_goratings_id =
      as.character(white_id),
    
    BlackID =
      as.character(black_id),
    
    WhiteID =
      as.character(white_id),
    
    black_name =
      as.character(black_name),
    
    white_name =
      as.character(white_name),
    
    Black =
      as.character(black_name),
    
    White =
      as.character(white_name),
    
    result_code =
      as.character(result_code),
    
    ResultCode =
      as.character(result_code),
    
    result =
      as.character(result),
    
    black_rating =
      as.integer(black_rating),
    
    white_rating =
      as.integer(white_rating),
    
    BlackRating =
      as.integer(black_rating),
    
    WhiteRating =
      as.integer(white_rating),
    
    Event =
      as.character(Event),
    
    source =
      as.character(source),
    
    tournament =
      as.character(Event),
    
    kifu_urls =
      as.character(kifu_urls),
    
    profile_urls =
      as.character(profile_urls),
    
    observation_count =
      as.integer(observation_count),
    
    perspective_count =
      as.integer(perspective_count),
    
    occurrence =
      as.integer(occurrence),
    
    SameSignatureSequence =
      as.integer(occurrence)
    
  ) %>%
  arrange(
    date,
    GameKey
  )

if (nrow(games_out) == 0L) {
  stop(
    "No valid canonical games were produced."
  )
}

if (anyDuplicated(games_out$GameKey)) {
  stop(
    "Duplicate GameKey values remain after canonicalisation."
  )
}

# -----------------------------
# Write outputs
# -----------------------------
write_csv(
  games_out,
  clean_games_file
)

write_csv(
  issues,
  issues_file
)

# -----------------------------
# Summary
# -----------------------------
cat("\nGoRatings game build complete.\n")
cat(
  "Input observations:",
  nrow(observations_raw),
  "\n"
)
cat(
  "Valid observations:",
  nrow(valid_observations),
  "\n"
)
cat(
  "Canonical games:",
  nrow(games_out),
  "\n"
)
cat(
  "Games with two profile perspectives:",
  sum(
    games_out$perspective_count >= 2L
  ),
  "\n"
)
cat(
  "Games with one profile perspective:",
  sum(
    games_out$perspective_count < 2L
  ),
  "\n"
)
cat(
  "Issue rows:",
  nrow(issues),
  "\n"
)
cat(
  "Date range:",
  format(
    min(
      games_out$date,
      na.rm = TRUE
    ),
    "%Y-%m-%d"
  ),
  "to",
  format(
    max(
      games_out$date,
      na.rm = TRUE
    ),
    "%Y-%m-%d"
  ),
  "\n"
)
cat(
  "Clean game file:",
  clean_games_file,
  "\n"
)
cat(
  "Issue file:",
  issues_file,
  "\n"
)