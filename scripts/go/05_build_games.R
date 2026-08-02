# ============================================================
# Build clean GoRatings games from the full profile scrape
#
# Input:
#   Go/pipeline_data/goratings/goratings_games_all_raw.csv
#
# Outputs:
#   Go/pipeline_data/goratings/goratings_games_2015_2025.csv
#   Go/pipeline_data/processed/goratings_games_2026.csv
#   Go/pipeline_data/processed/goratings_game_issues.csv
#
# The 2015–2025 matching-history file is created only if it
# does not already exist.
#
# This script does not alter the full raw scrape.
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

options(stringsAsFactors = FALSE)

# -----------------------------
# Settings
# -----------------------------
GORATINGS_START_DATE <- as.Date("2026-01-01")

MATCHING_START_DATE <- as.Date("2015-01-01")
MATCHING_END_DATE <- as.Date("2025-12-31")

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

raw_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "goratings",
  "goratings_games_all_raw.csv"
)

matching_history_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "goratings",
  "goratings_games_2015_2025.csv"
)

processed_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed"
)

output_file <- file.path(
  processed_dir,
  "goratings_games_2026.csv"
)

issues_file <- file.path(
  processed_dir,
  "goratings_game_issues.csv"
)

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# -----------------------------
# Input checks
# -----------------------------
if (!file.exists(raw_file)) {
  stop(
    "Missing full GoRatings raw file: ",
    raw_file
  )
}

# -----------------------------
# Read full raw scrape
# -----------------------------
cat("Reading full GoRatings history...\n")

raw <- read_csv(
  raw_file,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
) %>%
  mutate(
    date = as.Date(date)
  )

cat("Full raw rows loaded:", nrow(raw), "\n")

required_columns <- c(
  "player_id",
  "player_name",
  "date",
  "color",
  "result",
  "opponent_id",
  "opponent_name"
)

missing_columns <- setdiff(
  required_columns,
  names(raw)
)

if (length(missing_columns) > 0) {
  stop(
    "Raw GoRatings file is missing columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

# -----------------------------
# Create smaller matching-history file
#
# This is created only once. Delete it manually if you ever
# want it rebuilt from a newer full scrape.
# -----------------------------
if (!file.exists(matching_history_file)) {
  cat("Creating 2015–2025 matching-history file...\n")
  
  matching_history <- raw %>%
    filter(
      !is.na(date),
      date >= MATCHING_START_DATE,
      date <= MATCHING_END_DATE
    )
  
  write_csv(
    matching_history,
    matching_history_file
  )
  
  cat(
    "Created matching-history file:",
    matching_history_file,
    "\n"
  )
  
  cat(
    "Matching-history rows:",
    nrow(matching_history),
    "\n"
  )
} else {
  cat(
    "Matching-history file already exists:",
    matching_history_file,
    "\n"
  )
}

# -----------------------------
# Immediately reduce the data to 2026 onwards
#
# This happens before the slower name-repair work.
# -----------------------------
raw <- raw %>%
  filter(
    !is.na(date),
    date >= GORATINGS_START_DATE
  )

cat(
  "Rows retained from 2026 onwards:",
  nrow(raw),
  "\n"
)

# -----------------------------
# Helpers
# -----------------------------
mojibake_score <- function(x) {
  if (is.na(x) || x == "") {
    return(0L)
  }
  
  markers <- c(
    "Ã",
    "Â",
    "â",
    "ð",
    "ì",
    "ë",
    "ê",
    "å",
    "æ",
    "�"
  )
  
  sum(
    vapply(
      markers,
      function(marker) {
        str_count(
          x,
          fixed(marker)
        )
      },
      integer(1)
    )
  )
}

repair_mojibake_once <- function(x) {
  if (is.na(x) || x == "") {
    return(x)
  }
  
  repaired_raw <- tryCatch(
    iconv(
      x,
      from = "UTF-8",
      to = "latin1",
      sub = NA,
      toRaw = TRUE
    )[[1]],
    error = function(e) {
      NULL
    }
  )
  
  if (
    is.null(repaired_raw) ||
    length(repaired_raw) == 0
  ) {
    return(x)
  }
  
  repaired <- rawToChar(
    repaired_raw
  )
  
  Encoding(repaired) <- "UTF-8"
  
  if (!validUTF8(repaired)) {
    return(x)
  }
  
  repaired
}

repair_mojibake_one <- function(
    x,
    max_passes = 3L
) {
  if (is.na(x) || x == "") {
    return(x)
  }
  
  current <- x
  current_score <- mojibake_score(
    current
  )
  
  if (current_score == 0L) {
    return(current)
  }
  
  for (i in seq_len(max_passes)) {
    candidate <- repair_mojibake_once(
      current
    )
    
    candidate_score <- mojibake_score(
      candidate
    )
    
    if (
      identical(candidate, current) ||
      candidate_score >= current_score
    ) {
      break
    }
    
    current <- candidate
    current_score <- candidate_score
  }
  
  current
}

repair_mojibake <- function(x) {
  vapply(
    as.character(x),
    repair_mojibake_one,
    character(1),
    USE.NAMES = FALSE
  )
}

clean_name <- function(x) {
  x <- as.character(x)
  
  x <- repair_mojibake(x)
  
  x <- str_replace_all(
    x,
    "\u00A0",
    " "
  )
  
  x <- str_replace_all(
    x,
    "\\s+",
    " "
  )
  
  x <- str_trim(x)
  
  x[x == ""] <- NA_character_
  
  x
}

first_non_missing <- function(x) {
  x <- as.character(x)
  
  x <- x[
    !is.na(x) &
      x != ""
  ]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  x[[1]]
}

# -----------------------------
# Clean 2026 profile observations
# -----------------------------
cat("Cleaning 2026 profile observations...\n")

observations <- raw %>%
  transmute(
    profile_player_id = as.character(
      player_id
    ),
    profile_player_name = clean_name(
      player_name
    ),
    date = as.Date(date),
    color = str_to_title(
      str_trim(
        as.character(color)
      )
    ),
    result = str_to_title(
      str_trim(
        as.character(result)
      )
    ),
    opponent_id = as.character(
      opponent_id
    ),
    opponent_name = clean_name(
      opponent_name
    ),
    profile_url = if (
      "profile_url" %in% names(raw)
    ) {
      as.character(profile_url)
    } else {
      NA_character_
    }
  ) %>%
  filter(
    !is.na(date),
    date >= GORATINGS_START_DATE,
    !is.na(profile_player_id),
    profile_player_id != "",
    !is.na(opponent_id),
    opponent_id != ""
  )

cat(
  "Raw 2026 profile-game rows loaded:",
  nrow(observations),
  "\n"
)

# -----------------------------
# Identify malformed observations
# -----------------------------
invalid_observations <- observations %>%
  filter(
    !(color %in% c("Black", "White")) |
      !(result %in% c("Win", "Loss"))
  ) %>%
  mutate(
    issue = "Unsupported colour or result"
  )

valid_observations <- observations %>%
  filter(
    color %in% c("Black", "White"),
    result %in% c("Win", "Loss")
  )

# -----------------------------
# Convert each profile observation into a canonical
# Black-versus-White representation
# -----------------------------
canonical_observations <- valid_observations %>%
  mutate(
    black_id = if_else(
      color == "Black",
      profile_player_id,
      opponent_id
    ),
    black_name = if_else(
      color == "Black",
      profile_player_name,
      opponent_name
    ),
    white_id = if_else(
      color == "White",
      profile_player_id,
      opponent_id
    ),
    white_name = if_else(
      color == "White",
      profile_player_name,
      opponent_name
    ),
    winner_id = case_when(
      result == "Win" ~ profile_player_id,
      result == "Loss" ~ opponent_id,
      TRUE ~ NA_character_
    ),
    winner_name = case_when(
      result == "Win" ~ profile_player_name,
      result == "Loss" ~ opponent_name,
      TRUE ~ NA_character_
    ),
    reporting_side = if_else(
      profile_player_id == black_id,
      "Black",
      "White"
    )
  ) %>%
  filter(
    !is.na(black_id),
    !is.na(white_id),
    !is.na(winner_id),
    black_id != white_id,
    winner_id %in% c(
      black_id,
      white_id
    )
  )

# -----------------------------
# Count matching observations
#
# A game will normally appear on both profiles.
#
# More than one genuine game can occur between the same
# players, with the same colours and winner, on the same date.
#
# Therefore, the number of games represented by a signature
# is the larger of:
#   observations from Black's profile
#   observations from White's profile
# -----------------------------
signature_counts <- canonical_observations %>%
  count(
    date,
    black_id,
    white_id,
    winner_id,
    reporting_side,
    name = "observations"
  ) %>%
  pivot_wider(
    names_from = reporting_side,
    values_from = observations,
    values_fill = 0,
    names_prefix = "reported_by_"
  )

if (
  !("reported_by_Black" %in%
    names(signature_counts))
) {
  signature_counts$reported_by_Black <- 0L
}

if (
  !("reported_by_White" %in%
    names(signature_counts))
) {
  signature_counts$reported_by_White <- 0L
}

signature_counts <- signature_counts %>%
  mutate(
    game_count = pmax(
      reported_by_Black,
      reported_by_White
    ),
    profile_count_difference = abs(
      reported_by_Black -
        reported_by_White
    )
  )

# -----------------------------
# Choose one display name for each player in each signature
# -----------------------------
signature_names <- canonical_observations %>%
  group_by(
    date,
    black_id,
    white_id,
    winner_id
  ) %>%
  summarise(
    black_name = first_non_missing(
      black_name
    ),
    white_name = first_non_missing(
      white_name
    ),
    winner_name = first_non_missing(
      winner_name
    ),
    .groups = "drop"
  )

signatures <- signature_counts %>%
  left_join(
    signature_names,
    by = c(
      "date",
      "black_id",
      "white_id",
      "winner_id"
    )
  )

# -----------------------------
# Expand signatures to one row per actual game
# -----------------------------
games <- signatures %>%
  filter(
    game_count > 0
  ) %>%
  uncount(
    weights = game_count,
    .id = "same_signature_sequence"
  ) %>%
  mutate(
    ResultCode = if_else(
      winner_id == black_id,
      "B+",
      "W+"
    ),
    Event = "Professional Go",
    source = "GoRatings",
    game_key = paste(
      format(
        date,
        "%Y-%m-%d"
      ),
      black_id,
      white_id,
      winner_id,
      same_signature_sequence,
      sep = "|"
    )
  ) %>%
  transmute(
    Date = format(
      date,
      "%Y-%m-%d"
    ),
    BlackID = black_id,
    Black = black_name,
    WhiteID = white_id,
    White = white_name,
    WinnerID = winner_id,
    Winner = if_else(
      winner_id == black_id,
      black_name,
      white_name
    ),
    ResultCode,
    Event,
    Source = source,
    SameSignatureSequence =
      same_signature_sequence,
    ReportedByBlackProfile =
      reported_by_Black,
    ReportedByWhiteProfile =
      reported_by_White,
    GameKey = game_key
  ) %>%
  arrange(
    Date,
    BlackID,
    WhiteID,
    ResultCode,
    SameSignatureSequence
  )

# -----------------------------
# Build observation report
# -----------------------------
profile_count_issues <- signatures %>%
  filter(
    profile_count_difference > 0
  ) %>%
  transmute(
    issue =
      "Profile observation counts differ",
    date = format(
      date,
      "%Y-%m-%d"
    ),
    black_id,
    black_name,
    white_id,
    white_name,
    winner_id,
    reported_by_black =
      reported_by_Black,
    reported_by_white =
      reported_by_White,
    details = paste0(
      "Black profile rows: ",
      reported_by_Black,
      "; White profile rows: ",
      reported_by_White
    )
  )

invalid_issues <- invalid_observations %>%
  transmute(
    issue,
    date = format(
      date,
      "%Y-%m-%d"
    ),
    black_id = NA_character_,
    black_name = NA_character_,
    white_id = NA_character_,
    white_name = NA_character_,
    winner_id = NA_character_,
    reported_by_black = NA_integer_,
    reported_by_white = NA_integer_,
    details = paste0(
      "Player ",
      profile_player_id,
      "; colour ",
      color,
      "; result ",
      result,
      "; opponent ",
      opponent_id
    )
  )

issues <- bind_rows(
  profile_count_issues,
  invalid_issues
)

# -----------------------------
# Final validation
# -----------------------------
if (nrow(games) == 0) {
  stop(
    "No clean GoRatings games were produced."
  )
}

if (
  any(is.na(games$BlackID)) ||
  any(is.na(games$WhiteID))
) {
  stop(
    "Some games have missing player IDs."
  )
}

if (
  any(
    games$BlackID ==
    games$WhiteID
  )
) {
  stop(
    "Some games have the same player as Black and White."
  )
}

if (
  any(
    !(games$ResultCode %in%
      c("B+", "W+"))
  )
) {
  stop(
    "Some games have an invalid ResultCode."
  )
}

duplicate_keys <- games %>%
  count(
    GameKey,
    name = "n"
  ) %>%
  filter(
    n > 1
  )

if (nrow(duplicate_keys) > 0) {
  print(duplicate_keys)
  
  stop(
    "Duplicate GameKey values were produced."
  )
}

# -----------------------------
# Write processed outputs
# -----------------------------
write_csv(
  games,
  output_file
)

write_csv(
  issues,
  issues_file
)

cat("\nDone.\n")
cat(
  "Clean games produced:",
  nrow(games),
  "\n"
)
cat(
  "Earliest game:",
  min(games$Date),
  "\n"
)
cat(
  "Latest game:",
  max(games$Date),
  "\n"
)
cat(
  "Observation-report rows:",
  nrow(issues),
  "\n"
)
cat(
  "Wrote:",
  output_file,
  "\n"
)
cat(
  "Wrote:",
  issues_file,
  "\n"
)


