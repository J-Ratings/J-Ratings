# ============================================================
# Build combined Go game history
#
# Combines:
#   - historical GoGoD Elo game history through 2025-12-31
#   - live GoRatings Elo game history from 2026-01-01
#
# The historical source file is not overwritten.
#
# Output:
#   Go/pipeline_data/live_Elo/combined_game_history.csv
# ============================================================

library(dplyr)
library(readr)
library(tibble)

options(stringsAsFactors = FALSE)

# -----------------------------
# Settings
# -----------------------------
HISTORICAL_END_DATE <- as.Date("2025-12-31")
LIVE_START_DATE <- as.Date("2026-01-01")

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

historical_history_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "Elo",
  "game_history.csv"
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
  "combined_game_history.csv"
)

# -----------------------------
# Input checks
# -----------------------------
required_files <- c(
  historical_history_file,
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
cat("Reading historical game history...\n")

historical_history <- read_csv(
  historical_history_file,
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date)
  )

cat("Reading live game history...\n")

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
# Validate columns
# -----------------------------
shared_history_columns <- c(
  "Date",
  "Black",
  "White",
  "ResultCode",
  "Event",
  "GameKey",
  "BlackFirstAppearance",
  "WhiteFirstAppearance",
  "BlackStartRating",
  "WhiteStartRating",
  "Gb_Before",
  "Gw_Before",
  "Kb",
  "Kw",
  "Rb_Before",
  "Rw_Before",
  "ExpectedBlack",
  "ExpectedWhite",
  "Rb_After",
  "Rw_After",
  "Gb_After",
  "Gw_After"
)

check_columns(
  historical_history,
  shared_history_columns,
  "Historical game history"
)

check_columns(
  live_history,
  c(
    shared_history_columns,
    "BlackID",
    "WhiteID"
  ),
  "Live game history"
)

# -----------------------------
# Restrict each source to its intended period
# -----------------------------
historical_history <- historical_history %>%
  filter(
    !is.na(Date),
    Date <= HISTORICAL_END_DATE
  )

live_history <- live_history %>%
  filter(
    !is.na(Date),
    Date >= LIVE_START_DATE
  )

cat(
  "Historical rows retained:",
  nrow(historical_history),
  "\n"
)

cat(
  "Live rows retained:",
  nrow(live_history),
  "\n"
)

# -----------------------------
# Prepare historical rows
#
# GoRatings IDs do not exist for most historical-only games,
# so ID fields are left blank.
# -----------------------------
historical_output <- historical_history %>%
  transmute(
    Date,
    BlackID = NA_character_,
    WhiteID = NA_character_,
    Black,
    White,
    ResultCode,
    Event,
    GameKey,
    Source = "GoGoD",
    BlackFirstAppearance,
    WhiteFirstAppearance,
    BlackStartRating,
    WhiteStartRating,
    Gb_Before,
    Gw_Before,
    Kb,
    Kw,
    Rb_Before,
    Rw_Before,
    ExpectedBlack,
    ExpectedWhite,
    Rb_After,
    Rw_After,
    Gb_After,
    Gw_After
  )

# -----------------------------
# Prepare live rows
# -----------------------------
live_output <- live_history %>%
  transmute(
    Date,
    BlackID,
    WhiteID,
    Black,
    White,
    ResultCode,
    Event,
    GameKey,
    Source = "GoRatings",
    BlackFirstAppearance,
    WhiteFirstAppearance,
    BlackStartRating,
    WhiteStartRating,
    Gb_Before,
    Gw_Before,
    Kb,
    Kw,
    Rb_Before,
    Rw_Before,
    ExpectedBlack,
    ExpectedWhite,
    Rb_After,
    Rw_After,
    Gb_After,
    Gw_After
  )

# -----------------------------
# Combine
# -----------------------------
combined_history <- bind_rows(
  historical_output,
  live_output
) %>%
  arrange(
    Date,
    Source,
    GameKey
  )

# -----------------------------
# Validation
# -----------------------------
expected_rows <-
  nrow(historical_output) +
  nrow(live_output)

if (
  nrow(combined_history) !=
  expected_rows
) {
  stop(
    "Combined history row count is incorrect."
  )
}

duplicate_game_keys <- combined_history %>%
  count(
    GameKey,
    name = "n"
  ) %>%
  filter(
    n > 1
  )

if (nrow(duplicate_game_keys) > 0) {
  print(duplicate_game_keys)
  
  stop(
    "Combined history contains duplicate GameKey values."
  )
}

if (
  any(
    combined_history$Date <=
    HISTORICAL_END_DATE &
    combined_history$Source != "GoGoD"
  )
) {
  stop(
    "A pre-2026 game has the wrong source."
  )
}

if (
  any(
    combined_history$Date >=
    LIVE_START_DATE &
    combined_history$Source != "GoRatings"
  )
) {
  stop(
    "A 2026 game has the wrong source."
  )
}

# -----------------------------
# Write output
# -----------------------------
write_csv(
  combined_history,
  output_file
)

cat("\nDone.\n")

cat(
  "Historical games:",
  nrow(historical_output),
  "\n"
)

cat(
  "Live games:",
  nrow(live_output),
  "\n"
)

cat(
  "Combined games:",
  nrow(combined_history),
  "\n"
)

cat(
  "Earliest game:",
  format(
    min(combined_history$Date),
    "%Y-%m-%d"
  ),
  "\n"
)

cat(
  "Latest game:",
  format(
    max(combined_history$Date),
    "%Y-%m-%d"
  ),
  "\n"
)

cat(
  "Wrote:",
  output_file,
  "\n"
)


