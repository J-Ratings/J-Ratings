# ============================================================
# Create exact Go Elo checkpoint at 2025-12-31
# ============================================================

library(dplyr)
library(readr)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

history_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "Elo",
  "game_history.csv"
)

checkpoint_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "checkpoint",
  "ratings_2025-12-31.csv"
)

# -----------------------------
# Input checks
# -----------------------------
if (!file.exists(history_file)) {
  stop("Missing game history file: ", history_file)
}

# -----------------------------
# Load historical game states
# -----------------------------
history <- read_csv(
  history_file,
  show_col_types = FALSE
)

required_columns <- c(
  "Date",
  "Black",
  "White",
  "Rb_After",
  "Rw_After",
  "Gb_After",
  "Gw_After"
)

missing_columns <- setdiff(required_columns, names(history))

if (length(missing_columns) > 0) {
  stop(
    "Missing columns in game_history.csv: ",
    paste(missing_columns, collapse = ", ")
  )
}

# -----------------------------
# Build one final state per player
# -----------------------------
black_states <- history %>%
  transmute(
    date = as.Date(Date),
    name = Black,
    rating = Rb_After,
    games = Gb_After
  )

white_states <- history %>%
  transmute(
    date = as.Date(Date),
    name = White,
    rating = Rw_After,
    games = Gw_After
  )

checkpoint <- bind_rows(
  black_states,
  white_states
) %>%
  filter(
    !is.na(date),
    date <= as.Date("2025-12-31"),
    !is.na(name),
    name != "",
    !is.na(rating),
    !is.na(games)
  ) %>%
  arrange(name, date, games) %>%
  group_by(name) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    name,
    rating = as.numeric(rating),
    games = as.integer(games),
    last_historical_game = format(date, "%Y-%m-%d")
  ) %>%
  arrange(name)

# -----------------------------
# Write checkpoint
# -----------------------------
write_csv(
  checkpoint,
  checkpoint_file
)

cat("Wrote checkpoint:", checkpoint_file, "\n")
cat("Players:", nrow(checkpoint), "\n")
cat(
  "Latest historical game:",
  max(checkpoint$last_historical_game),
  "\n"
)



