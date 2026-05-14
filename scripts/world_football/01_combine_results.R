library(readr)
library(dplyr)
library(lubridate)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

source_dir <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "source"
)

processed_dir <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "processed"
)

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

results_file <- file.path(source_dir, "results.csv")
shootouts_file <- file.path(source_dir, "shootouts.csv")
output_file <- file.path(processed_dir, "results_with_winner.csv")

if (!file.exists(results_file)) {
  stop("Missing results file: ", results_file)
}

if (!file.exists(shootouts_file)) {
  stop("Missing shootouts file: ", shootouts_file)
}

# -----------------------------
# Helpers
# -----------------------------
parse_mixed_date <- function(x) {
  x_chr <- trimws(as.character(x))
  
  parsed <- case_when(
    is.na(x_chr) | x_chr == "" ~ as.Date(NA),
    grepl("^[0-9]+$", x_chr) ~ as.Date(as.numeric(x_chr), origin = "1899-12-30"),
    grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x_chr) ~ dmy(x_chr),
    TRUE ~ ymd(x_chr)
  )
  
  as.Date(parsed)
}

# -----------------------------
# Load source data
# -----------------------------
results <- read_csv(
  results_file,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

shootouts <- read_csv(
  shootouts_file,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

required_results_cols <- c(
  "date",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "tournament"
)

required_shootouts_cols <- c(
  "date",
  "home_team",
  "away_team",
  "winner",
  "first_shooter"
)

missing_results_cols <- setdiff(required_results_cols, names(results))
missing_shootouts_cols <- setdiff(required_shootouts_cols, names(shootouts))

if (length(missing_results_cols) > 0) {
  stop(
    "results.csv is missing required columns: ",
    paste(missing_results_cols, collapse = ", ")
  )
}

if (length(missing_shootouts_cols) > 0) {
  stop(
    "shootouts.csv is missing required columns: ",
    paste(missing_shootouts_cols, collapse = ", ")
  )
}

# -----------------------------
# Clean results
# -----------------------------
today <- Sys.Date()

results <- results %>%
  mutate(
    date = parse_mixed_date(date),
    home_team = trimws(as.character(home_team)),
    away_team = trimws(as.character(away_team)),
    home_score = as.integer(home_score),
    away_score = as.integer(away_score),
    tournament = trimws(as.character(tournament))
  ) %>%
  filter(
    !is.na(date),
    date <= today,
    !is.na(home_score),
    !is.na(away_score),
    home_team != "",
    away_team != ""
  )

# -----------------------------
# Clean shootouts
# -----------------------------
shootouts <- shootouts %>%
  mutate(
    date = parse_mixed_date(date),
    home_team = trimws(as.character(home_team)),
    away_team = trimws(as.character(away_team)),
    winner = trimws(as.character(winner)),
    first_shooter = trimws(as.character(first_shooter))
  ) %>%
  filter(
    !is.na(date),
    home_team != "",
    away_team != ""
  ) %>%
  select(date, home_team, away_team, winner, first_shooter)

# -----------------------------
# Combine results and shootouts
# -----------------------------
out <- results %>%
  left_join(
    shootouts,
    by = c("date", "home_team", "away_team")
  ) %>%
  mutate(
    result = case_when(
      home_score > away_score ~ home_team,
      away_score > home_score ~ away_team,
      home_score == away_score & !is.na(winner) & winner != "" ~ winner,
      TRUE ~ "Draw"
    ),
    score = case_when(
      home_score == away_score & !is.na(winner) & winner != "" ~ paste0(home_score, "-", away_score, " (pens)"),
      TRUE ~ paste0(home_score, "-", away_score)
    ),
    date = format(date, "%Y-%m-%d")
  ) %>%
  select(
    date,
    home_team,
    away_team,
    result,
    score,
    tournament
  ) %>%
  arrange(date, tournament, home_team, away_team)

# -----------------------------
# Write output
# -----------------------------
write_csv(out, output_file)

cat("Wrote:", output_file, "\n")
cat("Rows:", nrow(out), "\n")
cat("Latest date:", max(out$date, na.rm = TRUE), "\n")

# Basic check for 2022 World Cup final penalty handling
check_row <- out %>%
  filter(
    home_team == "Argentina",
    away_team == "France",
    date == "2022-12-18"
  )

if (nrow(check_row) > 0) {
  print(check_row)
}