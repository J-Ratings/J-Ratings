# install.packages(c("jsonlite", "dplyr", "readr", "purrr", "stringr", "lubridate", "tibble"))

library(jsonlite)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(tibble)

options(stringsAsFactors = FALSE)

# ============================================================
# Test Cricket Cricsheet JSON updater
#
# Purpose:
#   - Run inside the J-Ratings Git repo
#   - Download Cricsheet men's Test JSON zip if needed / requested
#   - Unzip raw JSON files
#   - Extract one match-level row per Test match
#   - Write clean master CSV for Elo calculation
#
# Main output:
#   Cricket/Test Cricket/pipeline_data/Matches/test_cricket_results_master.csv
#
# Raw source:
#   Cricket/Test Cricket/pipeline_data/Source/cricsheet/tests_male_json/
#
# GitHub Actions:
#   - Works from GITHUB_WORKSPACE
#   - Set CRICSHEET_FORCE_DOWNLOAD=true to force a fresh download
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
    dir.exists(file.path(wd, "Cricket", "Test Cricket")) &&
    dir.exists(file.path(wd, "scripts", "test_cricket"))
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

source_dir <- file.path(
  repo_dir,
  "Cricket", "Test Cricket", "pipeline_data", "Source", "cricsheet"
)

json_dir <- file.path(source_dir, "tests_male_json")
zip_file <- file.path(source_dir, "tests_male_json.zip")

match_dir <- file.path(
  repo_dir,
  "Cricket", "Test Cricket", "pipeline_data", "Matches"
)

out_file <- file.path(
  match_dir,
  "test_cricket_results_master.csv"
)

review_file <- file.path(
  match_dir,
  "test_cricket_results_review.csv"
)

dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(match_dir, recursive = TRUE, showWarnings = FALSE)

cat("Repo dir: ", repo_dir, "\n", sep = "")
cat("Source dir: ", source_dir, "\n", sep = "")
cat("JSON dir: ", json_dir, "\n", sep = "")
cat("Output CSV: ", out_file, "\n", sep = "")

# -----------------------------
# Download settings
# -----------------------------

zip_url <- "https://cricsheet.org/downloads/tests_male_json.zip"

force_download <- tolower(Sys.getenv("CRICSHEET_FORCE_DOWNLOAD", unset = "false")) %in%
  c("true", "1", "yes", "y")

need_download <- force_download || !dir.exists(json_dir) ||
  length(list.files(json_dir, pattern = "\\.json$", full.names = TRUE)) == 0

if (need_download) {
  cat("Downloading Cricsheet men's Test JSON zip...\n")
  
  download.file(
    url = zip_url,
    destfile = zip_file,
    mode = "wb",
    quiet = FALSE
  )
  
  if (dir.exists(json_dir)) {
    unlink(json_dir, recursive = TRUE, force = TRUE)
  }
  
  dir.create(json_dir, recursive = TRUE, showWarnings = FALSE)
  
  cat("Unzipping Cricsheet JSON files...\n")
  unzip(zip_file, exdir = json_dir)
} else {
  cat("Using existing local Cricsheet JSON files.\n")
}

# -----------------------------
# Helpers
# -----------------------------

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x[[1]])
  x <- str_squish(x)
  ifelse(x == "", NA_character_, x)
}

safe_int <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_integer_)
  suppressWarnings(as.integer(x[[1]]))
}

clean_team_name <- function(x) {
  x <- str_squish(as.character(x))
  
  team_map <- c(
    "United States of America" = "United States",
    "U.S.A." = "United States",
    "USA" = "United States"
  )
  
  ifelse(
    x %in% names(team_map),
    unname(team_map[x]),
    x
  )
}

make_margin <- function(winner, outcome_by, outcome_result) {
  if (!is.null(winner) && !is.null(outcome_by$innings) && !is.null(outcome_by$runs)) {
    return(paste0(winner, " won by an innings and ", outcome_by$runs, " runs"))
  }
  
  if (!is.null(winner) && !is.null(outcome_by$runs)) {
    return(paste0(winner, " won by ", outcome_by$runs, " runs"))
  }
  
  if (!is.null(winner) && !is.null(outcome_by$wickets)) {
    return(paste0(winner, " won by ", outcome_by$wickets, " wickets"))
  }
  
  if (!is.null(outcome_result)) {
    res <- safe_chr(outcome_result)
    if (!is.na(res) && tolower(res) == "draw") return("Draw")
    if (!is.na(res) && tolower(res) == "tie") return("Tie")
    return(res)
  }
  
  NA_character_
}

parse_match_file <- function(path) {
  dat <- tryCatch(
    fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      warning("Failed to parse JSON: ", basename(path), " - ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(dat) || is.null(dat$info)) {
    return(tibble(
      source_file = basename(path),
      parse_status = "missing_info"
    ))
  }
  
  info <- dat$info
  
  teams <- info$teams
  if (is.null(teams) || length(teams) < 2) {
    return(tibble(
      source_file = basename(path),
      parse_status = "missing_teams"
    ))
  }
  
  home_team <- clean_team_name(teams[[1]])
  away_team <- clean_team_name(teams[[2]])
  
  dates <- info$dates
  match_date <- if (!is.null(dates) && length(dates) > 0) {
    suppressWarnings(as.Date(dates[[1]]))
  } else {
    as.Date(NA)
  }
  
  outcome <- info$outcome
  winner <- if (!is.null(outcome$winner)) clean_team_name(safe_chr(outcome$winner)) else NA_character_
  outcome_result <- if (!is.null(outcome$result)) safe_chr(outcome$result) else NA_character_
  
  result <- case_when(
    !is.na(winner) ~ winner,
    !is.na(outcome_result) & tolower(outcome_result) == "draw" ~ "Draw",
    !is.na(outcome_result) & tolower(outcome_result) == "tie" ~ "Draw",
    TRUE ~ NA_character_
  )
  
  result_type <- case_when(
    !is.na(winner) ~ "W",
    !is.na(outcome_result) & tolower(outcome_result) == "draw" ~ "D",
    !is.na(outcome_result) & tolower(outcome_result) == "tie" ~ "T",
    TRUE ~ NA_character_
  )
  
  margin <- make_margin(
    winner = winner,
    outcome_by = outcome$by,
    outcome_result = outcome$result
  )
  
  margin_type <- case_when(
    !is.null(outcome$by$innings) & !is.null(outcome$by$runs) ~ "innings_runs",
    !is.null(outcome$by$runs) ~ "runs",
    !is.null(outcome$by$wickets) ~ "wickets",
    !is.na(outcome_result) & tolower(outcome_result) == "draw" ~ "draw",
    !is.na(outcome_result) & tolower(outcome_result) == "tie" ~ "tie",
    TRUE ~ NA_character_
  )
  
  margin_value <- case_when(
    !is.null(outcome$by$runs) ~ safe_int(outcome$by$runs),
    !is.null(outcome$by$wickets) ~ safe_int(outcome$by$wickets),
    TRUE ~ NA_integer_
  )
  
  event_name <- if (!is.null(info$event$name)) safe_chr(info$event$name) else NA_character_
  event_match_number <- if (!is.null(info$event$match_number)) safe_int(info$event$match_number) else NA_integer_
  
  tibble(
    date = format(match_date, "%Y-%m-%d"),
    home_team = home_team,
    away_team = away_team,
    result = result,
    result_type = result_type,
    margin = margin,
    margin_type = margin_type,
    margin_value = margin_value,
    city = safe_chr(info$city),
    venue = safe_chr(info$venue),
    event_name = event_name,
    event_match_number = event_match_number,
    season = safe_chr(info$season),
    match_type = safe_chr(info$match_type),
    match_type_number = safe_int(info$match_type_number),
    gender = safe_chr(info$gender),
    team_type = safe_chr(info$team_type),
    balls_per_over = safe_int(info$balls_per_over),
    source_file = basename(path),
    competition = "Test cricket",
    parse_status = "ok"
  )
}

# -----------------------------
# Parse JSON files
# -----------------------------

json_files <- list.files(
  json_dir,
  pattern = "\\.json$",
  full.names = TRUE
)

if (length(json_files) == 0) {
  stop("No JSON files found in: ", json_dir)
}

cat("JSON files found: ", length(json_files), "\n", sep = "")

raw_matches <- map_dfr(json_files, parse_match_file)

review_rows <- raw_matches %>%
  filter(parse_status != "ok" | is.na(date) | is.na(result))

if (nrow(review_rows) > 0) {
  write_csv(review_rows, review_file, na = "")
  warning(
    "Some files need review. Wrote: ",
    review_file,
    " rows: ",
    nrow(review_rows)
  )
}

# -----------------------------
# Clean master rows
# -----------------------------

master <- raw_matches %>%
  filter(parse_status == "ok") %>%
  mutate(
    date = as.Date(date),
    home_team = clean_team_name(home_team),
    away_team = clean_team_name(away_team),
    result = case_when(
      result == "Tie" ~ "Draw",
      TRUE ~ clean_team_name(result)
    ),
    city = str_squish(as.character(city)),
    venue = str_squish(as.character(venue)),
    event_name = str_squish(as.character(event_name)),
    season = str_squish(as.character(season)),
    match_type = str_squish(as.character(match_type)),
    gender = str_squish(as.character(gender)),
    team_type = str_squish(as.character(team_type)),
    competition = str_squish(as.character(competition))
  ) %>%
  filter(
    !is.na(date),
    !is.na(home_team),
    !is.na(away_team),
    home_team != "",
    away_team != "",
    match_type == "Test",
    gender == "male",
    !is.na(result),
    result != ""
  ) %>%
  arrange(date, match_type_number, home_team, away_team) %>%
  distinct(match_type_number, .keep_all = TRUE) %>%
  mutate(date = format(date, "%Y-%m-%d")) %>%
  select(
    date,
    home_team,
    away_team,
    result,
    result_type,
    margin,
    margin_type,
    margin_value,
    city,
    venue,
    event_name,
    event_match_number,
    season,
    match_type,
    match_type_number,
    gender,
    team_type,
    balls_per_over,
    source_file,
    competition
  )

if (nrow(master) == 0) {
  stop("No usable Test cricket rows after cleaning.")
}

# -----------------------------
# Validate result labels
# -----------------------------

bad_results <- master %>%
  filter(!(result %in% c(home_team, away_team, "Draw"))) %>%
  count(result, sort = TRUE)

if (nrow(bad_results) > 0) {
  cat("\nBad result labels:\n")
  print(bad_results, n = 100)
  stop("Stopped because some result labels do not match home/away team names or Draw.")
}

duplicate_check <- master %>%
  count(match_type_number, name = "n") %>%
  filter(!is.na(match_type_number), n > 1)

if (nrow(duplicate_check) > 0) {
  cat("\nDuplicate match_type_number values:\n")
  print(duplicate_check, n = 100)
  stop("Stopped because duplicate match_type_number values remain.")
}

# -----------------------------
# Save
# -----------------------------

write_csv(master, out_file, na = "")

# -----------------------------
# Summary
# -----------------------------

cat("\nDone.\n")
cat("Master rows: ", nrow(master), "\n", sep = "")
cat("Date range: ", min(master$date), " to ", max(master$date), "\n", sep = "")
cat("Teams: ", length(unique(c(master$home_team, master$away_team))), "\n", sep = "")
cat("Output: ", out_file, "\n", sep = "")

cat("\nLatest rows:\n")
print(tail(master, 20))