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

all_matches_file <- file.path(source_dir, "all_matches.csv")
output_file <- file.path(processed_dir, "results_with_winner.csv")

if (!file.exists(all_matches_file)) {
  stop("Missing all_matches file: ", all_matches_file)
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

clean_score_num <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null", "na")] <- NA_character_
  suppressWarnings(as.integer(x_chr))
}

normalise_team_name <- function(x) {
  x0 <- trimws(as.character(x))
  
  team_map <- c(
    "Åland" = "Åland Islands",
    
    # Match new data-source names to existing site asset names
    "Czechia" = "Czech Republic",
    "Ireland" = "Republic of Ireland",
    "China" = "China PR",
    
    # Spelling / diacritics
    "Curacao" = "Curaçao",
    "Reunion" = "Réunion",
    "Sao Tome and Principe" = "São Tomé and Príncipe",
    "São Tome and Principe" = "São Tomé and Príncipe",
    "Saint Barthelemy" = "Saint Barthélemy",
    
    # Common abbreviations
    "St Vincent & Grenadines" = "Saint Vincent and the Grenadines",
    "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
    "Saint Vincent & Grenadines" = "Saint Vincent and the Grenadines",
    
    # Territory / country naming
    "US Virgin Islands" = "United States Virgin Islands",
    "Macao" = "Macau",
    "Eastern Samoa" = "American Samoa",
    "East Timor" = "Timor-Leste"
  )
  
  ifelse(x0 %in% names(team_map), unname(team_map[x0]), x0)
}

# -----------------------------
# Load source data
# -----------------------------
all_matches <- read_csv(
  all_matches_file,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

required_cols <- c(
  "date",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "tournament"
)

missing_cols <- setdiff(required_cols, names(all_matches))

if (length(missing_cols) > 0) {
  stop(
    "all_matches.csv is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

# -----------------------------
# Clean results
# -----------------------------
today <- Sys.Date()

results <- all_matches %>%
  mutate(
    date = parse_mixed_date(date),
    home_team = normalise_team_name(home_team),
    away_team = normalise_team_name(away_team),
    home_score = clean_score_num(home_score),
    away_score = clean_score_num(away_score),
    tournament = trimws(as.character(tournament))
  ) %>%
  filter(
    !is.na(date),
    date <= today,
    !is.na(home_score),
    !is.na(away_score),
    home_team != "",
    away_team != "",
    tournament != ""
  )

# -----------------------------
# Build downstream results file
# -----------------------------
out <- results %>%
  mutate(
    result = case_when(
      home_score > away_score ~ home_team,
      away_score > home_score ~ away_team,
      TRUE ~ "Draw"
    ),
    score = paste0(home_score, "-", away_score),
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
# Sanity checks
# -----------------------------
if (nrow(out) == 0) {
  stop("No usable rows were produced from all_matches.csv.")
}

bad_results <- out %>%
  filter(
    result != "Draw",
    result != home_team,
    result != away_team
  )

if (nrow(bad_results) > 0) {
  print(bad_results)
  stop("Some result labels do not match home_team, away_team, or Draw.")
}

duplicate_rows <- out %>%
  count(date, home_team, away_team, score, tournament, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_rows) > 0) {
  warning(
    "Possible duplicate match rows found: ",
    nrow(duplicate_rows),
    ". These were not removed."
  )
  
  print(duplicate_rows, n = min(20, nrow(duplicate_rows)))
}

# -----------------------------
# Write output
# -----------------------------
write_csv(out, output_file)

cat("Wrote:", output_file, "\n")
cat("Rows:", nrow(out), "\n")
cat("Latest date:", max(out$date, na.rm = TRUE), "\n")

# Basic penalty sanity check: 2022 World Cup final should remain a draw
check_row <- out %>%
  filter(
    home_team == "Argentina",
    away_team == "France",
    date == "2022-12-18"
  )

if (nrow(check_row) > 0) {
  print(check_row)
}