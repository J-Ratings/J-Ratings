library(utils)

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

dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Kaggle token
# -----------------------------
token <- Sys.getenv("KAGGLE_API_TOKEN")

if (token == "") {
  stop(
    "KAGGLE_API_TOKEN is not set. ",
    "Set it as a GitHub Actions secret, or set it locally before running this script."
  )
}

# The Kaggle CLI reads KAGGLE_API_TOKEN when authenticating.

# -----------------------------
# Download dataset using Kaggle CLI
# -----------------------------
dataset_slug <- "patateriedata/all-international-football-results"

cmd <- paste(
  "kaggle datasets download",
  "-d", shQuote(dataset_slug),
  "-p", shQuote(source_dir),
  "--unzip",
  "--force"
)

cat("Downloading Kaggle dataset:\n")
cat(dataset_slug, "\n")
cat("Destination:", source_dir, "\n")

status <- system(cmd)

if (!identical(status, 0L)) {
  stop("Kaggle download failed with exit status: ", status)
}

# -----------------------------
# Check expected files
# -----------------------------
required_files <- file.path(
  source_dir,
  c(
    "all_matches.csv"
  )
)

optional_files <- file.path(
  source_dir,
  c(
    "countries_names.csv"
  )
)

missing_required_files <- required_files[!file.exists(required_files)]

if (length(missing_required_files) > 0) {
  stop(
    "Kaggle download completed, but required files are missing: ",
    paste(missing_required_files, collapse = ", ")
  )
}

missing_optional_files <- optional_files[!file.exists(optional_files)]

cat("Required files found:\n")
print(basename(required_files))

if (length(missing_optional_files) > 0) {
  warning(
    "Optional files are missing: ",
    paste(basename(missing_optional_files), collapse = ", ")
  )
} else {
  cat("Optional files found:\n")
  print(basename(optional_files))
}

# -----------------------------
# Basic source sanity checks
# -----------------------------
all_matches_file <- file.path(source_dir, "all_matches.csv")

all_matches <- read.csv(
  all_matches_file,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

required_columns <- c(
  "date",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "tournament",
  "country",
  "neutral"
)

missing_columns <- setdiff(required_columns, names(all_matches))

if (length(missing_columns) > 0) {
  stop(
    "all_matches.csv is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

all_matches$date <- as.Date(all_matches$date)

if (all(is.na(all_matches$date))) {
  stop("Could not parse any dates from all_matches.csv.")
}

latest_source_date <- max(all_matches$date, na.rm = TRUE)

cat("Downloaded rows:", nrow(all_matches), "\n")
cat("Latest all_matches.csv date:", as.character(latest_source_date), "\n")

# -----------------------------
# Optional country-name mapping check
# -----------------------------
countries_names_file <- file.path(source_dir, "countries_names.csv")

if (file.exists(countries_names_file)) {
  countries_names <- read.csv(
    countries_names_file,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8"
  )
  
  required_country_columns <- c(
    "original_name",
    "current_name"
  )
  
  missing_country_columns <- setdiff(required_country_columns, names(countries_names))
  
  if (length(missing_country_columns) > 0) {
    warning(
      "countries_names.csv exists but is missing columns: ",
      paste(missing_country_columns, collapse = ", ")
    )
  } else {
    cat("Country name mapping rows:", nrow(countries_names), "\n")
  }
}

cat("Done.\n")