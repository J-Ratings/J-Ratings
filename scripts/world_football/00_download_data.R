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

# The newer Kaggle token format uses KAGGLE_API_TOKEN directly.
# The Kaggle CLI reads this environment variable when authenticating.

# -----------------------------
# Download dataset using Kaggle CLI
# -----------------------------
dataset_slug <- "martj42/international-football-results-from-1872-to-2017"

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
expected_files <- file.path(
  source_dir,
  c(
    "results.csv",
    "shootouts.csv",
    "goalscorers.csv",
    "former_names.csv"
  )
)

missing_files <- expected_files[!file.exists(expected_files)]

if (length(missing_files) > 0) {
  stop(
    "Kaggle download completed, but expected files are missing: ",
    paste(missing_files, collapse = ", ")
  )
}

cat("Downloaded files:\n")
print(basename(expected_files))

results_file <- file.path(source_dir, "results.csv")

if (file.exists(results_file)) {
  results <- read.csv(results_file, stringsAsFactors = FALSE)
  
  if ("date" %in% names(results)) {
    latest_source_date <- max(as.Date(results$date), na.rm = TRUE)
    cat("Latest source results.csv date:", as.character(latest_source_date), "\n")
  }
}

cat("Done.\n")