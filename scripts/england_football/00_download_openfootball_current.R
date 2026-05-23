# scripts/england_football/00_download_openfootball_current.R

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

# -----------------------------
# Helpers
# -----------------------------

current_openfootball_season <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))
  
  start_year <- if (m >= 7L) y else y - 1L
  end_short <- sprintf("%02d", (start_year + 1L) %% 100L)
  
  paste0(start_year, "-", end_short)
}

download_one_file <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  
  tmp <- paste0(dest, ".tmp")
  
  if (file.exists(tmp)) {
    file.remove(tmp)
  }
  
  cat("Downloading:\n")
  cat("  ", url, "\n", sep = "")
  cat("To:\n")
  cat("  ", dest, "\n", sep = "")
  
  ok <- tryCatch(
    {
      download.file(
        url = url,
        destfile = tmp,
        mode = "wb",
        quiet = FALSE
      )
      TRUE
    },
    error = function(e) {
      message("Download failed: ", conditionMessage(e))
      FALSE
    },
    warning = function(w) {
      message("Download warning: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  if (!ok) {
    if (file.exists(tmp)) file.remove(tmp)
    stop("Failed to download: ", url)
  }
  
  if (!file.exists(tmp)) {
    stop("Download did not create a file: ", tmp)
  }
  
  if (file.info(tmp)$size == 0) {
    file.remove(tmp)
    stop("Downloaded file is empty: ", url)
  }
  
  file.copy(tmp, dest, overwrite = TRUE)
  file.remove(tmp)
  
  cat("Done.\n\n")
}

# -----------------------------
# Season and files
# -----------------------------

season_folder <- Sys.getenv(
  "OPENFOOTBALL_SEASON",
  unset = current_openfootball_season()
)

out_dir <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Source",
  "openfootball",
  "england",
  season_folder
)

files <- c(
  "1-premierleague.txt",
  "2-championship.txt",
  "3-league1.txt",
  "4-league2.txt"
)

base_url <- "https://raw.githubusercontent.com/openfootball/england/master"

# -----------------------------
# Download
# -----------------------------

cat("Repo:", repo_dir, "\n")
cat("OpenFootball season:", season_folder, "\n")
cat("Output folder:", out_dir, "\n\n")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (f in files) {
  url <- paste(base_url, season_folder, f, sep = "/")
  dest <- file.path(out_dir, f)
  
  download_one_file(url, dest)
}

downloaded_files <- file.path(out_dir, files)
missing_files <- downloaded_files[!file.exists(downloaded_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing downloaded file(s):\n",
    paste0(" - ", missing_files, collapse = "\n")
  )
}

cat("All OpenFootball files downloaded successfully.\n")
cat("Season:", season_folder, "\n")
cat("Files:\n")
cat(paste0(" - ", downloaded_files, collapse = "\n"), "\n")