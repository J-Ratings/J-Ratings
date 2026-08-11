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

source_root_dir <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Source",
  "openfootball"
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

season_folder_from_start_year <- function(start_year) {
  paste0(
    as.integer(start_year),
    "-",
    sprintf("%02d", (as.integer(start_year) + 1L) %% 100L)
  )
}

season_start_year <- function(season_folder) {
  as.integer(substr(season_folder, 1, 4))
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
      suppressWarnings(
        download.file(
          url = url,
          destfile = tmp,
          mode = "wb",
          quiet = FALSE
        )
      )
      TRUE
    },
    error = function(e) {
      message("Download failed: ", conditionMessage(e))
      FALSE
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
# Seasons
# -----------------------------

season_folder <- Sys.getenv(
  "OPENFOOTBALL_SEASON",
  unset = current_openfootball_season()
)

current_start_year <- season_start_year(season_folder)

# La Liga is available in the OpenFootball España repository from 2012/13.
la_liga_seasons <- vapply(
  2012L:current_start_year,
  season_folder_from_start_year,
  character(1)
)

# -----------------------------
# Download jobs
# -----------------------------

# England: keep the current behaviour - only refresh the current season.
england_jobs <- data.frame(
  repo = rep("england", 5),
  source_folder = rep("england", 5),
  season = rep(season_folder, 5),
  file = c(
    "1-premierleague.txt",
    "2-championship.txt",
    "3-league1.txt",
    "4-league2.txt",
    "5-nationalleague.txt"
  ),
  required = rep(FALSE, 5),
  stringsAsFactors = FALSE
)

# Spain: first new competition.
# Backfill La Liga from 2012/13 through the current season.
# Completed historical seasons are required. The current season is allowed
# to be missing temporarily because OpenFootball may publish it shortly
# before the league begins.
spain_jobs <- data.frame(
  repo = rep("espana", length(la_liga_seasons)),
  source_folder = rep("espana", length(la_liga_seasons)),
  season = la_liga_seasons,
  file = rep("1-liga.txt", length(la_liga_seasons)),
  required = la_liga_seasons != season_folder,
  stringsAsFactors = FALSE
)

download_jobs <- rbind(
  england_jobs,
  spain_jobs
)

# -----------------------------
# Download
# -----------------------------

cat("Repo:", repo_dir, "\n")
cat("OpenFootball current season:", season_folder, "\n")
cat("Source root:", source_root_dir, "\n")
cat("La Liga seasons requested:", paste(la_liga_seasons, collapse = ", "), "\n\n")

download_results <- data.frame(
  repo = character(),
  season = character(),
  file = character(),
  status = character(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(download_jobs))) {
  job <- download_jobs[i, ]
  
  base_url <- paste0(
    "https://raw.githubusercontent.com/openfootball/",
    job$repo,
    "/master"
  )
  
  url <- paste(base_url, job$season, job$file, sep = "/")
  
  dest <- file.path(
    source_root_dir,
    job$source_folder,
    job$season,
    job$file
  )
  
  result <- tryCatch(
    {
      download_one_file(url, dest)
      "downloaded"
    },
    error = function(e) {
      if (!isTRUE(job$required)) {
        message(
          "Optional file not downloaded: ",
          job$source_folder, "/", job$season, "/", job$file
        )
        message("Reason: ", conditionMessage(e))
        "missing_optional"
      } else {
        stop(e)
      }
    }
  )
  
  Sys.sleep(1)
  
  download_results <- rbind(
    download_results,
    data.frame(
      repo = job$repo,
      season = job$season,
      file = job$file,
      status = result,
      stringsAsFactors = FALSE
    )
  )
}

required_jobs <- download_jobs[download_jobs$required, ]

if (nrow(required_jobs) > 0) {
  required_paths <- file.path(
    source_root_dir,
    required_jobs$source_folder,
    required_jobs$season,
    required_jobs$file
  )
  
  missing_required <- required_paths[!file.exists(required_paths)]
  
  if (length(missing_required) > 0) {
    stop(
      "Missing required downloaded file(s):\n",
      paste0(" - ", missing_required, collapse = "\n")
    )
  }
}

cat("OpenFootball download complete.\n")
cat("Current season:", season_folder, "\n")
cat("Files:\n")
print(download_results)
