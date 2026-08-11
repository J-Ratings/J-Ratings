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
# Current season
# -----------------------------

season_folder <- Sys.getenv(
  "OPENFOOTBALL_SEASON",
  unset = current_openfootball_season()
)


# -----------------------------
# Download jobs
# -----------------------------

# Historical league files are already cached locally.
# Normal runs now check/refresh the current season only.

england_jobs <- data.frame(
  repo = rep("england", 5),
  source_folder = rep("england", 5),
  season = rep(season_folder, 5),
  remote_file = c(
    "1-premierleague.txt",
    "2-championship.txt",
    "3-league1.txt",
    "4-league2.txt",
    "5-nationalleague.txt"
  ),
  local_file = c(
    "1-premierleague.txt",
    "2-championship.txt",
    "3-league1.txt",
    "4-league2.txt",
    "5-nationalleague.txt"
  ),
  required = rep(FALSE, 5),
  stringsAsFactors = FALSE
)

spain_jobs <- data.frame(
  repo = rep("espana", 2),
  source_folder = rep("espana", 2),
  season = rep(season_folder, 2),
  remote_file = c("1-liga.txt", "2-liga2.txt"),
  local_file = c("1-liga.txt", "2-liga2.txt"),
  required = rep(FALSE, 2),
  stringsAsFactors = FALSE
)

italy_jobs <- data.frame(
  repo = rep("italy", 2),
  source_folder = rep("italy", 2),
  season = rep(season_folder, 2),
  remote_file = c("1-seriea.txt", "2-serieb.txt"),
  local_file = c("1-seriea.txt", "2-serieb.txt"),
  required = rep(FALSE, 2),
  stringsAsFactors = FALSE
)

germany_jobs <- data.frame(
  repo = rep("deutschland", 2),
  source_folder = rep("deutschland", 2),
  season = rep(season_folder, 2),
  remote_file = c("1-bundesliga.txt", "2-bundesliga2.txt"),
  local_file = c("1-bundesliga.txt", "2-bundesliga2.txt"),
  required = rep(FALSE, 2),
  stringsAsFactors = FALSE
)

# France is stored differently in the OpenFootball Europe repository:
# remote files live under /france and include the season in the filename.
france_jobs <- data.frame(
  repo = rep("europe", 2),
  source_folder = rep("france", 2),
  season = rep(season_folder, 2),
  remote_file = c(
    paste0("france/", season_folder, "_fr1.txt"),
    paste0("france/", season_folder, "_fr2.txt")
  ),
  local_file = c("1-ligue1.txt", "2-ligue2.txt"),
  required = rep(FALSE, 2),
  stringsAsFactors = FALSE
)

download_jobs <- rbind(
  england_jobs,
  spain_jobs,
  italy_jobs,
  germany_jobs,
  france_jobs
)

# -----------------------------
# Download
# -----------------------------

cat("Repo:", repo_dir, "\n")
cat("OpenFootball current season:", season_folder, "\n")
cat("Source root:", source_root_dir, "\n\n")

download_results <- data.frame(
  repo = character(),
  season = character(),
  file = character(),
  status = character(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(download_jobs))) {
  job <- download_jobs[i, ]
  
  dest <- file.path(
    source_root_dir,
    job$source_folder,
    job$season,
    job$local_file
  )
  
  base_url <- paste0(
    "https://raw.githubusercontent.com/openfootball/",
    job$repo,
    "/master"
  )
  
  url <- paste(base_url, job$season, job$remote_file, sep = "/")
  
  # France remote files are flat under /france rather than /<season>/.
  if (job$repo == "europe" && job$source_folder == "france") {
    url <- paste(base_url, job$remote_file, sep = "/")
  }
  
  result <- tryCatch(
    {
      download_one_file(url, dest)
      "downloaded"
    },
    error = function(e) {
      if (!isTRUE(job$required)) {
        message(
          "Optional file not downloaded: ",
          job$source_folder, "/", job$season, "/", job$local_file
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
      file = job$local_file,
      status = result,
      stringsAsFactors = FALSE
    )
  )
}

cat("OpenFootball download complete.\n")
cat("Current season:", season_folder, "\n")
cat("Files:\n")
print(download_results)
