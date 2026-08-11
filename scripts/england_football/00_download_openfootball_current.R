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

make_season_jobs <- function(
    repo,
    source_folder,
    first_start_year,
    current_start_year,
    remote_file_fun,
    local_file_fun = remote_file_fun,
    current_optional = TRUE
) {
  seasons <- vapply(
    first_start_year:current_start_year,
    season_folder_from_start_year,
    character(1)
  )
  
  data.frame(
    repo = repo,
    source_folder = source_folder,
    season = seasons,
    remote_file = vapply(seasons, remote_file_fun, character(1)),
    local_file = vapply(seasons, local_file_fun, character(1)),
    required = rep(FALSE, length(seasons)),
    refresh_current = TRUE,
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# Current season
# -----------------------------

season_folder <- Sys.getenv(
  "OPENFOOTBALL_SEASON",
  unset = current_openfootball_season()
)

current_start_year <- season_start_year(season_folder)

# -----------------------------
# Download jobs
# -----------------------------

# England: current season only.
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
  refresh_current = rep(TRUE, 5),
  stringsAsFactors = FALSE
)

# La Liga: historical backfill is already complete, so current season only.
la_liga_jobs <- data.frame(
  repo = "espana",
  source_folder = "espana",
  season = season_folder,
  remote_file = "1-liga.txt",
  local_file = "1-liga.txt",
  required = FALSE,
  refresh_current = TRUE,
  stringsAsFactors = FALSE
)

# Segunda División: Spain division 2, backfill once from 2012/13.
segunda_jobs <- make_season_jobs(
  repo = "espana",
  source_folder = "espana",
  first_start_year = 2012L,
  current_start_year = current_start_year,
  remote_file_fun = function(s) "2-liga2.txt"
)

# Italy: Serie A and Serie B, backfill once from 2013/14.
serie_a_jobs <- make_season_jobs(
  repo = "italy",
  source_folder = "italy",
  first_start_year = 2013L,
  current_start_year = current_start_year,
  remote_file_fun = function(s) "1-seriea.txt"
)

serie_b_jobs <- make_season_jobs(
  repo = "italy",
  source_folder = "italy",
  first_start_year = 2013L,
  current_start_year = current_start_year,
  remote_file_fun = function(s) "2-serieb.txt"
)

# Germany: Bundesliga and 2. Bundesliga, backfill once from 2010/11.
bundesliga_jobs <- make_season_jobs(
  repo = "deutschland",
  source_folder = "deutschland",
  first_start_year = 2010L,
  current_start_year = current_start_year,
  remote_file_fun = function(s) "1-bundesliga.txt"
)

bundesliga2_jobs <- make_season_jobs(
  repo = "deutschland",
  source_folder = "deutschland",
  first_start_year = 2010L,
  current_start_year = current_start_year,
  remote_file_fun = function(s) "2-bundesliga2.txt"
)

# France is stored differently in the OpenFootball Europe repository:
# files live under /france and include the season in the filename.
ligue1_jobs <- make_season_jobs(
  repo = "europe",
  source_folder = "france",
  first_start_year = 2014L,
  current_start_year = current_start_year,
  remote_file_fun = function(s) paste0("france/", s, "_fr1.txt"),
  local_file_fun = function(s) "1-ligue1.txt"
)

ligue2_jobs <- make_season_jobs(
  repo = "europe",
  source_folder = "france",
  first_start_year = 2014L,
  current_start_year = current_start_year,
  remote_file_fun = function(s) paste0("france/", s, "_fr2.txt"),
  local_file_fun = function(s) "2-ligue2.txt"
)

download_jobs <- rbind(
  england_jobs,
  la_liga_jobs,
  segunda_jobs,
  serie_a_jobs,
  serie_b_jobs,
  bundesliga_jobs,
  bundesliga2_jobs,
  ligue1_jobs,
  ligue2_jobs
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
  
  is_current <- identical(as.character(job$season), season_folder)
  
  # Historical files are cached permanently once downloaded.
  # Only current-season files are refreshed on every normal run.
  if (file.exists(dest) && !is_current) {
    result <- "already_exists"
  } else {
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
  }
  
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
