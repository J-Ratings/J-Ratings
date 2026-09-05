options(stringsAsFactors = FALSE)

script_start_time <- Sys.time()

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

current_openfootball_season <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))
  start_year <- if (m >= 7L) y else y - 1L
  sprintf("%04d-%02d", start_year, (start_year + 1L) %% 100L)
}

season_start_year <- function(season_folder) {
  as.integer(substr(season_folder, 1, 4))
}

wikipedia_page_url <- function(page_title) {
  encoded <- utils::URLencode(
    gsub(" ", "_", page_title, fixed = TRUE),
    reserved = TRUE
  )
  paste0("https://en.wikipedia.org/wiki/", encoded)
}

wikipedia_premier_league_title <- function(start_year) {
  start_year <- as.integer(start_year)
  end_short <- sprintf("%02d", (start_year + 1L) %% 100L)
  paste0(start_year, "\u2013", end_short, " Premier League")
}

download_one_file <- function(url, dest, headers = NULL) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  tmp <- paste0(dest, ".tmp")
  if (file.exists(tmp)) file.remove(tmp)

  cat("Downloading Premier League Wikipedia page:\n")
  cat("  ", url, "\n", sep = "")
  cat("To:\n")
  cat("  ", dest, "\n", sep = "")

  args <- list(
    url = url,
    destfile = tmp,
    mode = "wb",
    quiet = FALSE,
    method = "libcurl"
  )

  if (!is.null(headers)) args$headers <- headers

  ok <- tryCatch(
    {
      suppressWarnings(do.call(download.file, args))
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

  if (!file.exists(tmp) || file.info(tmp)$size == 0) {
    if (file.exists(tmp)) file.remove(tmp)
    stop("Wikipedia download produced an empty file.")
  }

  file.copy(tmp, dest, overwrite = TRUE)
  file.remove(tmp)

  cat("Done.\n")
}

season_folder <- Sys.getenv(
  "OPENFOOTBALL_SEASON",
  unset = current_openfootball_season()
)

start_year <- season_start_year(season_folder)

wikipedia_source_root_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "wikipedia"
)

dest <- file.path(
  wikipedia_source_root_dir,
  "premier_league",
  season_folder,
  "page.html"
)

wikimedia_user_agent <- Sys.getenv(
  "WIKIMEDIA_USER_AGENT",
  unset = "J-Ratings/1.0 (European football ratings project)"
)

page_title <- wikipedia_premier_league_title(start_year)
url <- wikipedia_page_url(page_title)

cat("Repo:", repo_dir, "\n")
cat("Season:", season_folder, "\n")
cat("Mode: Premier League Wikipedia only\n\n")

download_one_file(
  url,
  dest,
  headers = c(
    "User-Agent" = wikimedia_user_agent,
    "Accept-Language" = "en-GB,en;q=0.9"
  )
)

# Only one Wikimedia request is made in this test script, so there is no
# inter-request sleep to apply. Future multi-request Wikimedia downloaders
# should use at least 3 seconds between requests.

cat(
  "\n00 PL Wikipedia-only elapsed time: ",
  round(as.numeric(difftime(Sys.time(), script_start_time, units = "secs")), 1),
  " seconds\n",
  sep = ""
)
