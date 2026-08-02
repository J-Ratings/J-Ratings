# ============================================================
# Scrape complete GoRatings profile histories
#
# This script:
#   - discovers active player profiles from the GoRatings homepage
#   - downloads each profile once
#   - retains every dated game shown on each profile
#   - does not filter out games before 2026
#   - saves progress every 25 profiles
#
# Outputs:
#   Go/pipeline_data/goratings/goratings_games_all_raw.csv
#   Go/pipeline_data/goratings/failed_profiles_all.csv
#
# It does NOT overwrite:
#   Go/pipeline_data/goratings/goratings_games_raw.csv
#   Go/pipeline_data/processed/goratings_games_2026.csv
# ============================================================

library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(tibble)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

goratings_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "goratings"
)

dir.create(
  goratings_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  goratings_dir,
  "goratings_games_all_raw.csv"
)

failed_file <- file.path(
  goratings_dir,
  "failed_profiles_all.csv"
)

# -----------------------------
# Source settings
# -----------------------------
GORATINGS_BASE_URL <- "https://www.goratings.org/en"

cat("GoRatings output folder:", goratings_dir, "\n")
cat("This scrape will retain all dated profile games.\n")

# -----------------------------
# Read homepage
# -----------------------------
homepage <- read_html(
  GORATINGS_BASE_URL
)

page_title <- homepage %>%
  html_element("title") %>%
  html_text2()

cat("GoRatings page title:", page_title, "\n")

# -----------------------------
# Extract active player links
# -----------------------------
player_links <- homepage %>%
  html_elements('a[href*="/players/"]') %>%
  html_attr("href") %>%
  unique()

player_urls <- url_absolute(
  player_links,
  GORATINGS_BASE_URL
)

# Remove missing values before applying the pattern test
player_urls <- player_urls[
  !is.na(player_urls)
]

# Keep only standard numeric player profile URLs
player_urls <- player_urls[
  str_detect(
    player_urls,
    "/players/\\d+\\.html$"
  )
]

player_urls <- sort(
  unique(player_urls)
)

cat(
  "Player profile links found:",
  length(player_urls),
  "\n"
)

if (length(player_urls) == 0) {
  stop("No valid GoRatings player profile URLs were found.")
}

# -----------------------------
# Parse one complete player profile
# -----------------------------
parse_player_profile <- function(player_url) {
  profile <- read_html(
    player_url
  )
  
  player_id <- player_url %>%
    str_extract("\\d+(?=\\.html$)")
  
  player_name_node <- profile %>%
    html_element("h1")
  
  if (length(player_name_node) == 0) {
    warning(
      "No player heading found for: ",
      player_url
    )
    
    return(tibble())
  }
  
  player_name <- player_name_node %>%
    html_text2()
  
  tables <- profile %>%
    html_elements("table")
  
  if (length(tables) < 2) {
    warning(
      "No game table found for: ",
      player_url
    )
    
    return(tibble())
  }
  
  game_rows <- tables[[2]] %>%
    html_elements("tr")
  
  if (length(game_rows) <= 1) {
    return(tibble())
  }
  
  # Remove the table header row
  game_rows <- game_rows[-1]
  
  games <- lapply(
    game_rows,
    function(row) {
      cells <- row %>%
        html_elements("td")
      
      if (length(cells) < 9) {
        return(NULL)
      }
      
      game_date_text <- cells[[1]] %>%
        html_text2()
      
      game_date <- suppressWarnings(
        as.Date(game_date_text)
      )
      
      # Keep every valid dated game, regardless of year
      if (is.na(game_date)) {
        return(NULL)
      }
      
      opponent_link <- cells[[5]] %>%
        html_element("a")
      
      if (length(opponent_link) == 0) {
        return(NULL)
      }
      
      opponent_href <- opponent_link %>%
        html_attr("href")
      
      if (
        is.na(opponent_href) ||
        opponent_href == ""
      ) {
        return(NULL)
      }
      
      opponent_id <- opponent_href %>%
        str_extract("\\d+(?=\\.html$)")
      
      if (is.na(opponent_id)) {
        return(NULL)
      }
      
      kifu_links <- cells[[9]] %>%
        html_elements("a") %>%
        html_attr("href")
      
      kifu_links <- kifu_links[
        !is.na(kifu_links) &
          kifu_links != ""
      ]
      
      tibble(
        player_id = player_id,
        player_name = player_name,
        date = game_date,
        player_rating = suppressWarnings(
          cells[[2]] %>%
            html_text2() %>%
            as.integer()
        ),
        color = cells[[3]] %>%
          html_text2(),
        result = cells[[4]] %>%
          html_text2(),
        opponent_id = opponent_id,
        opponent_name = opponent_link %>%
          html_text2(),
        opponent_rating = suppressWarnings(
          cells[[6]] %>%
            html_text2() %>%
            as.integer()
        ),
        opponent_sex = cells[[7]] %>%
          html_text2(),
        kifu_urls = if (
          length(kifu_links) > 0
        ) {
          paste(
            url_absolute(
              kifu_links,
              player_url
            ),
            collapse = " | "
          )
        } else {
          NA_character_
        },
        profile_url = player_url
      )
    }
  ) %>%
    bind_rows()
  
  games
}

# -----------------------------
# Scrape all discovered profiles
# -----------------------------
all_results <- vector(
  mode = "list",
  length = length(player_urls)
)

failed_profiles <- tibble(
  profile_url = character(),
  error = character()
)

for (i in seq_along(player_urls)) {
  current_url <- player_urls[i]
  
  cat(
    "Scraping profile",
    i,
    "of",
    length(player_urls),
    ":",
    current_url,
    "\n"
  )
  
  all_results[[i]] <- tryCatch(
    parse_player_profile(
      current_url
    ),
    error = function(e) {
      error_message <- conditionMessage(e)
      
      warning(
        "Failed profile ",
        current_url,
        ": ",
        error_message
      )
      
      failed_profiles <<- bind_rows(
        failed_profiles,
        tibble(
          profile_url = current_url,
          error = error_message
        )
      )
      
      tibble()
    }
  )
  
  # Save progress every 25 profiles
  if (
    i %% 25 == 0 ||
    i == length(player_urls)
  ) {
    progress_games <- bind_rows(
      all_results[seq_len(i)]
    )
    
    write_csv(
      progress_games,
      output_file
    )
    
    write_csv(
      failed_profiles,
      failed_file
    )
    
    cat(
      "Saved progress:",
      i,
      "profiles,",
      nrow(progress_games),
      "rows\n"
    )
  }
  
  # Guarantee at least one second between requests,
  # with a small random variation
  Sys.sleep(
    1 + runif(
      1,
      min = 0,
      max = 0.5
    )
  )
}

# -----------------------------
# Final output
# -----------------------------
goratings_games_all_raw <- bind_rows(
  all_results
)

write_csv(
  goratings_games_all_raw,
  output_file
)

write_csv(
  failed_profiles,
  failed_file
)

cat("\nScrape complete.\n")
cat(
  "Profiles attempted:",
  length(player_urls),
  "\n"
)
cat(
  "Complete profile-game rows:",
  nrow(goratings_games_all_raw),
  "\n"
)
cat(
  "Earliest dated game:",
  format(
    min(
      goratings_games_all_raw$date,
      na.rm = TRUE
    ),
    "%Y-%m-%d"
  ),
  "\n"
)
cat(
  "Latest dated game:",
  format(
    max(
      goratings_games_all_raw$date,
      na.rm = TRUE
    ),
    "%Y-%m-%d"
  ),
  "\n"
)
cat(
  "Failed profiles:",
  nrow(failed_profiles),
  "\n"
)
cat(
  "Output file:",
  output_file,
  "\n"
)
cat(
  "Failed-profile file:",
  failed_file,
  "\n"
)

