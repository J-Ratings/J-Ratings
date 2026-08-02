# ============================================================
# Scrape GoRatings homepage metadata and optional full profiles
#
# Always:
#   - reads the GoRatings homepage
#   - extracts player ID, name, gender, country flag, rank and Elo
#   - writes goratings_players_homepage.csv
#
# Optionally:
#   - scrapes every active player profile
#   - retains every dated game shown on each profile
#   - writes goratings_games_all_raw.csv
#
# Set SCRAPE_PROFILE_HISTORIES to:
#   FALSE = homepage metadata only
#   TRUE  = homepage metadata plus full profile-history scrape
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

homepage_players_file <- file.path(
  goratings_dir,
  "goratings_players_homepage.csv"
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

# FALSE refreshes only the homepage player list.
# TRUE also performs the full profile-history scrape.
SCRAPE_PROFILE_HISTORIES <- FALSE

cat(
  "GoRatings output folder:",
  goratings_dir,
  "\n"
)

cat(
  "Profile-history scrape enabled:",
  SCRAPE_PROFILE_HISTORIES,
  "\n"
)

# -----------------------------
# Read homepage
# -----------------------------
homepage <- read_html(
  GORATINGS_BASE_URL
)

page_title <- homepage %>%
  html_element("title") %>%
  html_text2()

cat(
  "GoRatings page title:",
  page_title,
  "\n"
)

# -----------------------------
# Extract player metadata from homepage rating table
# -----------------------------
homepage_tables <- homepage %>%
  html_elements("table")

rating_table_index <- which(
  vapply(
    homepage_tables,
    function(table_node) {
      headings <- table_node %>%
        html_elements("th") %>%
        html_text2() %>%
        str_squish()
      
      all(
        c("Name", "Elo") %in%
          headings
      )
    },
    logical(1)
  )
)

if (length(rating_table_index) == 0) {
  stop(
    "Could not find the GoRatings homepage rating table."
  )
}

rating_table_node <- homepage_tables[[
  rating_table_index[[1]]
]]

rating_rows <- rating_table_node %>%
  html_elements("tbody tr")

if (length(rating_rows) == 0) {
  rating_rows <- rating_table_node %>%
    html_elements("tr")
  
  rating_rows <- rating_rows[
    vapply(
      rating_rows,
      function(row) {
        length(
          row %>%
            html_elements("td")
        ) > 0
      },
      logical(1)
    )
  ]
}

extract_homepage_player <- function(row) {
  cells <- row %>%
    html_elements("td")
  
  if (length(cells) < 5) {
    return(NULL)
  }
  
  player_link <- cells[[2]] %>%
    html_element("a")
  
  if (length(player_link) == 0) {
    return(NULL)
  }
  
  profile_href <- player_link %>%
    html_attr("href")
  
  if (
    is.na(profile_href) ||
    profile_href == ""
  ) {
    return(NULL)
  }
  
  profile_url <- url_absolute(
    profile_href,
    GORATINGS_BASE_URL
  )
  
  player_id <- profile_url %>%
    str_extract(
      "\\d+(?=\\.html$)"
    )
  
  if (is.na(player_id)) {
    return(NULL)
  }
  
  gender_symbol <- cells[[3]] %>%
    html_text2() %>%
    str_squish()
  
  flag_image <- cells[[4]] %>%
    html_element("img")
  
  flag_alt <- if (
    length(flag_image) > 0
  ) {
    flag_image %>%
      html_attr("alt")
  } else {
    NA_character_
  }
  
  flag_src <- if (
    length(flag_image) > 0
  ) {
    flag_image %>%
      html_attr("src")
  } else {
    NA_character_
  }
  
  flag_from_alt <- flag_alt %>%
    str_to_lower() %>%
    str_extract(
      "^[a-z]{2}(?=\\s+flag$)"
    )
  
  flag_from_src <- flag_src %>%
    str_to_lower() %>%
    str_extract(
      "[a-z]{2}(?=\\.(svg|png|gif|jpg|jpeg)(?:\\?.*)?$)"
    )
  
  flag <- coalesce(
    flag_from_alt,
    flag_from_src
  )
  
  tibble(
    player_id = as.character(
      player_id
    ),
    player_name = player_link %>%
      html_text2() %>%
      str_squish(),
    gender_symbol = gender_symbol,
    gender = case_when(
      gender_symbol == "♂" ~ "male",
      gender_symbol == "♀" ~ "female",
      TRUE ~ NA_character_
    ),
    flag = flag,
    homepage_rank = suppressWarnings(
      cells[[1]] %>%
        html_text2() %>%
        str_extract("\\d+") %>%
        as.integer()
    ),
    homepage_elo = suppressWarnings(
      cells[[5]] %>%
        html_text2() %>%
        str_extract("-?\\d+") %>%
        as.integer()
    ),
    profile_url = profile_url
  )
}

homepage_players <- lapply(
  rating_rows,
  extract_homepage_player
) %>%
  bind_rows() %>%
  distinct(
    player_id,
    .keep_all = TRUE
  ) %>%
  arrange(
    homepage_rank,
    player_name
  )

if (nrow(homepage_players) == 0) {
  stop(
    "No players were extracted from the homepage rating table."
  )
}

write_csv(
  homepage_players,
  homepage_players_file
)

cat(
  "Homepage players written:",
  nrow(homepage_players),
  "\n"
)

cat(
  "Homepage players with flags:",
  sum(
    !is.na(homepage_players$flag) &
      homepage_players$flag != ""
  ),
  "\n"
)

cat(
  "Homepage players with gender:",
  sum(
    !is.na(homepage_players$gender) &
      homepage_players$gender != ""
  ),
  "\n"
)

cat(
  "Homepage player file:",
  homepage_players_file,
  "\n"
)

# -----------------------------
# Stop here for homepage-only refresh
# -----------------------------
if (!SCRAPE_PROFILE_HISTORIES) {
  cat(
    "\nHomepage refresh complete. ",
    "Full profile-history scrape skipped.\n",
    sep = ""
  )
} else {
  
  # -----------------------------
  # Extract active player links
  # -----------------------------
  player_links <- homepage %>%
    html_elements(
      'a[href*="/players/"]'
    ) %>%
    html_attr("href") %>%
    unique()
  
  player_urls <- url_absolute(
    player_links,
    GORATINGS_BASE_URL
  )
  
  player_urls <- player_urls[
    !is.na(player_urls)
  ]
  
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
    stop(
      "No valid GoRatings player profile URLs were found."
    )
  }
  
  # -----------------------------
  # Parse one complete player profile
  # -----------------------------
  parse_player_profile <- function(
    player_url
  ) {
    profile <- read_html(
      player_url
    )
    
    player_id <- player_url %>%
      str_extract(
        "\\d+(?=\\.html$)"
      )
    
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
      html_text2() %>%
      str_squish()
    
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
          str_extract(
            "\\d+(?=\\.html$)"
          )
        
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
          player_id = as.character(
            player_id
          ),
          player_name = player_name,
          date = game_date,
          player_rating = suppressWarnings(
            cells[[2]] %>%
              html_text2() %>%
              as.integer()
          ),
          color = cells[[3]] %>%
            html_text2() %>%
            str_squish(),
          result = cells[[4]] %>%
            html_text2() %>%
            str_squish(),
          opponent_id = as.character(
            opponent_id
          ),
          opponent_name = opponent_link %>%
            html_text2() %>%
            str_squish(),
          opponent_rating = suppressWarnings(
            cells[[6]] %>%
              html_text2() %>%
              as.integer()
          ),
          opponent_sex = cells[[7]] %>%
            html_text2() %>%
            str_squish(),
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
    current_url <- player_urls[[i]]
    
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
    
    if (
      i %% 25 == 0 ||
      i == length(player_urls)
    ) {
      progress_games <- bind_rows(
        all_results[
          seq_len(i)
        ]
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
    
    Sys.sleep(
      1 + runif(
        1,
        min = 0,
        max = 0.5
      )
    )
  }
  
  # -----------------------------
  # Final full-history output
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
  
  if (nrow(goratings_games_all_raw) > 0) {
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
  }
  
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
}