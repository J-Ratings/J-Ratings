# ============================================================
# Incrementally scrape GoRatings player profiles
#
# Weekly mode:
#   - refreshes homepage metadata
#   - checks newly listed players
#   - checks players whose homepage Elo changed
#   - checks players with a known game in the last 120 days
#
# Full mode:
#   - checks every player listed on the GoRatings homepage
#
# GitHub Actions sets:
#   GORATINGS_SCRAPE_MODE=weekly
#   GORATINGS_SCRAPE_MODE=full
#
# Persistent output:
#   Go/pipeline_data/goratings/
#     goratings_games_2026_observations.csv
#
# Other outputs:
#   goratings_players_homepage.csv
#   goratings_failed_profiles.csv
#   goratings_scrape_log.csv
#
# Only games dated 2026-01-01 onwards are retained.
# ============================================================

library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(purrr)

options(stringsAsFactors = FALSE)

# -----------------------------
# Settings
# -----------------------------
GORATINGS_BASE_URL <- "https://www.goratings.org/en"

LIVE_START_DATE <- as.Date("2026-01-01")

ACTIVE_WINDOW_DAYS <- 60L

REQUEST_DELAY_MIN <- 0.8
REQUEST_DELAY_MAX <- 1.2

SAVE_EVERY <- 25L

BATCH_SIZE <- 50L
BATCH_PAUSE_SECONDS <- 10

SCRAPE_MODE <- tolower(
  trimws(
    Sys.getenv(
      "GORATINGS_SCRAPE_MODE",
      unset = "weekly"
    )
  )
)

if (!SCRAPE_MODE %in% c("weekly", "full")) {
  warning(
    "Unknown GORATINGS_SCRAPE_MODE='",
    SCRAPE_MODE,
    "'. Using weekly mode."
  )
  
  SCRAPE_MODE <- "weekly"
}

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

processed_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed"
)

dir.create(
  goratings_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

homepage_players_file <- file.path(
  goratings_dir,
  "goratings_players_homepage.csv"
)

observations_file <- file.path(
  goratings_dir,
  "goratings_games_2026_observations.csv"
)

clean_games_file <- file.path(
  processed_dir,
  "goratings_games_2026.csv"
)

failed_file <- file.path(
  goratings_dir,
  "goratings_failed_profiles.csv"
)

scrape_log_file <- file.path(
  goratings_dir,
  "goratings_scrape_log.csv"
)

cat("GoRatings scrape mode:", SCRAPE_MODE, "\n")
cat("Output folder:", goratings_dir, "\n")
cat("Live games start:", format(LIVE_START_DATE), "\n")
cat("Weekly activity window:", ACTIVE_WINDOW_DAYS, "days\n")

# -----------------------------
# Helpers
# -----------------------------
clean_text <- function(x) {
  x <- as.character(x)
  
  x <- str_replace_all(
    x,
    "\u00a0",
    " "
  )
  
  x <- str_squish(x)
  
  x[x == ""] <- NA_character_
  
  x
}

clean_id <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  x
}

safe_integer <- function(x) {
  suppressWarnings(
    as.integer(
      str_extract(
        as.character(x),
        "-?\\d+"
      )
    )
  )
}

empty_observations <- function() {
  tibble(
    player_id = character(),
    player_name = character(),
    date = as.Date(character()),
    player_rating = integer(),
    colour = character(),
    result = character(),
    opponent_id = character(),
    opponent_name = character(),
    opponent_rating = integer(),
    opponent_sex = character(),
    kifu_urls = character(),
    profile_url = character(),
    scraped_at = character()
  )
}

read_existing_observations <- function(path) {
  if (!file.exists(path)) {
    return(
      empty_observations()
    )
  }
  
  existing <- read_csv(
    path,
    show_col_types = FALSE,
    locale = locale(
      encoding = "UTF-8"
    ),
    col_types = cols(
      player_id = col_character(),
      opponent_id = col_character(),
      .default = col_guess()
    )
  )
  
  required <- names(
    empty_observations()
  )
  
  for (column_name in setdiff(required, names(existing))) {
    existing[[column_name]] <- NA
  }
  
  existing %>%
    transmute(
      player_id = clean_id(player_id),
      player_name = clean_text(player_name),
      date = as.Date(date),
      player_rating =
        suppressWarnings(
          as.integer(player_rating)
        ),
      colour = clean_text(colour),
      result = clean_text(result),
      opponent_id = clean_id(opponent_id),
      opponent_name = clean_text(opponent_name),
      opponent_rating =
        suppressWarnings(
          as.integer(opponent_rating)
        ),
      opponent_sex = clean_text(opponent_sex),
      kifu_urls = clean_text(kifu_urls),
      profile_url = clean_text(profile_url),
      scraped_at = clean_text(scraped_at)
    ) %>%
    filter(
      !is.na(player_id),
      !is.na(opponent_id),
      !is.na(date),
      date >= LIVE_START_DATE
    )
}

observation_key <- function(
    player_id,
    opponent_id,
    date,
    colour,
    result,
    player_rating,
    opponent_rating,
    kifu_urls
) {
  paste(
    player_id,
    opponent_id,
    format(
      as.Date(date),
      "%Y-%m-%d"
    ),
    str_to_lower(
      clean_text(colour)
    ),
    str_to_lower(
      clean_text(result)
    ),
    ifelse(
      is.na(player_rating),
      "",
      player_rating
    ),
    ifelse(
      is.na(opponent_rating),
      "",
      opponent_rating
    ),
    ifelse(
      is.na(kifu_urls),
      "",
      kifu_urls
    ),
    sep = "|"
  )
}

deduplicate_observations <- function(df) {
  if (nrow(df) == 0L) {
    return(
      empty_observations()
    )
  }
  
  df %>%
    mutate(
      observation_key = observation_key(
        player_id,
        opponent_id,
        date,
        colour,
        result,
        player_rating,
        opponent_rating,
        kifu_urls
      )
    ) %>%
    arrange(
      date,
      player_id,
      opponent_id,
      observation_key,
      scraped_at
    ) %>%
    distinct(
      observation_key,
      .keep_all = TRUE
    ) %>%
    select(
      -observation_key
    ) %>%
    arrange(
      date,
      player_id,
      opponent_id
    )
}

read_previous_homepage <- function(path) {
  if (!file.exists(path)) {
    return(
      tibble(
        player_id = character(),
        previous_homepage_elo = integer(),
        previous_homepage_rank = integer()
      )
    )
  }
  
  read_csv(
    path,
    show_col_types = FALSE,
    locale = locale(
      encoding = "UTF-8"
    ),
    col_types = cols(
      player_id = col_character(),
      .default = col_guess()
    )
  ) %>%
    transmute(
      player_id = clean_id(player_id),
      previous_homepage_elo =
        suppressWarnings(
          as.integer(homepage_elo)
        ),
      previous_homepage_rank =
        suppressWarnings(
          as.integer(homepage_rank)
        )
    ) %>%
    filter(
      !is.na(player_id)
    ) %>%
    distinct(
      player_id,
      .keep_all = TRUE
    )
}

read_last_known_dates <- function(
    observations_path,
    canonical_games_path
) {
  result <- tibble(
    player_id = character(),
    last_known_game_date =
      as.Date(character())
  )
  
  if (file.exists(observations_path)) {
    observations <- read_existing_observations(
      observations_path
    )
    
    if (nrow(observations) > 0L) {
      observation_dates <- observations %>%
        group_by(player_id) %>%
        summarise(
          last_known_game_date = max(
            date,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      result <- bind_rows(
        result,
        observation_dates
      )
    }
  }
  
  if (file.exists(canonical_games_path)) {
    canonical <- read_csv(
      canonical_games_path,
      show_col_types = FALSE,
      locale = locale(
        encoding = "UTF-8"
      ),
      col_types = cols(
        black_id = col_character(),
        white_id = col_character(),
        black_goratings_id =
          col_character(),
        white_goratings_id =
          col_character(),
        .default = col_guess()
      )
    )
    
    date_column <- intersect(
      c(
        "date",
        "Date"
      ),
      names(canonical)
    )
    
    black_column <- intersect(
      c(
        "black_id",
        "black_goratings_id",
        "BlackID"
      ),
      names(canonical)
    )
    
    white_column <- intersect(
      c(
        "white_id",
        "white_goratings_id",
        "WhiteID"
      ),
      names(canonical)
    )
    
    if (
      length(date_column) > 0L &&
      length(black_column) > 0L &&
      length(white_column) > 0L
    ) {
      canonical_dates <- bind_rows(
        tibble(
          player_id =
            clean_id(
              canonical[[
                black_column[[1]]
              ]]
            ),
          last_known_game_date =
            as.Date(
              canonical[[
                date_column[[1]]
              ]]
            )
        ),
        tibble(
          player_id =
            clean_id(
              canonical[[
                white_column[[1]]
              ]]
            ),
          last_known_game_date =
            as.Date(
              canonical[[
                date_column[[1]]
              ]]
            )
        )
      ) %>%
        filter(
          !is.na(player_id),
          !is.na(last_known_game_date)
        ) %>%
        group_by(player_id) %>%
        summarise(
          last_known_game_date = max(
            last_known_game_date
          ),
          .groups = "drop"
        )
      
      result <- bind_rows(
        result,
        canonical_dates
      )
    }
  }
  
  result %>%
    filter(
      !is.na(player_id),
      !is.na(last_known_game_date)
    ) %>%
    group_by(player_id) %>%
    summarise(
      last_known_game_date = max(
        last_known_game_date
      ),
      .groups = "drop"
    )
}

# -----------------------------
# Save previous homepage state
# before overwriting it
# -----------------------------
previous_homepage <- read_previous_homepage(
  homepage_players_file
)

existing_observations <- read_existing_observations(
  observations_file
)

last_known_dates <- read_last_known_dates(
  observations_file,
  clean_games_file
)

cat(
  "Existing 2026 observations:",
  nrow(existing_observations),
  "\n"
)

# -----------------------------
# Read GoRatings homepage
# -----------------------------
homepage <- read_html(
  GORATINGS_BASE_URL
)

page_title <- homepage %>%
  html_element("title") %>%
  html_text2()

cat("GoRatings page title:", page_title, "\n")

homepage_tables <- homepage %>%
  html_elements("table")

rating_table_index <- which(
  vapply(
    homepage_tables,
    function(table_node) {
      headings <- table_node %>%
        html_elements("th") %>%
        html_text2()
      
      all(
        c(
          "Name",
          "Elo"
        ) %in% headings
      )
    },
    logical(1)
  )
)

if (length(rating_table_index) == 0L) {
  stop(
    "Could not find the GoRatings homepage rating table."
  )
}

rating_table_node <- homepage_tables[[
  rating_table_index[[1]]
]]

rating_rows <- rating_table_node %>%
  html_elements("tbody tr")

if (length(rating_rows) == 0L) {
  rating_rows <- rating_table_node %>%
    html_elements("tr")
  
  rating_rows <- rating_rows[
    vapply(
      rating_rows,
      function(row) {
        length(
          row %>%
            html_elements("td")
        ) > 0L
      },
      logical(1)
    )
  ]
}

extract_homepage_player <- function(row) {
  cells <- row %>%
    html_elements("td")
  
  if (length(cells) < 5L) {
    return(NULL)
  }
  
  player_link <- cells[[2]] %>%
    html_element("a")
  
  if (length(player_link) == 0L) {
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
    str_trim()
  
  flag_image <- cells[[4]] %>%
    html_element("img")
  
  flag_alt <- if (
    length(flag_image) > 0L
  ) {
    flag_image %>%
      html_attr("alt")
  } else {
    NA_character_
  }
  
  flag_src <- if (
    length(flag_image) > 0L
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
      "[a-z]{2}(?=\\.(svg|png|gif|jpg|jpeg)$)"
    )
  
  tibble(
    player_id = player_id,
    player_name =
      player_link %>%
      html_text2() %>%
      clean_text(),
    gender_symbol = gender_symbol,
    gender = case_when(
      gender_symbol == "♂" ~
        "male",
      gender_symbol == "♀" ~
        "female",
      TRUE ~
        NA_character_
    ),
    flag = coalesce(
      flag_from_alt,
      flag_from_src
    ),
    homepage_rank =
      safe_integer(
        cells[[1]] %>%
          html_text2()
      ),
    homepage_elo =
      safe_integer(
        cells[[5]] %>%
          html_text2()
      ),
    profile_url = profile_url
  )
}

homepage_players <- map_dfr(
  rating_rows,
  extract_homepage_player
) %>%
  mutate(
    player_id =
      clean_id(player_id),
    player_name =
      clean_text(player_name)
  ) %>%
  filter(
    !is.na(player_id),
    !is.na(profile_url)
  ) %>%
  distinct(
    player_id,
    .keep_all = TRUE
  ) %>%
  arrange(
    homepage_rank,
    player_name
  )

if (nrow(homepage_players) == 0L) {
  stop(
    "No players were extracted from the homepage."
  )
}

cat(
  "Homepage players found:",
  nrow(homepage_players),
  "\n"
)

# -----------------------------
# Decide which profiles to check
# -----------------------------
today <- Sys.Date()

active_cutoff <- today -
  ACTIVE_WINDOW_DAYS

profile_selection <- homepage_players %>%
  left_join(
    previous_homepage,
    by = "player_id"
  ) %>%
  left_join(
    last_known_dates,
    by = "player_id"
  ) %>%
  mutate(
    is_new_player =
      is.na(previous_homepage_elo),
    
    recently_active =
      !is.na(last_known_game_date) &
      last_known_game_date >=
      active_cutoff,
    
    scrape_reason = case_when(
      SCRAPE_MODE == "full" ~
        "full refresh",
      
      is_new_player ~
        "new player",
      
      recently_active ~
        "recently active",
      
      TRUE ~
        NA_character_
    ),
    
    should_scrape =
      SCRAPE_MODE == "full" |
      is_new_player |
      recently_active
  )

profiles_to_scrape <- profile_selection %>%
  filter(
    should_scrape
  ) %>%
  arrange(
    homepage_rank,
    player_name
  )

cat(
  "Profiles selected:",
  nrow(profiles_to_scrape),
  "of",
  nrow(homepage_players),
  "\n"
)

cat("\nSelection reasons:\n")

print(
  profiles_to_scrape %>%
    count(
      scrape_reason,
      sort = TRUE
    )
)

# Write homepage only after comparing it with
# the previous saved version.
write_csv(
  homepage_players,
  homepage_players_file
)

cat(
  "\nHomepage metadata written:",
  homepage_players_file,
  "\n"
)

# -----------------------------
# Parse one profile
# -----------------------------
parse_player_profile <- function(
    player_url,
    expected_player_id = NA_character_,
    expected_player_name = NA_character_
) {
  profile <- read_html(
    player_url
  )
  
  player_id <- player_url %>%
    str_extract(
      "\\d+(?=\\.html$)"
    )
  
  if (
    is.na(player_id) &&
    !is.na(expected_player_id)
  ) {
    player_id <- expected_player_id
  }
  
  player_name_node <- profile %>%
    html_element("h1")
  
  player_name <- if (
    length(player_name_node) > 0L
  ) {
    player_name_node %>%
      html_text2() %>%
      clean_text()
  } else {
    clean_text(
      expected_player_name
    )
  }
  
  tables <- profile %>%
    html_elements("table")
  
  if (length(tables) < 2L) {
    warning(
      "No game table found for: ",
      player_url
    )
    
    return(
      empty_observations()
    )
  }
  
  game_rows <- tables[[2]] %>%
    html_elements("tr")
  
  game_rows <- game_rows[
    vapply(
      game_rows,
      function(row) {
        length(
          row %>%
            html_elements("td")
        ) >= 9L
      },
      logical(1)
    )
  ]
  
  if (length(game_rows) == 0L) {
    return(
      empty_observations()
    )
  }
  
  scraped_at <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  
  games <- map_dfr(
    game_rows,
    function(row) {
      cells <- row %>%
        html_elements("td")
      
      game_date_text <- cells[[1]] %>%
        html_text2() %>%
        clean_text()
      
      game_date <- suppressWarnings(
        as.Date(game_date_text)
      )
      
      if (
        is.na(game_date) ||
        game_date <
        LIVE_START_DATE
      ) {
        return(NULL)
      }
      
      opponent_link <- cells[[5]] %>%
        html_element("a")
      
      if (length(opponent_link) == 0L) {
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
      
      opponent_url <- url_absolute(
        opponent_href,
        player_url
      )
      
      opponent_id <- opponent_url %>%
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
      
      kifu_urls <- if (
        length(kifu_links) > 0L
      ) {
        paste(
          unique(
            url_absolute(
              kifu_links,
              player_url
            )
          ),
          collapse = " | "
        )
      } else {
        NA_character_
      }
      
      tibble(
        player_id =
          clean_id(player_id),
        player_name =
          clean_text(player_name),
        date = game_date,
        player_rating =
          safe_integer(
            cells[[2]] %>%
              html_text2()
          ),
        colour =
          cells[[3]] %>%
          html_text2() %>%
          clean_text(),
        result =
          cells[[4]] %>%
          html_text2() %>%
          clean_text(),
        opponent_id =
          clean_id(opponent_id),
        opponent_name =
          opponent_link %>%
          html_text2() %>%
          clean_text(),
        opponent_rating =
          safe_integer(
            cells[[6]] %>%
              html_text2()
          ),
        opponent_sex =
          cells[[7]] %>%
          html_text2() %>%
          clean_text(),
        kifu_urls =
          clean_text(kifu_urls),
        profile_url =
          clean_text(player_url),
        scraped_at =
          scraped_at
      )
    }
  )
  
  games %>%
    filter(
      !is.na(player_id),
      !is.na(opponent_id),
      !is.na(date),
      date >= LIVE_START_DATE
    )
}

# -----------------------------
# Scrape selected profiles
# -----------------------------
failed_profiles <- tibble(
  player_id = character(),
  player_name = character(),
  profile_url = character(),
  error = character(),
  failed_at = character()
)

scraped_results <- vector(
  mode = "list",
  length = nrow(profiles_to_scrape)
)

if (nrow(profiles_to_scrape) > 0L) {
  for (
    i in seq_len(
      nrow(profiles_to_scrape)
    )
  ) {
    player_row <- profiles_to_scrape[
      i,
      ,
      drop = FALSE
    ]
    
    cat(
      "\nScraping profile",
      i,
      "of",
      nrow(profiles_to_scrape),
      "-",
      player_row$player_name,
      "(",
      player_row$player_id,
      ")",
      "-",
      player_row$scrape_reason,
      "\n"
    )
    
    scraped_results[[i]] <- tryCatch(
      parse_player_profile(
        player_url =
          player_row$profile_url,
        expected_player_id =
          player_row$player_id,
        expected_player_name =
          player_row$player_name
      ),
      error = function(e) {
        error_message <-
          conditionMessage(e)
        
        warning(
          "Failed profile ",
          player_row$profile_url,
          ": ",
          error_message
        )
        
        failed_profiles <<- bind_rows(
          failed_profiles,
          tibble(
            player_id =
              player_row$player_id,
            player_name =
              player_row$player_name,
            profile_url =
              player_row$profile_url,
            error =
              error_message,
            failed_at = format(
              Sys.time(),
              "%Y-%m-%dT%H:%M:%SZ",
              tz = "UTC"
            )
          )
        )
        
        empty_observations()
      }
    )
    
    if (
      i %% SAVE_EVERY == 0L ||
      i ==
      nrow(profiles_to_scrape)
    ) {
      progress_new <- bind_rows(
        scraped_results[
          seq_len(i)
        ]
      )
      
      progress_all <- bind_rows(
        existing_observations,
        progress_new
      ) %>%
        deduplicate_observations()
      
      write_csv(
        progress_all,
        observations_file
      )
      
      write_csv(
        failed_profiles,
        failed_file
      )
      
      cat(
        "Saved progress:",
        i,
        "profiles;",
        nrow(progress_all),
        "persistent observations\n"
      )
    }
    
    if (
      i %% BATCH_SIZE == 0L &&
      i < nrow(profiles_to_scrape)
    ) {
      cat(
        "Pausing for",
        BATCH_PAUSE_SECONDS,
        "seconds after",
        i,
        "profiles\n"
      )
      
      Sys.sleep(
        BATCH_PAUSE_SECONDS
      )
    }
  }
}

new_observations <- bind_rows(
  scraped_results
)

all_observations <- bind_rows(
  existing_observations,
  new_observations
) %>%
  deduplicate_observations()

write_csv(
  all_observations,
  observations_file
)

write_csv(
  failed_profiles,
  failed_file
)

# -----------------------------
# Scrape log
# -----------------------------
newest_observation_date <- if (
  nrow(all_observations) > 0L
) {
  max(
    all_observations$date,
    na.rm = TRUE
  )
} else {
  as.Date(NA)
}

scrape_log_row <- tibble(
  run_time_utc = format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  ),
  mode = SCRAPE_MODE,
  homepage_players =
    nrow(homepage_players),
  profiles_selected =
    nrow(profiles_to_scrape),
  profiles_failed =
    nrow(failed_profiles),
  new_rows_scraped =
    nrow(new_observations),
  persistent_observations =
    nrow(all_observations),
  latest_game_date =
    as.character(
      newest_observation_date
    )
)

existing_log <- if (
  file.exists(scrape_log_file)
) {
  read_csv(
    scrape_log_file,
    show_col_types = FALSE
  )
} else {
  tibble()
}

write_csv(
  bind_rows(
    existing_log,
    scrape_log_row
  ),
  scrape_log_file
)

# -----------------------------
# Summary
# -----------------------------
cat("\nGoRatings scrape complete.\n")
cat("Mode:", SCRAPE_MODE, "\n")
cat(
  "Homepage players:",
  nrow(homepage_players),
  "\n"
)
cat(
  "Profiles checked:",
  nrow(profiles_to_scrape),
  "\n"
)
cat(
  "Rows returned this run:",
  nrow(new_observations),
  "\n"
)
cat(
  "Persistent 2026 observations:",
  nrow(all_observations),
  "\n"
)

if (nrow(all_observations) > 0L) {
  cat(
    "Observation date range:",
    format(
      min(
        all_observations$date,
        na.rm = TRUE
      ),
      "%Y-%m-%d"
    ),
    "to",
    format(
      max(
        all_observations$date,
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
  "Observation file:",
  observations_file,
  "\n"
)