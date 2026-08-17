library(httr)
library(jsonlite)
library(data.table)
library(tictoc)
library(beepr)

options(stringsAsFactors = FALSE)

tic("Snooker pipeline")

# ============================================================
# 00_refresh_lookups.R
#
# Refreshes player and event lookup files inside the Git repo.
#
# Monthly/default mode:
#   LOOKUP_MODE = "recent"
#   Refreshes recent/current seasons only.
#
# Full mode:
#   LOOKUP_MODE = "full"
#   Refreshes all seasons from 1974 to current/next season.
#
# Outputs:
#   Snooker/pipeline_data/Players/snooker_players_all_by_season.csv
#   Snooker/pipeline_data/Players/snooker_player_lookup_complete.csv
#   Snooker/pipeline_data/Events/events_YYYY.csv
#   Snooker/pipeline_data/Events/snooker_events_all_by_season.csv
#   Snooker/pipeline_data/Events/snooker_event_lookup_complete.csv
# ============================================================

# -----------------------------
# Paths
# -----------------------------
REPO_DIR <- Sys.getenv(
  "GITHUB_WORKSPACE",
  unset = "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"
)

SNOOKER_DIR <- file.path(REPO_DIR, "Snooker")
PIPELINE_DIR <- file.path(SNOOKER_DIR, "pipeline_data")

PLAYERS_DIR <- file.path(PIPELINE_DIR, "Players")
EVENTS_DIR <- file.path(PIPELINE_DIR, "Events")

dir.create(PLAYERS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(EVENTS_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# API settings
# -----------------------------
base_url <- "https://api.snooker.org/"

header_value <- Sys.getenv("SNOOKER_API_HEADER")
if (header_value == "") {
  stop("Missing SNOOKER_API_HEADER environment variable.")
}

# Hermund Årdalen confirmed the site-level IIS rate-limit window is two minutes.
# Use a conservative 70-second gap between normal requests.
REQUEST_PAUSE <- as.numeric(
  Sys.getenv("SNOOKER_REQUEST_PAUSE", unset = "70")
)

# After a 403, wait substantially longer before retrying.
FORBIDDEN_PAUSE <- as.numeric(
  Sys.getenv("SNOOKER_FORBIDDEN_PAUSE", unset = "180")
)

MAX_RETRIES <- 3L

# -----------------------------
# Lookup settings
# -----------------------------
LOOKUP_MODE <- Sys.getenv("LOOKUP_MODE", unset = "recent")
FULL_START_SEASON <- 1974L

players_all_file <- file.path(PLAYERS_DIR, "snooker_players_all_by_season.csv")
player_lookup_file <- file.path(PLAYERS_DIR, "snooker_player_lookup_complete.csv")

events_all_file <- file.path(EVENTS_DIR, "snooker_events_all_by_season.csv")
event_lookup_file <- file.path(EVENTS_DIR, "snooker_event_lookup_complete.csv")

# ============================================================
# Helpers
# ============================================================

get_current_snooker_season <- function(date = Sys.Date()) {
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  
  if (m >= 6L) {
    y
  } else {
    y - 1L
  }
}

get_snooker <- function(query, pause = REQUEST_PAUSE, retries = MAX_RETRIES) {
  url <- paste0(base_url, query)
  last_error <- NULL
  
  for (attempt in seq_len(retries)) {
    res <- tryCatch(
      GET(url, add_headers("X-Requested-By" = header_value)),
      error = function(e) e
    )
    
    if (inherits(res, "error")) {
      last_error <- conditionMessage(res)
      Sys.sleep(pause * attempt)
      next
    }
    
    status <- status_code(res)
    txt <- content(res, "text", encoding = "UTF-8")
    
    if (status == 403) {
      last_error <- "HTTP 403. Request refused by the API."
      
      if (attempt < retries) {
        cat("HTTP 403 received. Waiting", FORBIDDEN_PAUSE,
            "seconds before retrying...\n")
        Sys.sleep(FORBIDDEN_PAUSE)
        next
      }
      
      stop(last_error)
    }
    
    if (status != 200) {
      last_error <- paste("HTTP", status)
      Sys.sleep(pause * attempt)
      next
    }
    
    parsed <- tryCatch(
      fromJSON(txt, flatten = TRUE),
      error = function(e) e
    )
    
    if (!inherits(parsed, "error")) {
      Sys.sleep(pause)
      return(parsed)
    }
    
    last_error <- conditionMessage(parsed)
    Sys.sleep(pause * attempt)
  }
  
  stop(last_error)
}

clean_text <- function(x) {
  trimws(as.character(x))
}

make_name <- function(df) {
  if (!all(c("FirstName", "MiddleName", "LastName") %in% names(df))) {
    return(rep(NA_character_, nrow(df)))
  }
  
  first <- trimws(as.character(df$FirstName))
  middle <- trimws(as.character(df$MiddleName))
  last <- trimws(as.character(df$LastName))
  
  first[first %in% c("", "NA", "NULL")] <- NA_character_
  middle[middle %in% c("", "NA", "NULL")] <- NA_character_
  last[last %in% c("", "NA", "NULL")] <- NA_character_
  
  nationality <- if ("Nationality" %in% names(df)) {
    trimws(as.character(df$Nationality))
  } else {
    rep(NA_character_, nrow(df))
  }
  
  family_name_first <- nationality %in% c(
    "China",
    "Hong Kong",
    "Taiwan"
  )
  
  out <- character(nrow(df))
  
  for (i in seq_len(nrow(df))) {
    if (isTRUE(family_name_first[i])) {
      parts <- c(last[i], first[i], middle[i])
    } else {
      parts <- c(first[i], middle[i], last[i])
    }
    
    parts <- parts[!is.na(parts) & parts != ""]
    out[i] <- paste(parts, collapse = " ")
  }
  
  out
}

# ============================================================
# Seasons to refresh
# ============================================================

current_season <- get_current_snooker_season()

if (LOOKUP_MODE == "full") {
  seasons_to_refresh <- seq(current_season + 1L, FULL_START_SEASON, by = -1L)
} else if (LOOKUP_MODE == "recent") {
  seasons_to_refresh <- sort(unique(c(
    current_season - 1L,
    current_season,
    current_season + 1L
  )), decreasing = TRUE)
} else {
  stop("Invalid LOOKUP_MODE. Use 'recent' or 'full'.")
}

cat("Lookup mode:", LOOKUP_MODE, "\n")
cat("Current inferred snooker season:", current_season, "\n")
cat("Seasons to refresh:", paste(seasons_to_refresh, collapse = ", "), "\n")

# ============================================================
# Events
# ============================================================

existing_events_all <- if (file.exists(events_all_file)) {
  fread(events_all_file, encoding = "UTF-8")
} else {
  data.table()
}

events_new_list <- list()
event_row_num <- 1L

for (season in seasons_to_refresh) {
  cat(sprintf("\n===== Event season %s =====\n", season))
  
  dat <- tryCatch(
    get_snooker(paste0("?t=5&s=", season)),
    error = function(e) {
      cat("  -> failed:", conditionMessage(e), "\n")
      NULL
    }
  )
  
  if (!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0L) {
    dat <- as.data.table(dat)
    dat[, SeasonPulled := season]
    
    events_new_list[[event_row_num]] <- dat
    event_row_num <- event_row_num + 1L
    
    keep_event_cols <- intersect(
      c(
        "ID", "Name", "StartDate", "EndDate", "Sponsor", "Season",
        "Tour", "Type", "Num", "Venue", "City", "Country",
        "Discipline", "Main", "Sex", "AgeGroup", "TV",
        "WorldSnookerId", "NumCompetitors", "NumUpcoming",
        "NumActive", "NumResults", "SeasonPulled"
      ),
      names(dat)
    )
    
    fwrite(
      dat[, ..keep_event_cols],
      file.path(EVENTS_DIR, paste0("events_", season, ".csv"))
    )
    
    cat("  -> got", nrow(dat), "rows\n")
  } else {
    cat("  -> no rows returned\n")
  }
}

events_new <- if (length(events_new_list) > 0L) {
  rbindlist(events_new_list, use.names = TRUE, fill = TRUE)
} else {
  data.table()
}

if (nrow(events_new) > 0L) {
  if (nrow(existing_events_all) > 0L && "SeasonPulled" %in% names(existing_events_all)) {
    existing_events_all <- existing_events_all[
      !(SeasonPulled %in% unique(events_new$SeasonPulled))
    ]
  }
  
  date_cols <- c("StartDate", "EndDate")
  
  for (col in date_cols) {
    if (col %in% names(existing_events_all)) {
      existing_events_all[, (col) := as.character(get(col))]
    }
    if (col %in% names(events_new)) {
      events_new[, (col) := as.character(get(col))]
    }
  }
  
  events_all <- rbindlist(
    list(existing_events_all, events_new),
    use.names = TRUE,
    fill = TRUE
  )
  
  events_all <- unique(events_all)
  
  keep_event_cols <- intersect(
    c(
      "ID", "Name", "StartDate", "EndDate", "Sponsor", "Season",
      "Tour", "Type", "Num", "Venue", "City", "Country",
      "Discipline", "Main", "Sex", "AgeGroup", "TV",
      "WorldSnookerId", "NumCompetitors", "NumUpcoming",
      "NumActive", "NumResults", "SeasonPulled"
    ),
    names(events_all)
  )
  
  events_all <- events_all[, ..keep_event_cols]
  fwrite(events_all, events_all_file)
  
  event_lookup <- copy(events_all)
  event_lookup[, ID := clean_text(ID)]
  event_lookup <- event_lookup[ID != ""]
  setorder(event_lookup, ID, SeasonPulled)
  event_lookup <- event_lookup[!duplicated(ID)]
  
  fwrite(event_lookup, event_lookup_file)
  
  cat("\nWrote:", events_all_file, "\n")
  cat("Wrote:", event_lookup_file, "\n")
}

# ============================================================
# Players
# ============================================================

existing_players_all <- if (file.exists(players_all_file)) {
  fread(players_all_file, encoding = "UTF-8")
} else {
  data.table()
}

players_new_list <- list()
player_row_num <- 1L

for (season in seasons_to_refresh) {
  cat(sprintf("\n===== Player season %s =====\n", season))
  
  for (status_type in c("p", "a")) {
    cat(sprintf("Pulling t=10 season %s status %s\n", season, status_type))
    
    dat <- tryCatch(
      get_snooker(paste0("?t=10&st=", status_type, "&s=", season)),
      error = function(e) {
        cat("  -> failed:", conditionMessage(e), "\n")
        NULL
      }
    )
    
    if (!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0L) {
      dat <- as.data.table(dat)
      dat[, SeasonPulled := season]
      dat[, StatusTypePulled := status_type]
      
      players_new_list[[player_row_num]] <- dat
      player_row_num <- player_row_num + 1L
      
      cat("  -> got", nrow(dat), "rows\n")
    } else {
      cat("  -> no rows returned\n")
    }
  }
}

players_new <- if (length(players_new_list) > 0L) {
  rbindlist(players_new_list, use.names = TRUE, fill = TRUE)
} else {
  data.table()
}

if (nrow(players_new) > 0L) {
  if (nrow(existing_players_all) > 0L &&
      all(c("SeasonPulled", "StatusTypePulled") %in% names(existing_players_all))) {
    refresh_keys <- unique(players_new[, .(SeasonPulled, StatusTypePulled)])
    existing_players_all <- existing_players_all[
      !refresh_keys,
      on = .(SeasonPulled, StatusTypePulled)
    ]
  }
  
  player_date_cols <- c("Born", "Died")
  
  for (col in player_date_cols) {
    if (col %in% names(existing_players_all)) {
      existing_players_all[, (col) := as.character(get(col))]
    }
    if (col %in% names(players_new)) {
      players_new[, (col) := as.character(get(col))]
    }
  }
  
  players_all <- rbindlist(
    list(existing_players_all, players_new),
    use.names = TRUE,
    fill = TRUE
  )
  
  players_all <- unique(players_all)
  players_all[, Name := make_name(.SD)]
  
  keep_player_cols <- intersect(
    c(
      "ID", "Name", "FirstName", "MiddleName", "LastName", "Nationality",
      "Born", "Died", "Sex", "Type", "FirstSeasonAsPro", "LastSeasonAsPro",
      "SeasonPulled", "StatusTypePulled"
    ),
    names(players_all)
  )
  
  players_all <- players_all[, ..keep_player_cols]
  fwrite(players_all, players_all_file)
  
  player_lookup <- copy(players_all)
  player_lookup[, ID := clean_text(ID)]
  player_lookup <- player_lookup[ID != ""]
  setorder(player_lookup, ID, SeasonPulled, StatusTypePulled)
  player_lookup <- player_lookup[!duplicated(ID)]
  
  fwrite(player_lookup, player_lookup_file)
  
  cat("\nWrote:", players_all_file, "\n")
  cat("Wrote:", player_lookup_file, "\n")
  cat("Unique player IDs:", nrow(player_lookup), "\n")
}

cat("\nDone.\n")
toc()
beep()

