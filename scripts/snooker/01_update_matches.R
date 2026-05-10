library(httr)
library(jsonlite)
library(data.table)

options(stringsAsFactors = FALSE)

# ============================================================
# SNOOKER API MATCH REFRESH
#
# Purpose:
#   - Refresh selected event-level raw match CSVs from snooker.org
#   - Save/update season event list
#   - Rebuild one raw combined season CSV from all event-level CSVs
#
# Important:
#   - This script writes raw API files in Matches/
#   - It does not write Matches_Clean or Matches_Clean_Combined
#   - Cleaned combined files should be rebuilt after the cleaning step
#
# Output:
#   Events/events_YYYY.csv
#   Matches/matches_YYYY/event_<event_id>.csv
#   Matches/matches_YYYY_all.csv
#   Matches/latest_filled_match_YYYY.csv
# ============================================================

# -----------------------------
# Paths
# -----------------------------
ROOT_DIR <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Snooker"

MATCHES_DIR <- file.path(ROOT_DIR, "Matches")
EVENTS_DIR <- file.path(ROOT_DIR, "Events")

dir.create(MATCHES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(EVENTS_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# API settings
# -----------------------------
base_url <- "https://api.snooker.org/"

# For GitHub Actions later, use:
#   header_value <- Sys.getenv("SNOOKER_API_HEADER")
#
# For local use, this fallback keeps your existing behaviour.
header_value <- Sys.getenv("SNOOKER_API_HEADER", unset = "SamJRatings262")

if (header_value == "") {
  stop("Missing API header. Set SNOOKER_API_HEADER or hard-code header_value locally.")
}

REQUEST_PAUSE <- 8
MAX_RETRIES <- 3

# -----------------------------
# Refresh settings
# -----------------------------
# REFRESH_MODE options:
#   "all_season"        = refresh every event in the selected season(s)
#   "since_last_update" = refresh events with EndDate on/after LAST_UPDATE_DATE
#   "last_n_days"       = refresh events with EndDate in the last LAST_N_DAYS
#
# For monthly automation, "last_n_days" is safer than manually editing LAST_UPDATE_DATE.
REFRESH_MODE <- "last_n_days"
LAST_N_DAYS <- 35L

# -----------------------------
# Seasons
# -----------------------------
# snooker.org season year is not calendar year:
#   2025 = 2025/2026 season
#   2026 = 2026/2027 season
START_SEASON <- 2025
END_SEASON <- 2025

# ============================================================
# Helpers
# ============================================================

get_snooker <- function(query, pause = 8, retries = 3) {
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


first_existing_col <- function(dt, possible_cols) {
  found <- possible_cols[possible_cols %in% names(dt)]
  
  if (length(found) == 0L) {
    return(NULL)
  }
  
  found[1]
}


safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}


safe_as_date <- function(x) {
  suppressWarnings(as.Date(x))
}


clean_id <- function(x) {
  trimws(as.character(x))
}


select_events_to_refresh <- function(events,
                                     refresh_mode,
                                     last_update_date,
                                     last_n_days) {
  events <- as.data.frame(events, stringsAsFactors = FALSE)
  
  if (!"StartDate" %in% names(events)) events$StartDate <- NA_character_
  if (!"EndDate" %in% names(events)) events$EndDate <- NA_character_
  
  events$StartDate2 <- as.Date(events$StartDate)
  events$EndDate2 <- as.Date(events$EndDate)
  
  if (refresh_mode == "all_season") {
    out <- events
    cat("Refreshing all events in season.\n")
    
  } else if (refresh_mode == "since_last_update") {
    cutoff_date <- as.Date(last_update_date)
    
    out <- events[
      !is.na(events$EndDate2) & events$EndDate2 >= cutoff_date,
      ,
      drop = FALSE
    ]
    
    cat("Refreshing events ending on or after", format(cutoff_date, "%Y-%m-%d"), "\n")
    
  } else if (refresh_mode == "last_n_days") {
    cutoff_date <- Sys.Date() - as.integer(last_n_days)
    
    out <- events[
      !is.na(events$EndDate2) & events$EndDate2 >= cutoff_date,
      ,
      drop = FALSE
    ]
    
    cat(
      "Refreshing events ending in the last",
      as.integer(last_n_days),
      "days from",
      format(cutoff_date, "%Y-%m-%d"),
      "\n"
    )
    
  } else {
    stop("Invalid REFRESH_MODE. Use 'all_season', 'since_last_update', or 'last_n_days'.")
  }
  
  out
}


detect_filled_result_rows <- function(matches_all) {
  n <- nrow(matches_all)
  
  if (n == 0L) {
    return(logical(0))
  }
  
  has_result <- rep(FALSE, n)
  
  if (all(c("Score1", "Score2") %in% names(matches_all))) {
    score1 <- safe_as_numeric(matches_all$Score1)
    score2 <- safe_as_numeric(matches_all$Score2)
    
    has_result <- has_result | (!is.na(score1) & !is.na(score2))
  }
  
  possible_result_cols <- c(
    "WinnerID",
    "Winner",
    "Result",
    "FrameScores",
    "Frames",
    "Scores"
  )
  
  for (col in possible_result_cols) {
    if (col %in% names(matches_all)) {
      value <- as.character(matches_all[[col]])
      has_result <- has_result | (!is.na(value) & trimws(value) != "")
    }
  }
  
  possible_finished_cols <- c("Finished", "Completed", "Played")
  
  for (col in possible_finished_cols) {
    if (col %in% names(matches_all)) {
      value <- tolower(as.character(matches_all[[col]]))
      
      finished_yes <- value %in% c(
        "true", "t", "1", "yes", "y",
        "finished", "complete", "completed", "played"
      )
      
      has_result <- has_result | finished_yes
    }
  }
  
  has_result
}


rebuild_raw_combined_season <- function(season,
                                        season_match_dir,
                                        matches_dir) {
  files <- list.files(
    season_match_dir,
    full.names = TRUE,
    pattern = "^event_.*\\.csv$"
  )
  
  if (length(files) == 0L) {
    cat(sprintf("No raw event match CSV files exist for season %s\n", season))
    return(NULL)
  }
  
  cat("\n========== Rebuilding raw combined season file ==========\n")
  cat("Season:", season, "\n")
  cat("Raw event files:", length(files), "\n")
  
  matches_list <- lapply(files, function(f) {
    dt <- fread(f, encoding = "UTF-8")
    dt[, SourceFile := basename(f)]
    dt
  })
  
  matches_all <- rbindlist(
    matches_list,
    fill = TRUE,
    use.names = TRUE,
    ignore.attr = TRUE
  )
  
  rows_before <- nrow(matches_all)
  duplicate_id_values <- NA_integer_
  duplicate_rows_removed <- 0L
  
  id_col <- first_existing_col(matches_all, c("MatchID", "ID"))
  
  if (!is.null(id_col)) {
    matches_all[, MergeMatchID := clean_id(get(id_col))]
    
    duplicate_id_values <- matches_all[
      !is.na(MergeMatchID) & MergeMatchID != "",
      .N,
      by = MergeMatchID
    ][N > 1L, .N]
    
    with_id <- matches_all[!is.na(MergeMatchID) & MergeMatchID != ""]
    without_id <- matches_all[is.na(MergeMatchID) | MergeMatchID == ""]
    
    # Keep first version after stable ordering.
    setorder(with_id, MergeMatchID, SourceFile)
    with_id <- with_id[!duplicated(MergeMatchID)]
    
    matches_all <- rbindlist(
      list(with_id, without_id),
      use.names = TRUE,
      fill = TRUE
    )
    
    matches_all[, MergeMatchID := NULL]
    
    duplicate_rows_removed <- rows_before - nrow(matches_all)
    
    cat("ID column used for de-duplication:", id_col, "\n")
    cat("Duplicate MatchID/ID values found:", duplicate_id_values, "\n")
    cat("Duplicate rows removed:", duplicate_rows_removed, "\n")
  } else {
    cat("No MatchID or ID column found; no de-duplication applied.\n")
  }
  
  sort_cols <- intersect(
    c(
      "EventID",
      "EventName",
      "Date",
      "ScheduledDate",
      "MatchDate",
      "StartDate",
      "Round",
      "RoundName",
      "MatchNo",
      "MatchNum",
      "Number",
      "ID",
      "MatchID"
    ),
    names(matches_all)
  )
  
  if (length(sort_cols) > 0L) {
    setorderv(matches_all, sort_cols)
  }
  
  combined_file <- file.path(
    matches_dir,
    paste0("matches_", season, "_all.csv")
  )
  
  fwrite(matches_all, combined_file)
  
  cat("Rows before de-duplication:", rows_before, "\n")
  cat("Rows after de-duplication:", nrow(matches_all), "\n")
  cat("Combined columns:", ncol(matches_all), "\n")
  cat("Saved to:", combined_file, "\n")
  cat("=========================================================\n")
  
  matches_all
}


report_latest_filled_match <- function(matches_all,
                                       season,
                                       output_dir = NULL,
                                       save_csv = TRUE) {
  matches_all <- as.data.table(matches_all)
  
  if (nrow(matches_all) == 0L) {
    cat("\n========== Latest filled match ==========\n")
    cat("Season:", season, "\n")
    cat("No rows exist in the combined match file.\n")
    cat("=========================================\n")
    return(invisible(NULL))
  }
  
  date_col <- first_existing_col(
    matches_all,
    c(
      "Date",
      "ScheduledDate",
      "MatchDate",
      "StartDate",
      "EventEndDate",
      "EventStartDate"
    )
  )
  
  if (is.null(date_col)) {
    cat("\n========== Latest filled match ==========\n")
    cat("Season:", season, "\n")
    cat("Could not determine latest filled match because no usable date column exists.\n")
    cat("Available columns:\n")
    print(names(matches_all))
    cat("=========================================\n")
    return(invisible(NULL))
  }
  
  matches_all[, MatchDateForCheck := safe_as_date(get(date_col))]
  
  if (all(is.na(matches_all$MatchDateForCheck))) {
    if ("EventEndDate" %in% names(matches_all)) {
      date_col <- "EventEndDate"
      matches_all[, MatchDateForCheck := safe_as_date(EventEndDate)]
    } else if ("EventStartDate" %in% names(matches_all)) {
      date_col <- "EventStartDate"
      matches_all[, MatchDateForCheck := safe_as_date(EventStartDate)]
    }
  }
  
  has_result <- detect_filled_result_rows(matches_all)
  
  filled <- matches_all[
    has_result &
      !is.na(MatchDateForCheck) &
      MatchDateForCheck <= Sys.Date()
  ]
  
  if (nrow(filled) == 0L) {
    cat("\n========== Latest filled match ==========\n")
    cat("Season:", season, "\n")
    cat("No filled result rows found.\n")
    cat("This probably means the API currently only returned placeholders or future matches.\n")
    cat("Date column checked:", date_col, "\n")
    cat("=========================================\n")
    return(invisible(NULL))
  }
  
  setorder(filled, -MatchDateForCheck)
  
  latest <- filled[1]
  
  cat("\n========== Latest filled match ==========\n")
  cat("Season:", season, "\n")
  cat("Date column used:", date_col, "\n")
  cat("Latest filled date:", as.character(latest$MatchDateForCheck), "\n")
  
  if ("EventID" %in% names(latest)) {
    cat("Event ID:", latest$EventID, "\n")
  }
  
  if ("EventName" %in% names(latest)) {
    cat("Event:", latest$EventName, "\n")
  }
  
  if (all(c("Player1", "Player2") %in% names(latest))) {
    cat("Match:", latest$Player1, "v", latest$Player2, "\n")
  } else if (all(c("Player1ID", "Player2ID") %in% names(latest))) {
    cat("Player IDs:", latest$Player1ID, "v", latest$Player2ID, "\n")
  }
  
  if (all(c("Score1", "Score2") %in% names(latest))) {
    cat("Score:", latest$Score1, "-", latest$Score2, "\n")
  }
  
  if ("WinnerID" %in% names(latest)) {
    cat("Winner ID:", latest$WinnerID, "\n")
  }
  
  if ("Winner" %in% names(latest)) {
    cat("Winner:", latest$Winner, "\n")
  }
  
  cat("=========================================\n")
  
  if (!is.null(output_dir) && isTRUE(save_csv)) {
    out_file <- file.path(output_dir, paste0("latest_filled_match_", season, ".csv"))
    
    latest_to_write <- copy(latest)
    latest_to_write[, MatchDateForCheck := as.character(MatchDateForCheck)]
    
    fwrite(latest_to_write, out_file)
    
    cat("Latest filled match saved to:\n")
    cat(out_file, "\n")
  }
  
  invisible(latest)
}

# ============================================================
# Main loop
# ============================================================

for (season in START_SEASON:END_SEASON) {
  cat(sprintf("\n========== Season %s ==========\n", season))
  
  season_match_dir <- file.path(MATCHES_DIR, paste0("matches_", season))
  dir.create(season_match_dir, recursive = TRUE, showWarnings = FALSE)
  
  # -----------------------------
  # 1) Get event list for the season
  # -----------------------------
  events <- tryCatch(
    get_snooker(
      paste0("?t=5&s=", season),
      pause = REQUEST_PAUSE,
      retries = MAX_RETRIES
    ),
    error = function(e) {
      cat(sprintf("Failed to get events for season %s: %s\n", season, conditionMessage(e)))
      NULL
    }
  )
  
  if (is.null(events) || !is.data.frame(events) || nrow(events) == 0L) {
    cat(sprintf("No events returned for season %s\n", season))
    next
  }
  
  cat("Events returned for season:", nrow(events), "\n")
  
  # -----------------------------
  # 2) Save season event file
  # -----------------------------
  keep_event_cols <- intersect(
    c(
      "ID", "Name", "StartDate", "EndDate", "Season", "Tour", "Type",
      "Venue", "City", "Country", "Main", "Sex", "AgeGroup",
      "WorldSnookerId", "NumCompetitors", "NumUpcoming", "NumActive", "NumResults"
    ),
    names(events)
  )
  
  fwrite(
    as.data.table(events[, keep_event_cols, drop = FALSE]),
    file.path(EVENTS_DIR, paste0("events_", season, ".csv"))
  )
  
  # -----------------------------
  # 3) Select events to refresh
  # -----------------------------
  events_recent <- select_events_to_refresh(
    events = events,
    refresh_mode = REFRESH_MODE,
    last_update_date = LAST_UPDATE_DATE,
    last_n_days = LAST_N_DAYS
  )
  
  cat("Events selected for refresh:", nrow(events_recent), "\n")
  
  if (nrow(events_recent) > 0L) {
    print(
      events_recent[
        ,
        intersect(c("ID", "Name", "StartDate", "EndDate", "Tour"), names(events_recent)),
        drop = FALSE
      ]
    )
  }
  
  event_ids <- unique(events_recent$ID)
  
  # -----------------------------
  # 4) Refresh selected raw event files
  # -----------------------------
  if (length(event_ids) == 0L) {
    cat("No events matched the refresh filter for this season.\n")
    
  } else {
    season_failed <- data.frame(
      Season = integer(),
      EventID = character(),
      EventName = character(),
      Error = character(),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(event_ids)) {
      event_id <- event_ids[i]
      
      event_row <- events_recent[events_recent$ID == event_id, , drop = FALSE]
      
      event_name <- if ("Name" %in% names(event_row)) {
        as.character(event_row$Name[1])
      } else {
        NA_character_
      }
      
      out_file <- file.path(season_match_dir, paste0("event_", event_id, ".csv"))
      
      cat(sprintf(
        "[%d/%d] Season %s - Refreshing event %s (%s)\n",
        i, length(event_ids), season, event_id, event_name
      ))
      
      dat <- tryCatch(
        get_snooker(
          paste0("?t=6&e=", event_id),
          pause = REQUEST_PAUSE,
          retries = MAX_RETRIES
        ),
        error = function(e) {
          season_failed <<- rbind(
            season_failed,
            data.frame(
              Season = season,
              EventID = as.character(event_id),
              EventName = as.character(event_name),
              Error = conditionMessage(e),
              stringsAsFactors = FALSE
            )
          )
          
          NULL
        }
      )
      
      if (!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0L) {
        dat$EventID <- event_id
        dat$EventName <- event_name
        dat$EventStartDate <- if ("StartDate" %in% names(event_row)) event_row$StartDate[1] else NA
        dat$EventEndDate <- if ("EndDate" %in% names(event_row)) event_row$EndDate[1] else NA
        dat$Season <- season
        
        fwrite(as.data.table(dat), out_file)
        
        cat(sprintf("  -> wrote %d rows\n", nrow(dat)))
        
      } else {
        cat("  -> no match rows returned\n")
        
        # If a selected event now returns nothing, remove its cached raw file.
        # This prevents stale rows from staying in the rebuilt combined file.
        if (file.exists(out_file)) {
          file.remove(out_file)
          cat("  -> removed old raw event file because API returned no rows\n")
        }
      }
    }
    
    if (nrow(season_failed) > 0L) {
      failed_file <- file.path(MATCHES_DIR, paste0("failed_match_calls_", season, ".csv"))
      
      fwrite(
        as.data.table(season_failed),
        failed_file
      )
      
      cat("Failed event calls logged:", nrow(season_failed), "\n")
      cat("Failed calls saved to:", failed_file, "\n")
    }
  }
  
  # -----------------------------
  # 5) Rebuild raw combined file from all cached raw event files
  # -----------------------------
  matches_all <- rebuild_raw_combined_season(
    season = season,
    season_match_dir = season_match_dir,
    matches_dir = MATCHES_DIR
  )
  
  # -----------------------------
  # 6) Report latest filled match
  # -----------------------------
  if (!is.null(matches_all) && nrow(matches_all) > 0L) {
    latest_filled_match <- report_latest_filled_match(
      matches_all = matches_all,
      season = season,
      output_dir = MATCHES_DIR,
      save_csv = TRUE
    )
    
    cat("\nFirst few rows of raw combined file:\n")
    print(head(matches_all))
  }
}

cat("\nDone.\n")