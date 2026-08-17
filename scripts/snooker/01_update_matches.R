library(httr)
library(jsonlite)
library(data.table)
library(tictoc)
library(beepr)

options(stringsAsFactors = FALSE)

tic("Snooker pipeline")

# ============================================================
# 01_update_matches.R
#
# Purpose:
#   - Load event lists refreshed by 00_refresh_lookups.R
#   - Refresh recent snooker match data from snooker.org
#   - Update combined season match CSVs directly
#   - Avoid storing thousands of small per-event CSVs in Git
#
# Important safety rule:
#   - Do not delete old event rows before checking new API rows.
#   - Keep existing rows, append refreshed rows, then de-duplicate by MatchID.
#   - Prefer scored rows over 0-0/placeholders.
#   - If both rows are scored, prefer the newly refreshed row.
#
# Reads/writes only inside the Git repo:
#   Snooker/pipeline_data/Events/events_YYYY.csv
#   Snooker/pipeline_data/Matches_Clean_Combined/matches_YYYY_all.csv
#
# Does NOT write:
#   raw per-event match CSVs
#   cleaned per-event match CSVs
#   failed-call logs
# ============================================================

# -----------------------------
# Repo paths
# -----------------------------
REPO_DIR <- Sys.getenv(
  "GITHUB_WORKSPACE",
  unset = "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"
)

SNOOKER_DIR <- file.path(REPO_DIR, "Snooker")
PIPELINE_DIR <- file.path(SNOOKER_DIR, "pipeline_data")

EVENTS_DIR <- file.path(PIPELINE_DIR, "Events")
MATCHES_COMBINED_DIR <- file.path(PIPELINE_DIR, "Matches_Clean_Combined")

dir.create(EVENTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MATCHES_COMBINED_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# API settings
# -----------------------------
base_url <- "https://api.snooker.org/"

header_value <- Sys.getenv("SNOOKER_API_HEADER")

if (header_value == "") {
  stop(
    "Missing SNOOKER_API_HEADER environment variable. ",
    "For local testing, run: Sys.setenv(SNOOKER_API_HEADER = 'your header value')"
  )
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

# Active events are refreshed on every run.
#
# Completed events continue to be refreshed for seven days so that
# delayed results and corrections can still be collected.
COMPLETED_EVENT_REFRESH_DAYS <- as.integer(
  Sys.getenv("COMPLETED_EVENT_REFRESH_DAYS", unset = "7")
)

# -----------------------------
# Helpers
# -----------------------------
get_current_snooker_season <- function(date = Sys.Date()) {
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  
  # Snooker season generally runs June to May.
  # 2025 = 2025/2026 season.
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

clean_id <- function(x) {
  trimws(as.character(x))
}

safe_date <- function(x) {
  suppressWarnings(as.Date(x))
}

first_existing_col <- function(dt, candidates) {
  hit <- candidates[candidates %in% names(dt)]
  if (length(hit) == 0L) return(NULL)
  hit[1]
}

coalesce_first_existing <- function(dt, candidates) {
  found <- candidates[candidates %in% names(dt)]
  
  if (length(found) == 0L) {
    return(NULL)
  }
  
  out <- dt[[found[1]]]
  
  if (length(found) > 1L) {
    for (col in found[-1]) {
      missing <- is.na(out) | trimws(as.character(out)) == ""
      out[missing] <- dt[[col]][missing]
    }
  }
  
  out
}

normalise_event_file <- function(events, season) {
  events <- as.data.table(events)
  
  keep_cols <- intersect(
    c(
      "ID", "Name", "StartDate", "EndDate", "Sponsor", "Season",
      "Tour", "Type", "Num", "Venue", "City", "Country",
      "Discipline", "Main", "Sex", "AgeGroup", "TV",
      "WorldSnookerId", "NumCompetitors", "NumUpcoming",
      "NumActive", "NumResults"
    ),
    names(events)
  )
  
  out <- events[, ..keep_cols]
  
  if (!"Season" %in% names(out)) {
    out[, Season := season]
  }
  
  out
}

deduplicate_matches <- function(dt) {
  dt <- as.data.table(dt)
  
  if (nrow(dt) == 0L) {
    return(dt)
  }
  
  match_id_raw <- coalesce_first_existing(dt, c("MatchID", "ID"))
  
  if (is.null(match_id_raw)) {
    cat("No MatchID/ID column found; no match de-duplication applied.\n")
    return(dt)
  }
  
  dt[, MergeMatchID := clean_id(match_id_raw)]
  
  score1_col <- first_existing_col(dt, c("ScoreA", "Score1"))
  score2_col <- first_existing_col(dt, c("ScoreB", "Score2"))
  
  if (!is.null(score1_col) && !is.null(score2_col)) {
    s1 <- suppressWarnings(as.numeric(dt[[score1_col]]))
    s2 <- suppressWarnings(as.numeric(dt[[score2_col]]))
    
    dt[, HasResultForMerge := !is.na(s1) & !is.na(s2) & !(s1 == 0 & s2 == 0)]
  } else {
    dt[, HasResultForMerge := FALSE]
  }
  
  if (!"IsNewRefreshRow" %in% names(dt)) {
    dt[, IsNewRefreshRow := 0L]
  }
  
  dt[, IsNewRefreshRow := fifelse(is.na(IsNewRefreshRow), 0L, as.integer(IsNewRefreshRow))]
  
  with_id <- dt[!is.na(MergeMatchID) & MergeMatchID != ""]
  without_id <- dt[is.na(MergeMatchID) | MergeMatchID == ""]
  
  rows_before <- nrow(dt)
  
  # Preference:
  #   1. Rows with real results
  #   2. New refreshed rows
  #   3. Existing rows
  setorder(
    with_id,
    MergeMatchID,
    -HasResultForMerge,
    -IsNewRefreshRow
  )
  
  with_id <- with_id[!duplicated(MergeMatchID)]
  
  out <- rbindlist(
    list(with_id, without_id),
    use.names = TRUE,
    fill = TRUE
  )
  
  out[, c("MergeMatchID", "HasResultForMerge", "IsNewRefreshRow") := NULL]
  
  cat("Rows before match de-duplication:", rows_before, "\n")
  cat("Rows after match de-duplication:", nrow(out), "\n")
  
  out
}

sort_matches_stably <- function(dt) {
  dt <- as.data.table(dt)
  
  sort_cols <- intersect(
    c(
      "EventID",
      "EventName",
      "Date",
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
    names(dt)
  )
  
  if (length(sort_cols) > 0L) {
    setorderv(dt, sort_cols)
  }
  
  dt
}

force_date_columns_to_character <- function(existing, add_rows) {
  date_cols <- c(
    "EventStartDate",
    "EventEndDate",
    "StartDate",
    "EndDate",
    "Date",
    "MatchDate",
    "ScheduledDate",
    "PlayedDate"
  )
  
  for (col in date_cols) {
    if (col %in% names(existing)) {
      existing[, (col) := as.character(get(col))]
    }
    
    if (col %in% names(add_rows)) {
      add_rows[, (col) := as.character(get(col))]
    }
  }
  
  list(existing = existing, add_rows = add_rows)
}

# -----------------------------
# Select seasons to inspect
# -----------------------------
current_season <- get_current_snooker_season()

candidate_seasons <- sort(unique(c(
  current_season - 1L,
  current_season,
  current_season + 1L
)))

cat("Repo directory:", REPO_DIR, "\n")
cat("Snooker pipeline directory:", PIPELINE_DIR, "\n")
cat("Current inferred snooker season:", current_season, "\n")
cat("Candidate seasons:", paste(candidate_seasons, collapse = ", "), "\n")
cat("Completed-event refresh window:", COMPLETED_EVENT_REFRESH_DAYS, "days\n")
# ============================================================
# 1) Load event lists already refreshed by 00_refresh_lookups.R
# ============================================================

# 00_refresh_lookups.R runs before this script and writes events_YYYY.csv.
# Reusing those files avoids repeating the same season-list API calls.
events_list <- list()
event_list_index <- 1L

for (season in candidate_seasons) {
  cat("\n========== Event list season", season, "==========\n")
  
  event_file <- file.path(EVENTS_DIR, paste0("events_", season, ".csv"))
  
  if (!file.exists(event_file)) {
    cat("No local event file for season", season, "- skipping.\n")
    next
  }
  
  events_dt <- fread(event_file, encoding = "UTF-8")
  
  cat("Using event file refreshed by 00_refresh_lookups.R:", event_file, "\n")
  cat("Event rows:", nrow(events_dt), "\n")
  
  events_dt[, EventSeason := season]
  events_list[[event_list_index]] <- events_dt
  event_list_index <- event_list_index + 1L
}

if (length(events_list) == 0L) {
  stop(
    "No local event data available. ",
    "Run 00_refresh_lookups.R before 01_update_matches.R."
  )
}

events_all <- rbindlist(events_list, use.names = TRUE, fill = TRUE)

if (!"ID" %in% names(events_all)) {
  stop("Event data has no ID column.")
}

if (!"Name" %in% names(events_all)) {
  events_all[, Name := NA_character_]
}

if (!"StartDate" %in% names(events_all)) {
  events_all[, StartDate := NA_character_]
}

if (!"EndDate" %in% names(events_all)) {
  events_all[, EndDate := NA_character_]
}

events_all[, ID := clean_id(ID)]
events_all[, StartDate2 := safe_date(StartDate)]
events_all[, EndDate2 := safe_date(EndDate)]

# ============================================================
# 2) Select events to refresh
# ============================================================

today <- Sys.Date()
completed_cutoff_date <- today - COMPLETED_EVENT_REFRESH_DAYS

events_to_refresh <- events_all[
  ID != "" &
    (
      # Event is currently active.
      (
        !is.na(StartDate2) &
          !is.na(EndDate2) &
          StartDate2 <= today &
          EndDate2 >= today
      ) |
        
        # Event completed recently. Continue refreshing it briefly
        # to collect delayed results or corrections.
        (
          !is.na(EndDate2) &
            EndDate2 < today &
            EndDate2 >= completed_cutoff_date
        ) |
        
        # Fallback for events that do not have an end date.
        (
          !is.na(StartDate2) &
            is.na(EndDate2) &
            StartDate2 <= today &
            StartDate2 >= completed_cutoff_date
        )
    )
]

events_to_refresh <- unique(events_to_refresh, by = c("EventSeason", "ID"))

setorder(events_to_refresh, EventSeason, StartDate2, EndDate2, ID)

cat("\n========== Events selected for match refresh ==========\n")
cat("Completed-event cutoff date:",  format(completed_cutoff_date, "%Y-%m-%d"),  "\n")
cat("Events selected:", nrow(events_to_refresh), "\n")

if (nrow(events_to_refresh) > 0L) {
  print(events_to_refresh[, .(
    EventSeason,
    ID,
    Name,
    StartDate,
    EndDate
  )])
}

if (nrow(events_to_refresh) == 0L) {
  cat("No events matched the refresh window. Done.\n")
  quit(save = "no")
}

# ============================================================
# 3) Pull match data for selected events
# ============================================================

new_match_rows <- list()
new_match_index <- 1L

for (i in seq_len(nrow(events_to_refresh))) {
  event_id <- events_to_refresh$ID[i]
  event_name <- events_to_refresh$Name[i]
  season <- events_to_refresh$EventSeason[i]
  
  cat(sprintf(
    "\n[%d/%d] Refreshing season %s event %s (%s)\n",
    i,
    nrow(events_to_refresh),
    season,
    event_id,
    event_name
  ))
  
  dat <- tryCatch(
    get_snooker(paste0("?t=6&e=", event_id)),
    error = function(e) {
      cat("  -> failed:", conditionMessage(e), "\n")
      NULL
    }
  )
  
  if (!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0L) {
    dat <- as.data.table(dat)
    
    if ("EventID" %in% names(dat)) {
      dat[, EventID := as.character(EventID)]
    }
    
    dat[, EventID := as.character(event_id)]
    dat[, EventName := as.character(event_name)]
    dat[, EventStartDate := as.character(events_to_refresh$StartDate[i])]
    dat[, EventEndDate := as.character(events_to_refresh$EndDate[i])]
    dat[, Season := as.integer(season)]
    dat[, IsNewRefreshRow := 1L]
    
    new_match_rows[[new_match_index]] <- dat
    new_match_index <- new_match_index + 1L
    
    cat("  -> got", nrow(dat), "rows\n")
    
  } else {
    cat("  -> no rows returned\n")
  }
}

new_matches <- if (length(new_match_rows) > 0L) {
  rbindlist(new_match_rows, use.names = TRUE, fill = TRUE)
} else {
  data.table()
}

cat("\nNew/refreshed match rows returned:", nrow(new_matches), "\n")

# ============================================================
# 4) Update combined season files directly
# ============================================================

refresh_by_season <- split(events_to_refresh, events_to_refresh$EventSeason)

for (season_name in names(refresh_by_season)) {
  season <- as.integer(season_name)
  
  cat("\n========== Updating combined season", season, "==========\n")
  
  combined_file <- file.path(
    MATCHES_COMBINED_DIR,
    paste0("matches_", season, "_all.csv")
  )
  
  existing <- if (file.exists(combined_file)) {
    fread(combined_file, encoding = "UTF-8")
  } else {
    data.table()
  }
  
  cat("Existing rows:", nrow(existing), "\n")
  
  if (nrow(existing) > 0L) {
    existing[, IsNewRefreshRow := 0L]
  }
  
  add_rows <- if (nrow(new_matches) > 0L) {
    new_matches[Season == season]
  } else {
    data.table()
  }
  
  cat("Rows to add for season:", nrow(add_rows), "\n")
  
  fixed_tables <- force_date_columns_to_character(existing, add_rows)
  existing <- fixed_tables$existing
  add_rows <- fixed_tables$add_rows
  
  combined <- rbindlist(
    list(existing, add_rows),
    use.names = TRUE,
    fill = TRUE
  )
  
  combined <- deduplicate_matches(combined)
  combined <- sort_matches_stably(combined)
  
  fwrite(combined, combined_file)
  
  cat("Final combined rows:", nrow(combined), "\n")
  cat("Wrote:", combined_file, "\n")
}

cat("\nDone.\n")
toc()
beep()

