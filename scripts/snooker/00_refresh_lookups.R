library(httr)
library(jsonlite)

header_value <- "SamJRatings262"
base_url <- "https://api.snooker.org/"
save_dir <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Snooker/Players"

dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

request_pause <- 10
max_retries <- 3

# Change this range if needed
seasons <- 2025:1974

get_snooker <- function(query, pause = 10, retries = 3) {
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

make_name <- function(df) {
  if (all(c("FirstName", "MiddleName", "LastName") %in% names(df))) {
    apply(
      df[, c("FirstName", "MiddleName", "LastName"), drop = FALSE],
      1,
      function(x) paste(x[x != "" & !is.na(x)], collapse = " ")
    )
  } else {
    rep(NA_character_, nrow(df))
  }
}

all_players <- list()
failed_calls <- data.frame(
  Season = integer(),
  StatusType = character(),
  Error = character(),
  stringsAsFactors = FALSE
)

row_num <- 1

for (season in seasons) {
  cat(sprintf("\n===== Season %s =====\n", season))
  
  for (status_type in c("p", "a")) {
    cat(sprintf("Pulling t=10 for season %s, st=%s\n", season, status_type))
    
    dat <- tryCatch(
      get_snooker(
        paste0("?t=10&st=", status_type, "&s=", season),
        pause = request_pause,
        retries = max_retries
      ),
      error = function(e) {
        failed_calls <<- rbind(
          failed_calls,
          data.frame(
            Season = season,
            StatusType = status_type,
            Error = conditionMessage(e),
            stringsAsFactors = FALSE
          )
        )
        NULL
      }
    )
    
    if (!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0) {
      dat$SeasonPulled <- season
      dat$StatusTypePulled <- status_type
      all_players[[row_num]] <- dat
      row_num <- row_num + 1
      cat(sprintf("  -> got %d rows\n", nrow(dat)))
    } else {
      cat("  -> no rows returned\n")
    }
  }
}

players_all <- do.call(rbind, all_players)

players_all$Name <- make_name(players_all)

# Keep useful columns if present
keep_cols <- intersect(
  c(
    "ID", "Name", "FirstName", "MiddleName", "LastName", "Nationality",
    "Born", "Died", "Sex", "Type", "FirstSeasonAsPro", "LastSeasonAsPro",
    "SeasonPulled", "StatusTypePulled"
  ),
  names(players_all)
)

players_all <- players_all[, keep_cols, drop = FALSE]

# Deduplicate full table
players_all_unique <- unique(players_all)

# Create final lookup by player ID
player_lookup <- players_all_unique[order(players_all_unique$ID), ]
player_lookup <- player_lookup[!duplicated(player_lookup$ID), ]

write.csv(
  players_all_unique,
  file.path(save_dir, "snooker_players_all_by_season.csv"),
  row.names = FALSE
)

write.csv(
  player_lookup,
  file.path(save_dir, "snooker_player_lookup_complete.csv"),
  row.names = FALSE
)

write.csv(
  failed_calls,
  file.path(save_dir, "failed_player_calls_log.csv"),
  row.names = FALSE
)

cat("\nDone.\n")
cat("Rows in combined player table:", nrow(players_all_unique), "\n")
cat("Unique player IDs in final lookup:", nrow(player_lookup), "\n")
print(head(player_lookup))




library(httr)
library(jsonlite)

header_value <- "SamJRatings262"
base_url <- "https://api.snooker.org/"
save_dir <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Snooker/Events"

dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

request_pause <- 10
max_retries <- 3

# Change this range if needed
seasons <- 2025:1974

get_snooker <- function(query, pause = 10, retries = 3) {
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

all_events <- list()
failed_calls <- data.frame(
  Season = integer(),
  Error = character(),
  stringsAsFactors = FALSE
)

row_num <- 1

for (season in seasons) {
  cat(sprintf("\n===== Season %s =====\n", season))
  
  dat <- tryCatch(
    get_snooker(
      paste0("?t=5&s=", season),
      pause = request_pause,
      retries = max_retries
    ),
    error = function(e) {
      failed_calls <<- rbind(
        failed_calls,
        data.frame(
          Season = season,
          Error = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      )
      NULL
    }
  )
  
  if (!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0) {
    dat$SeasonPulled <- season
    all_events[[row_num]] <- dat
    row_num <- row_num + 1
    cat(sprintf("  -> got %d rows\n", nrow(dat)))
  } else {
    cat("  -> no rows returned\n")
  }
}

if (length(all_events) == 0) {
  stop("No event data was returned for any season.")
}

events_all <- do.call(rbind, all_events)

# Keep useful columns if present
keep_cols <- intersect(
  c(
    "ID",
    "Name",
    "StartDate",
    "EndDate",
    "Sponsor",
    "Season",
    "Tour",
    "Type",
    "Num",
    "Venue",
    "City",
    "Country",
    "Discipline",
    "Main",
    "Sex",
    "AgeGroup",
    "TV",
    "SeasonPulled"
  ),
  names(events_all)
)

events_all <- events_all[, keep_cols, drop = FALSE]

# Deduplicate full table
events_all_unique <- unique(events_all)

# Create final lookup by event ID
event_lookup <- events_all_unique[order(events_all_unique$ID, events_all_unique$SeasonPulled), ]
event_lookup <- event_lookup[!duplicated(event_lookup$ID), ]

write.csv(
  events_all_unique,
  file.path(save_dir, "snooker_events_all_by_season.csv"),
  row.names = FALSE
)

write.csv(
  event_lookup,
  file.path(save_dir, "snooker_event_lookup_complete.csv"),
  row.names = FALSE
)

write.csv(
  failed_calls,
  file.path(save_dir, "failed_event_calls_log.csv"),
  row.names = FALSE
)

cat("\nDone.\n")
cat("Rows in combined event table:", nrow(events_all_unique), "\n")
cat("Unique event IDs in final lookup:", nrow(event_lookup), "\n")
print(head(event_lookup))

