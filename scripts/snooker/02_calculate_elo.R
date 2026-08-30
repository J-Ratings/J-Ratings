library(data.table)

options(stringsAsFactors = FALSE)

# ============================================================
# 02_calculate_elo.R
#
# Snooker Elo - TWO PASS ZERO-SUM RETROSPECTIVE START VERSION
#
# Method:
#   - Single global Elo pool
#   - Constant K = 5 for both players in every match
#   - No provisional double-K and no opponent-frame dampening
#   - Every match update is exactly zero-sum
#   - Pass 1 starts everyone at 2600
#   - Players with at least 200 early frames receive a retrospective
#     frame-performance starting rating for Pass 2
#   - Players without 200 frames keep the 2600 fallback start
#   - Pass 2 is the final published history
#   - Checkpoints and season snapshots are rebuilt from the final pass
#   - Snapshots are season-end snapshots, not calendar-year snapshots
#
# Reads/writes only inside the Git repo:
#   Snooker/pipeline_data/
#
# Inputs:
#   Snooker/pipeline_data/Matches_Clean_Combined/matches_YYYY_all.csv
#   Snooker/pipeline_data/Events/events_YYYY.csv
#   Snooker/pipeline_data/Players/snooker_player_lookup_complete.csv
#
# Outputs:
#   Snooker/pipeline_data/Elo/snooker_elo_match_history.csv
#   Snooker/pipeline_data/Elo/snooker_elo_final_ratings.csv
#   Snooker/pipeline_data/Elo/checkpoints/checkpoint_season_YYYY_end.csv
#   Snooker/pipeline_data/Elo/season_history/snooker_elo_match_history_season_YYYY.csv
#   Snooker/pipeline_data/Elo/season_snapshots/snapshot_season_YYYY.csv
#   Snooker/pipeline_data/Elo/season_snapshots/snapshot_current.csv
# ============================================================

# -----------------------------
# Repo paths
# -----------------------------
REPO_DIR <- Sys.getenv(
  "GITHUB_WORKSPACE",
  unset = "C:/Users/stjuk/Documents/GitHub/J-Ratings"
)

SNOOKER_DIR <- file.path(REPO_DIR, "Snooker")
PIPELINE_DIR <- file.path(SNOOKER_DIR, "pipeline_data")

EVENTS_DIR <- file.path(PIPELINE_DIR, "Events")
MATCHES_CLEAN_DIR <- file.path(PIPELINE_DIR, "Matches_Clean_Combined")
PLAYERS_DIR <- file.path(PIPELINE_DIR, "Players")

OUT_DIR <- file.path(PIPELINE_DIR, "Elo")
CHECKPOINT_DIR <- file.path(OUT_DIR, "checkpoints")
SEASON_HISTORY_DIR <- file.path(OUT_DIR, "season_history")
SEASON_SNAPSHOT_DIR <- file.path(OUT_DIR, "season_snapshots")

PLAYER_LOOKUP_FILE <- file.path(PLAYERS_DIR, "snooker_player_lookup_complete.csv")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CHECKPOINT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SEASON_HISTORY_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SEASON_SNAPSHOT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTPUT_MATCH_HISTORY_CSV <- file.path(OUT_DIR, "snooker_elo_match_history.csv")
OUTPUT_FINAL_RATINGS_CSV <- file.path(OUT_DIR, "snooker_elo_final_ratings.csv")
OUTPUT_UPCOMING_MATCHES_CSV <- file.path(OUT_DIR, "snooker_upcoming_matches.csv")
OUTPUT_RETRO_STARTS_CSV <- file.path(OUT_DIR, "snooker_retro_start_ratings.csv")

# -----------------------------
# Elo settings
# -----------------------------
BASELINE_START_RATING <- 2600
K_VALUE <- 5
RETRO_FRAMES_N <- 200L
RETRO_RATING_LOWER <- 1000
RETRO_RATING_UPPER <- 4000

MODERN_SEASON_START <- 2008L
PLAUSIBLE_EVENT_WINDOW_DAYS <- 45L

# Legacy metadata fields are retained in outputs so downstream code that
# expects these columns does not break. They now describe the disabled rules.
NEW_PLAYER_MATCHES <- 0L
NEW_PLAYER_K_MULTIPLIER <- 1
PROVISIONAL_FRAME_THRESHOLD <- 0L
MIN_OPP_WEIGHT <- 1

MIN_LIST_FRAMES <- 200L
ACTIVE_YEARS <- 2L

# -----------------------------
# Checkpoint settings
# -----------------------------
# Normal monthly/cloud use:
#   FORCE_FULL_REBUILD <- FALSE
#   REBUILD_FROM_SEASON <- NA_integer_
#   FINALISE_CURRENT_SEASON <- FALSE
#
# Full rebuild:
#   set FORCE_FULL_REBUILD=true in environment
#
# Rebuild from old corrected season:
#   set REBUILD_FROM_SEASON=2018 in environment
#
# Finalise season after season ends:
#   set FINALISE_CURRENT_SEASON=true in environment
FORCE_FULL_REBUILD <- tolower(Sys.getenv("FORCE_FULL_REBUILD", unset = "false")) %in% c("true", "1", "yes", "y")
FINALISE_CURRENT_SEASON <- tolower(Sys.getenv("FINALISE_CURRENT_SEASON", unset = "false")) %in% c("true", "1", "yes", "y")

REBUILD_FROM_SEASON_RAW <- Sys.getenv("REBUILD_FROM_SEASON", unset = "")
REBUILD_FROM_SEASON <- suppressWarnings(as.integer(REBUILD_FROM_SEASON_RAW))
if (is.na(REBUILD_FROM_SEASON)) REBUILD_FROM_SEASON <- NA_integer_

# ============================================================
# Helpers
# ============================================================

expected_score <- function(Ra, Rb) {
  1 / (1 + 10 ^ ((Rb - Ra) / 400))
}

clean_id <- function(x) {
  trimws(as.character(x))
}

clean_text <- function(x) {
  trimws(as.character(x))
}

parse_dt_multi <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  
  out <- as.POSIXct(rep(NA_character_, length(x)), tz = "UTC")
  
  fmts <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%d",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d/%m/%Y",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%Y/%m/%d",
    "%d-%b-%Y %H:%M:%S",
    "%d-%b-%Y"
  )
  
  remaining <- is.na(out) & !is.na(x)
  
  for (fmt in fmts) {
    if (!any(remaining)) break
    
    parsed <- as.POSIXct(x[remaining], format = fmt, tz = "UTC")
    idx <- which(remaining)
    out[idx[!is.na(parsed)]] <- parsed[!is.na(parsed)]
    remaining <- is.na(out) & !is.na(x)
  }
  
  out
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

# These helpers are retained only for output/schema compatibility.
# The final Elo engine uses neither provisional K nor opponent dampening.
opponent_weight <- function(...) {
  1
}

player_k_multiplier <- function(...) {
  1
}

checkpoint_file_for_season <- function(season) {
  file.path(CHECKPOINT_DIR, paste0("checkpoint_season_", season, "_end.csv"))
}

season_history_file_for_season <- function(season) {
  file.path(SEASON_HISTORY_DIR, paste0("snooker_elo_match_history_season_", season, ".csv"))
}

season_snapshot_file_for_season <- function(season) {
  file.path(SEASON_SNAPSHOT_DIR, paste0("snapshot_season_", season, ".csv"))
}

extract_checkpoint_season <- function(path) {
  suppressWarnings(as.integer(sub("^checkpoint_season_(\\d{4})_end\\.csv$", "\\1", basename(path))))
}

# ============================================================
# Load player lookup
# ============================================================

if (!file.exists(PLAYER_LOOKUP_FILE)) {
  stop("Player lookup file not found: ", PLAYER_LOOKUP_FILE)
}

player_lookup <- fread(PLAYER_LOOKUP_FILE, encoding = "UTF-8")

required_lookup_cols <- c("ID", "Name")
missing_lookup_cols <- setdiff(required_lookup_cols, names(player_lookup))

if (length(missing_lookup_cols) > 0L) {
  stop(
    "Player lookup file is missing required columns: ",
    paste(missing_lookup_cols, collapse = ", ")
  )
}

player_lookup[, ID := clean_id(ID)]
player_lookup[, Name := clean_text(Name)]

if ("Nationality" %in% names(player_lookup)) {
  player_lookup[, Nationality := clean_text(Nationality)]
} else {
  player_lookup[, Nationality := NA_character_]
}

player_lookup[Nationality %in% c("", "NA", "NULL"), Nationality := NA_character_]
player_lookup <- player_lookup[ID != ""]
setorder(player_lookup, ID)
player_lookup <- player_lookup[!duplicated(ID), .(ID, Name, Nationality)]

cat("Repo directory:", REPO_DIR, "\n")
cat("Pipeline directory:", PIPELINE_DIR, "\n")
cat("Player lookup rows:", nrow(player_lookup), "\n")
cat("Unique player IDs in lookup:", uniqueN(player_lookup$ID), "\n")

# ============================================================
# Load event files
# ============================================================

event_files <- list.files(
  EVENTS_DIR,
  pattern = "^events_\\d{4}\\.csv$",
  full.names = TRUE
)

if (length(event_files) == 0L) {
  stop("No event CSV files found in: ", EVENTS_DIR)
}

cat("\nFound", length(event_files), "event CSV files.\n")

event_list <- lapply(event_files, function(f) {
  cat("Reading event file:", basename(f), "\n")
  e <- fread(f, encoding = "UTF-8")
  
  id_col <- first_existing_col(e, c("ID", "EventID", "Event_ID"))
  name_col <- first_existing_col(e, c("Name", "EventName"))
  start_col <- first_existing_col(e, c("StartDate", "EventStartDate"))
  end_col <- first_existing_col(e, c("EndDate", "EventEndDate"))
  season_col <- first_existing_col(e, c("Season"))
  
  if (is.null(id_col) || is.null(start_col)) {
    stop("Event file missing required columns: ", basename(f))
  }
  
  out <- data.table(
    EventID = clean_id(e[[id_col]]),
    EventName = if (!is.null(name_col)) clean_text(e[[name_col]]) else NA_character_,
    EventStartDateRaw = clean_text(e[[start_col]]),
    EventEndDateRaw = if (!is.null(end_col)) clean_text(e[[end_col]]) else NA_character_,
    EventSeason = if (!is.null(season_col)) suppressWarnings(as.integer(e[[season_col]])) else NA_integer_
  )
  
  file_season <- suppressWarnings(as.integer(sub("^events_(\\d{4})\\.csv$", "\\1", basename(f))))
  out[is.na(EventSeason), EventSeason := file_season]
  
  out[, EventStartDate := parse_dt_multi(EventStartDateRaw)]
  out[, EventEndDate := parse_dt_multi(EventEndDateRaw)]
  
  out
})

events_dt <- rbindlist(event_list, use.names = TRUE, fill = TRUE)
events_dt <- events_dt[EventID != ""]
setorder(events_dt, EventID, EventStartDate)
events_dt <- events_dt[!duplicated(EventID)]

cat("\nUnique events loaded:", nrow(events_dt), "\n")
cat(
  "Event date range:",
  format(min(events_dt$EventStartDate, na.rm = TRUE), "%Y-%m-%d"),
  "to",
  format(max(events_dt$EventStartDate, na.rm = TRUE), "%Y-%m-%d"),
  "\n"
)

# ============================================================
# Load cleaned combined season match files
# ============================================================

match_files <- list.files(
  MATCHES_CLEAN_DIR,
  pattern = "^matches_\\d{4}_all\\.csv$",
  full.names = TRUE
)

if (length(match_files) == 0L) {
  stop("No cleaned combined match CSV files found in: ", MATCHES_CLEAN_DIR)
}

cat("\nFound", length(match_files), "cleaned combined match CSV files.\n")

match_list <- lapply(match_files, function(f) {
  cat("Reading match file:", basename(f), "\n")
  m <- fread(f, encoding = "UTF-8")
  m[, SourceFile := basename(f)]
  m
})

dt_raw <- rbindlist(match_list, use.names = TRUE, fill = TRUE)

# ============================================================
# Standardise match columns
# ============================================================

match_id_raw <- coalesce_first_existing(dt_raw, c("MatchID", "ID"))
player_a_raw <- coalesce_first_existing(dt_raw, c("PlayerA_ID", "Player1ID", "A_ID"))
player_b_raw <- coalesce_first_existing(dt_raw, c("PlayerB_ID", "Player2ID", "B_ID"))
winner_col <- first_existing_col(dt_raw, c("WinnerID", "Winner_ID", "Winner"))
event_id_col <- first_existing_col(dt_raw, c("EventID", "Event_ID", "EID", "Event"))

score_a_raw <- coalesce_first_existing(dt_raw, c("ScoreA", "Score1"))
score_b_raw <- coalesce_first_existing(dt_raw, c("ScoreB", "Score2"))
match_date_raw <- coalesce_first_existing(dt_raw, c("MatchDate", "StartDate", "PlayedDate", "Date"))
scheduled_date_raw <- coalesce_first_existing(dt_raw, c("ScheduledDate"))

required_hits <- list(player_a_raw, player_b_raw)

if (is.null(match_id_raw)) {
  stop("No MatchID or ID column found in cleaned match files.")
}

if (any(vapply(required_hits, is.null, logical(1)))) {
  stop("One or more required match columns were not found.")
}

if (is.null(event_id_col)) {
  stop("No event ID column found in cleaned match files.")
}

dt <- data.table(
  MatchID = clean_id(match_id_raw),
  PlayerA_ID = clean_id(player_a_raw),
  PlayerB_ID = clean_id(player_b_raw),
  ScoreA = suppressWarnings(as.integer(score_a_raw)),
  ScoreB = suppressWarnings(as.integer(score_b_raw)),
  WinnerID = if (!is.null(winner_col)) clean_id(dt_raw[[winner_col]]) else "",
  MatchDateRaw = clean_text(match_date_raw),
  ScheduledDateRaw = if (!is.null(scheduled_date_raw)) {
    clean_text(scheduled_date_raw)
  } else {
    NA_character_
  },
  EventID = clean_id(dt_raw[[event_id_col]]),
  SourceFile = dt_raw$SourceFile
)


event_name_col <- first_existing_col(dt_raw, c("EventName"))
event_season_col <- first_existing_col(dt_raw, c("EventSeason", "Season"))
event_start_col <- first_existing_col(dt_raw, c("EventStartDate"))
event_end_col <- first_existing_col(dt_raw, c("EventEndDate"))

dt[, EventName_File := if (!is.null(event_name_col)) clean_text(dt_raw[[event_name_col]]) else NA_character_]
dt[, EventSeason_File := if (!is.null(event_season_col)) suppressWarnings(as.integer(dt_raw[[event_season_col]])) else NA_integer_]
dt[, EventStartDate_File := if (!is.null(event_start_col)) parse_dt_multi(dt_raw[[event_start_col]]) else as.POSIXct(NA, tz = "UTC")]
dt[, EventEndDate_File := if (!is.null(event_end_col)) parse_dt_multi(dt_raw[[event_end_col]]) else as.POSIXct(NA, tz = "UTC")]

round_col <- first_existing_col(dt_raw, c("Round", "RoundName", "RoundNo"))
table_col <- first_existing_col(dt_raw, c("TableNo"))
match_num_col <- first_existing_col(dt_raw, c("MatchNo", "MatchNum", "Number", "Num"))

dt[, RoundSort := if (!is.null(round_col)) clean_text(dt_raw[[round_col]]) else NA_character_]
dt[, TableNoSort := if (!is.null(table_col)) suppressWarnings(as.integer(dt_raw[[table_col]])) else NA_integer_]
dt[, MatchNumSort := if (!is.null(match_num_col)) suppressWarnings(as.integer(dt_raw[[match_num_col]])) else NA_integer_]

# -----------------------------
# De-duplicate by MatchID
# -----------------------------
dup_before <- dt[MatchID != "", .N, by = MatchID][N > 1L]
cat("\nDuplicate MatchID count before de-duplication:", nrow(dup_before), "\n")

dt[, HasEventID := as.integer(!is.na(EventID) & EventID != "")]
dt[, MetadataScore :=
     as.integer(!is.na(EventName_File) & EventName_File != "") +
     as.integer(!is.na(EventSeason_File)) +
     as.integer(!is.na(EventStartDate_File)) +
     as.integer(!is.na(EventEndDate_File))
]

setorder(
  dt,
  MatchID,
  -HasEventID,
  -MetadataScore,
  SourceFile
)

dt <- dt[MatchID != ""]
dt <- dt[!duplicated(MatchID)]

cat("Rows after MatchID de-duplication:", nrow(dt), "\n")

# ============================================================
# Join event reference data
# ============================================================

dt[events_dt, on = .(EventID), `:=`(
  EventName_Ref = i.EventName,
  EventSeason_Ref = i.EventSeason,
  EventStartDate_Ref = i.EventStartDate,
  EventEndDate_Ref = i.EventEndDate
)]

dt[, EventName := EventName_File]
dt[is.na(EventName) | EventName == "", EventName := EventName_Ref]

dt[, EventSeason := EventSeason_File]
dt[is.na(EventSeason), EventSeason := EventSeason_Ref]

dt[, EventStartDate := EventStartDate_File]
dt[is.na(EventStartDate), EventStartDate := EventStartDate_Ref]

dt[, EventEndDate := EventEndDate_File]
dt[is.na(EventEndDate), EventEndDate := EventEndDate_Ref]

dt[, MatchDateParsed := parse_dt_multi(MatchDateRaw)]
dt[, ScheduledDate := parse_dt_multi(ScheduledDateRaw)]

cat("\nEvent metadata check:\n")
print(dt[, .(
  MissingEventName = sum(is.na(EventName) | EventName == ""),
  MissingEventSeason = sum(is.na(EventSeason)),
  MissingEventStartDate = sum(is.na(EventStartDate)),
  MissingEventEndDate = sum(is.na(EventEndDate))
)])

# ============================================================
# Build final Elo date
# ============================================================

dt[, DateSource := NA_character_]
dt[, Date := as.POSIXct(NA, tz = "UTC")]

old_data_idx <- !is.na(dt$EventSeason) & dt$EventSeason < MODERN_SEASON_START

dt[old_data_idx, `:=`(
  Date = EventStartDate,
  DateSource = "EventStart_Pre2008Season"
)]

modern_idx <- !old_data_idx

modern_match_ok <- modern_idx &
  !is.na(dt$MatchDateParsed) &
  !is.na(dt$EventStartDate) &
  (
    (
      !is.na(dt$EventEndDate) &
        dt$MatchDateParsed >= (dt$EventStartDate - PLAUSIBLE_EVENT_WINDOW_DAYS * 86400) &
        dt$MatchDateParsed <= (dt$EventEndDate + PLAUSIBLE_EVENT_WINDOW_DAYS * 86400)
    ) |
      (
        is.na(dt$EventEndDate) &
          abs(as.numeric(difftime(dt$MatchDateParsed, dt$EventStartDate, units = "days"))) <= PLAUSIBLE_EVENT_WINDOW_DAYS
      )
  )

dt[modern_match_ok, `:=`(
  Date = MatchDateParsed,
  DateSource = "MatchDate_Modern"
)]

modern_fallback_idx <- modern_idx & is.na(dt$Date)

dt[modern_fallback_idx, `:=`(
  Date = EventStartDate,
  DateSource = "EventStart_Fallback"
)]

dt[is.na(Date) & !is.na(MatchDateParsed), `:=`(
  Date = MatchDateParsed,
  DateSource = "MatchDate_LastResort"
)]

cat("\nDate source breakdown:\n")
print(dt[, .N, by = DateSource][order(DateSource)])

cat("\nRows with missing event start date after join:", dt[is.na(EventStartDate), .N], "\n")
cat("Rows with missing final Date:", dt[is.na(Date), .N], "\n")

# ============================================================
# Drop unusable rows
# ============================================================

dt <- dt[
  !is.na(Date) &
    MatchID != "" &
    PlayerA_ID != "" &
    PlayerB_ID != "" &
    EventID != "" &
    !is.na(EventSeason) &
    !is.na(ScoreA) &
    !is.na(ScoreB)
]

dt <- dt[PlayerA_ID != PlayerB_ID]
dt <- dt[ScoreA >= 0 & ScoreB >= 0]

high_score_rows <- dt[ScoreA > 20 | ScoreB > 20]

if (nrow(high_score_rows) > 0L) {
  cat("\nSkipping", nrow(high_score_rows), "rows where a player's score is > 20.\n")
  print(high_score_rows[1:min(20, .N), .(
    MatchID, EventID, EventName, PlayerA_ID, PlayerB_ID,
    ScoreA, ScoreB, MatchDateRaw, DateSource, SourceFile
  )])
}

dt <- dt[!(ScoreA > 20 | ScoreB > 20)]

# ============================================================
# Preserve confirmed future matches before removing 0-0 rows
# ============================================================

zero_zero_rows <- dt[ScoreA == 0 & ScoreB == 0]

if (nrow(zero_zero_rows) > 0L) {
  cat(
    "\nFound",
    nrow(zero_zero_rows),
    "rows with 0-0 scores.\n"
  )
}

future_matches <- zero_zero_rows[
  !is.na(ScheduledDate) &
    ScheduledDate > Sys.time()
]

cat(
  "Future 0-0 matches found:",
  nrow(future_matches),
  "\n"
)

if (nrow(future_matches) > 0L) {
  
  # Put both players into one long table so we can determine
  # each player's earliest scheduled future match.
  player_future <- rbind(
    future_matches[, .(
      PlayerID = PlayerA_ID,
      OpponentID = PlayerB_ID,
      MatchID,
      EventID,
      EventName,
      ScheduledDate
    )],
    future_matches[, .(
      PlayerID = PlayerB_ID,
      OpponentID = PlayerA_ID,
      MatchID,
      EventID,
      EventName,
      ScheduledDate
    )]
  )
  
  # Find each player's earliest scheduled future time.
  player_future[
    ,
    EarliestDT := min(ScheduledDate),
    by = PlayerID
  ]
  
  player_next <- player_future[
    ScheduledDate == EarliestDT
  ]
  
  # A player must have exactly one possible opponent at that
  # earliest scheduled time.
  player_next[
    ,
    OpponentCount := uniqueN(OpponentID),
    by = PlayerID
  ]
  
  player_next_confirmed <- player_next[
    OpponentCount == 1L
  ]
  
  # A match is only confirmed when BOTH players regard this
  # same MatchID as their unique earliest future match.
  confirmed_ids <- player_next_confirmed[
    ,
    .N,
    by = MatchID
  ][N == 2L, MatchID]
  
  upcoming_matches <- future_matches[
    MatchID %in% confirmed_ids
  ]
  
  setorder(
    upcoming_matches,
    ScheduledDate,
    EventID,
    MatchID
  )
  
  upcoming_matches_out <- upcoming_matches[, .(
    MatchID,
    EventID,
    EventName,
    EventSeason,
    ScheduledDate = format(
      ScheduledDate,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    PlayerA_ID,
    PlayerB_ID
  )]
  
} else {
  
  upcoming_matches_out <- data.table(
    MatchID = character(),
    EventID = character(),
    EventName = character(),
    EventSeason = integer(),
    ScheduledDate = character(),
    PlayerA_ID = character(),
    PlayerB_ID = character()
  )
}

fwrite(
  upcoming_matches_out,
  OUTPUT_UPCOMING_MATCHES_CSV
)

cat(
  "Confirmed upcoming matches written:",
  nrow(upcoming_matches_out),
  "|",
  OUTPUT_UPCOMING_MATCHES_CSV,
  "\n"
)

# Remove all 0-0 placeholders before Elo calculation.
dt <- dt[!(ScoreA == 0 & ScoreB == 0)]

# ============================================================
# Secondary de-duplication
#
# Removes matches with different MatchIDs when they have:
#   - the same two players
#   - the same event
#   - the same day
#   - the same score
#
# Player order is standardised so A-v-B and B-v-A still match.
# ============================================================

dt[, PlayerLow := pmin(PlayerA_ID, PlayerB_ID)]
dt[, PlayerHigh := pmax(PlayerA_ID, PlayerB_ID)]

dt[, ScoreLow := fifelse(
  PlayerA_ID == PlayerLow,
  ScoreA,
  ScoreB
)]

dt[, ScoreHigh := fifelse(
  PlayerA_ID == PlayerLow,
  ScoreB,
  ScoreA
)]

dt[, MatchDay := as.Date(Date)]

secondary_duplicate_groups <- dt[
  ,
  .N,
  by = .(
    PlayerLow,
    PlayerHigh,
    EventID,
    MatchDay,
    ScoreLow,
    ScoreHigh
  )
][N > 1L]

cat(
  "\nSecondary duplicate groups found:",
  nrow(secondary_duplicate_groups),
  "\n"
)

if (nrow(secondary_duplicate_groups) > 0L) {
  secondary_duplicate_rows <- dt[
    secondary_duplicate_groups,
    on = .(
      PlayerLow,
      PlayerHigh,
      EventID,
      MatchDay,
      ScoreLow,
      ScoreHigh
    )
  ]
  
  cat("Rows belonging to secondary duplicate groups:\n")
  
  print(secondary_duplicate_rows[, .(
    MatchID,
    EventID,
    EventName,
    Date,
    PlayerA_ID,
    PlayerB_ID,
    ScoreA,
    ScoreB,
    SourceFile
  )])
}

rows_before_secondary_dedup <- nrow(dt)

dt[, DateSourcePreference := fcase(
  DateSource == "MatchDate_Modern", 1L,
  DateSource == "MatchDate_LastResort", 2L,
  DateSource == "EventStart_Fallback", 3L,
  DateSource == "EventStart_Pre2008Season", 4L,
  default = 5L
)]

setorder(
  dt,
  PlayerLow,
  PlayerHigh,
  EventID,
  MatchDay,
  ScoreLow,
  ScoreHigh,
  DateSourcePreference,
  MatchID
)

dt <- dt[
  !duplicated(
    dt,
    by = c(
      "PlayerLow",
      "PlayerHigh",
      "EventID",
      "MatchDay",
      "ScoreLow",
      "ScoreHigh"
    )
  )
]

cat(
  "Rows removed by secondary de-duplication:",
  rows_before_secondary_dedup - nrow(dt),
  "\n"
)

dt[, c(
  "PlayerLow",
  "PlayerHigh",
  "ScoreLow",
  "ScoreHigh",
  "MatchDay",
  "DateSourcePreference"
) := NULL]

dt[, WinnerFromScore := fifelse(
  ScoreA > ScoreB,
  PlayerA_ID,
  fifelse(ScoreB > ScoreA, PlayerB_ID, NA_character_)
)]

winner_mismatches <- dt[
  !is.na(WinnerFromScore) &
    WinnerID != "" &
    WinnerID != WinnerFromScore
]

if (nrow(winner_mismatches) > 0L) {
  cat("\nFound", nrow(winner_mismatches), "winner/score mismatches.\n")
  cat("Using score as source of truth for Elo calculations.\n")
  print(winner_mismatches[1:min(20, .N), .(
    MatchID, EventID, EventName, PlayerA_ID, PlayerB_ID,
    ScoreA, ScoreB, WinnerID, WinnerFromScore,
    MatchDateRaw, DateSource, SourceFile
  )])
}

setorder(
  dt,
  EventSeason,
  Date,
  EventID,
  RoundSort,
  MatchNumSort,
  TableNoSort,
  MatchID,
  PlayerA_ID,
  PlayerB_ID
)

cat("\nMatches to process after cleaning:", nrow(dt), "\n")
cat(
  "Date range:",
  format(min(dt$Date), "%Y-%m-%d"),
  "to",
  format(max(dt$Date), "%Y-%m-%d"),
  "\n"
)
cat(
  "Season range:",
  min(dt$EventSeason, na.rm = TRUE),
  "to",
  max(dt$EventSeason, na.rm = TRUE),
  "\n"
)

# ============================================================
# State helpers
# ============================================================

empty_state <- function() {
  data.table(
    PlayerID = character(),
    Rating = numeric(),
    MatchesPlayed = integer(),
    FramesPlayed = integer(),
    FirstMatchDate = as.POSIXct(character(), tz = "UTC"),
    LastMatchDate = as.POSIXct(character(), tz = "UTC"),
    EntryRating = numeric()
  )
}

write_checkpoint <- function(state, season) {
  out <- copy(state)
  out[, Rating := round(Rating, 6)]
  out[, EntryRating := round(EntryRating, 6)]
  out[, FirstMatchDate := format(FirstMatchDate, "%Y-%m-%d %H:%M:%S")]
  out[, LastMatchDate := format(LastMatchDate, "%Y-%m-%d %H:%M:%S")]
  
  out_file <- checkpoint_file_for_season(season)
  fwrite(out, out_file)
  cat("Wrote checkpoint:", out_file, "\n")
}

state_to_envs <- function(state) {
  ratings_env <- new.env(hash = TRUE, parent = emptyenv())
  matches_env <- new.env(hash = TRUE, parent = emptyenv())
  frames_env <- new.env(hash = TRUE, parent = emptyenv())
  first_date_env <- new.env(hash = TRUE, parent = emptyenv())
  last_date_env <- new.env(hash = TRUE, parent = emptyenv())
  entry_rating_env <- new.env(hash = TRUE, parent = emptyenv())
  
  if (nrow(state) > 0L) {
    for (i in seq_len(nrow(state))) {
      p <- state$PlayerID[i]
      assign(p, state$Rating[i], envir = ratings_env)
      assign(p, state$MatchesPlayed[i], envir = matches_env)
      assign(p, state$FramesPlayed[i], envir = frames_env)
      assign(p, state$FirstMatchDate[i], envir = first_date_env)
      assign(p, state$LastMatchDate[i], envir = last_date_env)
      assign(p, state$EntryRating[i], envir = entry_rating_env)
    }
  }
  
  list(
    ratings = ratings_env,
    matches = matches_env,
    frames = frames_env,
    first_date = first_date_env,
    last_date = last_date_env,
    entry_rating = entry_rating_env
  )
}

envs_to_state <- function(envs) {
  players <- ls(envs$ratings, all.names = TRUE)
  
  if (length(players) == 0L) {
    return(empty_state())
  }
  
  out <- data.table(
    PlayerID = players,
    Rating = as.numeric(unlist(mget(players, envir = envs$ratings))),
    MatchesPlayed = as.integer(unlist(mget(players, envir = envs$matches))),
    FramesPlayed = as.integer(unlist(mget(players, envir = envs$frames))),
    FirstMatchDate = as.POSIXct(
      unlist(mget(players, envir = envs$first_date)),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    LastMatchDate = as.POSIXct(
      unlist(mget(players, envir = envs$last_date)),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    EntryRating = as.numeric(unlist(mget(players, envir = envs$entry_rating)))
  )
  
  setorder(out, -Rating, PlayerID)
  out
}

# ============================================================
# Elo runner
# ============================================================

run_elo_segment <- function(dt_input,
                            state = empty_state(),
                            entry_mode = c("fixed", "retro"),
                            retro_start_map = NULL,
                            label = "segment") {
  entry_mode <- match.arg(entry_mode)
  n <- nrow(dt_input)
  
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Running Elo segment:", label, "\n")
  cat("Entry mode:", entry_mode, "\n")
  cat("Matches:", n, "\n")
  
  if (n == 0L) {
    return(list(
      history = data.table(),
      state = state
    ))
  }
  
  envs <- state_to_envs(state)
  
  PlayerAVec <- dt_input$PlayerA_ID
  PlayerBVec <- dt_input$PlayerB_ID
  ScoreAVec <- dt_input$ScoreA
  ScoreBVec <- dt_input$ScoreB
  DateVec <- dt_input$Date
  
  AFirstAppearance <- logical(n)
  BFirstAppearance <- logical(n)
  
  AGamesBefore <- integer(n)
  BGamesBefore <- integer(n)
  AFramesBefore <- integer(n)
  BFramesBefore <- integer(n)
  
  ARating_Before <- numeric(n)
  BRating_Before <- numeric(n)
  
  AStartRating <- numeric(n)
  BStartRating <- numeric(n)
  
  ABaseK <- numeric(n)
  BBaseK <- numeric(n)
  AKMultiplier <- numeric(n)
  BKMultiplier <- numeric(n)
  AEffectiveK <- numeric(n)
  BEffectiveK <- numeric(n)
  
  ExpectedA <- numeric(n)
  ExpectedB <- numeric(n)
  
  OppWeightA <- numeric(n)
  OppWeightB <- numeric(n)
  
  FramesA <- integer(n)
  FramesB <- integer(n)
  TotalFrames <- integer(n)
  
  ActualScoreA <- numeric(n)
  ActualScoreB <- numeric(n)
  
  DeltaA <- numeric(n)
  DeltaB <- numeric(n)
  
  ARating_After <- numeric(n)
  BRating_After <- numeric(n)
  AGamesAfter <- integer(n)
  BGamesAfter <- integer(n)
  AFramesAfter <- integer(n)
  BFramesAfter <- integer(n)
  
  get_entry_rating <- function(player_id) {
    if (
      entry_mode == "retro" &&
      !is.null(retro_start_map) &&
      player_id %in% names(retro_start_map) &&
      is.finite(as.numeric(retro_start_map[[player_id]]))
    ) {
      return(as.numeric(retro_start_map[[player_id]]))
    }
    
    BASELINE_START_RATING
  }
  
  for (i in seq_len(n)) {
    a <- PlayerAVec[i]
    b <- PlayerBVec[i]
    score_a <- ScoreAVec[i]
    score_b <- ScoreBVec[i]
    match_date <- DateVec[i]
    
    a_exists <- exists(a, envir = envs$ratings, inherits = FALSE)
    b_exists <- exists(b, envir = envs$ratings, inherits = FALSE)
    
    a_first <- !a_exists
    b_first <- !b_exists
    
    if (a_first) {
      a_start <- get_entry_rating(a)
      assign(a, a_start, envir = envs$ratings)
      assign(a, 0L, envir = envs$matches)
      assign(a, 0L, envir = envs$frames)
      assign(a, match_date, envir = envs$first_date)
      assign(a, match_date, envir = envs$last_date)
      assign(a, a_start, envir = envs$entry_rating)
    }
    
    if (b_first) {
      b_start <- get_entry_rating(b)
      assign(b, b_start, envir = envs$ratings)
      assign(b, 0L, envir = envs$matches)
      assign(b, 0L, envir = envs$frames)
      assign(b, match_date, envir = envs$first_date)
      assign(b, match_date, envir = envs$last_date)
      assign(b, b_start, envir = envs$entry_rating)
    }
    
    Ra <- get(a, envir = envs$ratings, inherits = FALSE)
    Rb <- get(b, envir = envs$ratings, inherits = FALSE)
    
    Ga <- get(a, envir = envs$matches, inherits = FALSE)
    Gb <- get(b, envir = envs$matches, inherits = FALSE)
    
    Fa_before <- get(a, envir = envs$frames, inherits = FALSE)
    Fb_before <- get(b, envir = envs$frames, inherits = FALSE)
    
    a_entry <- get(a, envir = envs$entry_rating, inherits = FALSE)
    b_entry <- get(b, envir = envs$entry_rating, inherits = FALSE)
    
    total_frames <- score_a + score_b
    
    Ea <- expected_score(Ra, Rb)
    Eb <- 1 - Ea
    
    Sa <- score_a / total_frames
    Sb <- score_b / total_frames
    
    base_k_a <- K_VALUE
    base_k_b <- K_VALUE
    mult_a <- 1
    mult_b <- 1
    effective_k_a <- K_VALUE
    effective_k_b <- K_VALUE
    weight_a <- 1
    weight_b <- 1
    
    delta_a <- K_VALUE * (score_a - total_frames * Ea)
    delta_b <- K_VALUE * (score_b - total_frames * Eb)
    
    Ra_new <- Ra + delta_a
    Rb_new <- Rb + delta_b
    
    Ga_new <- Ga + 1L
    Gb_new <- Gb + 1L
    
    Fa_new <- Fa_before + total_frames
    Fb_new <- Fb_before + total_frames
    
    assign(a, Ra_new, envir = envs$ratings)
    assign(b, Rb_new, envir = envs$ratings)
    
    assign(a, Ga_new, envir = envs$matches)
    assign(b, Gb_new, envir = envs$matches)
    
    assign(a, Fa_new, envir = envs$frames)
    assign(b, Fb_new, envir = envs$frames)
    
    assign(a, match_date, envir = envs$last_date)
    assign(b, match_date, envir = envs$last_date)
    
    AFirstAppearance[i] <- a_first
    BFirstAppearance[i] <- b_first
    
    AGamesBefore[i] <- Ga
    BGamesBefore[i] <- Gb
    AFramesBefore[i] <- Fa_before
    BFramesBefore[i] <- Fb_before
    
    ARating_Before[i] <- Ra
    BRating_Before[i] <- Rb
    
    AStartRating[i] <- a_entry
    BStartRating[i] <- b_entry
    
    ABaseK[i] <- base_k_a
    BBaseK[i] <- base_k_b
    AKMultiplier[i] <- mult_a
    BKMultiplier[i] <- mult_b
    AEffectiveK[i] <- effective_k_a
    BEffectiveK[i] <- effective_k_b
    
    ExpectedA[i] <- Ea
    ExpectedB[i] <- Eb
    
    OppWeightA[i] <- weight_a
    OppWeightB[i] <- weight_b
    
    FramesA[i] <- score_a
    FramesB[i] <- score_b
    TotalFrames[i] <- total_frames
    
    ActualScoreA[i] <- Sa
    ActualScoreB[i] <- Sb
    
    DeltaA[i] <- delta_a
    DeltaB[i] <- delta_b
    
    ARating_After[i] <- Ra_new
    BRating_After[i] <- Rb_new
    AGamesAfter[i] <- Ga_new
    BGamesAfter[i] <- Gb_new
    AFramesAfter[i] <- Fa_new
    BFramesAfter[i] <- Fb_new
    
    if (i %% 50000L == 0L) {
      cat("Processed", i, "matches (", round(100 * i / n, 1), "%)\n")
      flush.console()
    }
  }
  
  history <- copy(dt_input)
  history[, `:=`(
    AFirstAppearance = AFirstAppearance,
    BFirstAppearance = BFirstAppearance,
    AGamesBefore = AGamesBefore,
    BGamesBefore = BGamesBefore,
    AFramesBefore = AFramesBefore,
    BFramesBefore = BFramesBefore,
    ARating_Before = ARating_Before,
    BRating_Before = BRating_Before,
    AStartRating = AStartRating,
    BStartRating = BStartRating,
    ABaseK = ABaseK,
    BBaseK = BBaseK,
    AKMultiplier = AKMultiplier,
    BKMultiplier = BKMultiplier,
    AEffectiveK = AEffectiveK,
    BEffectiveK = BEffectiveK,
    ExpectedA = ExpectedA,
    ExpectedB = ExpectedB,
    OppWeightA = OppWeightA,
    OppWeightB = OppWeightB,
    FramesA = FramesA,
    FramesB = FramesB,
    TotalFrames = TotalFrames,
    ActualScoreA = ActualScoreA,
    ActualScoreB = ActualScoreB,
    DeltaA = DeltaA,
    DeltaB = DeltaB,
    ARating_After = ARating_After,
    BRating_After = BRating_After,
    AGamesAfter = AGamesAfter,
    BGamesAfter = BGamesAfter,
    AFramesAfter = AFramesAfter,
    BFramesAfter = BFramesAfter
  )]
  
  list(
    history = history,
    state = envs_to_state(envs)
  )
}

# ============================================================
# Build retrospective frame-performance starts from Pass 1
# ============================================================

solve_frame_performance <- function(opp_ratings,
                                    frames_won,
                                    total_frames,
                                    lower = RETRO_RATING_LOWER,
                                    upper = RETRO_RATING_UPPER) {
  opp_ratings <- as.numeric(opp_ratings)
  frames_won <- as.numeric(frames_won)
  total_frames <- as.numeric(total_frames)
  
  ok <- is.finite(opp_ratings) &
    is.finite(frames_won) &
    is.finite(total_frames) &
    total_frames > 0
  
  opp_ratings <- opp_ratings[ok]
  frames_won <- frames_won[ok]
  total_frames <- total_frames[ok]
  
  if (length(total_frames) == 0L) {
    return(NA_real_)
  }
  
  won <- sum(frames_won)
  total <- sum(total_frames)
  
  if (won <= 0) {
    return(max(lower, min(upper, min(opp_ratings) - 800)))
  }
  
  if (won >= total) {
    return(max(lower, min(upper, max(opp_ratings) + 800)))
  }
  
  f <- function(R) {
    sum(total_frames * expected_score(R, opp_ratings)) - won
  }
  
  flo <- f(lower)
  fhi <- f(upper)
  
  if (is.finite(flo) && is.finite(fhi) && flo * fhi <= 0) {
    return(uniroot(f, lower = lower, upper = upper, tol = 1e-8)$root)
  }
  
  p <- won / total
  p <- min(0.999, max(0.001, p))
  
  weighted_opp <- weighted.mean(opp_ratings, w = total_frames)
  estimate <- weighted_opp + 400 * log10(p / (1 - p))
  
  max(lower, min(upper, estimate))
}

build_retro_start_map <- function(pass1_history, n_frames = RETRO_FRAMES_N) {
  player_games <- rbindlist(
    list(
      pass1_history[, .(
        PlayerID = PlayerA_ID,
        OppRating = BRating_Before,
        FramesWon = FramesA,
        TotalFrames = TotalFrames,
        FramesBefore = AFramesBefore,
        Date = Date,
        MatchID = MatchID
      )],
      pass1_history[, .(
        PlayerID = PlayerB_ID,
        OppRating = ARating_Before,
        FramesWon = FramesB,
        TotalFrames = TotalFrames,
        FramesBefore = BFramesBefore,
        Date = Date,
        MatchID = MatchID
      )]
    ),
    use.names = TRUE
  )
  
  player_games <- player_games[
    PlayerID != "" &
      is.finite(OppRating) &
      is.finite(FramesWon) &
      is.finite(TotalFrames) &
      TotalFrames > 0
  ]
  
  setorder(player_games, PlayerID, FramesBefore, Date, MatchID)
  
  # Include complete matches until the player crosses the evidence threshold.
  # We never split a match, so FramesUsed may be slightly above n_frames.
  early_games <- player_games[FramesBefore < n_frames]
  
  retro_dt <- early_games[, .(
    GamesUsed = .N,
    FramesUsed = sum(TotalFrames),
    FramesWon = sum(FramesWon),
    FrameRate = sum(FramesWon) / sum(TotalFrames),
    RetroStart = solve_frame_performance(
      OppRating,
      FramesWon,
      TotalFrames
    )
  ), by = PlayerID]
  
  retro_dt <- retro_dt[
    FramesUsed >= n_frames &
      is.finite(RetroStart)
  ]
  
  retro_dt[, `:=`(
    BaselineStart = BASELINE_START_RATING,
    RetroAdjustment = RetroStart - BASELINE_START_RATING
  )]
  
  retro_dt[player_lookup, on = .(PlayerID = ID), `:=`(
    PlayerName = i.Name,
    Nationality = i.Nationality
  )]
  
  setcolorder(retro_dt, c(
    "PlayerID",
    "PlayerName",
    "Nationality",
    "GamesUsed",
    "FramesUsed",
    "FramesWon",
    "FrameRate",
    "BaselineStart",
    "RetroStart",
    "RetroAdjustment"
  ))
  
  setorder(retro_dt, -RetroStart, PlayerID)
  
  retro_map <- setNames(retro_dt$RetroStart, retro_dt$PlayerID)
  
  list(
    table = retro_dt,
    map = retro_map
  )
}

# ============================================================
# Output helpers
# ============================================================

add_player_names_to_history <- function(history) {
  out <- copy(history)
  
  out[player_lookup, on = .(PlayerA_ID = ID), `:=`(
    PlayerA_Name = i.Name,
    PlayerA_Nationality = i.Nationality
  )]
  
  out[player_lookup, on = .(PlayerB_ID = ID), `:=`(
    PlayerB_Name = i.Name,
    PlayerB_Nationality = i.Nationality
  )]
  
  out[player_lookup, on = .(WinnerID = ID), `:=`(
    Winner_Name = i.Name,
    Winner_Nationality = i.Nationality
  )]
  
  out[player_lookup, on = .(WinnerFromScore = ID), `:=`(
    WinnerFromScore_Name = i.Name,
    WinnerFromScore_Nationality = i.Nationality
  )]
  
  out
}

format_history_for_write <- function(history) {
  out <- add_player_names_to_history(history)
  
  out <- out[, .(
    MatchID,
    MatchDate = format(Date, "%Y-%m-%d %H:%M:%S"),
    DateSource,
    EventID,
    EventName,
    EventSeason,
    EventStartDate = format(EventStartDate, "%Y-%m-%d %H:%M:%S"),
    EventEndDate = format(EventEndDate, "%Y-%m-%d %H:%M:%S"),
    RawMatchDate = MatchDateRaw,
    SourceFile,
    
    PlayerA_ID,
    PlayerA_Name,
    PlayerA_Nationality,
    PlayerB_ID,
    PlayerB_Name,
    PlayerB_Nationality,
    
    WinnerID,
    Winner_Name,
    Winner_Nationality,
    WinnerFromScore,
    WinnerFromScore_Name,
    WinnerFromScore_Nationality,
    
    ScoreA,
    ScoreB,
    FramesA,
    FramesB,
    TotalFrames,
    
    AFirstAppearance,
    BFirstAppearance,
    AGamesBefore,
    BGamesBefore,
    AFramesBefore,
    BFramesBefore,
    ARating_Before,
    BRating_Before,
    AStartRating,
    BStartRating,
    
    ABaseK,
    BBaseK,
    AKMultiplier,
    BKMultiplier,
    AEffectiveK,
    BEffectiveK,
    
    ExpectedA,
    ExpectedB,
    OppWeightA,
    OppWeightB,
    ActualScoreA,
    ActualScoreB,
    DeltaA,
    DeltaB,
    ARating_After,
    BRating_After,
    AGamesAfter,
    BGamesAfter,
    AFramesAfter,
    BFramesAfter
  )]
  
  out
}

format_final_for_write <- function(state) {
  out <- copy(state)
  
  out[player_lookup, on = .(PlayerID = ID), `:=`(
    PlayerName = i.Name,
    Nationality = i.Nationality
  )]
  
  out[, `:=`(
    Rating = round(Rating, 2),
    EntryRating = round(EntryRating, 2),
    Method = "TwoPass_Retro200Frames_ZeroSumK5",
    KValue = K_VALUE,
    BaselineStartRating = BASELINE_START_RATING,
    RetroFramesN = RETRO_FRAMES_N,
    NewPlayerMatches = NEW_PLAYER_MATCHES,
    NewPlayerKMultiplier = NEW_PLAYER_K_MULTIPLIER,
    ProvisionalFrameThreshold = PROVISIONAL_FRAME_THRESHOLD,
    MinOpponentWeight = MIN_OPP_WEIGHT
  )]
  
  out[, FirstMatchDate := format(FirstMatchDate, "%Y-%m-%d %H:%M:%S")]
  out[, LastMatchDate := format(LastMatchDate, "%Y-%m-%d %H:%M:%S")]
  
  setcolorder(out, c(
    "PlayerID",
    "PlayerName",
    "Nationality",
    "Rating",
    "MatchesPlayed",
    "FramesPlayed",
    "FirstMatchDate",
    "LastMatchDate",
    "EntryRating",
    "Method",
    "KValue",
    "BaselineStartRating",
    "RetroFramesN",
    "NewPlayerMatches",
    "NewPlayerKMultiplier",
    "ProvisionalFrameThreshold",
    "MinOpponentWeight"
  ))
  
  setorder(out, -Rating, PlayerID)
  out
}

write_season_snapshot <- function(state, season, season_end_date) {
  snapshot <- copy(state)
  
  snapshot[player_lookup, on = .(PlayerID = ID), `:=`(
    PlayerName = i.Name,
    Nationality = i.Nationality
  )]
  
  inactive_days_int <- as.integer(round(ACTIVE_YEARS * 365.25))
  active_cutoff <- as.Date(season_end_date) - inactive_days_int
  
  snapshot[, ListedFrames := FramesPlayed]
  snapshot[, Listable := ListedFrames >= MIN_LIST_FRAMES]
  snapshot[, Active := as.Date(LastMatchDate) >= active_cutoff]
  snapshot[, RankEligible := Listable & Active]
  
  snapshot <- snapshot[RankEligible == TRUE]
  
  if (nrow(snapshot) > 0L) {
    setorder(snapshot, -Rating, PlayerName, PlayerID)
    snapshot[, Rank := frank(-Rating, ties.method = "min")]
  } else {
    snapshot[, Rank := integer()]
  }
  
  snapshot[, `:=`(
    Season = season,
    SeasonLabel = paste0(season, "/", season + 1L),
    SeasonEndDate = format(as.Date(season_end_date), "%Y-%m-%d"),
    Rating = round(Rating, 2),
    EntryRating = round(EntryRating, 2),
    FirstMatchDate = format(FirstMatchDate, "%Y-%m-%d %H:%M:%S"),
    LastMatchDate = format(LastMatchDate, "%Y-%m-%d %H:%M:%S")
  )]
  
  snapshot <- snapshot[, .(
    Season,
    SeasonLabel,
    SeasonEndDate,
    Rank,
    PlayerID,
    PlayerName,
    Nationality,
    Rating,
    MatchesPlayed,
    FramesPlayed,
    ListedFrames,
    FirstMatchDate,
    LastMatchDate,
    EntryRating,
    Active,
    Listable
  )]
  
  out_file <- season_snapshot_file_for_season(season)
  fwrite(snapshot, out_file)
  
  cat("Wrote season snapshot:", out_file, "players:", nrow(snapshot), "\n")
}

# ============================================================
# Pass 1 - fixed 2600 starts, constant zero-sum K
# ============================================================

cat("\nStarting Pass 1 from a clean state.\n")
cat("All players start at", BASELINE_START_RATING, "for calibration.\n")

pass1 <- run_elo_segment(
  dt_input = dt,
  state = empty_state(),
  entry_mode = "fixed",
  retro_start_map = NULL,
  label = "Pass 1 - fixed 2600 starts"
)

retro <- build_retro_start_map(
  pass1_history = pass1$history,
  n_frames = RETRO_FRAMES_N
)

fwrite(retro$table, OUTPUT_RETRO_STARTS_CSV)

cat("\nBuilt retrospective starts for", length(retro$map), "players.\n")
cat("Evidence threshold:", RETRO_FRAMES_N, "frames.\n")
cat("Retro start file:", OUTPUT_RETRO_STARTS_CSV, "\n")

if (nrow(retro$table) > 0L) {
  cat(
    "Retro start range:",
    round(min(retro$table$RetroStart), 1),
    "to",
    round(max(retro$table$RetroStart), 1),
    "\n"
  )
  cat(
    "Mean retro adjustment:",
    round(mean(retro$table$RetroAdjustment), 1),
    "\n"
  )
}

# ============================================================
# Pass 2 - final published history
# ============================================================

all_seasons <- sort(unique(dt$EventSeason))
max_season <- max(all_seasons, na.rm = TRUE)

if (FINALISE_CURRENT_SEASON) {
  completed_seasons <- all_seasons
} else {
  completed_seasons <- all_seasons[all_seasons < max_season]
}

cat("\nAvailable seasons:", paste(all_seasons, collapse = ", "), "\n")
cat("Current/live season:", max_season, "\n")
cat("Completed seasons eligible for checkpoints:", paste(completed_seasons, collapse = ", "), "\n")

if (FORCE_FULL_REBUILD || !is.na(REBUILD_FROM_SEASON)) {
  cat(
    "\nNote: this two-pass method always rebuilds the full rating history. ",
    "FORCE_FULL_REBUILD and REBUILD_FROM_SEASON no longer change the start point.\n",
    sep = ""
  )
}

# A retrospective start can change when a newer player reaches 200 frames,
# so using an old rating checkpoint as the computational starting state would
# make the historical pass internally inconsistent. Checkpoints are therefore
# regenerated as outputs, not loaded as inputs.
old_checkpoint_files <- list.files(
  CHECKPOINT_DIR,
  pattern = "^checkpoint_season_\\d{4}_end\\.csv$",
  full.names = TRUE
)
old_history_files <- list.files(
  SEASON_HISTORY_DIR,
  pattern = "^snooker_elo_match_history_season_\\d{4}\\.csv$",
  full.names = TRUE
)
old_snapshot_files <- list.files(
  SEASON_SNAPSHOT_DIR,
  pattern = "^(snapshot_season_\\d{4}|snapshot_current)\\.csv$",
  full.names = TRUE
)

unlink(old_checkpoint_files)
unlink(old_history_files)
unlink(old_snapshot_files)

state <- empty_state()
final_history_list <- vector("list", length(all_seasons))

for (j in seq_along(all_seasons)) {
  season <- all_seasons[j]
  season_dt <- dt[EventSeason == season]
  
  setorder(
    season_dt,
    Date,
    EventID,
    RoundSort,
    MatchNumSort,
    TableNoSort,
    MatchID,
    PlayerA_ID,
    PlayerB_ID
  )
  
  run <- run_elo_segment(
    dt_input = season_dt,
    state = state,
    entry_mode = "retro",
    retro_start_map = retro$map,
    label = paste0("Pass 2 final - EventSeason ", season, " (", season, "/", season + 1L, ")")
  )
  
  state <- run$state
  final_history_list[[j]] <- run$history
  
  season_history_out <- format_history_for_write(run$history)
  season_history_file <- season_history_file_for_season(season)
  fwrite(season_history_out, season_history_file)
  
  cat("Wrote season match history:", season_history_file, "\n")
  
  season_end_date <- max(season_dt$Date, na.rm = TRUE)
  
  if (season %in% completed_seasons) {
    write_checkpoint(state, season)
    write_season_snapshot(state, season, season_end_date)
  } else {
    cat("Season", season, "treated as live/current. No final checkpoint written.\n")
    
    temp_snapshot_path <- season_snapshot_file_for_season(season)
    current_snapshot_file <- file.path(SEASON_SNAPSHOT_DIR, "snapshot_current.csv")
    
    write_season_snapshot(state, season, season_end_date)
    
    if (file.exists(temp_snapshot_path)) {
      file.copy(temp_snapshot_path, current_snapshot_file, overwrite = TRUE)
      cat("Wrote current live snapshot:", current_snapshot_file, "\n")
    }
  }
}

# ============================================================
# Build combined final match history
# ============================================================

history_all_raw <- rbindlist(
  final_history_list,
  use.names = TRUE,
  fill = TRUE
)

history_all <- format_history_for_write(history_all_raw)

if ("MatchDate" %in% names(history_all)) {
  history_all[, MatchDateSort := parse_dt_multi(MatchDate)]
  setorder(history_all, MatchDateSort, EventSeason, EventID, MatchID)
  history_all[, MatchDateSort := NULL]
}

fwrite(history_all, OUTPUT_MATCH_HISTORY_CSV)

cat("\nWrote combined match history:", OUTPUT_MATCH_HISTORY_CSV, "\n")
cat("Combined match history rows:", nrow(history_all), "\n")

# ============================================================
# Final zero-sum checks
# ============================================================

history_all[, MatchDrift :=
              (ARating_After + BRating_After) -
              (ARating_Before + BRating_Before)
]

cat("\nFinal Elo drift check:\n")
cat("  Total match drift:", format(sum(history_all$MatchDrift), scientific = FALSE), "\n")
cat("  Maximum absolute match drift:", format(max(abs(history_all$MatchDrift)), scientific = FALSE), "\n")

history_all[, MatchDrift := NULL]

# ============================================================
# Write final ratings
# ============================================================

final_out <- format_final_for_write(state)
fwrite(final_out, OUTPUT_FINAL_RATINGS_CSV)

cat("\nWrote final ratings:", OUTPUT_FINAL_RATINGS_CSV, "\n")
cat("Final rating rows:", nrow(final_out), "\n")
cat("Unmatched final rating names:", final_out[is.na(PlayerName), .N], "\n")

cat("\nDone.\n")
cat("Method: two pass, retrospective 200-frame starts, constant zero-sum K=", K_VALUE, ".\n", sep = "")
cat("Current/live EventSeason:", max_season, "\n")
cat("Final ratings:", OUTPUT_FINAL_RATINGS_CSV, "\n")
cat("Match history:", OUTPUT_MATCH_HISTORY_CSV, "\n")
cat("Retro starts:", OUTPUT_RETRO_STARTS_CSV, "\n")
cat("Checkpoints:", CHECKPOINT_DIR, "\n")
cat("Season snapshots:", SEASON_SNAPSHOT_DIR, "\n")
