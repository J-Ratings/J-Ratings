library(dplyr)
library(readr)
library(jsonlite)
library(stringr)
library(data.table)

options(stringsAsFactors = FALSE)

# ============================================================
# Export SNOOKER ratings + history to JSON for website
#
# Inputs:
#   C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Snooker/Elo
#     - snooker_elo_final_ratings.csv
#     - snooker_elo_match_history.csv
#     - season_snapshots/snapshot_season_YYYY.csv
#     - season_snapshots/snapshot_current.csv
#
# Outputs:
#   C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings/Snooker/data
#     - meta.json
#     - players.json
#     - history/<player_id>.json
#     - games/<player_id>.json
#     - snapshots/seasons.json
#     - snapshots/<season>.json
#     - snapshots/current.json
#
# Notes:
#   - rating method now comes from the single-pass calculation:
#       * all players start at 2600
#       * first 20 matches use double K
#       * opponent-frame dampening is applied
#   - history rating = post-match Elo on that date
#   - history rank = world rank at end of that match timestamp
#   - games rank = player world rank after that match timestamp
#   - snapshots are season-end snapshots, not calendar-year snapshots
#   - player must have at least MIN_LIST_FRAMES cumulative frames to be ranked
#   - player must have played within ACTIVE_YEARS at that historical point to be ranked
# ============================================================

# -----------------------------
# Paths
# -----------------------------
repo_dir <- "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"
site_dir <- file.path(repo_dir, "Snooker")

elo_src_dir <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/Snooker/Elo"
season_snapshots_src_dir <- file.path(elo_src_dir, "season_snapshots")

base_data_dir <- file.path(site_dir, "data")
history_out <- file.path(base_data_dir, "history")
games_out <- file.path(base_data_dir, "games")
snapshots_out <- file.path(base_data_dir, "snapshots")

dir.create(base_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(history_out, recursive = TRUE, showWarnings = FALSE)
dir.create(games_out, recursive = TRUE, showWarnings = FALSE)
dir.create(snapshots_out, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Rules
# -----------------------------
MIN_LIST_FRAMES <- 200L
ACTIVE_YEARS <- 2L

# -----------------------------
# Input files
# -----------------------------
final_csv <- file.path(elo_src_dir, "snooker_elo_final_ratings.csv")
hist_csv  <- file.path(elo_src_dir, "snooker_elo_match_history.csv")

if (!file.exists(final_csv)) stop("Missing file: ", final_csv)
if (!file.exists(hist_csv))  stop("Missing file: ", hist_csv)

# -----------------------------
# Load CSVs
# -----------------------------
final <- read_csv(final_csv, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
mhist <- read_csv(hist_csv, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

# -----------------------------
# Required columns
# -----------------------------
required_final <- c(
  "PlayerID",
  "PlayerName",
  "Nationality",
  "Rating",
  "MatchesPlayed"
)

required_hist <- c(
  "MatchID",
  "MatchDate",
  "EventName",
  "EventSeason",
  "PlayerA_ID",
  "PlayerA_Name",
  "PlayerB_ID",
  "PlayerB_Name",
  "ScoreA",
  "ScoreB",
  "ARating_Before",
  "BRating_Before",
  "ARating_After",
  "BRating_After",
  "DeltaA",
  "DeltaB",
  "AGamesAfter",
  "BGamesAfter"
)

miss_final <- setdiff(required_final, names(final))
miss_hist  <- setdiff(required_hist, names(mhist))

if (length(miss_final) > 0) {
  stop("Missing columns in final CSV: ", paste(miss_final, collapse = ", "))
}

if (length(miss_hist) > 0) {
  stop("Missing columns in history CSV: ", paste(miss_hist, collapse = ", "))
}

# -----------------------------
# Clean / standardise final ratings
# -----------------------------
final <- final %>%
  mutate(
    PlayerID = trimws(as.character(PlayerID)),
    PlayerName = trimws(as.character(PlayerName)),
    Nationality = trimws(as.character(Nationality)),
    Nationality = na_if(Nationality, ""),
    Rating = suppressWarnings(as.numeric(Rating)),
    MatchesPlayed = suppressWarnings(as.integer(MatchesPlayed)),
    FramesPlayed = if ("FramesPlayed" %in% names(.)) {
      suppressWarnings(as.integer(FramesPlayed))
    } else {
      NA_integer_
    }
  ) %>%
  filter(PlayerID != "", PlayerName != "", !is.na(Rating))

# -----------------------------
# Clean / standardise match history
# -----------------------------
mhist <- mhist %>%
  mutate(
    MatchID = trimws(as.character(MatchID)),
    MatchDate = as.POSIXct(MatchDate, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    EventName = trimws(as.character(EventName)),
    EventSeason = suppressWarnings(as.integer(EventSeason)),
    
    PlayerA_ID = trimws(as.character(PlayerA_ID)),
    PlayerA_Name = trimws(as.character(PlayerA_Name)),
    PlayerB_ID = trimws(as.character(PlayerB_ID)),
    PlayerB_Name = trimws(as.character(PlayerB_Name)),
    
    ScoreA = suppressWarnings(as.integer(ScoreA)),
    ScoreB = suppressWarnings(as.integer(ScoreB)),
    
    ARating_Before = suppressWarnings(as.numeric(ARating_Before)),
    BRating_Before = suppressWarnings(as.numeric(BRating_Before)),
    ARating_After = suppressWarnings(as.numeric(ARating_After)),
    BRating_After = suppressWarnings(as.numeric(BRating_After)),
    
    DeltaA = suppressWarnings(as.numeric(DeltaA)),
    DeltaB = suppressWarnings(as.numeric(DeltaB)),
    
    AGamesAfter = suppressWarnings(as.integer(AGamesAfter)),
    BGamesAfter = suppressWarnings(as.integer(BGamesAfter))
  ) %>%
  filter(
    !is.na(MatchDate),
    !is.na(EventSeason),
    PlayerA_ID != "",
    PlayerB_ID != "",
    !is.na(ScoreA),
    !is.na(ScoreB)
  )

if (nrow(mhist) == 0L) {
  stop("No usable match history rows after cleaning.")
}

# -----------------------------
# Basic current metadata
# -----------------------------
asof_date <- max(as.Date(mhist$MatchDate), na.rm = TRUE)
current_season <- max(mhist$EventSeason, na.rm = TRUE)
latest_completed_season <- current_season - 1L

# -----------------------------
# Last played per player
# -----------------------------
player_last <- bind_rows(
  mhist %>% transmute(PlayerID = PlayerA_ID, last_played = as.Date(MatchDate)),
  mhist %>% transmute(PlayerID = PlayerB_ID, last_played = as.Date(MatchDate))
) %>%
  filter(PlayerID != "", !is.na(last_played)) %>%
  group_by(PlayerID) %>%
  summarise(last_played = max(last_played), .groups = "drop")

# -----------------------------
# Frame totals per player
# -----------------------------
player_frames <- bind_rows(
  mhist %>%
    transmute(
      PlayerID = PlayerA_ID,
      frames_played = as.integer(ScoreA + ScoreB)
    ),
  mhist %>%
    transmute(
      PlayerID = PlayerB_ID,
      frames_played = as.integer(ScoreA + ScoreB)
    )
) %>%
  filter(PlayerID != "", !is.na(frames_played)) %>%
  group_by(PlayerID) %>%
  summarise(
    listed_frames = sum(frames_played, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# meta.json
# -----------------------------
meta <- list(
  asof = format(asof_date, "%Y-%m-%d"),
  matches = dplyr::n_distinct(mhist$MatchID),
  current_season = current_season,
  current_season_label = paste0(current_season, "/", current_season + 1L),
  latest_completed_season = latest_completed_season,
  latest_completed_season_label = paste0(latest_completed_season, "/", latest_completed_season + 1L),
  snapshot_type = "season",
  min_list_frames = MIN_LIST_FRAMES,
  active_years = ACTIVE_YEARS,
  history_has_world_rank = TRUE,
  games_have_world_rank = TRUE,
  rating_method = "Single-pass Elo. Players start at 2600. First 20 matches use double K. Opponent-frame dampening is applied.",
  rank_method = paste0(
    "Ranked by latest known Elo at each match timestamp. ",
    "Players need at least ", MIN_LIST_FRAMES, " cumulative frames and must have played within ",
    ACTIVE_YEARS, " years at that historical point."
  )
)

write_json(
  meta,
  file.path(base_data_dir, "meta.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote meta.json (asof =", meta$asof, ")\n")

# -----------------------------
# Long history per player
# history = post-match rating
# -----------------------------
hist_long <- bind_rows(
  mhist %>%
    transmute(
      PlayerID = PlayerA_ID,
      PlayerName = PlayerA_Name,
      date = as.Date(MatchDate),
      datetime = MatchDate,
      season = EventSeason,
      event = EventName,
      rating = ARating_After,
      match_frames = as.integer(ScoreA + ScoreB),
      match_id = MatchID
    ),
  mhist %>%
    transmute(
      PlayerID = PlayerB_ID,
      PlayerName = PlayerB_Name,
      date = as.Date(MatchDate),
      datetime = MatchDate,
      season = EventSeason,
      event = EventName,
      rating = BRating_After,
      match_frames = as.integer(ScoreA + ScoreB),
      match_id = MatchID
    )
) %>%
  filter(PlayerID != "", !is.na(datetime), !is.na(rating), !is.na(match_frames)) %>%
  arrange(PlayerID, datetime, match_id) %>%
  group_by(PlayerID) %>%
  mutate(cum_frames = cumsum(match_frames)) %>%
  ungroup()

# -----------------------------
# Historical world rank
#
# Rank is calculated at each match timestamp.
# This version walks forward through time and keeps current player state,
# rather than repeatedly scanning the whole history.
# -----------------------------
cat("Building historical world ranks...\n")

rank_dt <- as.data.table(hist_long)
rank_dt[, date := as.Date(date)]
rank_dt[, datetime := as.POSIXct(datetime, tz = "UTC")]
setorder(rank_dt, datetime, match_id, PlayerID)

rank_times <- sort(unique(rank_dt$datetime))
inactive_days_int <- as.integer(round(ACTIVE_YEARS * 365.25))

state <- data.table(
  PlayerID = character(),
  PlayerName = character(),
  rating = numeric(),
  cum_frames = numeric(),
  date = as.Date(character())
)
setkey(state, PlayerID)

rank_rows <- vector("list", length(rank_times))

for (i in seq_along(rank_times)) {
  t <- rank_times[i]
  cutoff_date <- as.Date(t) - inactive_days_int
  
  updates <- rank_dt[datetime == t]
  
  if (nrow(updates) > 0L) {
    setorder(updates, PlayerID, datetime, match_id)
    updates <- updates[, .SD[.N], by = PlayerID]
    
    updates_state <- updates[, .(
      PlayerID,
      PlayerName,
      rating,
      cum_frames,
      date
    )]
    setkey(updates_state, PlayerID)
    
    # Update existing players
    state[updates_state, `:=`(
      PlayerName = i.PlayerName,
      rating = i.rating,
      cum_frames = i.cum_frames,
      date = i.date
    )]
    
    # Add new players
    new_players <- updates_state[!state, on = "PlayerID"]
    
    if (nrow(new_players) > 0L) {
      state <- rbindlist(
        list(state, new_players),
        use.names = TRUE,
        fill = TRUE
      )
      setkey(state, PlayerID)
    }
  }
  
  current_ratings <- state[
    is.finite(rating) &
      is.finite(cum_frames) &
      cum_frames >= MIN_LIST_FRAMES &
      !is.na(date) &
      date >= cutoff_date
  ]
  
  if (nrow(current_ratings) > 0L) {
    setorder(current_ratings, -rating, PlayerName, PlayerID)
    current_ratings[, rank := frank(-rating, ties.method = "min")]
    current_ratings[, rank_time := t]
    
    rank_rows[[i]] <- current_ratings[, .(
      PlayerID,
      rank_time,
      rank = as.integer(rank)
    )]
  } else {
    rank_rows[[i]] <- data.table(
      PlayerID = character(),
      rank_time = as.POSIXct(character(), tz = "UTC"),
      rank = integer()
    )
  }
  
  if (i %% 500L == 0L) {
    cat("Rank timestamps processed:", i, "of", length(rank_times), "\n")
  }
}

rank_tbl <- rbindlist(rank_rows, use.names = TRUE, fill = TRUE)

hist_long <- hist_long %>%
  left_join(
    as_tibble(rank_tbl),
    by = c("PlayerID" = "PlayerID", "datetime" = "rank_time")
  )

cat("Historical rank rows:", nrow(rank_tbl), "\n")

# -----------------------------
# Daily history per player
# history = last post-match rating on each day
# -----------------------------
hist_daily <- hist_long %>%
  arrange(PlayerID, datetime, match_id) %>%
  group_by(PlayerID, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# -----------------------------
# Peak per player
# Peak only counts from MIN_LIST_FRAMES cumulative frames onwards
# -----------------------------
peak_tbl <- hist_long %>%
  filter(cum_frames >= MIN_LIST_FRAMES) %>%
  arrange(PlayerID, datetime, match_id) %>%
  group_by(PlayerID) %>%
  slice_max(order_by = rating, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    PlayerID,
    peak = as.integer(round(rating)),
    peak_date = format(date, "%Y-%m-%d")
  )

# -----------------------------
# players.json
# -----------------------------
cutoff_date <- as.Date(asof_date) - 365L * ACTIVE_YEARS

players_tbl <- final %>%
  transmute(
    id = as.character(PlayerID),
    name = as.character(PlayerName),
    display_name = as.character(PlayerName),
    nationality = as.character(Nationality),
    rating = as.integer(round(Rating)),
    games = as.integer(MatchesPlayed),
    frames = ifelse(is.na(FramesPlayed), NA_integer_, as.integer(FramesPlayed))
  ) %>%
  left_join(player_last, by = c("id" = "PlayerID")) %>%
  left_join(peak_tbl, by = c("id" = "PlayerID")) %>%
  left_join(player_frames, by = c("id" = "PlayerID")) %>%
  mutate(
    listed_frames = ifelse(is.na(listed_frames), 0L, as.integer(listed_frames)),
    frames = ifelse(is.na(frames), listed_frames, frames),
    last_played = ifelse(is.na(last_played), NA_character_, format(last_played, "%Y-%m-%d")),
    active = !is.na(last_played) & as.Date(last_played) >= cutoff_date,
    listable = listed_frames >= MIN_LIST_FRAMES
  ) %>%
  arrange(desc(rating), name)

write_json(
  players_tbl,
  file.path(base_data_dir, "players.json"),
  auto_unbox = TRUE,
  pretty = FALSE,
  na = "null"
)

cat(
  "Wrote players.json (n =", nrow(players_tbl),
  ", active =", sum(players_tbl$active, na.rm = TRUE),
  ", listable =", sum(players_tbl$listable, na.rm = TRUE),
  ", cutoff =", format(cutoff_date, "%Y-%m-%d"), ")\n"
)

all_ids <- players_tbl$id

# -----------------------------
# Per-player history JSON
# -----------------------------
n_hist_written <- 0L

for (pid in all_ids) {
  df <- hist_daily %>%
    filter(PlayerID == pid) %>%
    arrange(datetime, match_id) %>%
    transmute(
      date = format(date, "%Y-%m-%d"),
      season = ifelse(is.na(season), NA_character_, as.character(season)),
      rating = as.integer(round(rating)),
      rank = as.integer(rank),
      event = as.character(event)
    )
  
  if (nrow(df) > 0L) {
    write_json(
      df,
      file.path(history_out, paste0(pid, ".json")),
      auto_unbox = TRUE,
      pretty = FALSE,
      na = "null"
    )
    n_hist_written <- n_hist_written + 1L
  }
}

cat("Wrote history files:", n_hist_written, "\n")

# -----------------------------
# Season snapshots
#
# Source:
#   Elo/season_snapshots/snapshot_season_YYYY.csv
#   Elo/season_snapshots/snapshot_current.csv
#
# Website output:
#   snapshots/seasons.json
#   snapshots/YYYY.json
#   snapshots/current.json
# -----------------------------
if (!dir.exists(season_snapshots_src_dir)) {
  stop("Missing season snapshots directory: ", season_snapshots_src_dir)
}

season_snapshot_files <- list.files(
  season_snapshots_src_dir,
  pattern = "^snapshot_season_\\d{4}\\.csv$",
  full.names = TRUE
)

extract_season_from_snapshot <- function(path) {
  suppressWarnings(as.integer(sub("^snapshot_season_(\\d{4})\\.csv$", "\\1", basename(path))))
}

season_snapshot_info <- data.table(
  file = season_snapshot_files,
  season = extract_season_from_snapshot(season_snapshot_files)
)

season_snapshot_info <- season_snapshot_info[!is.na(season)]

# Avoid exporting the live/current season as if it were final.
# The live table is exported separately as current.json.
season_snapshot_info <- season_snapshot_info[season < current_season]

setorder(season_snapshot_info, season)

snapshot_seasons <- season_snapshot_info$season

write_json(
  snapshot_seasons,
  file.path(snapshots_out, "seasons.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote snapshots/seasons.json (n =", length(snapshot_seasons), ")\n")

n_snapshots_written <- 0L

for (i in seq_len(nrow(season_snapshot_info))) {
  season <- season_snapshot_info$season[i]
  f <- season_snapshot_info$file[i]
  
  snap <- read_csv(f, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  
  required_snap_cols <- c(
    "Season",
    "SeasonLabel",
    "SeasonEndDate",
    "Rank",
    "PlayerID",
    "PlayerName",
    "Nationality",
    "Rating",
    "MatchesPlayed",
    "FramesPlayed",
    "ListedFrames",
    "LastMatchDate"
  )
  
  miss_snap <- setdiff(required_snap_cols, names(snap))
  
  if (length(miss_snap) > 0L) {
    stop("Missing columns in season snapshot ", basename(f), ": ", paste(miss_snap, collapse = ", "))
  }
  
  snapshot_tbl <- snap %>%
    transmute(
      season = as.integer(Season),
      season_label = as.character(SeasonLabel),
      season_end_date = as.character(SeasonEndDate),
      rank = as.integer(Rank),
      id = as.character(PlayerID),
      name = as.character(PlayerName),
      display_name = as.character(PlayerName),
      nationality = as.character(Nationality),
      rating = as.integer(round(as.numeric(Rating))),
      games = as.integer(MatchesPlayed),
      frames = as.integer(FramesPlayed),
      listed_frames = as.integer(ListedFrames),
      last_played = format(as.Date(LastMatchDate), "%Y-%m-%d"),
      active = if ("Active" %in% names(snap)) as.logical(Active) else TRUE,
      listable = if ("Listable" %in% names(snap)) as.logical(Listable) else TRUE
    ) %>%
    arrange(rank, desc(rating), name)
  
  write_json(
    snapshot_tbl,
    file.path(snapshots_out, paste0(season, ".json")),
    auto_unbox = TRUE,
    pretty = FALSE,
    na = "null"
  )
  
  n_snapshots_written <- n_snapshots_written + 1L
  cat("Wrote season snapshot:", season, "(players =", nrow(snapshot_tbl), ")\n")
}

current_snapshot_file <- file.path(season_snapshots_src_dir, "snapshot_current.csv")

if (file.exists(current_snapshot_file)) {
  current_snap <- read_csv(
    current_snapshot_file,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  )
  
  current_tbl <- current_snap %>%
    transmute(
      season = as.integer(Season),
      season_label = as.character(SeasonLabel),
      season_end_date = as.character(SeasonEndDate),
      rank = as.integer(Rank),
      id = as.character(PlayerID),
      name = as.character(PlayerName),
      display_name = as.character(PlayerName),
      nationality = as.character(Nationality),
      rating = as.integer(round(as.numeric(Rating))),
      games = as.integer(MatchesPlayed),
      frames = as.integer(FramesPlayed),
      listed_frames = as.integer(ListedFrames),
      last_played = format(as.Date(LastMatchDate), "%Y-%m-%d"),
      active = if ("Active" %in% names(current_snap)) as.logical(Active) else TRUE,
      listable = if ("Listable" %in% names(current_snap)) as.logical(Listable) else TRUE
    ) %>%
    arrange(rank, desc(rating), name)
  
  write_json(
    current_tbl,
    file.path(snapshots_out, "current.json"),
    auto_unbox = TRUE,
    pretty = FALSE,
    na = "null"
  )
  
  cat("Wrote snapshots/current.json (players =", nrow(current_tbl), ")\n")
  
} else {
  cat("No snapshot_current.csv found; snapshots/current.json was not written.\n")
}

cat("Wrote completed season snapshots:", n_snapshots_written, "\n")

# -----------------------------
# Per-player games JSON
# -----------------------------
rank_lookup_games <- hist_long %>%
  select(PlayerID, datetime, rank) %>%
  filter(!is.na(rank)) %>%
  arrange(PlayerID, datetime) %>%
  group_by(PlayerID, datetime) %>%
  slice_tail(n = 1) %>%
  ungroup()

n_games_written <- 0L

for (pid in all_ids) {
  df <- mhist %>%
    filter(PlayerA_ID == pid | PlayerB_ID == pid) %>%
    arrange(desc(MatchDate), desc(MatchID)) %>%
    mutate(
      season = EventSeason,
      
      am_a = PlayerA_ID == pid,
      am_b = PlayerB_ID == pid,
      
      delta_num = case_when(
        am_a ~ DeltaA,
        am_b ~ DeltaB,
        TRUE ~ NA_real_
      ),
      new_num = case_when(
        am_a ~ ARating_After,
        am_b ~ BRating_After,
        TRUE ~ NA_real_
      ),
      elo_num = case_when(
        am_a ~ ARating_Before,
        am_b ~ BRating_Before,
        TRUE ~ NA_real_
      ),
      opponent_elo_num = case_when(
        am_a ~ BRating_Before,
        am_b ~ ARating_Before,
        TRUE ~ NA_real_
      ),
      games_after = case_when(
        am_a ~ AGamesAfter,
        am_b ~ BGamesAfter,
        TRUE ~ NA_integer_
      )
    ) %>%
    left_join(
      rank_lookup_games %>%
        filter(PlayerID == pid) %>%
        select(datetime, rank),
      by = c("MatchDate" = "datetime")
    ) %>%
    transmute(
      date = format(as.Date(MatchDate), "%Y-%m-%d"),
      season = ifelse(is.na(season), NA_character_, as.character(season)),
      event = as.character(EventName),
      
      player_a_id = as.character(PlayerA_ID),
      player_a_name = as.character(PlayerA_Name),
      player_b_id = as.character(PlayerB_ID),
      player_b_name = as.character(PlayerB_Name),
      
      score_a = as.integer(ScoreA),
      score_b = as.integer(ScoreB),
      
      elo = ifelse(!is.na(elo_num), as.integer(round(elo_num)), NA_integer_),
      opponent_elo = ifelse(!is.na(opponent_elo_num), as.integer(round(opponent_elo_num)), NA_integer_),
      delta = ifelse(!is.na(delta_num), round(delta_num, 1), NA_real_),
      new = ifelse(!is.na(new_num), as.integer(round(new_num)), NA_integer_),
      
      games_after = as.integer(games_after),
      rank = as.integer(rank)
    )
  
  if (nrow(df) > 0L) {
    write_json(
      df,
      file.path(games_out, paste0(pid, ".json")),
      auto_unbox = TRUE,
      pretty = FALSE,
      na = "null"
    )
    n_games_written <- n_games_written + 1L
  }
}

cat("Wrote games files:", n_games_written, "\n")
cat("Done.\n")