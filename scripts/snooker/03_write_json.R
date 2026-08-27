library(dplyr)
library(readr)
library(jsonlite)
library(stringr)
library(data.table)

options(stringsAsFactors = FALSE)

# ============================================================
# 03_write_json.R
#
# Export SNOOKER ratings + history to JSON for website.
#
# Reads/writes only inside the Git repo:
#   Reads:
#     Snooker/pipeline_data/Elo/snooker_elo_final_ratings.csv
#     Snooker/pipeline_data/Elo/snooker_elo_match_history.csv
#     Snooker/pipeline_data/Elo/season_snapshots/
#
#   Writes:
#     Snooker/data/meta.json
#     Snooker/data/players.json
#     Snooker/data/history/<player_id>.json
#     Snooker/data/games/<player_id>.json
#     Snooker/data/snapshots/seasons.json
#     Snooker/data/snapshots/<season>.json
#     Snooker/data/snapshots/current.json
#
# Notes:
#   - rating method comes from the two-pass zero-sum calculation:
#       * constant K = 5 for both players
#       * no provisional double-K or opponent-frame dampening
#       * Pass 1 starts everyone at 2600
#       * eligible players receive retrospective 200-frame starts in Pass 2
#   - history rating = post-match Elo on that date
#   - history rank = world rank at end of that match timestamp
#   - games rank = player world rank after that match timestamp
#   - snapshots are season-end snapshots, not calendar-year snapshots
#   - player must have at least MIN_LIST_FRAMES cumulative frames to be ranked
#   - player must have played within ACTIVE_YEARS at that historical point to be ranked
#   - per-player games JSON is exported from the profile player's perspective
#   - expected W/L is calculated from pre-match Elo ratings; snooker has no draw
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

ELO_SRC_DIR <- file.path(PIPELINE_DIR, "Elo")
SEASON_SNAPSHOTS_SRC_DIR <- file.path(ELO_SRC_DIR, "season_snapshots")

BASE_DATA_DIR <- file.path(SNOOKER_DIR, "data")
HISTORY_OUT <- file.path(BASE_DATA_DIR, "history")
GAMES_OUT <- file.path(BASE_DATA_DIR, "games")
SNAPSHOTS_OUT <- file.path(BASE_DATA_DIR, "snapshots")

dir.create(BASE_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(HISTORY_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(GAMES_OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(SNAPSHOTS_OUT, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Rules
# -----------------------------
MIN_LIST_FRAMES <- 200L
ACTIVE_YEARS <- 2L

# -----------------------------
# Helpers
# -----------------------------
elo_expected <- function(player_elo, opponent_elo) {
  1 / (1 + 10 ^ ((opponent_elo - player_elo) / 400))
}

write_json_compact <- function(x, path, na = "null") {
  write_json(
    x,
    path,
    auto_unbox = TRUE,
    pretty = FALSE,
    na = na
  )
}

# -----------------------------
# Input files
# -----------------------------
final_csv    <- file.path(ELO_SRC_DIR, "snooker_elo_final_ratings.csv")
hist_csv     <- file.path(ELO_SRC_DIR, "snooker_elo_match_history.csv")
upcoming_csv <- file.path(ELO_SRC_DIR, "snooker_upcoming_matches.csv")

if (!file.exists(final_csv))    stop("Missing file: ", final_csv)
if (!file.exists(hist_csv))     stop("Missing file: ", hist_csv)
if (!file.exists(upcoming_csv)) stop("Missing file: ", upcoming_csv)

# -----------------------------
# Load CSVs
# -----------------------------
final <- read_csv(
  final_csv,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

mhist <- read_csv(
  hist_csv,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

upcoming <- read_csv(
  upcoming_csv,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

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

if (length(miss_final) > 0L) {
  stop("Missing columns in final CSV: ", paste(miss_final, collapse = ", "))
}

if (length(miss_hist) > 0L) {
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
# Prepare confirmed upcoming matches
# -----------------------------

upcoming <- upcoming %>%
  mutate(
    MatchID = trimws(as.character(MatchID)),
    EventID = trimws(as.character(EventID)),
    EventName = trimws(as.character(EventName)),
    EventSeason = suppressWarnings(as.integer(EventSeason)),
    ScheduledDate = as.POSIXct(
      ScheduledDate,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    PlayerA_ID = trimws(as.character(PlayerA_ID)),
    PlayerB_ID = trimws(as.character(PlayerB_ID))
  ) %>%
  filter(
    MatchID != "",
    PlayerA_ID != "",
    PlayerB_ID != "",
    !is.na(ScheduledDate)
  )

current_player_lookup <- final %>%
  select(
    PlayerID,
    PlayerName,
    Rating
  )

upcoming <- upcoming %>%
  left_join(
    current_player_lookup %>%
      rename(
        PlayerA_ID = PlayerID,
        PlayerA_Name = PlayerName,
        ARating = Rating
      ),
    by = "PlayerA_ID"
  ) %>%
  left_join(
    current_player_lookup %>%
      rename(
        PlayerB_ID = PlayerID,
        PlayerB_Name = PlayerName,
        BRating = Rating
      ),
    by = "PlayerB_ID"
  )

cat("Confirmed upcoming matches loaded:", nrow(upcoming), "\n")

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

cat("Repo directory:", REPO_DIR, "\n")
cat("Pipeline directory:", PIPELINE_DIR, "\n")
cat("Website data directory:", BASE_DATA_DIR, "\n")
cat("As of:", format(asof_date, "%Y-%m-%d"), "\n")
cat("Current EventSeason:", current_season, "\n")

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
  games_have_expected_wl = TRUE,
  rating_method = "Two-pass zero-sum Elo. Constant K=5. Pass 1 starts everyone at 2600; eligible players receive retrospective 200-frame starting ratings in Pass 2.",
  rank_method = paste0(
    "Ranked by latest known Elo at each match timestamp. ",
    "Players need at least ", MIN_LIST_FRAMES, " cumulative frames and must have played within ",
    ACTIVE_YEARS, " years at that historical point."
  ),
  expected_wl_method = "Win/loss expectation from profile player's pre-match Elo versus opponent's pre-match Elo. Draw probability is zero for snooker."
)

write_json_compact(
  meta,
  file.path(BASE_DATA_DIR, "meta.json")
)

cat("Wrote meta.json\n")

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
# This walks forward through time and keeps current player state.
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
    
    state[updates_state, `:=`(
      PlayerName = i.PlayerName,
      rating = i.rating,
      cum_frames = i.cum_frames,
      date = i.date
    )]
    
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

write_json_compact(
  players_tbl,
  file.path(BASE_DATA_DIR, "players.json")
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
# history = last post-match rating on each day
# -----------------------------
n_history_written <- 0L

for (pid in all_ids) {
  history_df <- hist_daily %>%
    filter(PlayerID == pid) %>%
    arrange(date, datetime, match_id) %>%
    transmute(
      date = format(as.Date(date), "%Y-%m-%d"),
      season = ifelse(
        is.na(season),
        NA_character_,
        as.character(season)
      ),
      event = as.character(event),
      rating = as.integer(round(rating)),
      rank = as.integer(rank)
    )
  
  if (nrow(history_df) > 0L) {
    write_json_compact(
      history_df,
      file.path(
        HISTORY_OUT,
        paste0(pid, ".json")
      )
    )
    
    n_history_written <- n_history_written + 1L
  }
}

cat("Wrote history files:", n_history_written, "\n")

# -----------------------------
# Per-player games JSON
#
# Output is from the profile player's perspective:
#   player on the left, opponent on the right.
#
# Completed matches use pre-match Elo.
# Upcoming matches use current Elo.
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
  
  # ==========================================================
  # Past matches
  # ==========================================================
  
  past_df <- mhist %>%
    filter(PlayerA_ID == pid | PlayerB_ID == pid) %>%
    arrange(desc(MatchDate), desc(MatchID)) %>%
    mutate(
      season = EventSeason,
      
      am_a = PlayerA_ID == pid,
      am_b = PlayerB_ID == pid,
      
      player_id = pid,
      player_name = if_else(am_a, PlayerA_Name, PlayerB_Name),
      opponent_id = if_else(am_a, PlayerB_ID, PlayerA_ID),
      opponent_name = if_else(am_a, PlayerB_Name, PlayerA_Name),
      
      player_score = if_else(am_a, ScoreA, ScoreB),
      opponent_score = if_else(am_a, ScoreB, ScoreA),
      
      score = if_else(
        !is.na(player_score) & !is.na(opponent_score),
        paste0(player_score, "-", opponent_score),
        NA_character_
      ),
      
      player_elo_num = if_else(am_a, ARating_Before, BRating_Before),
      opponent_elo_num = if_else(am_a, BRating_Before, ARating_Before),
      player_new_num = if_else(am_a, ARating_After, BRating_After),
      player_delta_num = if_else(am_a, DeltaA, DeltaB),
      games_after = if_else(am_a, AGamesAfter, BGamesAfter),
      
      expected_win = elo_expected(
        player_elo_num,
        opponent_elo_num
      ),
      
      winPct_num = as.integer(round(100 * expected_win)),
      lossPct_num = 100L - winPct_num,
      
      result = case_when(
        player_score > opponent_score ~ "Win",
        player_score < opponent_score ~ "Loss",
        TRUE ~ NA_character_
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
      season = ifelse(
        is.na(season),
        NA_character_,
        as.character(season)
      ),
      event = as.character(EventName),
      
      player_id = as.character(player_id),
      player_name = as.character(player_name),
      player_elo = ifelse(
        !is.na(player_elo_num),
        as.integer(round(player_elo_num)),
        NA_integer_
      ),
      
      opponent_id = as.character(opponent_id),
      opponent_name = as.character(opponent_name),
      opponent_elo = ifelse(
        !is.na(opponent_elo_num),
        as.integer(round(opponent_elo_num)),
        NA_integer_
      ),
      
      score = as.character(score),
      player_score = as.integer(player_score),
      opponent_score = as.integer(opponent_score),
      result = as.character(result),
      
      winPct = as.integer(winPct_num),
      lossPct = as.integer(lossPct_num),
      
      delta = ifelse(
        !is.na(player_delta_num),
        round(player_delta_num, 1),
        NA_real_
      ),
      
      new = ifelse(
        !is.na(player_new_num),
        as.integer(round(player_new_num)),
        NA_integer_
      ),
      
      games_after = as.integer(games_after),
      rank = as.integer(rank),
      
      upcoming = FALSE,
      
      # Legacy fields retained for compatibility.
      player_a_id = as.character(PlayerA_ID),
      player_a_name = as.character(PlayerA_Name),
      player_b_id = as.character(PlayerB_ID),
      player_b_name = as.character(PlayerB_Name),
      score_a = as.integer(ScoreA),
      score_b = as.integer(ScoreB),
      
      elo = ifelse(
        !is.na(player_elo_num),
        as.integer(round(player_elo_num)),
        NA_integer_
      )
    )
  
  # ==========================================================
  # Next match
  # ==========================================================
  
  future_df <- upcoming %>%
    filter(
      PlayerA_ID == pid |
        PlayerB_ID == pid
    ) %>%
    arrange(ScheduledDate, MatchID) %>%
    slice_head(n = 1)
  
  if (nrow(future_df) > 0L) {
    
    future_df <- future_df %>%
      mutate(
        am_a = PlayerA_ID == pid,
        
        player_id = pid,
        
        player_name = if_else(
          am_a,
          PlayerA_Name,
          PlayerB_Name
        ),
        
        opponent_id = if_else(
          am_a,
          PlayerB_ID,
          PlayerA_ID
        ),
        
        opponent_name = if_else(
          am_a,
          PlayerB_Name,
          PlayerA_Name
        ),
        
        player_elo_num = if_else(
          am_a,
          ARating,
          BRating
        ),
        
        opponent_elo_num = if_else(
          am_a,
          BRating,
          ARating
        ),
        
        expected_win = elo_expected(
          player_elo_num,
          opponent_elo_num
        ),
        
        winPct_num = as.integer(
          round(100 * expected_win)
        ),
        
        lossPct_num = 100L - winPct_num
      ) %>%
      transmute(
        date = format(
          as.Date(ScheduledDate),
          "%Y-%m-%d"
        ),
        
        season = ifelse(
          is.na(EventSeason),
          NA_character_,
          as.character(EventSeason)
        ),
        
        event = as.character(EventName),
        
        player_id = as.character(player_id),
        player_name = as.character(player_name),
        
        player_elo = ifelse(
          !is.na(player_elo_num),
          as.integer(round(player_elo_num)),
          NA_integer_
        ),
        
        opponent_id = as.character(opponent_id),
        opponent_name = as.character(opponent_name),
        
        opponent_elo = ifelse(
          !is.na(opponent_elo_num),
          as.integer(round(opponent_elo_num)),
          NA_integer_
        ),
        
        score = NA_character_,
        player_score = NA_integer_,
        opponent_score = NA_integer_,
        result = NA_character_,
        
        winPct = as.integer(winPct_num),
        lossPct = as.integer(lossPct_num),
        
        delta = NA_real_,
        new = NA_integer_,
        games_after = NA_integer_,
        rank = NA_integer_,
        
        upcoming = TRUE,
        
        # Legacy fields retained for compatibility.
        player_a_id = as.character(PlayerA_ID),
        player_a_name = as.character(PlayerA_Name),
        player_b_id = as.character(PlayerB_ID),
        player_b_name = as.character(PlayerB_Name),
        
        score_a = NA_integer_,
        score_b = NA_integer_,
        
        elo = ifelse(
          !is.na(player_elo_num),
          as.integer(round(player_elo_num)),
          NA_integer_
        )
      )
    
  } else {
    
    future_df <- past_df[0, ]
  }
  
  # Next match first, followed by completed matches.
  df <- bind_rows(
    future_df,
    past_df
  )
  
  if (nrow(df) > 0L) {
    
    write_json_compact(
      df,
      file.path(
        GAMES_OUT,
        paste0(pid, ".json")
      )
    )
    
    n_games_written <- n_games_written + 1L
  }
}

cat("Wrote games files:", n_games_written, "\n")
cat("Done.\n")
