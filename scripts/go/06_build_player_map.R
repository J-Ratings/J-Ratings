# ============================================================
# Build GoRatings-to-GoGoD player map
#
# Identity handling:
#   - GoRatings IDs sharing the same normalised display name are
#     automatically treated as aliases of one canonical player.
#
# Matching stages:
#   1. Unique exact normalised-name matches
#   2. Iterative historical game-pattern matches
#
# Historical game evidence compares:
#   - date
#   - colour
#   - result
#   - an opponent already mapped to a GoGoD player
#
# Newly accepted historical matches become opponent anchors
# for the next matching iteration.
#
# Repeated games with the same date, players, colour and result
# are preserved using occurrence counts.
#
# Outputs:
#   Go/pipeline_data/processed/goratings_player_map.csv
#   Go/pipeline_data/processed/goratings_unmatched_players.csv
#   Go/pipeline_data/processed/goratings_match_candidates.csv
#   Go/pipeline_data/processed/goratings_match_evidence.csv
#
# This script does not alter ratings or game data.
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(tibble)

options(stringsAsFactors = FALSE)

# -----------------------------
# Matching thresholds
# -----------------------------
MIN_GAME_MATCHES <- 3L
MIN_LEAD_OVER_SECOND <- 2L
MAX_ITERATIONS <- 20L

MATCHING_START_DATE <- as.Date("2015-01-01")
MATCHING_END_DATE <- as.Date("2025-12-31")

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

games_2026_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed",
  "goratings_games_2026.csv"
)

matching_history_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "goratings",
  "goratings_games_2015_2025.csv"
)

gogod_history_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "Elo",
  "game_history.csv"
)

checkpoint_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "checkpoint",
  "ratings_2025-12-31.csv"
)

output_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed",
  "goratings_player_map.csv"
)

unmatched_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed",
  "goratings_unmatched_players.csv"
)

candidates_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed",
  "goratings_match_candidates.csv"
)

evidence_file <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "processed",
  "goratings_match_evidence.csv"
)

# -----------------------------
# Input checks
# -----------------------------
required_files <- c(
  games_2026_file,
  matching_history_file,
  gogod_history_file,
  checkpoint_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required files:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}

# -----------------------------
# Helpers
# -----------------------------
normalise_match_name <- function(x) {
  x <- as.character(x)
  
  x <- str_replace_all(
    x,
    "\u00A0",
    " "
  )
  
  x <- str_squish(x)
  x <- str_to_lower(x)
  
  x <- str_replace_all(
    x,
    "[[:punct:]]",
    " "
  )
  
  x <- str_squish(x)
  
  x[x == ""] <- NA_character_
  
  x
}

first_non_missing <- function(x) {
  x <- as.character(x)
  
  x <- x[
    !is.na(x) &
      x != ""
  ]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  x[[1]]
}

empty_candidate_table <- function() {
  tibble(
    iteration = integer(),
    goratings_id = character(),
    goratings_name = character(),
    historical_name = character(),
    matching_games = integer(),
    candidate_rank = integer(),
    top_candidate_ties = integer(),
    second_best_game_count = integer(),
    lead_over_second = integer(),
    accepted_this_iteration = logical(),
    rejection_reason = character()
  )
}

# -----------------------------
# Read inputs
# -----------------------------
cat("Reading inputs...\n")

games_2026 <- read_csv(
  games_2026_file,
  show_col_types = FALSE,
  col_types = cols(
    BlackID = col_character(),
    WhiteID = col_character(),
    .default = col_guess()
  )
)

matching_history <- read_csv(
  matching_history_file,
  show_col_types = FALSE,
  col_types = cols(
    player_id = col_character(),
    opponent_id = col_character(),
    .default = col_guess()
  )
) %>%
  mutate(
    date = as.Date(date)
  )

gogod_history <- read_csv(
  gogod_history_file,
  show_col_types = FALSE
) %>%
  mutate(
    Date = as.Date(Date)
  )

checkpoint <- read_csv(
  checkpoint_file,
  show_col_types = FALSE
)

# -----------------------------
# Validate required columns
# -----------------------------
required_2026_columns <- c(
  "BlackID",
  "Black",
  "WhiteID",
  "White"
)

required_matching_columns <- c(
  "player_id",
  "player_name",
  "date",
  "color",
  "result",
  "opponent_id",
  "opponent_name"
)

required_gogod_columns <- c(
  "Date",
  "Black",
  "White",
  "ResultCode"
)

required_checkpoint_columns <- c(
  "name",
  "rating",
  "games",
  "last_historical_game"
)

check_columns <- function(
    data,
    required,
    label
) {
  missing <- setdiff(
    required,
    names(data)
  )
  
  if (length(missing) > 0) {
    stop(
      label,
      " is missing columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
}

check_columns(
  games_2026,
  required_2026_columns,
  "2026 games file"
)

check_columns(
  matching_history,
  required_matching_columns,
  "GoRatings matching-history file"
)

check_columns(
  gogod_history,
  required_gogod_columns,
  "GoGoD game-history file"
)

check_columns(
  checkpoint,
  required_checkpoint_columns,
  "Checkpoint file"
)

# -----------------------------
# Build current GoRatings player universe
#
# GoRatings can occasionally expose the same player under more
# than one ID.  Exact duplicate normalised display names are
# treated as aliases of one canonical live player.  The ID seen
# most often in the 2026 game file is retained as the canonical
# ID, with lexical ID order as a deterministic tie-break.
# -----------------------------
goratings_aliases <- bind_rows(
  games_2026 %>%
    transmute(
      goratings_id = as.character(BlackID),
      goratings_name = as.character(Black)
    ),
  games_2026 %>%
    transmute(
      goratings_id = as.character(WhiteID),
      goratings_name = as.character(White)
    )
) %>%
  filter(
    !is.na(goratings_id),
    goratings_id != "",
    !is.na(goratings_name),
    goratings_name != ""
  ) %>%
  count(
    goratings_id,
    goratings_name,
    name = "name_occurrences"
  ) %>%
  arrange(
    goratings_id,
    desc(name_occurrences),
    goratings_name
  ) %>%
  group_by(goratings_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    match_name = normalise_match_name(
      goratings_name
    )
  ) %>%
  filter(
    !is.na(match_name),
    match_name != ""
  ) %>%
  group_by(match_name) %>%
  arrange(
    desc(name_occurrences),
    goratings_id,
    .by_group = TRUE
  ) %>%
  mutate(
    canonical_goratings_id =
      first(goratings_id),
    alias_count = n()
  ) %>%
  ungroup()

duplicate_alias_groups <- goratings_aliases %>%
  filter(alias_count > 1L) %>%
  arrange(
    match_name,
    goratings_id
  )

if (nrow(duplicate_alias_groups) > 0L) {
  cat(
    "Automatic GoRatings ID aliases detected:",
    nrow(
      duplicate_alias_groups %>%
        distinct(match_name)
    ),
    "player(s) across",
    nrow(duplicate_alias_groups),
    "ID rows.\n"
  )
  
  print(
    duplicate_alias_groups %>%
      select(
        goratings_id,
        goratings_name,
        canonical_goratings_id,
        alias_count
      )
  )
}

goratings_players <- goratings_aliases %>%
  group_by(
    canonical_goratings_id,
    match_name
  ) %>%
  arrange(
    desc(name_occurrences),
    goratings_id,
    .by_group = TRUE
  ) %>%
  summarise(
    goratings_id =
      first(canonical_goratings_id),
    goratings_name =
      first(goratings_name),
    name_occurrences =
      sum(name_occurrences),
    alias_count =
      max(alias_count),
    .groups = "drop"
  ) %>%
  select(
    goratings_id,
    goratings_name,
    name_occurrences,
    match_name,
    alias_count
  )

alias_id_map <- setNames(
  goratings_aliases$canonical_goratings_id,
  goratings_aliases$goratings_id
)

canonicalise_live_id <- function(x) {
  x <- as.character(x)
  mapped <- unname(alias_id_map[x])
  ifelse(
    !is.na(mapped) & mapped != "",
    mapped,
    x
  )
}

cat(
  "Raw GoRatings IDs in 2026 games:",
  nrow(goratings_aliases),
  "\n"
)

cat(
  "Canonical GoRatings players in 2026 games:",
  nrow(goratings_players),
  "\n"
)

# -----------------------------
# Prepare historical checkpoint
# -----------------------------
historical_players <- checkpoint %>%
  transmute(
    historical_name = as.character(name),
    checkpoint_rating = as.numeric(rating),
    checkpoint_games = as.integer(games),
    last_historical_game =
      as.Date(last_historical_game),
    match_name = normalise_match_name(name)
  )

historical_name_counts <- historical_players %>%
  count(
    match_name,
    name = "historical_name_count"
  )

historical_players <- historical_players %>%
  left_join(
    historical_name_counts,
    by = "match_name"
  )

# -----------------------------
# Stage 1: exact-name candidates
# -----------------------------
exact_candidates <- goratings_players %>%
  inner_join(
    historical_players %>%
      filter(
        historical_name_count == 1
      ),
    by = "match_name"
  )

# Do not allow multiple GoRatings IDs to claim the same
# historical player through exact-name matching.
exact_claim_counts <- exact_candidates %>%
  count(
    historical_name,
    name = "exact_claim_count"
  )

exact_matches <- exact_candidates %>%
  left_join(
    exact_claim_counts,
    by = "historical_name"
  ) %>%
  filter(
    exact_claim_count == 1
  ) %>%
  transmute(
    goratings_id,
    goratings_name,
    historical_name,
    match_status = "exact_name_match",
    match_iteration = 0L,
    match_game_count = NA_integer_,
    second_best_game_count = NA_integer_,
    checkpoint_rating,
    checkpoint_games,
    last_historical_game,
    name_occurrences
  )

ambiguous_exact_claims <- exact_candidates %>%
  left_join(
    exact_claim_counts,
    by = "historical_name"
  ) %>%
  filter(
    exact_claim_count > 1
  )

cat(
  "Unique exact-name matches:",
  nrow(exact_matches),
  "\n"
)

cat(
  "Ambiguous duplicate exact-name claims:",
  nrow(ambiguous_exact_claims),
  "\n"
)

# -----------------------------
# Prepare GoRatings historical observations
#
# Counts preserve repeated same-signature games.
# -----------------------------
goratings_observations <- matching_history %>%
  transmute(
    goratings_id =
      canonicalise_live_id(player_id),
    date = as.Date(date),
    color = str_to_title(
      str_trim(
        as.character(color)
      )
    ),
    result = str_to_title(
      str_trim(
        as.character(result)
      )
    ),
    opponent_id =
      canonicalise_live_id(opponent_id)
  ) %>%
  filter(
    goratings_id %in%
      goratings_players$goratings_id,
    !is.na(date),
    date >= MATCHING_START_DATE,
    date <= MATCHING_END_DATE,
    color %in% c(
      "Black",
      "White"
    ),
    result %in% c(
      "Win",
      "Loss"
    ),
    !is.na(opponent_id),
    opponent_id != ""
  ) %>%
  count(
    goratings_id,
    date,
    color,
    result,
    opponent_id,
    name = "goratings_occurrences"
  )

# -----------------------------
# Prepare GoGoD observations
#
# One observation is produced for each side of every game.
# Counts preserve repeated same-signature games.
# -----------------------------
gogod_black_observations <- gogod_history %>%
  filter(
    str_starts(
      as.character(ResultCode),
      "B+"
    ) |
      str_starts(
        as.character(ResultCode),
        "W+"
      )
  ) %>%
  transmute(
    historical_name = as.character(Black),
    date = Date,
    color = "Black",
    result = if_else(
      str_starts(
        as.character(ResultCode),
        "B+"
      ),
      "Win",
      "Loss"
    ),
    historical_opponent_key =
      normalise_match_name(White)
  )

gogod_white_observations <- gogod_history %>%
  filter(
    str_starts(
      as.character(ResultCode),
      "B+"
    ) |
      str_starts(
        as.character(ResultCode),
        "W+"
      )
  ) %>%
  transmute(
    historical_name = as.character(White),
    date = Date,
    color = "White",
    result = if_else(
      str_starts(
        as.character(ResultCode),
        "W+"
      ),
      "Win",
      "Loss"
    ),
    historical_opponent_key =
      normalise_match_name(Black)
  )

gogod_observations <- bind_rows(
  gogod_black_observations,
  gogod_white_observations
) %>%
  filter(
    !is.na(date),
    date >= MATCHING_START_DATE,
    date <= MATCHING_END_DATE,
    !is.na(historical_name),
    historical_name != "",
    !is.na(historical_opponent_key),
    historical_opponent_key != ""
  ) %>%
  count(
    historical_name,
    date,
    color,
    result,
    historical_opponent_key,
    name = "gogod_occurrences"
  )

# -----------------------------
# Iterative historical matching
# -----------------------------
accepted_matches <- exact_matches

all_candidate_scores <- list()

iteration <- 1L

repeat {
  unresolved_players <- goratings_players %>%
    anti_join(
      accepted_matches,
      by = "goratings_id"
    )
  
  if (nrow(unresolved_players) == 0) {
    cat(
      "All players matched after iteration",
      iteration - 1L,
      "\n"
    )
    
    break
  }
  
  if (iteration > MAX_ITERATIONS) {
    cat(
      "Stopped after maximum iterations:",
      MAX_ITERATIONS,
      "\n"
    )
    
    break
  }
  
  cat(
    "\nHistorical matching iteration",
    iteration,
    "\n"
  )
  
  cat(
    "Unresolved players:",
    nrow(unresolved_players),
    "\n"
  )
  
  # Every accepted mapping can now act as an opponent anchor.
  opponent_anchor_map <- accepted_matches %>%
    transmute(
      opponent_id = goratings_id,
      historical_opponent_name =
        historical_name,
      historical_opponent_key =
        normalise_match_name(
          historical_name
        )
    )
  
  # Convert GoRatings opponent IDs to historical opponent names.
  goratings_evidence <- goratings_observations %>%
    filter(
      goratings_id %in%
        unresolved_players$goratings_id
    ) %>%
    inner_join(
      opponent_anchor_map,
      by = "opponent_id"
    ) %>%
    group_by(
      goratings_id,
      date,
      color,
      result,
      historical_opponent_key
    ) %>%
    summarise(
      goratings_occurrences =
        sum(goratings_occurrences),
      .groups = "drop"
    )
  
  cat(
    "Usable evidence signatures:",
    nrow(goratings_evidence),
    "\n"
  )
  
  if (nrow(goratings_evidence) == 0) {
    cat(
      "No usable evidence remains.\n"
    )
    
    break
  }
  
  already_claimed_names <- accepted_matches %>%
    pull(historical_name) %>%
    unique()
  
  # A signature contributes the smaller occurrence count
  # from the two sources.
  candidate_signature_scores <-
    goratings_evidence %>%
    inner_join(
      gogod_observations %>%
        filter(
          !(historical_name %in%
              already_claimed_names)
        ),
      by = c(
        "date",
        "color",
        "result",
        "historical_opponent_key"
      ),
      relationship = "many-to-many"
    ) %>%
    mutate(
      matched_occurrences = pmin(
        goratings_occurrences,
        gogod_occurrences
      )
    )
  
  candidate_scores <-
    candidate_signature_scores %>%
    group_by(
      goratings_id,
      historical_name
    ) %>%
    summarise(
      matching_games =
        sum(matched_occurrences),
      .groups = "drop"
    ) %>%
    inner_join(
      unresolved_players %>%
        select(
          goratings_id,
          goratings_name
        ),
      by = "goratings_id"
    ) %>%
    group_by(goratings_id) %>%
    mutate(
      top_score = max(matching_games),
      top_candidate_ties = sum(
        matching_games == top_score
      )
    ) %>%
    arrange(
      goratings_id,
      desc(matching_games),
      historical_name
    ) %>%
    mutate(
      candidate_rank = row_number(),
      second_best_game_count = case_when(
        n() >= 2 ~ matching_games[2],
        TRUE ~ 0L
      ),
      lead_over_second =
        matching_games -
        second_best_game_count
    ) %>%
    ungroup()
  
  if (nrow(candidate_scores) == 0) {
    cat(
      "No historical candidates were found.\n"
    )
    
    break
  }
  
  provisional_matches <- candidate_scores %>%
    filter(
      candidate_rank == 1,
      top_candidate_ties == 1,
      matching_games >= MIN_GAME_MATCHES,
      lead_over_second >=
        MIN_LEAD_OVER_SECOND
    )
  
  # Prevent two GoRatings IDs from claiming the same historical
  # player in the same iteration.
  provisional_claim_counts <-
    provisional_matches %>%
    count(
      historical_name,
      name = "claim_count"
    )
  
  accepted_this_iteration <-
    provisional_matches %>%
    left_join(
      provisional_claim_counts,
      by = "historical_name"
    ) %>%
    filter(
      claim_count == 1
    )
  
  candidate_scores_for_output <-
    candidate_scores %>%
    left_join(
      provisional_claim_counts,
      by = "historical_name"
    ) %>%
    mutate(
      iteration = iteration,
      accepted_this_iteration =
        goratings_id %in%
        accepted_this_iteration$goratings_id &
        historical_name %in%
        accepted_this_iteration$historical_name,
      rejection_reason = case_when(
        accepted_this_iteration ~
          NA_character_,
        candidate_rank != 1 ~
          "Not top candidate",
        top_candidate_ties > 1 ~
          "Top score tied",
        matching_games <
          MIN_GAME_MATCHES ~
          "Below minimum matching games",
        lead_over_second <
          MIN_LEAD_OVER_SECOND ~
          "Lead over second candidate too small",
        !is.na(claim_count) &
          claim_count > 1 ~
          "Historical player claimed by multiple GoRatings IDs",
        TRUE ~
          "Not accepted"
      )
    ) %>%
    select(
      iteration,
      goratings_id,
      goratings_name,
      historical_name,
      matching_games,
      candidate_rank,
      top_candidate_ties,
      second_best_game_count,
      lead_over_second,
      accepted_this_iteration,
      rejection_reason
    )
  
  all_candidate_scores[[iteration]] <-
    candidate_scores_for_output
  
  cat(
    "Candidates assessed:",
    nrow(candidate_scores),
    "\n"
  )
  
  cat(
    "Matches accepted this iteration:",
    nrow(accepted_this_iteration),
    "\n"
  )
  
  if (nrow(accepted_this_iteration) == 0) {
    cat(
      "No further safe matches found.\n"
    )
    
    break
  }
  
  new_matches <- accepted_this_iteration %>%
    inner_join(
      historical_players %>%
        select(
          historical_name,
          checkpoint_rating,
          checkpoint_games,
          last_historical_game
        ),
      by = "historical_name"
    ) %>%
    transmute(
      goratings_id,
      goratings_name,
      historical_name,
      match_status =
        "historical_game_match",
      match_iteration = iteration,
      match_game_count =
        as.integer(matching_games),
      second_best_game_count =
        as.integer(
          second_best_game_count
        ),
      checkpoint_rating,
      checkpoint_games,
      last_historical_game,
      name_occurrences = NA_integer_
    )
  
  accepted_matches <- bind_rows(
    accepted_matches,
    new_matches
  )
  
  iteration <- iteration + 1L
}

# -----------------------------
# Final duplicate-claim validation
# -----------------------------
duplicate_goratings_claims <- accepted_matches %>%
  count(
    goratings_id,
    name = "claim_count"
  ) %>%
  filter(
    claim_count > 1
  )

if (nrow(duplicate_goratings_claims) > 0) {
  print(duplicate_goratings_claims)
  
  stop(
    "A GoRatings ID was assigned more than once."
  )
}

duplicate_historical_claims <- accepted_matches %>%
  count(
    historical_name,
    name = "claim_count"
  ) %>%
  filter(
    claim_count > 1
  )

if (nrow(duplicate_historical_claims) > 0) {
  print(duplicate_historical_claims)
  
  stop(
    "A historical player was assigned to more than one GoRatings ID."
  )
}

# -----------------------------
# Build remaining unmatched rows
# -----------------------------
remaining_players <- goratings_players %>%
  anti_join(
    accepted_matches,
    by = "goratings_id"
  ) %>%
  transmute(
    goratings_id,
    goratings_name,
    historical_name = NA_character_,
    match_status = "unmatched_or_new",
    match_iteration = NA_integer_,
    match_game_count = NA_integer_,
    second_best_game_count = NA_integer_,
    checkpoint_rating = NA_real_,
    checkpoint_games = NA_integer_,
    last_historical_game = as.Date(NA),
    name_occurrences
  )

canonical_player_map <- bind_rows(
  accepted_matches,
  remaining_players
) %>%
  arrange(
    match_status,
    match_iteration,
    goratings_name
  )

# Expand the canonical mapping back to every raw GoRatings ID.
# Downstream Elo code uses canonical_goratings_id as the actual
# rating identity, while retaining each source ID for traceability.
player_map <- goratings_aliases %>%
  select(
    goratings_id,
    source_goratings_name = goratings_name,
    canonical_goratings_id,
    alias_count
  ) %>%
  left_join(
    canonical_player_map %>%
      rename(
        canonical_goratings_id =
          goratings_id,
        canonical_goratings_name =
          goratings_name
      ),
    by = "canonical_goratings_id"
  ) %>%
  mutate(
    goratings_name =
      canonical_goratings_name
  ) %>%
  select(
    goratings_id,
    goratings_name,
    source_goratings_name,
    canonical_goratings_id,
    alias_count,
    historical_name,
    match_status,
    match_iteration,
    match_game_count,
    second_best_game_count,
    checkpoint_rating,
    checkpoint_games,
    last_historical_game,
    name_occurrences
  ) %>%
  arrange(
    match_status,
    match_iteration,
    goratings_name,
    goratings_id
  )

unmatched_players <- player_map %>%
  filter(
    match_status ==
      "unmatched_or_new"
  ) %>%
  arrange(goratings_name)

# -----------------------------
# Combine candidate audit output
# -----------------------------
if (length(all_candidate_scores) > 0) {
  candidate_output <- bind_rows(
    all_candidate_scores
  )
} else {
  candidate_output <- empty_candidate_table()
}

# -----------------------------
# Build final match evidence
#
# This uses the complete accepted map, so evidence can include
# anchors discovered in later iterations.
# -----------------------------
final_opponent_anchor_map <- accepted_matches %>%
  transmute(
    opponent_id = goratings_id,
    historical_opponent_name =
      historical_name,
    historical_opponent_key =
      normalise_match_name(
        historical_name
      )
  )

historical_game_matches <- accepted_matches %>%
  filter(
    match_status ==
      "historical_game_match"
  ) %>%
  select(
    goratings_id,
    goratings_name,
    historical_name,
    match_iteration
  )

if (nrow(historical_game_matches) > 0) {
  final_goratings_evidence <-
    goratings_observations %>%
    inner_join(
      historical_game_matches %>%
        select(
          goratings_id,
          goratings_name,
          historical_name,
          match_iteration
        ),
      by = "goratings_id"
    ) %>%
    inner_join(
      final_opponent_anchor_map,
      by = "opponent_id"
    ) %>%
    group_by(
      goratings_id,
      goratings_name,
      historical_name,
      match_iteration,
      date,
      color,
      result,
      historical_opponent_name,
      historical_opponent_key
    ) %>%
    summarise(
      goratings_occurrences =
        sum(goratings_occurrences),
      .groups = "drop"
    )
  
  match_evidence <-
    final_goratings_evidence %>%
    inner_join(
      gogod_observations,
      by = c(
        "historical_name",
        "date",
        "color",
        "result",
        "historical_opponent_key"
      )
    ) %>%
    mutate(
      matched_occurrences = pmin(
        goratings_occurrences,
        gogod_occurrences
      )
    ) %>%
    filter(
      matched_occurrences > 0
    ) %>%
    transmute(
      goratings_id,
      goratings_name,
      historical_name,
      match_iteration,
      date = format(
        date,
        "%Y-%m-%d"
      ),
      color,
      result,
      historical_opponent_name,
      goratings_occurrences,
      gogod_occurrences,
      matched_occurrences
    ) %>%
    arrange(
      match_iteration,
      goratings_name,
      date,
      color,
      historical_opponent_name
    )
} else {
  match_evidence <- tibble(
    goratings_id = character(),
    goratings_name = character(),
    historical_name = character(),
    match_iteration = integer(),
    date = character(),
    color = character(),
    result = character(),
    historical_opponent_name = character(),
    goratings_occurrences = integer(),
    gogod_occurrences = integer(),
    matched_occurrences = integer()
  )
}

# -----------------------------
# Final validation
# -----------------------------
if (nrow(player_map) != nrow(goratings_aliases)) {
  stop(
    "Final player-map row count does not equal the raw GoRatings ID count."
  )
}

if (
  anyDuplicated(
    player_map$goratings_id
  ) > 0
) {
  stop(
    "Final player map contains duplicate GoRatings IDs."
  )
}

matched_historical_names <- player_map %>%
  filter(
    match_status !=
      "unmatched_or_new"
  ) %>%
  distinct(
    canonical_goratings_id,
    historical_name
  ) %>%
  pull(historical_name)

if (
  anyDuplicated(
    matched_historical_names
  ) > 0
) {
  stop(
    "Final player map contains conflicting historical assignments across canonical players."
  )
}

# -----------------------------
# Write outputs
# -----------------------------
write_csv(
  player_map,
  output_file
)

write_csv(
  unmatched_players,
  unmatched_file
)

write_csv(
  candidate_output,
  candidates_file
)

write_csv(
  match_evidence,
  evidence_file
)

cat("\nDone.\n")

cat(
  "Total raw GoRatings IDs:",
  nrow(player_map),
  "\n"
)

cat(
  "Canonical GoRatings players:",
  nrow(canonical_player_map),
  "\n"
)

cat(
  "Exact-name matches:",
  sum(
    player_map$match_status ==
      "exact_name_match"
  ),
  "\n"
)

cat(
  "Historical game matches:",
  sum(
    player_map$match_status ==
      "historical_game_match"
  ),
  "\n"
)

cat(
  "Matching iterations used:",
  ifelse(
    any(
      player_map$match_status ==
        "historical_game_match"
    ),
    max(
      player_map$match_iteration,
      na.rm = TRUE
    ),
    0L
  ),
  "\n"
)

cat(
  "Still unmatched or new:",
  nrow(unmatched_players),
  "\n"
)

cat(
  "Evidence rows:",
  nrow(match_evidence),
  "\n"
)

cat(
  "Wrote:",
  output_file,
  "\n"
)

cat(
  "Wrote:",
  unmatched_file,
  "\n"
)

cat(
  "Wrote:",
  candidates_file,
  "\n"
)

cat(
  "Wrote:",
  evidence_file,
  "\n"
)