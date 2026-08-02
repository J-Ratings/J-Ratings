# ============================================================
# Export combined Go ratings and history to website JSON
#
# Inputs:
#   - combined_final_ratings.csv
#   - combined_game_history.csv
#   - Names-Gender-Country.txt
#   - name_fixes.txt
#   - flag_overrides.txt
#   - goratings_players_homepage.csv
#
# Outputs:
#   - Go/data/meta.json
#   - Go/data/players.json
#   - Go/data/era_starts.json
#   - Go/data/history/<player_id>.json
#   - Go/data/games/<player_id>.json
#
# Flag and gender priority:
#   1. Manual flag overrides
#   2. GoRatings homepage metadata matched by GoRatings ID
#   3. Historical name-country lookup
# ============================================================

library(dplyr)
library(readr)
library(jsonlite)
library(stringi)
library(lubridate)
library(stringr)
library(tibble)
library(data.table)

options(stringsAsFactors = FALSE)

# -----------------------------
# Settings
# -----------------------------
MIN_GAMES_FOR_TABLE <- 20L
RANK_INACTIVE_YEARS <- 4

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

src_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "live_Elo"
)

source_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "source"
)

goratings_dir <- file.path(
  repo_dir,
  "Go",
  "pipeline_data",
  "goratings"
)

data_dir <- file.path(
  repo_dir,
  "Go",
  "data"
)

history_out <- file.path(
  data_dir,
  "history"
)

games_out <- file.path(
  data_dir,
  "games"
)

final_csv <- file.path(
  src_dir,
  "combined_final_ratings.csv"
)

hist_csv <- file.path(
  src_dir,
  "combined_game_history.csv"
)

name_country_file <- file.path(
  source_dir,
  "Names-Gender-Country.txt"
)

name_fixes_file <- file.path(
  source_dir,
  "name_fixes.txt"
)

flag_overrides_file <- file.path(
  source_dir,
  "flag_overrides.txt"
)

goratings_homepage_file <- file.path(
  goratings_dir,
  "goratings_players_homepage.csv"
)

dir.create(
  data_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  history_out,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  games_out,
  recursive = TRUE,
  showWarnings = FALSE
)

# -----------------------------
# Helpers
# -----------------------------
slug <- function(x) {
  x <- stringi::stri_trans_general(
    x,
    "Latin-ASCII"
  )
  
  x <- tolower(
    trimws(x)
  )
  
  x <- gsub(
    "[^a-z0-9]+",
    "-",
    x
  )
  
  gsub(
    "(^-+|-+$)",
    "",
    x
  )
}

parse_date_robust <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  
  x <- as.character(x)
  
  y <- suppressWarnings(
    lubridate::ymd(x)
  )
  
  if (all(!is.na(y))) {
    return(y)
  }
  
  y <- suppressWarnings(
    lubridate::dmy(x)
  )
  
  if (all(!is.na(y))) {
    return(y)
  }
  
  as.Date(x)
}

era_label <- function(date_value) {
  as.character(
    lubridate::year(date_value)
  )
}

normalise_tournament <- function(x) {
  x <- trimws(
    as.character(x)
  )
  
  x[
    is.na(x) |
      x == ""
  ] <- "Go"
  
  x
}

clean_name_basic <- function(x) {
  x <- as.character(x)
  
  x <- str_replace_all(
    x,
    "\u00A0",
    " "
  )
  
  x <- str_replace_all(
    x,
    "\\{",
    "("
  )
  
  x <- str_replace_all(
    x,
    "\\}",
    ")"
  )
  
  x <- str_replace_all(
    x,
    "\u2018|\u2019",
    "'"
  )
  
  x <- str_replace_all(
    x,
    "\\s+",
    " "
  )
  
  x <- str_trim(x)
  
  x[x == ""] <- NA_character_
  
  x
}

normalise_player_name_for_lookup <- function(x) {
  x <- clean_name_basic(x)
  
  x <- str_remove(
    x,
    "\\s+[0-9]p$"
  )
  
  str_trim(x)
}

normalise_country_code <- function(x) {
  x <- tolower(
    trimws(
      as.character(x)
    )
  )
  
  x <- str_remove(
    x,
    "\\s*flag\\s*$"
  )
  
  x <- str_trim(x)
  
  x[x == ""] <- NA_character_
  
  x
}

apply_name_fixes <- function(
    x,
    fixes_tbl
) {
  x <- as.character(x)
  
  if (nrow(fixes_tbl) == 0) {
    return(x)
  }
  
  idx <- match(
    x,
    fixes_tbl$from_name
  )
  
  ifelse(
    !is.na(idx),
    fixes_tbl$to_name[idx],
    x
  )
}

elo_expected <- function(
    player_elo,
    opponent_elo
) {
  1 / (
    1 +
      10 ^ (
        (
          opponent_elo -
            player_elo
        ) /
          400
      )
  )
}

safe_pct <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  x <- pmax(
    0,
    pmin(
      1,
      x
    )
  )
  
  as.integer(
    round(
      100 * x
    )
  )
}

write_json_compact <- function(
    x,
    path,
    na = "null"
) {
  write_json(
    x,
    path,
    auto_unbox = TRUE,
    pretty = FALSE,
    na = na
  )
}

# -----------------------------
# Input checks
# -----------------------------
if (!file.exists(final_csv)) {
  stop(
    "Missing file: ",
    final_csv
  )
}

if (!file.exists(hist_csv)) {
  stop(
    "Missing file: ",
    hist_csv
  )
}

if (!file.exists(name_country_file)) {
  stop(
    "Missing file: ",
    name_country_file
  )
}

if (!file.exists(goratings_homepage_file)) {
  warning(
    "GoRatings homepage metadata file is missing: ",
    goratings_homepage_file,
    ". Flags and gender will use only the historical lookup."
  )
}

# -----------------------------
# Load ratings and history
# -----------------------------
final_raw <- read_csv(
  final_csv,
  show_col_types = FALSE,
  locale = locale(
    encoding = "UTF-8"
  ),
  col_types = cols(
    goratings_id = col_character(),
    .default = col_guess()
  )
)

ghist <- read_csv(
  hist_csv,
  show_col_types = FALSE,
  locale = locale(
    encoding = "UTF-8"
  )
)

required_final <- c(
  "name",
  "rating",
  "games"
)

required_hist <- c(
  "Date",
  "Black",
  "White",
  "Rb_Before",
  "Rw_Before",
  "Rb_After",
  "Rw_After"
)

missing_final <- setdiff(
  required_final,
  names(final_raw)
)

missing_hist <- setdiff(
  required_hist,
  names(ghist)
)

if (length(missing_final) > 0) {
  stop(
    "Missing columns in final CSV: ",
    paste(
      missing_final,
      collapse = ", "
    )
  )
}

if (length(missing_hist) > 0) {
  stop(
    "Missing columns in history CSV: ",
    paste(
      missing_hist,
      collapse = ", "
    )
  )
}

has_expected_cols <- all(
  c(
    "ExpectedBlack",
    "ExpectedWhite"
  ) %in%
    names(ghist)
)

if (!has_expected_cols) {
  warning(
    "ExpectedBlack and ExpectedWhite were not found. ",
    "They will be calculated from pre-game Elo."
  )
}

# -----------------------------
# Load name fixes
# -----------------------------
name_fixes <- tibble(
  from_name = character(),
  to_name = character()
)

if (file.exists(name_fixes_file)) {
  name_fixes <- read_csv(
    name_fixes_file,
    show_col_types = FALSE,
    locale = locale(
      encoding = "UTF-8"
    )
  ) %>%
    transmute(
      from_name =
        normalise_player_name_for_lookup(
          from_name
        ),
      to_name =
        normalise_player_name_for_lookup(
          to_name
        )
    ) %>%
    filter(
      !is.na(from_name),
      from_name != "",
      !is.na(to_name),
      to_name != ""
    ) %>%
    distinct(
      from_name,
      .keep_all = TRUE
    )
}

# -----------------------------
# Load historical name-country lookup
# -----------------------------
name_country_raw <- read_csv(
  name_country_file,
  col_names = c(
    "Name",
    "Gender",
    "Country"
  ),
  show_col_types = FALSE,
  locale = locale(
    encoding = "UTF-8"
  )
)

name_country <- name_country_raw %>%
  transmute(
    name =
      normalise_player_name_for_lookup(
        Name
      ),
    name = apply_name_fixes(
      name,
      name_fixes
    ),
    gender_symbol = str_trim(
      as.character(Gender)
    ),
    gender = case_when(
      gender_symbol == "♂" ~
        "male",
      gender_symbol == "♀" ~
        "female",
      TRUE ~
        NA_character_
    ),
    flag = normalise_country_code(
      Country
    )
  ) %>%
  filter(
    !is.na(name),
    name != "",
    !is.na(flag),
    flag != ""
  )

name_country_dedup <- name_country %>%
  count(
    name,
    flag,
    gender,
    sort = TRUE
  ) %>%
  group_by(name) %>%
  slice_max(
    order_by = n,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    name,
    historical_flag = flag,
    historical_gender = gender
  )

# -----------------------------
# Load manual flag overrides
# -----------------------------
flag_overrides <- tibble(
  name = character(),
  override_flag = character()
)

if (file.exists(flag_overrides_file)) {
  flag_overrides <- read_csv(
    flag_overrides_file,
    show_col_types = FALSE,
    locale = locale(
      encoding = "UTF-8"
    )
  ) %>%
    transmute(
      name =
        normalise_player_name_for_lookup(
          name
        ),
      name = apply_name_fixes(
        name,
        name_fixes
      ),
      override_flag =
        normalise_country_code(
          flag
        )
    ) %>%
    filter(
      !is.na(name),
      name != "",
      !is.na(override_flag),
      override_flag != ""
    ) %>%
    distinct(
      name,
      .keep_all = TRUE
    )
}

# -----------------------------
# Load GoRatings homepage metadata
# -----------------------------
goratings_homepage <- tibble(
  goratings_id = character(),
  goratings_name = character(),
  goratings_flag = character(),
  goratings_gender = character()
)

if (file.exists(goratings_homepage_file)) {
  goratings_homepage <- read_csv(
    goratings_homepage_file,
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
      goratings_id =
        as.character(player_id),
      goratings_name =
        normalise_player_name_for_lookup(
          player_name
        ),
      goratings_flag =
        normalise_country_code(
          flag
        ),
      goratings_gender = case_when(
        str_to_lower(
          str_trim(
            as.character(gender)
          )
        ) %in%
          c(
            "male",
            "female"
          ) ~
          str_to_lower(
            str_trim(
              as.character(gender)
            )
          ),
        TRUE ~
          NA_character_
      )
    ) %>%
    filter(
      !is.na(goratings_id),
      goratings_id != ""
    ) %>%
    distinct(
      goratings_id,
      .keep_all = TRUE
    )
}

cat(
  "Name-country rows loaded:",
  nrow(name_country_raw),
  "\n"
)

cat(
  "Unique historical player-country rows:",
  nrow(name_country_dedup),
  "\n"
)

cat(
  "Name fixes loaded:",
  nrow(name_fixes),
  "\n"
)

cat(
  "Manual flag overrides loaded:",
  nrow(flag_overrides),
  "\n"
)

cat(
  "GoRatings homepage metadata rows:",
  nrow(goratings_homepage),
  "\n"
)

# -----------------------------
# Clean history
# -----------------------------
ghist <- ghist %>%
  mutate(
    Date = parse_date_robust(Date),
    
    Black =
      normalise_player_name_for_lookup(
        Black
      ),
    
    White =
      normalise_player_name_for_lookup(
        White
      ),
    
    Black = apply_name_fixes(
      Black,
      name_fixes
    ),
    
    White = apply_name_fixes(
      White,
      name_fixes
    ),
    
    Rb_Before =
      suppressWarnings(
        as.numeric(Rb_Before)
      ),
    
    Rw_Before =
      suppressWarnings(
        as.numeric(Rw_Before)
      ),
    
    Rb_After =
      suppressWarnings(
        as.numeric(Rb_After)
      ),
    
    Rw_After =
      suppressWarnings(
        as.numeric(Rw_After)
      ),
    
    ExpectedBlack = if (
      "ExpectedBlack" %in%
      names(.)
    ) {
      suppressWarnings(
        as.numeric(ExpectedBlack)
      )
    } else {
      elo_expected(
        Rb_Before,
        Rw_Before
      )
    },
    
    ExpectedWhite = if (
      "ExpectedWhite" %in%
      names(.)
    ) {
      suppressWarnings(
        as.numeric(ExpectedWhite)
      )
    } else {
      elo_expected(
        Rw_Before,
        Rb_Before
      )
    }
  ) %>%
  mutate(
    ExpectedBlack = if_else(
      is.finite(ExpectedBlack),
      ExpectedBlack,
      elo_expected(
        Rb_Before,
        Rw_Before
      )
    ),
    
    ExpectedWhite = if_else(
      is.finite(ExpectedWhite),
      ExpectedWhite,
      1 - ExpectedBlack
    )
  ) %>%
  filter(
    !is.na(Date),
    !is.na(Black),
    Black != "",
    !is.na(White),
    White != ""
  ) %>%
  filter(
    Date >
      as.Date("1949-12-31")
  )

if (nrow(ghist) == 0L) {
  stop(
    "No game-history rows remain after cleaning."
  )
}

# -----------------------------
# Tournament field
# -----------------------------
if (!("tournament" %in% names(ghist))) {
  if ("Tournament" %in% names(ghist)) {
    ghist <- ghist %>%
      mutate(
        tournament =
          normalise_tournament(
            Tournament
          )
      )
  } else if ("Event" %in% names(ghist)) {
    ghist <- ghist %>%
      mutate(
        tournament =
          normalise_tournament(
            Event
          )
      )
  } else if ("Source" %in% names(ghist)) {
    ghist <- ghist %>%
      mutate(
        tournament =
          normalise_tournament(
            Source
          )
      )
  } else {
    ghist <- ghist %>%
      mutate(
        tournament = "Go"
      )
  }
} else {
  ghist <- ghist %>%
    mutate(
      tournament =
        normalise_tournament(
          tournament
        )
    )
}

# -----------------------------
# Game key
# -----------------------------
if (!("GameKey" %in% names(ghist))) {
  ghist <- ghist %>%
    mutate(
      GameKey = paste(
        format(
          Date,
          "%Y-%m-%d"
        ),
        Black,
        White,
        if (
          "ResultCode" %in%
          names(.)
        ) {
          as.character(ResultCode)
        } else {
          ""
        },
        tournament,
        row_number(),
        sep = "|"
      )
    )
}

# -----------------------------
# Result display and eras
# -----------------------------
if (!("ResultCode" %in% names(ghist))) {
  ghist <- ghist %>%
    mutate(
      ResultCode =
        NA_character_
    )
}

ghist <- ghist %>%
  mutate(
    result = case_when(
      startsWith(
        as.character(ResultCode),
        "B"
      ) ~
        "1-0",
      
      startsWith(
        as.character(ResultCode),
        "W"
      ) ~
        "0-1",
      
      startsWith(
        as.character(ResultCode),
        "D"
      ) ~
        "½-½",
      
      startsWith(
        as.character(ResultCode),
        "J"
      ) ~
        "½-½",
      
      TRUE ~
        NA_character_
    ),
    
    era = era_label(Date),
    
    era_year =
      as.integer(era)
  )

# -----------------------------
# Clean final ratings
# -----------------------------
final <- final_raw %>%
  mutate(
    name =
      normalise_player_name_for_lookup(
        name
      ),
    
    name = apply_name_fixes(
      name,
      name_fixes
    ),
    
    goratings_id = if (
      "goratings_id" %in%
      names(final_raw)
    ) {
      as.character(goratings_id)
    } else {
      NA_character_
    }
  ) %>%
  filter(
    name %in%
      unique(
        c(
          ghist$Black,
          ghist$White
        )
      )
  ) %>%
  filter(
    !is.na(games),
    games >= MIN_GAMES_FOR_TABLE
  )

# -----------------------------
# meta.json
# -----------------------------
asof_date <- max(
  ghist$Date,
  na.rm = TRUE
)

meta <- list(
  asof = format(
    asof_date,
    "%Y-%m-%d"
  ),
  
  games = nrow(ghist),
  
  sources = list(
    list(
      name = "GoGoD",
      period =
        "Through 2025-12-31"
    ),
    list(
      name = "GoRatings",
      period =
        "From 2026-01-01",
      url =
        "https://www.goratings.org/en",
      credit =
        "GoRatings data by Rémi Coulom"
    )
  ),
  
  min_games_for_table =
    MIN_GAMES_FOR_TABLE,
  
  history_has_world_rank =
    TRUE,
  
  games_have_world_rank =
    TRUE,
  
  games_have_expected_wl =
    TRUE,
  
  rank_inactive_years =
    RANK_INACTIVE_YEARS,
  
  rank_method = paste0(
    "Ranked by latest known Elo on each game date. ",
    "Only players eligible for the public table are ranked. ",
    "Players inactive for more than ",
    RANK_INACTIVE_YEARS,
    " years at that date are excluded."
  ),
  
  expected_wl_method = paste0(
    "Black and White win percentages are based on each side's pre-game Elo ",
    "expected score. Draw probability is zero for Go."
  )
)

write_json_compact(
  meta,
  file.path(
    data_dir,
    "meta.json"
  )
)

cat(
  "Wrote meta.json (asof =",
  meta$asof,
  ")\n"
)

# -----------------------------
# Build long history
# -----------------------------
hist_long_base <- bind_rows(
  ghist %>%
    transmute(
      name = Black,
      date = Date,
      rating = Rb_After,
      era,
      era_year,
      tournament
    ),
  
  ghist %>%
    transmute(
      name = White,
      date = Date,
      rating = Rw_After,
      era,
      era_year,
      tournament
    )
) %>%
  filter(
    !is.na(name),
    name != "",
    !is.na(date),
    !is.na(rating)
  ) %>%
  arrange(
    name,
    date
  )

hist_long <- hist_long_base %>%
  group_by(
    name,
    date
  ) %>%
  slice_tail(n = 1) %>%
  ungroup()

# -----------------------------
# Historical world ranks
# -----------------------------
cat(
  "Building historical world ranks...\n"
)

eligible_rank_names <- unique(
  final$name
)

rank_dt <- hist_long %>%
  filter(
    name %in%
      eligible_rank_names
  ) %>%
  select(
    name,
    date,
    rating
  ) %>%
  as.data.table()

rank_dt[
  ,
  date := as.Date(date)
]

setorder(
  rank_dt,
  name,
  date
)

rank_dates <- sort(
  unique(
    rank_dt$date
  )
)

inactive_days_int <- as.integer(
  round(
    RANK_INACTIVE_YEARS *
      365.25
  )
)

rank_rows <- vector(
  "list",
  length(rank_dates)
)

for (i in seq_along(rank_dates)) {
  d <- rank_dates[[i]]
  
  cutoff <- d -
    inactive_days_int
  
  current_ratings <- rank_dt[
    date <= d &
      date >= cutoff,
    .SD[.N],
    by = name
  ]
  
  if (nrow(current_ratings) > 0) {
    setorder(
      current_ratings,
      -rating,
      name
    )
    
    current_ratings[
      ,
      rank := frank(
        -rating,
        ties.method = "min"
      )
    ]
    
    current_ratings[
      ,
      rank_date := d
    ]
    
    rank_rows[[i]] <-
      current_ratings[
        ,
        .(
          name,
          rank_date,
          rank =
            as.integer(rank)
        )
      ]
  } else {
    rank_rows[[i]] <-
      data.table(
        name = character(),
        rank_date =
          as.Date(character()),
        rank = integer()
      )
  }
  
  if (i %% 500L == 0L) {
    cat(
      "Rank dates processed:",
      i,
      "of",
      length(rank_dates),
      "\n"
    )
  }
}

rank_tbl <- bind_rows(
  rank_rows
) %>%
  mutate(
    rank_date =
      as.Date(rank_date)
  )

hist_long <- hist_long %>%
  left_join(
    rank_tbl,
    by = c(
      "name" = "name",
      "date" = "rank_date"
    )
  )

cat(
  "Historical rank rows:",
  nrow(rank_tbl),
  "\n"
)

# -----------------------------
# Player peaks and activity
# -----------------------------
peaks_tbl <- hist_long %>%
  group_by(name) %>%
  summarise(
    peak = as.integer(
      round(
        max(
          rating,
          na.rm = TRUE
        )
      )
    ),
    
    peak_date = format(
      date[
        which.max(rating)
      ],
      "%Y-%m-%d"
    ),
    
    .groups = "drop"
  )

player_activity <- bind_rows(
  ghist %>%
    transmute(
      name = Black,
      date = Date
    ),
  
  ghist %>%
    transmute(
      name = White,
      date = Date
    )
) %>%
  group_by(name) %>%
  summarise(
    games = n(),
    
    first_date = min(
      date,
      na.rm = TRUE
    ),
    
    last_date = max(
      date,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# -----------------------------
# players.json
# -----------------------------
players_tbl <- final %>%
  mutate(
    id_base = slug(name)
  ) %>%
  group_by(id_base) %>%
  mutate(
    n = row_number(),
    
    id = if_else(
      n == 1L,
      id_base,
      paste0(
        id_base,
        "-",
        n - 1L
      )
    )
  ) %>%
  ungroup() %>%
  transmute(
    id = as.character(id),
    name = as.character(name),
    rating = as.integer(
      round(rating)
    ),
    goratings_id =
      as.character(goratings_id)
  ) %>%
  left_join(
    peaks_tbl,
    by = "name"
  ) %>%
  left_join(
    name_country_dedup,
    by = "name"
  ) %>%
  left_join(
    goratings_homepage,
    by = "goratings_id"
  ) %>%
  left_join(
    flag_overrides,
    by = "name"
  ) %>%
  left_join(
    player_activity,
    by = "name"
  ) %>%
  mutate(
    flag = coalesce(
      override_flag,
      goratings_flag,
      historical_flag
    ),
    
    gender = coalesce(
      goratings_gender,
      historical_gender
    ),
    
    games =
      as.integer(games),
    
    first_date = if_else(
      !is.na(first_date),
      format(
        first_date,
        "%Y-%m-%d"
      ),
      NA_character_
    ),
    
    last_date = if_else(
      !is.na(last_date),
      format(
        last_date,
        "%Y-%m-%d"
      ),
      NA_character_
    )
  ) %>%
  select(
    id,
    name,
    rating,
    peak,
    peak_date,
    flag,
    gender,
    games,
    first_date,
    last_date
  ) %>%
  arrange(
    desc(rating),
    name
  )

if (anyDuplicated(players_tbl$id)) {
  players_tbl <- players_tbl %>%
    group_by(id) %>%
    mutate(
      duplicate_number =
        row_number(),
      
      id = if_else(
        duplicate_number == 1L,
        id,
        paste0(
          id,
          "-",
          duplicate_number - 1L
        )
      )
    ) %>%
    ungroup() %>%
    select(
      -duplicate_number
    )
}

cat(
  "Players with flag:",
  sum(
    !is.na(players_tbl$flag) &
      players_tbl$flag != ""
  ),
  "of",
  nrow(players_tbl),
  "\n"
)

unmatched_flags <- players_tbl %>%
  filter(
    is.na(flag) |
      flag == "",
    !is.na(peak),
    peak >= 3200
  ) %>%
  filter(
    !is.na(games),
    games >=
      MIN_GAMES_FOR_TABLE
  ) %>%
  arrange(
    desc(peak),
    desc(rating),
    desc(games),
    name
  )

if (nrow(unmatched_flags) > 0) {
  print(
    unmatched_flags %>%
      select(
        name,
        peak,
        rating,
        games
      ),
    n = 100
  )
}

cat(
  "Players without flag (peak >= 3200):",
  nrow(unmatched_flags),
  "\n"
)

write_csv(
  unmatched_flags,
  file.path(
    data_dir,
    "players_missing_flags_peak_3200_plus.csv"
  )
)

name_to_id <- setNames(
  players_tbl$id,
  players_tbl$name
)

write_json_compact(
  players_tbl,
  file.path(
    data_dir,
    "players.json"
  )
)

cat(
  "Wrote players.json (n =",
  nrow(players_tbl),
  ")\n"
)

# -----------------------------
# era_starts.json
# -----------------------------
era_starts_long <- bind_rows(
  ghist %>%
    transmute(
      name = Black,
      date = Date,
      era,
      era_year,
      elo = Rb_Before,
      tournament
    ),
  
  ghist %>%
    transmute(
      name = White,
      date = Date,
      era,
      era_year,
      elo = Rw_Before,
      tournament
    )
) %>%
  filter(
    !is.na(name),
    name != "",
    !is.na(date),
    !is.na(era_year),
    !is.na(elo)
  ) %>%
  arrange(
    name,
    era_year,
    date
  ) %>%
  group_by(
    name,
    era_year
  ) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    id = unname(
      name_to_id[name]
    ),
    
    elo = as.integer(
      round(elo)
    )
  ) %>%
  filter(
    !is.na(id)
  ) %>%
  left_join(
    players_tbl %>%
      select(
        id,
        flag,
        gender
      ),
    by = "id"
  ) %>%
  transmute(
    era = as.character(era),
    season =
      as.integer(era_year),
    id = as.character(id),
    name = as.character(name),
    flag = as.character(flag),
    gender =
      as.character(gender),
    elo = as.integer(elo),
    tournament =
      as.character(tournament)
  ) %>%
  arrange(
    desc(season),
    desc(elo),
    name
  )

write_json_compact(
  era_starts_long,
  file.path(
    data_dir,
    "era_starts.json"
  )
)

cat(
  "Wrote era_starts.json (rows =",
  nrow(era_starts_long),
  ")\n"
)

# -----------------------------
# Per-player rating history JSON
# -----------------------------
n_history_written <- 0L

for (player_name in names(name_to_id)) {
  player_id <- name_to_id[[
    player_name
  ]]
  
  player_history <- hist_long %>%
    filter(
      name == player_name
    ) %>%
    arrange(date) %>%
    transmute(
      date = format(
        date,
        "%Y-%m-%d"
      ),
      era = as.character(era),
      rating = as.integer(
        round(rating)
      ),
      rank = as.integer(rank),
      tournament =
        as.character(tournament)
    )
  
  if (nrow(player_history) > 0) {
    write_json_compact(
      player_history,
      file.path(
        history_out,
        paste0(
          player_id,
          ".json"
        )
      )
    )
    
    n_history_written <-
      n_history_written + 1L
  }
}

cat(
  "Wrote rating history files:",
  n_history_written,
  "\n"
)

# -----------------------------
# Per-player games JSON
# -----------------------------
ghist_games <- ghist %>%
  mutate(
    ExpectedBlack = if_else(
      is.finite(ExpectedBlack),
      ExpectedBlack,
      elo_expected(
        Rb_Before,
        Rw_Before
      )
    ),
    
    ExpectedWhite = if_else(
      is.finite(ExpectedWhite),
      ExpectedWhite,
      1 - ExpectedBlack
    )
  ) %>%
  transmute(
    date = Date,
    era = as.character(era),
    season =
      as.integer(era_year),
    tournament =
      as.character(tournament),
    black =
      as.character(Black),
    blackElo =
      as.integer(
        round(Rb_Before)
      ),
    white =
      as.character(White),
    whiteElo =
      as.integer(
        round(Rw_Before)
      ),
    blackWinPct =
      safe_pct(ExpectedBlack),
    whiteWinPct =
      safe_pct(ExpectedWhite),
    result =
      as.character(result),
    blackBeforeRaw =
      Rb_Before,
    blackAfterRaw =
      Rb_After,
    whiteBeforeRaw =
      Rw_Before,
    whiteAfterRaw =
      Rw_After,
    GameKey =
      as.character(GameKey)
  )

player_rank_lookup <- hist_long %>%
  select(
    name,
    date,
    rank
  ) %>%
  filter(
    !is.na(rank)
  ) %>%
  arrange(
    name,
    date
  ) %>%
  group_by(
    name,
    date
  ) %>%
  slice_tail(n = 1) %>%
  ungroup()

n_games_written <- 0L

for (player_name in names(name_to_id)) {
  player_id <- name_to_id[[
    player_name
  ]]
  
  player_games <- ghist_games %>%
    filter(
      black == player_name |
        white == player_name
    ) %>%
    arrange(
      desc(date)
    ) %>%
    distinct(
      GameKey,
      .keep_all = TRUE
    ) %>%
    mutate(
      delta_num = case_when(
        black == player_name ~
          blackAfterRaw -
          blackBeforeRaw,
        
        white == player_name ~
          whiteAfterRaw -
          whiteBeforeRaw,
        
        TRUE ~
          NA_real_
      ),
      
      delta = ifelse(
        !is.na(delta_num),
        sprintf(
          "%+0.1f",
          round(
            delta_num,
            1
          )
        ),
        NA_character_
      )
    ) %>%
    left_join(
      player_rank_lookup %>%
        filter(
          name == player_name
        ) %>%
        select(
          date,
          rank
        ),
      by = "date"
    ) %>%
    transmute(
      date = format(
        date,
        "%Y-%m-%d"
      ),
      era = as.character(era),
      season =
        as.integer(season),
      tournament =
        as.character(tournament),
      black =
        as.character(black),
      blackElo =
        as.integer(blackElo),
      white =
        as.character(white),
      whiteElo =
        as.integer(whiteElo),
      blackWinPct =
        as.integer(blackWinPct),
      whiteWinPct =
        as.integer(whiteWinPct),
      result =
        as.character(result),
      delta =
        as.character(delta),
      rank =
        as.integer(rank)
    )
  
  if (nrow(player_games) > 0) {
    write_json_compact(
      player_games,
      file.path(
        games_out,
        paste0(
          player_id,
          ".json"
        )
      )
    )
    
    n_games_written <-
      n_games_written + 1L
  }
}

cat(
  "Wrote games files:",
  n_games_written,
  "\n"
)

cat("Done.\n")