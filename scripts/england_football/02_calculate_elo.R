
library(data.table)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

INPUT_CSV <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Matches_Clean_Combined",
  "england_leagues_1_to_5_all_seasons.csv"
)

TEAM_ALIASES_CSV <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Reference",
  "team_aliases.csv"
)

OUT_DIR <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Elo"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTPUT_GAME_HISTORY_CSV_PASS1  <- file.path(OUT_DIR, "football_elo_game_history_pass1.csv")
OUTPUT_FINAL_RATINGS_CSV_PASS1 <- file.path(OUT_DIR, "football_elo_final_ratings_pass1.csv")

OUTPUT_GAME_HISTORY_CSV  <- file.path(OUT_DIR, "football_elo_game_history.csv")
OUTPUT_FINAL_RATINGS_CSV <- file.path(OUT_DIR, "football_elo_final_ratings.csv")
OUTPUT_UPCOMING_FIXTURES_CSV <- file.path(OUT_DIR, "football_upcoming_fixtures.csv")

# -----------------------------
# Elo settings
# -----------------------------
K_NORMAL <- 20
K_CONTINENTAL <- 30
K_NEW <- 20
K_NEW_GAMES <- 100L

# Country-specific starting seeds.
#
# Tier 1 values come from the UEFA top-club calibration adjusted for each
# country's observed top-6-to-full-league depth.
#
# Tier 2 values use each country's historical median Tier 1 -> Tier 2 gap:
#   England 222
#   Spain   185
#   France  194
#   Germany 194
#   Italy   196
#
# England Tiers 3-5 remain at their existing absolute seeds.
# Smaller European leagues are currently Tier 1 only and use provisional seeds
# that can be recalibrated later from the expanded UEFA network.

COUNTRY_TIER_SEEDS <- data.table(
  Country = c(
    "England", "England", "England", "England", "England",
    "Spain",   "Spain",
    "France",  "France",
    "Germany", "Germany",
    "Italy",   "Italy",
    "Portugal",
    "Netherlands",
    "Belgium",
    "Austria",
    "Turkey",
    "Scotland",
    "Switzerland",
    "Greece",
    "Czechia",
    "Ukraine"
  ),
  Tier = c(
    1L, 2L, 3L, 4L, 5L,
    1L, 2L,
    1L, 2L,
    1L, 2L,
    1L, 2L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L
  ),
  SeedRating = c(
    2750, 2528, 2350, 2150, 1950,
    2737, 2552,
    2689, 2495,
    2685, 2491,
    2675, 2479,
    2550,  # Portugal
    2575,  # Netherlands
    2515,  # Belgium
    2510,  # Austria
    2510,  # Turkey
    2430,  # Scotland
    2500,  # Switzerland
    2450,  # Greece
    2430,  # Czechia
    2475   # Ukraine
  )
)

COUNTRY_TIER_SEED_KEY <- paste(
  COUNTRY_TIER_SEEDS$Country,
  COUNTRY_TIER_SEEDS$Tier,
  sep = "\r"
)

COUNTRY_TIER_SEED_MAP <- setNames(
  COUNTRY_TIER_SEEDS$SeedRating,
  COUNTRY_TIER_SEED_KEY
)

RETRO_GAMES_N <- 50L

# -----------------------------
# Helpers
# -----------------------------
expected_score <- function(Ra, Rb) {
  1 / (1 + 10 ^ ((Rb - Ra) / 400))
}

result_to_scores <- function(res) {
  if (is.na(res) || !nzchar(trimws(res))) return(c(NA_real_, NA_real_))
  
  r <- trimws(as.character(res))
  parts <- strsplit(r, "-", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(c(NA_real_, NA_real_))
  
  a <- suppressWarnings(as.numeric(trimws(parts[1])))
  b <- suppressWarnings(as.numeric(trimws(parts[2])))
  if (is.na(a) || is.na(b)) return(c(NA_real_, NA_real_))
  
  if (a > b) return(c(1, 0))
  if (a < b) return(c(0, 1))
  c(0.5, 0.5)
}

seed_from_country_tier <- function(country, tier) {
  country <- trimws(as.character(country))
  tier <- as.integer(tier)
  
  key <- paste(
    country,
    tier,
    sep = "\r"
  )
  
  out <- rep(NA_real_, length(key))
  
  matched <- key %in% names(
    COUNTRY_TIER_SEED_MAP
  )
  
  out[matched] <- as.numeric(
    COUNTRY_TIER_SEED_MAP[
      key[matched]
    ]
  )
  
  out
}

# -----------------------------
# Team-name aliases
# -----------------------------
if (!file.exists(TEAM_ALIASES_CSV)) {
  stop(
    "Team alias file not found:\n",
    TEAM_ALIASES_CSV,
    "\n\nExpected columns: Country, SourceName, CanonicalName"
  )
}

team_aliases <- fread(
  TEAM_ALIASES_CSV,
  encoding = "UTF-8",
  na.strings = c("", "NA")
)

required_alias_cols <- c("Country", "SourceName", "CanonicalName")
missing_alias_cols <- setdiff(required_alias_cols, names(team_aliases))

if (length(missing_alias_cols) > 0) {
  stop(
    "team_aliases.csv is missing required column(s): ",
    paste(missing_alias_cols, collapse = ", ")
  )
}

team_aliases[, Country := trimws(as.character(Country))]
team_aliases[, SourceName := trimws(as.character(SourceName))]
team_aliases[, CanonicalName := trimws(as.character(CanonicalName))]

team_aliases <- team_aliases[
  !is.na(Country) & Country != "" &
    !is.na(SourceName) & SourceName != "" &
    !is.na(CanonicalName) & CanonicalName != ""
]

alias_conflicts <- team_aliases[
  ,
  .(CanonicalNames = uniqueN(CanonicalName)),
  by = .(Country, SourceName)
][CanonicalNames > 1L]

if (nrow(alias_conflicts) > 0) {
  stop(
    "Conflicting team aliases found in team_aliases.csv for:\n",
    paste0(
      alias_conflicts$Country,
      " | ",
      alias_conflicts$SourceName,
      collapse = "\n"
    )
  )
}

team_aliases <- unique(
  team_aliases[, .(Country, SourceName, CanonicalName)],
  by = c("Country", "SourceName")
)

team_alias_key <- paste(
  team_aliases$Country,
  team_aliases$SourceName,
  sep = "\r"
)

team_alias_map <- setNames(
  team_aliases$CanonicalName,
  team_alias_key
)

# Continental files use Country = "Europe", so country-specific alias lookup
# cannot be used directly there. Build a second lookup only for source names
# that map unambiguously to one canonical club across all countries.
continental_alias_candidates <- team_aliases[
  ,
  .(
    CanonicalNames = uniqueN(CanonicalName),
    CanonicalName = CanonicalName[1L]
  ),
  by = SourceName
][CanonicalNames == 1L]

continental_alias_map <- setNames(
  continental_alias_candidates$CanonicalName,
  continental_alias_candidates$SourceName
)

cat(
  "Loaded team aliases:",
  nrow(team_aliases),
  "|",
  TEAM_ALIASES_CSV,
  "\n"
)

is_clearly_bad_team_name <- function(x) {
  x <- trimws(as.character(x))
  
  bad <- is.na(x) | x == ""
  bad <- bad | grepl("^\\(.*\\)$", x)
  bad <- bad | grepl("\\bv\\b", x)
  bad <- bad | grepl("\\[awarded\\]", x, ignore.case = TRUE)
  bad <- bad | grepl("^[0-9.\\-]+$", x)
  bad <- bad | grepl(",", x)
  
  bad
}

normalise_team_name <- function(x, country) {
  x0 <- trimws(as.character(x))
  country0 <- trimws(as.character(country))
  
  x0[is_clearly_bad_team_name(x0)] <- NA_character_
  x0 <- gsub("\\s*\\[.*?\\]\\s*$", "", x0, perl = TRUE)
  x0 <- gsub("\\s+", " ", x0)
  x0 <- trimws(x0)
  
  key <- paste(country0, x0, sep = "\r")
  matched <- key %in% names(team_alias_map)
  
  out <- x0
  out[matched] <- unname(team_alias_map[key[matched]])
  
  # For continental rows, use a source-name alias only when it resolves to
  # exactly one canonical club across the domestic alias registry.
  is_continental_country <- country0 == "Europe"
  continental_matched <- is_continental_country & x0 %in% names(continental_alias_map)
  out[continental_matched] <- unname(
    continental_alias_map[x0[continental_matched]]
  )
  
  out[is_clearly_bad_team_name(out)] <- NA_character_
  out
}

# -----------------------------
# Load and prepare data
# -----------------------------
dt <- fread(INPUT_CSV)

required_cols <- c("Country", "Competition", "CompetitionType", "Tier", "League", "Date", "Home", "Away", "Result")
missing_cols <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dt[, Country := trimws(as.character(Country))]
dt[, Competition := trimws(as.character(Competition))]
dt[, CompetitionType := trimws(as.character(CompetitionType))]
dt[, Tier := as.integer(Tier)]
dt[, League  := trimws(as.character(League))]
dt[, Source := if ("Source" %in% names(dt)) trimws(as.character(Source)) else NA_character_]
dt[, DateRaw := trimws(as.character(Date))]
dt[, HomeRaw := trimws(as.character(Home))]
dt[, AwayRaw := trimws(as.character(Away))]
dt[, Score   := if ("Score" %in% names(dt)) trimws(as.character(Score)) else NA_character_]

dt[, Home := normalise_team_name(HomeRaw, Country)]
dt[, Away := normalise_team_name(AwayRaw, Country)]
dt[, Result := trimws(as.character(Result))]

bad_name_rows <- dt[
  is.na(Home) | Home == "" |
    is.na(Away) | Away == ""
]

if (nrow(bad_name_rows) > 0) {
  cat("\nDropping rows with bad parsed team names:\n")
  print(bad_name_rows[, .(Country, Competition, League, DateRaw, HomeRaw, AwayRaw, Result, Score)])
}

dt <- dt[
  !(is.na(Home) | Home == "" |
      is.na(Away) | Away == "")
]

# Domestic cups can include clubs below J-Ratings' covered domestic leagues.
# A domestic cup match counts only when BOTH clubs have already appeared in a
# covered domestic league in that same country by the date of the cup match.
# This prevents unseeded lower/non-covered cup clubs from entering Elo.

cup_domestic_appearances <- rbindlist(list(
  dt[CompetitionType == "league", .(Country, Team = Home, DateRaw)],
  dt[CompetitionType == "league", .(Country, Team = Away, DateRaw)]
), use.names = TRUE)

cup_domestic_appearances[, Date := as.Date(DateRaw, format = "%Y-%m-%d")]

cup_first_domestic <- cup_domestic_appearances[
  !is.na(Date),
  .(FirstDomesticDate = min(Date)),
  by = .(Country, Team)
]

domestic_cup_rows <- dt[CompetitionType == "domestic_cup"]

if (nrow(domestic_cup_rows) > 0) {
  domestic_cup_rows[, MatchDate := as.Date(DateRaw, format = "%Y-%m-%d")]
  
  domestic_cup_rows[
    cup_first_domestic,
    on = .(Country, Home = Team),
    HomeFirstDomestic := i.FirstDomesticDate
  ]
  
  domestic_cup_rows[
    cup_first_domestic,
    on = .(Country, Away = Team),
    AwayFirstDomestic := i.FirstDomesticDate
  ]
  
  domestic_cup_keep <- domestic_cup_rows[
    !is.na(HomeFirstDomestic) &
      !is.na(AwayFirstDomestic) &
      HomeFirstDomestic <= MatchDate &
      AwayFirstDomestic <= MatchDate
  ]
  
  domestic_cup_drop <- domestic_cup_rows[
    is.na(HomeFirstDomestic) |
      is.na(AwayFirstDomestic) |
      HomeFirstDomestic > MatchDate |
      AwayFirstDomestic > MatchDate
  ]
  
  cat(
    "\nDomestic cup match eligibility:\n",
    "  Domestic cup rows available: ", nrow(domestic_cup_rows), "\n",
    "  Domestic cup rows kept: ", nrow(domestic_cup_keep), "\n",
    "  Domestic cup rows dropped (club absent/not yet domestically covered): ",
    nrow(domestic_cup_drop), "\n",
    sep = ""
  )
  
  domestic_cup_keep[, c("MatchDate", "HomeFirstDomestic", "AwayFirstDomestic") := NULL]
  domestic_cup_drop[, c("MatchDate", "HomeFirstDomestic", "AwayFirstDomestic") := NULL]
  domestic_cup_rows[, c("MatchDate", "HomeFirstDomestic", "AwayFirstDomestic") := NULL]
  
  dt <- rbindlist(
    list(
      dt[CompetitionType != "domestic_cup"],
      domestic_cup_keep
    ),
    use.names = TRUE
  )
}

# Continental matches count only when BOTH clubs have already appeared in a
# covered domestic league by the date of that continental match. This avoids
# seeding a club from a Tier = NA Champions League row before its domestic
# history begins. The complete Champions League source stays in the combined
# CSV, so older games can become eligible later if more domestic history or
# leagues are added.
domestic_appearances <- rbindlist(list(
  dt[CompetitionType == "league", .(Team = Home, DateRaw)],
  dt[CompetitionType == "league", .(Team = Away, DateRaw)]
), use.names = TRUE)

domestic_appearances[, Date := as.Date(DateRaw, format = "%Y-%m-%d")]

first_domestic_date <- domestic_appearances[
  !is.na(Team) & Team != "" & !is.na(Date),
  .(FirstDomesticDate = min(Date)),
  by = Team
]

first_domestic_map <- setNames(
  first_domestic_date$FirstDomesticDate,
  first_domestic_date$Team
)

continental_rows <- dt[CompetitionType == "continental"]

if (nrow(continental_rows) > 0) {
  continental_rows[, MatchDate := as.Date(DateRaw, format = "%Y-%m-%d")]
  continental_rows[, HomeFirstDomestic := as.Date(first_domestic_map[Home], origin = "1970-01-01")]
  continental_rows[, AwayFirstDomestic := as.Date(first_domestic_map[Away], origin = "1970-01-01")]
  
  continental_keep <- continental_rows[
    !is.na(HomeFirstDomestic) &
      !is.na(AwayFirstDomestic) &
      MatchDate >= HomeFirstDomestic &
      MatchDate >= AwayFirstDomestic
  ]
  
  continental_drop <- continental_rows[
    is.na(HomeFirstDomestic) |
      is.na(AwayFirstDomestic) |
      MatchDate < HomeFirstDomestic |
      MatchDate < AwayFirstDomestic
  ]
  
  continental_keep[, c("MatchDate", "HomeFirstDomestic", "AwayFirstDomestic") := NULL]
  continental_drop[, c("MatchDate", "HomeFirstDomestic", "AwayFirstDomestic") := NULL]
  
  cat(
    "\nContinental match eligibility:\n",
    "  Domestic teams in database: ", nrow(first_domestic_date), "\n",
    "  Continental rows available: ", nrow(continental_rows), "\n",
    "  Continental rows kept: ", nrow(continental_keep), "\n",
    "  Continental rows dropped (club absent/not yet domestically covered): ",
    nrow(continental_drop), "\n",
    sep = ""
  )
  
  continental_rows[, c("MatchDate", "HomeFirstDomestic", "AwayFirstDomestic") := NULL]
  
  dt <- rbindlist(
    list(
      dt[CompetitionType != "continental"],
      continental_keep
    ),
    use.names = TRUE,
    fill = TRUE
  )
}

dt[, Date := as.Date(DateRaw, format = "%Y-%m-%d")]
dt[, SeedRatingForTier := seed_from_country_tier(Country, Tier)]

bad_tier_rows <- dt[
  CompetitionType == "league" &
    (
      is.na(Tier) |
        is.na(SeedRatingForTier)
    )
]

if (nrow(bad_tier_rows) > 0) {
  stop(
    "League match row(s) have a missing/unsupported Tier or no country-specific seed.\n",
    "Add the missing Country/Tier combination to COUNTRY_TIER_SEEDS.\n\n",
    paste0(
      unique(
        paste(
          bad_tier_rows$Country,
          bad_tier_rows$Competition,
          bad_tier_rows$League,
          bad_tier_rows$Tier,
          sep = " | "
        )
      ),
      collapse = "\n"
    )
  )
}

# -----------------------------
# Upcoming fixtures
# -----------------------------

upcoming_fixtures <- dt[
  !is.na(Date) &
    !is.na(Home) & Home != "" &
    !is.na(Away) & Away != "" &
    (is.na(Result) | Result == "")
]

upcoming_fixtures <- upcoming_fixtures[, .(
  Country,
  Competition,
  CompetitionType,
  League,
  Tier,
  Source,
  Date = format(Date, "%Y-%m-%d"),
  Home,
  Away
)]

setorder(upcoming_fixtures, Date, Country, Tier, Competition, League, Home, Away)

fwrite(
  upcoming_fixtures,
  OUTPUT_UPCOMING_FIXTURES_CSV
)

cat(
  "Upcoming fixtures written:",
  nrow(upcoming_fixtures),
  "|",
  OUTPUT_UPCOMING_FIXTURES_CSV,
  "\n"
)

# Keep only completed matches for Elo calculation
dt <- dt[
  !is.na(Date) &
    !is.na(Home) & Home != "" &
    !is.na(Away) & Away != "" &
    !is.na(Result) &
    Result != ""
]

scores <- t(vapply(dt$Result, result_to_scores, numeric(2)))
dt[, HomeScore := scores[, 1]]
dt[, AwayScore := scores[, 2]]
dt <- dt[!is.na(HomeScore) & !is.na(AwayScore)]

setorder(dt, Date, Country, Tier, Competition, League, Home, Away, Result)

# -----------------------------
# Generic Elo runner
# -----------------------------
run_elo <- function(dt_input,
                    entry_mode = c("seed", "retro"),
                    retro_start_map = NULL,
                    pass_label = "pass") {
  
  entry_mode <- match.arg(entry_mode)
  
  n <- nrow(dt_input)
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Running ", pass_label, "\n", sep = "")
  cat("Matches to process:", n, "\n")
  cat("Date range:", as.character(min(dt_input$Date)), "to", as.character(max(dt_input$Date)), "\n")
  
  ratings_env <- new.env(hash = TRUE, parent = emptyenv())
  games_env   <- new.env(hash = TRUE, parent = emptyenv())
  
  first_league_env <- new.env(hash = TRUE, parent = emptyenv())
  first_tier_env   <- new.env(hash = TRUE, parent = emptyenv())
  first_date_env   <- new.env(hash = TRUE, parent = emptyenv())
  entry_rating_env <- new.env(hash = TRUE, parent = emptyenv())
  
  HomeVec <- dt_input$Home
  AwayVec <- dt_input$Away
  LeagueVec <- dt_input$League
  TierVec <- dt_input$Tier
  SeedVec <- dt_input$SeedRatingForTier
  DateVec <- dt_input$Date
  HomeScoreVec <- dt_input$HomeScore
  AwayScoreVec <- dt_input$AwayScore
  
  HomeFirstAppearance <- logical(n)
  AwayFirstAppearance <- logical(n)
  HomeStartRating <- numeric(n)
  AwayStartRating <- numeric(n)
  
  HomeGamesBefore   <- integer(n)
  AwayGamesBefore   <- integer(n)
  HomeRating_Before <- numeric(n)
  AwayRating_Before <- numeric(n)
  
  ExpectedHome <- numeric(n)
  ExpectedAway <- numeric(n)
  KHome <- numeric(n)
  KAway <- numeric(n)
  
  HomeRating_After <- numeric(n)
  AwayRating_After <- numeric(n)
  HomeGamesAfter <- integer(n)
  AwayGamesAfter <- integer(n)
  
  for (i in seq_len(n)) {
    home <- HomeVec[i]
    away <- AwayVec[i]
    league_i <- LeagueVec[i]
    tier_i <- TierVec[i]
    seed_i <- SeedVec[i]
    date_i <- DateVec[i]
    
    home_exists <- exists(home, envir = ratings_env, inherits = FALSE)
    away_exists <- exists(away, envir = ratings_env, inherits = FALSE)
    
    home_first <- !home_exists
    away_first <- !away_exists
    
    if (home_first) {
      home_entry <- if (entry_mode == "seed") {
        seed_i
      } else {
        if (!is.null(retro_start_map) && home %in% names(retro_start_map)) {
          as.numeric(retro_start_map[[home]])
        } else {
          seed_i
        }
      }
      
      assign(home, home_entry, envir = ratings_env)
      assign(home, 0L, envir = games_env)
      assign(home, league_i, envir = first_league_env)
      assign(home, tier_i, envir = first_tier_env)
      assign(home, date_i, envir = first_date_env)
      assign(home, home_entry, envir = entry_rating_env)
    }
    
    if (away_first) {
      away_entry <- if (entry_mode == "seed") {
        seed_i
      } else {
        if (!is.null(retro_start_map) && away %in% names(retro_start_map)) {
          as.numeric(retro_start_map[[away]])
        } else {
          seed_i
        }
      }
      
      assign(away, away_entry, envir = ratings_env)
      assign(away, 0L, envir = games_env)
      assign(away, league_i, envir = first_league_env)
      assign(away, tier_i, envir = first_tier_env)
      assign(away, date_i, envir = first_date_env)
      assign(away, away_entry, envir = entry_rating_env)
    }
    
    Rh <- get(home, envir = ratings_env, inherits = FALSE)
    Ra <- get(away, envir = ratings_env, inherits = FALSE)
    Gh <- get(home, envir = games_env, inherits = FALSE)
    Ga <- get(away, envir = games_env, inherits = FALSE)
    
    home_entry_assigned <- get(home, envir = entry_rating_env, inherits = FALSE)
    away_entry_assigned <- get(away, envir = entry_rating_env, inherits = FALSE)
    
    base_k <- if (dt_input$CompetitionType[i] == "continental") {
      K_CONTINENTAL
    } else {
      K_NORMAL
    }
    
    Kh <- if (Gh < K_NEW_GAMES) K_NEW else base_k
    Ka <- if (Ga < K_NEW_GAMES) K_NEW else base_k
    
    Eh <- expected_score(Rh, Ra)
    Ea <- 1 - Eh
    
    Sh <- HomeScoreVec[i]
    Sa <- AwayScoreVec[i]
    
    Rh_new <- Rh + Kh * (Sh - Eh)
    Ra_new <- Ra + Ka * (Sa - Ea)
    
    Gh_new <- Gh + 1L
    Ga_new <- Ga + 1L
    
    assign(home, Rh_new, envir = ratings_env)
    assign(away, Ra_new, envir = ratings_env)
    assign(home, Gh_new, envir = games_env)
    assign(away, Ga_new, envir = games_env)
    
    HomeFirstAppearance[i] <- home_first
    AwayFirstAppearance[i] <- away_first
    HomeStartRating[i] <- home_entry_assigned
    AwayStartRating[i] <- away_entry_assigned
    
    HomeGamesBefore[i] <- Gh
    AwayGamesBefore[i] <- Ga
    HomeRating_Before[i] <- Rh
    AwayRating_Before[i] <- Ra
    
    ExpectedHome[i] <- Eh
    ExpectedAway[i] <- Ea
    KHome[i] <- Kh
    KAway[i] <- Ka
    
    HomeRating_After[i] <- Rh_new
    AwayRating_After[i] <- Ra_new
    HomeGamesAfter[i] <- Gh_new
    AwayGamesAfter[i] <- Ga_new
    
    if (i %% 10000L == 0L) {
      cat("Processed", i, "matches (", round(100 * i / n, 1), "%)\n")
      flush.console()
    }
  }
  
  dt_out <- copy(dt_input)
  dt_out[, `:=`(
    HomeFirstAppearance = HomeFirstAppearance,
    AwayFirstAppearance = AwayFirstAppearance,
    HomeStartRating = HomeStartRating,
    AwayStartRating = AwayStartRating,
    HomeGamesBefore = HomeGamesBefore,
    AwayGamesBefore = AwayGamesBefore,
    HomeRating_Before = HomeRating_Before,
    AwayRating_Before = AwayRating_Before,
    ExpectedHome = ExpectedHome,
    ExpectedAway = ExpectedAway,
    KHome = KHome,
    KAway = KAway,
    HomeRating_After = HomeRating_After,
    AwayRating_After = AwayRating_After,
    HomeGamesAfter = HomeGamesAfter,
    AwayGamesAfter = AwayGamesAfter
  )]
  
  teams <- ls(ratings_env, all.names = TRUE)
  
  final_ratings <- data.table(
    Team = teams,
    Rating = as.numeric(mget(teams, envir = ratings_env)),
    Games = as.integer(unlist(mget(teams, envir = games_env))),
    FirstLeague = as.character(unlist(mget(teams, envir = first_league_env))),
    FirstTier = as.integer(unlist(mget(teams, envir = first_tier_env))),
    FirstMatchDate = as.Date(as.numeric(unlist(mget(teams, envir = first_date_env))), origin = "1970-01-01"),
    EntryRating = as.numeric(unlist(mget(teams, envir = entry_rating_env)))
  )
  
  setorder(final_ratings, -Rating, Team)
  final_ratings[, Rating := round(Rating, 0)]
  final_ratings[, EntryRating := round(EntryRating, 1)]
  final_ratings[, FirstMatchDate := format(FirstMatchDate, "%Y-%m-%d")]
  final_ratings[, IsSeed := Games >= 20]
  
  list(
    dt = dt_out,
    final = final_ratings
  )
}

build_retro_start_map <- function(pass1_dt, n_games = 100L) {
  team_games <- rbindlist(list(
    pass1_dt[, .(
      Team = Home,
      GamesAfter = HomeGamesAfter,
      RatingAfter = HomeRating_After,
      Date = Date
    )],
    pass1_dt[, .(
      Team = Away,
      GamesAfter = AwayGamesAfter,
      RatingAfter = AwayRating_After,
      Date = Date
    )]
  ), use.names = TRUE)
  
  setorder(team_games, Team, GamesAfter, Date)
  team_games <- team_games[, .SD[.N], by = .(Team, GamesAfter)]
  
  teams <- unique(team_games$Team)
  retro_list <- vector("list", length(teams))
  
  for (j in seq_along(teams)) {
    tm <- teams[j]
    x <- team_games[Team == tm][order(GamesAfter)]
    if (nrow(x) == 0) next
    
    if (any(x$GamesAfter == n_games)) {
      r0 <- x[GamesAfter == n_games][1L, RatingAfter]
    } else {
      r0 <- x[.N, RatingAfter]
    }
    
    retro_list[[j]] <- list(Team = tm, RetroStart = as.numeric(r0))
  }
  
  retro_dt <- rbindlist(retro_list, fill = TRUE)
  retro_dt <- retro_dt[!is.na(Team) & !is.na(RetroStart)]
  setNames(as.list(retro_dt$RetroStart), retro_dt$Team)
}

# -----------------------------
# Pass 1
# -----------------------------
pass1 <- run_elo(
  dt_input = dt,
  entry_mode = "seed",
  retro_start_map = NULL,
  pass_label = "Pass 1 (seed by first tier)"
)

game_history_out_pass1 <- pass1$dt[, .(
  Country,
  Competition,
  CompetitionType,
  League,
  Tier,
  Source,
  Date = format(Date, "%Y-%m-%d"),
  Home,
  Away,
  Result,
  Score,
  HomeScore,
  AwayScore,
  HomeFirstAppearance,
  AwayFirstAppearance,
  HomeStartRating,
  AwayStartRating,
  HomeGamesBefore,
  AwayGamesBefore,
  HomeRating_Before,
  AwayRating_Before,
  ExpectedHome,
  ExpectedAway,
  KHome,
  KAway,
  HomeRating_After,
  AwayRating_After,
  HomeGamesAfter,
  AwayGamesAfter
)]
fwrite(game_history_out_pass1, OUTPUT_GAME_HISTORY_CSV_PASS1)

final_pass1 <- copy(pass1$final)
final_pass1[, `:=`(
  Pass = "Pass1_SeedTier",
  EntryMode = "SeedByFirstTier",
  BaseK = K_NORMAL,
  NewK = K_NEW,
  NewGames = K_NEW_GAMES,
  SeedModel = "CountrySpecific_Tier1AndTier2",
  RetroGamesN = RETRO_GAMES_N
)]
fwrite(final_pass1, OUTPUT_FINAL_RATINGS_CSV_PASS1)

retro_start_map <- build_retro_start_map(pass1$dt, n_games = RETRO_GAMES_N)

cat("\nBuilt retro start ratings from Pass 1 using first", RETRO_GAMES_N, "games.\n")
cat("Teams in retro map:", length(retro_start_map), "\n")

# -----------------------------
# Pass 2
# -----------------------------
pass2 <- run_elo(
  dt_input = dt,
  entry_mode = "retro",
  retro_start_map = retro_start_map,
  pass_label = "Pass 2 (retro starts from Pass 1)"
)

game_history_out <- pass2$dt[, .(
  Country,
  Competition,
  CompetitionType,
  League,
  Tier,
  Source,
  Date = format(Date, "%Y-%m-%d"),
  Home,
  Away,
  Result,
  Score,
  HomeScore,
  AwayScore,
  HomeFirstAppearance,
  AwayFirstAppearance,
  HomeStartRating,
  AwayStartRating,
  HomeGamesBefore,
  AwayGamesBefore,
  HomeRating_Before,
  AwayRating_Before,
  ExpectedHome,
  ExpectedAway,
  KHome,
  KAway,
  HomeRating_After,
  AwayRating_After,
  HomeGamesAfter,
  AwayGamesAfter
)]
fwrite(game_history_out, OUTPUT_GAME_HISTORY_CSV)

final_ratings <- copy(pass2$final)
final_ratings[, `:=`(
  Pass = "Pass2_RetroStart",
  EntryMode = "RetroFromPass1FirstN",
  BaseK = K_NORMAL,
  NewK = K_NEW,
  NewGames = K_NEW_GAMES,
  SeedModel = "CountrySpecific_Tier1AndTier2",
  RetroGamesN = RETRO_GAMES_N
)]
fwrite(final_ratings, OUTPUT_FINAL_RATINGS_CSV)

cat("\nDone.\n")
cat("Pass 1 game history:", OUTPUT_GAME_HISTORY_CSV_PASS1, "\n")
cat("Pass 1 final ratings:", OUTPUT_FINAL_RATINGS_CSV_PASS1, "\n")
cat("Pass 2 game history (final):", OUTPUT_GAME_HISTORY_CSV, "\n")
cat("Pass 2 final ratings (final):", OUTPUT_FINAL_RATINGS_CSV, "\n")


beep()