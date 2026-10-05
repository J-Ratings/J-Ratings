stage02_started <- proc.time()[["elapsed"]]

library(data.table)

options(stringsAsFactors = FALSE)

library(beepr)

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

INPUT_CSV <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Matches_Clean_Combined",
  "european_football_all_matches.csv"
)

TEAM_ALIASES_CSV <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Reference",
  "team_aliases.csv"
)

OUT_DIR <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Elo"
)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
CHECKPOINT_DATE <- as.Date("2024-12-31")
CHECKPOINT_FILE <- file.path(OUT_DIR, "checkpoint_2024_12_31.rds")
elo_checkpoint <- if (file.exists(CHECKPOINT_FILE)) readRDS(CHECKPOINT_FILE) else NULL
if (!is.null(elo_checkpoint) && !identical(elo_checkpoint$version, 1L)) stop("Unsupported Elo checkpoint version.")

OUTPUT_GAME_HISTORY_CSV_PASS1  <- file.path(OUT_DIR, "football_elo_game_history_pass1.csv")
OUTPUT_FINAL_RATINGS_CSV_PASS1 <- file.path(OUT_DIR, "football_elo_final_ratings_pass1.csv")

OUTPUT_GAME_HISTORY_CSV  <- file.path(OUT_DIR, "football_elo_game_history.csv")
OUTPUT_FINAL_RATINGS_CSV <- file.path(OUT_DIR, "football_elo_final_ratings.csv")
OUTPUT_UPCOMING_FIXTURES_CSV <- file.path(OUT_DIR, "football_upcoming_fixtures.csv")

# -----------------------------
# Elo settings
# -----------------------------
K_NORMAL <- 20
K_SAME_CONFED <- 40
K_INTERCONFED <- 60
K_NEW <- 20
K_NEW_GAMES <- 100L

# Country/tier starting seeds and confederations are maintained together in one
# reference table. Ratings here are the actual entry ratings: no global offset
# is applied in this script.
COUNTRY_SEEDS_CSV <- file.path(
  repo_dir, "EuropeanFootball", "pipeline_data", "Reference",
  "non_uefa_country_seeds.csv"
)
if (!file.exists(COUNTRY_SEEDS_CSV)) {
  stop("Missing starting-seed file: ", COUNTRY_SEEDS_CSV)
}

COUNTRY_TIER_SEEDS <- fread(COUNTRY_SEEDS_CSV, encoding = "UTF-8")
required_seed_columns <- c("Country", "Tier", "SeedRating", "Confederation")
if (!all(required_seed_columns %in% names(COUNTRY_TIER_SEEDS))) {
  stop(
    "The starting-seed file must contain: ",
    paste(required_seed_columns, collapse = ", "), "."
  )
}

COUNTRY_TIER_SEEDS <- COUNTRY_TIER_SEEDS[, .(
  Country = trimws(as.character(Country)),
  Tier = as.integer(Tier),
  SeedRating = as.numeric(SeedRating),
  Confederation = toupper(trimws(as.character(Confederation)))
)]
if (COUNTRY_TIER_SEEDS[, anyNA(Country) || anyNA(Tier) || anyNA(SeedRating) ||
  anyNA(Confederation) || any(!nzchar(Confederation))]) {
  stop("Starting-seed file has missing country, tier, rating, or confederation values.")
}
if (COUNTRY_TIER_SEEDS[, anyDuplicated(paste(Country, Tier, sep = "\r"))]) {
  stop("Duplicate Country/Tier starting seeds found.")
}

COUNTRY_CONFEDERATIONS <- unique(COUNTRY_TIER_SEEDS[, .(Country, Confederation)])
confed_conflicts <- COUNTRY_CONFEDERATIONS[
  , .(N = uniqueN(Confederation)),
  by = Country
][N > 1L]
if (nrow(confed_conflicts) > 0L) {
  stop(
    "A country has conflicting confederations in the starting-seed file: ",
    paste(confed_conflicts$Country, collapse = ", ")
  )
}
COUNTRY_CONFED_MAP <- setNames(
  COUNTRY_CONFEDERATIONS$Confederation,
  COUNTRY_CONFEDERATIONS$Country
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

source(file.path(dirname(dirname(TEAM_ALIASES_CSV)), "../../scripts/EuropeanFootball/club_identity_resolution.R"))

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
  
  # Confederation files use a regional Country value, so use a source-name
  # alias only when it resolves to one canonical club across all countries.
  is_continental_country <- country0 %in% c(
    "Europe", "Asia", "Africa", "North America", "South America", "Oceania"
  )
  continental_matched <- is_continental_country & x0 %in% names(continental_alias_map)
  out[continental_matched] <- unname(
    continental_alias_map[x0[continental_matched]]
  )
  
  out <- resolve_alias_chain(x0, country0, team_alias_map, continental_alias_map)
  out[is_clearly_bad_team_name(out)] <- NA_character_
  out
}

# -----------------------------
# Load and prepare data
# -----------------------------
input_names <- names(fread(INPUT_CSV,nrows=0L))
elo_input_columns <- intersect(input_names,c("Season","Country","Competition","CompetitionType","Tier","League",
  "Date","Home","Away","Result","Score","Source","HomeAssociation","AwayAssociation"))
dt <- fread(INPUT_CSV,select=elo_input_columns,encoding="UTF-8")
if (any(nchar(dt$Home)>200L | nchar(dt$Away)>200L, na.rm=TRUE)) {
  stop("Oversized team names in the master. Run repair_master_quote_expansion.R before calculating Elo.")
}

required_cols <- c("Country", "Competition", "CompetitionType", "Tier", "League", "Date", "Home", "Away", "Result")
missing_cols <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dt[, Country := trimws(as.character(Country))]
dt[, Competition := trimws(as.character(Competition))]
dt[, CompetitionType := tolower(trimws(as.character(CompetitionType)))]
dt[, Tier := as.integer(Tier)]
dt[, League  := trimws(as.character(League))]
dt[, Source := if ("Source" %in% names(dt)) trimws(as.character(Source)) else NA_character_]
dt[, DateRaw := trimws(as.character(Date))]
# Repair the known older OpenFootball parser's July rollover on load. These
# two regular-stage schedules end before July; July headers belong to the
# opening year, not the ending year. Stage 01 is also corrected at source.
july_year_start <- suppressWarnings(as.integer(substr(as.character(dt$Season),1L,4L)))
july_date_year <- suppressWarnings(as.integer(substr(dt$DateRaw,1L,4L)))
july_repair <- which(dt$Competition %in% c("swiss_super_league","czech_first_league") &
  grepl("openfootball",dt$Source,ignore.case=TRUE) & july_year_start >= 2025L &
  substr(dt$DateRaw,6L,7L)=="07" & july_date_year==july_year_start+1L)
if (length(july_repair)) {
  dt[july_repair,DateRaw:=paste0(july_year_start[july_repair],substr(DateRaw,5L,10L))]
  cat("Corrected OpenFootball July rollover dates on load:",length(july_repair),"\n")
}
dt[, HomeRaw := trimws(as.character(Home))]
dt[, AwayRaw := trimws(as.character(Away))]
dt[, Score   := if ("Score" %in% names(dt)) trimws(as.character(Score)) else NA_character_]
dt[, HomeAssociation := if ("HomeAssociation" %in% names(dt)) trimws(as.character(HomeAssociation)) else NA_character_]
dt[, AwayAssociation := if ("AwayAssociation" %in% names(dt)) trimws(as.character(AwayAssociation)) else NA_character_]

dt[, Home := normalise_team_name(HomeRaw, Country)]
dt[, Away := normalise_team_name(AwayRaw, Country)]
dt[, Result := trimws(as.character(Result))]

bad_name_rows <- dt[
  is.na(Home) | Home == "" |
    is.na(Away) | Away == ""
]

if (nrow(bad_name_rows) > 0) {
  cat("\nDropping rows with bad parsed team names:\n")
  fwrite(bad_name_rows[, .(Country, Competition, League, DateRaw, HomeRaw, AwayRaw, Result, Score)],
    file.path(OUT_DIR, "bad_team_name_rows.csv"))
  cat("  Rows: ", nrow(bad_name_rows), "; full report: bad_team_name_rows.csv\n", sep = "")
  print(head(bad_name_rows[, .(Country, Competition, League, DateRaw, HomeRaw, AwayRaw, Result, Score)], 10L))
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
  dt[CompetitionType == "league", .(Country=club_association(Country,Home), Team = Home, DateRaw)],
  dt[CompetitionType == "league", .(Country=club_association(Country,Away), Team = Away, DateRaw)]
), use.names = TRUE)

cup_domestic_appearances[, Date := as.Date(DateRaw, format = "%Y-%m-%d")]

# The installed data.table DLL overflows its native stack grouping this expanded
# set of normalised names. Use base-R integer groups for the same minimum dates.
first_domestic_dates <- function(appearances) {
  valid <- which(!is.na(appearances$Date))
  keys <- paste(appearances$Country[valid], appearances$Team[valid], sep = "\r")
  unique_keys <- unique(keys)
  if (!length(unique_keys)) return(data.table(Country=character(), Team=character(),
    FirstDomesticDate=as.Date(character())))
  groups <- match(keys, unique_keys)
  minima <- tapply(as.integer(appearances$Date[valid]), groups, min)
  first_rows <- valid[match(unique_keys, keys)]
  data.table(Country=appearances$Country[first_rows], Team=appearances$Team[first_rows],
    FirstDomesticDate=as.Date(as.numeric(minima[as.character(seq_along(unique_keys))]), origin="1970-01-01"))
}
cat("Computing first domestic dates for cup eligibility...\n")
cup_first_domestic <- first_domestic_dates(cup_domestic_appearances)

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
  dt[CompetitionType == "league", .(Country=club_association(Country,Home), Team = Home, DateRaw)],
  dt[CompetitionType == "league", .(Country=club_association(Country,Away), Team = Away, DateRaw)]
), use.names = TRUE)

domestic_appearances[, Date := as.Date(DateRaw, format = "%Y-%m-%d")]

cat("Computing first domestic dates for continental eligibility...\n")
first_domestic_date <- first_domestic_dates(domestic_appearances[!is.na(Team) & Team != ""])

first_domestic_key <- paste(first_domestic_date$Country, first_domestic_date$Team, sep = "\r")
first_domestic_map <- setNames(first_domestic_date$FirstDomesticDate, first_domestic_key)
# A continental club name is usable only when it maps to one domestic country.
# This deliberately excludes ambiguous labels (e.g. a club named Inter in
# multiple countries) instead of allowing them to share a rating.
club_country_candidates <- unique(first_domestic_date[, .(Country, Team)])[, .(Countries = uniqueN(Country), ClubCountry = Country[1L]), by = Team]
club_country_map <- setNames(club_country_candidates[Countries == 1L]$ClubCountry, club_country_candidates[Countries == 1L]$Team)

continental_rows <- dt[CompetitionType == "continental"]

if (nrow(continental_rows) > 0) {
  continental_rows[, MatchDate := as.Date(DateRaw, format = "%Y-%m-%d")]
  # Cross-confederation fixtures can include names shared by clubs in several
  # countries (River Plate is one example). The audited source supplies the
  # participant's association explicitly, which takes precedence over the
  # otherwise conservative unique-name inference.
  continental_rows[, `:=`(
    HomeClubCountry = fifelse(!is.na(HomeAssociation) & nzchar(HomeAssociation),
                              HomeAssociation, unname(club_country_map[Home])),
    AwayClubCountry = fifelse(!is.na(AwayAssociation) & nzchar(AwayAssociation),
                              AwayAssociation, unname(club_country_map[Away]))
  )]
  continental_rows[, `:=`(HomeClubCountry=club_association(HomeClubCountry,Home),
                          AwayClubCountry=club_association(AwayClubCountry,Away))]
  continental_rows[, HomeFirstDomestic := as.Date(first_domestic_map[paste(HomeClubCountry, Home, sep = "\r")], origin = "1970-01-01")]
  continental_rows[, AwayFirstDomestic := as.Date(first_domestic_map[paste(AwayClubCountry, Away, sep = "\r")], origin = "1970-01-01")]
  
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

# Country-qualified keys are the Elo identity. Names remain separate display
# labels, so Everton (England) and Everton (Chile) can never be merged.
dt[CompetitionType != "continental", `:=`(
  HomeClubCountry = club_association(Country,Home),
  AwayClubCountry = club_association(Country,Away))]

# Resolve the confederation of each club from its domestic association.
# This lets K depend on the information bridge made by the fixture:
#   same country                         -> K_NORMAL (20)
#   different countries, same confed    -> K_SAME_CONFED (40)
#   different confederations            -> K_INTERCONFED (60)
dt[, `:=`(
  HomeConfederation = unname(COUNTRY_CONFED_MAP[HomeClubCountry]),
  AwayConfederation = unname(COUNTRY_CONFED_MAP[AwayClubCountry])
)]

missing_confed_rows <- dt[
  is.na(HomeConfederation) | HomeConfederation == "" |
    is.na(AwayConfederation) | AwayConfederation == ""
]
if (nrow(missing_confed_rows) > 0L) {
  stop(
    "Missing confederation mapping for one or more club associations.\n",
    paste(
      unique(c(
        missing_confed_rows[is.na(HomeConfederation) | HomeConfederation == "", HomeClubCountry],
        missing_confed_rows[is.na(AwayConfederation) | AwayConfederation == "", AwayClubCountry]
      )),
      collapse = ", "
    )
  )
}

dt[, KClass := fifelse(
  CompetitionType == "league" | HomeClubCountry == AwayClubCountry,
  "same_country",
  fifelse(
    HomeConfederation == AwayConfederation,
    "same_confederation",
    "inter_confederation"
  )
)]

dt[, `:=`(HomeKey = paste(HomeClubCountry, Home, sep = "\r"), AwayKey = paste(AwayClubCountry, Away, sep = "\r"))]

dt[, Date := as.Date(DateRaw, format = "%Y-%m-%d")]
identity_fixture_cols <- c("Country","Competition","CompetitionType","Date",
                          "HomeKey","AwayKey","Score","Result")
identity_duplicates <- duplicated(dt,by=identity_fixture_cols)
if(any(identity_duplicates)) {
  fwrite(dt[identity_duplicates],file.path(OUT_DIR,"canonical_duplicate_fixtures.csv"))
  cat("Repeated identical fixtures after identity resolution:",sum(identity_duplicates),
      "; counted once; report: canonical_duplicate_fixtures.csv\n")
  dt <- dt[!identity_duplicates]
}
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
  KClass,
  HomeClubCountry,
  AwayClubCountry,
  HomeConfederation,
  AwayConfederation,
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

# Use Score where available; fall back to Result for older source formats.
dt[, EloResult := fifelse(
  !is.na(Score) & trimws(Score) != "",
  trimws(Score),
  trimws(Result)
)]

scores <- t(vapply(dt$EloResult, result_to_scores, numeric(2)))

dt[, HomeScore := scores[, 1]]
dt[, AwayScore := scores[, 2]]

bad_result_rows <- dt[
  is.na(HomeScore) | is.na(AwayScore)
]

if (nrow(bad_result_rows) > 0L) {
  cat(
    "\nRows dropped because neither Score nor Result could be parsed:",
    nrow(bad_result_rows),
    "\n"
  )
}

dt <- dt[
  !is.na(HomeScore) &
    !is.na(AwayScore)
]

cat(
  "\nCompleted matches entering Elo after score parsing:",
  nrow(dt),
  "\n"
)
setorder(dt, Date, Country, Tier, Competition, League, Home, Away, Result)
future_completed <- dt[Date > Sys.Date()]
if (nrow(future_completed)) {
  fwrite(future_completed, file.path(OUT_DIR,"future_dated_completed_matches.csv"))
  cat("Excluded future-dated completed results:",nrow(future_completed),"; see future_dated_completed_matches.csv\n")
  dt <- dt[Date <= Sys.Date()]
}
early_cwc_count <- nrow(dt[Competition == "fifa_club_world_cup" &
  Date >= as.Date("2025-01-01") & Date < as.Date("2026-01-01")])
if (early_cwc_count != 63L) stop("Preflight failed: 2025 Club World Cup has ",
  early_cwc_count, " / 63 eligible games. No Elo passes were run. Check participant associations and domestic identity coverage.")

# -----------------------------
# Generic Elo runner
# -----------------------------
run_elo <- function(dt_input,
                    entry_mode = c("seed", "retro"),
                    retro_start_map = NULL,
                    pass_label = "pass",
                    initial_state = NULL,
                    frozen_history = NULL) {
  
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
  state_envs <- list(ratings=ratings_env, games=games_env, first_league=first_league_env,
                    first_tier=first_tier_env, first_date=first_date_env, entry_rating=entry_rating_env)
  if (!is.null(initial_state)) {
    for (nm in names(state_envs)) list2env(initial_state[[nm]], envir=state_envs[[nm]])
  }
  snapshot_state <- function() lapply(state_envs, as.list, all.names=TRUE)
  boundary_state <- initial_state
  
  HomeVec <- dt_input$HomeKey
  AwayVec <- dt_input$AwayKey
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
    if (is.null(boundary_state) && dt_input$Date[i] > CHECKPOINT_DATE) boundary_state <- snapshot_state()
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
    
    # K-factor reflects how much new cross-ecosystem information the match
    # provides, not merely the competition label. Domestic/same-country ties
    # use normal K, cross-country ties within one confederation use double K,
    # and inter-confederation ties use triple K.
    k_class_i <- dt_input$KClass[i]
    
    if (k_class_i == "inter_confederation") {
      Kh <- K_INTERCONFED
      Ka <- K_INTERCONFED
    } else if (k_class_i == "same_confederation") {
      Kh <- K_SAME_CONFED
      Ka <- K_SAME_CONFED
    } else {
      Kh <- if (Gh < K_NEW_GAMES) K_NEW else K_NORMAL
      Ka <- if (Ga < K_NEW_GAMES) K_NEW else K_NORMAL
    }
    
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
  if (is.null(boundary_state)) boundary_state <- snapshot_state()
  
  final_ratings <- data.table(
    TeamKey = teams,
    Team = sub("^.*\\r", "", teams),
    Country = sub("\\r.*$", "", teams),
    Rating = as.numeric(mget(teams, envir = ratings_env)),
    Games = as.integer(unlist(mget(teams, envir = games_env))),
    FirstLeague = as.character(unlist(mget(teams, envir = first_league_env))),
    FirstTier = as.integer(unlist(mget(teams, envir = first_tier_env))),
    FirstMatchDate = as.Date(as.numeric(unlist(mget(teams, envir = first_date_env))), origin = "1970-01-01"),
    EntryRating = as.numeric(unlist(mget(teams, envir = entry_rating_env)))
  )
  
  setorder(final_ratings, -Rating, Country, Team)
  final_ratings[, Rating := round(Rating, 0)]
  final_ratings[, EntryRating := round(EntryRating, 1)]
  final_ratings[, FirstMatchDate := format(FirstMatchDate, "%Y-%m-%d")]
  final_ratings[, IsSeed := Games >= 20]
  
  list(
    dt = if (is.null(frozen_history)) dt_out else rbindlist(list(frozen_history, dt_out), use.names=TRUE, fill=TRUE),
    final = final_ratings,
    boundary_state = boundary_state
  )
}

build_retro_start_map <- function(pass1_dt, n_games = 100L, frozen_teams = character()) {
  if (length(frozen_teams)) pass1_dt <- pass1_dt[!HomeKey %chin% frozen_teams | !AwayKey %chin% frozen_teams]
  team_games <- rbindlist(list(
    pass1_dt[, .(
      Team = HomeKey,
      GamesAfter = HomeGamesAfter,
      RatingAfter = HomeRating_After,
      Date = Date
    )],
    pass1_dt[, .(
      Team = AwayKey,
      GamesAfter = AwayGamesAfter,
      RatingAfter = AwayRating_After,
      Date = Date
    )]
  ), use.names = TRUE)
  
  if (length(frozen_teams)) team_games <- team_games[!Team %chin% frozen_teams]
  if (!nrow(team_games)) return(list())
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

cat(
  "\nUEFA 1st Division matches entering Elo:",
  nrow(dt[Competition == "UEFA 1st Division"]),
  "\n"
)


if (!is.null(elo_checkpoint)) {
  cat("\nUsing frozen checkpoint through 2024-12-31. Only later games are recalculated.\n")
  historical_input <- dt[Date <= CHECKPOINT_DATE]
  if (!is.null(elo_checkpoint$historical_input) && !identical(historical_input, elo_checkpoint$historical_input)) {
    warning("Pre-2025 input differs from the frozen checkpoint; historical changes are ignored. Restore the archived 02 script for a deliberate full rebuild.")
  }
  dt <- dt[Date > CHECKPOINT_DATE]
}
pass1 <- run_elo(
  dt_input = dt,
  entry_mode = "seed",
  retro_start_map = NULL,
  pass_label = "Pass 1 (seed by first tier)",
  initial_state = if (!is.null(elo_checkpoint)) elo_checkpoint$pass1_state else NULL,
  frozen_history = if (!is.null(elo_checkpoint)) elo_checkpoint$pass1_history else NULL
)

game_history_out_pass1 <- pass1$dt[, .(
  Country,
  Competition,
  CompetitionType,
  KClass,
  HomeClubCountry,
  AwayClubCountry,
  HomeConfederation,
  AwayConfederation,
  League,
  Tier,
  Source,
  Date = format(Date, "%Y-%m-%d"),
  Home,
  Away,
  HomeKey,
  AwayKey,
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
  SameConfedK = K_SAME_CONFED,
  InterconfedK = K_INTERCONFED,
  NewK = K_NEW,
  NewGames = K_NEW_GAMES,
  SeedModel = "CountrySpecific_Tier1AndTier2",
  RetroGamesN = RETRO_GAMES_N
)]
fwrite(final_pass1, OUTPUT_FINAL_RATINGS_CSV_PASS1)

retro_start_map <- build_retro_start_map(pass1$dt, n_games = RETRO_GAMES_N,
  frozen_teams = if (!is.null(elo_checkpoint)) names(elo_checkpoint$frozen_retro_map) else character())
if (!is.null(elo_checkpoint)) retro_start_map[names(elo_checkpoint$frozen_retro_map)] <- elo_checkpoint$frozen_retro_map

cat("\nBuilt retro start ratings from Pass 1 using first", RETRO_GAMES_N, "games.\n")
cat("Teams in retro map:", length(retro_start_map), "\n")

# -----------------------------
# Pass 2
# -----------------------------
pass2 <- run_elo(
  dt_input = dt,
  entry_mode = "retro",
  retro_start_map = retro_start_map,
  pass_label = "Pass 2 (retro starts from Pass 1)",
  initial_state = if (!is.null(elo_checkpoint)) elo_checkpoint$pass2_state else NULL,
  frozen_history = if (!is.null(elo_checkpoint)) elo_checkpoint$pass2_history else NULL
)

game_history_out <- pass2$dt[, .(
  Country,
  Competition,
  CompetitionType,
  KClass,
  HomeClubCountry,
  AwayClubCountry,
  HomeConfederation,
  AwayConfederation,
  League,
  Tier,
  Source,
  Date = format(Date, "%Y-%m-%d"),
  Home,
  Away,
  HomeKey,
  AwayKey,
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
  SameConfedK = K_SAME_CONFED,
  InterconfedK = K_INTERCONFED,
  NewK = K_NEW,
  NewGames = K_NEW_GAMES,
  SeedModel = "CountrySpecific_Tier1AndTier2",
  RetroGamesN = RETRO_GAMES_N
)]
fwrite(final_ratings, OUTPUT_FINAL_RATINGS_CSV)

# Catch missing intercontinental links before the much slower JSON export.
cwc_2025 <- game_history_out[
  Competition == "fifa_club_world_cup" & Date >= "2025-01-01" & Date < "2026-01-01"
]
cat("\n2025 FIFA Club World Cup games included in Elo:", nrow(cwc_2025), "/ 63\n")
if (nrow(cwc_2025) != 63L) {
  stop("Club World Cup coverage failed: expected all 63 games from 2025. Do not run 03_write_json.R yet.")
}
if (is.null(elo_checkpoint)) {
  checkpoint <- list(version=1L, cutoff=CHECKPOINT_DATE, created=Sys.time(),
    historical_input=dt[Date <= CHECKPOINT_DATE],
    pass1_state=pass1$boundary_state, pass2_state=pass2$boundary_state,
    pass1_history=pass1$dt[Date <= CHECKPOINT_DATE],
    pass2_history=pass2$dt[Date <= CHECKPOINT_DATE],
    frozen_retro_map=retro_start_map[names(pass2$boundary_state$ratings)])
  checkpoint_tmp <- paste0(CHECKPOINT_FILE, ".tmp")
  saveRDS(checkpoint, checkpoint_tmp, compress=FALSE)
  if (!file.rename(checkpoint_tmp, CHECKPOINT_FILE)) stop("Could not save checkpoint.")
  cat("Created permanent end-of-2024 checkpoint:", CHECKPOINT_FILE, "\n")
}

club_rankings <- copy(final_ratings)[order(-Rating)]
club_rankings[, Rank := .I]
for (nation in c("Brazil", "Argentina")) {
  top_club <- club_rankings[Country == nation][1L]
  if (nrow(top_club) == 0L) stop("No rated clubs found for ", nation)
  cat(nation, "top club:", top_club$Team, "rating", round(top_club$Rating),
      "rank", top_club$Rank, "\n")
}

cat("\nDone.\n")
cat("Pass 1 game history:", OUTPUT_GAME_HISTORY_CSV_PASS1, "\n")
cat("Pass 1 final ratings:", OUTPUT_FINAL_RATINGS_CSV_PASS1, "\n")
cat("Pass 2 game history (final):", OUTPUT_GAME_HISTORY_CSV, "\n")
cat("Pass 2 final ratings (final):", OUTPUT_FINAL_RATINGS_CSV, "\n")

if (interactive()) beep()

stage02_seconds <- proc.time()[["elapsed"]] - stage02_started
cat("02 total elapsed:", round(stage02_seconds, 1), "seconds\n")
writeLines(paste(format(Sys.time()), "elapsed_seconds", stage02_seconds), file.path(OUT_DIR, "stage_02_latest_timing.txt"))
