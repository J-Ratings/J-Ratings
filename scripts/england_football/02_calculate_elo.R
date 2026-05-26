
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

# -----------------------------
# Elo settings
# -----------------------------
K_NORMAL <- 20
K_NEW <- 20
K_NEW_GAMES <- 100L

SEED_TIER_1 <- 2800
SEED_TIER_2 <- 2600
SEED_TIER_3 <- 2400
SEED_TIER_4 <- 2200
SEED_TIER_5 <- 2000

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

league_to_tier <- function(x) {
  lx <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(lx))
  
  out[grepl("^premier league$", lx)] <- 1L
  out[is.na(out) & grepl("^division\\s*1$|^championship$", lx)] <- 2L
  out[is.na(out) & grepl("^division\\s*2$|^league\\s*1$|^league one$", lx)] <- 3L
  out[is.na(out) & grepl("^division\\s*3$|^league\\s*2$|^league two$", lx)] <- 4L
  out[is.na(out) & grepl("^national league$", lx)] <- 5L
  
  out
}

seed_from_tier <- function(tier) {
  out <- rep(NA_real_, length(tier))
  out[tier == 1L] <- SEED_TIER_1
  out[tier == 2L] <- SEED_TIER_2
  out[tier == 3L] <- SEED_TIER_3
  out[tier == 4L] <- SEED_TIER_4
  out[tier == 5L] <- SEED_TIER_5
  out
}

canonical_teams <- c(
  "Liverpool","Manchester City","Chelsea","Manchester United","Arsenal",
  "Tottenham Hotspur","Aston Villa","Blackburn Rovers","Leeds United",
  "Newcastle United","Everton","Leicester City","Brighton & Hove Albion",
  "Crystal Palace","West Ham United","Sunderland","Bournemouth","Southampton",
  "Brentford","Bolton Wanderers","Nottingham Forest","Wolverhampton Wanderers",
  "Norwich City","Portsmouth","Fulham","Middlesbrough","Ipswich Town",
  "Charlton Athletic","Wimbledon","Stoke City","Burnley","West Bromwich Albion",
  "Sheffield Wednesday","Queens Park Rangers","Derby County","Swansea City",
  "Birmingham City","Sheffield United","Reading","Coventry City","Wigan Athletic",
  "Watford","Bradford City","Oldham Athletic","Cardiff City","Hull City",
  "Preston North End","Barnsley","Luton Town","Blackpool","Huddersfield Town",
  "Millwall","Bristol City","Swindon Town","Stockport County","Gillingham",
  "Grimsby Town","Tranmere Rovers","Plymouth Argyle","Doncaster Rovers",
  "Crewe Alexandra","Peterborough United","Colchester United","Rotherham United",
  "Wrexham","Walsall","Port Vale","Oxford United","Burton Albion",
  "Milton Keynes Dons","Wycombe Wanderers","Leyton Orient","Bristol Rovers",
  "Fleetwood Town","Carlisle United","Shrewsbury Town","Lincoln City",
  "Stevenage","Notts County","Northampton Town","Crawley Town","Chesterfield",
  "Accrington Stanley","Exeter City","Mansfield Town","Cheltenham Town",
  "Cambridge United","Morecambe","Salford City","Newport County","Barrow",
  "Bromley","Barnet","Harrogate Town"
)

team_alias_map <- c(
  "Arsenal FC" = "Arsenal",
  "Arsenal.FC" = "Arsenal",
  "Chelsea FC" = "Chelsea",
  "Coventry City FC" = "Coventry City",
  "Crystal Palace FC" = "Crystal Palace",
  "Everton FC" = "Everton",
  "Ipswich Town FC" = "Ipswich Town",
  "Leeds United FC" = "Leeds United",
  "Sheffield United FC" = "Sheffield United",
  "Sheffield Utd" = "Sheffield United",
  "Southampton FC" = "Southampton",
  "Nottingham Forest FC" = "Nottingham Forest",
  "Manchester City FC" = "Manchester City",
  "Manchester City" = "Manchester City",
  "Blackburn Rovers FC" = "Blackburn Rovers",
  "Blackburn Rovers" = "Blackburn Rovers",
  "Blackburn" = "Blackburn Rovers",
  "Wimbledon FC" = "Wimbledon",
  "Aston Villa FC" = "Aston Villa",
  "Liverpool FC" = "Liverpool",
  "Manchester United FC" = "Manchester United",
  "Manchester United" = "Manchester United",
  "Middlesbrough FC" = "Middlesbrough",
  "Norwich City FC" = "Norwich City",
  "Norwich" = "Norwich City",
  "Oldham Athletic AFC" = "Oldham Athletic",
  "Oldham Athletic" = "Oldham Athletic",
  "Oldham" = "Oldham Athletic",
  "Queens Park Rangers FC" = "Queens Park Rangers",
  "Queens Park Rangers" = "Queens Park Rangers",
  "QPR" = "Queens Park Rangers",
  "Sheffield Wednesday FC" = "Sheffield Wednesday",
  "Sheffield Wednesday" = "Sheffield Wednesday",
  "Sheffield Wed" = "Sheffield Wednesday",
  "Tottenham Hotspur FC" = "Tottenham Hotspur",
  "Tottenham Hotspur" = "Tottenham Hotspur",
  "Tottenham" = "Tottenham Hotspur",
  "Newcastle United FC" = "Newcastle United",
  "Newcastle United" = "Newcastle United",
  "Newcastle Utd" = "Newcastle United",
  "West Ham United FC" = "West Ham United",
  "West Ham United" = "West Ham United",
  "West Ham" = "West Ham United",
  "Swindon Town FC" = "Swindon Town",
  "Swindon Town" = "Swindon Town",
  "Swindon" = "Swindon Town",
  "Leicester City FC" = "Leicester City",
  "Leicester City" = "Leicester City",
  "Leicester" = "Leicester City",
  "Bolton Wanderers FC" = "Bolton Wanderers",
  "Bolton Wanderers" = "Bolton Wanderers",
  "Bolton" = "Bolton Wanderers",
  "Derby County FC" = "Derby County",
  "Derby County" = "Derby County",
  "Derby" = "Derby County",
  "Sunderland AFC" = "Sunderland",
  "Sunderland" = "Sunderland",
  "Barnsley FC" = "Barnsley",
  "Barnsley" = "Barnsley",
  "Bradford" = "Bradford City",
  "Bradford City" = "Bradford City",
  "Crystal Palace" = "Crystal Palace",
  "Port Vale FC" = "Port Vale",
  "Port Vale" = "Port Vale",
  "Portsmouth FC" = "Portsmouth",
  "Portsmouth" = "Portsmouth",
  "Wolves" = "Wolverhampton Wanderers",
  "Wolverhampton Wanderers" = "Wolverhampton Wanderers",
  "Wolverhampton Wanderers FC" = "Wolverhampton Wanderers",
  "Bournemouth" = "Bournemouth",
  "AFC Bournemouth" = "Bournemouth",
  "Burnley FC" = "Burnley",
  "Burnley" = "Burnley",
  "Colchester" = "Colchester United",
  "Colchester United" = "Colchester United",
  "Gillingham FC" = "Gillingham",
  "Gillingham" = "Gillingham",
  "Macclesfield" = "Macclesfield Town",
  "Macclesfield Town" = "Macclesfield Town",
  "Northampton" = "Northampton Town",
  "Northampton Town" = "Northampton Town",
  "Preston" = "Preston North End",
  "Preston North End" = "Preston North End",
  "Preston North End FC" = "Preston North End",
  "Wigan" = "Wigan Athletic",
  "Wigan Athletic" = "Wigan Athletic",
  "Wigan Athletic FC" = "Wigan Athletic",
  "Wrexham" = "Wrexham",
  "Wrexham AFC" = "Wrexham",
  "Wrexham FC" = "Wrexham",
  "Wycombe" = "Wycombe Wanderers",
  "Wycombe Wanderers" = "Wycombe Wanderers",
  "Wycombe Wanderers FC" = "Wycombe Wanderers",
  "Brentford" = "Brentford",
  "Brentford FC" = "Brentford",
  "Carlisle" = "Carlisle United",
  "Carlisle United" = "Carlisle United",
  "Chester" = "Chester City",
  "Darlington" = "Darlington",
  "Hartlepool" = "Hartlepool United",
  "Hartlepool United" = "Hartlepool United",
  "Peterborough" = "Peterborough United",
  "Peterborough United" = "Peterborough United",
  "Peterborough United FC" = "Peterborough United",
  "Plymouth" = "Plymouth Argyle",
  "Plymouth Argyle" = "Plymouth Argyle",
  "Plymouth Argyle FC" = "Plymouth Argyle",
  "Rotherham" = "Rotherham United",
  "Rotherham United" = "Rotherham United",
  "Rotherham United FC" = "Rotherham United",
  "Scarborough FC" = "Scarborough",
  "Shrewsbury" = "Shrewsbury Town",
  "Shrewsbury Town" = "Shrewsbury Town",
  "Swansea" = "Swansea City",
  "Swansea City" = "Swansea City",
  "Swansea City AFC" = "Swansea City",
  "Torquay" = "Torquay United",
  "Torquay United" = "Torquay United",
  "Grimsby" = "Grimsby Town",
  "Grimsby Town" = "Grimsby Town",
  "Fulham FC" = "Fulham",
  "Fulham" = "Fulham",
  "Crewe" = "Crewe Alexandra",
  "Crewe Alexandra" = "Crewe Alexandra",
  "Huddersfield" = "Huddersfield Town",
  "Huddersfield Town" = "Huddersfield Town",
  "Huddersfield Town AFC" = "Huddersfield Town",
  "Ipswich" = "Ipswich Town",
  "Oxford Utd" = "Oxford United",
  "Oxford United" = "Oxford United",
  "Oxford United FC" = "Oxford United",
  "Watford FC" = "Watford",
  "Watford" = "Watford",
  "West Brom" = "West Bromwich Albion",
  "West Bromwich Albion" = "West Bromwich Albion",
  "West Bromwich Albion FC" = "West Bromwich Albion",
  "Blackpool FC" = "Blackpool",
  "Blackpool" = "Blackpool",
  "Bristol Rovers" = "Bristol Rovers",
  "Chesterfield FC" = "Chesterfield",
  "Chesterfield" = "Chesterfield",
  "Lincoln City" = "Lincoln City",
  "Luton" = "Luton Town",
  "Luton Town" = "Luton Town",
  "Luton Town FC" = "Luton Town",
  "Millwall FC" = "Millwall",
  "Millwall" = "Millwall",
  "Walsall FC" = "Walsall",
  "Walsall" = "Walsall",
  "York" = "York City",
  "York City" = "York City",
  "Barnet FC" = "Barnet",
  "Barnet" = "Barnet",
  "Brighton" = "Brighton & Hove Albion",
  "Brighton & Hove Albion" = "Brighton & Hove Albion",
  "Brighton & Hove Albion FC" = "Brighton & Hove Albion",
  "Cambridge Utd" = "Cambridge United",
  "Cambridge United" = "Cambridge United",
  "Cardiff" = "Cardiff City",
  "Cardiff City" = "Cardiff City",
  "Cardiff City FC" = "Cardiff City",
  "Exeter" = "Exeter City",
  "Exeter City" = "Exeter City",
  "Halifax" = "Halifax Town",
  "Halifax Town" = "Halifax Town",
  "Hull City" = "Hull City",
  "Hull City AFC" = "Hull City",
  "Leyton Orient" = "Leyton Orient",
  "Mansfield" = "Mansfield Town",
  "Mansfield Town" = "Mansfield Town",
  "Rochdale" = "Rochdale",
  "Rochdale AFC" = "Rochdale",
  "Scunthorpe" = "Scunthorpe United",
  "Scunthorpe United" = "Scunthorpe United",
  "Southend" = "Southend United",
  "Southend United" = "Southend United",
  "Birmingham" = "Birmingham City",
  "Birmingham City" = "Birmingham City",
  "Birmingham City FC" = "Birmingham City",
  "Reading FC" = "Reading",
  "Reading" = "Reading",
  "Charlton Athletic FC" = "Charlton Athletic",
  "Charlton Athletic" = "Charlton Athletic",
  "Charlton" = "Charlton Athletic",
  "Chelsea" = "Chelsea",
  "Coventry" = "Coventry City",
  "Leeds" = "Leeds United",
  "Everton" = "Everton",
  "Aston Villa" = "Aston Villa",
  "Southampton" = "Southampton",
  "Nottingham Forest" = "Nottingham Forest",
  "Liverpool" = "Liverpool",
  "Grimsby Town FC" = "Grimsby Town",
  "Cambridge United FC" = "Cambridge United",
  "Burnley" = "Burnley",
  "Doncaster Rovers" = "Doncaster Rovers",
  "Yeovil Town" = "Yeovil Town",
  "Milton Keynes Dons" = "Milton Keynes Dons",
  "Chester City" = "Chester City",
  "Accrington Stanley" = "Accrington Stanley",
  "Hereford United" = "Hereford United",
  "Morecambe FC" = "Morecambe",
  "Morecambe" = "Morecambe",
  "Dagenham & Redbridge" = "Dagenham & Redbridge",
  "Aldershot Town" = "Aldershot Town",
  "Burton Albion" = "Burton Albion",
  "Stevenage FC" = "Stevenage",
  "Stevenage" = "Stevenage",
  "AFC Wimbledon" = "Wimbledon",
  "Crawley Town" = "Crawley Town",
  "Fleetwood Town" = "Fleetwood Town",
  "Newport County" = "Newport County",
  "Forest Green Rovers" = "Forest Green Rovers",
  "Salford City" = "Salford City",
  "Bristol City FC" = "Bristol City",
  "Harrogate Town" = "Harrogate Town",
  "Stoke City FC" = "Stoke City",
  "Stoke City" = "Stoke City",
  "Barrow AFC" = "Barrow",
  "Barrow" = "Barrow",
  "Sutton United" = "Sutton United",
  "Bromley FC" = "Bromley",
  "Bromley" = "Bromley",
  "Newport County AFC" = "Newport County"
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

normalise_team_name <- function(x) {
  x0 <- trimws(as.character(x))
  x0[is_clearly_bad_team_name(x0)] <- NA_character_
  x0 <- gsub("\\s*\\[.*?\\]\\s*$", "", x0, perl = TRUE)
  x0 <- gsub("\\s+(FC|AFC)$", "", x0, ignore.case = TRUE)
  x0 <- gsub("^AFC\\s+", "", x0, ignore.case = TRUE)
  x0 <- gsub("\\s+", " ", x0)
  x0 <- trimws(x0)
  
  out <- ifelse(x0 %in% names(team_alias_map), unname(team_alias_map[x0]), x0)
  out[is_clearly_bad_team_name(out)] <- NA_character_
  out
}

# -----------------------------
# Load and prepare data
# -----------------------------
dt <- fread(INPUT_CSV)

required_cols <- c("League", "Date", "Home", "Away", "Result")
missing_cols <- setdiff(required_cols, names(dt))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

dt[, League  := trimws(as.character(League))]
dt[, DateRaw := trimws(as.character(Date))]
dt[, HomeRaw := trimws(as.character(Home))]
dt[, AwayRaw := trimws(as.character(Away))]
dt[, Score   := if ("Score" %in% names(dt)) trimws(as.character(Score)) else NA_character_]

dt[, Home := normalise_team_name(HomeRaw)]
dt[, Away := normalise_team_name(AwayRaw)]
dt[, Result := trimws(as.character(Result))]

bad_name_rows <- dt[
  is.na(Home) | Home == "" |
    is.na(Away) | Away == ""
]

if (nrow(bad_name_rows) > 0) {
  cat("\nDropping rows with bad parsed team names:\n")
  print(bad_name_rows[, .(League, DateRaw, HomeRaw, AwayRaw, Result, Score)])
}

dt <- dt[
  !(is.na(Home) | Home == "" |
      is.na(Away) | Away == "")
]

dt[, Date := as.Date(DateRaw, format = "%Y-%m-%d")]
dt[, Tier := league_to_tier(League)]
dt[, SeedRatingForTier := seed_from_tier(Tier)]

bad_leagues <- unique(dt[is.na(Tier), League])
if (length(bad_leagues) > 0) {
  stop(
    "Unrecognised league name(s) in League column:\n",
    paste0(" - ", bad_leagues, collapse = "\n")
  )
}

dt <- dt[
  !is.na(Date) &
    !is.na(Home) & Home != "" &
    !is.na(Away) & Away != "" &
    Result != ""
]

scores <- t(vapply(dt$Result, result_to_scores, numeric(2)))
dt[, HomeScore := scores[, 1]]
dt[, AwayScore := scores[, 2]]
dt <- dt[!is.na(HomeScore) & !is.na(AwayScore)]

setorder(dt, Date, Tier, League, Home, Away, Result)

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
    
    Kh <- if (Gh < K_NEW_GAMES) K_NEW else K_NORMAL
    Ka <- if (Ga < K_NEW_GAMES) K_NEW else K_NORMAL
    
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
  League,
  Tier,
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
  SeedTier1 = SEED_TIER_1,
  SeedTier2 = SEED_TIER_2,
  SeedTier3 = SEED_TIER_3,
  SeedTier4 = SEED_TIER_4,
  SeedTier5 = SEED_TIER_5,
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
  League,
  Tier,
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
  SeedTier1 = SEED_TIER_1,
  SeedTier2 = SEED_TIER_2,
  SeedTier3 = SEED_TIER_3,
  SeedTier4 = SEED_TIER_4,
  SeedTier5 = SEED_TIER_5,
  RetroGamesN = RETRO_GAMES_N
)]
fwrite(final_ratings, OUTPUT_FINAL_RATINGS_CSV)

cat("\nDone.\n")
cat("Pass 1 game history:", OUTPUT_GAME_HISTORY_CSV_PASS1, "\n")
cat("Pass 1 final ratings:", OUTPUT_FINAL_RATINGS_CSV_PASS1, "\n")
cat("Pass 2 game history (final):", OUTPUT_GAME_HISTORY_CSV, "\n")
cat("Pass 2 final ratings (final):", OUTPUT_FINAL_RATINGS_CSV, "\n")

