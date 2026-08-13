library(data.table)

# ============================================================
# J-RATINGS CALIBRATION DIAGNOSTIC
#
# This script checks:
#
# 1. UEFA country strength differences
# 2. Home advantage in UEFA matches
# 3. Current top-division depth
# 4. Top-6 vs full-league strength
# 5. Suggested relative Tier 1 country seeds
# 6. Historical Elo gaps between adjacent domestic divisions
#
# It does NOT change the Elo model.
# It only prints diagnostics.
# ============================================================


# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

repo_dir <- normalizePath(
  Sys.getenv(
    "J_RATINGS_REPO",
    "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"
  ),
  winslash = "/",
  mustWork = FALSE
)

MATCH_FILE <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Matches_Clean_Combined",
  "england_leagues_1_to_5_all_seasons.csv"
)

ELO_DIR <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Elo"
)

RATINGS_FILE <- file.path(
  ELO_DIR,
  "football_elo_final_ratings.csv"
)

HISTORY_FILE <- file.path(
  ELO_DIR,
  "football_elo_game_history.csv"
)

PASS1_HISTORY_FILE <- file.path(
  ELO_DIR,
  "football_elo_game_history_pass1.csv"
)

AUDIT_DIR <- file.path(
  repo_dir, "EnglishFootball", "pipeline_data", "Audit", "elo_calibration"
)
dir.create(AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# 2. Settings
# ------------------------------------------------------------

BASE_ELO <- 2750
TOP_N <- 6L


# ------------------------------------------------------------
# 3. Load data
# ------------------------------------------------------------

dt <- fread(MATCH_FILE)

final_ratings <- fread(
  RATINGS_FILE
)

game_history <- fread(
  HISTORY_FILE
)

pass1_history <- if (file.exists(PASS1_HISTORY_FILE)) {
  fread(PASS1_HISTORY_FILE)
} else {
  NULL
}


# ------------------------------------------------------------
# 4. Build team -> domestic country lookup
# ------------------------------------------------------------

home_domestic <- dt[
  CompetitionType == "league",
  .(
    Team = Home,
    TeamCountry = Country
  )
]

away_domestic <- dt[
  CompetitionType == "league",
  .(
    Team = Away,
    TeamCountry = Country
  )
]

domestic <- rbind(
  home_domestic,
  away_domestic
)

team_country_dt <- domestic[
  !is.na(Team) &
    Team != "" &
    !is.na(TeamCountry) &
    TeamCountry != "",
  .N,
  by = .(
    Team,
    TeamCountry
  )
][
  order(
    Team,
    -N
  )
][
  ,
  .SD[1],
  by = Team
]

team_country <- setNames(
  team_country_dt$TeamCountry,
  team_country_dt$Team
)

domestic_teams_mapped <- length(
  team_country
)


# ------------------------------------------------------------
# 5. Completed UEFA / continental matches
# ------------------------------------------------------------

uefa <- copy(
  dt[
    CompetitionType == "continental" &
      !is.na(Result) &
      Result != ""
  ]
)

continental_matches_before_filter <- nrow(
  uefa
)

uefa[
  ,
  HomeCountry := unname(
    team_country[Home]
  )
]

uefa[
  ,
  AwayCountry := unname(
    team_country[Away]
  )
]

uefa <- uefa[
  !is.na(HomeCountry) &
    !is.na(AwayCountry) &
    HomeCountry != AwayCountry
]

cross_country_matches_usable <- nrow(
  uefa
)


# ------------------------------------------------------------
# 6. Convert UEFA result to match score
#
# Win  = 1
# Draw = 0.5
# Loss = 0
# ------------------------------------------------------------

uefa[
  ,
  HomeScore := fifelse(
    Result == "1-0",
    1,
    fifelse(
      Result == "0-1",
      0,
      0.5
    )
  )
]


# ------------------------------------------------------------
# 7. Raw country-pair UEFA results
# ------------------------------------------------------------

uefa[
  ,
  `:=`(
    CountryA = pmin(
      HomeCountry,
      AwayCountry
    ),
    
    CountryB = pmax(
      HomeCountry,
      AwayCountry
    )
  )
]

uefa[
  ,
  ScoreA := fifelse(
    HomeCountry == CountryA,
    HomeScore,
    1 - HomeScore
  )
]

pairwise <- uefa[
  ,
  .(
    Games = .N,
    
    WinsA = sum(
      ScoreA == 1
    ),
    
    Draws = sum(
      ScoreA == 0.5
    ),
    
    LossesA = sum(
      ScoreA == 0
    ),
    
    ScorePctA = 100 *
      mean(
        ScoreA
      )
  ),
  by = .(
    CountryA,
    CountryB
  )
]

pairwise[
  ,
  RawEloDifferenceA :=
    round(
      400 *
        log10(
          (ScorePctA / 100) /
            (
              1 -
                ScorePctA / 100
            )
        )
    )
]

pairwise[
  ,
  ScorePctA :=
    round(
      ScorePctA,
      1
    )
]

setorder(
  pairwise,
  CountryA,
  CountryB
)


# ------------------------------------------------------------
# 8. Global UEFA country model
# ------------------------------------------------------------

countries <- sort(
  unique(
    c(
      uefa$HomeCountry,
      uefa$AwayCountry
    )
  )
)

reference_country <- if (
  "England" %in% countries
) {
  "England"
} else {
  countries[1]
}

uefa[
  ,
  HomeCountry := factor(
    HomeCountry,
    levels = countries
  )
]

uefa[
  ,
  AwayCountry := factor(
    AwayCountry,
    levels = countries
  )
]

model_countries <- setdiff(
  countries,
  reference_country
)

for (
  country in model_countries
) {
  
  col <- paste0(
    "country_",
    make.names(
      country
    )
  )
  
  uefa[
    ,
    (col) :=
      as.integer(
        HomeCountry ==
          country
      ) -
      as.integer(
        AwayCountry ==
          country
      )
  ]
}

country_cols <- paste0(
  "country_",
  make.names(
    model_countries
  )
)

formula_text <- paste(
  "HomeScore ~",
  paste(
    country_cols,
    collapse = " + "
  )
)

model <- glm(
  as.formula(
    formula_text
  ),
  data = uefa,
  family = quasibinomial(
    link = "logit"
  )
)


# ------------------------------------------------------------
# 9. Convert global model coefficients to Elo
# ------------------------------------------------------------

ELO_PER_LOGIT <- 400 /
  log(10)

coefs <- coef(
  model
)

home_advantage_elo <- unname(
  coefs[
    "(Intercept)"
  ] *
    ELO_PER_LOGIT
)

country_ratings <- data.table(
  Country = reference_country,
  RelativeUEFAElo = 0
)

for (
  country in model_countries
) {
  
  coef_name <- paste0(
    "country_",
    make.names(
      country
    )
  )
  
  country_ratings <- rbind(
    country_ratings,
    
    data.table(
      Country = country,
      
      RelativeUEFAElo =
        unname(
          coefs[
            coef_name
          ] *
            ELO_PER_LOGIT
        )
    )
  )
}

country_ratings[
  ,
  RelativeUEFAElo :=
    round(
      RelativeUEFAElo
    )
]


# ------------------------------------------------------------
# 10. UEFA match sample size by country
# ------------------------------------------------------------

country_games <- rbind(
  
  uefa[
    ,
    .(
      Country =
        as.character(
          HomeCountry
        )
    )
  ],
  
  uefa[
    ,
    .(
      Country =
        as.character(
          AwayCountry
        )
    )
  ]
  
)[
  ,
  .(
    UEFAMatches = .N
  ),
  by = Country
]

setorder(
  country_games,
  -UEFAMatches
)


# ------------------------------------------------------------
# 11. Prepare canonical Elo outputs
# ------------------------------------------------------------

required_rating_cols <- c(
  "Team",
  "Rating"
)

missing_rating_cols <- setdiff(
  required_rating_cols,
  names(
    final_ratings
  )
)

if (
  length(
    missing_rating_cols
  ) > 0
) {
  
  stop(
    "Final ratings CSV is missing: ",
    paste(
      missing_rating_cols,
      collapse = ", "
    )
  )
}

required_history_cols <- c(
  "Country",
  "CompetitionType",
  "Tier",
  "League",
  "Date",
  "Home",
  "Away",
  "HomeRating_After",
  "AwayRating_After"
)

missing_history_cols <- setdiff(
  required_history_cols,
  names(
    game_history
  )
)

if (
  length(
    missing_history_cols
  ) > 0
) {
  
  stop(
    "Game history CSV is missing: ",
    paste(
      missing_history_cols,
      collapse = ", "
    )
  )
}

final_ratings[
  ,
  `:=`(
    Team =
      trimws(
        as.character(
          Team
        )
      ),
    
    Rating =
      as.numeric(
        Rating
      )
  )
]

game_history[
  ,
  `:=`(
    Country =
      trimws(
        as.character(
          Country
        )
      ),
    
    CompetitionType =
      trimws(
        as.character(
          CompetitionType
        )
      ),
    
    Tier =
      as.integer(
        Tier
      ),
    
    League =
      trimws(
        as.character(
          League
        )
      ),
    
    Date =
      as.Date(
        Date
      ),
    
    Home =
      trimws(
        as.character(
          Home
        )
      ),
    
    Away =
      trimws(
        as.character(
          Away
        )
      ),
    
    HomeRating_After =
      as.numeric(
        HomeRating_After
      ),
    
    AwayRating_After =
      as.numeric(
        AwayRating_After
      )
  )
]


# ------------------------------------------------------------
# 12. Current domestic league membership
#
# Use each team's latest completed domestic league match.
# Names are already canonical in game_history.
# ------------------------------------------------------------

league_history <- game_history[
  CompetitionType ==
    "league" &
    !is.na(Date)
]

home_membership <- league_history[
  ,
  .(
    Team = Home,
    Country,
    Tier,
    League,
    Date
  )
]

away_membership <- league_history[
  ,
  .(
    Team = Away,
    Country,
    Tier,
    League,
    Date
  )
]

membership_history <- rbind(
  home_membership,
  away_membership
)

membership_history <- membership_history[
  !is.na(Team) &
    Team != ""
]

setorder(
  membership_history,
  Team,
  Date
)

current_membership <- membership_history[
  ,
  .SD[.N],
  by = Team
]

current_teams <- merge(
  current_membership,
  
  final_ratings[
    ,
    .(
      Team,
      Rating
    )
  ],
  
  by = "Team",
  
  all = FALSE
)


# ------------------------------------------------------------
# 13. Current top divisions only
# ------------------------------------------------------------

current_top_divisions <- current_teams[
  Tier == 1L &
    Country %in%
    country_ratings$Country &
    !is.na(Rating)
]


# ------------------------------------------------------------
# 14. Current top-division depth
#
# Full league average vs top 6 average
# ------------------------------------------------------------

league_depth <- current_top_divisions[
  order(
    Country,
    -Rating
  ),
  {
    
    ratings_sorted <- sort(
      Rating,
      decreasing = TRUE
    )
    
    top_n_actual <- min(
      TOP_N,
      length(
        ratings_sorted
      )
    )
    
    top_ratings <- ratings_sorted[
      seq_len(
        top_n_actual
      )
    ]
    
    .(
      Teams =
        length(
          ratings_sorted
        ),
      
      LeagueAverageElo =
        mean(
          ratings_sorted
        ),
      
      TopN =
        top_n_actual,
      
      TopAverageElo =
        mean(
          top_ratings
        ),
      
      DepthGap =
        mean(
          top_ratings
        ) -
        mean(
          ratings_sorted
        )
    )
  },
  by = Country
]

league_depth[
  ,
  `:=`(
    LeagueAverageElo =
      round(
        LeagueAverageElo
      ),
    
    TopAverageElo =
      round(
        TopAverageElo
      ),
    
    DepthGap =
      round(
        DepthGap
      )
  )
]


# ------------------------------------------------------------
# 15. Combine UEFA calibration with domestic league depth
# ------------------------------------------------------------

calibration <- merge(
  country_ratings,
  league_depth,
  by = "Country",
  all.x = TRUE
)

reference_depth_gap <- calibration[
  Country ==
    reference_country,
  DepthGap
]

if (
  length(
    reference_depth_gap
  ) != 1L ||
  is.na(
    reference_depth_gap
  )
) {
  
  stop(
    "Could not determine reference-country depth gap."
  )
}

calibration[
  ,
  RelativeLeagueElo :=
    RelativeUEFAElo -
    (
      DepthGap -
        reference_depth_gap
    )
]

calibration[
  ,
  SuggestedTier1Seed :=
    BASE_ELO +
    RelativeLeagueElo
]

calibration[
  ,
  `:=`(
    RelativeLeagueElo =
      round(
        RelativeLeagueElo
      ),
    
    SuggestedTier1Seed =
      round(
        SuggestedTier1Seed
      )
  )
]

setorder(
  calibration,
  -SuggestedTier1Seed
)


# ------------------------------------------------------------
# 16. Top-6 clubs used
# ------------------------------------------------------------

top_team_detail <- current_top_divisions[
  order(
    Country,
    -Rating
  ),
  head(
    .SD,
    TOP_N
  ),
  by = Country
][
  ,
  .(
    Country,
    Team,
    Rating =
      round(
        Rating
      )
  )
]


# ============================================================
# HISTORICAL DOMESTIC TIER GAP ANALYSIS
# ============================================================


# ------------------------------------------------------------
# 17. Add football-season labels to game history
# ------------------------------------------------------------

gh <- copy(
  game_history
)

gh[
  ,
  SeasonStart :=
    fifelse(
      as.integer(
        format(
          Date,
          "%m"
        )
      ) >= 7,
      
      as.integer(
        format(
          Date,
          "%Y"
        )
      ),
      
      as.integer(
        format(
          Date,
          "%Y"
        )
      ) - 1L
    )
]

gh[
  ,
  Season := paste0(
    SeasonStart,
    "/",
    sprintf(
      "%02d",
      (
        SeasonStart + 1L
      ) %% 100
    )
  )
]


# ------------------------------------------------------------
# 18. Domestic league matches only
# ------------------------------------------------------------

league_games <- gh[
  CompetitionType ==
    "league" &
    !is.na(Tier) &
    !is.na(Date)
]


# ------------------------------------------------------------
# 19. Build team-season Elo history
#
# For each domestic match we record each team's
# Elo immediately after that match.
# ------------------------------------------------------------

home_end <- league_games[
  ,
  .(
    Team = Home,
    Country,
    Tier,
    League,
    SeasonStart,
    Season,
    Date,
    EloAfter =
      HomeRating_After
  )
]

away_end <- league_games[
  ,
  .(
    Team = Away,
    Country,
    Tier,
    League,
    SeasonStart,
    Season,
    Date,
    EloAfter =
      AwayRating_After
  )
]

team_season_history <- rbind(
  home_end,
  away_end
)

team_season_history <- team_season_history[
  !is.na(Team) &
    Team != "" &
    !is.na(EloAfter) &
    !is.na(SeasonStart)
]


# ------------------------------------------------------------
# 20. Find each team's final Elo in each domestic season
# ------------------------------------------------------------

setorder(
  team_season_history,
  Team,
  SeasonStart,
  Date
)

team_season_end <- team_season_history[
  ,
  .SD[.N],
  by = .(
    Team,
    SeasonStart
  )
]


# ------------------------------------------------------------
# 21. Average Elo by country / season / tier
# ------------------------------------------------------------

season_tier_averages <- team_season_end[
  ,
  .(
    Teams =
      uniqueN(
        Team
      ),
    
    AverageElo =
      mean(
        EloAfter
      )
  ),
  by = .(
    Country,
    SeasonStart,
    Season,
    Tier,
    League
  )
]

season_tier_averages[
  ,
  AverageElo :=
    round(
      AverageElo
    )
]

setorder(
  season_tier_averages,
  Country,
  SeasonStart,
  Tier
)


# ------------------------------------------------------------
# 22. Collapse any duplicate league labels within same tier
#
# This protects against historical naming changes such as
# Division 2 / League 1.
# ------------------------------------------------------------

season_tier_country <- team_season_end[
  ,
  .(
    Teams =
      uniqueN(
        Team
      ),
    
    AverageElo =
      mean(
        EloAfter
      )
  ),
  by = .(
    Country,
    SeasonStart,
    Season,
    Tier
  )
]

season_tier_country[
  ,
  AverageElo :=
    round(
      AverageElo
    )
]

setorder(
  season_tier_country,
  Country,
  SeasonStart,
  Tier
)


# ------------------------------------------------------------
# 23. Tier 1 -> Tier 2 gaps
# ------------------------------------------------------------

tier1 <- season_tier_country[
  Tier == 1L,
  .(
    Country,
    SeasonStart,
    Season,
    HigherTierTeams = Teams,
    HigherTierAverage =
      AverageElo
  )
]

tier2 <- season_tier_country[
  Tier == 2L,
  .(
    Country,
    SeasonStart,
    LowerTierTeams = Teams,
    LowerTierAverage =
      AverageElo
  )
]

tier12 <- merge(
  tier1,
  tier2,
  by = c(
    "Country",
    "SeasonStart"
  ),
  all = FALSE
)

tier12[
  ,
  `:=`(
    FromTier = 1L,
    ToTier = 2L,
    
    Gap =
      HigherTierAverage -
      LowerTierAverage
  )
]


# ------------------------------------------------------------
# 24. Tier 2 -> Tier 3 gaps
# ------------------------------------------------------------

tier2_high <- season_tier_country[
  Tier == 2L,
  .(
    Country,
    SeasonStart,
    Season,
    HigherTierTeams = Teams,
    HigherTierAverage =
      AverageElo
  )
]

tier3 <- season_tier_country[
  Tier == 3L,
  .(
    Country,
    SeasonStart,
    LowerTierTeams = Teams,
    LowerTierAverage =
      AverageElo
  )
]

tier23 <- merge(
  tier2_high,
  tier3,
  by = c(
    "Country",
    "SeasonStart"
  ),
  all = FALSE
)

tier23[
  ,
  `:=`(
    FromTier = 2L,
    ToTier = 3L,
    
    Gap =
      HigherTierAverage -
      LowerTierAverage
  )
]


# ------------------------------------------------------------
# 25. Tier 3 -> Tier 4 gaps
# ------------------------------------------------------------

tier3_high <- season_tier_country[
  Tier == 3L,
  .(
    Country,
    SeasonStart,
    Season,
    HigherTierTeams = Teams,
    HigherTierAverage =
      AverageElo
  )
]

tier4 <- season_tier_country[
  Tier == 4L,
  .(
    Country,
    SeasonStart,
    LowerTierTeams = Teams,
    LowerTierAverage =
      AverageElo
  )
]

tier34 <- merge(
  tier3_high,
  tier4,
  by = c(
    "Country",
    "SeasonStart"
  ),
  all = FALSE
)

tier34[
  ,
  `:=`(
    FromTier = 3L,
    ToTier = 4L,
    
    Gap =
      HigherTierAverage -
      LowerTierAverage
  )
]


# ------------------------------------------------------------
# 26. Tier 4 -> Tier 5 gaps
# ------------------------------------------------------------

tier4_high <- season_tier_country[
  Tier == 4L,
  .(
    Country,
    SeasonStart,
    Season,
    HigherTierTeams = Teams,
    HigherTierAverage =
      AverageElo
  )
]

tier5 <- season_tier_country[
  Tier == 5L,
  .(
    Country,
    SeasonStart,
    LowerTierTeams = Teams,
    LowerTierAverage =
      AverageElo
  )
]

tier45 <- merge(
  tier4_high,
  tier5,
  by = c(
    "Country",
    "SeasonStart"
  ),
  all = FALSE
)

tier45[
  ,
  `:=`(
    FromTier = 4L,
    ToTier = 5L,
    
    Gap =
      HigherTierAverage -
      LowerTierAverage
  )
]


# ------------------------------------------------------------
# 27. Combine all adjacent-tier observations
# ------------------------------------------------------------

season_tier_gaps <- rbindlist(
  list(
    
    tier12[
      ,
      .(
        Country,
        SeasonStart,
        FromTier,
        ToTier,
        HigherTierTeams,
        LowerTierTeams,
        HigherTierAverage,
        LowerTierAverage,
        Gap
      )
    ],
    
    tier23[
      ,
      .(
        Country,
        SeasonStart,
        FromTier,
        ToTier,
        HigherTierTeams,
        LowerTierTeams,
        HigherTierAverage,
        LowerTierAverage,
        Gap
      )
    ],
    
    tier34[
      ,
      .(
        Country,
        SeasonStart,
        FromTier,
        ToTier,
        HigherTierTeams,
        LowerTierTeams,
        HigherTierAverage,
        LowerTierAverage,
        Gap
      )
    ],
    
    tier45[
      ,
      .(
        Country,
        SeasonStart,
        FromTier,
        ToTier,
        HigherTierTeams,
        LowerTierTeams,
        HigherTierAverage,
        LowerTierAverage,
        Gap
      )
    ]
  ),
  
  fill = TRUE
)

season_tier_gaps[
  ,
  Season := paste0(
    SeasonStart,
    "/",
    sprintf(
      "%02d",
      (
        SeasonStart + 1L
      ) %% 100
    )
  )
]

setorder(
  season_tier_gaps,
  Country,
  SeasonStart,
  FromTier
)


# ------------------------------------------------------------
# 28. Summary across all seasons
# ------------------------------------------------------------

tier_gap_summary <- season_tier_gaps[
  ,
  .(
    Seasons = .N,
    
    MeanGap =
      round(
        mean(
          Gap
        )
      ),
    
    MedianGap =
      round(
        median(
          Gap
        )
      ),
    
    MinGap =
      min(
        Gap
      ),
    
    MaxGap =
      max(
        Gap
      )
  ),
  by = .(
    FromTier,
    ToTier
  )
]

setorder(
  tier_gap_summary,
  FromTier
)


# ------------------------------------------------------------
# 29. Tier 1 -> Tier 2 summary by country
# ------------------------------------------------------------

country_tier12_summary <- season_tier_gaps[
  FromTier == 1L &
    ToTier == 2L,
  .(
    Seasons = .N,
    
    MeanGap =
      round(
        mean(
          Gap
        )
      ),
    
    MedianGap =
      round(
        median(
          Gap
        )
      ),
    
    MinGap =
      min(
        Gap
      ),
    
    MaxGap =
      max(
        Gap
      )
  ),
  by = Country
]

setorder(
  country_tier12_summary,
  -MeanGap
)


# ------------------------------------------------------------
# 30. Latest season available for each country/tier gap
# ------------------------------------------------------------

latest_tier_gaps <- season_tier_gaps[
  order(
    Country,
    FromTier,
    SeasonStart
  ),
  .SD[.N],
  by = .(
    Country,
    FromTier,
    ToTier
  )
]

setorder(
  latest_tier_gaps,
  Country,
  FromTier
)


# ============================================================
# PRINT EVERYTHING AT THE END
# ============================================================


cat("\n============================================================\n")
cat("UEFA COUNTRY CALIBRATION SUMMARY\n")
cat("============================================================\n\n")

cat(
  "Domestic teams mapped:",
  domestic_teams_mapped,
  "\n"
)

cat(
  "Continental matches before country filtering:",
  continental_matches_before_filter,
  "\n"
)

cat(
  "Cross-country matches usable:",
  cross_country_matches_usable,
  "\n"
)

cat(
  "Top domestic clubs used per league:",
  TOP_N,
  "\n"
)


cat("\n============================================================\n")
cat("RAW COUNTRY PAIR RESULTS\n")
cat("============================================================\n\n")

print(
  pairwise
)


cat("\n============================================================\n")
cat("GLOBAL UEFA TOP-CLUB CALIBRATION\n")
cat("============================================================\n\n")

cat(
  "Reference country:",
  reference_country,
  "\n"
)

cat(
  "Estimated UEFA home advantage:",
  round(
    home_advantage_elo
  ),
  "Elo\n\n"
)

print(
  country_ratings[
    order(
      -RelativeUEFAElo
    )
  ]
)


cat("\n============================================================\n")
cat("CURRENT TOP-DIVISION DEPTH\n")
cat("============================================================\n\n")

print(
  league_depth[
    order(
      -LeagueAverageElo
    )
  ]
)


cat("\n============================================================\n")
cat("TOP 6 CLUBS USED\n")
cat("============================================================\n\n")

print(
  top_team_detail
)


cat("\n============================================================\n")
cat("DEPTH-ADJUSTED COUNTRY CALIBRATION\n")
cat("============================================================\n\n")

print(
  calibration[
    ,
    .(
      Country,
      RelativeUEFAElo,
      Teams,
      LeagueAverageElo,
      TopAverageElo,
      DepthGap,
      RelativeLeagueElo,
      SuggestedTier1Seed
    )
  ]
)


cat("\n============================================================\n")
cat("UEFA SAMPLE SIZE BY COUNTRY\n")
cat("============================================================\n\n")

print(
  country_games
)


cat("\n============================================================\n")
cat("DOMESTIC TIER GAP SUMMARY - ALL SEASONS\n")
cat("============================================================\n\n")

print(
  tier_gap_summary
)


cat("\n============================================================\n")
cat("TIER 1 TO TIER 2 GAP BY COUNTRY\n")
cat("============================================================\n\n")

print(
  country_tier12_summary
)


cat("\n============================================================\n")
cat("LATEST DOMESTIC TIER GAPS\n")
cat("============================================================\n\n")

print(
  latest_tier_gaps[
    ,
    .(
      Country,
      Season,
      FromTier,
      ToTier,
      HigherTierTeams,
      LowerTierTeams,
      HigherTierAverage,
      LowerTierAverage,
      Gap
    )
  ]
)


cat("\n============================================================\n")
cat("LATEST 20 SEASON/TIER GAP OBSERVATIONS\n")
cat("============================================================\n\n")

print(
  tail(
    season_tier_gaps[
      order(
        SeasonStart,
        Country,
        FromTier
      )
    ],
    20
  )
)

# ============================================================
# 31. INFLATION / ENTRY CALIBRATION AUDIT
# ============================================================
# Main evidence for future seed changes.  This separates broad rating
# inflation from top-end concentration, then tests new-team entry,
# coverage expansion and promotion/relegation directly.
#
# PASS 1 is preferred for seed diagnostics because it contains the
# actual initial seed assumptions before the retrospective second pass.
# ============================================================

safe_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(quantile(x, p, na.rm = TRUE, names = FALSE))
}

period_label <- function(y) {
  fifelse(y < 1990, "Before 1990",
          fifelse(y < 2000, "1990-1999",
                  fifelse(y < 2010, "2000-2009",
                          fifelse(y < 2020, "2010-2019", "2020-present"))))
}

prep_history <- function(x) {
  y <- copy(x)
  must <- c("Date","Country","CompetitionType","Tier","League","Home","Away",
            "HomeRating_After","AwayRating_After")
  miss <- setdiff(must, names(y))
  if (length(miss)) stop("History file missing: ", paste(miss, collapse=", "))
  y[, `:=`(
    Date=as.Date(Date), Country=trimws(as.character(Country)),
    CompetitionType=trimws(as.character(CompetitionType)), Tier=as.integer(Tier),
    League=trimws(as.character(League)), Home=trimws(as.character(Home)),
    Away=trimws(as.character(Away)), HomeRating_After=as.numeric(HomeRating_After),
    AwayRating_After=as.numeric(AwayRating_After)
  )]
  if (all(c("HomeRating_Before","AwayRating_Before") %in% names(y))) {
    y[, `:=`(HomeRating_Before=as.numeric(HomeRating_Before),
             AwayRating_Before=as.numeric(AwayRating_Before))]
  }
  y
}

hf <- prep_history(game_history)
hs <- if (!is.null(pass1_history)) prep_history(pass1_history) else copy(hf)
cat("\nSeed diagnostics source:", if (!is.null(pass1_history)) "PASS 1" else "FINAL HISTORY", "\n")

# ---- A. Active rating pool by calendar year -----------------
make_yearly <- function(h) {
  z <- rbind(
    h[!is.na(Date) & is.finite(HomeRating_After), .(Team=Home, Date, Rating=HomeRating_After)],
    h[!is.na(Date) & is.finite(AwayRating_After), .(Team=Away, Date, Rating=AwayRating_After)]
  )
  z <- z[!is.na(Team) & Team != "" & is.finite(Rating)]
  z[, Year := as.integer(format(Date, "%Y"))]
  setorder(z, Team, Year, Date)
  z[, .SD[.N], by=.(Team, Year)]
}

yr <- make_yearly(hf)

rating_pool_by_year <- yr[, .(
  ActiveTeams=uniqueN(Team), TotalElo=round(sum(Rating)),
  MeanElo=round(mean(Rating),1), MedianElo=round(median(Rating),1),
  P10=round(safe_q(Rating,.10),1), P25=round(safe_q(Rating,.25),1),
  P75=round(safe_q(Rating,.75),1), P90=round(safe_q(Rating,.90),1),
  P95=round(safe_q(Rating,.95),1),
  Top10Average=round(mean(sort(Rating,decreasing=TRUE)[seq_len(min(10L,.N))]),1)
), by=Year]
setorder(rating_pool_by_year, Year)
fwrite(rating_pool_by_year, file.path(AUDIT_DIR,"rating_pool_by_year.csv"))

# ---- B. Top-tier distribution by year -----------------------
lg <- hf[CompetitionType=="league" & !is.na(Date) & !is.na(Tier)]
mem <- rbind(
  lg[,.(Team=Home,Year=as.integer(format(Date,"%Y")),Date,Country,Tier)],
  lg[,.(Team=Away,Year=as.integer(format(Date,"%Y")),Date,Country,Tier)]
)
setorder(mem, Team, Year, Date)
mem <- mem[, .SD[.N], by=.(Team,Year)]

top <- merge(yr[,.(Team,Year,Rating)], mem[,.(Team,Year,Country,Tier)],
             by=c("Team","Year"), all=FALSE)[Tier==1L]

top_tier_distribution <- top[, .(
  Teams=uniqueN(Team), MeanElo=round(mean(Rating),1), MedianElo=round(median(Rating),1),
  P10=round(safe_q(Rating,.10),1), P25=round(safe_q(Rating,.25),1),
  P75=round(safe_q(Rating,.75),1), P90=round(safe_q(Rating,.90),1),
  P95=round(safe_q(Rating,.95),1),
  Top6Average=round(mean(sort(Rating,decreasing=TRUE)[seq_len(min(6L,.N))]),1)
), by=Year]
top_tier_distribution[, `:=`(
  Top6MinusMedian=round(Top6Average-MedianElo,1),
  P90MinusMedian=round(P90-MedianElo,1),
  P10MinusMedian=round(P10-MedianElo,1)
)]
setorder(top_tier_distribution, Year)
fwrite(top_tier_distribution, file.path(AUDIT_DIR,"top_tier_distribution_by_year.csv"))

country_top <- top[, .(
  Teams=uniqueN(Team), MeanElo=round(mean(Rating),1), MedianElo=round(median(Rating),1),
  P90=round(safe_q(Rating,.90),1),
  Top6Average=round(mean(sort(Rating,decreasing=TRUE)[seq_len(min(6L,.N))]),1)
), by=.(Country,Year)]
fwrite(country_top, file.path(AUDIT_DIR,"top_tier_distribution_by_country_year.csv"))

# ---- C. New-team / coverage-entry seed test -----------------
seedlg <- hs[CompetitionType=="league" & !is.na(Date) & !is.na(Tier)]
has_before <- all(c("HomeRating_Before","AwayRating_Before") %in% names(seedlg))
entry_rating_change_summary <- data.table()
coverage_entry_summary <- data.table()

if (has_before) {
  tm <- rbind(
    seedlg[,.(Team=Home,Date,Country,Tier,League,Before=HomeRating_Before,After=HomeRating_After)],
    seedlg[,.(Team=Away,Date,Country,Tier,League,Before=AwayRating_Before,After=AwayRating_After)]
  )
  tm <- tm[!is.na(Team) & Team!="" & is.finite(Before) & is.finite(After)]
  setorder(tm, Team, Date)
  tm[, GameNo:=seq_len(.N), by=Team]
  
  ent <- tm[GameNo==1L, .(Team,EntryDate=Date,EntryYear=as.integer(format(Date,"%Y")),
                          EntryCountry=Country,EntryTier=Tier,InitialRating=Before)]
  ent[, EntryPeriod:=period_label(EntryYear)]
  
  cp <- dcast(tm[GameNo %in% c(20L,50L,100L),
                 .(Team,Key=paste0("R",GameNo),After)], Team~Key, value.var="After")
  ent <- merge(ent, cp, by="Team", all.x=TRUE)
  for (n in c(20L,50L,100L)) {
    rc <- paste0("R",n); cc <- paste0("Change",n)
    ent[, (cc) := if (rc %in% names(ent)) get(rc)-InitialRating else NA_real_]
  }
  
  seedlg[, SeasonStart:=fifelse(as.integer(format(Date,"%m"))>=7,
                                as.integer(format(Date,"%Y")),
                                as.integer(format(Date,"%Y"))-1L)]
  cov <- seedlg[,.(CoverageStartSeason=min(SeasonStart)), by=.(Country,Tier)]
  ent[, EntrySeasonStart:=fifelse(as.integer(format(EntryDate,"%m"))>=7,
                                  as.integer(format(EntryDate,"%Y")),
                                  as.integer(format(EntryDate,"%Y"))-1L)]
  ent <- merge(ent,cov,by.x=c("EntryCountry","EntryTier"),by.y=c("Country","Tier"),all.x=TRUE)
  ent[, CoverageEntry:=EntrySeasonStart==CoverageStartSeason]
  
  entry_rating_change_summary <- ent[, .(
    Teams=.N, MedianInitial=round(median(InitialRating,na.rm=TRUE),1),
    Teams20=sum(is.finite(Change20)), MeanChange20=round(mean(Change20,na.rm=TRUE),1),
    MedianChange20=round(median(Change20,na.rm=TRUE),1),
    Teams50=sum(is.finite(Change50)), MeanChange50=round(mean(Change50,na.rm=TRUE),1),
    MedianChange50=round(median(Change50,na.rm=TRUE),1),
    Teams100=sum(is.finite(Change100)), MeanChange100=round(mean(Change100,na.rm=TRUE),1),
    MedianChange100=round(median(Change100,na.rm=TRUE),1)
  ), by=.(EntryCountry,EntryTier,EntryPeriod,CoverageEntry)]
  
  coverage_entry_summary <- ent[CoverageEntry==TRUE, .(
    Teams=.N, MedianInitial=round(median(InitialRating),1),
    MeanChange20=round(mean(Change20,na.rm=TRUE),1),
    MeanChange50=round(mean(Change50,na.rm=TRUE),1),
    MeanChange100=round(mean(Change100,na.rm=TRUE),1)
  ), by=.(EntryCountry,EntryTier,CoverageStartSeason)]
  
  fwrite(ent, file.path(AUDIT_DIR,"team_entry_checkpoints.csv"))
  fwrite(entry_rating_change_summary, file.path(AUDIT_DIR,"entry_rating_change_summary.csv"))
  fwrite(coverage_entry_summary, file.path(AUDIT_DIR,"coverage_entry_summary.csv"))
} else {
  cat("WARNING: Before-rating columns absent; entry seed test skipped.\n")
}

# ---- D. Promotion / relegation test -------------------------
tr <- rbind(
  lg[,.(Team=Home,Country,Tier,Date,Rating=HomeRating_After)],
  lg[,.(Team=Away,Country,Tier,Date,Rating=AwayRating_After)]
)
tr[, SeasonStart:=fifelse(as.integer(format(Date,"%m"))>=7,
                          as.integer(format(Date,"%Y")),
                          as.integer(format(Date,"%Y"))-1L)]
setorder(tr,Team,SeasonStart,Date)
ts <- tr[,.(Country=Country[.N],Tier=Tier[.N]),by=.(Team,SeasonStart)]
setorder(ts,Team,SeasonStart)
ts[,`:=`(PrevSeasonStart=shift(SeasonStart),PrevTier=shift(Tier),PrevCountry=shift(Country)),by=Team]
changes <- ts[!is.na(PrevTier) & SeasonStart==PrevSeasonStart+1L & Country==PrevCountry & Tier!=PrevTier]
changes[,Direction:=fifelse(Tier<PrevTier,"Promoted","Relegated")]

tr2 <- merge(tr,changes[,.(Team,SeasonStart,Country,PrevTier,Tier,Direction)],
             by=c("Team","SeasonStart","Country","Tier"),all=FALSE)
setorder(tr2,Team,SeasonStart,Date)
tr2[,N:=seq_len(.N),by=.(Team,SeasonStart)]
chk <- tr2[,.(StartRating=Rating[1],R20=if(.N>=20L) Rating[20L] else NA_real_,
              R30=if(.N>=30L) Rating[30L] else NA_real_),
           by=.(Team,SeasonStart,Country,PrevTier,Tier,Direction)]
chk[,`:=`(Change20=R20-StartRating,Change30=R30-StartRating)]

promotion_relegation_summary <- chk[,.(
  Clubs=.N, Clubs20=sum(is.finite(Change20)),
  MeanChange20=round(mean(Change20,na.rm=TRUE),1), MedianChange20=round(median(Change20,na.rm=TRUE),1),
  Clubs30=sum(is.finite(Change30)),
  MeanChange30=round(mean(Change30,na.rm=TRUE),1), MedianChange30=round(median(Change30,na.rm=TRUE),1)
),by=.(Country,PrevTier,Tier,Direction)]

fwrite(chk,file.path(AUDIT_DIR,"promotion_relegation_checkpoints.csv"))
fwrite(promotion_relegation_summary,file.path(AUDIT_DIR,"promotion_relegation_summary.csv"))

# ---- E. Same-team cohort drift ------------------------------
cohort_rows <- list()
for (b in c(1990L,1995L,2000L)) {
  teams_b <- yr[Year==b,unique(Team)]
  if (!length(teams_b)) next
  z <- yr[Team %in% teams_b & Year>=b,.(
    ActiveCohortTeams=uniqueN(Team),MeanElo=round(mean(Rating),1),
    MedianElo=round(median(Rating),1),P90=round(safe_q(Rating,.90),1)
  ),by=Year]
  z[,BenchmarkYear:=b]
  cohort_rows[[as.character(b)]] <- z
}
stable_cohort_drift <- rbindlist(cohort_rows,fill=TRUE)
if (nrow(stable_cohort_drift)) {
  setcolorder(stable_cohort_drift,c("BenchmarkYear","Year","ActiveCohortTeams","MeanElo","MedianElo","P90"))
  fwrite(stable_cohort_drift,file.path(AUDIT_DIR,"stable_cohort_drift.csv"))
}

# ---- F. Compact console output ------------------------------
sel <- unique(c(1990L,1995L,2000L,2005L,2010L,2015L,2020L,max(rating_pool_by_year$Year)))
cat("\n============================================================\nRATING POOL - SELECTED YEARS\n============================================================\n\n")
print(rating_pool_by_year[Year %in% sel])
cat("\n============================================================\nTOP-TIER DISTRIBUTION - SELECTED YEARS\n============================================================\n\n")
print(top_tier_distribution[Year %in% sel])
if (nrow(coverage_entry_summary)) {
  cat("\n============================================================\nBULK COVERAGE ENTRY\n============================================================\n\n")
  print(coverage_entry_summary)
}
if (nrow(entry_rating_change_summary)) {
  cat("\n============================================================\nENTRY CHANGE AFTER 20 / 50 / 100 LEAGUE GAMES\n============================================================\n\n")
  print(entry_rating_change_summary)
}
cat("\n============================================================\nPROMOTION / RELEGATION ADJUSTMENT\n============================================================\n\n")
print(promotion_relegation_summary)
if (nrow(stable_cohort_drift)) {
  cat("\n============================================================\nSTABLE COHORT DRIFT - SELECTED YEARS\n============================================================\n\n")
  print(stable_cohort_drift[Year %in% sel])
}
cat("\nAudit CSVs written to:\n",AUDIT_DIR,"\n")

beep()