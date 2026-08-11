options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

source_root_dir <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Source",
  "openfootball"
)

combined_dir <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Matches_Clean_Combined"
)

dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(
  combined_dir,
  "england_leagues_1_to_5_all_seasons.csv"
)

# -----------------------------
# Helpers
# -----------------------------

trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

month_num <- function(mon) {
  m <- c(
    Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
    Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12
  )
  
  unname(m[mon])
}

infer_year_from_season <- function(season_str) {
  parts <- strsplit(season_str, "/")[[1]]
  start_year <- as.integer(parts[1])
  end_part <- parts[2]
  
  if (nchar(end_part) == 2) {
    start_century <- (start_year %/% 100) * 100
    start_yy <- start_year %% 100
    end_yy <- as.integer(end_part)
    
    if (end_yy < start_yy) {
      end_year <- start_century + 100 + end_yy
    } else {
      end_year <- start_century + end_yy
    }
  } else {
    end_year <- as.integer(end_part)
  }
  
  list(start_year = start_year, end_year = end_year)
}

current_openfootball_season <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))
  
  start_year <- if (m >= 7L) y else y - 1L
  end_short <- sprintf("%02d", (start_year + 1L) %% 100L)
  
  paste0(start_year, "-", end_short)
}

result_code <- function(hg, ag) {
  if (hg > ag) {
    "1-0"
  } else if (hg < ag) {
    "0-1"
  } else {
    "0.5-0.5"
  }
}

normalise_openfootball_season_label <- function(season_folder) {
  # Converts folder style 2025-26 to file/header style 2025/26
  gsub("-", "/", season_folder, fixed = TRUE)
}

# -----------------------------
# Parser
# -----------------------------

parse_comp_file <- function(txt_path, country, competition_id, competition_type, league_label, tier, source = "openfootball") {
  lines <- readLines(txt_path, warn = FALSE, encoding = "UTF-8")
  
  hdr_idx <- grep("^\\s*=\\s*.*\\b\\d{4}/\\d{2,4}\\b\\s*$", lines)
  
  if (length(hdr_idx) == 0) {
    stop("Could not find season header in: ", txt_path)
  }
  
  season_line <- lines[hdr_idx[1]]
  season_str <- sub(".*\\b(\\d{4}/\\d{2,4})\\b.*", "\\1", season_line)
  yrs <- infer_year_from_season(season_str)
  
  current_date <- as.Date(NA)
  
  out_season <- character()
  out_date   <- character()
  out_home   <- character()
  out_away   <- character()
  out_res    <- character()
  out_score  <- character()
  
  # Some OpenFootball files split a penalty-decided match across two lines:
  #   Home v Away 2-4 pen.
  #   1-0 a.e.t. (...)
  # The first score is the shoot-out; the second is the actual match score.
  pending_penalty <- NULL
  
  append_match <- function(home, away, hg = NA_integer_, ag = NA_integer_, score = "") {
    home <- trim(home)
    away <- trim(away)
    
    # Remove penalty shoot-out text accidentally attached to a team name.
    home <- sub("\\s+\\d+-\\d+\\s+pen\\.?\\s*$", "", home, ignore.case = TRUE)
    away <- sub("\\s+\\d+-\\d+\\s+pen\\.?\\s*$", "", away, ignore.case = TRUE)
    
    # Champions League files append a three-letter association code to clubs,
    # e.g. "Arsenal FC (ENG)" or "FC Barcelona (ESP)".
    if (identical(competition_type, "continental")) {
      home <- sub("\\s+\\([A-Z]{3}\\)\\s*$", "", home)
      away <- sub("\\s+\\([A-Z]{3}\\)\\s*$", "", away)
    }
    
    home <- trim(home)
    away <- trim(away)
    
    if (home == "" || away == "") return(invisible(NULL))
    
    out_season <<- c(out_season, season_str)
    out_date   <<- c(out_date, format(current_date, "%Y-%m-%d"))
    out_home   <<- c(out_home, home)
    out_away   <<- c(out_away, away)
    
    if (is.na(hg) || is.na(ag)) {
      out_res   <<- c(out_res, "")
      out_score <<- c(out_score, "")
    } else {
      out_res   <<- c(out_res, result_code(hg, ag))
      out_score <<- c(out_score, if (score == "") sprintf("%d-%d", hg, ag) else score)
    }
    
    invisible(NULL)
  }
  
  # Supports:
  #   [Sat Aug/15]
  #   [Sat Aug 15]
  #   Sat Aug/15
  #   Sat Aug 15
  #   Fri Aug 15 2025
  date_pat_old_bracket <- "^\\s*\\[[A-Za-z]{3}\\s+([A-Za-z]{3})(?:/|\\s+)(\\d{1,2})(?:\\s+(\\d{4}))?\\]\\s*$"
  date_pat_new_plain   <- "^\\s*[A-Za-z]{3}\\s+([A-Za-z]{3})(?:/|\\s+)(\\d{1,2})(?:\\s+(\\d{4}))?\\s*$"
  
  time_pat <- "(?:\\d{1,2}[:.]\\d{2}\\s+)?"
  
  # Old score-in-the-middle format, e.g. Team A 2-1 Team B.
  match_pat_old <- paste0(
    "^\\s*", time_pat,
    "(.+?)\\s+(\\d+)-(\\d+)(?:\\s+\\([^)]*\\))?\\s+(.+?)\\s*$"
  )
  
  # Older cup-style formats retained for future knockout competitions.
  match_pat_pen <- paste0(
    "^\\s*", time_pat,
    "(.+?)\\s+(\\d+)-(\\d+)\\s+pen\\.?\\s+(\\d+)-(\\d+)(?:\\s+a\\.e\\.t\\.)?(?:\\s+\\([^)]*\\))?\\s+(.+?)\\s*$"
  )
  
  match_pat_aet <- paste0(
    "^\\s*", time_pat,
    "(.+?)\\s+(\\d+)-(\\d+)\\s+a\\.e\\.t\\.(?:\\s+\\([^)]*\\))?\\s+(.+?)\\s*$"
  )
  
  for (ln in lines) {
    ln <- gsub("\\t", " ", ln)
    ln <- gsub("\u00a0", " ", ln, fixed = TRUE)
    ln <- gsub("\\s+", " ", ln)
    ln <- trim(ln)
    
    # Resolve a split penalty record. Elo uses the actual match score,
    # never the penalty shoot-out score.
    if (!is.null(pending_penalty)) {
      if (ln == "" || grepl("^#", ln) || grepl("^==", ln)) {
        next
      }
      
      continuation_pat <- "^(\\d+)-(\\d+)(?:\\s+a\\.e\\.t\\.)?(?:\\s+\\([^)]*\\))?\\s*$"
      
      if (!grepl(continuation_pat, ln, ignore.case = TRUE)) {
        stop(
          "Penalty shoot-out line was not followed by an actual match score.\n",
          "File: ", txt_path, "\n",
          "Home: ", pending_penalty$home, "\n",
          "Away: ", pending_penalty$away, "\n",
          "Penalty score: ", pending_penalty$pen_h, "-", pending_penalty$pen_a, "\n",
          "Next line: ", ln
        )
      }
      
      hg <- as.integer(sub(continuation_pat, "\\1", ln, ignore.case = TRUE))
      ag <- as.integer(sub(continuation_pat, "\\2", ln, ignore.case = TRUE))
      
      append_match(
        pending_penalty$home,
        pending_penalty$away,
        hg,
        ag,
        sprintf(
          "%d-%d (p %d-%d)",
          hg, ag,
          pending_penalty$pen_h,
          pending_penalty$pen_a
        )
      )
      
      pending_penalty <- NULL
      next
    }
    
    if (grepl("^#", ln)) next
    if (grepl("^==", ln)) next
    if (grepl("Matchday", ln, ignore.case = TRUE)) next
    if (ln == "") next
    
    # Date headers.
    if (grepl(date_pat_old_bracket, ln)) {
      mon <- sub(date_pat_old_bracket, "\\1", ln)
      dd  <- as.integer(sub(date_pat_old_bracket, "\\2", ln))
      yy_txt <- sub(date_pat_old_bracket, "\\3", ln)
      mm <- month_num(mon)
      yy <- suppressWarnings(as.integer(yy_txt))
      
      if (is.na(yy)) {
        yy <- if (!is.na(mm) && mm >= 8) yrs$start_year else yrs$end_year
      }
      
      current_date <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
      next
    }
    
    if (grepl(date_pat_new_plain, ln) && !grepl("^\\d{1,2}[:.]\\d{2}\\s+", ln)) {
      mon <- sub(date_pat_new_plain, "\\1", ln)
      dd  <- as.integer(sub(date_pat_new_plain, "\\2", ln))
      yy_txt <- sub(date_pat_new_plain, "\\3", ln)
      mm <- month_num(mon)
      yy <- suppressWarnings(as.integer(yy_txt))
      
      if (is.na(yy)) {
        yy <- if (!is.na(mm) && mm >= 8) yrs$start_year else yrs$end_year
      }
      
      current_date <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
      next
    }
    
    if (is.na(current_date)) next
    
    # Historical source annotations. Cancelled/postponed/abandoned matches did
    # not take place and must not become fixtures or team names.
    if (grepl("\\[(cancelled|postponed|abandoned)\\]", ln, ignore.case = TRUE)) {
      next
    }
    
    # Awarded matches count using the score supplied by OpenFootball; remove
    # the annotation before parsing the teams and score.
    ln <- gsub("\\s*\\[awarded\\]\\s*$", "", ln, ignore.case = TRUE)
    ln <- trim(ln)
    
    # Strip a leading kick-off time once. This makes v-format parsing much
    # less fragile than embedding the time in every regular expression.
    ln_no_time <- sub("^\\d{1,2}[:.]\\d{2}\\s+", "", ln)
    
    # Modern OpenFootball format: Home v Away [metadata] score [metadata].
    # Parse every v-line here BEFORE the old score-in-the-middle fallback.
    if (grepl("\\s+v\\s+", ln_no_time)) {
      vpos <- regexpr("\\s+v\\s+", ln_no_time)
      home <- trim(substr(ln_no_time, 1, vpos[1] - 1))
      rhs  <- trim(substr(ln_no_time, vpos[1] + attr(vpos, "match.length"), nchar(ln_no_time)))
      
      # Penalty marker at the end. In some OpenFootball files this contains
      # only the shoot-out score; the actual match score is on the next line.
      # Store the shoot-out temporarily and wait for that actual score.
      # Penalty shoot-out and actual match score on the same line, e.g.
      # "Team B 4-3 pen. 1-1 a.e.t. (1-1, 0-1)".
      # Elo uses the actual match score, never the shoot-out score.
      pen_same_line <- "^(.*?)\\s+(\\d+)-(\\d+)\\s+pen\\.?\\s+(\\d+)-(\\d+)(?:\\s+a\\.e\\.t\\.)?(?:\\s+\\([^)]*\\))?\\s*$"
      
      if (grepl(pen_same_line, rhs, ignore.case = TRUE)) {
        away  <- trim(sub(pen_same_line, "\\1", rhs, ignore.case = TRUE))
        pen_h <- as.integer(sub(pen_same_line, "\\2", rhs, ignore.case = TRUE))
        pen_a <- as.integer(sub(pen_same_line, "\\3", rhs, ignore.case = TRUE))
        hg    <- as.integer(sub(pen_same_line, "\\4", rhs, ignore.case = TRUE))
        ag    <- as.integer(sub(pen_same_line, "\\5", rhs, ignore.case = TRUE))
        
        append_match(
          home,
          away,
          hg,
          ag,
          sprintf("%d-%d (p %d-%d)", hg, ag, pen_h, pen_a)
        )
        next
      }
      
      pen_tail <- "^(.*?)\\s+(\\d+)-(\\d+)\\s+pen\\.?\\s*$"
      
      if (grepl(pen_tail, rhs, ignore.case = TRUE)) {
        away <- trim(sub(pen_tail, "\\1", rhs, ignore.case = TRUE))
        pen_h <- as.integer(sub(pen_tail, "\\2", rhs, ignore.case = TRUE))
        pen_a <- as.integer(sub(pen_tail, "\\3", rhs, ignore.case = TRUE))
        
        pending_penalty <- list(
          home = home,
          away = away,
          pen_h = pen_h,
          pen_a = pen_a
        )
        next
      }
      
      # Score at the end with optional aggregate/leg information BEFORE it,
      # e.g. "Team B (0-1, 0-0) 1-1".
      score_after_meta <- "^(.*?)\\s+\\([^)]*\\)\\s+(\\d+)-(\\d+)\\s*$"
      
      if (grepl(score_after_meta, rhs)) {
        away <- trim(sub(score_after_meta, "\\1", rhs))
        hg <- as.integer(sub(score_after_meta, "\\2", rhs))
        ag <- as.integer(sub(score_after_meta, "\\3", rhs))
        append_match(home, away, hg, ag)
        next
      }
      
      # Normal completed match, optionally followed by half-time/leg metadata
      # or an a.e.t. marker.
      normal_score <- "^(.*?)\\s+(\\d+)-(\\d+)(?:\\s+a\\.e\\.t\\.)?(?:\\s+\\([^)]*\\))?\\s*$"
      
      if (grepl(normal_score, rhs, ignore.case = TRUE)) {
        away <- trim(sub(normal_score, "\\1", rhs, ignore.case = TRUE))
        hg <- as.integer(sub(normal_score, "\\2", rhs, ignore.case = TRUE))
        ag <- as.integer(sub(normal_score, "\\3", rhs, ignore.case = TRUE))
        append_match(home, away, hg, ag)
        next
      }
      
      # If a score-like token or source annotation remains, this is a format
      # we have not safely understood. Do not turn the remainder into a team.
      if (grepl("\\d+\\s*-\\s*\\d+|\\[[^]]+\\]", rhs)) {
        next
      }
      
      # Otherwise it is a genuine future fixture.
      append_match(home, rhs)
      next
    }
    
    # Penalties in old score-in-the-middle format.
    if (grepl(match_pat_pen, ln, ignore.case = TRUE)) {
      home  <- trim(sub(match_pat_pen, "\\1", ln, ignore.case = TRUE))
      pen_h <- as.integer(sub(match_pat_pen, "\\2", ln, ignore.case = TRUE))
      pen_a <- as.integer(sub(match_pat_pen, "\\3", ln, ignore.case = TRUE))
      hg    <- as.integer(sub(match_pat_pen, "\\4", ln, ignore.case = TRUE))
      ag    <- as.integer(sub(match_pat_pen, "\\5", ln, ignore.case = TRUE))
      away  <- trim(sub(match_pat_pen, "\\6", ln, ignore.case = TRUE))
      append_match(
        home,
        away,
        hg,
        ag,
        sprintf("%d-%d (p %d-%d)", hg, ag, pen_h, pen_a)
      )
      next
    }
    
    # Extra time in old score-in-the-middle format.
    if (grepl(match_pat_aet, ln, ignore.case = TRUE)) {
      home <- trim(sub(match_pat_aet, "\\1", ln, ignore.case = TRUE))
      hg   <- as.integer(sub(match_pat_aet, "\\2", ln, ignore.case = TRUE))
      ag   <- as.integer(sub(match_pat_aet, "\\3", ln, ignore.case = TRUE))
      away <- trim(sub(match_pat_aet, "\\4", ln, ignore.case = TRUE))
      append_match(home, away, hg, ag)
      next
    }
    
    # Old score-in-the-middle format.
    if (grepl("\\d+-\\d+", ln) && grepl(match_pat_old, ln)) {
      home <- trim(sub(match_pat_old, "\\1", ln))
      hg   <- as.integer(sub(match_pat_old, "\\2", ln))
      ag   <- as.integer(sub(match_pat_old, "\\3", ln))
      away <- trim(sub(match_pat_old, "\\4", ln))
      append_match(home, away, hg, ag)
      next
    }
  }
  
  if (!is.null(pending_penalty)) {
    stop(
      "Unresolved penalty shoot-out record at end of file.\n",
      "File: ", txt_path, "\n",
      "Home: ", pending_penalty$home, "\n",
      "Away: ", pending_penalty$away, "\n",
      "Penalty score: ", pending_penalty$pen_h, "-", pending_penalty$pen_a
    )
  }
  
  n <- length(out_date)
  
  if (n == 0) {
    return(data.frame(
      Season          = character(),
      Country         = character(),
      Competition     = character(),
      CompetitionType = character(),
      Tier            = integer(),
      League          = character(),
      Date            = character(),
      Home            = character(),
      Away            = character(),
      Result          = character(),
      Score           = character(),
      Source          = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  out <- data.frame(
    Season          = rep(season_str, n),
    Country         = rep(country, n),
    Competition     = rep(competition_id, n),
    CompetitionType = rep(competition_type, n),
    Tier            = rep(as.integer(tier), n),
    League          = rep(league_label, n),
    Date            = out_date,
    Home            = out_home,
    Away            = out_away,
    Result          = out_res,
    Score           = out_score,
    Source          = rep(source, n),
    stringsAsFactors = FALSE
  )
  
  # Hard safety check: source annotations or whole match fragments must never
  # survive as team names. Stop immediately rather than contaminating Elo.
  bad_team <- grepl("\\[[^]]+\\]|\\s+v\\s+|^\\s*\\(", out$Home, ignore.case = TRUE) |
    grepl("\\[[^]]+\\]|\\s+v\\s+|^\\s*\\(", out$Away, ignore.case = TRUE)
  
  if (any(bad_team)) {
    bad <- out[bad_team, c("Season", "Country", "Competition", "Date", "Home", "Away", "Result", "Score")]
    cat("\\nInvalid parsed team names detected in:\\n", txt_path, "\\n", sep = "")
    print(bad)
    stop("Parser safety check failed: source annotation/match fragment found in team name.")
  }
  
  out
}

# -----------------------------
# Competition configuration
# -----------------------------

english_competition_files <- data.frame(
  file = c(
    "1-premierleague.txt",
    "2-division1.txt",
    "2-championship.txt",
    "3-division2.txt",
    "3-league1.txt",
    "4-division3.txt",
    "4-league2.txt",
    "5-nationalleague.txt"
  ),
  source_folder = rep("england", 8),
  country = rep("England", 8),
  competition = c(
    "premier_league",
    "championship",
    "championship",
    "league_one",
    "league_one",
    "league_two",
    "league_two",
    "national_league"
  ),
  competition_type = rep("league", 8),
  league = c(
    "Premier League",
    "Division 1",
    "Championship",
    "Division 2",
    "League 1",
    "Division 3",
    "League 2",
    "National League"
  ),
  tier = c(1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L),
  source = rep("openfootball", 8),
  stringsAsFactors = FALSE
)

english_competition_from_league <- function(x) {
  lx <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(lx))
  out[grepl("^premier league$", lx)] <- "premier_league"
  out[is.na(out) & grepl("^division\\s*1$|^championship$", lx)] <- "championship"
  out[is.na(out) & grepl("^division\\s*2$|^league\\s*1$|^league one$", lx)] <- "league_one"
  out[is.na(out) & grepl("^division\\s*3$|^league\\s*2$|^league two$", lx)] <- "league_two"
  out[is.na(out) & grepl("^national league$", lx)] <- "national_league"
  out
}

english_tier_from_league <- function(x) {
  comp <- english_competition_from_league(x)
  out <- rep(NA_integer_, length(comp))
  out[comp == "premier_league"] <- 1L
  out[comp == "championship"] <- 2L
  out[comp == "league_one"] <- 3L
  out[comp == "league_two"] <- 4L
  out[comp == "national_league"] <- 5L
  out
}

season_folder_from_start_year <- function(start_year) {
  paste0(
    as.integer(start_year),
    "-",
    sprintf("%02d", (as.integer(start_year) + 1L) %% 100L)
  )
}

season_start_year <- function(season_folder) {
  as.integer(substr(season_folder, 1, 4))
}

season_header_label <- function(season_folder) {
  gsub("-", "/", season_folder, fixed = TRUE)
}

make_jobs <- function(
    first_start_year,
    current_start_year,
    file,
    source_folder,
    country,
    competition,
    league,
    tier,
    competition_type = "league"
) {
  seasons <- vapply(
    first_start_year:current_start_year,
    season_folder_from_start_year,
    character(1)
  )
  
  data.frame(
    file = rep(file, length(seasons)),
    source_folder = rep(source_folder, length(seasons)),
    country = rep(country, length(seasons)),
    competition = rep(competition, length(seasons)),
    competition_type = rep(competition_type, length(seasons)),
    league = rep(league, length(seasons)),
    tier = rep(as.integer(tier), length(seasons)),
    source = rep("openfootball", length(seasons)),
    season_folder = seasons,
    stringsAsFactors = FALSE
  )
}

declared_match_count <- function(txt_path) {
  lines <- readLines(txt_path, warn = FALSE, encoding = "UTF-8")
  hit <- grep("^\\s*#\\s*Matches\\s+\\d+", lines, value = TRUE)
  
  if (length(hit) == 0) {
    return(NA_integer_)
  }
  
  suppressWarnings(
    as.integer(sub("^.*?([0-9]+)\\s*$", "\\1", hit[1]))
  )
}

# -----------------------------
# Seasons and parse jobs
# -----------------------------

season_folder <- Sys.getenv(
  "OPENFOOTBALL_SEASON",
  unset = current_openfootball_season()
)

expected_season_folder <- current_openfootball_season()
current_start_year <- season_start_year(season_folder)

cat("System date:", as.character(Sys.Date()), "\n")
cat("Repo dir:", repo_dir, "\n")
cat("OPENFOOTBALL_SEASON env var:", Sys.getenv("OPENFOOTBALL_SEASON", unset = "<not set>"), "\n")
cat("Expected OpenFootball season from current date:", expected_season_folder, "\n")
cat("Using OpenFootball current season folder:", season_folder, "\n")

if (!identical(season_folder, expected_season_folder)) {
  warning(
    "Using season folder '", season_folder,
    "' but current date implies '", expected_season_folder, "'. ",
    "This may be deliberate if OPENFOOTBALL_SEASON is set."
  )
}

# England: only refresh current season because historical England data is
# already in the combined CSV.
english_jobs <- english_competition_files
english_jobs$season_folder <- season_folder

# Spain
la_liga_jobs <- make_jobs(
  2012L, current_start_year,
  "1-liga.txt", "espana", "Spain",
  "la_liga", "La Liga", 1L
)

segunda_jobs <- make_jobs(
  2012L, current_start_year,
  "2-liga2.txt", "espana", "Spain",
  "segunda_division", "Segunda División", 2L
)

# Italy
serie_a_jobs <- make_jobs(
  2013L, current_start_year,
  "1-seriea.txt", "italy", "Italy",
  "serie_a", "Serie A", 1L
)

serie_b_jobs <- make_jobs(
  2013L, current_start_year,
  "2-serieb.txt", "italy", "Italy",
  "serie_b", "Serie B", 2L
)

# Germany
bundesliga_jobs <- make_jobs(
  2010L, current_start_year,
  "1-bundesliga.txt", "deutschland", "Germany",
  "bundesliga", "Bundesliga", 1L
)

bundesliga2_jobs <- make_jobs(
  2010L, current_start_year,
  "2-bundesliga2.txt", "deutschland", "Germany",
  "bundesliga_2", "2. Bundesliga", 2L
)

# France
ligue1_jobs <- make_jobs(
  2014L, current_start_year,
  "1-ligue1.txt", "france", "France",
  "ligue_1", "Ligue 1", 1L
)

ligue2_jobs <- make_jobs(
  2014L, current_start_year,
  "2-ligue2.txt", "france", "France",
  "ligue_2", "Ligue 2", 2L
)

# UEFA Champions League.
# Historical files are available from 2011/12 onward. The current season will
# parse automatically as soon as OpenFootball publishes cl.txt.
champions_league_jobs <- make_jobs(
  2011L, current_start_year,
  "cl.txt", "champions-league", "Europe",
  "champions_league", "Champions League", NA_integer_,
  competition_type = "continental"
)

parse_jobs <- rbind(
  english_jobs,
  la_liga_jobs,
  segunda_jobs,
  serie_a_jobs,
  serie_b_jobs,
  bundesliga_jobs,
  bundesliga2_jobs,
  ligue1_jobs,
  ligue2_jobs,
  champions_league_jobs
)

# -----------------------------
# Parse configured files
# -----------------------------

all_df_list <- list()
k <- 1L

for (i in seq_len(nrow(parse_jobs))) {
  cfg <- parse_jobs[i, ]
  
  season_dir <- file.path(
    source_root_dir,
    cfg$source_folder,
    cfg$season_folder
  )
  
  fpath <- file.path(season_dir, cfg$file)
  
  if (!file.exists(fpath)) {
    next
  }
  
  tmp <- tryCatch(
    parse_comp_file(
      txt_path = fpath,
      country = cfg$country,
      competition_id = cfg$competition,
      competition_type = cfg$competition_type,
      league_label = cfg$league,
      tier = cfg$tier,
      source = cfg$source
    ),
    error = function(e) {
      message("Skipping parse error: ", fpath, " | ", conditionMessage(e))
      NULL
    }
  )
  
  if (!is.null(tmp) && nrow(tmp) > 0) {
    declared_matches <- declared_match_count(fpath)
    
    # OpenFootball's declared match count can include fixtures marked
    # cancelled, postponed or abandoned. We deliberately do not import
    # those placeholder rows.
    if (!is.na(declared_matches) && nrow(tmp) != declared_matches) {
      
      source_lines <- readLines(
        fpath,
        warn = FALSE,
        encoding = "UTF-8"
      )
      
      ignored_annotation_rows <- sum(
        grepl(
          "\\s+v\\s+.*\\[(cancelled|postponed|abandoned)\\]\\s*$",
          source_lines,
          ignore.case = TRUE
        )
      )
      
      unexplained_missing <- declared_matches - nrow(tmp)
      
      if (
        unexplained_missing < 0 ||
        unexplained_missing > ignored_annotation_rows
      ) {
        stop(
          "Parsed row count does not match source-declared match count.\n",
          "File: ", fpath, "\n",
          "Competition: ", cfg$competition, "\n",
          "Parsed rows: ", nrow(tmp), "\n",
          "Declared matches: ", declared_matches, "\n",
          "Ignored cancelled/postponed/abandoned rows: ",
          ignored_annotation_rows
        )
      }
      
      cat(
        "Note: ",
        declared_matches - nrow(tmp),
        " cancelled/postponed/abandoned fixture(s) deliberately excluded.\n",
        sep = ""
      )
    }
    
    cat(
      "Parsed ", cfg$source_folder, "/", cfg$season_folder, "/", cfg$file,
      " | Competition: ", cfg$competition,
      " | Rows: ", nrow(tmp),
      " | Date range: ", min(tmp$Date), " to ", max(tmp$Date),
      "\n",
      sep = ""
    )
    
    all_df_list[[k]] <- tmp
    k <- k + 1L
  } else {
    cat(
      "Parsed ", cfg$source_folder, "/", cfg$season_folder, "/", cfg$file,
      " | Competition: ", cfg$competition,
      " | Rows: 0\n",
      sep = ""
    )
  }
}

if (length(all_df_list) == 0) {
  stop("No OpenFootball data parsed.")
}

parsed_df <- do.call(rbind, all_df_list)

required_cols <- c(
  "Season",
  "Country",
  "Competition",
  "CompetitionType",
  "Tier",
  "League",
  "Date",
  "Home",
  "Away",
  "Result",
  "Score",
  "Source"
)

parsed_df <- parsed_df[, required_cols]
parsed_df$Date <- as.character(parsed_df$Date)

cat(
  "Parsed competitions:",
  paste(sort(unique(parsed_df$Competition)), collapse = ", "),
  "\n"
)

cat(
  "Parsed rows:",
  nrow(parsed_df),
  "\n"
)

# -----------------------------
# Merge parsed competition-seasons into combined CSV
# -----------------------------

make_key <- function(season, country, competition) {
  paste(season, country, competition, sep = "||")
}

parsed_keys <- unique(
  make_key(
    parsed_df$Season,
    parsed_df$Country,
    parsed_df$Competition
  )
)

if (file.exists(out_file)) {
  old_df <- read.csv(out_file, stringsAsFactors = FALSE)
  
  # Backwards-compatible migration for the original English-only CSV.
  if (!"Country" %in% names(old_df)) old_df$Country <- "England"
  
  if (!"Competition" %in% names(old_df)) {
    old_df$Competition <- english_competition_from_league(old_df$League)
  }
  
  if (!"CompetitionType" %in% names(old_df)) {
    old_df$CompetitionType <- "league"
  }
  
  if (!"Tier" %in% names(old_df)) {
    old_df$Tier <- english_tier_from_league(old_df$League)
  }
  
  if (!"Source" %in% names(old_df)) {
    old_df$Source <- "openfootball"
  }
  
  missing_cols <- setdiff(required_cols, names(old_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Existing combined CSV is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  old_df <- old_df[, required_cols]
  old_df$Date <- as.character(old_df$Date)
  
  old_keys <- make_key(
    old_df$Season,
    old_df$Country,
    old_df$Competition
  )
  
  rows_replaced <- sum(old_keys %in% parsed_keys)
  
  cat("Existing combined CSV:", out_file, "\n")
  cat("Existing rows replaced by freshly parsed competition-seasons:", rows_replaced, "\n")
  
  old_keep <- old_df[!(old_keys %in% parsed_keys), ]
  all_df <- rbind(old_keep, parsed_df)
} else {
  cat("No existing combined CSV found. Creating a new one.\n")
  all_df <- parsed_df
}

# -----------------------------
# Duplicate protection
# -----------------------------

match_key <- paste(
  all_df$Season,
  all_df$Country,
  all_df$Competition,
  all_df$Date,
  all_df$Home,
  all_df$Away,
  sep = "||"
)

if (anyDuplicated(match_key)) {
  dup_rows <- all_df[
    duplicated(match_key) | duplicated(match_key, fromLast = TRUE),
  ]
  
  cat("\nDuplicate match keys detected:\n")
  print(
    dup_rows[
      order(
        dup_rows$Season,
        dup_rows$Competition,
        dup_rows$Date,
        dup_rows$Home,
        dup_rows$Away
      ),
    ]
  )
  
  stop("Duplicate match records detected. Refusing to write combined CSV.")
}

# -----------------------------
# Write
# -----------------------------

all_df <- all_df[, required_cols]

all_df <- all_df[
  order(
    all_df$Date,
    all_df$Country,
    all_df$Competition,
    all_df$Home,
    all_df$Away
  ),
]

write.csv(
  all_df,
  out_file,
  row.names = FALSE
)

cat("\nWrote:", out_file, "\n")
cat("Total rows:", nrow(all_df), "\n")

cat(
  "Countries:",
  paste(sort(unique(all_df$Country)), collapse = ", "),
  "\n"
)

cat(
  "Competitions:",
  paste(sort(unique(all_df$Competition)), collapse = ", "),
  "\n"
)

competition_summary <- aggregate(
  Date ~ Country + Competition,
  data = all_df,
  FUN = length
)

names(competition_summary)[names(competition_summary) == "Date"] <- "Rows"

cat("\nCompetition row summary:\n")
print(
  competition_summary[
    order(competition_summary$Country, competition_summary$Competition),
  ]
)
