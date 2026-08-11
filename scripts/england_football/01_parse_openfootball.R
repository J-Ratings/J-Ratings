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
  
  # Supports:
  #   [Sat Aug/15]
  #   [Sat Aug 15]
  #   Sat Aug/15
  #   Sat Aug 15
  #   Fri Aug 15 2025
  date_pat_old_bracket <- "^\\s*\\[[A-Za-z]{3}\\s+([A-Za-z]{3})(?:/|\\s+)(\\d{1,2})(?:\\s+(\\d{4}))?\\]\\s*$"
  date_pat_new_plain   <- "^\\s*[A-Za-z]{3}\\s+([A-Za-z]{3})(?:/|\\s+)(\\d{1,2})(?:\\s+(\\d{4}))?\\s*$"
  
  # Supports:
  #   20.00  Team A v Team B 2-1
  #   20:00  Team A v Team B 2-1
  #          Team A v Team B 2-1
  #   Team A 2-1 Team B
  time_pat <- "(?:\\d{1,2}[:.]\\d{2}\\s+)?"
  
  match_pat_old <- paste0(
    "^\\s*", time_pat,
    "(.+?)\\s+(\\d+)-(\\d+)(?:\\s+\\([^)]*\\))?\\s+(.+?)\\s*$"
  )
  
  match_pat_new_v <- paste0(
    "^\\s*", time_pat,
    "(.+?)\\s+v\\s+(.+?)\\s+(\\d+)-(\\d+)(?:\\s+\\([^)]*\\))?\\s*$"
  )
  
  fixture_pat_v <- paste0(
    "^\\s*", time_pat,
    "(.+?)\\s+v\\s+(.+?)\\s*$"
  )
  
  match_pat_pen <- paste0(
    "^\\s*", time_pat,
    "(.+?)\\s+(\\d+)-(\\d+)\\s+pen\\.?\\s+(\\d+)-(\\d+)(?:\\s+a\\.e\\.t\\.)?(?:\\s+\\([^)]*\\))?\\s+(.+?)\\s*$"
  )
  
  match_pat_aet <- paste0(
    "^\\s*", time_pat,
    "(.+?)\\s+(\\d+)-(\\d+)\\s+a\\.e\\.t\\.(?:\\s+\\([^)]*\\))?\\s+(.+?)\\s*$"
  )
  
  for (ln in lines) {
    ln <- gsub("\t", " ", ln)
    ln <- gsub("\u00a0", " ", ln, fixed = TRUE)
    
    if (grepl("^\\s*#", ln)) next
    if (grepl("^\\s*==", ln)) next
    if (grepl("Matchday", ln, ignore.case = TRUE)) next
    if (trim(ln) == "") next
    
    # Old/new bracket date style
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
    
    # New plain date style
    # Prevent timed match rows like "20:00 Team A..." from being read as dates.
    if (grepl(date_pat_new_plain, ln) && !grepl("^\\s*\\d{1,2}[:.]\\d{2}\\s+", ln)) {
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
    
    # Penalties
    if (grepl(match_pat_pen, ln, ignore.case = TRUE)) {
      home  <- trim(sub(match_pat_pen, "\\1", ln, ignore.case = TRUE))
      pen_h <- as.integer(sub(match_pat_pen, "\\2", ln, ignore.case = TRUE))
      pen_a <- as.integer(sub(match_pat_pen, "\\3", ln, ignore.case = TRUE))
      hg    <- as.integer(sub(match_pat_pen, "\\4", ln, ignore.case = TRUE))
      ag    <- as.integer(sub(match_pat_pen, "\\5", ln, ignore.case = TRUE))
      away  <- trim(sub(match_pat_pen, "\\6", ln, ignore.case = TRUE))
      
      out_season <- c(out_season, season_str)
      out_date   <- c(out_date, format(current_date, "%Y-%m-%d"))
      out_home   <- c(out_home, home)
      out_away   <- c(out_away, away)
      out_res    <- c(out_res, result_code(pen_h, pen_a))
      out_score  <- c(out_score, sprintf("%d-%d (p %d-%d)", hg, ag, pen_h, pen_a))
      next
    }
    
    # Extra time, no penalties
    if (grepl(match_pat_aet, ln, ignore.case = TRUE)) {
      home <- trim(sub(match_pat_aet, "\\1", ln, ignore.case = TRUE))
      hg   <- as.integer(sub(match_pat_aet, "\\2", ln, ignore.case = TRUE))
      ag   <- as.integer(sub(match_pat_aet, "\\3", ln, ignore.case = TRUE))
      away <- trim(sub(match_pat_aet, "\\4", ln, ignore.case = TRUE))
      
      out_season <- c(out_season, season_str)
      out_date   <- c(out_date, format(current_date, "%Y-%m-%d"))
      out_home   <- c(out_home, home)
      out_away   <- c(out_away, away)
      out_res    <- c(out_res, result_code(hg, ag))
      out_score  <- c(out_score, sprintf("%d-%d", hg, ag))
      next
    }
    
    # New "v" format - completed match
    if (grepl(match_pat_new_v, ln)) {
      home <- trim(sub(match_pat_new_v, "\\1", ln))
      away <- trim(sub(match_pat_new_v, "\\2", ln))
      hg   <- as.integer(sub(match_pat_new_v, "\\3", ln))
      ag   <- as.integer(sub(match_pat_new_v, "\\4", ln))
      
      out_season <- c(out_season, season_str)
      out_date   <- c(out_date, format(current_date, "%Y-%m-%d"))
      out_home   <- c(out_home, home)
      out_away   <- c(out_away, away)
      out_res    <- c(out_res, result_code(hg, ag))
      out_score  <- c(out_score, sprintf("%d-%d", hg, ag))
      next
    }
    
    # New "v" format - unplayed fixture
    if (grepl(fixture_pat_v, ln)) {
      home <- trim(sub(fixture_pat_v, "\\1", ln))
      away <- trim(sub(fixture_pat_v, "\\2", ln))
      
      out_season <- c(out_season, season_str)
      out_date   <- c(out_date, format(current_date, "%Y-%m-%d"))
      out_home   <- c(out_home, home)
      out_away   <- c(out_away, away)
      out_res    <- c(out_res, "")
      out_score  <- c(out_score, "")
      next
    }
    
    # Old format
    if (grepl("\\d+-\\d+", ln) && grepl(match_pat_old, ln)) {
      home <- trim(sub(match_pat_old, "\\1", ln))
      hg   <- as.integer(sub(match_pat_old, "\\2", ln))
      ag   <- as.integer(sub(match_pat_old, "\\3", ln))
      away <- trim(sub(match_pat_old, "\\4", ln))
      
      out_season <- c(out_season, season_str)
      out_date   <- c(out_date, format(current_date, "%Y-%m-%d"))
      out_home   <- c(out_home, home)
      out_away   <- c(out_away, away)
      out_res    <- c(out_res, result_code(hg, ag))
      out_score  <- c(out_score, sprintf("%d-%d", hg, ag))
      next
    }
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
  
  data.frame(
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

# England continues to refresh only the current season because the historical
# English data is already present in the combined CSV.
english_jobs <- english_competition_files
english_jobs$season_folder <- season_folder

# First new competition: La Liga (Spain division 1).
# OpenFootball's España repository contains La Liga from 2012/13 onwards.
la_liga_season_folders <- vapply(
  2012L:current_start_year,
  season_folder_from_start_year,
  character(1)
)

la_liga_jobs <- data.frame(
  file = rep("1-liga.txt", length(la_liga_season_folders)),
  source_folder = rep("espana", length(la_liga_season_folders)),
  country = rep("Spain", length(la_liga_season_folders)),
  competition = rep("la_liga", length(la_liga_season_folders)),
  competition_type = rep("league", length(la_liga_season_folders)),
  league = rep("La Liga", length(la_liga_season_folders)),
  tier = rep(1L, length(la_liga_season_folders)),
  source = rep("openfootball", length(la_liga_season_folders)),
  season_folder = la_liga_season_folders,
  stringsAsFactors = FALSE
)

parse_jobs <- rbind(
  english_jobs,
  la_liga_jobs
)

cat(
  "La Liga seasons to parse:",
  paste(la_liga_season_folders, collapse = ", "),
  "\n"
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

# -----------------------------
# Sanity checks
# -----------------------------

# Keep the existing completed Premier League 2025/26 check.
pl_2526 <- parsed_df[
  parsed_df$Competition == "premier_league" &
    parsed_df$Season == "2025/26",
]

if (nrow(pl_2526) > 0) {
  cat("Premier League 2025/26 parsed rows:", nrow(pl_2526), "\n")
  
  if (nrow(pl_2526) != 380L) {
    stop(
      "Premier League 2025/26 should have 380 parsed rows, but parser found ",
      nrow(pl_2526),
      ". Refusing to continue."
    )
  }
  
  pl_max_date <- max(as.Date(pl_2526$Date), na.rm = TRUE)
  
  if (pl_max_date < as.Date("2026-05-24")) {
    stop(
      "Premier League 2025/26 does not parse through 2026-05-24. ",
      "Latest parsed Premier League date: ", pl_max_date,
      ". Refusing to continue."
    )
  }
}

# Every completed La Liga season in our backfill should contain 380 fixtures.
current_season_label <- season_header_label(season_folder)

la_liga_completed <- parsed_df[
  parsed_df$Competition == "la_liga" &
    parsed_df$Season != current_season_label,
]

if (nrow(la_liga_completed) > 0) {
  la_liga_counts <- aggregate(
    Date ~ Season,
    data = la_liga_completed,
    FUN = length
  )
  
  names(la_liga_counts)[names(la_liga_counts) == "Date"] <- "Rows"
  
  bad_la_liga_seasons <- la_liga_counts[
    la_liga_counts$Rows != 380L,
  ]
  
  if (nrow(bad_la_liga_seasons) > 0) {
    stop(
      "One or more completed La Liga seasons did not parse to 380 rows:\n",
      paste(
        paste0(
          bad_la_liga_seasons$Season,
          " = ",
          bad_la_liga_seasons$Rows
        ),
        collapse = "\n"
      )
    )
  }
}

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

# Important: replacement is now keyed by Season + Country + Competition.
# This means we can refresh England's current season and backfill La Liga
# without deleting unrelated competitions from the same season.

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
  
  # Backwards-compatible migration for the existing English-only CSV.
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

# A match/fixture should only appear once within a competition.
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
  dup_rows <- all_df[duplicated(match_key) | duplicated(match_key, fromLast = TRUE), ]
  
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

la_liga_all <- all_df[all_df$Competition == "la_liga", ]

if (nrow(la_liga_all) > 0) {
  cat("La Liga rows:", nrow(la_liga_all), "\n")
  cat(
    "La Liga seasons:",
    paste(sort(unique(la_liga_all$Season)), collapse = ", "),
    "\n"
  )
  cat(
    "La Liga date range:",
    min(la_liga_all$Date),
    "to",
    max(la_liga_all$Date),
    "\n"
  )
}
