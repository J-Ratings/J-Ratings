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
  "openfootball",
  "england"
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
  "england_leagues_1_to_4_all_seasons.csv"
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

parse_comp_file <- function(txt_path, league_label) {
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
    
    # New "v" format
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
      Season = character(),
      League = character(),
      Date   = character(),
      Home   = character(),
      Away   = character(),
      Result = character(),
      Score  = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    Season = rep(season_str, n),
    League = rep(league_label, n),
    Date   = out_date,
    Home   = out_home,
    Away   = out_away,
    Result = out_res,
    Score  = out_score,
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# League file map
# -----------------------------

file_to_league <- c(
  "1-premierleague.txt" = "Premier League",
  "2-division1.txt"     = "Division 1",
  "2-championship.txt"  = "Championship",
  "3-division2.txt"     = "Division 2",
  "3-league1.txt"       = "League 1",
  "4-division3.txt"     = "Division 3",
  "4-league2.txt"       = "League 2"
)

candidate_files <- names(file_to_league)

# -----------------------------
# Current season
# -----------------------------

season_folder <- Sys.getenv(
  "OPENFOOTBALL_SEASON",
  unset = current_openfootball_season()
)

expected_season_folder <- current_openfootball_season()

cat("System date:", as.character(Sys.Date()), "\n")
cat("Repo dir:", repo_dir, "\n")
cat("OPENFOOTBALL_SEASON env var:", Sys.getenv("OPENFOOTBALL_SEASON", unset = "<not set>"), "\n")
cat("Expected OpenFootball season from current date:", expected_season_folder, "\n")
cat("Using OpenFootball season folder:", season_folder, "\n")

if (!identical(season_folder, expected_season_folder)) {
  warning(
    "Using season folder '", season_folder,
    "' but current date implies '", expected_season_folder, "'. ",
    "This may be deliberate if OPENFOOTBALL_SEASON is set."
  )
}

season_dir <- file.path(source_root_dir, season_folder)

if (!dir.exists(season_dir)) {
  stop("Missing OpenFootball season folder: ", season_dir)
}

cat("Source folder:", season_dir, "\n")

# -----------------------------
# Parse current season only
# -----------------------------

all_df_list <- list()
k <- 1L

for (cf in candidate_files) {
  fpath <- file.path(season_dir, cf)
  if (!file.exists(fpath)) next
  
  league_label <- unname(file_to_league[cf])
  
  tmp <- tryCatch(
    parse_comp_file(fpath, league_label),
    error = function(e) {
      message("Skipping parse error: ", fpath, " | ", conditionMessage(e))
      NULL
    }
  )
  
  if (!is.null(tmp) && nrow(tmp) > 0) {
    cat(
      "Parsed ", cf,
      " | League: ", league_label,
      " | Rows: ", nrow(tmp),
      " | Date range: ", min(tmp$Date), " to ", max(tmp$Date),
      "\n",
      sep = ""
    )
    
    all_df_list[[k]] <- tmp
    k <- k + 1L
  } else {
    cat("Parsed ", cf, " | League: ", league_label, " | Rows: 0\n", sep = "")
  }
}

if (length(all_df_list) == 0) {
  stop("No current-season data parsed. Check season folder: ", season_dir)
}

current_df <- do.call(rbind, all_df_list)
current_df <- current_df[, c("Season", "League", "Date", "Home", "Away", "Result", "Score")]

current_seasons <- unique(current_df$Season)

if (length(current_seasons) != 1L) {
  stop("Expected exactly one parsed season, got: ", paste(current_seasons, collapse = ", "))
}

current_season <- current_seasons[[1]]
current_max_date <- max(as.Date(current_df$Date), na.rm = TRUE)

cat("Parsed current season:", current_season, "\n")
cat("Current-season rows parsed:", nrow(current_df), "\n")
cat("Current-season latest parsed date:", as.character(current_max_date), "\n")

# -----------------------------
# Hard sanity checks for the completed 2025/26 season
# -----------------------------

if (current_season == "2025/26") {
  pl_rows <- current_df[current_df$League == "Premier League", ]
  
  cat("Premier League 2025/26 parsed rows:", nrow(pl_rows), "\n")
  
  if (nrow(pl_rows) != 380L) {
    stop(
      "Premier League 2025/26 should have 380 parsed rows, but parser found ",
      nrow(pl_rows),
      ". Refusing to continue."
    )
  }
  
  pl_max_date <- max(as.Date(pl_rows$Date), na.rm = TRUE)
  
  if (pl_max_date < as.Date("2026-05-24")) {
    stop(
      "Premier League 2025/26 does not parse through 2026-05-24. ",
      "Latest parsed Premier League date: ", pl_max_date,
      ". Refusing to continue."
    )
  }
}

# -----------------------------
# Merge current season into combined CSV
# -----------------------------

required_cols <- c("Season", "League", "Date", "Home", "Away", "Result", "Score")

current_df$Date <- as.character(current_df$Date)

new_current_max_date <- max(as.Date(current_df$Date), na.rm = TRUE)
new_current_rows <- nrow(current_df)

cat("New parsed current-season max date:", as.character(new_current_max_date), "\n")
cat("New parsed current-season rows:", new_current_rows, "\n")

if (file.exists(out_file)) {
  old_df <- read.csv(out_file, stringsAsFactors = FALSE)
  
  missing_cols <- setdiff(required_cols, names(old_df))
  if (length(missing_cols) > 0) {
    stop("Existing combined CSV is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  old_df <- old_df[, required_cols]
  old_df$Date <- as.character(old_df$Date)
  
  old_current_df <- old_df[old_df$Season == current_season, ]
  old_other_df <- old_df[old_df$Season != current_season, ]
  
  old_current_rows <- nrow(old_current_df)
  old_current_max_date <- if (old_current_rows > 0) {
    max(as.Date(old_current_df$Date), na.rm = TRUE)
  } else {
    as.Date(NA)
  }
  
  cat("Existing combined CSV:", out_file, "\n")
  cat("Old current-season rows:", old_current_rows, "\n")
  cat("Old current-season max date:", as.character(old_current_max_date), "\n")
  
  if (
    old_current_rows > 0 &&
    !is.na(old_current_max_date) &&
    !is.na(new_current_max_date) &&
    new_current_max_date < old_current_max_date
  ) {
    stop(
      "Refusing to overwrite current season with older data.\n",
      "Season: ", current_season, "\n",
      "Old max date: ", old_current_max_date, "\n",
      "New max date: ", new_current_max_date, "\n",
      "This usually means the parser has missed rows or the downloaded source is incomplete."
    )
  }
  
  if (
    old_current_rows > 0 &&
    new_current_rows < old_current_rows &&
    !is.na(old_current_max_date) &&
    !is.na(new_current_max_date) &&
    new_current_max_date <= old_current_max_date
  ) {
    stop(
      "Refusing to overwrite current season with fewer rows.\n",
      "Season: ", current_season, "\n",
      "Old rows: ", old_current_rows, "\n",
      "New rows: ", new_current_rows, "\n",
      "Old max date: ", old_current_max_date, "\n",
      "New max date: ", new_current_max_date, "\n"
    )
  }
  
  cat("Rows removed for ", current_season, ": ", old_current_rows, "\n", sep = "")
  cat("Rows added for ", current_season, ": ", new_current_rows, "\n", sep = "")
  
  all_df <- rbind(old_other_df, current_df)
} else {
  cat("No existing combined CSV found. Creating a new one.\n")
  all_df <- current_df
}

all_df <- all_df[, required_cols]
all_df <- all_df[order(all_df$Date, all_df$League, all_df$Home, all_df$Away), ]

write.csv(all_df, out_file, row.names = FALSE)

cat("\nWrote:", out_file, "\n")
cat("Total rows:", nrow(all_df), "\n")
cat("Current season:", current_season, "\n")
cat("Current-season rows:", nrow(current_df), "\n")
cat("Current-season latest date:", as.character(max(as.Date(current_df$Date), na.rm = TRUE)), "\n")
cat("Leagues in file:", paste(sort(unique(all_df$League)), collapse = ", "), "\n")