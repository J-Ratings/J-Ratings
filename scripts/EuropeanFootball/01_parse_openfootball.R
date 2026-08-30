options(stringsAsFactors = FALSE)
# Local completion sound only; GitHub Actions does not need beepr.
if (interactive()) {
  library(beepr)
}
# -----------------------------
# Paths
# -----------------------------

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

source_root_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "openfootball"
)

wikipedia_source_root_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "wikipedia"
)

schoch_source_root_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "schochastics"
)

schoch_results_file <- file.path(
  schoch_source_root_dir,
  "games.parquet"
)

combined_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Matches_Clean_Combined"
)

dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(
  combined_dir,
  "european_football_all_matches.csv"
)

# -----------------------------
# Helpers
# -----------------------------

declared_match_count <- function(txt_path) {
  lines <- readLines(
    txt_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  # OpenFootball files do not always expose a reliable declared
  # match count in a consistent format. If none can be identified,
  # skip this optional validation.
  candidates <- grep(
    "\\b[0-9]+\\s+matches\\b",
    lines,
    ignore.case = TRUE,
    value = TRUE
  )
  
  if (length(candidates) == 0L) {
    return(NA_integer_)
  }
  
  m <- regexpr(
    "[0-9]+(?=\\s+matches\\b)",
    candidates[1],
    ignore.case = TRUE,
    perl = TRUE
  )
  
  if (m[1] == -1L) {
    return(NA_integer_)
  }
  
  as.integer(regmatches(candidates[1], m))
}

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
    
    # Awarded matches count using the score supplied by OpenFootball.
    # The [awarded] marker can occur either at the end of the line or between
    # the away club and the supplied score, so remove it wherever it appears.
    ln <- gsub("\\s*\\[awarded\\]\\s*", " ", ln, ignore.case = TRUE)
    ln <- gsub("\\s+", " ", ln)
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
# Wikipedia UEFA parser
# -----------------------------

clean_wikipedia_text <- function(x) {
  if (length(x) == 0) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("\\[[0-9]+\\]", "", x)
  x <- gsub("\\s+", " ", x)
  trim(x)
}

parse_wikipedia_date <- function(x) {
  x <- clean_wikipedia_text(x)
  
  month_names <- paste(
    c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    ),
    collapse = "|"
  )
  
  pat <- paste0("(\\d{1,2}\\s+(?:", month_names, ")\\s+\\d{4})")
  m <- regexpr(pat, x, perl = TRUE)
  if (m[1] == -1) return(as.Date(NA))
  
  as.Date(regmatches(x, m), format = "%d %B %Y")
}

parse_wikipedia_competition_page <- function(
    html_path,
    season_folder,
    competition_id,
    league_label,
    country = "Europe",
    competition_type = "continental"
) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required. Install it once with install.packages('xml2').")
  }
  
  doc <- xml2::read_html(html_path, encoding = "UTF-8")
  season_str <- normalise_openfootball_season_label(season_folder)
  rows <- list()
  k <- 1L
  
  clean_team_name <- function(x) {
    x <- clean_wikipedia_text(x)
    # Wikipedia cup pages commonly append the league-system tier, e.g. "(3)".
    x <- gsub("\\s*\\([0-9]+\\)\\s*$", "", x, perl = TRUE)
    trim(x)
  }
  
  add_row <- function(date, home, away, score_text = "") {
    if (length(date) == 0 || is.na(date[1])) return(invisible(NULL))
    
    home <- clean_team_name(home)
    away <- clean_team_name(away)
    score_text <- clean_wikipedia_text(score_text)
    
    if (length(home) == 0 || length(away) == 0 || home[1] == "" || away[1] == "") {
      return(invisible(NULL))
    }
    
    home <- home[1]
    away <- away[1]
    score_text <- score_text[1]
    
    score_pat <- "([0-9]+)\\s*[\\-\u2013\u2014]\\s*([0-9]+)"
    
    if (grepl(score_pat, score_text, perl = TRUE)) {
      hg <- as.integer(sub(paste0("^.*?", score_pat, ".*$"), "\\1", score_text, perl = TRUE))
      ag <- as.integer(sub(paste0("^.*?", score_pat, ".*$"), "\\2", score_text, perl = TRUE))
      result <- result_code(hg, ag)
      score <- sprintf("%d-%d", hg, ag)
    } else {
      result <- ""
      score <- ""
    }
    
    rows[[k]] <<- data.frame(
      Season = season_str,
      Country = country,
      Competition = competition_id,
      CompetitionType = competition_type,
      Tier = NA_integer_,
      League = league_label,
      Date = format(date[1], "%Y-%m-%d"),
      Home = home,
      Away = away,
      Result = result,
      Score = score,
      Source = "wikipedia",
      stringsAsFactors = FALSE
    )
    k <<- k + 1L
    invisible(NULL)
  }
  
  # 1) Standard Wikipedia footballbox template.
  boxes <- xml2::xml_find_all(
    doc,
    "//*[contains(concat(' ', normalize-space(@class), ' '), ' footballbox ')]"
  )
  
  for (box in boxes) {
    home_node <- xml2::xml_find_first(
      box,
      ".//*[contains(concat(' ', normalize-space(@class), ' '), ' fhome ')]"
    )
    away_node <- xml2::xml_find_first(
      box,
      ".//*[contains(concat(' ', normalize-space(@class), ' '), ' faway ')]"
    )
    score_node <- xml2::xml_find_first(
      box,
      ".//*[contains(concat(' ', normalize-space(@class), ' '), ' fscore ')]"
    )
    date_node <- xml2::xml_find_first(
      box,
      ".//*[contains(concat(' ', normalize-space(@class), ' '), ' fdate ')]"
    )
    
    if (
      inherits(home_node, "xml_missing") ||
      inherits(away_node, "xml_missing") ||
      inherits(date_node, "xml_missing")
    ) next
    
    add_row(
      parse_wikipedia_date(xml2::xml_text(date_node)),
      xml2::xml_text(home_node),
      xml2::xml_text(away_node),
      if (inherits(score_node, "xml_missing")) "" else xml2::xml_text(score_node)
    )
  }
  
  # 2) Generic rendered Wikipedia result rows.
  #
  # Cup pages are inconsistent across seasons. Many use ordinary tables rather
  # than the footballbox class. A match row normally contains:
  #   date | home | score | away | ...
  # Some tables put the date on a row immediately above the matches, so retain
  # the most recent date found inside each table as a fallback.
  score_pat <- "^[[:space:]]*[0-9]+[[:space:]]*[\\-\u2013\u2014][[:space:]]*[0-9]+"
  tables <- xml2::xml_find_all(doc, "//table")
  
  for (tbl in tables) {
    trs <- xml2::xml_find_all(tbl, ".//tr")
    if (length(trs) == 0) next
    
    current_date <- as.Date(NA)
    
    for (tr in trs) {
      cells <- xml2::xml_find_all(tr, "./th|./td")
      vals <- clean_wikipedia_text(xml2::xml_text(cells))
      if (length(vals) == 0) next
      
      # Update the table-local date whenever a row contains one.
      row_dates <- lapply(vals, parse_wikipedia_date)
      date_hits <- which(!vapply(row_dates, function(x) is.na(x[1]), logical(1)))
      
      if (length(date_hits) > 0) {
        current_date <- row_dates[[date_hits[1]]][1]
      }
      
      score_hits <- which(grepl(score_pat, vals, perl = TRUE))
      if (length(score_hits) == 0) next
      
      # Prefer a score with a sensible cell on both sides.
      for (score_col in score_hits) {
        if (score_col <= 1L || score_col >= length(vals)) next
        
        home <- vals[score_col - 1L]
        away <- vals[score_col + 1L]
        
        # If the same row contains a date, use that; otherwise use the most
        # recent date heading encountered in this table.
        row_date <- as.Date(NA)
        if (length(date_hits) > 0) {
          row_date <- row_dates[[date_hits[1]]][1]
        } else if (!is.na(current_date)) {
          row_date <- current_date
        }
        
        if (is.na(row_date)) next
        
        add_row(row_date, home, away, vals[score_col])
        break
      }
    }
  }
  
  if (length(rows) == 0) {
    message("No Wikipedia match rows found yet in: ", html_path)
    return(data.frame(
      Season=character(), Country=character(), Competition=character(),
      CompetitionType=character(), Tier=integer(), League=character(),
      Date=character(), Home=character(), Away=character(), Result=character(),
      Score=character(), Source=character(), stringsAsFactors=FALSE
    ))
  }
  
  out <- do.call(rbind, rows)
  
  # The same match can be exposed more than once on a page.
  out$has_score <- out$Score != ""
  out <- out[order(out$Date, out$Home, out$Away, -out$has_score), , drop=FALSE]
  key <- paste(out$Date, out$Home, out$Away, sep="||")
  out <- out[!duplicated(key), , drop=FALSE]
  out$has_score <- NULL
  
  out
}

# -----------------------------
# Schochastics historical top-flight parser
# -----------------------------

read_schoch_parquet <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Schochastics source file is missing:\n",
      path,
      "\nRun 00_download_openfootball_current.R first."
    )
  }
  
  if (requireNamespace("nanoparquet", quietly = TRUE)) {
    return(as.data.frame(nanoparquet::read_parquet(path)))
  }
  
  if (requireNamespace("arrow", quietly = TRUE)) {
    return(as.data.frame(arrow::read_parquet(path)))
  }
  
  stop(
    "Reading Schochastics games.parquet requires either 'nanoparquet' ",
    "or 'arrow'. Install once with:\n",
    "  install.packages(\"nanoparquet\")"
  )
}

normalise_schoch_competition <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

schoch_country_key <- function(x) {
  x <- normalise_schoch_competition(x)
  
  # Small spelling/name variants that can occur in football datasets.
  x[x %in% c("czech republic", "czechia")] <- "czechia"
  x[x %in% c("turkiye", "turkey")] <- "turkey"
  
  x
}

schoch_season_start_year <- function(date) {
  d <- as.Date(date)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  
  # European leagues in this J-Ratings set use an autumn-to-spring season.
  # July-December belongs to the season starting that calendar year;
  # January-June belongs to the preceding start year.
  ifelse(m >= 7L, y, y - 1L)
}

schoch_season_label <- function(country_key, start_year, date) {
  d <- as.Date(date)
  
  # A few inaugural national leagues were short calendar-year competitions
  # played entirely in the first half of the year.
  if (country_key == "spain" && d >= as.Date("1929-01-01") && d <= as.Date("1929-06-30")) {
    return("1929")
  }
  
  if (country_key == "turkey" && d >= as.Date("1959-01-01") && d <= as.Date("1959-06-30")) {
    return("1959")
  }
  
  if (country_key == "ukraine" && d >= as.Date("1992-01-01") && d <= as.Date("1992-06-30")) {
    return("1992")
  }
  
  paste0(
    start_year,
    "/",
    sprintf("%02d", (start_year + 1L) %% 100L)
  )
}

schoch_league_label <- function(country_key, start_year, default_label) {
  # Preserve the most important historical competition-name changes while
  # keeping a stable Competition ID for Elo/filtering.
  if (country_key == "england" && start_year < 1992L) {
    return("First Division")
  }
  
  if (country_key == "france" && start_year < 2002L) {
    return("Division 1")
  }
  
  default_label
}

parse_schoch_historical_top_flights <- function(path) {
  raw <- read_schoch_parquet(path)
  
  required <- c(
    "home", "away", "date", "gh", "ga",
    "competition", "level"
  )
  
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop(
      "Schochastics parquet is missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
  
  raw$date <- as.Date(raw$date)
  raw$competition_key <- schoch_country_key(raw$competition)
  raw$level_key <- tolower(trimws(as.character(raw$level)))
  
  # Small country-name variants that may occur in historical datasets.
  raw$competition_key[raw$competition_key == "holland"] <- "netherlands"
  
  # Cutover = first season handled by J-Ratings' existing source.
  # Schoch is used only for seasons before this, so there is no source overlap.
  cfg <- data.frame(
    country_key = c(
      "england", "spain", "italy", "germany", "france",
      "portugal", "netherlands", "belgium", "austria", "turkey",
      "scotland", "switzerland", "greece", "czechia", "ukraine"
    ),
    Country = c(
      "England", "Spain", "Italy", "Germany", "France",
      "Portugal", "Netherlands", "Belgium", "Austria", "Turkey",
      "Scotland", "Switzerland", "Greece", "Czechia", "Ukraine"
    ),
    Competition = c(
      "premier_league", "la_liga", "serie_a", "bundesliga", "ligue_1",
      "primeira_liga", "eredivisie", "belgian_pro_league",
      "austrian_bundesliga", "super_lig", "scottish_premiership",
      "swiss_super_league", "super_league_greece",
      "czech_first_league", "ukrainian_premier_league"
    ),
    League = c(
      "Premier League", "La Liga", "Serie A", "Bundesliga", "Ligue 1",
      "Primeira Liga", "Eredivisie", "Belgian Pro League",
      "Austrian Bundesliga", "SÃ¼per Lig", "Scottish Premiership",
      "Swiss Super League", "Super League Greece",
      "Czech First League", "Ukrainian Premier League"
    ),
    FirstSeasonStartYear = c(
      1888L, 1928L, 1929L, 1963L, 1932L,
      1934L, 1956L, 1895L, 1911L, 1958L,
      1890L, 1897L, 1959L, 1993L, 1991L
    ),
    CutoverStartYear = c(
      1992L, 2012L, 2013L, 2010L, 2014L,
      2018L, 2018L, 2018L, 2010L, 2018L,
      2018L, 2014L, 2018L, 2018L, 2023L
    ),
    stringsAsFactors = FALSE
  )
  
  # Conservative policy agreed after the all-15 audit:
  #   PASS    -> import the whole season
  #   CHECK   -> drop the whole season
  #   MISSING -> naturally remains a gap
  #
  # A season is CHECK if it contains any missing team/score, self-match,
  # duplicate fixture/date key, conflicting score, team double-booking,
  # or an obvious placeholder-date pattern.
  
  audit_schoch_season <- function(z) {
    home <- trimws(as.character(z$home))
    away <- trimws(as.character(z$away))
    
    missing_date <- is.na(z$date)
    missing_team <- is.na(z$home) | is.na(z$away) |
      is.na(home) | is.na(away) | home == "" | away == ""
    missing_score <- is.na(z$gh) | is.na(z$ga)
    
    home_key <- normalise_schoch_competition(home)
    away_key <- normalise_schoch_competition(away)
    
    self_match <- !missing_team & home_key == away_key
    
    valid_fixture <- !missing_date & !missing_team
    fixture_key <- paste(
      z$date,
      home_key,
      away_key,
      sep = "|||"
    )
    
    fixture_tab <- table(fixture_key[valid_fixture])
    duplicate_fixture_keys <- sum(fixture_tab > 1L)
    
    score_txt <- ifelse(
      missing_score,
      NA_character_,
      paste(z$gh, z$ga, sep = "-")
    )
    
    conflicting_fixture_keys <- 0L
    if (any(valid_fixture)) {
      split_idx <- split(
        seq_len(nrow(z))[valid_fixture],
        fixture_key[valid_fixture]
      )
      
      conflicting_fixture_keys <- sum(vapply(
        split_idx,
        function(ii) {
          scores <- unique(score_txt[ii][!is.na(score_txt[ii])])
          length(scores) > 1L
        },
        logical(1)
      ))
    }
    
    valid_dates <- z$date[!missing_date]
    date_tab <- table(valid_dates)
    max_on_date <- if (length(date_tab)) max(date_tab) else 0L
    share_one_date <- if (nrow(z)) 100 * max_on_date / nrow(z) else NA_real_
    
    team_dates <- rbind(
      data.frame(date = z$date, team = home_key, stringsAsFactors = FALSE),
      data.frame(date = z$date, team = away_key, stringsAsFactors = FALSE)
    )
    
    team_dates <- team_dates[
      !is.na(team_dates$date) &
        !is.na(team_dates$team) &
        team_dates$team != "",
      ,
      drop = FALSE
    ]
    
    td_key <- paste(team_dates$date, team_dates$team, sep = "|||")
    td_tab <- table(td_key)
    team_double_booked <- sum(td_tab > 1L)
    
    reasons <- character()
    
    if (sum(missing_date) > 0L) {
      reasons <- c(reasons, "MISSING_DATES")
    }
    if (sum(missing_team) > 0L) {
      reasons <- c(reasons, "MISSING_TEAMS")
    }
    if (sum(missing_score) > 0L) {
      reasons <- c(reasons, "MISSING_SCORES")
    }
    if (sum(self_match, na.rm = TRUE) > 0L) {
      reasons <- c(reasons, "SELF_MATCH")
    }
    if (duplicate_fixture_keys > 0L) {
      reasons <- c(reasons, "DUPLICATE_FIXTURE_DATE")
    }
    if (conflicting_fixture_keys > 0L) {
      reasons <- c(reasons, "CONFLICTING_SCORE")
    }
    if (team_double_booked > 0L) {
      reasons <- c(reasons, "TEAM_DOUBLE_BOOKED")
    }
    if (nrow(z) >= 50L && length(unique(valid_dates)) < 5L) {
      reasons <- c(reasons, "TOO_FEW_UNIQUE_DATES")
    }
    if (
      nrow(z) >= 50L &&
      !is.na(share_one_date) &&
      share_one_date >= 20
    ) {
      reasons <- c(reasons, "PLACEHOLDER_DATE_CONCENTRATION")
    }
    
    list(
      Status = if (length(reasons)) "CHECK" else "PASS",
      Reason = if (length(reasons)) {
        paste(unique(reasons), collapse = ";")
      } else {
        ""
      },
      Matches = nrow(z),
      Teams = length(unique(c(
        home[!is.na(home) & home != ""],
        away[!is.na(away) & away != ""]
      ))),
      FirstDate = if (length(valid_dates)) min(valid_dates) else as.Date(NA),
      LastDate = if (length(valid_dates)) max(valid_dates) else as.Date(NA)
    )
  }
  
  out_list <- list()
  audit_list <- list()
  k <- 1L
  a <- 1L
  
  for (i in seq_len(nrow(cfg))) {
    c0 <- cfg[i, ]
    
    z_country <- raw[
      raw$level_key == "national" &
        raw$competition_key == c0$country_key,
      ,
      drop = FALSE
    ]
    
    if (nrow(z_country) == 0) {
      warning(
        "No Schochastics national top-flight rows found for ",
        c0$Country,
        "."
      )
      next
    }
    
    # Rows with no date cannot be assigned safely to a season, so they are
    # never imported. Dated rows are grouped into their inferred season.
    z_country$SeasonStartYear <- schoch_season_start_year(z_country$date)
    
    z_country <- z_country[
      !is.na(z_country$SeasonStartYear) &
        z_country$SeasonStartYear >= c0$FirstSeasonStartYear &
        z_country$SeasonStartYear < c0$CutoverStartYear,
      ,
      drop = FALSE
    ]
    
    if (nrow(z_country) == 0) next
    
    season_years <- sort(unique(z_country$SeasonStartYear))
    
    for (sy in season_years) {
      z <- z_country[
        z_country$SeasonStartYear == sy,
        ,
        drop = FALSE
      ]
      
      season_name <- schoch_season_label(
        c0$country_key,
        sy,
        z$date[which(!is.na(z$date))[1]]
      )
      
      qa <- audit_schoch_season(z)
      
      audit_list[[a]] <- data.frame(
        Country = c0$Country,
        Competition = c0$Competition,
        Season = season_name,
        SeasonStartYear = sy,
        Matches = qa$Matches,
        Teams = qa$Teams,
        FirstDate = as.character(qa$FirstDate),
        LastDate = as.character(qa$LastDate),
        Status = qa$Status,
        Reason = qa$Reason,
        stringsAsFactors = FALSE
      )
      a <- a + 1L
      
      # Entire suspect season is deliberately excluded.
      if (qa$Status != "PASS") next
      
      home_txt <- trimws(as.character(z$home))
      away_txt <- trimws(as.character(z$away))
      
      # PASS guarantees these are all complete, but keep this as a local
      # defensive filter as well.
      keep <- !is.na(z$date) &
        !is.na(z$gh) & !is.na(z$ga) &
        !is.na(z$home) & !is.na(z$away) &
        !is.na(home_txt) & !is.na(away_txt) &
        nzchar(home_txt) & nzchar(away_txt)
      
      z <- z[keep, , drop = FALSE]
      home_txt <- home_txt[keep]
      away_txt <- away_txt[keep]
      
      if (nrow(z) == 0) next
      
      league_label <- schoch_league_label(
        c0$country_key,
        sy,
        c0$League
      )
      
      hg <- as.integer(z$gh)
      ag <- as.integer(z$ga)
      
      out_list[[k]] <- data.frame(
        Season = rep(season_name, nrow(z)),
        Country = rep(c0$Country, nrow(z)),
        Competition = rep(c0$Competition, nrow(z)),
        CompetitionType = rep("league", nrow(z)),
        Tier = rep(1L, nrow(z)),
        League = rep(league_label, nrow(z)),
        Date = format(z$date, "%Y-%m-%d"),
        Home = home_txt,
        Away = away_txt,
        Result = ifelse(
          hg > ag, "1-0",
          ifelse(hg < ag, "0-1", "0.5-0.5")
        ),
        Score = sprintf("%d-%d", hg, ag),
        Source = rep("schochastics", nrow(z)),
        stringsAsFactors = FALSE
      )
      
      k <- k + 1L
    }
  }
  
  if (length(audit_list) == 0) {
    stop("No historical Schochastics seasons were available to audit.")
  }
  
  audit_df <- do.call(rbind, audit_list)
  audit_df <- audit_df[
    order(
      match(audit_df$Country, cfg$Country),
      audit_df$SeasonStartYear
    ),
    ,
    drop = FALSE
  ]
  
  cat("\nSchochastics historical season screening:\n")
  cat(
    "  PASS seasons imported: ",
    sum(audit_df$Status == "PASS"),
    "\n",
    sep = ""
  )
  cat(
    "  CHECK seasons excluded: ",
    sum(audit_df$Status == "CHECK"),
    "\n",
    sep = ""
  )
  
  excluded <- audit_df[audit_df$Status == "CHECK", , drop = FALSE]
  if (nrow(excluded)) {
    cat("\nExcluded Schochastics seasons:\n")
    print(
      excluded[, c("Country", "Season", "Matches", "Reason")],
      row.names = FALSE
    )
  }
  
  if (length(out_list) == 0) {
    stop("No PASS historical Schochastics rows were produced.")
  }
  
  out <- do.call(rbind, out_list)
  
  # Structural safety checks on the imported PASS rows.
  if (any(is.na(as.Date(out$Date)))) {
    stop("Schochastics parser produced an invalid date.")
  }
  
  if (
    any(is.na(out$Home)) ||
    any(is.na(out$Away)) ||
    any(out$Home == "", na.rm = TRUE) ||
    any(out$Away == "", na.rm = TRUE)
  ) {
    stop("Schochastics parser produced a blank or missing team name.")
  }
  
  if (any(out$Home == out$Away, na.rm = TRUE)) {
    stop("Schochastics PASS import unexpectedly contains a self-match.")
  }
  
  key <- paste(
    out$Season, out$Country, out$Competition,
    out$Date, out$Home, out$Away,
    sep = "||"
  )
  
  if (anyDuplicated(key)) {
    dup <- out[
      duplicated(key) | duplicated(key, fromLast = TRUE),
      ,
      drop = FALSE
    ]
    cat("\nDuplicate Schochastics PASS historical match keys:\n")
    print(dup)
    stop("Duplicate Schochastics PASS historical matches detected.")
  }
  
  out <- out[
    order(out$Date, out$Country, out$Home, out$Away),
    ,
    drop = FALSE
  ]
  
  cat("\nSchochastics historical top-flight import:\n")
  
  schoch_summary <- aggregate(
    Date ~ Country + Competition,
    data = out,
    FUN = length
  )
  names(schoch_summary)[3] <- "Rows"
  
  season_summary <- aggregate(
    SeasonStartYear ~ Country,
    data = audit_df[audit_df$Status == "PASS", , drop = FALSE],
    FUN = length
  )
  names(season_summary)[2] <- "PASS_Seasons"
  
  schoch_summary <- merge(
    schoch_summary,
    season_summary,
    by = "Country",
    all.x = TRUE,
    sort = FALSE
  )
  
  print(
    schoch_summary[order(match(schoch_summary$Country, cfg$Country)), ],
    row.names = FALSE
  )
  
  cat(
    "Schochastics historical rows imported: ",
    nrow(out),
    "\n",
    sep = ""
  )
  cat(
    "Schochastics imported date range: ",
    min(out$Date),
    " to ",
    max(out$Date),
    "\n",
    sep = ""
  )
  
  out
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

# Existing historical rows stay in the combined CSV. We only need their keys
# here so the new Wikipedia competitions backfill missing seasons once.
existing_keys <- character()
existing_counts <- integer()
names(existing_counts) <- character()

if (file.exists(out_file)) {
  existing_for_jobs <- read.csv(out_file, stringsAsFactors = FALSE)
  
  if (all(c("Season", "Country", "Competition") %in% names(existing_for_jobs))) {
    existing_row_keys <- paste(
      existing_for_jobs$Season,
      existing_for_jobs$Country,
      existing_for_jobs$Competition,
      sep = "||"
    )
    
    existing_keys <- unique(existing_row_keys)
    existing_counts <- table(existing_row_keys)
  }
}

make_current_job <- function(
    file,
    source_folder,
    country,
    competition,
    league,
    tier,
    competition_type = "league",
    source = "openfootball"
) {
  data.frame(
    file = file,
    source_folder = source_folder,
    country = country,
    competition = competition,
    competition_type = competition_type,
    league = league,
    tier = as.integer(tier),
    source = source,
    season_folder = season_folder,
    parser = "openfootball",
    stringsAsFactors = FALSE
  )
}

make_cached_league_jobs <- function(
    seasons,
    file,
    source_folder,
    country,
    competition,
    league,
    tier = 1L
) {
  seasons <- unique(as.character(seasons))
  season_labels <- vapply(
    seasons,
    normalise_openfootball_season_label,
    character(1)
  )
  
  keys <- paste(
    season_labels,
    country,
    competition,
    sep = "||"
  )
  
  # Historical competition-seasons already present in the combined CSV are
  # checkpoints: keep them as-is. Missing historical seasons are parsed once,
  # while the current season is always reparsed so new results/fixtures flow in.
  needed <- !(keys %in% existing_keys) | seasons == season_folder
  
  data.frame(
    file = rep(file, sum(needed)),
    source_folder = rep(source_folder, sum(needed)),
    country = rep(country, sum(needed)),
    competition = rep(competition, sum(needed)),
    competition_type = rep("league", sum(needed)),
    league = rep(league, sum(needed)),
    tier = rep(as.integer(tier), sum(needed)),
    source = rep("openfootball", sum(needed)),
    season_folder = seasons[needed],
    parser = rep("openfootball", sum(needed)),
    stringsAsFactors = FALSE
  )
}

# Established OpenFootball competitions.
#
# The combined CSV is a checkpoint, but the source cache is now reconstructible.
# Top-flight coverage begins at the same cutover used by the Schochastics
# historical backbone. Lower divisions retain the existing 2018/19+ policy.

england_pl_seasons <- vapply(
  1992:current_start_year,
  season_folder_from_start_year,
  character(1)
)
spain_top_seasons <- vapply(
  2012:current_start_year,
  season_folder_from_start_year,
  character(1)
)
italy_top_seasons <- vapply(
  2013:current_start_year,
  season_folder_from_start_year,
  character(1)
)
germany_top_seasons <- vapply(
  2010:current_start_year,
  season_folder_from_start_year,
  character(1)
)
france_top_seasons <- vapply(
  2014:current_start_year,
  season_folder_from_start_year,
  character(1)
)
recent_major_seasons <- vapply(
  2018:current_start_year,
  season_folder_from_start_year,
  character(1)
)

english_jobs <- rbind(
  make_cached_league_jobs(
    england_pl_seasons,
    "1-premierleague.txt", "england", "England",
    "premier_league", "Premier League", 1L
  ),
  make_cached_league_jobs(
    recent_major_seasons,
    "2-championship.txt", "england", "England",
    "championship", "Championship", 2L
  ),
  make_cached_league_jobs(
    recent_major_seasons,
    "3-league1.txt", "england", "England",
    "league_1", "League 1", 3L
  ),
  make_cached_league_jobs(
    recent_major_seasons,
    "4-league2.txt", "england", "England",
    "league_2", "League 2", 4L
  ),
  make_cached_league_jobs(
    recent_major_seasons,
    "5-nationalleague.txt", "england", "England",
    "national_league", "National League", 5L
  )
)

# One-time targeted Championship rebuilds.
english_championship_repairs <- data.frame(
  file = rep("2-championship.txt", 3L),
  source_folder = rep("england", 3L),
  country = rep("England", 3L),
  competition = rep("championship", 3L),
  competition_type = rep("league", 3L),
  league = rep("Championship", 3L),
  tier = rep(2L, 3L),
  source = rep("openfootball", 3L),
  season_folder = c("2018-19", "2024-25", "2025-26"),
  parser = rep("openfootball", 3L),
  stringsAsFactors = FALSE
)

repair_keys <- paste(
  normalise_openfootball_season_label(english_championship_repairs$season_folder),
  english_championship_repairs$country,
  english_championship_repairs$competition,
  sep = "||"
)
english_job_keys <- paste(
  normalise_openfootball_season_label(english_jobs$season_folder),
  english_jobs$country,
  english_jobs$competition,
  sep = "||"
)
english_championship_repairs <- english_championship_repairs[
  !(repair_keys %in% english_job_keys),
]
english_jobs <- rbind(english_jobs, english_championship_repairs)

la_liga_jobs <- make_cached_league_jobs(
  spain_top_seasons,
  "1-liga.txt", "espana", "Spain",
  "la_liga", "La Liga", 1L
)
segunda_jobs <- make_cached_league_jobs(
  recent_major_seasons,
  "2-liga2.txt", "espana", "Spain",
  "segunda_division", "Segunda DivisiÃ³n", 2L
)
serie_a_jobs <- make_cached_league_jobs(
  italy_top_seasons,
  "1-seriea.txt", "italy", "Italy",
  "serie_a", "Serie A", 1L
)
serie_b_jobs <- make_cached_league_jobs(
  recent_major_seasons,
  "2-serieb.txt", "italy", "Italy",
  "serie_b", "Serie B", 2L
)
bundesliga_jobs <- make_cached_league_jobs(
  germany_top_seasons,
  "1-bundesliga.txt", "deutschland", "Germany",
  "bundesliga", "Bundesliga", 1L
)
bundesliga2_jobs <- make_cached_league_jobs(
  recent_major_seasons,
  "2-bundesliga2.txt", "deutschland", "Germany",
  "bundesliga_2", "2. Bundesliga", 2L
)
ligue1_jobs <- make_cached_league_jobs(
  france_top_seasons,
  "1-ligue1.txt", "france", "France",
  "ligue_1", "Ligue 1", 1L
)
ligue2_jobs <- make_cached_league_jobs(
  recent_major_seasons,
  "2-ligue2.txt", "france", "France",
  "ligue_2", "Ligue 2", 2L
)

# Champions League: current season only. Historical rows are preserved.
champions_league_jobs <- make_current_job(
  "cl.txt", "champions-league", "Europe",
  "champions_league", "Champions League", NA_integer_,
  competition_type = "continental"
)


# Smaller European top divisions.
portugal_jobs <- make_cached_league_jobs(
  c(
    "2018-19", "2019-20", "2020-21", "2021-22",
    "2022-23", "2023-24", "2024-25", "2025-26", "2026-27"
  ),
  "1-primeira-liga.txt", "portugal", "Portugal",
  "primeira_liga", "Primeira Liga"
)

netherlands_jobs <- make_cached_league_jobs(
  c(
    "2018-19", "2019-20", "2020-21", "2021-22",
    "2022-23", "2023-24", "2024-25", "2025-26", "2026-27"
  ),
  "1-eredivisie.txt", "netherlands", "Netherlands",
  "eredivisie", "Eredivisie"
)

austria_jobs <- make_cached_league_jobs(
  unique(c(
    vapply(2010:2025, season_folder_from_start_year, character(1)),
    season_folder
  )),
  "1-austrian-bundesliga.txt", "austria", "Austria",
  "austrian_bundesliga", "Austrian Bundesliga"
)

belgium_jobs <- make_cached_league_jobs(
  unique(c(
    "2018-19", "2019-20", "2021-22",
    "2023-24", "2024-25", "2025-26", "2026-27",
    season_folder
  )),
  "1-belgian-pro-league.txt", "belgium", "Belgium",
  "belgian_pro_league", "Belgian Pro League"
)

switzerland_jobs <- make_cached_league_jobs(
  unique(c(
    "2014-15", "2018-19", "2019-20", "2020-21",
    "2023-24", "2024-25", season_folder
  )),
  "1-swiss-super-league.txt", "switzerland", "Switzerland",
  "swiss_super_league", "Swiss Super League"
)

scotland_jobs <- make_cached_league_jobs(
  unique(c(
    "2018-19", "2019-20", "2020-21",
    "2023-24", "2024-25", "2025-26", season_folder
  )),
  "1-scottish-premiership.txt", "scotland", "Scotland",
  "scottish_premiership", "Scottish Premiership"
)

turkey_jobs <- make_cached_league_jobs(
  unique(c(
    "2018-19", "2019-20", "2020-21",
    "2023-24", "2024-25", "2025-26", season_folder
  )),
  "1-super-lig.txt", "turkey", "Turkey",
  "super_lig", "SÃ¼per Lig"
)

greece_jobs <- make_cached_league_jobs(
  unique(c(
    "2018-19", "2019-20", "2020-21",
    "2023-24", "2024-25", "2025-26", season_folder
  )),
  "1-super-league-greece.txt", "greece", "Greece",
  "super_league_greece", "Super League Greece"
)

czechia_jobs <- make_cached_league_jobs(
  unique(c(
    "2018-19", "2020-21", "2023-24", "2024-25", season_folder
  )),
  "1-czech-first-league.txt", "czech-republic", "Czechia",
  "czech_first_league", "Czech First League"
)

ukraine_jobs <- make_cached_league_jobs(
  unique(c("2023-24", "2024-25", season_folder)),
  "1-ukrainian-premier-league.txt", "ukraine", "Ukraine",
  "ukrainian_premier_league", "Ukrainian Premier League"
)

dfb_pokal_jobs <- make_cached_league_jobs(
  unique(c(vapply(2010:2025, season_folder_from_start_year, character(1)), season_folder)),
  "cup-dfb-pokal.txt", "deutschland", "Germany",
  "dfb_pokal", "DFB-Pokal"
)
dfb_pokal_jobs$competition_type <- "domestic_cup"
dfb_pokal_jobs$tier <- NA_integer_

openfootball_jobs <- rbind(
  english_jobs,
  la_liga_jobs,
  segunda_jobs,
  serie_a_jobs,
  serie_b_jobs,
  bundesliga_jobs,
  bundesliga2_jobs,
  ligue1_jobs,
  ligue2_jobs,
  champions_league_jobs,
  portugal_jobs,
  netherlands_jobs,
  austria_jobs,
  belgium_jobs,
  switzerland_jobs,
  scotland_jobs,
  turkey_jobs,
  greece_jobs,
  czechia_jobs,
  ukraine_jobs,
  dfb_pokal_jobs
)

make_wikipedia_parse_jobs <- function(
    first_start_year,
    competition,
    league
) {
  start_years <- first_start_year:current_start_year
  jobs <- list()
  k <- 1L
  
  for (start_year in start_years) {
    season <- season_folder_from_start_year(start_year)
    season_label <- normalise_openfootball_season_label(season)
    key <- paste(season_label, "Europe", competition, sep = "||")
    
    existing_n <- if (key %in% names(existing_counts)) {
      as.integer(existing_counts[[key]])
    } else {
      0L
    }
    
    # The previous parser accidentally imported only the final from each season.
    # Treat obviously tiny historical seasons as incomplete and rebuild them once.
    minimum_complete_rows <- if (competition == "europa_league") 50L else 20L
    
    needs_rebuild <- existing_n < minimum_complete_rows
    is_current <- identical(season, season_folder)
    
    if (!needs_rebuild && !is_current) {
      next
    }
    
    first_stage <- if (start_year >= 2024L) "league_phase.html" else "group_stage.html"
    
    stage_files <- c(first_stage, "knockout_phase.html")
    
    for (stage_file in stage_files) {
      jobs[[k]] <- data.frame(
        file = stage_file,
        source_folder = competition,
        country = "Europe",
        competition = competition,
        competition_type = "continental",
        league = league,
        tier = NA_integer_,
        source = "wikipedia",
        season_folder = season,
        parser = "wikipedia",
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  
  if (length(jobs) == 0) {
    return(data.frame(
      file = character(),
      source_folder = character(),
      country = character(),
      competition = character(),
      competition_type = character(),
      league = character(),
      tier = integer(),
      source = character(),
      season_folder = character(),
      parser = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind, jobs)
}

make_wikipedia_domestic_cup_parse_jobs <- function(first_start_year, competition, league, country) {
  start_years <- first_start_year:current_start_year
  seasons <- vapply(start_years, season_folder_from_start_year, character(1))
  season_labels <- vapply(seasons, normalise_openfootball_season_label, character(1))
  keys <- paste(season_labels, country, competition, sep="||")
  
  # Rebuild historical cup seasons that were previously imported with only
  # a final or a handful of late-round matches. Once a season has a sensible
  # number of rows it remains cached; the current season is always reparsed.
  minimum_complete_rows <- switch(
    competition,
    fa_cup = 50L,
    efl_cup = 30L,
    copa_del_rey = 20L,
    coppa_italia = 20L,
    coupe_de_france = 30L,
    20L
  )
  
  existing_n <- vapply(
    keys,
    function(key) {
      if (key %in% names(existing_counts)) {
        as.integer(existing_counts[[key]])
      } else {
        0L
      }
    },
    integer(1)
  )
  
  needs_rebuild <- existing_n < minimum_complete_rows
  is_current <- seasons == season_folder
  needed <- needs_rebuild | is_current
  
  data.frame(
    file=rep("page.html", sum(needed)),
    source_folder=rep(competition, sum(needed)),
    country=rep(country, sum(needed)),
    competition=rep(competition, sum(needed)),
    competition_type=rep("domestic_cup", sum(needed)),
    league=rep(league, sum(needed)),
    tier=rep(NA_integer_, sum(needed)),
    source=rep("wikipedia", sum(needed)),
    season_folder=seasons[needed],
    parser=rep("wikipedia", sum(needed)),
    stringsAsFactors=FALSE
  )
}

europa_league_jobs <- make_wikipedia_parse_jobs(
  2011L,
  "europa_league",
  "Europa League"
)

conference_league_jobs <- make_wikipedia_parse_jobs(
  2021L,
  "conference_league",
  "Conference League"
)

fa_cup_jobs <- make_wikipedia_domestic_cup_parse_jobs(1998L, "fa_cup", "FA Cup", "England")
efl_cup_jobs <- make_wikipedia_domestic_cup_parse_jobs(1998L, "efl_cup", "EFL Cup", "England")
copa_del_rey_jobs <- make_wikipedia_domestic_cup_parse_jobs(2012L, "copa_del_rey", "Copa del Rey", "Spain")
coppa_italia_jobs <- make_wikipedia_domestic_cup_parse_jobs(2013L, "coppa_italia", "Coppa Italia", "Italy")
coupe_de_france_jobs <- make_wikipedia_domestic_cup_parse_jobs(2014L, "coupe_de_france", "Coupe de France", "France")

parse_jobs <- rbind(
  openfootball_jobs,
  europa_league_jobs,
  conference_league_jobs,
  fa_cup_jobs,
  efl_cup_jobs,
  copa_del_rey_jobs,
  coppa_italia_jobs,
  coupe_de_france_jobs
)

# -----------------------------
# Parse configured files
# -----------------------------

all_df_list <- list()
k <- 1L

for (i in seq_len(nrow(parse_jobs))) {
  cfg <- parse_jobs[i, ]
  parser_name <- as.character(cfg$parser)
  
  root_dir <- if (identical(parser_name, "wikipedia")) {
    wikipedia_source_root_dir
  } else {
    source_root_dir
  }
  
  season_dir <- file.path(
    root_dir,
    cfg$source_folder,
    cfg$season_folder
  )
  
  fpath <- file.path(season_dir, cfg$file)
  
  if (!file.exists(fpath)) {
    protected_top_flights <- c(
      "premier_league", "la_liga", "serie_a", "bundesliga", "ligue_1"
    )
    
    if (
      identical(parser_name, "openfootball") &&
      cfg$competition %in% protected_top_flights
    ) {
      stop(
        "Required top-flight source file is missing:\n",
        fpath,
        "\nRun 00_download_openfootball_current.R first. ",
        "The parser will not silently publish a historical gap."
      )
    }
    
    message("Skipping unavailable optional source file: ", fpath)
    next
  }
  
  tmp <- tryCatch(
    {
      if (identical(parser_name, "wikipedia")) {
        parse_wikipedia_competition_page(
          html_path = fpath,
          season_folder = cfg$season_folder,
          competition_id = cfg$competition,
          league_label = cfg$league,
          country = cfg$country,
          competition_type = cfg$competition_type
        )
      } else {
        parse_comp_file(
          txt_path = fpath,
          country = cfg$country,
          competition_id = cfg$competition,
          competition_type = cfg$competition_type,
          league_label = cfg$league,
          tier = cfg$tier,
          source = cfg$source
        )
      }
    },
    error = function(e) {
      message("Skipping parse error: ", fpath, " | ", conditionMessage(e))
      NULL
    }
  )
  
  if (!is.null(tmp) && nrow(tmp) > 0) {
    declared_matches <- if (identical(parser_name, "openfootball")) {
      declared_match_count(fpath)
    } else {
      NA_integer_
    }
    
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
  stop("No source data parsed.")
}

parsed_df <- do.call(rbind, all_df_list)

# Add the historical top-flight backbone from Schochastics. This is deliberately
# cut off immediately before each competition's existing J-Ratings source begins.
schoch_df <- parse_schoch_historical_top_flights(schoch_results_file)
parsed_df <- rbind(parsed_df, schoch_df)

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
# Duplicate protection / reconciliation
# -----------------------------
# Some OpenFootball files occasionally repeat an identical fixture line.
# Exact/reconcilable repeats are harmless and should not block the pipeline.
# However, genuinely conflicting duplicates (same fixture key but different
# completed results or structural metadata) remain a hard failure.

make_match_key <- function(df) {
  paste(
    df$Season,
    df$Country,
    df$Competition,
    df$Date,
    df$Home,
    df$Away,
    sep = "||"
  )
}

match_key <- make_match_key(all_df)

if (anyDuplicated(match_key)) {
  dup_keys <- unique(match_key[duplicated(match_key)])
  
  conflict_messages <- character()
  
  for (k in dup_keys) {
    z <- all_df[match_key == k, , drop = FALSE]
    
    result_vals <- unique(trimws(as.character(z$Result)))
    result_vals <- result_vals[!is.na(result_vals) & result_vals != ""]
    
    if (length(result_vals) > 1L) {
      conflict_messages <- c(
        conflict_messages,
        paste0(k, " | conflicting Result values: ", paste(result_vals, collapse = ", "))
      )
      next
    }
    
    # Structural metadata for the same fixture must agree. Source is excluded
    # because an otherwise identical fixture can legitimately be represented
    # by more than one source.
    for (col in c("CompetitionType", "Tier", "League")) {
      vals <- unique(trimws(as.character(z[[col]])))
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) > 1L) {
        conflict_messages <- c(
          conflict_messages,
          paste0(k, " | conflicting ", col, " values: ", paste(vals, collapse = ", "))
        )
      }
    }
  }
  
  if (length(conflict_messages) > 0L) {
    cat("\nConflicting duplicate fixture records detected:\n")
    cat(paste0("  ", conflict_messages, collapse = "\n"), "\n")
    stop("Conflicting duplicate match records detected. Refusing to write combined CSV.")
  }
  
  # Reconcile harmless duplicates. Prefer the row with a completed Result; if
  # neither/both have one, prefer a populated Score; otherwise keep the first.
  has_result <- !is.na(all_df$Result) & trimws(as.character(all_df$Result)) != ""
  has_score <- !is.na(all_df$Score) & trimws(as.character(all_df$Score)) != ""
  original_order <- seq_len(nrow(all_df))
  
  ord <- order(match_key, -as.integer(has_result), -as.integer(has_score), original_order)
  all_df <- all_df[ord, , drop = FALSE]
  match_key <- make_match_key(all_df)
  
  duplicate_drop <- duplicated(match_key)
  n_removed <- sum(duplicate_drop)
  
  cat(
    "\nReconciled harmless duplicate fixture rows:",
    n_removed,
    "across",
    length(dup_keys),
    "fixture keys.\n"
  )
  
  all_df <- all_df[!duplicate_drop, , drop = FALSE]
  rownames(all_df) <- NULL
  match_key <- make_match_key(all_df)
}

if (anyDuplicated(match_key)) {
  stop("Duplicate match records remain after reconciliation. Refusing to write combined CSV.")
}

# -----------------------------
# Major-league rollover QA
# -----------------------------
# The previous season must still exist before we publish a refreshed combined
# CSV. This catches the exact failure mode where current fixtures are present
# but last season's league history has disappeared.
previous_start_year <- current_start_year - 1L
previous_season <- normalise_openfootball_season_label(
  season_folder_from_start_year(previous_start_year)
)

major_league_qa <- data.frame(
  Country = c("England", "Spain", "Italy", "Germany", "France"),
  Competition = c("premier_league", "la_liga", "serie_a", "bundesliga", "ligue_1"),
  League = c("Premier League", "La Liga", "Serie A", "Bundesliga", "Ligue 1"),
  stringsAsFactors = FALSE
)

major_league_qa$Rows <- vapply(
  seq_len(nrow(major_league_qa)),
  function(i) {
    sum(
      all_df$Season == previous_season &
        all_df$Country == major_league_qa$Country[i] &
        all_df$Competition == major_league_qa$Competition[i]
    )
  },
  integer(1)
)

major_league_qa$Teams <- vapply(
  seq_len(nrow(major_league_qa)),
  function(i) {
    z <- all_df[
      all_df$Season == previous_season &
        all_df$Country == major_league_qa$Country[i] &
        all_df$Competition == major_league_qa$Competition[i],
    ]
    length(unique(c(z$Home, z$Away)))
  },
  integer(1)
)

major_league_qa$Status <- ifelse(
  major_league_qa$Rows > 0L & major_league_qa$Teams >= 16L,
  "PASS",
  "FAIL"
)

cat("\nPrevious-season major-league QA (", previous_season, "):\n", sep = "")
print(major_league_qa, row.names = FALSE)

if (any(major_league_qa$Status == "FAIL")) {
  failed <- major_league_qa[major_league_qa$Status == "FAIL", ]
  stop(
    "Previous-season league history is missing/incomplete for: ",
    paste(failed$League, collapse = ", "),
    ". Refusing to overwrite the combined CSV."
  )
}

cat("Previous-season major-league QA: PASS\n")


# -----------------------------
# Major top-flight continuity QA
# -----------------------------
# Refuse to publish if any post-cutover completed season has disappeared.
# This protects against a damaged combined CSV or missing local source cache.

continuity_cfg <- data.frame(
  Country = c("England", "Spain", "Italy", "Germany", "France"),
  Competition = c("premier_league", "la_liga", "serie_a", "bundesliga", "ligue_1"),
  FirstStartYear = c(1992L, 2012L, 2013L, 2010L, 2014L),
  stringsAsFactors = FALSE
)

continuity_failures <- character()

for (i in seq_len(nrow(continuity_cfg))) {
  c0 <- continuity_cfg[i, ]
  
  expected_starts <- c0$FirstStartYear:previous_start_year
  expected_seasons <- vapply(
    expected_starts,
    function(y) normalise_openfootball_season_label(
      season_folder_from_start_year(y)
    ),
    character(1)
  )
  
  z <- all_df[
    all_df$Country == c0$Country &
      all_df$Competition == c0$Competition,
    ,
    drop = FALSE
  ]
  
  season_counts <- table(z$Season)
  
  for (s in expected_seasons) {
    n <- if (s %in% names(season_counts)) as.integer(season_counts[[s]]) else 0L
    
    # All five protected leagues have comfortably more than 250 matches in a
    # completed season; this also catches accidentally partial imports.
    if (n < 250L) {
      continuity_failures <- c(
        continuity_failures,
        paste0(c0$Country, " | ", c0$Competition, " | ", s, " | rows=", n)
      )
    }
  }
}

if (length(continuity_failures) > 0L) {
  cat("\nMajor top-flight continuity failures:\n")
  cat(paste0("  ", continuity_failures, collapse = "\n"), "\n")
  stop(
    "Historical top-flight continuity QA failed. ",
    "Refusing to overwrite the combined CSV."
  )
}

cat("Major top-flight continuity QA: PASS\n")

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
if (interactive()) beep()
