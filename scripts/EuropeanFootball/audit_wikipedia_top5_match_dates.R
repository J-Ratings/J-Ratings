# audit_wikipedia_top5_match_dates.R
#
# Purpose:
#   Check Wikipedia season pages for the historical top-flight seasons that
#   pre-date J-Ratings' current coverage, and estimate whether each season page
#   contains individual league match records with exact dates.
#
# This script DOES NOT change J-Ratings data or Elo.
# It only downloads/caches Wikipedia HTML and writes an audit CSV.
#
# Packages:
#   install.packages("xml2")   # only if you do not already have it

library(tictoc)
options(stringsAsFactors = FALSE)


tic()
if (!requireNamespace("xml2", quietly = TRUE)) {
  stop('Package "xml2" is required. Run install.packages("xml2") first.')
}

# ------------------------------------------------------------------
# Settings
# ------------------------------------------------------------------

repo_dir <- normalizePath(
  Sys.getenv(
    "J_RATINGS_REPO",
    "C:/Users/stjuk/Documents/GitHub/J-Ratings"
  ),
  winslash = "/",
  mustWork = FALSE
)

OUT_DIR <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Audit",
  "wikipedia_top5_dates"
)

CACHE_DIR <- file.path(OUT_DIR, "html")
RESULT_CSV <- file.path(OUT_DIR, "wikipedia_top5_date_audit.csv")

dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

# Be polite to Wikimedia. Historical pages are cached permanently, so this
# delay only applies the first time a page is downloaded.
REQUEST_SLEEP_SECONDS <- 1.5

WIKIMEDIA_USER_AGENT <- Sys.getenv(
  "WIKIMEDIA_USER_AGENT",
  unset = "J-Ratings/1.0 (European football ratings project)"
)

# Only audit the historical gap BEFORE current J-Ratings top-flight coverage.
#
# first_year = first season we would potentially like to add
# last_year  = start year of the season immediately before J-Ratings begins
SPECS <- data.frame(
  Country = c("England", "Spain", "Italy", "Germany", "France"),
  FirstStartYear = c(1888L, 1929L, 1929L, 1963L, 1932L),
  LastStartYear  = c(1991L, 2011L, 2012L, 2009L, 2013L),
  stringsAsFactors = FALSE
)

# Known seasons in which the normal national top-flight league did not run.
# These are excluded from the audit rather than treated as "missing pages".
SKIP_START_YEARS <- list(
  England = c(1915:1918, 1939:1945),
  Spain   = c(1936:1938),
  Italy   = c(1943:1945),
  Germany = integer(),
  France  = c(1939:1944)
)

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

season_label <- function(start_year) {
  start_year <- as.integer(start_year)
  end_short <- sprintf("%02d", (start_year + 1L) %% 100L)
  paste0(start_year, "-", end_short)
}

wiki_dash_season <- function(start_year) {
  start_year <- as.integer(start_year)
  end_short <- sprintf("%02d", (start_year + 1L) %% 100L)
  paste0(start_year, "\u2013", end_short)
}

wikipedia_page_title <- function(country, start_year) {
  s <- wiki_dash_season(start_year)
  
  if (country == "England") {
    return(paste(s, "Football League"))
  }
  
  if (country == "Spain") {
    if (start_year == 1929L) return("1929 La Liga")
    return(paste(s, "La Liga"))
  }
  
  if (country == "Italy") {
    return(paste(s, "Serie A"))
  }
  
  if (country == "Germany") {
    return(paste(s, "Bundesliga"))
  }
  
  if (country == "France") {
    if (start_year >= 2002L) return(paste(s, "Ligue 1"))
    return(paste(s, "French Division 1"))
  }
  
  stop("Unknown country: ", country)
}

wiki_url <- function(page_title) {
  paste0(
    "https://en.wikipedia.org/wiki/",
    utils::URLencode(gsub(" ", "_", page_title), reserved = TRUE)
  )
}

safe_file_part <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

download_page <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 1000) {
    return("cached")
  }
  
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  
  tmp <- paste0(dest, ".tmp")
  if (file.exists(tmp)) file.remove(tmp)
  
  status <- tryCatch(
    {
      suppressWarnings(
        download.file(
          url = url,
          destfile = tmp,
          mode = "wb",
          quiet = TRUE,
          method = "libcurl",
          headers = c(
            "User-Agent" = WIKIMEDIA_USER_AGENT,
            "Accept-Language" = "en-GB,en;q=0.9"
          )
        )
      )
      
      if (!file.exists(tmp) || file.info(tmp)$size < 1000) {
        stop("Downloaded file is empty or too small.")
      }
      
      file.rename(tmp, dest)
      "downloaded"
    },
    error = function(e) {
      if (file.exists(tmp)) file.remove(tmp)
      attr(e, "jr_status") <- "download_failed"
      stop(e)
    }
  )
  
  Sys.sleep(REQUEST_SLEEP_SECONDS)
  status
}

normalise_space <- function(x) {
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Covers common Wikipedia date styles:
#   10 February 1929
#   10 Feb 1929
#   February 10, 1929
#   10 February
#   10 Feb
date_regex <- paste0(
  "(",
  "\\b[0-3]?[0-9]\\s+",
  "(January|February|March|April|May|June|July|August|September|October|November|December|",
  "Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)",
  "(\\s+[12][0-9]{3})?\\b",
  "|",
  "\\b(January|February|March|April|May|June|July|August|September|October|November|December|",
  "Jan|Feb|Mar|Apr|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)",
  "\\s+[0-3]?[0-9],?\\s+[12][0-9]{3}\\b",
  ")"
)

# Score patterns likely to represent a played football match.
# Avoid plain table positions/records by requiring two numbers separated by a
# football-style dash/hyphen/colon.
score_regex <- "\\b[0-9]{1,2}\\s*[-\u2013\u2014:]\\s*[0-9]{1,2}\\b"

has_date <- function(x) {
  grepl(date_regex, x, ignore.case = TRUE, perl = TRUE)
}

has_score <- function(x) {
  grepl(score_regex, x, perl = TRUE)
}

count_dated_footballboxes <- function(doc) {
  boxes <- xml2::xml_find_all(
    doc,
    "//*[contains(concat(' ', normalize-space(@class), ' '), ' footballbox ')]"
  )
  
  if (!length(boxes)) return(0L)
  
  txt <- vapply(boxes, function(x) normalise_space(xml2::xml_text(x)), character(1))
  sum(vapply(txt, function(x) has_date(x) && has_score(x), logical(1)))
}

count_dated_match_table_rows <- function(doc) {
  rows <- xml2::xml_find_all(doc, "//table//tr")
  if (!length(rows)) return(0L)
  
  txt <- vapply(rows, function(x) normalise_space(xml2::xml_text(x)), character(1))
  
  # Match-like table rows need both an exact date and a football score.
  sum(vapply(txt, function(x) has_date(x) && has_score(x), logical(1)))
}

count_undated_score_rows <- function(doc) {
  rows <- xml2::xml_find_all(doc, "//table//tr")
  if (!length(rows)) return(0L)
  
  txt <- vapply(rows, function(x) normalise_space(xml2::xml_text(x)), character(1))
  
  sum(vapply(txt, function(x) !has_date(x) && has_score(x), logical(1)))
}

extract_expected_matches <- function(doc) {
  # Look in the infobox for "Matches played" or a simple "Matches" field.
  rows <- xml2::xml_find_all(
    doc,
    "//table[contains(@class,'infobox')]//tr"
  )
  
  if (!length(rows)) return(NA_integer_)
  
  for (r in rows) {
    cells <- xml2::xml_find_all(r, "./th|./td")
    if (length(cells) < 2) next
    
    key <- normalise_space(xml2::xml_text(cells[[1]]))
    val <- normalise_space(xml2::xml_text(cells[[2]]))
    
    if (grepl("^Matches( played)?$", key, ignore.case = TRUE)) {
      n <- suppressWarnings(as.integer(gsub("[^0-9]", "", sub("\\[.*$", "", val))))
      if (is.finite(n) && !is.na(n) && n > 0) return(n)
    }
  }
  
  NA_integer_
}

audit_page <- function(country, start_year) {
  season <- season_label(start_year)
  title <- wikipedia_page_title(country, start_year)
  url <- wiki_url(title)
  
  dest <- file.path(
    CACHE_DIR,
    safe_file_part(country),
    paste0(safe_file_part(season), ".html")
  )
  
  cat(sprintf("%-9s %-7s  %s\n", country, season, title))
  
  download_status <- tryCatch(
    download_page(url, dest),
    error = function(e) {
      message("  -> download failed: ", conditionMessage(e))
      return("download_failed")
    }
  )
  
  if (download_status == "download_failed" || !file.exists(dest)) {
    return(data.frame(
      Country = country,
      Season = season,
      StartYear = start_year,
      PageTitle = title,
      URL = url,
      DownloadStatus = download_status,
      ExpectedMatches = NA_integer_,
      DatedFootballBoxes = NA_integer_,
      DatedTableRows = NA_integer_,
      UndatedScoreRows = NA_integer_,
      BestDatedCount = NA_integer_,
      CoveragePct = NA_real_,
      Assessment = "PAGE_MISSING_OR_FAILED",
      stringsAsFactors = FALSE
    ))
  }
  
  doc <- tryCatch(
    xml2::read_html(dest, encoding = "UTF-8"),
    error = function(e) NULL
  )
  
  if (is.null(doc)) {
    return(data.frame(
      Country = country,
      Season = season,
      StartYear = start_year,
      PageTitle = title,
      URL = url,
      DownloadStatus = download_status,
      ExpectedMatches = NA_integer_,
      DatedFootballBoxes = NA_integer_,
      DatedTableRows = NA_integer_,
      UndatedScoreRows = NA_integer_,
      BestDatedCount = NA_integer_,
      CoveragePct = NA_real_,
      Assessment = "HTML_PARSE_FAILED",
      stringsAsFactors = FALSE
    ))
  }
  
  expected <- extract_expected_matches(doc)
  n_boxes <- count_dated_footballboxes(doc)
  n_rows <- count_dated_match_table_rows(doc)
  n_undated <- count_undated_score_rows(doc)
  
  # footballbox and table-row representations may overlap. Use the larger count,
  # not their sum, to avoid obvious double-counting.
  best <- max(n_boxes, n_rows, na.rm = TRUE)
  
  coverage <- if (!is.na(expected) && expected > 0) {
    100 * best / expected
  } else {
    NA_real_
  }
  
  assessment <- if (!is.na(coverage)) {
    if (coverage >= 95) {
      "LIKELY_DATE_COMPLETE"
    } else if (best > 0) {
      "SOME_DATED_MATCHES"
    } else if (n_undated > 0) {
      "RESULTS_PRESENT_BUT_NO_MATCH_DATES_FOUND"
    } else {
      "NO_MATCH_LIST_DETECTED"
    }
  } else {
    if (best >= 20) {
      "MANY_DATED_MATCHES_EXPECTED_TOTAL_UNKNOWN"
    } else if (best > 0) {
      "SOME_DATED_MATCHES_EXPECTED_TOTAL_UNKNOWN"
    } else if (n_undated > 0) {
      "RESULTS_PRESENT_BUT_NO_MATCH_DATES_FOUND"
    } else {
      "NO_MATCH_LIST_DETECTED"
    }
  }
  
  data.frame(
    Country = country,
    Season = season,
    StartYear = start_year,
    PageTitle = title,
    URL = url,
    DownloadStatus = download_status,
    ExpectedMatches = expected,
    DatedFootballBoxes = n_boxes,
    DatedTableRows = n_rows,
    UndatedScoreRows = n_undated,
    BestDatedCount = best,
    CoveragePct = ifelse(is.na(coverage), NA_real_, round(coverage, 1)),
    Assessment = assessment,
    stringsAsFactors = FALSE
  )
}

earliest_continuous_complete <- function(x) {
  x <- x[order(x$StartYear), ]
  
  ok <- x$Assessment == "LIKELY_DATE_COMPLETE"
  if (!any(ok, na.rm = TRUE)) return(NA_character_)
  
  # Find the earliest season such that every audited season from there onward
  # is marked complete.
  for (i in seq_len(nrow(x))) {
    if (all(ok[i:nrow(x)], na.rm = FALSE)) {
      return(x$Season[i])
    }
  }
  
  NA_character_
}

# ------------------------------------------------------------------
# Run audit
# ------------------------------------------------------------------

results <- list()
k <- 1L

cat("\nWikipedia historical top-flight date audit\n")
cat("=========================================\n")
cat("Sleep between new downloads:", REQUEST_SLEEP_SECONDS, "seconds\n")
cat("Cache:", CACHE_DIR, "\n\n")

for (i in seq_len(nrow(SPECS))) {
  country <- SPECS$Country[i]
  years <- SPECS$FirstStartYear[i]:SPECS$LastStartYear[i]
  years <- setdiff(years, SKIP_START_YEARS[[country]])
  
  for (yr in years) {
    results[[k]] <- audit_page(country, yr)
    k <- k + 1L
  }
}

audit <- do.call(rbind, results)
audit <- audit[order(audit$Country, audit$StartYear), ]

write.csv(audit, RESULT_CSV, row.names = FALSE, na = "")

cat("\n\nSUMMARY\n")
cat("=======\n")

for (country in SPECS$Country) {
  x <- audit[audit$Country == country, ]
  
  cat("\n", country, "\n", sep = "")
  cat("  Seasons audited: ", nrow(x), "\n", sep = "")
  cat(
    "  Likely date-complete season pages: ",
    sum(x$Assessment == "LIKELY_DATE_COMPLETE", na.rm = TRUE),
    "\n",
    sep = ""
  )
  cat(
    "  Some dated matches: ",
    sum(grepl("^SOME_DATED", x$Assessment), na.rm = TRUE),
    "\n",
    sep = ""
  )
  cat(
    "  Results found but no match dates detected: ",
    sum(x$Assessment == "RESULTS_PRESENT_BUT_NO_MATCH_DATES_FOUND", na.rm = TRUE),
    "\n",
    sep = ""
  )
  
  continuous <- earliest_continuous_complete(x)
  cat(
    "  Earliest continuous LIKELY_DATE_COMPLETE season: ",
    ifelse(is.na(continuous), "none found", continuous),
    "\n",
    sep = ""
  )
  
  incomplete <- x[
    x$Assessment != "LIKELY_DATE_COMPLETE",
    c("Season", "ExpectedMatches", "BestDatedCount", "CoveragePct", "Assessment")
  ]
  
  if (nrow(incomplete)) {
    cat("  First 10 non-complete seasons:\n")
    print(utils::head(incomplete, 10), row.names = FALSE)
  }
}

cat("\nFull audit written to:\n", RESULT_CSV, "\n", sep = "")
cat("\nIMPORTANT:\n")
cat(
  "This is an automated structural audit of Wikipedia SEASON pages. ",
  "It tells us where the main page appears to contain individually dated ",
  "match records. A season marked incomplete may still have dates spread ",
  "across individual club-season pages. We can investigate those only after ",
  "seeing this first-pass result.\n",
  sep = ""
)

toc()
