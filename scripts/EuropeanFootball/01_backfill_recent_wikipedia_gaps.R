options(stringsAsFactors = FALSE)

# J-Ratings European Football
# Recent historical league-gap backfill:
#   Wikipedia = score/result source
#   OpenFootball = date/fixture skeleton only
#
# The script is deliberately conservative:
#   - Wikipedia pages are cached locally.
#   - 5 seconds between Wikipedia requests.
#   - OpenFootball is only requested if the matching local date file is absent.
#   - no Elo mechanics are changed.
#   - nothing is written to the master CSV unless every requested season passes QA.
#   - score conflicts hard-stop the run.
#   - a timestamped backup is created immediately before the master CSV is changed.

script_started <- Sys.time()

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

combined_file <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Matches_Clean_Combined",
  "european_football_all_matches.csv"
)

wiki_root <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "wikipedia",
  "historical_gap_backfill"
)

openfootball_root <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "openfootball"
)

audit_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Audits"
)

dir.create(wiki_root, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(combined_file)) {
  stop("Master combined CSV not found:\n", combined_file)
}

if (!requireNamespace("xml2", quietly = TRUE)) {
  stop("Package 'xml2' is required. Install it once with install.packages('xml2').")
}

# ----------------------------------------------------------------------
# Target historical gaps through 2025/26.
# 2026/27 is deliberately NOT part of this backfill.
# ----------------------------------------------------------------------

jobs <- data.frame(
  season_folder = c(
    "2021-22", "2022-23",                         # Belgium
    "2021-22", "2022-23",                         # Scotland
    "2021-22", "2022-23",                         # Turkey
    "2021-22", "2022-23",                         # Greece
    "2021-22", "2022-23", "2025-26",              # Switzerland
    "2019-20", "2021-22", "2022-23", "2025-26",  # Czechia
    "2022-23", "2025-26"                          # Ukraine
  ),
  country = c(
    "Belgium", "Belgium",
    "Scotland", "Scotland",
    "Turkey", "Turkey",
    "Greece", "Greece",
    "Switzerland", "Switzerland", "Switzerland",
    "Czechia", "Czechia", "Czechia", "Czechia",
    "Ukraine", "Ukraine"
  ),
  competition = c(
    "belgian_pro_league", "belgian_pro_league",
    "scottish_premiership", "scottish_premiership",
    "super_lig", "super_lig",
    "super_league_greece", "super_league_greece",
    "swiss_super_league", "swiss_super_league", "swiss_super_league",
    "czech_first_league", "czech_first_league", "czech_first_league", "czech_first_league",
    "ukrainian_premier_league", "ukrainian_premier_league"
  ),
  league = c(
    "Belgian Pro League", "Belgian Pro League",
    "Scottish Premiership", "Scottish Premiership",
    "Süper Lig", "Süper Lig",
    "Super League Greece", "Super League Greece",
    "Swiss Super League", "Swiss Super League", "Swiss Super League",
    "Czech First League", "Czech First League", "Czech First League", "Czech First League",
    "Ukrainian Premier League", "Ukrainian Premier League"
  ),
  wiki_title = c(
    "2021–22 Belgian First Division A",
    "2022–23 Belgian Pro League",
    "2021–22 Scottish Premiership",
    "2022–23 Scottish Premiership",
    "2021–22 Süper Lig",
    "2022–23 Süper Lig",
    "2021–22 Super League Greece",
    "2022–23 Super League Greece",
    "2021–22 Swiss Super League",
    "2022–23 Swiss Super League",
    "2025–26 Swiss Super League",
    "2019–20 Czech First League",
    "2021–22 Czech First League",
    "2022–23 Czech First League",
    "2025–26 Czech First League",
    "2022–23 Ukrainian Premier League",
    "2025–26 Ukrainian Premier League"
  ),
  of_repo = c(
    "belgium", "belgium",
    rep("europe", 15)
  ),
  of_source_folder = c(
    "belgium", "belgium",
    "scotland", "scotland",
    "turkey", "turkey",
    "greece", "greece",
    "switzerland", "switzerland", "switzerland",
    "czech-republic", "czech-republic", "czech-republic", "czech-republic",
    "ukraine", "ukraine"
  ),
  of_remote_suffix = c(
    "be1.txt", "be1.txt",
    "sco1", "sco1",
    "tr1", "tr1",
    "gr1", "gr1",
    "ch1", "ch1", "ch1",
    "cz1", "cz1", "cz1", "cz1",
    "ua1", "ua1"
  ),
  of_local_file = c(
    "1-belgian-pro-league.txt", "1-belgian-pro-league.txt",
    "1-scottish-premiership.txt", "1-scottish-premiership.txt",
    "1-super-lig.txt", "1-super-lig.txt",
    "1-super-league-greece.txt", "1-super-league-greece.txt",
    "1-swiss-super-league.txt", "1-swiss-super-league.txt", "1-swiss-super-league.txt",
    "1-czech-first-league.txt", "1-czech-first-league.txt", "1-czech-first-league.txt", "1-czech-first-league.txt",
    "1-ukrainian-premier-league.txt", "1-ukrainian-premier-league.txt"
  ),
  rsssf_url = c(
    "", "",
    "", "",
    "", "",
    "", "",
    "https://www.rsssf.org/tablesz/zwit2022.html", "", "",
    "", "", "", "",
    "", ""
  ),
  worldfootball_slug = c(
    "", "",
    "", "",
    "", "",
    "", "",
    "", "", "",
    "", "", "", "",
    "", ""
  ),
  worldfootball_rounds = c(
    0L, 0L,
    0L, 0L,
    0L, 0L,
    0L, 0L,
    0L, 0L, 0L,
    0L, 0L, 0L, 0L,
    0L, 0L
  ),
  stringsAsFactors = FALSE
)

# Optional targeted run:
#   set J_RATINGS_WIKI_BACKFILL_COMPETITION=swiss_super_league
#   set J_RATINGS_WIKI_BACKFILL_SEASON=2021-22
competition_filter <- trimws(Sys.getenv("J_RATINGS_WIKI_BACKFILL_COMPETITION", unset = ""))
season_filter <- trimws(Sys.getenv("J_RATINGS_WIKI_BACKFILL_SEASON", unset = ""))

if (nzchar(competition_filter)) {
  jobs <- jobs[jobs$competition == competition_filter, , drop = FALSE]
}
if (nzchar(season_filter)) {
  jobs <- jobs[jobs$season_folder == season_filter, , drop = FALSE]
}
if (nrow(jobs) == 0L) {
  stop("No backfill jobs match the supplied filters.")
}

WIKIPEDIA_DELAY_SECONDS <- 5
OPENFOOTBALL_DELAY_SECONDS <- 2
RSSSF_DELAY_SECONDS <- 5
WORLDFOOTBALL_DELAY_SECONDS <- 5

# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("\\[[0-9]+\\]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

club_key <- function(x) {
  x <- clean_text(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("st[.]?", "saint", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)

  # Remove common legal/club designators for matching only.
  # The actual team name written to the master remains the OpenFootball name.
  stop_tokens <- c(
    "fc", "afc", "cf", "fk", "sk", "nk", "ac", "ssc", "as",
    "sc", "sv", "rc", "rsc", "kv", "krc", "rkc",
    "club", "football", "fussball", "futbol",
    "royal", "royale", "koninklijke"
  )

  normalise_one <- function(z) {
    toks <- strsplit(z, "\\s+")[[1]]
    toks <- toks[nzchar(toks)]
    toks <- toks[!(toks %in% stop_tokens)]
    z <- paste(toks, collapse = " ")

    # Known harmless variants in the target leagues.
    z <- gsub("^grasshopper zurich$", "grasshoppers", z)
    z <- gsub("^grasshopper$", "grasshoppers", z)
    z <- gsub("^union saint gilloise$", "union gilloise", z)
    z <- gsub("^union saint gilloise$", "union gilloise", z)
    z <- gsub("^union sg$", "union gilloise", z)
    z <- gsub("^saint truiden$", "sint truiden", z)
    z <- gsub("^saint gallen$", "sankt gallen", z)
    z <- gsub("^st gallen$", "sankt gallen", z)
    z <- gsub("^zurich$", "zurich", z)
    trimws(gsub("\\s+", " ", z))
  }

  vapply(x, normalise_one, character(1))
}

result_code <- function(hg, ag) {
  if (hg > ag) "1-0" else if (hg < ag) "0-1" else "0.5-0.5"
}

season_label <- function(folder) {
  gsub("-", "/", folder, fixed = TRUE)
}

month_num <- function(mon) {
  m <- c(
    Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
    Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12
  )
  unname(m[mon])
}

infer_years <- function(folder) {
  start_year <- as.integer(substr(folder, 1, 4))
  list(start_year = start_year, end_year = start_year + 1L)
}

safe_download <- function(url, dest, user_agent, pause_seconds) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(dest) && file.info(dest)$size > 0) {
    cat("  cache: ", dest, "\n", sep = "")
    return("cached")
  }

  tmp <- paste0(dest, ".tmp")
  if (file.exists(tmp)) file.remove(tmp)

  cat("  GET:   ", url, "\n", sep = "")
  cat("  save:  ", dest, "\n", sep = "")

  ok <- tryCatch(
    {
      status <- download.file(
        url,
        tmp,
        mode = "wb",
        quiet = TRUE,
        method = "libcurl",
        headers = c("User-Agent" = user_agent)
      )
      identical(status, 0L) || identical(status, 0)
    },
    error = function(e) {
      message("  download error: ", conditionMessage(e))
      FALSE
    }
  )

  if (!ok || !file.exists(tmp) || file.info(tmp)$size == 0) {
    if (file.exists(tmp)) file.remove(tmp)
    return("failed")
  }

  file.copy(tmp, dest, overwrite = TRUE)
  file.remove(tmp)

  if (pause_seconds > 0) Sys.sleep(pause_seconds)
  "downloaded"
}

wikipedia_url <- function(title) {
  slug <- gsub(" ", "_", title, fixed = TRUE)
  paste0(
    "https://en.wikipedia.org/wiki/",
    URLencode(slug, reserved = TRUE)
  )
}

openfootball_url <- function(job) {
  if (job$of_repo == "europe") {
    remote <- paste0(
      job$of_source_folder, "/",
      job$season_folder, "_",
      job$of_remote_suffix, ".txt"
    )
    return(paste0(
      "https://raw.githubusercontent.com/openfootball/europe/master/",
      remote
    ))
  }

  paste0(
    "https://raw.githubusercontent.com/openfootball/",
    job$of_repo,
    "/master/",
    job$season_folder,
    "/",
    job$of_remote_suffix
  )
}

cell_team_name <- function(cell) {
  link <- xml2::xml_find_first(cell, ".//a")
  if (!inherits(link, "xml_missing")) {
    title <- xml2::xml_attr(link, "title")
    if (!is.na(title) && nzchar(title)) {
      return(clean_text(title))
    }
    txt <- clean_text(xml2::xml_text(link))
    if (nzchar(txt)) return(txt)
  }
  clean_text(xml2::xml_text(cell))
}

# ----------------------------------------------------------------------
# Wikipedia league result matrices
#
# A season can contain several Home/Away matrices (regular rounds, split,
# championship/relegation groups, etc.). We retain page order and number
# repeated Home/Away pairs. This lets us attach matrix scores to the same
# repeated pair in chronological order from the date skeleton.
# ----------------------------------------------------------------------

parse_wikipedia_matrices <- function(html_path) {
  doc <- xml2::read_html(html_path, encoding = "UTF-8")
  tables <- xml2::xml_find_all(doc, "//table")

  score_pat <- "^\\s*([0-9]+)\\s*[\\-\u2013\u2014]\\s*([0-9]+)\\s*$"

  rows <- list()
  k <- 1L
  matrix_no <- 0L

  for (tbl in tables) {
    first_row <- xml2::xml_find_first(tbl, ".//tr[1]")
    if (inherits(first_row, "xml_missing")) next

    first_cells <- xml2::xml_find_all(first_row, "./th|./td")
    if (length(first_cells) < 4L) next

    corner <- clean_text(xml2::xml_text(first_cells[[1]]))
    first_text <- clean_text(xml2::xml_text(first_row))

    looks_like_matrix <- (
      grepl("Home", corner, ignore.case = TRUE) &&
      grepl("Away", corner, ignore.case = TRUE)
    ) || (
      grepl("Home", first_text, ignore.case = TRUE) &&
      grepl("Away", first_text, ignore.case = TRUE) &&
      length(first_cells) >= 5L
    )

    if (!looks_like_matrix) next

    away_names <- vapply(first_cells[-1], cell_team_name, character(1))

    # Ignore false positives where most headings are clearly not team names.
    if (sum(nzchar(away_names)) < 3L) next

    matrix_no <- matrix_no + 1L
    trs <- xml2::xml_find_all(tbl, ".//tr[position()>1]")

    for (tr in trs) {
      cells <- xml2::xml_find_all(tr, "./th|./td")
      if (length(cells) < 2L) next

      home_name <- cell_team_name(cells[[1]])
      if (!nzchar(home_name)) next

      n_away <- min(length(away_names), length(cells) - 1L)

      for (j in seq_len(n_away)) {
        score_text <- clean_text(xml2::xml_text(cells[[j + 1L]]))
        if (!grepl(score_pat, score_text, perl = TRUE)) next

        hg <- as.integer(sub(score_pat, "\\1", score_text, perl = TRUE))
        ag <- as.integer(sub(score_pat, "\\2", score_text, perl = TRUE))

        rows[[k]] <- data.frame(
          MatrixNo = matrix_no,
          HomeWiki = home_name,
          AwayWiki = away_names[j],
          HomeKey = club_key(home_name),
          AwayKey = club_key(away_names[j]),
          Result = result_code(hg, ag),
          Score = sprintf("%d-%d", hg, ag),
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
  }

  if (length(rows) == 0L) {
    stop("No completed Home/Away result matrices were parsed from:\n", html_path)
  }

  out <- do.call(rbind, rows)
  out$PairKey <- paste(out$HomeKey, out$AwayKey, sep = "||")

  # Occurrence within a repeated Home/Away pairing in page/matrix order.
  out$PairOccurrence <- ave(
    seq_len(nrow(out)),
    out$PairKey,
    FUN = seq_along
  )

  exact_key <- paste(out$PairKey, out$PairOccurrence, sep = "||")
  if (anyDuplicated(exact_key)) {
    stop("Wikipedia parser generated a duplicate pair-occurrence key.")
  }

  out
}



# ----------------------------------------------------------------------
# One-off RSSSF historical date fallback.
#
# RSSSF is used here only as a dated historical fixture skeleton and
# independent score cross-check. Wikipedia remains the score source.
# ----------------------------------------------------------------------

rsssf_swiss_team <- function(x) {
  z <- clean_text(x)
  k <- tolower(iconv(z, from = "", to = "ASCII//TRANSLIT"))
  k[is.na(k)] <- tolower(z)
  k <- gsub("[^a-z0-9]+", " ", k)
  k <- trimws(gsub("\\s+", " ", k))

  if (grepl("^luzern$", k)) return("FC Luzern")
  if (grepl("^young boys", k)) return("BSC Young Boys")
  if (grepl("^lausanne", k)) return("FC Lausanne-Sport")
  if (grepl("sankt gallen|saint gallen|st gallen", k)) return("FC St. Gallen")
  if (grepl("^lugano$", k)) return("FC Lugano")
  # RSSSF's legacy encoding can render Zürich as forms such as
  # "Z�rich" / "Zï¿½rich". After transliteration these become variants
  # such as "z rich" or "zi 1 2 rich". Match the stable outer letters.
  if (grepl("^z.*rich$", k)) return("FC Zürich")
  if (grepl("^grasshopper", k)) return("Grasshopper Club Zürich")
  if (grepl("^basel$", k)) return("FC Basel")
  if (grepl("^sion$", k)) return("FC Sion")
  if (grepl("^servette", k)) return("Servette FC")

  z
}

parse_rsssf_swiss_super_league <- function(html_path, season_folder) {
  doc <- xml2::read_html(html_path)

  # RSSSF's older pages are not consistently labelled UTF-8.  Convert every
  # PRE block explicitly before doing any line-based parsing.
  pre <- xml2::xml_find_all(doc, "//pre")
  if (length(pre) == 0L) {
    stop("RSSSF page contained no <pre> result blocks:\n", html_path)
  }

  pre_text <- vapply(
    pre,
    function(node) {
      z <- xml2::xml_text(node)
      # The downloaded Swiss page is effectively a legacy single-byte page.
      # iconv() also makes strsplit() safe if xml2 leaves invalid bytes behind.
      z2 <- suppressWarnings(iconv(z, from = "latin1", to = "UTF-8", sub = "?"))
      if (is.na(z2)) z2 <- enc2utf8(z)
      z2
    },
    character(1)
  )

  raw <- paste(pre_text, collapse = "\n")
  raw <- gsub("\r\n?", "\n", raw, perl = TRUE)
  lines <- unlist(strsplit(raw, "\n", fixed = TRUE), use.names = FALSE)
  lines <- trimws(gsub("[\t ]+", " ", lines))

  # The Super League is the first round-by-round competition on this page.
  # Do not depend on the "Super League" heading being inside <pre>; on RSSSF
  # it is an HTML heading immediately before the preformatted results.
  round1 <- which(grepl("^Round 1$", lines, ignore.case = TRUE))
  round36 <- which(grepl("^Round 36$", lines, ignore.case = TRUE))

  if (length(round1) == 0L || length(round36) == 0L) {
    cat("\nRSSSF headings detected near start of PRE text:\n")
    print(head(lines[nzchar(lines)], 80), quote = FALSE)
    stop(
      "Could not locate Round 1 through Round 36 in the RSSSF Swiss page. ",
      "Nothing will be written."
    )
  }

  start_i <- round1[1L]
  end_candidates <- round36[round36 >= start_i]
  if (length(end_candidates) == 0L) {
    stop("Could not locate Swiss Super League Round 36 after Round 1.")
  }

  # Include everything after the Round 36 heading until the next obvious
  # competition/summary boundary.  The five Round 36 matches occur before it.
  end_i <- min(length(lines), end_candidates[1L] + 80L)
  lines <- lines[start_i:end_i]

  # Stop once another competition begins, if that heading appears in PRE text.
  next_comp <- which(
    grepl(
      "^(Challenge League|Promotion League|1\\. Liga|Schweizer Cup|Cup)$",
      lines,
      ignore.case = TRUE
    )
  )
  if (length(next_comp) > 0L && next_comp[1L] > 1L) {
    lines <- lines[seq_len(next_comp[1L] - 1L)]
  }

  yrs <- infer_years(season_folder)
  month_lookup <- c(
    jan=1L, feb=2L, mar=3L, apr=4L, may=5L, jun=6L,
    jul=7L, aug=8L, sep=9L, oct=10L, nov=11L, dec=12L
  )

  current_date <- as.Date(NA)
  rows <- list()
  k <- 1L

  date_pat <- "^\\[([A-Za-z]{3,9})\\s+(\\d{1,2})(?:st|nd|rd|th)?\\]$"
  # RSSSF top-flight result rows are "Home 3-1 Away".
  match_pat <- "^(.+?)\\s+(\\d+)\\s*-\\s*(\\d+)\\s+(.+?)$"

  for (ln_clean in lines) {
    if (!nzchar(ln_clean)) next

    if (grepl(date_pat, ln_clean, ignore.case = TRUE, perl = TRUE)) {
      mon_txt <- tolower(substr(
        sub(date_pat, "\\1", ln_clean, ignore.case = TRUE, perl = TRUE),
        1L, 3L
      ))
      dd <- as.integer(sub(
        date_pat, "\\2", ln_clean,
        ignore.case = TRUE, perl = TRUE
      ))
      mm <- unname(month_lookup[mon_txt])

      if (is.na(mm)) next
      yy <- if (mm >= 7L) yrs$start_year else yrs$end_year
      current_date <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
      next
    }

    if (is.na(current_date)) next
    if (grepl("^Round\\s+\\d+$", ln_clean, ignore.case = TRUE)) next
    if (grepl("^Halfway Table:|^Final Table:", ln_clean, ignore.case = TRUE)) next
    if (grepl("^\\[", ln_clean)) next  # scorer annotation rows
    if (!grepl(match_pat, ln_clean, perl = TRUE)) next

    home <- trimws(sub(match_pat, "\\1", ln_clean, perl = TRUE))
    hg <- as.integer(sub(match_pat, "\\2", ln_clean, perl = TRUE))
    ag <- as.integer(sub(match_pat, "\\3", ln_clean, perl = TRUE))
    away <- trimws(sub(match_pat, "\\4", ln_clean, perl = TRUE))

    home <- rsssf_swiss_team(home)
    away <- rsssf_swiss_team(away)

    rows[[k]] <- data.frame(
      Date = format(current_date, "%Y-%m-%d"),
      Home = home,
      Away = away,
      Result = result_code(hg, ag),
      Score = sprintf("%d-%d", hg, ag),
      stringsAsFactors = FALSE
    )
    k <- k + 1L
  }

  if (length(rows) == 0L) {
    stop("RSSSF Swiss parser produced no dated matches.")
  }

  out <- do.call(rbind, rows)

  valid_teams <- c(
    "FC Luzern", "BSC Young Boys", "FC Lausanne-Sport", "FC St. Gallen",
    "FC Lugano", "FC Zürich", "Grasshopper Club Zürich", "FC Basel",
    "FC Sion", "Servette FC"
  )

  out <- out[
    out$Home %in% valid_teams & out$Away %in% valid_teams,
    ,
    drop = FALSE
  ]
  out <- out[!duplicated(out), , drop = FALSE]
  out <- out[order(out$Date, out$Home, out$Away), , drop = FALSE]

  if (nrow(out) != 180L) {
    cat("\nRSSSF Swiss parsed rows:", nrow(out), "\n")
    cat("Parsed team names:\n")
    print(sort(unique(c(out$Home, out$Away))), quote = FALSE)
    stop(
      "Expected exactly 180 dated Swiss Super League matches from RSSSF. ",
      "Nothing will be written."
    )
  }

  out$HomeKey <- club_key(out$Home)
  out$AwayKey <- club_key(out$Away)
  out$PairKey <- paste(out$HomeKey, out$AwayKey, sep = "||")
  out$PairOccurrence <- ave(
    seq_len(nrow(out)),
    out$PairKey,
    FUN = seq_along
  )

  out
}

download_rsssf_season <- function(job) {
  url <- as.character(job$rsssf_url)
  if (!nzchar(url)) return(data.frame())

  cache_dir <- file.path(
    repo_dir,
    "EuropeanFootball",
    "pipeline_data",
    "Source",
    "rsssf",
    job$competition,
    job$season_folder
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  f <- file.path(cache_dir, "page.html")

  status <- safe_download(
    url,
    f,
    user_agent = "J-Ratings/1.0 (one-off historical football data repair)",
    pause_seconds = RSSSF_DELAY_SECONDS
  )

  if (identical(status, "failed")) {
    stop(
      "RSSSF historical date fallback failed for ",
      job$competition, " ", job$season_folder,
      ". Nothing will be written."
    )
  }

  parse_rsssf_swiss_super_league(f, job$season_folder)
}


# ----------------------------------------------------------------------
# One-off WorldFootball.net historical date fallback.
#
# This is NOT intended to become a live/current-season dependency.
# It is used only where the historical OpenFootball date file is absent.
# We fetch one round page at a time, sleep generously, cache every page,
# and use it only as a dated fixture skeleton. Wikipedia remains the
# score source; WorldFootball scores are used only as a cross-check.
# ----------------------------------------------------------------------

parse_worldfootball_round <- function(html_path) {
  doc <- xml2::read_html(html_path, encoding = "UTF-8")
  trs <- xml2::xml_find_all(doc, "//tr")

  rows <- list()
  k <- 1L
  carried_date <- ""

  date_pat <- "^\\d{2}/\\d{2}/\\d{4}$"
  score_pat <- "^(\\d+)\\s*:\\s*(\\d+)"

  for (tr in trs) {
    cells <- xml2::xml_find_all(tr, "./th|./td")
    if (length(cells) < 4L) next

    vals <- vapply(cells, function(x) clean_text(xml2::xml_text(x)), character(1))

    date_candidates <- vals[grepl(date_pat, vals)]
    if (length(date_candidates) > 0L) {
      carried_date <- date_candidates[1]
    }
    if (!nzchar(carried_date)) next

    score_idx <- which(grepl(score_pat, vals, perl = TRUE))
    if (length(score_idx) == 0L) next
    score_idx <- score_idx[1]

    # Team links on WorldFootball round pages point to /teams/... .
    team_links <- xml2::xml_find_all(tr, ".//a[contains(@href, '/teams/')]")
    team_names <- clean_text(xml2::xml_text(team_links))
    team_names <- team_names[nzchar(team_names)]
    team_names <- team_names[!duplicated(team_names)]

    if (length(team_names) < 2L) {
      # Fallback for layout changes: infer team-name cells around the dash.
      dash_idx <- which(vals %in% c("-", "–", "—"))
      if (length(dash_idx) > 0L) {
        d <- dash_idx[1]
        left <- vals[seq_len(max(1L, d - 1L))]
        right <- vals[seq.int(min(length(vals), d + 1L), length(vals))]

        junk <- function(z) {
          !nzchar(z) |
            grepl("^\\d{2}/\\d{2}/\\d{4}$", z) |
            grepl("^\\d{1,2}:\\d{2}$", z) |
            grepl("^\\d+\\s*:\\s*\\d+", z) |
            z %in% c("-", "–", "—", "Ende", "Finished")
        }

        left <- left[!junk(left)]
        right <- right[!junk(right)]

        if (length(left) > 0L && length(right) > 0L) {
          team_names <- c(tail(left, 1L), head(right, 1L))
        }
      }
    }

    if (length(team_names) < 2L) next

    score_text <- vals[score_idx]
    hg <- as.integer(sub(score_pat, "\\1", score_text, perl = TRUE))
    ag <- as.integer(sub(score_pat, "\\2", score_text, perl = TRUE))

    d <- as.Date(carried_date, format = "%d/%m/%Y")

    rows[[k]] <- data.frame(
      Date = format(d, "%Y-%m-%d"),
      Home = team_names[1],
      Away = team_names[2],
      Result = result_code(hg, ag),
      Score = sprintf("%d-%d", hg, ag),
      stringsAsFactors = FALSE
    )
    k <- k + 1L
  }

  if (length(rows) == 0L) return(data.frame())
  do.call(rbind, rows)
}

download_worldfootball_season <- function(job) {
  slug <- as.character(job$worldfootball_slug)
  rounds <- as.integer(job$worldfootball_rounds)

  if (!nzchar(slug) || is.na(rounds) || rounds < 1L) {
    return(data.frame())
  }

  cache_dir <- file.path(
    repo_dir,
    "EuropeanFootball",
    "pipeline_data",
    "Source",
    "worldfootball",
    job$competition,
    job$season_folder
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  parts <- list()

  for (round_no in seq_len(rounds)) {
    f <- file.path(cache_dir, sprintf("round_%02d.html", round_no))
    url <- paste0(
      "https://www.worldfootball.net/schedule/",
      slug,
      "-spieltag/",
      round_no,
      "/"
    )

    status <- safe_download(
      url,
      f,
      user_agent = "J-Ratings/1.0 (one-off historical football data repair)",
      pause_seconds = WORLDFOOTBALL_DELAY_SECONDS
    )

    if (identical(status, "failed")) {
      stop(
        "WorldFootball historical date fallback failed at round ",
        round_no, " for ", job$competition, " ", job$season_folder,
        ". Nothing will be written."
      )
    }

    tmp <- parse_worldfootball_round(f)
    if (nrow(tmp) == 0L) {
      stop(
        "WorldFootball round page parsed zero matches:\n",
        f,
        "\nNothing will be written."
      )
    }

    cat(
      "    WorldFootball round ", round_no,
      ": ", nrow(tmp), " matches\n",
      sep = ""
    )
    parts[[length(parts) + 1L]] <- tmp
  }

  out <- do.call(rbind, parts)
  out <- out[!duplicated(out), , drop = FALSE]
  out <- out[order(out$Date, out$Home, out$Away), , drop = FALSE]

  out$HomeKey <- club_key(out$Home)
  out$AwayKey <- club_key(out$Away)
  out$PairKey <- paste(out$HomeKey, out$AwayKey, sep = "||")
  out$PairOccurrence <- ave(
    seq_len(nrow(out)),
    out$PairKey,
    FUN = seq_along
  )

  out
}


# ----------------------------------------------------------------------
# Lightweight OpenFootball fixture/date parser.
# It deliberately uses OpenFootball only for date + Home/Away ordering.
# Scores from this parser are NOT used as the imported result.
# ----------------------------------------------------------------------

parse_openfootball_fixture_dates <- function(txt_path, folder) {
  lines <- readLines(txt_path, warn = FALSE, encoding = "UTF-8")
  yrs <- infer_years(folder)

  current_date <- as.Date(NA)
  out_date <- character()
  out_home <- character()
  out_away <- character()

  append_fixture <- function(home, away) {
    home <- clean_text(home)
    away <- clean_text(away)

    # Association suffix sometimes used in continental files; harmless here too.
    home <- sub("\\s+\\([A-Z]{3}\\)\\s*$", "", home)
    away <- sub("\\s+\\([A-Z]{3}\\)\\s*$", "", away)

    if (!nzchar(home) || !nzchar(away) || is.na(current_date)) return(invisible(NULL))

    out_date <<- c(out_date, format(current_date, "%Y-%m-%d"))
    out_home <<- c(out_home, home)
    out_away <<- c(out_away, away)
    invisible(NULL)
  }

  date_pat_bracket <- "^\\s*\\[[A-Za-z]{3}\\s+([A-Za-z]{3})(?:/|\\s+)(\\d{1,2})(?:\\s+(\\d{4}))?\\]\\s*$"
  date_pat_plain <- "^\\s*[A-Za-z]{3}\\s+([A-Za-z]{3})(?:/|\\s+)(\\d{1,2})(?:\\s+(\\d{4}))?\\s*$"

  for (ln in lines) {
    ln <- gsub("\\t", " ", ln)
    ln <- gsub("\u00a0", " ", ln, fixed = TRUE)
    ln <- trimws(gsub("\\s+", " ", ln))

    if (!nzchar(ln) || grepl("^#", ln) || grepl("^==", ln)) next
    if (grepl("Matchday", ln, ignore.case = TRUE)) next

    if (grepl(date_pat_bracket, ln)) {
      mon <- sub(date_pat_bracket, "\\1", ln)
      dd <- as.integer(sub(date_pat_bracket, "\\2", ln))
      yy_txt <- sub(date_pat_bracket, "\\3", ln)
      mm <- month_num(mon)
      yy <- suppressWarnings(as.integer(yy_txt))
      if (is.na(yy)) yy <- if (!is.na(mm) && mm >= 7L) yrs$start_year else yrs$end_year
      current_date <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
      next
    }

    if (grepl(date_pat_plain, ln) && !grepl("^\\d{1,2}[:.]\\d{2}\\s+", ln)) {
      mon <- sub(date_pat_plain, "\\1", ln)
      dd <- as.integer(sub(date_pat_plain, "\\2", ln))
      yy_txt <- sub(date_pat_plain, "\\3", ln)
      mm <- month_num(mon)
      yy <- suppressWarnings(as.integer(yy_txt))
      if (is.na(yy)) yy <- if (!is.na(mm) && mm >= 7L) yrs$start_year else yrs$end_year
      current_date <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
      next
    }

    if (is.na(current_date)) next

    if (grepl("\\[(cancelled|postponed|abandoned)\\]", ln, ignore.case = TRUE)) {
      next
    }

    ln <- gsub("\\s*\\[awarded\\]\\s*", " ", ln, ignore.case = TRUE)
    ln <- trimws(gsub("\\s+", " ", ln))
    ln <- sub("^\\d{1,2}[:.]\\d{2}\\s+", "", ln)

    # Modern: Home v Away [optional metadata/result]
    if (grepl("\\s+v\\s+", ln)) {
      vpos <- regexpr("\\s+v\\s+", ln)
      home <- trimws(substr(ln, 1, vpos[1] - 1))
      rhs <- trimws(substr(ln, vpos[1] + attr(vpos, "match.length"), nchar(ln)))

      # Remove trailing score/penalty/aggregate material from away side.
      rhs <- sub(
        "\\s+\\([^)]*\\)\\s+\\d+\\s*-\\s*\\d+\\s*$",
        "",
        rhs,
        perl = TRUE
      )
      rhs <- sub(
        "\\s+\\d+\\s*-\\s*\\d+\\s+pen\\.?\\s+\\d+\\s*-\\s*\\d+.*$",
        "",
        rhs,
        ignore.case = TRUE,
        perl = TRUE
      )
      rhs <- sub(
        "\\s+\\d+\\s*-\\s*\\d+(?:\\s+a\\.e\\.t\\.)?(?:\\s+\\([^)]*\\))?\\s*$",
        "",
        rhs,
        ignore.case = TRUE,
        perl = TRUE
      )
      rhs <- sub("\\s+\\[[^]]+\\]\\s*$", "", rhs)
      away <- trimws(rhs)

      append_fixture(home, away)
      next
    }

    # Older score-in-middle: Home 2-1 Away
    old_pat <- "^(.+?)\\s+(\\d+)\\s*-\\s*(\\d+)(?:\\s+\\([^)]*\\))?\\s+(.+?)\\s*$"
    if (grepl(old_pat, ln, perl = TRUE)) {
      home <- trimws(sub(old_pat, "\\1", ln, perl = TRUE))
      away <- trimws(sub(old_pat, "\\4", ln, perl = TRUE))
      append_fixture(home, away)
      next
    }
  }

  if (length(out_date) == 0L) {
    stop("No fixture dates could be parsed from OpenFootball file:\n", txt_path)
  }

  out <- data.frame(
    Date = out_date,
    Home = out_home,
    Away = out_away,
    stringsAsFactors = FALSE
  )

  out <- out[order(out$Date, out$Home, out$Away), , drop = FALSE]
  out$HomeKey <- club_key(out$Home)
  out$AwayKey <- club_key(out$Away)
  out$PairKey <- paste(out$HomeKey, out$AwayKey, sep = "||")
  out$PairOccurrence <- ave(
    seq_len(nrow(out)),
    out$PairKey,
    FUN = seq_along
  )
  out
}

existing_fixture_dates <- function(master, job) {
  idx <- which(
    master$Season == season_label(job$season_folder) &
    master$Country == job$country &
    master$Competition == job$competition &
    nzchar(trimws(master$Date)) &
    nzchar(trimws(master$Home)) &
    nzchar(trimws(master$Away))
  )

  if (length(idx) == 0L) {
    return(data.frame())
  }

  out <- master[idx, c("Date", "Home", "Away"), drop = FALSE]
  out <- out[!duplicated(out), , drop = FALSE]
  out <- out[order(out$Date, out$Home, out$Away), , drop = FALSE]
  out$HomeKey <- club_key(out$Home)
  out$AwayKey <- club_key(out$Away)
  out$PairKey <- paste(out$HomeKey, out$AwayKey, sep = "||")
  out$PairOccurrence <- ave(
    seq_len(nrow(out)),
    out$PairKey,
    FUN = seq_along
  )
  out
}

attach_dates <- function(wiki, fixtures, job) {
  if (nrow(fixtures) == 0L) {
    stop(
      "No date skeleton is available for ",
      job$competition, " ", job$season_folder, "."
    )
  }

  wiki_key <- paste(wiki$PairKey, wiki$PairOccurrence, sep = "||")
  fixture_key <- paste(fixtures$PairKey, fixtures$PairOccurrence, sep = "||")

  m <- match(wiki_key, fixture_key)

  if (any(is.na(m))) {
    bad <- wiki[is.na(m), c(
      "MatrixNo", "HomeWiki", "AwayWiki", "Score",
      "HomeKey", "AwayKey", "PairOccurrence"
    ), drop = FALSE]

    cat("\nUnmatched Wikipedia result rows:\n")
    print(bad, row.names = FALSE)

    cat("\nAvailable fixture-team keys:\n")
    print(
      sort(unique(c(fixtures$HomeKey, fixtures$AwayKey))),
      quote = FALSE
    )

    stop(
      "Could not attach dates to every Wikipedia result for ",
      job$competition, " ", job$season_folder,
      ". Nothing will be written."
    )
  }

  if ("Score" %in% names(fixtures)) {
    fixture_scores <- trimws(as.character(fixtures$Score[m]))
    wiki_scores <- trimws(as.character(wiki$Score))
    score_conflict <- nzchar(fixture_scores) & fixture_scores != wiki_scores

    if (any(score_conflict)) {
      cat("\nWikipedia/date-source score conflicts:\n")
      print(
        data.frame(
          Date = fixtures$Date[m][score_conflict],
          Home = fixtures$Home[m][score_conflict],
          Away = fixtures$Away[m][score_conflict],
          WikipediaScore = wiki_scores[score_conflict],
          DateSourceScore = fixture_scores[score_conflict],
          stringsAsFactors = FALSE
        ),
        row.names = FALSE
      )
      stop(
        "Wikipedia scores conflict with the historical date source. ",
        "Nothing will be written."
      )
    }

    cat("  Score cross-check:", sum(nzchar(fixture_scores)), "matches verified\n")
  }

  data.frame(
    Season = season_label(job$season_folder),
    Country = job$country,
    Competition = job$competition,
    CompetitionType = "league",
    Tier = 1L,
    League = job$league,
    Date = fixtures$Date[m],
    Home = fixtures$Home[m],
    Away = fixtures$Away[m],
    Result = wiki$Result,
    Score = wiki$Score,
    Source = "wikipedia",
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------
# Load master once.
# ----------------------------------------------------------------------

master <- read.csv(
  combined_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c()
)

required_cols <- c(
  "Season", "Country", "Competition", "CompetitionType", "Tier", "League",
  "Date", "Home", "Away", "Result", "Score", "Source"
)

missing_cols <- setdiff(required_cols, names(master))
if (length(missing_cols) > 0L) {
  stop(
    "Master CSV is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

cat("Wikipedia recent historical-gap backfill\n")
cat("Master:", combined_file, "\n")
cat("Jobs:", nrow(jobs), "\n")
cat("Wikipedia request delay:", WIKIPEDIA_DELAY_SECONDS, "seconds\n")
cat("OpenFootball fallback delay:", OPENFOOTBALL_DELAY_SECONDS, "seconds\n")
cat("RSSSF one-off fallback delay:", RSSSF_DELAY_SECONDS, "seconds\n")
cat("WorldFootball one-off fallback delay:", WORLDFOOTBALL_DELAY_SECONDS, "seconds\n\n")

all_imports <- list()
audit_rows <- list()
a <- 1L

for (i in seq_len(nrow(jobs))) {
  job_started <- Sys.time()
  job <- jobs[i, , drop = FALSE]

  cat(
    "\n[", i, "/", nrow(jobs), "] ",
    job$competition, " ", job$season_folder, "\n",
    sep = ""
  )

  wiki_file <- file.path(
    wiki_root,
    job$competition,
    job$season_folder,
    "page.html"
  )

  wiki_status <- safe_download(
    wikipedia_url(job$wiki_title),
    wiki_file,
    user_agent = "J-Ratings/1.0 (historical football data backfill; personal statistics project)",
    pause_seconds = WIKIPEDIA_DELAY_SECONDS
  )

  if (identical(wiki_status, "failed")) {
    stop("Wikipedia download failed for: ", job$wiki_title)
  }

  wiki <- parse_wikipedia_matrices(wiki_file)
  cat("  Wikipedia completed matrix results:", nrow(wiki), "\n")

  # Prefer a fixture skeleton already present in the master, if it exists.
  fixtures <- existing_fixture_dates(master, job)
  date_source <- "master-existing"

  if (nrow(fixtures) == 0L) {
    of_file <- file.path(
      openfootball_root,
      job$of_source_folder,
      job$season_folder,
      job$of_local_file
    )

    of_ok <- file.exists(of_file) && file.info(of_file)$size > 0
    skip_known_missing_of <- (
      job$competition == "swiss_super_league" &&
      job$season_folder == "2021-22" &&
      nzchar(as.character(job$rsssf_url))
    )

    if (!of_ok && skip_known_missing_of) {
      cat("  Known-missing OpenFootball Swiss 2021-22 file; skipping repeat 404 request.\n")
    } else if (!of_ok) {
      cat("  OpenFootball date cache missing; trying one targeted date-file request.\n")
      of_status <- safe_download(
        openfootball_url(job),
        of_file,
        user_agent = "J-Ratings/1.0",
        pause_seconds = OPENFOOTBALL_DELAY_SECONDS
      )
      of_ok <- !identical(of_status, "failed")
    } else {
      cat("  OpenFootball date cache:", of_file, "\n")
    }

    if (of_ok) {
      fixtures <- parse_openfootball_fixture_dates(of_file, job$season_folder)
      date_source <- "openfootball"
    } else if (nzchar(as.character(job$rsssf_url))) {
      cat(
        "  OpenFootball historical file unavailable.\n",
        "  Using one-off RSSSF historical page for dates and score cross-check.\n",
        sep = ""
      )
      fixtures <- download_rsssf_season(job)
      date_source <- "rsssf"
    } else if (nzchar(as.character(job$worldfootball_slug))) {
      cat(
        "  OpenFootball historical file unavailable.\n",
        "  Using one-off WorldFootball round pages for dates and score cross-check.\n",
        sep = ""
      )
      fixtures <- download_worldfootball_season(job)
      date_source <- "worldfootball"
    } else {
      stop(
        "No existing master fixture skeleton and no usable historical date fallback for ",
        job$competition, " ", job$season_folder,
        ". Nothing will be written."
      )
    }
  }

  cat("  Date skeleton rows:", nrow(fixtures), " | source:", date_source, "\n")

  imported <- attach_dates(wiki, fixtures, job)

  # Internal uniqueness.
  import_key <- paste(
    imported$Season, imported$Country, imported$Competition,
    imported$Date, club_key(imported$Home), club_key(imported$Away),
    sep = "||"
  )

  if (anyDuplicated(import_key)) {
    dup <- imported[
      duplicated(import_key) | duplicated(import_key, fromLast = TRUE),
      ,
      drop = FALSE
    ]
    cat("\nDuplicate dated imported matches:\n")
    print(dup, row.names = FALSE)
    stop("Backfill import generated duplicate dated match keys.")
  }

  all_imports[[length(all_imports) + 1L]] <- imported

  audit_rows[[a]] <- data.frame(
    Competition = job$competition,
    Season = season_label(job$season_folder),
    WikipediaRows = nrow(wiki),
    DateSkeletonRows = nrow(fixtures),
    DateSource = date_source,
    WikipediaCache = wiki_status,
    ElapsedSeconds = round(
      as.numeric(difftime(Sys.time(), job_started, units = "secs")),
      1
    ),
    stringsAsFactors = FALSE
  )
  a <- a + 1L

  cat(
    "  job elapsed:",
    round(as.numeric(difftime(Sys.time(), job_started, units = "secs")), 1),
    "seconds\n"
  )
}

imports <- do.call(rbind, all_imports)
audit <- do.call(rbind, audit_rows)

cat("\n==============================\n")
cat("PRE-WRITE QA\n")
cat("==============================\n")
print(audit, row.names = FALSE)
cat("\nTotal Wikipedia results prepared:", nrow(imports), "\n")

# ----------------------------------------------------------------------
# Reconcile imports against master.
# Match by season/country/competition/date + normalised Home/Away.
# Existing rows are preserved; blank scores can be filled.
# Conflicts stop the run.
# ----------------------------------------------------------------------

master_match_key <- paste(
  master$Season,
  master$Country,
  master$Competition,
  master$Date,
  club_key(master$Home),
  club_key(master$Away),
  sep = "||"
)

import_match_key <- paste(
  imports$Season,
  imports$Country,
  imports$Competition,
  imports$Date,
  club_key(imports$Home),
  club_key(imports$Away),
  sep = "||"
)

if (anyDuplicated(import_match_key)) {
  stop("Duplicate match keys exist within the prepared import.")
}

m <- match(import_match_key, master_match_key)

new_rows <- imports[is.na(m), , drop = FALSE]
existing_imports <- imports[!is.na(m), , drop = FALSE]
existing_idx <- m[!is.na(m)]

conflict <- logical(length(existing_idx))
filled <- logical(length(existing_idx))
verified <- logical(length(existing_idx))

if (length(existing_idx) > 0L) {
  old_score <- trimws(master$Score[existing_idx])
  new_score <- trimws(existing_imports$Score)

  conflict <- nzchar(old_score) & old_score != new_score
  filled <- !nzchar(old_score)
  verified <- nzchar(old_score) & old_score == new_score

  if (any(conflict)) {
    cat("\nCONFLICTING EXISTING SCORES:\n")
    conflict_table <- data.frame(
      Season = existing_imports$Season[conflict],
      Competition = existing_imports$Competition[conflict],
      Date = existing_imports$Date[conflict],
      Home = existing_imports$Home[conflict],
      Away = existing_imports$Away[conflict],
      ExistingScore = old_score[conflict],
      WikipediaScore = new_score[conflict],
      stringsAsFactors = FALSE
    )
    print(conflict_table, row.names = FALSE)
    stop("Score conflicts detected. Master CSV has NOT been changed.")
  }
}

cat("New match rows to append:", nrow(new_rows), "\n")
cat("Existing blank-result rows to fill:", sum(filled), "\n")
cat("Existing scored rows verified:", sum(verified), "\n")
cat("Score conflicts:", sum(conflict), "\n")

# Apply fills to existing rows.
if (length(existing_idx) > 0L && any(filled)) {
  idx_to_fill <- existing_idx[filled]
  source_rows <- existing_imports[filled, , drop = FALSE]

  master$Result[idx_to_fill] <- source_rows$Result
  master$Score[idx_to_fill] <- source_rows$Score
  master$Source[idx_to_fill] <- "wikipedia"
}

# Append genuinely absent matches.
if (nrow(new_rows) > 0L) {
  master <- rbind(master, new_rows[, required_cols, drop = FALSE])
}

# Final duplicate/conflict QA.
master_key_after <- paste(
  master$Season,
  master$Country,
  master$Competition,
  master$Date,
  club_key(master$Home),
  club_key(master$Away),
  sep = "||"
)

if (anyDuplicated(master_key_after)) {
  dup_idx <- duplicated(master_key_after) | duplicated(master_key_after, fromLast = TRUE)
  dup <- master[dup_idx, required_cols, drop = FALSE]

  # A duplicate is only harmless if its populated scores do not disagree.
  split_scores <- split(trimws(dup$Score), master_key_after[dup_idx])
  bad_keys <- names(Filter(function(z) length(unique(z[nzchar(z)])) > 1L, split_scores))

  if (length(bad_keys) > 0L) {
    cat("\nConflicting duplicate rows after reconciliation:\n")
    print(dup[master_key_after[dup_idx] %in% bad_keys, ], row.names = FALSE)
    stop("Conflicting duplicate match rows remain. Master CSV has NOT been changed.")
  }

  # Prefer a scored row; otherwise first row.
  has_score <- nzchar(trimws(master$Score))
  original_order <- seq_len(nrow(master))
  ord <- order(master_key_after, -as.integer(has_score), original_order)
  master <- master[ord, , drop = FALSE]
  master_key_after <- master_key_after[ord]
  master <- master[!duplicated(master_key_after), , drop = FALSE]
}

# Preserve the normal canonical master ordering.
master <- master[, required_cols, drop = FALSE]
master <- master[
  order(
    master$Date,
    master$Country,
    master$Competition,
    master$Home,
    master$Away
  ),
  ,
  drop = FALSE
]

# ----------------------------------------------------------------------
# Backup + atomic-ish write only after ALL jobs have passed.
# ----------------------------------------------------------------------

stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
backup_file <- file.path(
  dirname(combined_file),
  paste0("european_football_all_matches.backup_", stamp, ".csv")
)

if (!file.copy(combined_file, backup_file, overwrite = FALSE)) {
  stop("Could not create master CSV backup. Refusing to write.")
}

tmp_file <- paste0(combined_file, ".tmp")
if (file.exists(tmp_file)) file.remove(tmp_file)

write.csv(
  master,
  tmp_file,
  row.names = FALSE,
  na = ""
)

if (!file.exists(tmp_file) || file.info(tmp_file)$size == 0) {
  stop("Temporary master CSV write failed. Original master remains intact.")
}

if (!file.copy(tmp_file, combined_file, overwrite = TRUE)) {
  stop(
    "Could not replace master CSV. Backup is at:\n",
    backup_file
  )
}
file.remove(tmp_file)

audit_file <- file.path(
  audit_dir,
  paste0("wikipedia_recent_gap_backfill_", stamp, ".csv")
)
write.csv(audit, audit_file, row.names = FALSE)

cat("\n==============================\n")
cat("BACKFILL COMPLETE\n")
cat("==============================\n")
cat("Updated master:", combined_file, "\n")
cat("Backup:", backup_file, "\n")
cat("Audit:", audit_file, "\n")
cat("Rows now in master:", nrow(master), "\n")
cat(
  "Total elapsed:",
  round(as.numeric(difftime(Sys.time(), script_started, units = "secs")), 1),
  "seconds\n"
)
