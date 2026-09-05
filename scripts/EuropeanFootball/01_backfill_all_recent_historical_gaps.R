options(stringsAsFactors = FALSE)

# J-Ratings — one-off recent historical league-gap backfill
#
# Purpose:
#   Fill the known recent gaps through 2025/26 before freezing the Elo baseline.
#
# Sources:
#   Wikipedia -> result matrices (Home, Away, Score)
#   RSSSF     -> dated historical match listings (Date, Home, Away, Score)
#
# Safety:
#   * 5-second courtesy delay between uncached requests to either site.
#   * Every downloaded page is cached.
#   * RSSSF scores must cross-check against Wikipedia before a season can pass.
#   * Existing nonblank master scores may not conflict.
#   * Failed seasons are reported and skipped; passing seasons can still be written.
#   * Timestamped master backup before write.
#   * No Elo mechanics are touched.
#
# Optional filters:
#   set J_RATINGS_BACKFILL_COMPETITION=belgian_pro_league
#   set J_RATINGS_BACKFILL_SEASON=2021-22
#
# Optional dry run:
#   set J_RATINGS_BACKFILL_DRY_RUN=1

started <- Sys.time()

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

master_file <- file.path(
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

rsssf_root <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "rsssf",
  "historical_gap_backfill"
)

audit_dir <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Audits"
)

dir.create(wiki_root, recursive = TRUE, showWarnings = FALSE)
dir.create(rsssf_root, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(master_file)) {
  stop("Master combined CSV not found:\n", master_file)
}

if (!requireNamespace("xml2", quietly = TRUE)) {
  stop("Package 'xml2' is required. Install once with install.packages('xml2').")
}

REQUEST_DELAY_SECONDS <- 5

# ----------------------------------------------------------------------
# Remaining known recent historical gaps. Swiss 2021/22 is intentionally
# omitted because it was already reconstructed and independently verified.
# ----------------------------------------------------------------------

jobs <- data.frame(
  season_folder = c(
    "2022-23", "2025-26",                         # Switzerland
    "2021-22", "2022-23",                         # Belgium
    "2021-22", "2022-23",                         # Scotland
    "2021-22", "2022-23",                         # Turkey
    "2021-22", "2022-23",                         # Greece
    "2019-20", "2021-22", "2022-23", "2025-26",  # Czechia
    "2022-23", "2025-26"                          # Ukraine
  ),
  country = c(
    "Switzerland", "Switzerland",
    "Belgium", "Belgium",
    "Scotland", "Scotland",
    "Turkey", "Turkey",
    "Greece", "Greece",
    "Czechia", "Czechia", "Czechia", "Czechia",
    "Ukraine", "Ukraine"
  ),
  competition = c(
    "swiss_super_league", "swiss_super_league",
    "belgian_pro_league", "belgian_pro_league",
    "scottish_premiership", "scottish_premiership",
    "super_lig", "super_lig",
    "super_league_greece", "super_league_greece",
    "czech_first_league", "czech_first_league",
    "czech_first_league", "czech_first_league",
    "ukrainian_premier_league", "ukrainian_premier_league"
  ),
  league = c(
    "Swiss Super League", "Swiss Super League",
    "Belgian Pro League", "Belgian Pro League",
    "Scottish Premiership", "Scottish Premiership",
    "Süper Lig", "Süper Lig",
    "Super League Greece", "Super League Greece",
    "Czech First League", "Czech First League",
    "Czech First League", "Czech First League",
    "Ukrainian Premier League", "Ukrainian Premier League"
  ),
  wiki_title = c(
    "2022–23 Swiss Super League",
    "2025–26 Swiss Super League",
    "2021–22 Belgian First Division A",
    "2022–23 Belgian Pro League",
    "2021–22 Scottish Premiership",
    "2022–23 Scottish Premiership",
    "2021–22 Süper Lig",
    "2022–23 Süper Lig",
    "2021–22 Super League Greece",
    "2022–23 Super League Greece",
    "2019–20 Czech First League",
    "2021–22 Czech First League",
    "2022–23 Czech First League",
    "2025–26 Czech First League",
    "2022–23 Ukrainian Premier League",
    "2025–26 Ukrainian Premier League"
  ),
  rsssf_url = c(
    "https://www.rsssf.org/tablesz/zwit2023.html",
    "https://www.rsssf.org/tablesz/zwit2026.html",
    "https://www.rsssf.org/tablesb/belg2022.html",
    "https://www.rsssf.org/tablesb/belg2023.html",
    "https://www.rsssf.org/tabless/scot2022.html",
    "https://www.rsssf.org/tabless/scot2023.html",
    "https://www.rsssf.org/tablest/tur2022.html",
    "https://www.rsssf.org/tablest/tur2023.html",
    "https://www.rsssf.org/tablesg/grk2022.html",
    "https://www.rsssf.org/tablesg/grk2023.html",
    "https://www.rsssf.org/tablest/tsje2020.html",
    "https://www.rsssf.org/tablest/tsje2022.html",
    "https://www.rsssf.org/tablest/tsje2023.html",
    "https://www.rsssf.org/tablest/tsje2026.html",
    "https://www.rsssf.org/tableso/oekr2023.html",
    "https://www.rsssf.org/tableso/oekr2026.html"
  ),
  stringsAsFactors = FALSE
)

competition_filter <- trimws(Sys.getenv("J_RATINGS_BACKFILL_COMPETITION", unset = ""))
season_filter <- trimws(Sys.getenv("J_RATINGS_BACKFILL_SEASON", unset = ""))
dry_run <- tolower(trimws(Sys.getenv("J_RATINGS_BACKFILL_DRY_RUN", unset = "0"))) %in%
  c("1", "true", "yes", "y")

if (nzchar(competition_filter)) {
  jobs <- jobs[jobs$competition == competition_filter, , drop = FALSE]
}
if (nzchar(season_filter)) {
  jobs <- jobs[jobs$season_folder == season_filter, , drop = FALSE]
}
if (nrow(jobs) == 0L) stop("No jobs match the supplied filters.")

# ----------------------------------------------------------------------
# Generic helpers
# ----------------------------------------------------------------------

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("\\[[0-9]+\\]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

ascii_text <- function(x) {
  z <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT", sub = " "))
  z[is.na(z)] <- x[is.na(z)]
  z
}

club_key <- function(x) {
  x <- clean_text(x)
  x <- ascii_text(x)
  x <- tolower(x)

  # Common RSSSF geographical/club annotations.
  x <- gsub("\\([^)]*\\)", " ", x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(gsub("\\s+", " ", x))

  stop_tokens <- c(
    "fc", "afc", "cf", "fk", "sk", "nk", "ac", "ssc", "as", "sc", "sv",
    "rc", "rsc", "kv", "krc", "rkc", "vv", "ksv", "nfc", "asf", "pfk",
    "club", "football", "fussball", "futbol", "futbolnyy",
    "royal", "royale", "koninklijke"
  )

  norm_one <- function(z) {
    toks <- strsplit(z, "\\s+")[[1]]
    toks <- toks[nzchar(toks)]
    toks <- toks[!grepl("^[a-z]$", toks)]  # R. Antwerp, K. Eupen etc.
    toks <- toks[!(toks %in% stop_tokens)]
    z <- paste(toks, collapse = " ")

    # Stable country-specific spelling variants used by RSSSF/Wikipedia.
    z <- gsub("\\bsaint\\b", "sint", z)
    z <- gsub("\\bsankt\\b", "st", z)
    z <- gsub("\\bathinai\\b", "athens", z)
    z <- gsub("\\bpiraeus\\b", "piraeus", z)
    z <- gsub("\\bthessaloniki\\b", "thessaloniki", z)
    z <- gsub("\\bkyiv\\b", "kiev", z)
    z <- gsub("\\bkiev\\b", "kiev", z)
    z <- gsub("\\bolexandria\\b", "oleksandriya", z)
    z <- gsub("\\balexandriya\\b", "oleksandriya", z)
    z <- gsub("\\bzorya\\b", "zoria", z)
    z <- gsub("\\bzorya\\b", "zoria", z)
    z <- gsub("\\bplzen\\b", "plzen", z)
    z <- gsub("\\bceske budejovice\\b", "ceske budejovice", z)

    # Legacy RSSSF encodings can split Zürich into odd fragments.
    if (grepl("^z.*rich$", z)) z <- "zurich"
    z <- gsub("^grasshopper.*zurich$", "grasshopper zurich", z)

    trimws(gsub("\\s+", " ", z))
  }

  vapply(x, norm_one, character(1))
}

result_code <- function(hg, ag) {
  if (hg > ag) "1-0" else if (hg < ag) "0-1" else "0.5-0.5"
}

season_label <- function(folder) gsub("-", "/", folder, fixed = TRUE)

season_years <- function(folder) {
  y <- as.integer(substr(folder, 1, 4))
  list(start = y, end = y + 1L)
}

safe_download <- function(url, dest, user_agent) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(dest) && file.info(dest)$size > 0) {
    cat("  cache:", dest, "\n")
    return("cached")
  }

  tmp <- paste0(dest, ".tmp")
  if (file.exists(tmp)) file.remove(tmp)

  cat("  GET:  ", url, "\n", sep = "")
  cat("  save: ", dest, "\n", sep = "")

  ok <- tryCatch(
    {
      status <- download.file(
        url, tmp,
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

  # Courtesy pause after a real request.
  Sys.sleep(REQUEST_DELAY_SECONDS)
  "downloaded"
}

wikipedia_url <- function(title) {
  paste0(
    "https://en.wikipedia.org/wiki/",
    URLencode(gsub(" ", "_", title, fixed = TRUE), reserved = TRUE)
  )
}

cell_team_name <- function(cell) {
  link <- xml2::xml_find_first(cell, ".//a")
  if (!inherits(link, "xml_missing")) {
    title <- xml2::xml_attr(link, "title")
    if (!is.na(title) && nzchar(title)) return(clean_text(title))
    txt <- clean_text(xml2::xml_text(link))
    if (nzchar(txt)) return(txt)
  }
  clean_text(xml2::xml_text(cell))
}

# ----------------------------------------------------------------------
# Wikipedia: collect ALL Home/Away result matrices on the season page.
# ----------------------------------------------------------------------

parse_wikipedia_matrices <- function(html_path) {
  doc <- xml2::read_html(html_path)
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

    looks_matrix <- (
      grepl("Home", corner, ignore.case = TRUE) &&
      grepl("Away", corner, ignore.case = TRUE)
    ) || (
      grepl("Home", first_text, ignore.case = TRUE) &&
      grepl("Away", first_text, ignore.case = TRUE) &&
      length(first_cells) >= 5L
    )
    if (!looks_matrix) next

    away <- vapply(first_cells[-1], cell_team_name, character(1))
    if (sum(nzchar(away)) < 3L) next

    matrix_no <- matrix_no + 1L
    trs <- xml2::xml_find_all(tbl, ".//tr[position()>1]")

    for (tr in trs) {
      cells <- xml2::xml_find_all(tr, "./th|./td")
      if (length(cells) < 2L) next

      home <- cell_team_name(cells[[1]])
      if (!nzchar(home)) next

      n_away <- min(length(away), length(cells) - 1L)
      for (j in seq_len(n_away)) {
        s <- clean_text(xml2::xml_text(cells[[j + 1L]]))
        if (!grepl(score_pat, s, perl = TRUE)) next

        hg <- as.integer(sub(score_pat, "\\1", s, perl = TRUE))
        ag <- as.integer(sub(score_pat, "\\2", s, perl = TRUE))

        rows[[k]] <- data.frame(
          MatrixNo = matrix_no,
          HomeWiki = home,
          AwayWiki = away[j],
          HomeKey = club_key(home),
          AwayKey = club_key(away[j]),
          Result = result_code(hg, ag),
          Score = sprintf("%d-%d", hg, ag),
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
  }

  if (length(rows) == 0L) {
    stop("No completed Wikipedia Home/Away result matrices were parsed.")
  }

  out <- do.call(rbind, rows)
  out <- out[nzchar(out$HomeKey) & nzchar(out$AwayKey), , drop = FALSE]
  out
}

# ----------------------------------------------------------------------
# RSSSF: extract dated numeric match rows from the entire page.
#
# We deliberately parse the whole national page rather than assuming where
# a heading/pre block sits. Later we retain only matches whose two teams can
# be mapped to the Wikipedia top-flight roster AND whose score occurs in the
# Wikipedia season results. This is robust to RSSSF's varying page layouts.
# ----------------------------------------------------------------------

rsssf_text_lines <- function(html_path) {
  size <- file.info(html_path)$size
  bytes <- readBin(html_path, what = "raw", n = size)

  # RSSSF pages use a mix of encodings. Detect BOM first, otherwise fall back
  # to latin1/Windows-1252-style single-byte decoding. This prevents both
  # "invalid in this locale" and embedded-NUL failures on Windows.
  decode_raw <- function(b) {
    if (length(b) >= 2L && identical(as.integer(b[1:2]), c(255L, 254L))) {
      # UTF-16LE BOM
      if (length(b) %% 2L == 1L) b <- b[-length(b)]
      ints <- as.integer(b)
      lo <- ints[seq(3L, length(ints), by = 2L)]
      hi <- ints[seq(4L, length(ints), by = 2L)]
      code <- lo + 256L * hi
      # Basic BMP reconstruction is enough for RSSSF HTML.
      return(intToUtf8(code, multiple = FALSE))
    }

    if (length(b) >= 2L && identical(as.integer(b[1:2]), c(254L, 255L))) {
      # UTF-16BE BOM
      if (length(b) %% 2L == 1L) b <- b[-length(b)]
      ints <- as.integer(b)
      hi <- ints[seq(3L, length(ints), by = 2L)]
      lo <- ints[seq(4L, length(ints), by = 2L)]
      code <- 256L * hi + lo
      return(intToUtf8(code, multiple = FALSE))
    }

    if (length(b) >= 3L && identical(as.integer(b[1:3]), c(239L, 187L, 191L))) {
      b <- b[-(1:3)]
      return(rawToChar(b))
    }

    s <- rawToChar(b)
    # If it is valid UTF-8 already, keep it.
    ok <- !is.na(suppressWarnings(iconv(s, from = "UTF-8", to = "UTF-8")))
    if (ok) return(enc2utf8(s))

    # RSSSF's older pages are commonly latin1/Windows-1252-ish.
    s2 <- suppressWarnings(iconv(s, from = "latin1", to = "UTF-8", sub = " "))
    if (is.na(s2)) s2 <- s
    enc2utf8(s2)
  }

  html <- decode_raw(bytes)

  # Avoid passing raw/invalid byte strings to xml2 path handling. Parse from
  # text after decoding, which also avoids Windows "file name too long" errors.
  doc <- xml2::read_html(charToRaw(html))
  txt <- xml2::xml_text(doc)

  txt <- suppressWarnings(iconv(txt, from = "", to = "UTF-8", sub = " "))
  if (is.na(txt)) txt <- enc2utf8(txt)

  txt <- gsub("\r\n?", "\n", txt, perl = TRUE)
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  lines <- gsub("\u00a0", " ", lines, fixed = TRUE)
  lines <- trimws(gsub("[\t ]+", " ", lines))
  lines
}

extract_rsssf_candidates <- function(html_path, folder) {
  lines <- rsssf_text_lines(html_path)
  yrs <- season_years(folder)

  months <- c(
    jan=1L, feb=2L, mar=3L, apr=4L, may=5L, jun=6L,
    jul=7L, aug=8L, sep=9L, oct=10L, nov=11L, dec=12L
  )

  current_date <- as.Date(NA)
  rows <- list()
  k <- 1L

  # Supports:
  # [Jul 31]
  # Round 1 [Jul 31]
  # [Jul 31, 2021]
  # [31 Jul]
  bracket_mon_day <- "\\[([A-Za-z]{3,9})\\s+(\\d{1,2})(?:st|nd|rd|th)?(?:,?\\s+(\\d{4}))?\\]"
  bracket_day_mon <- "\\[(\\d{1,2})\\s+([A-Za-z]{3,9})(?:\\s+(\\d{4}))?\\]"

  # Home 2-1 Away. Restrict to ordinary numeric football scores.
  match_pat <- "^(.+?)\\s+(\\d{1,2})\\s*-\\s*(\\d{1,2})\\s+(.+?)$"

  for (ln in lines) {
    if (!nzchar(ln)) next

    # Date can appear on a line by itself or beside "Round N".
    if (grepl(bracket_mon_day, ln, ignore.case = TRUE, perl = TRUE)) {
      mon <- tolower(substr(
        sub(paste0(".*", bracket_mon_day, ".*"), "\\1", ln,
            ignore.case = TRUE, perl = TRUE),
        1L, 3L
      ))
      dd <- suppressWarnings(as.integer(
        sub(paste0(".*", bracket_mon_day, ".*"), "\\2", ln,
            ignore.case = TRUE, perl = TRUE)
      ))
      yy_txt <- sub(paste0(".*", bracket_mon_day, ".*"), "\\3", ln,
                    ignore.case = TRUE, perl = TRUE)
      mm <- unname(months[mon])
      yy <- suppressWarnings(as.integer(yy_txt))
      if (is.na(yy)) yy <- if (!is.na(mm) && mm >= 7L) yrs$start else yrs$end
      if (!is.na(mm) && !is.na(dd)) {
        current_date <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
      }
      # A pure date/round heading cannot also be a match row.
      if (!grepl(match_pat, ln, perl = TRUE)) next
    } else if (grepl(bracket_day_mon, ln, ignore.case = TRUE, perl = TRUE)) {
      dd <- suppressWarnings(as.integer(
        sub(paste0(".*", bracket_day_mon, ".*"), "\\1", ln,
            ignore.case = TRUE, perl = TRUE)
      ))
      mon <- tolower(substr(
        sub(paste0(".*", bracket_day_mon, ".*"), "\\2", ln,
            ignore.case = TRUE, perl = TRUE),
        1L, 3L
      ))
      yy_txt <- sub(paste0(".*", bracket_day_mon, ".*"), "\\3", ln,
                    ignore.case = TRUE, perl = TRUE)
      mm <- unname(months[mon])
      yy <- suppressWarnings(as.integer(yy_txt))
      if (is.na(yy)) yy <- if (!is.na(mm) && mm >= 7L) yrs$start else yrs$end
      if (!is.na(mm) && !is.na(dd)) {
        current_date <- as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd))
      }
      if (!grepl(match_pat, ln, perl = TRUE)) next
    }

    if (is.na(current_date)) next

    # Remove trailing RSSSF annotations before parsing away side.
    z <- ln
    z <- gsub("\\s+\\[[^]]*\\]\\s*$", "", z)
    z <- gsub("\\s+\\([^)]*\\)\\s*$", "", z)
    z <- trimws(z)

    if (!grepl(match_pat, z, perl = TRUE)) next

    home <- clean_text(sub(match_pat, "\\1", z, perl = TRUE))
    hg <- suppressWarnings(as.integer(sub(match_pat, "\\2", z, perl = TRUE)))
    ag <- suppressWarnings(as.integer(sub(match_pat, "\\3", z, perl = TRUE)))
    away <- clean_text(sub(match_pat, "\\4", z, perl = TRUE))

    # Obvious non-fixture/table rows.
    if (
      !nzchar(home) || !nzchar(away) ||
      grepl("^\\d", home) ||
      grepl("final table|halfway table|aggregate|attendance|points", home,
            ignore.case = TRUE) ||
      nchar(home) > 80L || nchar(away) > 80L
    ) next

    rows[[k]] <- data.frame(
      Date = format(current_date, "%Y-%m-%d"),
      HomeRSSSF = home,
      AwayRSSSF = away,
      HomeKeyRaw = club_key(home),
      AwayKeyRaw = club_key(away),
      ResultRSSSF = result_code(hg, ag),
      ScoreRSSSF = sprintf("%d-%d", hg, ag),
      stringsAsFactors = FALSE
    )
    k <- k + 1L
  }

  if (length(rows) == 0L) stop("RSSSF parser produced no dated numeric matches.")

  out <- do.call(rbind, rows)
  out <- out[!duplicated(out), , drop = FALSE]
  out
}

# ----------------------------------------------------------------------
# Team-name mapping: RSSSF roster -> Wikipedia roster.
#
# Exact normalised name first; then containment/token/edit-distance matching.
# We only accept a mapping with a strong, unique score. Anything ambiguous
# causes the season to fail rather than guessing.
# ----------------------------------------------------------------------

name_similarity <- function(a, b) {
  if (!nzchar(a) || !nzchar(b)) return(0)
  if (identical(a, b)) return(1)

  if ((grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)) &&
      min(nchar(a), nchar(b)) >= 4L) {
    return(0.96)
  }

  ta <- unique(strsplit(a, "\\s+")[[1]])
  tb <- unique(strsplit(b, "\\s+")[[1]])
  inter <- length(intersect(ta, tb))
  union <- length(union(ta, tb))
  token_score <- if (union > 0L) inter / union else 0

  d <- utils::adist(a, b)[1]
  edit_score <- 1 - d / max(nchar(a), nchar(b), 1)

  # Strongly reward a distinctive shared token.
  longest_shared <- 0L
  shared <- intersect(ta, tb)
  if (length(shared)) longest_shared <- max(nchar(shared))

  bonus <- if (longest_shared >= 6L) 0.12 else if (longest_shared >= 4L) 0.06 else 0
  min(1, 0.55 * edit_score + 0.45 * token_score + bonus)
}

map_rsssf_teams <- function(candidates, wiki) {
  wiki_roster <- unique(data.frame(
    Name = c(wiki$HomeWiki, wiki$AwayWiki),
    Key = c(wiki$HomeKey, wiki$AwayKey),
    stringsAsFactors = FALSE
  ))
  wiki_roster <- wiki_roster[nzchar(wiki_roster$Key), , drop = FALSE]
  wiki_roster <- wiki_roster[!duplicated(wiki_roster$Key), , drop = FALSE]

  rsssf_names <- unique(c(candidates$HomeRSSSF, candidates$AwayRSSSF))
  rsssf_keys <- club_key(rsssf_names)

  mapping <- data.frame(
    RSSSFName = rsssf_names,
    RSSSFKey = rsssf_keys,
    WikiName = "",
    WikiKey = "",
    Similarity = 0,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(mapping))) {
    rk <- mapping$RSSSFKey[i]

    exact <- which(wiki_roster$Key == rk)
    if (length(exact) == 1L) {
      mapping$WikiName[i] <- wiki_roster$Name[exact]
      mapping$WikiKey[i] <- wiki_roster$Key[exact]
      mapping$Similarity[i] <- 1
      next
    }

    scores <- vapply(
      wiki_roster$Key,
      function(wk) name_similarity(rk, wk),
      numeric(1)
    )

    ord <- order(scores, decreasing = TRUE)
    best <- ord[1]
    second <- if (length(ord) >= 2L) scores[ord[2]] else 0

    # Strict enough to avoid mapping random lower-division/cup clubs.
    if (scores[best] >= 0.72 && (scores[best] - second) >= 0.08) {
      mapping$WikiName[i] <- wiki_roster$Name[best]
      mapping$WikiKey[i] <- wiki_roster$Key[best]
      mapping$Similarity[i] <- scores[best]
    }
  }

  key_lookup <- setNames(mapping$WikiKey, mapping$RSSSFName)

  candidates$HomeKey <- unname(key_lookup[candidates$HomeRSSSF])
  candidates$AwayKey <- unname(key_lookup[candidates$AwayRSSSF])
  candidates$HomeKey[is.na(candidates$HomeKey)] <- ""
  candidates$AwayKey[is.na(candidates$AwayKey)] <- ""

  list(candidates = candidates, mapping = mapping)
}

# ----------------------------------------------------------------------
# Reconcile Wikipedia score rows to RSSSF dated rows.
#
# Key is HomeKey + AwayKey + Score. If the same scored pairing occurs more
# than once, chronological RSSSF order and Wikipedia matrix order provide
# occurrence numbers. Cup/lower-division rows are naturally excluded because
# they cannot consume more occurrences than Wikipedia has.
# ----------------------------------------------------------------------

reconcile_sources <- function(wiki, rsssf, mapping, job) {
  r <- mapping$candidates
  r <- r[nzchar(r$HomeKey) & nzchar(r$AwayKey), , drop = FALSE]

  # Only keep RSSSF rows whose score/pair occurs somewhere in Wikipedia.
  wiki_base <- paste(wiki$HomeKey, wiki$AwayKey, wiki$Score, sep = "||")
  rsssf_base <- paste(r$HomeKey, r$AwayKey, r$ScoreRSSSF, sep = "||")
  r <- r[rsssf_base %in% wiki_base, , drop = FALSE]

  if (nrow(r) == 0L) {
    stop("No RSSSF dated results survived Wikipedia roster/score filtering.")
  }

  r <- r[order(r$Date, r$HomeKey, r$AwayKey), , drop = FALSE]
  rsssf_base <- paste(r$HomeKey, r$AwayKey, r$ScoreRSSSF, sep = "||")
  r$Occurrence <- ave(seq_len(nrow(r)), rsssf_base, FUN = seq_along)
  r$JoinKey <- paste(rsssf_base, r$Occurrence, sep = "||")

  # Wikipedia matrix order is stable enough for repeated pair+score cases.
  wiki$Base <- wiki_base
  wiki$Occurrence <- ave(seq_len(nrow(wiki)), wiki$Base, FUN = seq_along)
  wiki$JoinKey <- paste(wiki$Base, wiki$Occurrence, sep = "||")

  m <- match(wiki$JoinKey, r$JoinKey)

  if (any(is.na(m))) {
    bad <- wiki[is.na(m), c(
      "MatrixNo", "HomeWiki", "AwayWiki", "Score", "HomeKey", "AwayKey"
    ), drop = FALSE]

    cat("\nWikipedia rows without a unique dated RSSSF match:\n")
    print(bad, row.names = FALSE)

    # Show RSSSF names that mapped close to the Wikipedia roster for diagnosis.
    useful_map <- mapping$mapping[
      nzchar(mapping$mapping$WikiKey) |
        mapping$mapping$Similarity >= 0.55,
      ,
      drop = FALSE
    ]
    cat("\nRSSSF -> Wikipedia team-name mapping candidates:\n")
    print(
      useful_map[order(-useful_map$Similarity), ],
      row.names = FALSE
    )

    stop(
      "Only ", sum(!is.na(m)), " of ", nrow(wiki),
      " Wikipedia results could be given dates from RSSSF."
    )
  }

  # Exact score agreement is built into the join key.
  imported <- data.frame(
    Season = season_label(job$season_folder),
    Country = job$country,
    Competition = job$competition,
    CompetitionType = "league",
    Tier = 1L,
    League = job$league,
    Date = r$Date[m],
    Home = wiki$HomeWiki,
    Away = wiki$AwayWiki,
    Result = wiki$Result,
    Score = wiki$Score,
    Source = "wikipedia",
    stringsAsFactors = FALSE
  )

  dated_key <- paste(
    imported$Date, imported$Home, imported$Away,
    sep = "||"
  )
  if (anyDuplicated(dated_key)) {
    stop("Dated import contains duplicate Date/Home/Away rows.")
  }

  cat("  RSSSF dated matches matched:", nrow(imported), "/", nrow(wiki), "\n")
  cat("  Exact score cross-check:", nrow(imported), "/", nrow(wiki), "\n")

  imported
}

# ----------------------------------------------------------------------
# Master reconciliation.
# ----------------------------------------------------------------------

required_cols <- c(
  "Season", "Country", "Competition", "CompetitionType", "Tier", "League",
  "Date", "Home", "Away", "Result", "Score", "Source"
)

master <- read.csv(
  master_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c()
)

missing_cols <- setdiff(required_cols, names(master))
if (length(missing_cols)) {
  stop("Master CSV missing columns: ", paste(missing_cols, collapse = ", "))
}

master_key <- function(df) {
  paste(
    df$Season,
    df$Country,
    df$Competition,
    df$Date,
    club_key(df$Home),
    club_key(df$Away),
    sep = "||"
  )
}

all_passed <- list()
audit <- list()
failures <- list()

cat("European Football historical gap backfill\n")
cat("Master:", master_file, "\n")
cat("Jobs:", nrow(jobs), "\n")
cat("Courtesy delay:", REQUEST_DELAY_SECONDS, "seconds per uncached request\n")
cat("Dry run:", dry_run, "\n\n")

for (i in seq_len(nrow(jobs))) {
  job_started <- Sys.time()
  job <- jobs[i, , drop = FALSE]

  cat(
    "\n============================================================\n",
    "[", i, "/", nrow(jobs), "] ",
    job$competition, " ", job$season_folder, "\n",
    "============================================================\n",
    sep = ""
  )

  outcome <- tryCatch(
    {
      wiki_file <- file.path(
        wiki_root,
        job$competition,
        job$season_folder,
        "page.html"
      )
      rsssf_file <- file.path(
        rsssf_root,
        job$competition,
        job$season_folder,
        "page.html"
      )

      wstat <- safe_download(
        wikipedia_url(job$wiki_title),
        wiki_file,
        "J-Ratings/1.0 (one-off historical football statistics repair)"
      )
      if (wstat == "failed") stop("Wikipedia download failed.")

      rstat <- safe_download(
        job$rsssf_url,
        rsssf_file,
        "J-Ratings/1.0 (one-off historical football statistics repair)"
      )
      if (rstat == "failed") stop("RSSSF download failed.")

      wiki <- parse_wikipedia_matrices(wiki_file)
      cat("  Wikipedia completed results:", nrow(wiki), "\n")

      raw_rsssf <- extract_rsssf_candidates(rsssf_file, job$season_folder)
      cat("  RSSSF dated numeric candidates:", nrow(raw_rsssf), "\n")

      mapped <- map_rsssf_teams(raw_rsssf, wiki)
      imported <- reconcile_sources(wiki, raw_rsssf, mapped, job)

      # Check existing master for conflicts now, before accepting the season.
      mk <- master_key(master)
      ik <- master_key(imported)
      m <- match(ik, mk)

      if (any(!is.na(m))) {
        old_score <- trimws(master$Score[m[!is.na(m)]])
        new_score <- trimws(imported$Score[!is.na(m)])
        conflict <- nzchar(old_score) & old_score != new_score

        if (any(conflict)) {
          x <- imported[!is.na(m), , drop = FALSE][conflict, ]
          cat("\nExisting master score conflicts:\n")
          print(x, row.names = FALSE)
          stop("Existing master score conflict.")
        }
      }

      elapsed <- round(as.numeric(difftime(Sys.time(), job_started, units = "secs")), 1)

      list(
        ok = TRUE,
        imported = imported,
        audit = data.frame(
          Competition = job$competition,
          Season = season_label(job$season_folder),
          WikipediaRows = nrow(wiki),
          ImportedRows = nrow(imported),
          WikipediaCache = wstat,
          RSSSFCache = rstat,
          Status = "PASS",
          Message = "",
          ElapsedSeconds = elapsed,
          stringsAsFactors = FALSE
        )
      )
    },
    error = function(e) {
      elapsed <- round(as.numeric(difftime(Sys.time(), job_started, units = "secs")), 1)
      list(
        ok = FALSE,
        message = conditionMessage(e),
        audit = data.frame(
          Competition = job$competition,
          Season = season_label(job$season_folder),
          WikipediaRows = NA_integer_,
          ImportedRows = NA_integer_,
          WikipediaCache = "",
          RSSSFCache = "",
          Status = "FAIL",
          Message = conditionMessage(e),
          ElapsedSeconds = elapsed,
          stringsAsFactors = FALSE
        )
      )
    }
  )

  audit[[length(audit) + 1L]] <- outcome$audit

  if (outcome$ok) {
    all_passed[[length(all_passed) + 1L]] <- outcome$imported
    cat("  STATUS: PASS\n")
  } else {
    failures[[length(failures) + 1L]] <- paste(
      job$competition, job$season_folder, "->", outcome$message
    )
    cat("  STATUS: FAIL\n")
    cat("  Reason:", outcome$message, "\n")
  }
}

audit_df <- do.call(rbind, audit)

cat("\n\n==============================\n")
cat("RUN SUMMARY\n")
cat("==============================\n")
print(audit_df, row.names = FALSE)

stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
audit_file <- file.path(
  audit_dir,
  paste0("historical_gap_backfill_", stamp, ".csv")
)
write.csv(audit_df, audit_file, row.names = FALSE)

if (length(all_passed) == 0L) {
  cat("\nNo seasons passed. Master CSV unchanged.\n")
  cat("Audit:", audit_file, "\n")
  quit(status = 1L)
}

imports <- do.call(rbind, all_passed)
cat("\nPassing seasons:", length(all_passed), "/", nrow(jobs), "\n")
cat("Passing imported rows:", nrow(imports), "\n")

if (dry_run) {
  cat("\nDRY RUN: master CSV has NOT been changed.\n")
  cat("Audit:", audit_file, "\n")
  cat(
    "Total elapsed:",
    round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
    "seconds\n"
  )
  quit(status = if (length(failures)) 2L else 0L)
}

# Reconcile passing imports with master.
mk <- master_key(master)
ik <- master_key(imports)
m <- match(ik, mk)

new_rows <- imports[is.na(m), , drop = FALSE]
existing <- imports[!is.na(m), , drop = FALSE]
existing_idx <- m[!is.na(m)]

filled <- 0L
verified <- 0L

if (length(existing_idx)) {
  old_score <- trimws(master$Score[existing_idx])
  new_score <- trimws(existing$Score)

  conflicts <- nzchar(old_score) & old_score != new_score
  if (any(conflicts)) {
    stop("Unexpected conflict appeared during final master reconciliation.")
  }

  fill <- !nzchar(old_score)
  verified <- sum(nzchar(old_score) & old_score == new_score)
  filled <- sum(fill)

  if (any(fill)) {
    idx <- existing_idx[fill]
    src <- existing[fill, , drop = FALSE]
    master$Result[idx] <- src$Result
    master$Score[idx] <- src$Score
    master$Source[idx] <- "wikipedia"
  }
}

if (nrow(new_rows)) {
  master <- rbind(master, new_rows[, required_cols, drop = FALSE])
}

# Dedupe harmless duplicates; any score disagreement has already hard-stopped.
key_after <- master_key(master)
has_score <- nzchar(trimws(master$Score))
ord <- order(key_after, -as.integer(has_score), seq_len(nrow(master)))
master <- master[ord, , drop = FALSE]
key_after <- key_after[ord]
master <- master[!duplicated(key_after), , drop = FALSE]

master <- master[, required_cols, drop = FALSE]
master <- master[
  order(master$Date, master$Country, master$Competition, master$Home, master$Away),
  ,
  drop = FALSE
]

backup_file <- file.path(
  dirname(master_file),
  paste0("european_football_all_matches.backup_", stamp, ".csv")
)

if (!file.copy(master_file, backup_file, overwrite = FALSE)) {
  stop("Could not create master backup. Refusing to write.")
}

tmp <- paste0(master_file, ".tmp")
if (file.exists(tmp)) file.remove(tmp)

write.csv(master, tmp, row.names = FALSE, na = "")

if (!file.exists(tmp) || file.info(tmp)$size == 0) {
  stop("Temporary master write failed. Backup exists at:\n", backup_file)
}

if (!file.copy(tmp, master_file, overwrite = TRUE)) {
  stop("Could not replace master. Backup exists at:\n", backup_file)
}
file.remove(tmp)

cat("\n==============================\n")
cat("WRITE COMPLETE\n")
cat("==============================\n")
cat("New match rows appended:", nrow(new_rows), "\n")
cat("Existing blank scores filled:", filled, "\n")
cat("Existing scored rows verified:", verified, "\n")
cat("Updated master:", master_file, "\n")
cat("Backup:", backup_file, "\n")
cat("Audit:", audit_file, "\n")
cat("Rows now in master:", nrow(master), "\n")

if (length(failures)) {
  cat("\nSEASONS STILL NEEDING ATTENTION:\n")
  cat(paste0("  ", unlist(failures), collapse = "\n"), "\n")
}

cat(
  "\nTotal elapsed:",
  round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
  "seconds\n"
)

# Exit non-zero if anything failed, even though passing seasons were safely written.
if (length(failures)) quit(status = 2L)
