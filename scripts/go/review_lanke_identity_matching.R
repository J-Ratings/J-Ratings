# Blind identity test using the saved Lanke bracket review and GoRatings games.
# No aliases are supplied or written; website/ratings/pipeline inputs stay unchanged.
# source("scripts/go/review_lanke_identity_matching.R")
# GoRatings fallback is enabled for unmatched players; set
# LANKE_CHECK_GORATINGS_ONLINE=0 for an explicitly offline test.
# Network fallback: cache first, one request at a time, >=5 seconds between requests,
# maximum 8 requests, stop on any HTTP failure. No retries or kifu downloads.
local({
  started <- proc.time()[[3]]
  suppressPackageStartupMessages({library(data.table); library(jsonlite)})
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  stopifnot(file.exists(file.path(root, "Go/data/players.json")))
  out <- file.path(root, "Go/pipeline_data/tournament_tests/lanke_2026")
  samsung_test <- identical(Sys.getenv("GO_TOURNAMENT_TEST"), "samsung")
  completed_test <- identical(Sys.getenv("GO_COMPLETED_TOURNAMENT_TEST"), "1") || samsung_test
  completed_name <- if (samsung_test) "30th Samsung Cup" else "1st Beihai Xinyi Cup"
  test_year <- if (completed_test) 2025L else 2026L
  source_path <- file.path(out, if (samsung_test) "samsung_source.html" else if (completed_test) "completed_source.html" else "source.html")
  if (completed_test) {
    # A second tournament exercises the same identity resolver, with no supplied aliases.
    # Expand actual HTML row/column spans; do not assume a printed bracket is a flat table.
    stopifnot(requireNamespace("xml2", quietly = TRUE))
    doc <- xml2::read_html(file.path(out, if (samsung_test) "samsung_source.html" else "completed_source.html"))
    table <- xml2::xml_find_first(doc, "//table[contains(@class,'tournament-wide')]")
    rows <- xml2::xml_find_all(table, "./tr | ./tbody/tr")
    headers <- xml2::xml_find_all(rows[[1]], "./th")
    nr <- length(rows) - 1L
    grid <- matrix(NA_integer_, nr, length(headers))
    cells <- list()
    for (r in seq_len(nr)) {
      col <- 1L
      for (node in xml2::xml_find_all(rows[[r + 1L]], "./td")) {
        while (col <= ncol(grid) && !is.na(grid[r, col])) col <- col + 1L
        span <- function(attr) {
          x <- suppressWarnings(as.integer(xml2::xml_attr(node, attr)))
          if (is.na(x)) 1L else x
        }
        rs <- span("rowspan"); cs <- span("colspan")
        items <- trimws(xml2::xml_text(xml2::xml_find_all(node, ".//span[contains(@class,'tournament-result-item')]")))
        cells[[length(cells) + 1L]] <- list(row = r, col = col, span = rs, node = node,
                                          text = trimws(xml2::xml_text(node)), items = items)
        stopifnot(r + rs - 1L <= nr, col + cs - 1L <= ncol(grid))
        grid[r:(r + rs - 1L), col:(col + cs - 1L)] <- length(cells)
        col <- col + cs
      }
    }
    clean_source <- function(x) trimws(sub("\\([^)]*\\)$", "", sub("^\\([a-z]+\\)", "", x)))
    entrants_new <- lapply(Filter(function(c) c$col == 1L, cells), function(c) list(source_name = clean_source(c$text)))
    fs_new <- list()
    for (cell in cells) {
      if (cell$col <= 1L) next
      prev <- unique(grid[cell$row:(cell$row + cell$span - 1L), cell$col - 1L])
      prev <- prev[!is.na(prev)]
      opponents <- vapply(prev, function(i) {
        p <- cells[[i]]
        if (p$col == 1L) clean_source(p$text) else if (length(p$items)) clean_source(p$items[1]) else ""
      }, "")
      if (length(opponents) != 2L || any(!nzchar(opponents))) next
      matches <- xml2::xml_find_all(cell$node, ".//span[contains(concat(' ',normalize-space(@class),' '),' match ')]")
      if (!length(matches)) matches <- list(cell$node)
      header_items <- xml2::xml_text(xml2::xml_find_all(headers[[cell$col]], ".//span[contains(@class,'tournament-result-item')]"))
      round <- if (length(header_items)) header_items[1] else xml2::xml_text(headers[[cell$col]])
      for (match_node in matches) {
        items <- trimws(xml2::xml_text(xml2::xml_find_all(match_node, ".//span[contains(@class,'tournament-result-item')]")))
        result <- items[grepl("^[BW]\\+", items)]
        if (length(result) != 1L) next
        winner <- items[match(result, items) - 1L]
        dates <- c(items, header_items)
        dates <- dates[grepl("^2025-[0-9]{2}-[0-9]{2}$", dates)]
        stopifnot(length(dates) >= 1L, winner %in% opponents)
        fs_new[[length(fs_new) + 1L]] <- list(date = dates[1], players = as.list(opponents),
           winner = winner, result = result, round = round, tournament_name = completed_name)
      }
    }
    out <- file.path(root, "Go/pipeline_data/tournament_tests", if (samsung_test) "samsung_2025" else "beihai_2025")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    write_json(list(entrants = entrants_new, fixtures = fs_new), file.path(out, "review.json"),
               pretty = TRUE, auto_unbox = TRUE)
  }
  review_path <- file.path(out, "review.json")
  stopifnot(file.exists(review_path))
  norm <- function(x) {
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    gsub("[^a-z0-9]", "", tolower(x))
  }
  cat("[1/4] Reading bracket snapshot, published profiles and cached GoRatings games...\n")
  bracket <- fromJSON(review_path, simplifyVector = FALSE)
  profiles <- as.data.table(fromJSON(file.path(root, "Go/data/players.json")))
  profiles[, NameKey := norm(name)]
  home <- fread(file.path(root, "Go/pipeline_data/goratings/goratings_players_homepage.csv"))
  home[, NameKey := norm(player_name)]
  if (completed_test) {
    raw <- fread(file.path(root, "Go/pipeline_data/goratings/goratings_games_2015_2025.csv"),
                 select = c("date", "player_id", "player_name", "opponent_id", "opponent_name", "color", "result"))
    raw <- raw[substr(as.character(date), 1L, 4L) == "2025" & result %in% c("Win", "Loss")]
    games <- unique(raw[, .(Date = as.character(date),
      BlackID = ifelse(color == "Black", player_id, opponent_id),
      WhiteID = ifelse(color == "White", player_id, opponent_id),
      WinnerID = ifelse(result == "Win", player_id, opponent_id))])
  } else {
    games <- fread(file.path(root, paste0("Go/pipeline_data/processed/goratings_games_", test_year, ".csv")),
                   select = c("Date", "BlackID", "WhiteID", "WinnerID"))
  }
  games[, Date := as.character(Date)]
  id_profile <- merge(home[, .(GoID = as.character(player_id), NameKey)],
                      profiles[, .(ProfileID = id, ProfileName = name, NameKey)], by = "NameKey")
  id_profile <- id_profile[, if (.N == 1L) .SD, by = GoID]
  if (completed_test) {
    # Older GoRatings IDs/names may differ from the current homepage. Bridge using
    # names actually recorded with those IDs, never a tournament-specific alias.
    legacy <- unique(rbind(raw[, .(GoID = as.character(player_id), NameKey = norm(player_name))],
                           raw[, .(GoID = as.character(opponent_id), NameKey = norm(opponent_name))]))
    legacy <- merge(legacy, profiles[, .(ProfileID = id, ProfileName = name, NameKey)], by = "NameKey")
    id_profile <- unique(rbind(id_profile, legacy))
    id_profile <- id_profile[, if (uniqueN(ProfileID) == 1L) .SD[1], by = GoID]
  }
  entrants <- data.table(SourceName = vapply(bracket$entrants, `[[`, "", "source_name"))
  # Retain the source's dan/amateur designation, independently of name matching.
  source_doc <- xml2::read_html(source_path)
  source_labels <- trimws(xml2::xml_text(xml2::xml_find_all(source_doc, "//td")))
  source_labels <- source_labels[grepl("\\((?:[0-9]+d|ama)\\)$", source_labels, perl = TRUE)]
  ranks <- data.table(
    SourceName = trimws(sub("\\([^)]*\\)$", "", sub("^\\([a-z]+\\)", "", source_labels))),
    SourceRank = sub(".*\\(([^)]*)\\)$", "\\1", source_labels))
  ranks <- unique(ranks)
  entrants[, SourceRank := vapply(SourceName, function(s) {
    r <- ranks[SourceName == s, unique(SourceRank)]
    if (length(r) == 1L) r else ""
  }, "")]
  entrants[, `:=`(GoID = "", ProfileID = "", ProfileName = "", Method = "UNRESOLVED")]
  # Deliberately ignore profile_ids/status in the previous review. No saved aliases.
  for (i in seq_len(nrow(entrants))) {
    p <- profiles[NameKey == norm(entrants$SourceName[i])]
    h <- home[NameKey == norm(entrants$SourceName[i])]
    if (nrow(p) == 1L && nrow(h) == 1L) {
      set(entrants, i, c("GoID", "ProfileID", "ProfileName", "Method"),
          list(as.character(h$player_id), p$id, p$name, "EXACT_NAME"))
    } else if (nrow(p) == 1L) {
      legacy_ids <- id_profile[ProfileID == p$id, unique(GoID)]
      set(entrants, i, c("GoID", "ProfileID", "ProfileName", "Method"),
          list(if (length(legacy_ids) == 1L) legacy_ids else "", p$id, p$name, "EXACT_PROFILE_NAME"))
    }
  }
  initially_unknown <- entrants[Method == "UNRESOLVED", SourceName]
  cat("  Direct matches:", sum(nzchar(entrants$ProfileID)), "/", nrow(entrants),
      "; blind unresolved labels:", length(initially_unknown), "\n")
  fixtures <- rbindlist(lapply(bracket$fixtures, function(f) {
    if (!nzchar(f$result) || length(f$players) != 2L) return(NULL)
    data.table(Date = f$date, A = f$players[[1]], B = f$players[[2]], Winner = f$winner,
               Round = f$round, SourceResult = f$result)
  }))
  # Cache is the normal first choice: no live requests needed for already saved games.
  evidence <- data.table()
  inspect <- function(subject, f, other, other_id, records, source) {
    hit <- records[Date == f$Date & (BlackID == other_id | WhiteID == other_id)]
    if (!nrow(hit)) return(NULL)
    known_won <- identical(f$Winner, other)
    hit <- hit[(WinnerID == other_id) == known_won]
    hit <- hit[(WinnerID == BlackID) == startsWith(f$SourceResult, "B+")]
    if (!nrow(hit)) return(NULL)
    ids <- unique(as.character(ifelse(hit$BlackID == other_id, hit$WhiteID, hit$BlackID)))
    data.table(SourceName = subject, Date = f$Date, KnownOpponent = other,
               CandidateGoID = ids, Source = source, Round = f$Round,
               CompatibleOutcome = TRUE)
  }
  cat("[2/4] Resolving identities from known opponents, dates and outcomes...\n")
  resolve <- function(source_records = games, source_label = "CACHED_GORATINGS") {
    changed <- TRUE
    iteration <- 0L
    while (changed) {
      changed <- FALSE
      iteration <- iteration + 1L
      for (subject in initially_unknown) {
        fs <- fixtures[A == subject | B == subject]
        ev <- rbindlist(lapply(seq_len(nrow(fs)), function(j) {
          f <- fs[j]
          other <- if (f$A == subject) f$B else f$A
          known <- entrants[SourceName == other & nzchar(GoID)]
          if (nrow(known) != 1L) return(NULL)
          inspect(subject, f, other, known$GoID, source_records, source_label)
        }), fill = TRUE)
        if (!nrow(ev)) next
        evidence <<- unique(rbindlist(list(evidence, ev), fill = TRUE))
        all_ev <- evidence[SourceName == subject]
        candidates <- unique(all_ev$CandidateGoID)
        # Fail closed: competing candidates or conflicting evidence require review.
        if (length(candidates) != 1L) {
          entrants[SourceName == subject, `:=`(GoID = "", ProfileID = "", ProfileName = "", Method = "CONFLICT_REVIEW")]
          next
        }
        candidate <- candidates[[1]]
        if (nrow(entrants[SourceName != subject & GoID == candidate])) next
        if (entrants[SourceName == subject, GoID] == candidate) next
        p <- id_profile[GoID == candidate]
        entrants[SourceName == subject, `:=`(GoID = candidate,
          ProfileID = if (nrow(p) == 1L) p$ProfileID else "",
          ProfileName = if (nrow(p) == 1L) p$ProfileName else "",
          Method = if (nrow(p) == 1L) "OPPONENT_DATE_OUTCOME" else "IDENTIFIED_NO_PUBLISHED_PROFILE")]
        changed <- TRUE
      }
      if (iteration > nrow(entrants) + 1L) stop("Resolution did not converge; review required.")
    }
  }
  resolve()
  cat("[3/4] Conservative GoRatings backup check...\n")
  online <- !identical(Sys.getenv("LANKE_CHECK_GORATINGS_ONLINE"), "0")
  requests <- 0L
  if (online && nrow(entrants[!nzchar(ProfileID)])) {
    if (!requireNamespace("xml2", quietly = TRUE)) stop("Install xml2 for the optional online check.")
    cache <- file.path(out, "goratings_cache")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    targets <- unique(unlist(lapply(entrants[!nzchar(ProfileID), SourceName], function(s) {
      fs <- fixtures[A == s | B == s]
      others <- unique(c(fs$A, fs$B)); others <- setdiff(others, s)
      entrants[SourceName %in% others & nzchar(GoID), GoID]
    })))
    extra <- copy(games[0])
    for (go_id in targets) {
      url <- paste0("https://www.goratings.org/en/players/", go_id, ".html")
      path <- file.path(cache, paste0(go_id, ".html"))
      if (!file.exists(path)) {
        if (requests >= 8L) break
        if (requests > 0L) Sys.sleep(5)
        requests <- requests + 1L
        cat("  Website request", requests, "/8:", go_id, "\n"); flush.console()
        ok <- tryCatch({
          status <- download.file(url, path, quiet = TRUE, mode = "wb", method = "libcurl")
          identical(status, 0L)
        }, error = function(e) FALSE)
        if (!ok) {cat("  Request failed; stopping online checks without retrying.\n"); break}
      }
      doc <- xml2::read_html(path)
      rows <- xml2::xml_find_all(doc, "//tr[td]")
      parsed <- rbindlist(lapply(rows, function(row) {
        td <- xml2::xml_find_all(row, "./td")
        if (length(td) < 5L) return(NULL)
        values <- trimws(xml2::xml_text(td))
        if (!grepl(paste0("^", test_year, "-[0-9]{2}-[0-9]{2}$"), values[1])) return(NULL)
        opponent <- xml2::xml_find_first(td[[5]], ".//a[contains(@href, '/players/')]")
        href <- xml2::xml_attr(opponent, "href")
        if (is.na(href) || !grepl("[0-9]+\\.html$", href)) return(NULL)
        oid <- sub(".*?([0-9]+)\\.html$", "\\1", href)
        if (!values[3] %in% c("Black", "White") || !values[4] %in% c("Win", "Loss")) return(NULL)
        data.table(Date = values[1], BlackID = if (values[3] == "Black") go_id else oid,
                   WhiteID = if (values[3] == "White") go_id else oid,
                   WinnerID = if (values[4] == "Win") go_id else oid)
      }), fill = TRUE)
      if (nrow(parsed)) extra <- rbind(extra, parsed)
    }
    if (nrow(extra)) resolve(extra, "GORATINGS_PROFILE_PAGE")
  }
  cat("[4/4] Writing local identity evidence and resolved fixture preview...\n")
  entrants[, Dan := suppressWarnings(as.integer(sub("d$", "", SourceRank)))]
  entrants[, ReviewDecision := fifelse(nzchar(ProfileID), "MATCHED",
    fifelse(SourceRank == "ama" | (!is.na(Dan) & Dan < 5L), "SKIP_LOW_RANK_OR_AMATEUR",
      fifelse(!is.na(Dan) & Dan >= 5L, "FLAG_UNMATCHED_5D_PLUS", "FLAG_UNKNOWN_RANK")))]
  result <- copy(entrants[SourceName %in% initially_unknown])
  fwrite(result, file.path(out, "identity_review.csv"))
  fwrite(evidence, file.path(out, "identity_evidence.csv"))
  preview <- lapply(bracket$fixtures, function(f) {
    p <- entrants[match(unlist(f$players), SourceName)]
    f$profile_ids <- as.list(p$ProfileID)
    f$profile_names <- as.list(p$ProfileName)
    f$eligible_existing_profiles <- nrow(p) == 2L && all(nzchar(p$ProfileID)) && !anyNA(p$ProfileID)
    f$public_prediction_ready <- FALSE
    f
  })
  write_json(list(tournament = if (completed_test) completed_name else "4th Quzhou-Lanke Cup", fixtures = preview),
             file.path(out, "resolved_preview.json"), pretty = TRUE, auto_unbox = TRUE)
  print(result[, .(SourceName, SourceRank, ProfileName, Method, ReviewDecision)])
  cat("Published profiles matched:", sum(nzchar(entrants$ProfileID)), "/", nrow(entrants),
      "; new website requests:", requests, "\n")
  cat("No production changes. Report:", out, "\nElapsed:", round(proc.time()[[3]] - started, 1), "seconds\n")
})
