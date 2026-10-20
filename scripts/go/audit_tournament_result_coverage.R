# Read-only experiment: source("scripts/go/audit_tournament_result_coverage.R")
# Run from the repository root. No production files or aliases are changed.
# GO_COVERAGE_OFFLINE=1: use saved pages only.
# GO_COVERAGE_MAX_PAGES=0: all editions; a positive number limits a trial.
# Source requests >=1.5 seconds apart. Optional GoRatings backup >=5 seconds,
# cache first, max 8 requests. GO_COVERAGE_GORATINGS_ONLINE=0 disables it.
local({
  notify <- function(kind) {
    if (identical(Sys.getenv("GO_COVERAGE_SILENT"), "1")) return(invisible(NULL))
    if (.Platform$OS.type == "windows") {
      # Play a WAV through normal Windows audio; console Beep can be inaudible
      # in RStudio. A script file avoids PowerShell command-quoting problems.
      wav <- file.path(Sys.getenv("WINDIR", "C:/Windows"), "Media",
        if (kind == "success") "ding.wav" else "chord.wav")
      ps <- tempfile(fileext = ".ps1")
      on.exit(unlink(ps), add = TRUE)
      writeLines(c("$ErrorActionPreference = 'Stop'",
        paste0("$player = New-Object System.Media.SoundPlayer '", gsub("'", "''", wav, fixed = TRUE), "'"),
        "$player.Load()", "$player.PlaySync()", "$player.Dispose()"), ps)
      status <- tryCatch(system2("powershell.exe",
        c("-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass", "-File", shQuote(ps)),
        wait = TRUE, invisible = TRUE), error = function(e) 1L)
      if (!identical(status, 0L)) warning("Windows audio notification failed. Run the sound test before the next audit.")
    } else cat("\a")
    invisible(NULL)
  }
  sound_test <- Sys.getenv("GO_COVERAGE_SOUND_TEST")
  if (nzchar(sound_test)) {
    if (!sound_test %in% c("success", "error")) stop("GO_COVERAGE_SOUND_TEST must be success or error.")
    notify(sound_test)
    cat("Sound test finished; no audit was run.\n")
    return(invisible(NULL))
  }
  tryCatch({
  started <- proc.time()[[3]]
  pkgs <- c("data.table", "xml2", "jsonlite")
  absent <- pkgs[!vapply(pkgs, requireNamespace, TRUE, quietly = TRUE)]
  if (length(absent)) stop("Install required packages: ", paste(absent, collapse = ", "))
  suppressPackageStartupMessages({library(data.table); library(xml2); library(jsonlite)})
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  audit <- file.path(root, "Go/pipeline_data/tournament_format_audit")
  output_override <- Sys.getenv("GO_COVERAGE_OUTPUT")
  out <- if (nzchar(output_override)) output_override else file.path(root, "Go/pipeline_data/tournament_result_coverage")
  cache <- file.path(audit, "cache")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.exists(file.path(audit, "calendar_events.csv")))
  offline <- identical(Sys.getenv("GO_COVERAGE_OFFLINE"), "1")
  limit <- as.integer(Sys.getenv("GO_COVERAGE_MAX_PAGES", "0"))
  if (is.na(limit) || limit < 0L) stop("GO_COVERAGE_MAX_PAGES must be nonnegative.")
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT")))
  txt <- function(n) trimws(gsub("[[:space:]]+", " ", xml_text(n)))
  clean <- function(x) trimws(sub("\\([^)]*\\)\\s*$", "", sub("^\\([a-z]+\\)", "", x)))
  rank <- function(x) {
    if (!grepl("\\((?:[0-9]+\\s*(?:d|dan)|ama)\\)\\s*$", x, perl = TRUE)) return("")
    gsub(" dan", "d", sub(".*\\(([^)]*)\\)\\s*$", "\\1", x))
  }
  items <- function(n) trimws(xml_text(xml_find_all(n, ".//span[contains(@class,'tournament-result-item')]")))
  score_rx <- "^[BW]\\+(?:R|T|[0-9]+(?:\\.[0-9]+)?)$"
  date_rx <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}(?:~(?:[0-9]{4}-)?(?:[0-9]{2}-)?[0-9]{2})?$|^[0-9]{2}-[0-9]{2}$"
  sig <- function(d, a, b, w) paste(d, pmin(norm(a), norm(b)), pmax(norm(a), norm(b)), norm(w), sep = "|")
  last_request <- as.POSIXct(0, origin = "1970-01-01")
  requests <- 0L
  download_failures <- list()
  consecutive_server_errors <- 0L
  fetch <- function(url, path, delay = 1.5) {
    if (file.exists(path)) return(path)
    if (offline) return(NULL)
    pause <- delay - as.numeric(difftime(Sys.time(), last_request, units = "secs"))
    if (pause > 0) Sys.sleep(pause)
    last_request <<- Sys.time(); requests <<- requests + 1L
    tmp <- paste0(path, ".part"); http_status <- NA_integer_
    status <- tryCatch(withCallingHandlers(download.file(url, tmp, method = "libcurl", mode = "wb", quiet = TRUE),
      warning = function(w) {
        message <- conditionMessage(w)
        code <- regmatches(message, regexpr("HTTP status was '[0-9]{3}", message))
        if (length(code) && nzchar(code)) http_status <<- as.integer(sub(".*'", "", code))
      }),
      error = function(e) {
        download_failures[[length(download_failures) + 1L]] <<- data.table(
          URL = url, HTTPStatus = http_status, Message = conditionMessage(e))
        fwrite(rbindlist(download_failures), file.path(out, "download_failures.csv"))
        if (!is.na(http_status) && http_status %in% c(404L, 500L, 502L, 503L, 504L)) {
          if (http_status >= 500L) consecutive_server_errors <<- consecutive_server_errors + 1L
          if (consecutive_server_errors >= 3L) stop("Three consecutive server errors; pausing requests. Cache retained.")
          cat("  Skipping HTTP", http_status, "without retry:", url, "\n")
          return(http_status)
        }
        stop("Network/access failure; stopped without retries. Cache retained. ", url, " | ", conditionMessage(e))
      })
    if (status %in% c(404L, 500L, 502L, 503L, 504L)) return(NULL)
    if (status != 0L || !file.rename(tmp, path)) stop("Could not save: ", path)
    consecutive_server_errors <<- 0L
    path
  }
  cat("[1/5] Reading existing games and edition queue...\n")
  events <- fread(file.path(audit, "calendar_events.csv"))
  events[, Date := as.character(Date)]
  registry <- fread(file.path(audit, "tournament_registry.csv"))
  skipped <- registry[Classification == "Unresolved / source unavailable", Family]
  pages <- unique(events[!Family %in% skipped, .(Family, Link)])
  pages[, Discovery := "CALENDAR"]
  # Calendar entries are not an edition catalogue. Supplement them with the
  # same-family edition navigation already present in saved pages. Keep one
  # preceding edition for seasons that cross the start of the audit window.
  navigation <- list()
  for (family in unique(pages$Family)) {
    seeds <- pages[Family == family, Link]
    ordinals <- suppressWarnings(as.integer(sub(".*/([0-9]+)/$", "\\1", seeds)))
    if (!any(!is.na(ordinals))) next
    lower <- max(1L, min(ordinals, na.rm = TRUE) - 1L)
    for (seed in seeds) {
      saved <- file.path(cache, paste0(gsub("[^a-zA-Z0-9]+", "_", seed), ".html"))
      if (!file.exists(saved)) next
      navdoc <- read_html(saved)
      links <- unique(xml_attr(xml_find_all(navdoc, "//nav[contains(@class,'select-holding')]//a[@href]"), "href"))
      links <- links[!is.na(links) & startsWith(links, family)]
      editions <- suppressWarnings(as.integer(substring(sub("/$", "", links), nchar(family) + 1L)))
      links <- links[!is.na(editions) & editions >= lower]
      if (length(links)) navigation[[length(navigation) + 1L]] <- data.table(Family = family, Link = links, Discovery = "SAVED_EDITION_NAVIGATION")
    }
  }
  calendar_count <- nrow(pages)
  if (length(navigation)) pages <- unique(rbind(pages, rbindlist(navigation)), by = c("Family", "Link"))
  cat("  Calendar editions:", calendar_count, "; additional navigation editions:", nrow(pages) - calendar_count, "\n")
  setorder(pages, Family, Link)
  full_queue <- nrow(pages)
  test_link <- Sys.getenv("GO_COVERAGE_TEST_LINK")
  if (nzchar(test_link)) pages <- pages[Link == test_link]
  if (limit > 0L) pages <- head(pages, limit)
  fwrite(pages, file.path(out, "edition_queue.csv"))
  # Stage 09's combined history is what the current website exports use.
  # The older Elo/game_history.csv stops at the historical checkpoint.
  history_path <- file.path(root, "Go/pipeline_data/live_Elo/combined_game_history.csv")
  if (!file.exists(history_path)) stop("Missing combined history. Run the normal Go stage 09 first: ", history_path)
  history <- fread(history_path, select = c("Date", "Black", "White", "ResultCode", "Event", "Source"))
  history[, Date := as.character(Date)]
  history <- history[Date >= "2022-03-01" & Date <= as.character(Sys.Date()) & grepl("^[BW](\\+|$)", ResultCode)]
  if (!nrow(history)) stop("No comparison games in date window.")
  end_date <- max(history$Date)
  history[, Winner := ifelse(startsWith(ResultCode, "B"), Black, White)]
  history[, Signature := sig(Date, Black, White, Winner)]
  profiles <- as.data.table(fromJSON(file.path(root, "Go/data/players.json")))
  profiles[, NameKey := norm(name)]
  names_local <- unique(rbind(history[, .(Canonical = Black)], history[, .(Canonical = White)], profiles[, .(Canonical = name)]))
  names_local[, NameKey := norm(Canonical)]
  names_local <- names_local[, if (.N == 1L) .SD, by = NameKey]
  pm <- fread(file.path(root, "Go/pipeline_data/processed/goratings_player_map.csv"))
  pm <- pm[!is.na(historical_name) & nzchar(historical_name)]
  pm[, Canonical := historical_name]
  aliases <- unique(rbind(pm[, .(NameKey = norm(goratings_name), Canonical)],
                          pm[, .(NameKey = norm(source_goratings_name), Canonical)]))
  aliases <- aliases[, if (uniqueN(Canonical) == 1L) .SD[1], by = NameKey]
  go_names <- unique(pm[, .(GoID = as.character(goratings_id), Canonical)])
  go_names <- go_names[, if (uniqueN(Canonical) == 1L) .SD[1], by = GoID]
  raw <- fread(file.path(root, "Go/pipeline_data/goratings/goratings_games_2015_2025.csv"),
    select = c("date", "player_id", "opponent_id", "color", "result"))
  raw <- raw[as.character(date) >= "2022-03-01" & result %in% c("Win", "Loss") & color %in% c("Black", "White")]
  direct <- unique(raw[, .(Date = as.character(date),
    BlackID = as.character(ifelse(color == "Black", player_id, opponent_id)),
    WhiteID = as.character(ifelse(color == "White", player_id, opponent_id)),
    WinnerID = as.character(ifelse(result == "Win", player_id, opponent_id)))])
  current <- file.path(root, "Go/pipeline_data/processed/goratings_games_2026.csv")
  if (file.exists(current)) {
    cur <- fread(current, select = c("Date", "BlackID", "WhiteID", "WinnerID"))
    for (col in names(cur)) set(cur, j = col, value = as.character(cur[[col]]))
    direct <- unique(rbind(direct, cur))
  }
  direct[, Black := go_names$Canonical[match(BlackID, go_names$GoID)]]
  direct[, White := go_names$Canonical[match(WhiteID, go_names$GoID)]]
  direct[, Winner := go_names$Canonical[match(WinnerID, go_names$GoID)]]
  direct_raw_n <- uniqueN(direct[Date <= end_date, paste(Date, BlackID, WhiteID, WinnerID)])
  direct <- direct[Date <= end_date & !is.na(Black) & !is.na(White) & !is.na(Winner)]
  direct[, Signature := sig(Date, Black, White, Winner)]
  direct <- unique(direct, by = "Signature")
  rows <- list(); issues <- list(); page_stats <- list()
  issue <- function(link, stage, code, detail = "") {
    issues[[length(issues) + 1L]] <<- data.table(Link = link, Stage = stage, Issue = code, Detail = detail)
  }
  # Never guess an edition year from its ordinal. Section-history inference is
  # explicitly labelled for this read-only audit, not approved for importing.
  date_from <- function(v, link, round, stage) {
    v <- v[!is.na(v)]
    ranges <- unique(v[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}~(?:[0-9]{4}-)?(?:[0-9]{2}-)?[0-9]{2}$", v, perl = TRUE)])
    if (length(ranges) == 1L) {
      parts <- strsplit(ranges, "~", fixed = TRUE)[[1]]
      finish <- if (nchar(parts[2]) == 2L) paste0(substr(parts[1], 1, 8), parts[2]) else
        if (nchar(parts[2]) == 5L) paste0(substr(parts[1], 1, 5), parts[2]) else parts[2]
      first <- suppressWarnings(as.Date(parts[1])); last <- suppressWarnings(as.Date(finish))
      if (!is.na(first) && !is.na(last) && last < first && nchar(parts[2]) == 5L)
        last <- as.Date(paste0(as.integer(substr(parts[1], 1, 4)) + 1L, "-", parts[2]))
      # Existing GoGoD history dates these games by their starting day.
      if (!is.na(first) && !is.na(last) && as.integer(last - first) == 1L)
        return(list(Date = as.character(first), Evidence = "TWO_DAY_GAME_START_DATE_CONVENTION"))
      return(list(Date = "", Evidence = "DATE_RANGE_REVIEW"))
    }
    if (length(ranges) > 1L) return(list(Date = "", Evidence = "AMBIGUOUS_DATE_RANGE"))
    ds <- unique(v[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v)])
    if (length(ds) == 1L) return(list(Date = ds, Evidence = "EXPLICIT_FULL_DATE"))
    if (length(ds) > 1L) return(list(Date = "", Evidence = "AMBIGUOUS_DATE"))
    md <- unique(v[grepl("^[0-9]{2}-[0-9]{2}$", v)])
    if (length(md) == 1L) {
      ev <- events[Link == link & substr(Date, 6, 10) == md]
      ds <- unique(ev$Date)
      if (length(ds) == 1L) return(list(Date = ds, Evidence = "CALENDAR_EDITION_MONTH_DAY"))
      r <- tolower(round)
      if (grepl("semi", r)) ev <- ev[grepl("semifinal", Label, ignore.case = TRUE)]
      else if (grepl("quarter", r)) ev <- ev[grepl("quarterfinal", Label, ignore.case = TRUE)]
      else if (grepl("^[0-9]+(st|nd|rd|th) round$", r)) ev <- ev[grepl(r, tolower(Label), fixed = TRUE)]
      else if (grepl("title match", stage, ignore.case = TRUE)) ev <- ev[grepl("title match", Label, ignore.case = TRUE)]
      else ev <- ev[grepl("league", Label, ignore.case = TRUE)]
      ds <- unique(ev$Date)
      if (length(ds) == 1L) return(list(Date = ds, Evidence = "CALENDAR_EDITION_ROUND_MONTH_DAY"))
      # Use only history notes belonging to this section, never the page-wide
      # year: qualifiers and title matches can belong to different years.
      notes <- xml_find_all(doc, "//span[contains(@class,'note-description-history')]")
      section <- vapply(notes, function(n) txt(xml_find_first(n, "preceding::*[self::h3][1]")), "")
      allowed_sections <- c(stage, if (identical(stage, "League")) "League scene")
      note_text <- xml_text(notes[which(!is.na(section) & section %in% allowed_sections)])
      if (!length(note_text) && grepl("^League [A-Z](?:-[0-9]+)?$", stage, perl = TRUE))
        note_text <- xml_text(notes[which(!is.na(section) & grepl("^League [A-Z](?:-[0-9]+)?$", section, perl = TRUE))])
      years <- unique(unlist(regmatches(note_text, gregexpr("[0-9]{4}(?=-[0-9]{2}-)", note_text, perl = TRUE))))
      if (length(years) == 1L) {
        inferred <- paste0(years, "-", md)
        parsed <- suppressWarnings(as.Date(inferred, format = "%Y-%m-%d"))
        if (!is.na(parsed) && format(parsed, "%Y-%m-%d") == inferred)
          return(list(Date = inferred, Evidence = "INFERRED_SINGLE_SECTION_HISTORY_YEAR"))
      }
      # A season can cross New Year. Bound possible years by the section's
      # publication history, allowing 31 days before its first recorded note.
      # This remains inferred audit evidence, never an import-approved date.
      if (length(years) > 1L) {
        history_dates <- character()
        for (note in note_text) {
          groups <- regmatches(note, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}(?:,[0-9]{2})*", note, perl = TRUE))[[1]]
          for (group in groups) {
            parts <- strsplit(group, ",", fixed = TRUE)[[1]]
            history_dates <- c(history_dates, parts[1], if (length(parts) > 1L) paste0(substr(parts[1], 1, 8), parts[-1]))
          }
        }
        bounds <- suppressWarnings(as.Date(history_dates)); bounds <- bounds[!is.na(bounds)]
        candidates <- paste0(years, "-", md)
        parsed <- suppressWarnings(as.Date(candidates, format = "%Y-%m-%d"))
        if (length(bounds)) {
          valid <- !is.na(parsed) & parsed >= min(bounds) - 31L & parsed <= max(bounds)
          candidates <- candidates[which(valid)]
          if (length(candidates) == 1L)
            return(list(Date = candidates, Evidence = "INFERRED_SECTION_HISTORY_WINDOW"))
        }
      }
    }
    list(Date = "", Evidence = "DATE_REVIEW")
  }
  grid_of <- function(table) {
    trs <- xml_find_all(table, "./tr | ./thead/tr | ./tbody/tr | ./tfoot/tr")
    cells <- list(); grid <- matrix(NA_integer_, length(trs), 64L)
    for (r in seq_along(trs)) {
      col <- 1L
      for (node in xml_find_all(trs[[r]], "./th | ./td")) {
        while (col <= 64L && !is.na(grid[r, col])) col <- col + 1L
        span <- function(a) {v <- suppressWarnings(as.integer(xml_attr(node, a))); if (is.na(v)) 1L else v}
        rs <- span("rowspan"); cs <- span("colspan")
        if (col + cs - 1L > 64L || r + rs - 1L > nrow(grid)) stop("Invalid/oversized spans")
        cells[[length(cells) + 1L]] <- list(node = node, row = r, col = col, rs = rs,
          text = txt(node), items = items(node), header = xml_name(node) == "th")
        grid[r:(r + rs - 1L), col:(col + cs - 1L)] <- length(cells); col <- col + cs
      }
    }
    list(cells = cells, grid = grid)
  }
  emit <- function(link, family, tournament, stage, round, a, b, winner, result, dates, parser, context = "", ra = "", rb = "", kifu = "") {
    dt <- date_from(dates, link, round, stage)
    if (nzchar(dt$Date) && (dt$Date > end_date || dt$Date < "2022-03-01")) return(invisible(NULL))
    status <- if (!nzchar(a) || !nzchar(b) || norm(a) == norm(b) || !norm(winner) %in% norm(c(a, b))) "PAIRING_REVIEW" else if (!nzchar(dt$Date)) "DATE_REVIEW" else "EXTRACTED"
    rows[[length(rows) + 1L]] <<- data.table(Family = family, Tournament = tournament, Stage = stage, Round = round,
      Date = dt$Date, DateEvidence = dt$Evidence, SourceDateTokens = paste(dates, collapse = ";"), PlayerA = a, PlayerB = b, Winner = winner,
      Result = result, RankA = ra, RankB = rb, Parser = parser, ExtractionStatus = status,
      SourceURL = paste0("https://gotoeveryone.k2ss.info", link), Context = context, KifuLink = kifu)
  }
  cat("[2/5] Extracting results; saved pages reused...\n")
  for (pi in seq_len(nrow(pages))) {
    link <- pages$Link[pi]; family <- pages$Family[pi]
    path <- fetch(paste0("https://gotoeveryone.k2ss.info", link),
      file.path(cache, paste0(gsub("[^a-zA-Z0-9]+", "_", link), ".html")))
    before <- length(rows)
    if (is.null(path)) {issue(link, "", "PAGE_UNAVAILABLE_OR_NOT_CACHED"); next}
    doc <- read_html(path)
    tournament <- txt(xml_find_first(doc, "//main//h2 | //article//h2"))
    if (is.na(tournament) || !nzchar(tournament)) tournament <- txt(xml_find_first(doc, "//h1"))
    # Explicit individual board-result lists. Colour styling identifies the winner;
    # B+/W+ specifies the winning stone colour, NOT the first player's colour.
    for (row in xml_find_all(doc, "//li[contains(concat(' ',normalize-space(@class),' '),' table-row ')]")) {
      if (grepl("table-row-header", xml_attr(row, "class"))) next
      nodes <- xml_find_all(row, "./div | ./span")
      if (!length(nodes)) next
      scores <- which(grepl(score_rx, vapply(nodes, txt, ""), perl = TRUE))
      if (length(scores) != 1L) next
      p <- which(grepl("knockout_player|al_name|kl_name|player", xml_attr(nodes, "class")))
      if (length(p) != 2L) {issue(link, "", "LIST_PAIRING_LAYOUT_REVIEW", txt(row)); next}
      cl <- xml_attr(nodes[[scores]], "class")
      winstyle <- regmatches(cl, regexpr("back[0-9]+", cl))
      wp <- if (length(winstyle) && nzchar(winstyle)) p[grepl(paste0("(^| )", winstyle, "( |$)"), xml_attr(nodes[p], "class"))] else integer()
      stage <- txt(xml_find_first(row, "preceding::*[self::h3][1]"))
      context <- txt(xml_find_first(row, "preceding::*[self::h4][1]"))
      if (is.na(context)) context <- ""
      dates <- vapply(nodes, txt, "")
      # Korean team fixtures place an exact day in a preceding header within
      # the same list. The round heading only gives a multi-day range.
      day <- txt(xml_find_first(row, "preceding-sibling::li[.//div[contains(@class,'kl_day')]][1]//div[contains(@class,'kl_day')]"))
      if (!is.na(day) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", day)) dates <- c(dates, day)
      ctxdates <- regmatches(context, gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", context))[[1]]
      if (!grepl("~", context, fixed = TRUE)) dates <- c(dates, ctxdates)
      emit(link, family, tournament, stage, context, clean(txt(nodes[[p[1]]])), clean(txt(nodes[[p[2]]])),
        if (length(wp) == 1L) clean(txt(nodes[[wp]])) else "", txt(nodes[[scores]]), dates, "INDIVIDUAL_RESULT_LIST",
        context, rank(txt(nodes[[p[1]]])), rank(txt(nodes[[p[2]]])))
    }
    for (table in xml_find_all(doc, "//table")) {
      stage <- txt(xml_find_first(table, "preceding::*[self::h3][1]")); if (is.na(stage)) stage <- ""
      cl <- xml_attr(table, "class"); if (is.na(cl)) cl <- ""
      if (grepl("final rank|members|prize", stage, ignore.case = TRUE)) next
      g <- tryCatch(grid_of(table), error = function(e) {issue(link, stage, "GRID_REVIEW", conditionMessage(e)); NULL})
      if (is.null(g)) next
      cells <- g$cells; grid <- g$grid
      header_for <- function(cell) {
        ids <- unique(grid[seq_len(max(1L, cell$row - 1L)), cell$col]); ids <- ids[!is.na(ids)]
        hs <- ids[vapply(ids, function(i) cells[[i]]$header, TRUE)]
        if (!length(hs)) return(list(text = "", items = character()))
        cells[[tail(hs, 1L)]]
      }
      row_player <- function(r) {
        ids <- unique(grid[r, 1:2]); ids <- ids[!is.na(ids)]
        for (id in ids) {
          c <- cells[[id]]
          if (!c$header && nzchar(c$text) && !grepl("^[0-9-]+$", c$text) && !length(c$items) && !length(xml_find_all(c$node, ".//table")))
            return(list(Name = clean(c$text), Rank = rank(c$text), Col = c$col))
        }
        list(Name = "", Rank = "", Col = 0L)
      }
      prs <- unique(vapply(Filter(function(c) !c$header && c$col <= 2L, cells), function(c) c$row, 0L))
      players <- lapply(prs, row_player)
      players <- players[vapply(players, function(p) nzchar(p$Name), TRUE)]
      for (cell in cells) {
        if (cell$header || length(xml_find_all(cell$node, ".//table"))) next
        match_nodes <- xml_find_all(cell$node, ".//span[contains(concat(' ',normalize-space(@class),' '),' match ')]")
        if (!length(match_nodes)) match_nodes <- list(cell$node)
        for (mn in match_nodes) {
          its <- items(mn); its <- its[nzchar(its)]
          result <- its[grepl(score_rx, its, perl = TRUE)]
          if (!length(result) && grepl(score_rx, txt(mn), perl = TRUE)) result <- txt(mn)
          if (!length(result)) next
          if (length(result) != 1L) {issue(link, stage, "MULTIPLE_RESULTS_IN_CELL", txt(mn)); next}
          h <- header_for(cell); round <- if (length(h$items)) h$items[1] else h$text
          own_dates <- its[grepl(date_rx, its)]
          dates <- if (length(own_dates)) own_dates else h$items[grepl(date_rx, h$items)]
          a <- b <- winner <- ""; ra <- rb <- ""; parser <- "BRACKET"
          rp <- row_player(cell$row)
          if (grepl("^title-", cl)) {
            parser <- "TITLE_SERIES"; winner <- rp$Name
            pn <- unique(vapply(players, function(p) p$Name, ""))
            if (length(pn) == 2L && winner %in% pn) {a <- winner; b <- setdiff(pn, winner); ra <- rp$Rank}
          } else if (grepl("^league-", cl)) {
            parser <- "LEAGUE_OR_SWISS"; a <- winner <- rp$Name; ra <- rp$Rank
            ix <- match(result, its)
            if (!is.na(ix) && ix > 1L && !grepl(date_rx, its[ix - 1L])) b <- clean(its[ix - 1L])
            else {
              oi <- cell$col - rp$Col
              if (oi >= 1L && oi <= length(players)) {b <- players[[oi]]$Name; rb <- players[[oi]]$Rank}
            }
          } else {
            ix <- match(result, its)
            if (!is.na(ix) && ix > 1L) winner <- clean(its[ix - 1L])
            if (cell$col > 1L) {
              prev <- unique(grid[cell$row:(cell$row + cell$rs - 1L), cell$col - 1L]); prev <- prev[!is.na(prev)]
              incoming <- lapply(prev, function(i) {
                c <- cells[[i]]; s <- if (length(c$items)) c$items[1] else c$text
                list(Name = clean(s), Rank = rank(s))
              })
              incoming <- incoming[vapply(incoming, function(p) nzchar(p$Name) && !grepl("^[0-9-]+$", p$Name), TRUE)]
              if (length(incoming) == 2L) {a <- incoming[[1]]$Name; b <- incoming[[2]]$Name; ra <- incoming[[1]]$Rank; rb <- incoming[[2]]$Rank}
            }
          }
          kifu <- xml_attr(xml_find_first(mn, ".//a[contains(@href,'/kifu/')]"), "href")
          if (is.na(kifu)) kifu <- ""
          emit(link, family, tournament, stage, round, a, b, winner, result, dates, parser, txt(mn), ra, rb, kifu)
        }
      }
    }
    score_leaves <- xml_find_all(doc, "//*[not(*) and (starts-with(normalize-space(.),'B+') or starts-with(normalize-space(.),'W+'))]")
    score_tokens <- sum(grepl(score_rx, trimws(xml_text(score_leaves)), perl = TRUE))
    page_stats[[length(page_stats) + 1L]] <- data.table(Link = link, Family = family, Tournament = tournament,
      ResultRows = length(rows) - before, VisibleScoreTokens = score_tokens,
      NeedsExtractionReview = score_tokens > length(rows) - before)
    if (length(rows)) fwrite(rbindlist(rows, fill = TRUE), file.path(out, "source_results.csv"))
    cat("  Page", pi, "/", nrow(pages), ":", tournament, "; rows:", length(rows) - before, "\n"); flush.console()
  }
  if (!length(rows)) stop("No result rows. Inspect the cache and edition queue.")
  source <- rbindlist(rows, fill = TRUE); source[, RowID := .I]
  entities <- unique(rbind(source[, .(Family, SourceName = PlayerA, SourceRank = RankA)],
                           source[, .(Family, SourceName = PlayerB, SourceRank = RankB)]))
  entities <- entities[nzchar(SourceName), .(SourceRank = paste(unique(SourceRank[nzchar(SourceRank)]), collapse = ";")), by = .(Family, SourceName)]
  entities[, NameKey := norm(SourceName)]; entities[, Canonical := ""]; entities[, Method := "UNRESOLVED"]
  for (i in seq_len(nrow(entities))) {
    p <- names_local[NameKey == entities$NameKey[i]]; a <- aliases[NameKey == entities$NameKey[i]]
    if (nrow(p) == 1L) set(entities, i, c("Canonical", "Method"), list(p$Canonical, "EXACT_LOCAL_NAME"))
    else if (nrow(a) == 1L) set(entities, i, c("Canonical", "Method"), list(a$Canonical, "EXISTING_LOCAL_GORATINGS_MAP"))
  }
  evidence <- list()
  cat("[3/5] Resolving identities using known opponents, dates, winners and colours...\n")
  resolve <- function(records, evidence_source) {
    setkey(records, Date)
    for (iteration in seq_len(nrow(entities) + 1L)) {
      changed <- FALSE
      for (i in which(!nzchar(entities$Canonical))) {
        ent <- entities[i]
        fs <- source[Family == ent$Family & ExtractionStatus == "EXTRACTED" & (PlayerA == ent$SourceName | PlayerB == ent$SourceName)]
        candidates <- character()
        for (j in seq_len(nrow(fs))) {
          f <- fs[j]; other <- if (f$PlayerA == ent$SourceName) f$PlayerB else f$PlayerA
          known <- entities[Family == ent$Family & SourceName == other, Canonical]
          if (length(known) != 1L || !nzchar(known)) next
          hit <- records[.(f$Date)]
          hit <- hit[norm(Black) == norm(known) | norm(White) == norm(known)]
          hit <- hit[(norm(Winner) == norm(known)) == (f$Winner == other)]
          hit <- hit[(norm(Winner) == norm(Black)) == startsWith(f$Result, "B+")]
          if (!nrow(hit)) next
          cs <- unique(ifelse(norm(hit$Black) == norm(known), hit$White, hit$Black))
          candidates <- c(candidates, cs)
          evidence[[length(evidence) + 1L]] <<- data.table(Family = ent$Family, SourceName = ent$SourceName,
            Date = f$Date, Opponent = known, Candidate = cs, EvidenceSource = evidence_source)
        }
        previous <- if (length(evidence)) rbindlist(evidence)[Family == ent$Family & SourceName == ent$SourceName, Candidate] else character()
        candidates <- unique(c(candidates, previous))
        candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
        if (length(candidates) == 1L && !nrow(entities[Family == ent$Family & SourceName != ent$SourceName & Canonical == candidates])) {
          set(entities, i, c("Canonical", "Method"), list(candidates, "OPPONENT_DATE_WINNER_COLOUR")); changed <- TRUE
        } else if (length(candidates) > 1L) set(entities, i, "Method", "CONFLICT_REVIEW")
      }
      if (!changed) break
    }
  }
  resolve(history[, .(Date, Black, White, Winner)], "LOCAL_J_RATINGS_HISTORY")
  resolve(direct[, .(Date, Black, White, Winner)], "LOCAL_GORATINGS_CACHE")
  backup_requests <- 0L
  if (!offline && !identical(Sys.getenv("GO_COVERAGE_GORATINGS_ONLINE"), "0") && nrow(entities[!nzchar(Canonical)])) {
    dir.create(file.path(out, "goratings_cache"), showWarnings = FALSE)
    targets <- character()
    for (i in which(!nzchar(entities$Canonical))) {
      e <- entities[i]; fs <- source[Family == e$Family & (PlayerA == e$SourceName | PlayerB == e$SourceName)]
      others <- setdiff(unique(c(fs$PlayerA, fs$PlayerB)), e$SourceName)
      known <- entities[Family == e$Family & SourceName %in% others & nzchar(Canonical), Canonical]
      targets <- c(targets, go_names[Canonical %in% known, GoID])
    }
    for (id in unique(targets)) {
      path <- file.path(out, "goratings_cache", paste0(id, ".html"))
      if (!file.exists(path) && backup_requests >= 8L) break
      if (!file.exists(path)) backup_requests <- backup_requests + 1L
      path <- tryCatch(fetch(paste0("https://www.goratings.org/en/players/", id, ".html"), path, 5),
        error = function(e) {cat("GoRatings backup stopped:", conditionMessage(e), "\n"); NULL})
      if (is.null(path)) break
      extra <- list(); doc <- read_html(path)
      for (r in xml_find_all(doc, "//tr[td]")) {
        td <- xml_find_all(r, "./td"); v <- trimws(xml_text(td))
        if (length(v) < 5L || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v[1]) || !v[3] %in% c("Black", "White") || !v[4] %in% c("Win", "Loss")) next
        href <- xml_attr(xml_find_first(td[[5]], ".//a"), "href")
        if (is.na(href) || !grepl("[0-9]+\\.html$", href)) next
        oid <- sub(".*?([0-9]+)\\.html$", "\\1", href)
        mine <- go_names[GoID == id, Canonical]; other <- go_names[GoID == oid, Canonical]
        if (length(mine) != 1L || length(other) != 1L) next
        extra[[length(extra) + 1L]] <- data.table(Date = v[1], Black = if (v[3] == "Black") mine else other,
          White = if (v[3] == "White") mine else other, Winner = if (v[4] == "Win") mine else other)
      }
      if (length(extra)) resolve(rbindlist(extra), "GORATINGS_PLAYER_PAGE")
    }
  }
  entities[, ProfileID := profiles$id[match(norm(Canonical), profiles$NameKey)]]
  entities[, Dan := suppressWarnings(as.integer(sub("(?:d| dan)$", "", SourceRank, perl = TRUE)))]
  entities[, Decision := fifelse(nzchar(Canonical), "MATCHED", fifelse(SourceRank == "ama" | (!is.na(Dan) & Dan < 5L),
    "SKIP_LOW_RANK_OR_AMATEUR", fifelse(!is.na(Dan) & Dan >= 5L, "FLAG_UNMATCHED_5D_PLUS", "FLAG_UNKNOWN_RANK")))]
  entities[, HasPublishedProfile := !is.na(ProfileID) & nzchar(ProfileID)]
  cat("[4/5] Comparing source games with existing history...\n")
  identity_lookup <- entities[!is.na(Canonical) & nzchar(Canonical),
    .(Canonical = if (uniqueN(Canonical) == 1L) Canonical[1L] else NA_character_),
    by = .(Key = paste(Family, norm(SourceName)))]
  lookup <- function(f, p) identity_lookup$Canonical[match(paste(f, norm(p)), identity_lookup$Key)]
  source[, CanonicalA := lookup(Family, PlayerA)]; source[, CanonicalB := lookup(Family, PlayerB)]
  source[, CanonicalWinner := lookup(Family, Winner)]; source[, Signature := ""]
  source[, EligibleExistingProfiles := norm(CanonicalA) %in% profiles$NameKey &
    norm(CanonicalB) %in% profiles$NameKey & !is.na(CanonicalA) & !is.na(CanonicalB)]
  source[ExtractionStatus == "EXTRACTED" & !is.na(CanonicalA) & nzchar(CanonicalA) & !is.na(CanonicalB) & nzchar(CanonicalB) &
    !is.na(CanonicalWinner) & nzchar(CanonicalWinner), Signature := sig(Date, CanonicalA, CanonicalB, CanonicalWinner)]
  source[, Comparison := fifelse(ExtractionStatus != "EXTRACTED", ExtractionStatus, fifelse(!nzchar(Signature),
    "IDENTITY_REVIEW", fifelse(Signature %in% history$Signature, "MATCHED_LOCAL_HISTORY", "NOT_IN_LOCAL_HISTORY")))]
  source[, ExpectedBlack := norm(fifelse(startsWith(Result, "B+"), CanonicalWinner,
    fifelse(CanonicalA == CanonicalWinner, CanonicalB, CanonicalA)))]
  counts <- history[, .(BlackKey = paste(sort(unique(norm(Black))), collapse = ";"), Occurrences = .N), by = Signature]
  source[, ExistingOccurrences := counts$Occurrences[match(Signature, counts$Signature)]]
  source[, ExistingBlack := counts$BlackKey[match(Signature, counts$Signature)]]
  source[Comparison == "MATCHED_LOCAL_HISTORY" & ExpectedBlack != ExistingBlack, Comparison := "COLOUR_CONFLICT_REVIEW"]
  source[Comparison == "MATCHED_LOCAL_HISTORY" & ExistingOccurrences > 1L, Comparison := "REPEATED_GAME_SIGNATURE_REVIEW"]
  accepted <- unique(source[Comparison == "MATCHED_LOCAL_HISTORY", Signature])
  keys <- unique(source[nzchar(Signature), Signature])
  history[, CoveredByTournamentSource := Signature %in% accepted]
  # Check the cache directly too, even when a cached game was not published.
  direct_keys <- unique(direct[, .(Signature, BlackKey = norm(Black))])
  source[, MatchesGoRatingsCache := paste(Signature, ExpectedBlack) %in%
    paste(direct_keys$Signature, direct_keys$BlackKey) & nzchar(Signature)]
  cache_accepted <- unique(source[MatchesGoRatingsCache == TRUE, Signature])
  direct[, CoveredByTournamentSource := Signature %in% cache_accepted]
  stats <- data.table(Metric = c("Existing J-Ratings rows", "Existing J-Ratings distinct signatures",
    "GoRatings cache raw ID signatures", "GoRatings cache mapped signatures", "Extracted result rows", "Fully dated/paired rows",
    "Matched J-Ratings signatures", "J-Ratings signature coverage percent", "GoRatings-cache mapped coverage percent",
    "Mapped source signatures found in J-Ratings percent"), Value = c(nrow(history), uniqueN(history$Signature), direct_raw_n,
    nrow(direct), nrow(source), source[ExtractionStatus == "EXTRACTED", .N], length(accepted),
    round(100 * length(accepted) / uniqueN(history$Signature), 2),
    if (nrow(direct)) round(100 * sum(direct$CoveredByTournamentSource) / nrow(direct), 2) else NA_real_,
    if (length(keys)) round(100 * length(accepted) / length(keys), 2) else NA_real_))
  stats <- rbind(stats, data.table(Metric = c("Calendar/navigation editions in full queue", "Editions selected in this run",
    "Skipped unavailable families", "Partial run (1=yes)"),
    Value = c(full_queue, nrow(pages), length(skipped), as.integer(nrow(pages) < full_queue))))
  yearly <- history[, .(ExistingRows = .N, ExistingSignatures = uniqueN(Signature),
    CoveredSignatures = uniqueN(Signature[CoveredByTournamentSource])), by = .(Year = substr(Date, 1, 4))]
  yearly[, CoveragePercent := round(100 * CoveredSignatures / ExistingSignatures, 2)]
  tournament_stats <- source[, .(Rows = .N, DatedPairedRows = sum(ExtractionStatus == "EXTRACTED"),
    MatchedSignatures = uniqueN(Signature[Comparison == "MATCHED_LOCAL_HISTORY"]),
    IdentityHeld = sum(Comparison == "IDENTITY_REVIEW"), ExtractionHeld = sum(ExtractionStatus != "EXTRACTED")), by = .(Family, Tournament)]
  cat("[5/5] Writing reports...\n")
  fwrite(source, file.path(out, "source_results.csv"))
  fwrite(source[Comparison != "MATCHED_LOCAL_HISTORY"], file.path(out, "held_or_unmatched_results.csv"))
  fwrite(entities, file.path(out, "player_identity_review.csv"))
  fwrite(if (length(evidence)) unique(rbindlist(evidence)) else data.table(Note = character()), file.path(out, "identity_evidence.csv"))
  fwrite(history[CoveredByTournamentSource == FALSE], file.path(out, "existing_games_not_reproduced.csv"))
  fwrite(direct[CoveredByTournamentSource == FALSE], file.path(out, "goratings_games_not_reproduced.csv"))
  fwrite(stats, file.path(out, "coverage_summary.csv")); fwrite(yearly, file.path(out, "coverage_by_year.csv"))
  fwrite(tournament_stats, file.path(out, "coverage_by_tournament.csv"))
  fwrite(if (length(issues)) rbindlist(issues) else data.table(Link = character(), Stage = character(), Issue = character(), Detail = character()), file.path(out, "parser_issues.csv"))
  fwrite(rbindlist(page_stats, fill = TRUE), file.path(out, "page_summary.csv"))
  fwrite(data.table(SkippedFamily = skipped), file.path(out, "skipped_families.csv"))
  fwrite(if (length(download_failures)) rbindlist(download_failures) else
    data.table(URL = character(), HTTPStatus = integer(), Message = character()), file.path(out, "download_failures.csv"))
  writeLines(c("# Tournament result coverage audit", "",
    paste0("Window: 2022-03-01 through ", end_date, ". Pages selected: ", nrow(pages), " of ", full_queue, "."),
    "Calendar editions are supplemented from saved same-family edition navigation; unavailable families are skipped. See edition_queue.csv.",
    "Source rows retain tournament, stage, round, players, winner, result and date evidence. No imports.",
    "Missing from extraction does NOT prove absent online: inspect parser_issues, page_summary and held_or_unmatched_results.",
    "Coverage counts distinct date/pair/winner signatures; stone colour is checked; repeated signatures are held. Score margins cannot be compared to B/W-only feeds.",
    "Identity recovery uses existing games, so this measures retrospective coverage, not independent prediction accuracy or advance fixture availability.",
    "The mapped GoRatings denominator excludes identities without existing local bridges; raw ID signature count is also reported.",
    "Team aggregates, defaults, unplayed/conditional future games are excluded. Ambiguous dates and pairings remain held.",
    "Even individual games are presumed from professional source layouts; rules have not been verified from SGFs. This is not import approval.",
    "Format classifications are research evidence; not every parser has been validated. No live data, aliases, ratings or website changes."), file.path(out, "README.md"))
  if (nrow(pages) < full_queue) cat("PARTIAL RUN: percentages below are sample coverage, not the complete source audit.\n")
  print(stats); print(yearly)
  cat("Report:", out, "\nNew requests:", requests, "; GoRatings backup requests:", backup_requests,
      "\nElapsed:", round(proc.time()[[3]] - started, 1), "seconds. No production changes.\n")
  unavailable_pages <- if (length(issues)) uniqueN(rbindlist(issues)[Issue == "PAGE_UNAVAILABLE_OR_NOT_CACHED", Link]) else 0L
  if (length(download_failures) || unavailable_pages > 0L) {
    cat("Completed with", unavailable_pages, "unavailable/uncached pages and", length(download_failures),
      "download failures: check parser_issues.csv and download_failures.csv. Coverage is incomplete.\n")
    notify("error")
  } else notify("success")
  }, error = function(e) {
    notify("error")
    stop(e)
  })
})
