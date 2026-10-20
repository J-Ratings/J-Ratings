# One-off, cached research audit. Does not import games or change the live pipeline.
# source("scripts/go/audit_tournament_formats.R")
local({
  suppressPackageStartupMessages({library(jsonlite); library(data.table); library(xml2)})
  out <- "Go/pipeline_data/tournament_format_audit"
  dir.create(file.path(out, "cache"), recursive = TRUE, showWarnings = FALSE)
  last_request <- as.POSIXct(0, origin = "1970-01-01")
  fetch <- function(url, path) {
    if (file.exists(path)) return(path)
    delay <- 1.5 - as.numeric(difftime(Sys.time(), last_request, units = "secs"))
    if (delay > 0) Sys.sleep(delay)
    last_request <<- Sys.time()
    tmp <- paste0(path, ".part")
    missing <- FALSE
    status <- tryCatch(withCallingHandlers(download.file(url, tmp, method = "libcurl", mode = "wb", quiet = TRUE),
      warning = function(w) {
        if (grepl("404", conditionMessage(w))) missing <<- TRUE
      }), error = function(e) {
        if (missing) return(404L)
        stop("Download stopped: ", url, " | ", conditionMessage(e))
      })
    if (identical(status, 404L)) return(NULL)
    if (status != 0L) stop("Download stopped: ", url)
    if (!file.rename(tmp, path)) stop("Cache write failed: ", path)
    path
  }
  # The site's available calendar span established in the earlier audit.
  months <- format(seq(as.Date("2022-03-01"), as.Date("2026-11-01"), by = "month"), "%Y/%m")
  events <- list()
  for (month in months) {
    path <- fetch(paste0("https://gotoeveryone.k2ss.info/json/schedules/", month, ".json"),
                  file.path(out, "cache", paste0(gsub("/", "_", month), ".json")))
    if (is.null(path)) next
    days <- fromJSON(path, simplifyVector = FALSE)
    for (day in days) for (item in day$items) {
      if (!is.null(item$link) && grepl("^/news/[^/]+/[^/]+/[^/]+/", item$link))
        events[[length(events) + 1L]] <- data.table(Date = day$date, Label = item$name, Link = item$link)
    }
    cat("Calendar:", month, "\n"); flush.console()
  }
  events <- unique(rbindlist(events))
  events[, Family := sub("^(/news/[^/]+/[^/]+/).*", "\\1", Link)]
  fwrite(events, file.path(out, "calendar_events.csv"))
  editions <- unique(events[, .(Family, Link)])
  editions[, EditionNumber := suppressWarnings(as.integer(sub(".*/([^/]+)/$", "\\1", Link)))]
  setorder(editions, Family, -EditionNumber)
  # Latest edition of every family first; include older editions already cached.
  if (!identical(Sys.getenv("GO_AUDIT_ALL_EDITIONS"), "1")) {
    editions[, Cached := file.exists(file.path(out, "cache", paste0(gsub("[^a-zA-Z0-9]+", "_", Link), ".html")))]
    editions <- editions[, .SD[seq_len(.N) == 1L | Cached], by = Family]
  }
  # Inspect every edition, rather than assume that a family never changes format.
  stages <- list()
  for (i in seq_len(nrow(editions))) {
    link <- editions$Link[i]
    path <- fetch(paste0("https://gotoeveryone.k2ss.info", link),
                  file.path(out, "cache", paste0(gsub("[^a-zA-Z0-9]+", "_", link), ".html")))
    if (is.null(path)) {
      stages[[length(stages) + 1L]] <- data.table(Family = editions$Family[i], Link = link,
        Title = "", Stage = "SOURCE_UNAVAILABLE_404", Format = "REVIEW", Headers = "", Rows = 0L, TableClass = "")
      next
    }
    doc <- read_html(path)
    title <- trimws(xml_text(xml_find_first(doc, "//h1")))
    tables <- xml_find_all(doc, "//table | //ul[contains(concat(' ', normalize-space(@class), ' '), ' table ')]")
    if (!length(tables)) {
      stages[[length(stages) + 1L]] <- data.table(Family = editions$Family[i], Link = link,
        Title = title, Stage = "NO_HTML_TABLE", Format = "REVIEW", Headers = "", Rows = 0L, TableClass = "")
    }
    for (table in tables) {
      heading <- trimws(xml_text(xml_find_first(table, "preceding::*[self::h2 or self::h3 or self::h4][1]")))
      headers <- trimws(xml_text(xml_find_all(table, "(./tr | ./thead/tr | ./tbody/tr)[1]/*[self::th or self::td]")))
      if (xml_name(table) == "ul") headers <- trimws(xml_text(xml_find_all(table, "./li[contains(@class,'table-row-header')]/*")))
      header <- paste(headers, collapse = " | ")
      class <- xml_attr(table, "class")
      if (is.na(class)) class <- ""
      rows <- xml_find_all(table, ".//tr")
      if (xml_name(table) == "ul") rows <- xml_find_all(table, "./li")
      spans <- suppressWarnings(as.integer(xml_attr(xml_find_all(table, ".//td[@rowspan]"), "rowspan")))
      kind <- "REVIEW"
      if (grepl("league|round.robin", heading, ignore.case = TRUE)) kind <- "ROUND_ROBIN"
      if (grepl("title match", heading, ignore.case = TRUE)) kind <- "TITLE_SERIES"
      if (grepl("^title-", class)) kind <- "TITLE_SERIES"
      if (kind == "REVIEW" && grepl("quarterfinal|semifinal|1st round|2nd round|Final", header, ignore.case = TRUE) && any(spans >= 2L)) kind <- "KNOCKOUT"
      if (kind == "REVIEW" && grepl("Players.*Result", header, ignore.case = TRUE)) kind <- "SINGLE_MATCH"
      if (grepl("Knockout competition", heading, ignore.case = TRUE)) kind <- "SURVIVAL_REVIEW"
      if (grepl("Loser's tournament", xml_text(table), fixed = TRUE)) kind <- "DOUBLE_ELIMINATION"
      if (grepl("SOS", header)) kind <- "SWISS_STYLE"
      if (kind == "ROUND_ROBIN" && (!nzchar(header) || identical(header, "Rank | Players | Result"))) kind <- "LEAGUE_DETAILS_REVIEW"
      if (kind == "ROUND_ROBIN" && grepl("decision (match|tournament)", heading, ignore.case = TRUE)) kind <- "PLAYOFF_REVIEW"
      if (kind == "REVIEW" && grepl("Rank.*Players.*Win.*Loss", header)) kind <- "NON_FIXTURE_TABLE"
      if (grepl("final rank|member|prize", heading, ignore.case = TRUE)) kind <- "NON_FIXTURE_TABLE"
      if (grepl("team|nations|relay", paste(title, heading, header), ignore.case = TRUE)) kind <- paste0("TEAM_REVIEW_", kind)
      if (editions$Family[i] %in% c("/news/cn/al/", "/news/jp/fleague/", "/news/kr/kl/") && kind != "NON_FIXTURE_TABLE") kind <- paste0("TEAM_", sub("^TEAM_REVIEW_", "", kind))
      if (kind == "SURVIVAL_REVIEW") kind <- if (editions$Family[i] %in% c("/news/cn/lx/", "/news/jp/ryusei/")) "SURVIVAL_LADDER" else "TEAM_RELAY"
      if (grepl("Block tournament", heading, fixed = TRUE) && kind == "REVIEW") kind <- "GROUP_QUALIFICATION_REVIEW"
      # Reviewed exceptions from the saved source layouts, keyed by family + stage.
      if (editions$Family[i] == "/news/wr/gc/" && grepl("Block tournament", heading, fixed = TRUE)) kind <- "DOUBLE_ELIMINATION_GROUPS"
      if (editions$Family[i] == "/news/jp/kisei/" && heading == "League B 1st place decision match") kind <- "SINGLE_MATCH"
      if (editions$Family[i] == "/news/jp/teikei-s/" && heading == "League B 1st place decision tournament") kind <- "LADDER_PLAYOFF"
      stages[[length(stages) + 1L]] <- data.table(Family = editions$Family[i], Link = link,
        Title = title, Stage = heading, Format = kind, Headers = header, Rows = length(rows), TableClass = class,
        HasSeriesCells = length(xml_find_all(table, ".//span[contains(concat(' ',normalize-space(@class),' '),' match ')]")) > 0L)
    }
    cat("Edition:", i, "/", nrow(editions), link, "\n"); flush.console()
    fwrite(rbindlist(stages, fill = TRUE), file.path(out, "stage_evidence.csv"))
  }
  stages <- rbindlist(stages, fill = TRUE)
  families <- stages[, .(Title = Title[1], Editions = uniqueN(Link),
     Formats = paste(sort(unique(Format)), collapse = "; "),
     StageNames = paste(unique(Stage), collapse = "; "),
     NeedsReview = any(grepl("REVIEW", Format))), by = Family]
  families[, CalendarEditions := vapply(Family, function(f) uniqueN(events[Family == f, Link]), 0L)]
  families[, SourceURL := paste0("https://gotoeveryone.k2ss.info", vapply(Family, function(f) editions[Family == f, Link][1], ""))]
  families[, ParserValidated := FALSE]
  families[, Classification := vapply(Family, function(f) {
    types <- unique(stages[Family == f & Format != "NON_FIXTURE_TABLE", Format])
    if (any(grepl("TEAM_", types))) return("Team / mixed team formats")
    if (any(types == "SWISS_STYLE")) return("Swiss style")
    if (identical(types, "DOUBLE_ELIMINATION")) return("Double elimination")
    if (all(grepl("REVIEW", types))) return("Unresolved / source unavailable")
    if (length(setdiff(types, c("KNOCKOUT", "SINGLE_MATCH"))) == 0L)
      return(if ("KNOCKOUT" %in% types) "Knockout (with optional placement matches)" else "Single match")
    if (all(types %in% c("ROUND_ROBIN", "SINGLE_MATCH"))) return("Round robin (with optional playoff)")
    "Mixed stages"
  }, "")]
  fwrite(families, file.path(out, "tournament_registry.csv"))
  md <- c("# Go tournament format inventory", "",
    "Calendar coverage: March 2022–November 2026. 104 URL-based families, 348 linked editions.",
    "Latest linked edition of each family inspected, plus older cached editions. This is not a review of every historical edition or proof of advance fixture availability.",
    "Classification uses source table structures. NeedsReview marks uncertain stages; ParserValidated is false until each parser has been tested. A known family must still be checked for unexpected layout changes.", "",
    "| Region | Tournament | Classification | Stage formats | Needs review |", "|---|---|---|---|---|")
  for (i in seq_len(nrow(families))) {
    x <- families[i]
    name <- if (nzchar(x$Title)) x$Title else sub("^/news/[^/]+/([^/]+)/$", "\\1", x$Family)
    md <- c(md, sprintf("| %s | [%s](%s) | %s | %s | %s |",
       sub("^/news/([^/]+)/.*", "\\1", x$Family), name, x$SourceURL,
       x$Classification, x$Formats, if (x$NeedsReview) "Yes" else "No"))
  }
  writeLines(md, file.path(out, "README.md"))
  print(families[, .N, by = Classification])
  cat("Families:", nrow(families), "; editions:", nrow(editions), "\n")
})
