# Profile the modern UEFA top-flight seasons which currently have no dated RSSSF
# rows.  This is a read-only recovery manifest: no downloads, aliases, parser
# outputs, master CSV, or site files are changed.
#
# Run:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/prepare_uefa_modern_gap_recovery.R")

prepare_uefa_modern_gap_recovery <- function() {
  required <- c("data.table", "xml2", "tictoc", "beepr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "))

  tictoc::tic("Prepare modern UEFA gap recovery")
  on.exit({
    tictoc::toc()
    try(suppressWarnings(beepr::beep()), silent = TRUE)
  }, add = TRUE)

  root <- normalizePath(Sys.getenv(
    "J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"
  ), winslash = "/", mustWork = TRUE)
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  rebuild_dir <- file.path(base, "Manual_Sources/RSSSF_UEFA_Rebuild")
  out <- file.path(rebuild_dir, "modern_gap_recovery")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  audit_path <- file.path(rebuild_dir, "season_audit.csv")
  if (!file.exists(audit_path)) stop("Missing rebuild audit: ", audit_path)
  audit <- data.table::fread(audit_path, encoding = "UTF-8")
  required_columns <- c("Country", "Season", "StartYear", "ExpectedGames", "RSSSF",
                        "LocalPath", "RSSSFResults", "RSSSFDatedResults", "Error")
  if (!all(required_columns %in% names(audit))) {
    stop("Unexpected rebuild audit schema")
  }

  # These are the high-value modern gaps: an official season with no dated rows
  # from the RSSSF-only rebuild.  Deduplicate old/modern alternate source URLs.
  queue <- audit[
    StartYear >= 2010L & (is.na(RSSSFDatedResults) | RSSSFDatedResults == 0L),
    .SD[1L], by = .(Country, Season)
  ]
  data.table::setorder(queue, Country, StartYear)

  month_pattern <- paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), collapse = "|")
  date_pattern <- paste0("\\[\\s*(?:", month_pattern,
                         ")\\.?\\s+[0-9]{1,2}(?:\\s*[-,/&]\\s*[0-9]{1,2})?")
  result_pattern <- "(?<![0-9])[0-9]{1,2}\\s*[-:]\\s*[0-9]{1,2}(?![0-9])"

  top_block_text <- function(path) {
    doc <- xml2::read_html(path)
    first <- xml2::xml_find_first(doc, "//h4[following::pre]")
    if (inherits(first, "xml_missing")) return(xml2::xml_text(doc))
    # RSSSF keeps the whole top-flight competition, including its championship
    # and relegation phases, in the PRE immediately after the first H4. Using
    # that node directly excludes cup dates in later H4 sections.
    first_pre <- xml2::xml_find_first(first, "following::pre[1]")
    paste(xml2::xml_text(first), xml2::xml_text(first_pre), sep = "\n")
  }

  profile_one <- function(row) {
    path <- as.character(row$LocalPath)
    if (is.na(path) || !nzchar(path) || !file.exists(path)) {
      return(data.table::data.table(
        Country = row$Country, Season = row$Season, StartYear = row$StartYear,
        ExpectedGames = row$ExpectedGames, RSSSF = row$RSSSF, LocalPath = path,
        PageAvailable = FALSE, DateHeadings = 0L, ResultTokens = 0L,
        HasCrossTable = FALSE, HasRoundList = FALSE, RecoveryRoute = "source_page_not_cached",
        Evidence = ""
      ))
    }
    text <- tryCatch(top_block_text(path), error = function(e) NA_character_)
    if (is.na(text)) {
      return(data.table::data.table(
        Country = row$Country, Season = row$Season, StartYear = row$StartYear,
        ExpectedGames = row$ExpectedGames, RSSSF = row$RSSSF, LocalPath = path,
        PageAvailable = TRUE, DateHeadings = 0L, ResultTokens = 0L,
        HasCrossTable = FALSE, HasRoundList = FALSE, RecoveryRoute = "unreadable_page",
        Evidence = ""
      ))
    }
    lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
    lines <- lines[nzchar(lines)]
    date_lines <- lines[grepl(date_pattern, lines, perl = TRUE, ignore.case = TRUE)]
    result_lines <- lines[grepl(result_pattern, lines, perl = TRUE)]
    cross_table <- any(grepl("cross[- ]table|results matrix", text, ignore.case = TRUE, perl = TRUE))
    round_list <- any(grepl("\\b(round|matchday|week)\\b", text, ignore.case = TRUE, perl = TRUE))
    # A full league season needs dozens of dated fixture blocks.  In these
    # pages, two or four dates are almost invariably a cup final, pro/rel tie,
    # or other end-of-season exception beside an undated cross-table; do not
    # mislabel those isolated dates as a viable parser recovery.
    route <- if (length(date_lines) >= 12L && length(result_lines)) "parser_candidate_many_dates_present" else
      if (length(date_lines)) "only_playoff_or_non_league_dates" else
      if (cross_table && length(result_lines)) "source_has_scores_but_no_fixture_dates" else
      if (length(result_lines)) "structural_review_results_without_dates" else
      "source_has_no_extractable_results"
    evidence <- unique(c(head(date_lines, 2L), head(result_lines, 2L)))
    data.table::data.table(
      Country = row$Country, Season = row$Season, StartYear = row$StartYear,
      ExpectedGames = row$ExpectedGames, RSSSF = row$RSSSF, LocalPath = path,
      PageAvailable = TRUE, DateHeadings = length(date_lines), ResultTokens = length(result_lines),
      HasCrossTable = cross_table, HasRoundList = round_list, RecoveryRoute = route,
      Evidence = paste(evidence, collapse = " | ")
    )
  }

  profiles <- vector("list", nrow(queue))
  for (i in seq_len(nrow(queue))) {
    profiles[[i]] <- profile_one(queue[i])
    if (i %% 10L == 0L || i == nrow(queue)) {
      message("Profiled ", i, "/", nrow(queue), " modern zero-row seasons")
    }
  }
  profile <- data.table::rbindlist(profiles, fill = TRUE)
  data.table::setorder(profile, RecoveryRoute, Country, StartYear)

  # A compact, deliberately varied list for manual HTML inspection. One page per
  # country/route avoids sending the same page structure through review dozens
  # of times.
  review <- profile[, .SD[1L], by = .(RecoveryRoute, Country)]
  data.table::setorder(review, RecoveryRoute, -ExpectedGames, Country, StartYear)
  review[, Priority := data.table::frank(-ExpectedGames, ties.method = "first"), by = RecoveryRoute]

  summary <- profile[, .(
    Seasons = .N,
    ExpectedGames = sum(ExpectedGames, na.rm = TRUE),
    Countries = data.table::uniqueN(Country)
  ), by = RecoveryRoute][order(-ExpectedGames, RecoveryRoute)]
  data.table::fwrite(profile, file.path(out, "modern_zero_row_manifest.csv"), na = "")
  data.table::fwrite(review, file.path(out, "varied_html_review_queue.csv"), na = "")
  data.table::fwrite(summary, file.path(out, "recovery_route_summary.csv"), na = "")

  print(summary)
  message("Wrote recovery manifest to: ", out)
  message("No downloads or production files were changed.")
  invisible(list(manifest = profile, review = review, summary = summary))
}

prepare_uefa_modern_gap_recovery()
