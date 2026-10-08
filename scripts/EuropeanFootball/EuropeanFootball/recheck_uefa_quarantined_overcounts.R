# Read-only diagnostic for RSSSF UEFA seasons previously quarantined as
# suspicious overcounts.  It applies the current top-flight page isolation and
# reports whether the result now resembles a plausible top-flight season.
#
# No downloads, aliases, rebuild output, master CSV, or site files are changed.
# Run:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/recheck_uefa_quarantined_overcounts.R")

recheck_uefa_quarantined_overcounts <- function() {
  required <- c("data.table", "xml2", "rvest", "stringi", "tictoc", "beepr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "))

  tictoc::tic("Recheck quarantined UEFA RSSSF seasons")
  on.exit({ tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE) }, add = TRUE)

  root <- normalizePath(Sys.getenv(
    "J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"
  ), winslash = "/", mustWork = TRUE)
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  rebuild_dir <- file.path(base, "Manual_Sources/RSSSF_UEFA_Rebuild")
  out <- file.path(rebuild_dir, "quarantined_overcount_recheck")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  old <- Sys.getenv("FOUR_LEAGUES_FUNCTIONS_ONLY", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("FOUR_LEAGUES_FUNCTIONS_ONLY") else
    Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY = old), add = TRUE)
  Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY = "1")
  engine <- new.env(parent = globalenv())
  # `source()` can misread UTF-8 smart punctuation under a non-UTF-8 Rscript
  # locale on Windows. Parse explicitly with the file encoding instead.
  engine_file <- file.path(root, "scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")
  eval(parse(engine_file, encoding = "UTF-8"), envir = engine)

  audit <- data.table::fread(file.path(rebuild_dir, "season_audit.csv"), encoding = "UTF-8")
  cases <- audit[QualityStatus == "suspicious_overcount"]
  # A few historical manifests retain alternate RSSSF URLs for the same
  # country-season. Assess the strongest old extraction once, rather than
  # spending time reparsing duplicate source variants.
  data.table::setorder(cases, Country, Season, -RSSSFDatedResults, -RSSSFResults)
  cases <- cases[, .SD[1L], by = .(Country, Season)]
  if (!nrow(cases)) stop("No suspicious-overcount seasons found in the current audit.")
  data.table::setorder(cases, Country, StartYear)

  top_flight_document <- function(doc) {
    clipped <- xml2::read_html(as.character(doc))
    first_competition <- xml2::xml_find_first(clipped, "//h4[following::pre]")
    if (inherits(first_competition, "xml_missing")) {
      return(list(doc = clipped, method = "legacy_full_page_no_h4", heading = ""))
    }
    heading <- engine$clean_text(xml2::xml_text(first_competition))
    next_competition <- xml2::xml_find_first(first_competition, "following::h4[1]")
    if (!inherits(next_competition, "xml_missing")) {
      xml2::xml_remove(xml2::xml_find_all(next_competition, ". | following::*"))
    }
    cross_tier <- xml2::xml_find_first(clipped,
      "//b[a[translate(@name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='prorel']] | //h3[a[translate(@name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='prorel']]")
    if (!inherits(cross_tier, "xml_missing")) {
      in_pre <- xml2::xml_find_first(cross_tier, "ancestor::pre[1]")
      if (!inherits(in_pre, "xml_missing"))
        xml2::xml_remove(xml2::xml_find_all(cross_tier, ". | following-sibling::node()"))
      else
        xml2::xml_remove(xml2::xml_find_all(cross_tier, ". | following::node()"))
    }
    list(doc = clipped, method = "first_h4_top_flight_block", heading = heading)
  }

  results <- vector("list", nrow(cases))
  for (i in seq_len(nrow(cases))) {
    row <- cases[i]
    outcome <- tryCatch({
      if (is.na(row$LocalPath) || !file.exists(row$LocalPath)) stop("RSSSF page is not cached")
      isolated <- top_flight_document(xml2::read_html(row$LocalPath))
      end_year <- row$StartYear + as.integer(grepl("/", row$Season, fixed = TRUE))
      parsed <- engine$rsssf_games(isolated$doc, row$StartYear, end_year)
      played <- if (nrow(parsed)) sum(!parsed$Annotated) else 0L
      dated <- if (nrow(parsed)) sum(!is.na(parsed$Date)) else 0L
      expected <- as.integer(row$ExpectedGames)
      ratio <- if (!is.na(expected) && expected > 0L) dated / expected else NA_real_
      route <- if (!nrow(parsed)) "no_rows_after_current_isolation" else
        if (is.na(ratio)) "rows_without_reference_count" else
        if (ratio >= .75 && ratio <= 1.25) "plausible_recovery_candidate" else
        if (ratio > 1.25) "still_overcount_needs_structure_review" else
        "under_reference_needs_structure_review"
      data.table::data.table(
        Country = row$Country, Season = row$Season, StartYear = row$StartYear,
        ExpectedGames = expected, OldDatedRows = row$RSSSFDatedResults,
        CurrentResults = nrow(parsed), CurrentPlayedResults = played,
        CurrentDatedResults = dated, CurrentPercentOfExpected = round(100 * ratio, 2),
        RecoveryAssessment = route, IsolationMethod = isolated$method,
        TopFlightHeading = isolated$heading, SourceAssessment = attr(parsed, "source_assessment"),
        RSSSF = row$RSSSF, LocalPath = row$LocalPath, Error = ""
      )
    }, error = function(e) data.table::data.table(
      Country = row$Country, Season = row$Season, StartYear = row$StartYear,
      ExpectedGames = row$ExpectedGames, OldDatedRows = row$RSSSFDatedResults,
      CurrentResults = 0L, CurrentPlayedResults = 0L, CurrentDatedResults = 0L,
      CurrentPercentOfExpected = NA_real_, RecoveryAssessment = "recheck_error",
      IsolationMethod = "", TopFlightHeading = "", SourceAssessment = "",
      RSSSF = row$RSSSF, LocalPath = row$LocalPath, Error = conditionMessage(e)
    ))
    results[[i]] <- outcome
    if (i %% 10L == 0L || i == nrow(cases)) {
      message("Rechecked ", i, "/", nrow(cases), " quarantined seasons")
    }
  }

  report <- data.table::rbindlist(results, fill = TRUE)
  data.table::setorder(report, RecoveryAssessment, Country, StartYear)
  summary <- report[, .(
    Seasons = .N,
    ExpectedGames = sum(ExpectedGames, na.rm = TRUE),
    CurrentDatedResults = sum(CurrentDatedResults, na.rm = TRUE),
    Countries = data.table::uniqueN(Country)
  ), by = RecoveryAssessment][order(-ExpectedGames, RecoveryAssessment)]
  candidates <- report[RecoveryAssessment == "plausible_recovery_candidate"]
  data.table::fwrite(report, file.path(out, "recheck_detail.csv"), na = "")
  data.table::fwrite(summary, file.path(out, "recheck_summary.csv"), na = "")
  data.table::fwrite(candidates, file.path(out, "plausible_recovery_candidates.csv"), na = "")
  print(summary)
  message("Plausible recovery candidates: ", nrow(candidates), ".")
  message("Review written to: ", out)
  message("No production or rebuild files were changed.")
  invisible(list(detail = report, summary = summary, candidates = candidates))
}

recheck_uefa_quarantined_overcounts()
