# RSSSF-only historical UEFA top-flight rebuild.
#
# The cached RSSSF pages provide BOTH results and dates. Wikipedia is not read
# and no production CSV is changed. A normal run reparses every selected season.
# RSSSF_REBUILD_RECHECK_STATUSES can refresh only named audit statuses and merge
# them back into the existing complete output.
#
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/rsssf_uefa_rebuild.R")
#
# Defaults: all cached historical seasons ending before 2025/26, no downloads.
# Optional filters:
# Sys.setenv(RSSSF_REBUILD_COUNTRIES="Hungary,Cyprus")
# Sys.setenv(RSSSF_REBUILD_MIN_YEAR="2010", RSSSF_REBUILD_MAX_YEAR="2024")
# Sys.setenv(RSSSF_REBUILD_RECHECK_STATUSES="suspicious_overcount")

run_rsssf_uefa_rebuild <- function() {
  required <- c("data.table", "xml2", "rvest", "stringi", "tictoc", "beepr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "))

  tictoc::tic("RSSSF-only UEFA rebuild")
  on.exit({
    tictoc::toc()
    try(suppressWarnings(beepr::beep()), silent = TRUE)
  }, add = TRUE)

  root <- normalizePath(Sys.getenv(
    "J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"
  ), winslash = "/", mustWork = TRUE)
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  old_dir <- file.path(base, "Manual_Sources/Wikipedia_RSSSF_Alias_Audit")
  out <- file.path(base, "Manual_Sources/RSSSF_UEFA_Rebuild")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  manifest_path <- file.path(old_dir, "season_audit.csv")
  if (!file.exists(manifest_path)) stop("Missing source manifest: ", manifest_path)

  previous <- Sys.getenv("FOUR_LEAGUES_FUNCTIONS_ONLY", unset = NA_character_)
  on.exit({
    if (is.na(previous)) Sys.unsetenv("FOUR_LEAGUES_FUNCTIONS_ONLY") else
      Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY = previous)
  }, add = TRUE)
  Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY = "1")
  engine <- new.env(parent = globalenv())
  source(file.path(root, "scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R"),
         local = engine, encoding = "UTF-8")

  data.table::setDTthreads(0L)
  manifest <- data.table::fread(manifest_path, encoding = "UTF-8")
  needed <- c("Country", "Season", "StartYear", "RSSSF")
  if (!all(needed %in% names(manifest))) stop("Unexpected season_audit.csv schema")
  for (nm in c("Error", "RSSSFResults", "RSSSFDatedResults", "WikipediaGames"))
    if (!nm %in% names(manifest)) manifest[, (nm) := if (nm == "Error") "" else NA_integer_]
  old_candidates <- file.path(old_dir, "dated_candidate.csv")
  if (!file.exists(old_candidates)) stop("Missing league-name reference: ", old_candidates)
  league_map <- unique(data.table::fread(old_candidates,
    select = c("Country", "League"), encoding = "UTF-8"))
  if (league_map[, anyDuplicated(Country)]) stop("More than one league name found for a country")
  manifest <- league_map[manifest, on = "Country"]
  if (anyNA(manifest$League)) stop("At least one country has no league-name mapping")

  min_year <- as.integer(Sys.getenv("RSSSF_REBUILD_MIN_YEAR", "1900"))
  # 2024/25 is the final season before the planned 2025/26 checkpoint.
  max_year <- as.integer(Sys.getenv("RSSSF_REBUILD_MAX_YEAR", "2024"))
  stopifnot(!is.na(min_year), !is.na(max_year), min_year <= max_year)
  requested <- trimws(strsplit(Sys.getenv(
    "RSSSF_REBUILD_COUNTRIES", paste(sort(unique(manifest$Country)), collapse = ",")
  ), ",", fixed = TRUE)[[1L]])
  unknown <- setdiff(requested, unique(manifest$Country))
  if (length(unknown)) stop("Unknown countries: ", paste(unknown, collapse = ", "))

  manifest[, StartYear := as.integer(StartYear)]
  manifest <- manifest[Country %in% requested & StartYear >= min_year & StartYear <= max_year &
                         !is.na(RSSSF) & nzchar(RSSSF)]
  # Old and modern prefixes can occasionally identify the same season. Prefer
  # the page that previously yielded the most RSSSF evidence, then a clean row.
  manifest[, HasError := nzchar(data.table::fcoalesce(as.character(Error), ""))]
  data.table::setorder(manifest, Country, StartYear, HasError, -RSSSFDatedResults, -RSSSFResults)
  manifest <- manifest[, .SD[1L], by = .(Country, Season)]
  data.table::setorder(manifest, Country, StartYear)
  if (!nrow(manifest)) stop("No seasons matched the requested filters.")

  recheck_statuses <- trimws(strsplit(Sys.getenv("RSSSF_REBUILD_RECHECK_STATUSES", ""),
                                     ",", fixed = TRUE)[[1L]])
  recheck_statuses <- recheck_statuses[nzchar(recheck_statuses)]
  previous_audit <- data.table::data.table()
  previous_games <- data.table::data.table()
  if (length(recheck_statuses)) {
    previous_audit_path <- file.path(out, "season_audit.csv")
    previous_games_path <- file.path(out, "all_rsssf_games.csv")
    if (!file.exists(previous_audit_path) || !file.exists(previous_games_path))
      stop("Targeted recheck requires an existing complete season_audit.csv and all_rsssf_games.csv")
    previous_audit <- data.table::fread(previous_audit_path, encoding = "UTF-8")
    previous_games <- data.table::fread(previous_games_path, encoding = "UTF-8")
    targets <- unique(previous_audit[QualityStatus %in% recheck_statuses, .(Country, Season)])
    targets <- unique(manifest[targets, on = .(Country, Season), nomatch = 0L,
                               .(Country, Season)])
    manifest <- manifest[targets, on = .(Country, Season), nomatch = 0L]
    if (!nrow(manifest)) stop("No existing seasons have the requested recheck statuses")
    previous_audit <- previous_audit[!targets, on = .(Country, Season)]
    previous_games <- previous_games[!targets, on = .(Country, Season)]
    message("Targeted recheck: parsing ", nrow(manifest), " seasons with status: ",
            paste(recheck_statuses, collapse = ", "), ". Other seasons will be reused.")
    data.table::fwrite(manifest, file.path(out, "recheck_manifest.csv"))
  } else {
    data.table::fwrite(manifest, file.path(out, "source_manifest.csv"))
  }

  locate_page <- function(url) {
    relative <- sub("^https://www[.]rsssf[.]org/", "", url)
    cache_name <- paste0(gsub("[^A-Za-z0-9._-]", "_", URLdecode(url)), ".html")
    candidates <- c(
      file.path(base, "Source/rsssf/all/pages", relative),
      file.path(old_dir, "cache", cache_name),
      file.path(base, "Manual_Sources/Wikipedia_RSSSF_Remaining_UEFA_Leagues/cache", cache_name),
      file.path(base, "Manual_Sources/Wikipedia_RSSSF_Four_Leagues/cache", cache_name)
    )
    hit <- candidates[file.exists(candidates)]
    if (length(hit)) normalizePath(hit[1L], winslash = "/", mustWork = TRUE) else NA_character_
  }

  top_flight_document <- function(doc) {
    # Modern RSSSF country pages normally put each competition in an H4 block.
    # Keep the first competition and its internal championship/relegation phases,
    # then discard later cups, lower divisions and duplicate Details sections.
    clipped <- xml2::read_html(as.character(doc))
    first_competition <- xml2::xml_find_first(clipped, "//h4[following::pre]")
    if (inherits(first_competition, "xml_missing"))
      return(list(doc = clipped, method = "legacy_full_page_no_h4", heading = ""))
    heading <- engine$clean_text(xml2::xml_text(first_competition))
    next_competition <- xml2::xml_find_first(first_competition, "following::h4[1]")
    if (!inherits(next_competition, "xml_missing"))
      xml2::xml_remove(xml2::xml_find_all(next_competition, ". | following::*"))
    # A separately named prorel block is a cross-tier playoff, not the league's
    # championship/relegation group. Remove it and anything after it.
    cross_tier <- xml2::xml_find_first(clipped,
      "//b[a[translate(@name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='prorel']] | //h3[a[translate(@name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='prorel']]")
    if (!inherits(cross_tier, "xml_missing")) {
      # RSSSF often puts the anchor and all subsequent fixtures inside one PRE.
      # Removing following elements alone leaves those plain-text fixture nodes
      # behind, so remove every following sibling node within the PRE as well.
      in_pre <- xml2::xml_find_first(cross_tier, "ancestor::pre[1]")
      if (!inherits(in_pre, "xml_missing"))
        xml2::xml_remove(xml2::xml_find_all(cross_tier, ". | following-sibling::node()"))
      else
        xml2::xml_remove(xml2::xml_find_all(cross_tier, ". | following::node()"))
    }
    list(doc = clipped, method = "first_h4_top_flight_block", heading = heading)
  }

  resolve_round_years <- function(parsed, start_year, end_year) {
    # In the extended 2019/20 season, bare Aug/Sep headings can mean either
    # year. RSSSF's explicit round number supplies local evidence: use the year
    # half of the competition's numbered rounds. This also handles postponed
    # games listed under an early round but played in the following May: the
    # August fixtures around them still belong to the starting year.
    if (start_year == end_year || !nrow(parsed)) return(parsed)
    round_number <- function(x) suppressWarnings(as.integer(sub(
      "(?i)^.*?round +([0-9]+).*$", "\\1", x, perl = TRUE
    )))
    rounds <- round_number(parsed$RSSSFStage)
    inferred_by_neighbor <- grepl("^same_phase_neighbor_year", parsed$DateBasis)
    targets <- which((is.na(parsed$Date) | inferred_by_neighbor) & !parsed$Annotated &
                       !is.na(rounds) & nzchar(parsed$DateHeading))
    for (j in targets) {
      same_block <- which(parsed$RSSSFSection == parsed$RSSSFSection[j] &
                            parsed$RSSSFPhase == parsed$RSSSFPhase[j] & !is.na(rounds))
      max_round <- max(rounds[same_block], na.rm = TRUE)
      if (!is.finite(max_round) || max_round < 2L) next
      inferred_year <- if (rounds[j] <= ceiling(max_round / 2)) start_year else end_year
      recovered <- engine$rsssf_date(parsed$DateHeading[j], start_year, end_year,
                                     year_hint = inferred_year)
      if (!is.na(recovered)) parsed[j, `:=`(
        Date = recovered,
        DateBasis = paste0("numbered_round_half_year:", inferred_year)
      )]
    }
    parsed
  }

  audits <- list()
  games_out <- list()
  errors <- 0L
  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i]
    key <- paste(row$Country, gsub("/", "-", row$Season), sep = "_")
    local_path <- locate_page(row$RSSSF)
    message(sprintf("[%d/%d] %s %s", i, nrow(manifest), row$Country, row$Season))
    began <- proc.time()[["elapsed"]]
    result <- tryCatch({
      if (is.na(local_path)) stop("RSSSF page is not cached")
      raw_doc <- xml2::read_html(local_path)
      isolated <- top_flight_document(raw_doc)
      end_year <- row$StartYear + as.integer(grepl("/", row$Season, fixed = TRUE))
      parsed <- engine$rsssf_games(isolated$doc, row$StartYear, end_year)
      if (!nrow(parsed)) stop("No top-flight result rows extracted")
      parsed <- data.table::copy(resolve_round_years(parsed, row$StartYear, end_year))
      source_assessment <- attr(parsed, "source_assessment")

      parsed[, `:=`(
        Country = row$Country,
        Season = row$Season,
        StartYear = row$StartYear,
        Competition = row$League,
        CompetitionType = "league",
        Tier = 1L,
        League = row$League,
        Home = RHome,
        Away = RAway,
        Source = "rsssf",
        SourcePage = row$RSSSF,
        SourceFile = local_path,
        Stage = RSSSFStage,
        DateApprox = FALSE,
        DateStatus = data.table::fifelse(!is.na(Date), "matched", "rsssf_date_missing_or_unresolved"),
        MatchMethod = data.table::fifelse(!is.na(Date), "rsssf_result_and_date", "rsssf_result_without_exact_date")
      )]
      parsed[, Result := vapply(strsplit(Score, "-", fixed = TRUE), function(s) {
        score <- as.integer(s)
        if (score[1L] > score[2L]) "1-0" else if (score[1L] < score[2L]) "0-1" else "0.5-0.5"
      }, character(1))]

      # SourceLine distinguishes genuine repeats with the same clubs, score and
      # date. Never collapse them merely because their fixture keys coincide.
      parsed[, SourceFixtureId := sprintf("%s:%s", basename(local_path), SourceLine)]
      season_dir <- file.path(out, key)
      dir.create(season_dir, showWarnings = FALSE)
      data.table::fwrite(parsed, file.path(season_dir, "rsssf_games.csv"), na = "")
      data.table::fwrite(parsed[is.na(Date)], file.path(season_dir, "unresolved.csv"), na = "")
      games_out[[key]] <- parsed

      expected <- suppressWarnings(as.integer(row$WikipediaGames))
      dated <- sum(!is.na(parsed$Date))
      played <- sum(!parsed$Annotated)
      coverage <- if (!is.na(expected) && expected > 0L) round(100 * dated / expected, 2) else NA_real_
      dated_share <- if (played > 0L) round(100 * dated / played, 2) else NA_real_
      reference_status <- if (is.na(expected) || expected == 0L) "no_reference_count" else
        if (dated > expected * 1.05) "rsssf_above_reference_105" else
        if (dated < expected * 0.95) "rsssf_below_reference_95" else "within_reference_range"
      # The old Wikipedia count is a useful warning, but it often omits valid
      # championship/relegation phases. Once cups, lower levels and named
      # cross-tier prorel blocks are structurally excluded, more than 100% is
      # not itself a reason to quarantine a season.
      # A slightly higher RSSSF count can be a legitimate championship or
      # relegation phase that the old Wikipedia reference omitted.  At 125%+
      # it is overwhelmingly a remaining cup/lower-tier boundary failure, so
      # preserve the season for review rather than letting it enter candidates.
      quality <- if (is.na(expected) || expected == 0L)
        "no_reference_count" else if (dated > expected * 1.25)
        "suspicious_overcount" else if (dated >= expected * 0.95)
        "at_least_95" else if (dated < expected * 0.75)
        "partial_source_coverage" else "below_95"
      data.table::data.table(
        Country = row$Country, Season = row$Season, StartYear = row$StartYear,
        ExpectedGames = expected, RSSSFResults = nrow(parsed),
        RSSSFPlayedResults = played, RSSSFDatedResults = dated,
        CoveragePercent = coverage, DatedShareOfRSSSFPercent = dated_share,
        CountDifference = if (!is.na(expected)) nrow(parsed) - expected else NA_integer_,
        DatedCountDifference = if (!is.na(expected)) dated - expected else NA_integer_,
        ReferenceCountStatus = reference_status,
        QualityStatus = quality, IsolationMethod = isolated$method,
        TopFlightHeading = isolated$heading, SourceAssessment = source_assessment, RSSSF = row$RSSSF,
        LocalPath = local_path, Error = ""
      )
    }, error = function(e) {
      errors <<- errors + 1L
      data.table::data.table(
        Country = row$Country, Season = row$Season, StartYear = row$StartYear,
        ExpectedGames = suppressWarnings(as.integer(row$WikipediaGames)),
        RSSSFResults = 0L, RSSSFPlayedResults = 0L, RSSSFDatedResults = 0L,
        CoveragePercent = NA_real_, DatedShareOfRSSSFPercent = NA_real_,
        CountDifference = NA_integer_, DatedCountDifference = NA_integer_,
        ReferenceCountStatus = "season_error",
        QualityStatus = "season_error", IsolationMethod = "", TopFlightHeading = "",
        SourceAssessment = "", RSSSF = row$RSSSF,
        LocalPath = local_path, Error = conditionMessage(e)
      )
    })
    result[, ElapsedSeconds := round(proc.time()[["elapsed"]] - began, 3)]
    audits[[key]] <- result
    audit_now <- data.table::rbindlist(c(list(previous_audit), audits), fill = TRUE)
    progress_path <- if (length(recheck_statuses)) "recheck_progress_audit.csv" else "season_audit.csv"
    data.table::fwrite(audit_now, file.path(out, progress_path), na = "")
    print(result[, .(Country, Season, ExpectedGames, RSSSFResults,
                     RSSSFDatedResults, CoveragePercent,
                     DatedShareOfRSSSFPercent, QualityStatus, Error)])
  }

  combined <- data.table::rbindlist(c(list(previous_games), games_out), fill = TRUE)
  dated <- combined[!is.na(Date)]
  audit <- data.table::rbindlist(c(list(previous_audit), audits), fill = TRUE)
  data.table::setorder(audit, Country, StartYear)
  data.table::fwrite(audit, file.path(out, "season_audit.csv"), na = "")
  safe_seasons <- audit[
    !nzchar(Error) & !QualityStatus %in% c("suspicious_overcount", "partial_source_coverage"),
    .(Country, Season)
  ]
  safe_dated <- dated[safe_seasons, on = .(Country, Season), nomatch = 0L]
  quarantined <- dated[!safe_seasons, on = .(Country, Season)]
  data.table::fwrite(combined, file.path(out, "all_rsssf_games.csv"), na = "")
  data.table::fwrite(dated, file.path(out, "all_dated_rsssf_games.csv"), na = "")
  data.table::fwrite(safe_dated, file.path(out, "dated_candidate.csv"), na = "")
  data.table::fwrite(quarantined, file.path(out, "quarantined_overcounts.csv"), na = "")
  data.table::fwrite(combined[is.na(Date)], file.path(out, "unresolved.csv"), na = "")
  summarise_coverage <- function(x) x[, {
    has_reference <- !is.na(ExpectedGames) & ExpectedGames > 0L
    reference_expected <- sum(ExpectedGames[has_reference])
    reference_dated <- sum(RSSSFDatedResults[has_reference])
    safe_season <- !QualityStatus %in% c("season_error", "suspicious_overcount", "partial_source_coverage")
    played_results <- data.table::fcoalesce(
      as.integer(RSSSFPlayedResults), as.integer(RSSSFResults)
    )
    list(
      Seasons = .N,
      ReferenceSeasons = sum(has_reference),
      SeasonsWithoutReference = sum(!has_reference),
      SeasonErrors = sum(nzchar(Error)),
      SeasonsAtLeast95 = sum(QualityStatus == "at_least_95"),
      SuspiciousOvercounts = sum(QualityStatus == "suspicious_overcount"),
      HighReferenceMismatches = sum(ReferenceCountStatus == "rsssf_above_reference_105", na.rm = TRUE),
      SeasonsBelow95 = sum(QualityStatus == "below_95"),
      SafeSeasons = sum(safe_season),
      GoodShareOfSafeSeasonsPercent = if (sum(safe_season))
        round(100 * sum(QualityStatus == "at_least_95") / sum(safe_season), 2) else NA_real_,
      ExpectedGames = reference_expected,
      ReferenceDatedResults = reference_dated,
      RSSSFResults = sum(RSSSFResults),
      RSSSFPlayedResults = sum(played_results),
      RSSSFDatedResults = sum(RSSSFDatedResults),
      CoveragePercent = if (reference_expected > 0L)
        round(100 * reference_dated / reference_expected, 2) else NA_real_,
      DatedShareOfRSSSFPercent = if (sum(played_results) > 0L)
        round(100 * sum(RSSSFDatedResults) / sum(played_results), 2) else NA_real_
    )
  }, by = Country][order(Country)]
  summary <- summarise_coverage(audit)
  recent_summary <- summarise_coverage(audit[StartYear >= 2010L])
  data.table::fwrite(summary, file.path(out, "coverage_summary.csv"), na = "")
  data.table::fwrite(recent_summary, file.path(out, "coverage_summary_2010_onwards.csv"), na = "")
  print(summary)
  message("RSSSF-only dated rows: ", nrow(dated), "; candidate rows after structural/error quarantine: ",
          nrow(safe_dated), "; quarantined rows: ", nrow(quarantined),
          ". Season errors: ", sum(nzchar(audit$Error)), ".")
  message("Review ", file.path(out, "season_audit.csv"), ". Production master unchanged.")
  invisible(list(games = combined, audit = audit, summary = summary))
}

run_rsssf_uefa_rebuild()
