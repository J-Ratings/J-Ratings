# RSSSF-only OFC audit: domestic top flights plus OFC club competition.
#
# Uses cached HTML files only. No web requests and no production changes.
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/rsssf_ofc_audit.R")
#
# Optional filters:
# Sys.setenv(OFC_AUDIT_COUNTRIES = "New Zealand,Fiji,Oceania")
# Sys.setenv(OFC_AUDIT_MIN_YEAR = "2010", OFC_AUDIT_MAX_YEAR = "2025")
# Reparse only pages whose earlier result needed structural review:
# Sys.setenv(OFC_AUDIT_RECHECK_STRUCTURAL = "1")

run_rsssf_ofc_audit <- function(config_override = NULL,
                                audit_name = "OFC",
                                output_folder = "RSSSF_OFC_Audit",
                                env_prefix = "OFC_AUDIT",
                                completion_beep = TRUE) {
  required <- c("data.table", "xml2", "stringi", "tictoc", "beepr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "))

  tictoc::tic(paste0("RSSSF-only ", audit_name, " audit"))
  on.exit({
    tictoc::toc()
    if (isTRUE(completion_beep)) try(suppressWarnings(beepr::beep()), silent = TRUE)
  }, add = TRUE)

  root <- normalizePath(
    Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
    winslash = "/",
    mustWork = TRUE
  )
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  cache_roots <- c(
    file.path(base, "Source/rsssf/all/pages"),
    file.path(base, "Source/rsssf/all/review/pages"),
    file.path(base, "Source/rsssf/uefa_raw/pages")
  )
  cache_roots <- cache_roots[dir.exists(cache_roots)]
  out <- file.path(base, "Manual_Sources", output_folder)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  if (!length(cache_roots)) stop("No RSSSF cache directories were found.")

  previous <- Sys.getenv("FOUR_LEAGUES_FUNCTIONS_ONLY", unset = NA_character_)
  on.exit({
    if (is.na(previous)) Sys.unsetenv("FOUR_LEAGUES_FUNCTIONS_ONLY") else
      Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY = previous)
  }, add = TRUE)
  Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY = "1")
  engine <- new.env(parent = globalenv())
  source(
    file.path(root, "scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R"),
    local = engine,
    encoding = "UTF-8"
  )

  configs <- data.table::data.table(
    Country = c(
      "American Samoa", "Cook Islands", "Fiji", "New Caledonia",
      "New Zealand", "Papua New Guinea", "Samoa", "Solomon Islands",
      "Tahiti", "Tonga", "Vanuatu", "Oceania"
    ),
    Directory = c(
      "tablesa", "tablesc", "tablesf", "tablesn", "tablesn", "tablesp",
      "tabless", "tabless", "tablest", "tablest", "tablesv", "tableso"
    ),
    FilePattern = c(
      "^amsamoa(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^cook(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^fiji(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^newcal(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^nz(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^png(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^samoa(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^solom(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^tahiti(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^tonga(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^vanuatu(?:[0-9]{2}|[0-9]{4})[.]html$",
      "^oceacup(?:[0-9]{2}|[0-9]{4})[.]html$"
    ),
    Competition = c(
      "FFAS National League", "Cook Islands Round Cup", "Fiji Premier League",
      "New Caledonia Super Ligue", "New Zealand National League",
      "Papua New Guinea National Soccer League", "Samoa Premier League",
      "Solomon Islands S-League", "Tahiti Ligue 1", "Tonga Major League",
      "Vanuatu National League", "OFC Champions League"
    ),
    CompetitionType = c(rep("league", 11L), "continental")
  )

  if (!is.null(config_override)) configs <- data.table::as.data.table(config_override)
  if (!"Confederation" %in% names(configs)) configs[, Confederation := audit_name]

  requested <- trimws(strsplit(Sys.getenv(
    paste0(env_prefix, "_COUNTRIES"), paste(configs$Country, collapse = ",")
  ), ",", fixed = TRUE)[[1L]])
  requested <- requested[nzchar(requested)]
  unknown <- setdiff(requested, configs$Country)
  if (length(unknown)) stop("Unknown OFC audit countries: ", paste(unknown, collapse = ", "))
  configs <- configs[Country %in% requested]

  min_year <- as.integer(Sys.getenv(paste0(env_prefix, "_MIN_YEAR"), "1990"))
  max_year <- as.integer(Sys.getenv(paste0(env_prefix, "_MAX_YEAR"), "2025"))
  if (is.na(min_year) || is.na(max_year) || min_year > max_year)
    stop("Invalid OFC_AUDIT_MIN_YEAR/OFC_AUDIT_MAX_YEAR")

  filename_year <- function(path) {
    digits <- sub("^.*?([0-9]{2}|[0-9]{4})[a-z]?[.]html$", "\\1", basename(path), perl = TRUE)
    year <- suppressWarnings(as.integer(digits))
    ifelse(nchar(digits) == 2L, ifelse(year <= 29L, 2000L + year, 1900L + year), year)
  }

  page_period <- function(doc, fallback_year) {
    title_node <- xml2::xml_find_first(doc, "//title | //h2")
    title <- if (inherits(title_node, "xml_missing")) "" else
      engine$clean_text(xml2::xml_text(title_node))
    match <- regexec("((?:19|20)[0-9]{2})(?:[/–-]([0-9]{2,4}))?", title, perl = TRUE)
    parts <- regmatches(title, match)[[1L]]
    if (!length(parts)) {
      return(list(StartYear = fallback_year, EndYear = fallback_year,
                  Season = as.character(fallback_year), PageTitle = title))
    }
    start <- as.integer(parts[2L])
    if (length(parts) >= 3L && nzchar(parts[3L])) {
      finish <- as.integer(parts[3L])
      if (finish < 100L) finish <- (start %/% 100L) * 100L + finish
      season <- sprintf("%d/%02d", start, finish %% 100L)
    } else {
      finish <- start
      season <- as.character(start)
    }
    list(StartYear = start, EndYear = finish, Season = season, PageTitle = title)
  }

  top_flight_document <- function(doc) {
    clipped <- xml2::read_html(as.character(doc))
    # Modern South American pages commonly mark levels with named H3 anchors.
    # Keep everything through the first level (including a preceding top-flight
    # league cup) and cut the document at the explicitly named second level.
    first_non_league <- xml2::xml_find_first(
      clipped,
      "(//h2[a[@name='second' or @name='copa' or @name='nationalcup' or @name='supercopa' or @name='leaguesuper' or @name='lpfsuper']] | //h3[a[@name='second' or @name='copa' or @name='nationalcup' or @name='supercopa' or @name='leaguesuper' or @name='lpfsuper']] | //h4[a[@name='second' or @name='copa' or @name='nationalcup' or @name='supercopa' or @name='leaguesuper' or @name='lpfsuper']])[1]"
    )
    # Prefer an earlier competition heading over a later named lower division.
    # Otherwise pages such as Kazakhstan retain the national cup before the
    # "second" anchor and misclassify its fixtures as top-flight league games.
    first_h4 <- xml2::xml_find_first(clipped, "//h4[following::pre]")
    if (!inherits(first_h4, "xml_missing") &&
        !length(xml2::xml_find_all(first_h4, "a[@name='leaguecup']"))) {
      next_h4 <- xml2::xml_find_first(first_h4, "following::h4[1]")
      if (!inherits(next_h4, "xml_missing") &&
          !inherits(first_non_league, "xml_missing") &&
          length(xml2::xml_find_all(next_h4, "preceding::*")) <
            length(xml2::xml_find_all(first_non_league, "preceding::*")))
        first_non_league <- next_h4
    }
    if (!inherits(first_non_league, "xml_missing")) {
      xml2::xml_remove(xml2::xml_find_all(first_non_league, ". | following::*"))
      # Some associations use a league cup as an official phase of the
      # top-flight season before the ordinary league (modern Argentina is the
      # main example).  Its explicit `leaguecup` anchor tells us that this is
      # part of the retained first-level block.  Rename only the parser-facing
      # copy so the generic word "cup" does not switch fixture extraction off.
      top_flight_league_cup <- xml2::xml_find_all(
        clipped,
        "//h2[a[@name='leaguecup']] | //h3[a[@name='leaguecup']] | //h4[a[@name='leaguecup']]"
      )
      if (length(top_flight_league_cup))
        xml2::xml_text(top_flight_league_cup) <- "Top-flight opening competition"
      return(list(doc = clipped, method = "named_second_level_boundary", heading = "first level"))
    }
    first_competition <- xml2::xml_find_first(clipped, "//h4[following::pre]")
    if (inherits(first_competition, "xml_missing"))
      return(list(doc = clipped, method = "legacy_full_page_no_h4", heading = ""))
    heading <- engine$clean_text(xml2::xml_text(first_competition))
    next_competition <- xml2::xml_find_first(first_competition, "following::h4[1]")
    if (!inherits(next_competition, "xml_missing"))
      xml2::xml_remove(xml2::xml_find_all(next_competition, ". | following::*"))
    list(doc = clipped, method = "first_h4_competition_block", heading = heading)
  }

  resolve_round_years <- function(parsed, start_year, end_year) {
    if (start_year == end_year || !nrow(parsed)) return(parsed)
    round_number <- function(x) suppressWarnings(as.integer(sub(
      "(?i)^.*?round +([0-9]+).*$", "\\1", x, perl = TRUE
    )))
    rounds <- round_number(parsed$RSSSFStage)
    targets <- which(is.na(parsed$Date) & !parsed$Annotated & !is.na(rounds) &
                       nzchar(parsed$DateHeading))
    for (j in targets) {
      same_block <- which(parsed$RSSSFSection == parsed$RSSSFSection[j] &
                            parsed$RSSSFPhase == parsed$RSSSFPhase[j] & !is.na(rounds))
      max_round <- max(rounds[same_block], na.rm = TRUE)
      if (!is.finite(max_round) || max_round < 2L) next
      inferred_year <- if (rounds[j] <= ceiling(max_round / 2)) start_year else end_year
      recovered <- engine$rsssf_date(
        parsed$DateHeading[j], start_year, end_year, year_hint = inferred_year
      )
      if (!is.na(recovered)) parsed[j, `:=`(
        Date = recovered,
        DateBasis = paste0("numbered_round_half_year:", inferred_year)
      )]
    }
    parsed
  }

  manifests <- lapply(seq_len(nrow(configs)), function(i) {
    cfg <- configs[i]
    paths <- unlist(lapply(cache_roots, function(cache_root) {
      folder <- file.path(cache_root, cfg$Directory)
      if (dir.exists(folder)) list.files(
        folder, pattern = cfg$FilePattern, full.names = TRUE, ignore.case = TRUE
      ) else character()
    }), use.names = FALSE)
    # Cache roots are ordered by preference. Keep one copy of each RSSSF page.
    paths <- paths[!duplicated(tolower(basename(paths)))]
    if (!length(paths)) return(NULL)
    data.table::data.table(
      Country = cfg$Country,
      Confederation = cfg$Confederation,
      Competition = cfg$Competition,
      CompetitionType = cfg$CompetitionType,
      SourceFile = normalizePath(paths, winslash = "/", mustWork = TRUE),
      FilenameYear = filename_year(paths)
    )
  })
  manifest <- data.table::rbindlist(manifests, fill = TRUE)
  manifest <- manifest[FilenameYear >= min_year & FilenameYear <= max_year]
  data.table::setorder(manifest, Country, FilenameYear, SourceFile)
  if (!nrow(manifest)) stop("No cached OFC pages matched the selected filters")
  data.table::fwrite(manifest, file.path(out, "source_manifest.csv"), na = "")

  audits <- vector("list", nrow(manifest))
  games_out <- vector("list", nrow(manifest))
  resume_file <- file.path(out, "season_audit.csv")
  resume_enabled <- identical(Sys.getenv(paste0(env_prefix, "_RESUME"), "1"), "1")
  old_audit <- if (resume_enabled && file.exists(resume_file))
    data.table::fread(resume_file, encoding = "UTF-8") else data.table::data.table()
  if (nrow(old_audit)) message("Resuming from ", nrow(old_audit), " previously audited pages.")
  recheck_structural <- identical(Sys.getenv(paste0(env_prefix, "_RECHECK_STRUCTURAL")), "1")
  if (recheck_structural) {
    n_targets <- old_audit[grepl("structural_review", QualityStatus), .N]
    message("Targeted structural recheck: ", n_targets,
            " cached pages will be reparsed; all other saved pages will be retained.")
  }

  for (i in seq_len(nrow(manifest))) {
    row <- manifest[i]
    old <- if (nrow(old_audit)) old_audit[SourceFile == row$SourceFile] else old_audit
    if (nrow(old) && !(recheck_structural && grepl("structural_review", old$QualityStatus[1L]))) {
      old <- old[1L]
      season_key <- paste(
        gsub("[^A-Za-z0-9]+", "_", row$Country),
        gsub("[^A-Za-z0-9]+", "_", old$Season), sep = "_"
      )
      saved_games <- file.path(out, season_key, "rsssf_games.csv")
      if (file.exists(saved_games)) {
        audits[[i]] <- old
        games_out[[i]] <- data.table::fread(saved_games, encoding = "UTF-8")
        message(sprintf("[%d/%d] retained %s: %s", i, nrow(manifest),
                        row$Country, basename(row$SourceFile)))
        next
      }
    }
    message(sprintf("[%d/%d] %s: %s", i, nrow(manifest), row$Country, basename(row$SourceFile)))
    began <- proc.time()[["elapsed"]]
    result <- tryCatch({
      raw_doc <- xml2::read_html(row$SourceFile)
      period <- page_period(raw_doc, row$FilenameYear)
      isolated <- if (row$CompetitionType == "continental")
        list(doc = raw_doc, method = "full_continental_page", heading = row$Competition) else
        top_flight_document(raw_doc)
      parsed <- engine$rsssf_games(isolated$doc, period$StartYear, period$EndYear)
      if (!nrow(parsed)) stop("No result rows extracted")
      parsed <- data.table::copy(resolve_round_years(
        parsed, period$StartYear, period$EndYear
      ))
      source_assessment <- attr(parsed, "source_assessment")
      parsed[, `:=`(
        Country = row$Country,
        Confederation = row$Confederation,
        Season = period$Season,
        StartYear = period$StartYear,
        Competition = row$Competition,
        CompetitionType = row$CompetitionType,
        Tier = if (row$CompetitionType == "league") 1L else NA_integer_,
        League = row$Competition,
        Home = RHome,
        Away = RAway,
        Source = "rsssf",
        SourcePage = basename(row$SourceFile),
        SourceFile = row$SourceFile,
        Stage = RSSSFStage,
        DateApprox = FALSE,
        DateStatus = data.table::fifelse(
          !is.na(Date), "matched", "rsssf_date_missing_or_unresolved"
        ),
        MatchMethod = data.table::fifelse(
          !is.na(Date), "rsssf_result_and_date", "rsssf_result_without_exact_date"
        )
      )]
      parsed[, Result := vapply(strsplit(Score, "-", fixed = TRUE), function(s) {
        score <- suppressWarnings(as.integer(s))
        if (length(score) != 2L || anyNA(score)) return(NA_character_)
        if (score[1L] > score[2L]) "1-0" else if (score[1L] < score[2L]) "0-1" else "0.5-0.5"
      }, character(1))]
      parsed[, SourceFixtureId := sprintf("%s:%s", basename(row$SourceFile), SourceLine)]

      played <- sum(!parsed$Annotated)
      dated <- sum(!is.na(parsed$Date) & !parsed$Annotated)
      dated_share <- if (played) round(100 * dated / played, 2) else NA_real_
      quality <- if (!played) "no_played_results" else if (dated_share >= 95)
        "at_least_95_dated" else "below_95_dated"
      if (isolated$method == "legacy_full_page_no_h4")
        quality <- paste0(quality, "_structural_review")

      season_key <- paste(
        gsub("[^A-Za-z0-9]+", "_", row$Country),
        gsub("[^A-Za-z0-9]+", "_", period$Season),
        sep = "_"
      )
      season_dir <- file.path(out, season_key)
      dir.create(season_dir, recursive = TRUE, showWarnings = FALSE)
      data.table::fwrite(parsed, file.path(season_dir, "rsssf_games.csv"), na = "")
      data.table::fwrite(parsed[is.na(Date)], file.path(season_dir, "unresolved.csv"), na = "")
      games_out[[i]] <- parsed

      data.table::data.table(
        Country = row$Country,
        Confederation = row$Confederation,
        Competition = row$Competition,
        CompetitionType = row$CompetitionType,
        Season = period$Season,
        StartYear = period$StartYear,
        PageTitle = period$PageTitle,
        RSSSFResults = nrow(parsed),
        RSSSFPlayedResults = played,
        RSSSFDatedResults = dated,
        DatedShareOfRSSSFPercent = dated_share,
        QualityStatus = quality,
        IsolationMethod = isolated$method,
        TopFlightHeading = isolated$heading,
        SourceAssessment = source_assessment,
        SourceFile = row$SourceFile,
        Error = ""
      )
    }, error = function(e) {
      data.table::data.table(
        Country = row$Country,
        Confederation = row$Confederation,
        Competition = row$Competition,
        CompetitionType = row$CompetitionType,
        Season = as.character(row$FilenameYear),
        StartYear = row$FilenameYear,
        PageTitle = "",
        RSSSFResults = 0L,
        RSSSFPlayedResults = 0L,
        RSSSFDatedResults = 0L,
        DatedShareOfRSSSFPercent = NA_real_,
        QualityStatus = "season_error",
        IsolationMethod = "",
        TopFlightHeading = "",
        SourceAssessment = "",
        SourceFile = row$SourceFile,
        Error = conditionMessage(e)
      )
    })
    result[, ElapsedSeconds := round(proc.time()[["elapsed"]] - began, 3)]
    audits[[i]] <- result
    data.table::fwrite(
      data.table::rbindlist(audits[seq_len(i)], fill = TRUE),
      file.path(out, "season_audit.csv"),
      na = ""
    )
    print(result[, .(
      Country, Season, RSSSFPlayedResults, RSSSFDatedResults,
      DatedShareOfRSSSFPercent, QualityStatus, Error
    )])
  }

  audit <- data.table::rbindlist(audits, fill = TRUE)
  games <- data.table::rbindlist(games_out, fill = TRUE)
  data.table::setorder(audit, Country, StartYear)
  data.table::fwrite(audit, file.path(out, "season_audit.csv"), na = "")
  data.table::fwrite(games, file.path(out, "all_rsssf_games.csv"), na = "")
  data.table::fwrite(games[!is.na(Date)], file.path(out, "all_dated_rsssf_games.csv"), na = "")
  data.table::fwrite(games[is.na(Date)], file.path(out, "unresolved.csv"), na = "")

  summary <- audit[, .(
    Seasons = .N,
    SeasonErrors = sum(nzchar(Error)),
    SeasonsAtLeast95 = sum(QualityStatus == "at_least_95_dated"),
    StructuralReviewSeasons = sum(grepl("structural_review", QualityStatus)),
    RSSSFPlayedResults = sum(RSSSFPlayedResults),
    RSSSFDatedResults = sum(RSSSFDatedResults),
    DatedShareOfRSSSFPercent = if (sum(RSSSFPlayedResults) > 0L)
      round(100 * sum(RSSSFDatedResults) / sum(RSSSFPlayedResults), 2) else NA_real_
  ), by = .(Confederation, Country, Competition)][order(Confederation, Country)]
  data.table::fwrite(summary, file.path(out, "coverage_summary.csv"), na = "")
  print(summary)
  message("Local page reads: ", nrow(manifest), "; download attempts: 0.")
  message("Review ", file.path(out, "season_audit.csv"), ". Production master unchanged.")
  invisible(list(games = games, audit = audit, summary = summary))
}

if (!identical(Sys.getenv("RSSSF_OFC_FUNCTIONS_ONLY"), "1")) run_rsssf_ofc_audit()
