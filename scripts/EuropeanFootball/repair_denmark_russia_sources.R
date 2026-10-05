# Run in R: source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/repair_denmark_russia_sources.R")
# Optional preview: Sys.setenv(CLUB_SOURCE_REPAIR_DRY_RUN = "1")
# Requires a complete agreeing fixture schedule and matching available scores.
# Fixtures without a replacement score remain intact and reported.
repair_denmark_russia_sources <- function() {
  required <- c("data.table", "xml2", "stringi")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
  if (length(missing)) stop("Install required packages: ", paste(missing, collapse=", "))
  root <- normalizePath(Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"), winslash="/", mustWork=TRUE)
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  master_path <- file.path(base, "Matches_Clean_Combined/european_football_all_matches.csv")
  report <- file.path(base, "Audit/denmark_russia_source_repair")
  cache <- file.path(report, "cache")
  dir.create(cache, recursive=TRUE, showWarnings=FALSE)
  dry <- Sys.getenv("CLUB_SOURCE_REPAIR_DRY_RUN", "0") == "1"
  master <- data.table::fread(master_path, encoding="UTF-8")
  aliases <- data.table::fread(file.path(base, "Reference/team_aliases.csv"), encoding="UTF-8")
  normalize <- function(x) gsub("[^a-z0-9]", "", tolower(stringi::stri_trans_general(x, "Latin-ASCII")))
  canonical <- function(x, country) {
    a <- aliases[aliases$Country == country]
    lookup <- setNames(a$CanonicalName, a$SourceName)
    vapply(x, function(name) {
      seen <- character()
      while (name %in% names(lookup) && !name %in% seen) {
        seen <- c(seen, name)
        next_name <- unname(lookup[[name]])
        if (identical(name, next_name)) break
        name <- next_name
      }
      normalize(name)
    }, character(1), USE.NAMES=FALSE)
  }
  # Load only pure parser helpers; do not execute the normal updater.
  parser <- new.env(parent=environment())
  wanted <- c("trim", "month_num", "infer_year_from_season", "result_code", "parse_comp_file")
  for (expr in parse(file.path(root, "scripts/EuropeanFootball/01_parse_openfootball.R"))) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]])[1] %in% wanted) eval(expr, parser)
  }
  fetch <- function(url, name) {
    path <- file.path(cache, name)
    cached_russia <- file.path(base, "Source/rsssf/all/pages/tablesr/rus2026.html")
    if (!file.exists(path) && name == "rus2026.html" && file.exists(cached_russia))
      file.copy(cached_russia, path)
    if (!file.exists(path)) {
      Sys.sleep(1.5)
      old <- options(timeout=max(120, getOption("timeout")))
      on.exit(options(old), add=TRUE)
      tmp <- paste0(path, ".download")
      download.file(url, tmp, mode="wb", quiet=TRUE)
      if (!file.rename(tmp, path)) stop("Cannot save source cache: ", path)
    }
    path
  }
  russia_parser <- function(path) {
    # Cached RSSSF HTML sometimes contains an incorrect encoding declaration.
    bom <- readBin(path, "raw", n=2L)
    encoding <- if (identical(bom, as.raw(c(255, 254)))) "UTF-16LE" else "UTF-8"
    doc <- xml2::read_html(path, encoding=encoding)
    block <- xml2::xml_find_first(doc, "//h4[a[@name='1l']]/following-sibling::pre[1]")
    if (inherits(block, "xml_missing")) stop("RSSSF Premier League block missing")
    lines <- strsplit(xml2::xml_text(block), "\n", fixed=TRUE)[[1]]
    date <- as.Date(NA); rounds <- integer(); round <- NA_integer_; rows <- list()
    for (line in trimws(lines)) {
      if (!is.na(round) && grepl("^Final Table:", line)) break
      if (grepl("^Round [0-9]+", line)) {
        round <- as.integer(sub("^Round ([0-9]+).*", "\\1", line))
        rounds <- c(rounds, round)
        line <- sub("^Round [0-9]+ \\[([^.]*)\\..*", "[\\1]", line)
      }
      m <- regmatches(line, regexec("^\\[([A-Z][a-z]{2}) ([0-9]{1,2})(?:,? (20[0-9]{2}))?\\]$", line, perl=TRUE))[[1]]
      if (length(m)) {
        month <- match(m[2], month.abb)
        year <- if (nzchar(m[4])) as.integer(m[4]) else if (month >= 7L) 2025L else 2026L
        date <- as.Date(sprintf("%04d-%02d-%02d", year, month, as.integer(m[3])))
        next
      }
      m <- regmatches(line, regexec("^(.+?)\\s{2,}([0-9]+)-([0-9]+)\\s{2,}(.+?)\\s*$", line, perl=TRUE))[[1]]
      if (!length(m) || grepl("^(\\[|[0-9]+\\.)", line) || is.na(round) || round > 30L) next
      if (is.na(date)) stop("RSSSF match has no date: ", line)
      rows[[length(rows)+1L]] <- data.frame(Home=trimws(m[2]), Away=trimws(m[5]),
        Date=as.character(date), Score=paste0(m[3], "-", m[4]), Round=round)
    }
    if (!length(rows)) stop("No RSSSF league matches parsed")
    x <- data.table::rbindlist(rows)
    if (nrow(x) != 240L || !setequal(unique(x$Round), 1:30) || any(table(x$Round) != 8L)) {
      stop("RSSSF season must contain exactly 30 rounds of 8 matches; parsed ", nrow(x))
    }
    # Source abbreviations are mapped explicitly, never by fuzzy name matching.
    short <- c("Soci"="Sochi", "Krylja S."="Krylia Sovetov Samara",
               "Baltika"="Baltika", "Orenburg"="Orenburg",
               "Lokomotiv"="Lokomotiv Moscow", "Krasnodar"="Krasnodar")
    for (col in c("Home", "Away")) {
      hit <- x[[col]] %in% names(short)
      data.table::set(x, which(hit), col, unname(short[x[[col]][hit]]))
    }
    x
  }
  jobs <- list(
    list(country="Denmark", season="2023/24", source="openfootball", old="weltfussball_manual", count=193L,
         url="https://raw.githubusercontent.com/openfootball/europe/master/denmark/2023-24_dk1.txt", file="2023-24_dk1.txt"),
    list(country="Denmark", season="2024/25", source="openfootball", old="weltfussball_manual", count=193L,
         url="https://raw.githubusercontent.com/openfootball/europe/master/denmark/2024-25_dk1.txt", file="2024-25_dk1.txt"),
    list(country="Russia", season="2025/26", source="rsssf", old="betexplorer", count=240L,
         url="https://www.rsssf.org/tablesr/rus2026.html", file="rus2026.html")
  )
  summaries <- list(); changes <- list()
  for (job in jobs) {
    cat("Checking ", job$country, " ", job$season, "...\n", sep="")
    outcome <- tryCatch({
      old <- master[master$Country == job$country & master$CompetitionType == "league" &
                    master$Tier == 1 & gsub("-", "/", master$Season, fixed=TRUE) == job$season]
      if (nrow(old) != job$count) stop("Existing season has ", nrow(old), " rows; expected ", job$count)
      if (!all(old$Source %in% c(job$old, job$source))) stop("Season contains an unexpected source; left intact")
      path <- fetch(job$url, job$file)
      fresh <- if (job$country == "Russia") russia_parser(path) else
        data.table::as.data.table(parser$parse_comp_file(path, "Denmark", "danish_superliga", "league", "Danish Superliga", 1L))
      if (nrow(fresh) != job$count || anyNA(fresh$Date))
        stop("Replacement does not contain the complete dated fixture list")
      scored <- grepl("^[0-9]+-[0-9]+$", fresh$Score)
      if (any(!scored & nzchar(fresh$Score))) stop("Unrecognised score format")
      # Independent source must agree on the entire season before any old rows
      # are replaced. Dates/results/club identities are deliberately unchanged.
      key <- function(x) paste(as.character(x$Date), canonical(x$Home, job$country),
                              canonical(x$Away, job$country), sub(" .*", "", x$Score), sep="|")
      fixture_key <- function(x) paste(as.character(x$Date), canonical(x$Home, job$country),
                                       canonical(x$Away, job$country), sep="|")
      if (!setequal(fixture_key(old), fixture_key(fresh))) {
        diff <- data.table::rbindlist(list(
          data.table::data.table(Side="existing_only", Key=setdiff(fixture_key(old), fixture_key(fresh))),
          data.table::data.table(Side="replacement_only", Key=setdiff(fixture_key(fresh), fixture_key(old)))))
        data.table::fwrite(diff, file.path(report, paste0(job$country, "_", gsub("/", "-", job$season), "_differences.csv")))
        stop("Fixture dates or club names disagree; see differences report; season left intact")
      }
      old_key <- key(old); new_key <- key(fresh[scored])
      if (anyDuplicated(old_key) || anyDuplicated(new_key)) stop("Duplicate fixture keys")
      expected <- old_key[fixture_key(old) %in% fixture_key(fresh[scored])]
      if (!setequal(expected, new_key)) {
        diff <- data.table::rbindlist(list(
          data.table::data.table(Side="existing_only", Key=setdiff(old_key, new_key)),
          data.table::data.table(Side="replacement_only", Key=setdiff(new_key, old_key))))
        data.table::fwrite(diff, file.path(report, paste0(job$country, "_", gsub("/", "-", job$season), "_differences.csv")))
        stop("Source disagreements or unresolved club names; see differences report")
      }
      change <- old[old_key %in% new_key, .(Country, Season, Date=as.character(Date), Home, Away, Score,
                         OldSource=Source, NewSource=job$source, SourcePage=job$url, SourceFile=path)]
      changes[[length(changes)+1L]] <- change
      list(status=if (all(scored)) "VALIDATED" else "PARTIAL_VALIDATED",
           detail=paste(sum(scored), "fixtures independently agree;", sum(!scored), "unscored source fixtures retained unchanged"))
    }, error=function(e) list(status="HELD", detail=conditionMessage(e)))
    summaries[[length(summaries)+1L]] <- data.table::data.table(Country=job$country, Season=job$season,
      NewSource=job$source, Status=outcome$status, Detail=outcome$detail)
    cat("  ", outcome$status, ": ", outcome$detail, "\n", sep="")
  }
  summary <- data.table::rbindlist(summaries)
  data.table::fwrite(summary, file.path(report, "season_review.csv"))
  plan <- if (length(changes)) data.table::rbindlist(changes) else data.table::data.table(
    Country=character(), Season=character(), Date=character(), Home=character(), Away=character(),
    Score=character(), OldSource=character(), NewSource=character(), SourcePage=character(), SourceFile=character())
  data.table::fwrite(plan, file.path(report, "validated_replacements.csv"))
  if (dry || !nrow(plan)) {
    cat(if (dry) "Preview complete. " else "No seasons passed validation. ", "Master and ratings unchanged.\n", sep="")
    return(invisible(summary))
  }
  # Update only attribution for independently identical fixtures. Include both
  # exported histories and frozen history so the next Elo run preserves it.
  stamp <- function(x) {
    if (is.null(x) || !all(c("Country", "Date", "Home", "Away", "Score", "Source") %in% names(x))) return(x)
    x <- data.table::copy(data.table::as.data.table(x))
    for (col in intersect(c("Source", "SourcePage", "SourceFile"), names(x)))
      data.table::set(x, j=col, value=as.character(x[[col]]))
    for (country in unique(plan$Country)) {
      p <- plan[plan$Country == country]
      key <- function(z) paste(as.character(z$Date), canonical(z$Home, country), canonical(z$Away, country), sub(" .*", "", z$Score), sep="|")
      ids <- which(x$Country == country)
      match_id <- match(key(x[ids]), key(p))
      keep <- which(!is.na(match_id))
      ids <- ids[keep]; match_id <- match_id[keep]
      eligible <- x$Source[ids] %in% c(p$OldSource, p$NewSource)
      ids <- ids[eligible]; match_id <- match_id[eligible]
      for (col in intersect(c("Source", "SourcePage", "SourceFile"), names(x))) {
        vals <- if (col == "Source") p$NewSource[match_id] else p[[col]][match_id]
        data.table::set(x, ids, col, vals)
      }
    }
    x
  }
  # Stage all writes and make backups before touching any production file.
  stamp_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", Sys.getpid())
  backup <- tempfile(pattern=paste0("backup_", stamp_id, "_"), tmpdir=report)
  if (!dir.create(backup, showWarnings=FALSE)) stop("Cannot create backup directory")
  staged <- list(); targets <- character()
  stage_csv <- function(path, value) {
    temp <- file.path(backup, paste0(length(staged)+1L, "_staged.csv"))
    data.table::fwrite(value, temp, na="")
    staged[[length(staged)+1L]] <<- temp; targets <<- c(targets, path)
  }
  stage_csv(master_path, stamp(master))
  for (name in c("football_elo_game_history.csv", "football_elo_game_history_pass1.csv")) {
    path <- file.path(base, "Elo", name)
    if (file.exists(path)) stage_csv(path, stamp(data.table::fread(path, encoding="UTF-8")))
  }
  checkpoint <- file.path(base, "Elo/checkpoint_2024_12_31.rds")
  if (file.exists(checkpoint)) {
    x <- readRDS(checkpoint)
    for (name in c("historical_input", "pass1_history", "pass2_history")) x[[name]] <- stamp(x[[name]])
    temp <- file.path(backup, "staged_checkpoint.rds"); saveRDS(x, temp)
    staged[[length(staged)+1L]] <- temp; targets <- c(targets, checkpoint)
  }
  originals <- file.path(backup, paste0(seq_along(targets), "_original_", basename(targets)))
  for (i in seq_along(targets)) if (!file.copy(targets[i], originals[i], overwrite=FALSE)) stop("Cannot back up ", targets[i])
  applied <- integer()
  tryCatch({
    for (i in seq_along(targets)) {
      applied <- c(applied, i)
      if (!file.copy(staged[[i]], targets[i], overwrite=TRUE)) stop("Cannot publish ", targets[i])
    }
  }, error=function(e) {
    restored <- vapply(applied, function(i) file.copy(originals[i], targets[i], overwrite=TRUE), logical(1))
    stop(conditionMessage(e), if (all(restored)) "; originals restored" else paste0("; restore backup manually: ", backup))
  })
  remaining <- stamp(master)[Source %in% c("weltfussball_manual", "betexplorer"),
    .(Matches=.N), by=.(Country, Season, Source)]
  data.table::fwrite(remaining, file.path(report, "remaining_old_sources.csv"))
  cat("\nApplied ", nrow(plan), " independently verified source replacements.\n", sep="")
  cat("Dates, scores, club identities and Elo values unchanged; no recalculation needed.\n")
  cat("Unsupported Denmark seasons remain in place and are listed in remaining_old_sources.csv.\n")
  cat("Backups: ", backup, "\nReports: ", report, "\n", sep="")
  invisible(summary)
}

repair_denmark_russia_sources()
