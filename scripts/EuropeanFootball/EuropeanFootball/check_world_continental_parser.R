# Short validation of RSSSF continental club competition layouts.
# Cached pages only; no downloads and no production changes.

run_world_continental_parser_check <- function() {
  required <- c("data.table", "xml2", "stringi", "tictoc", "beepr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install these packages first: ", paste(missing, collapse = ", "))
  tictoc::tic("RSSSF continental parser check")
  on.exit({tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE)}, add = TRUE)

  root <- normalizePath(Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
                        winslash = "/", mustWork = TRUE)
  cache <- file.path(root, "EuropeanFootball/pipeline_data/Source/rsssf/all/pages")
  full_run <- identical(Sys.getenv("WORLD_CONTINENTAL_FULL"), "1")
  out <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit",
                   if (full_run) "continental_rebuild" else "continental_parser_check")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)

  clean <- function(x) trimws(gsub("[[:space:]]+", " ", stringi::stri_trans_nfkc(x)))
  norm <- function(x) {
    x <- tolower(stringi::stri_trans_general(clean(x), "Latin-ASCII"))
    gsub("[^a-z0-9]+", "", x)
  }
  score_date <- function(mon, day, start, end) {
    m <- match(tolower(substr(mon, 1L, 3L)), tolower(month.abb))
    if (is.na(m)) return(as.Date(NA))
    year <- if (start == end || m >= 7L) start else end
    suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", year, m, as.integer(day))))
  }
  numeric_date <- function(day, mon, start, end) {
    year <- if (start == end || as.integer(mon) >= 7L) start else end
    suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", year, as.integer(mon), as.integer(day))))
  }
  capture <- function(pattern, x) {
    m <- regexec(pattern, x, perl = TRUE)
    z <- regmatches(x, m)[[1L]]
    if (!length(z)) character() else z
  }
  period <- function(doc, fallback) {
    title <- clean(xml2::xml_text(xml2::xml_find_first(doc, "//title | //h2")))
    z <- capture("((?:19|20)[0-9]{2})(?:[/–-]([0-9]{2,4}))?", title)
    if (!length(z)) return(list(start = fallback, end = fallback, season = as.character(fallback), title = title))
    start <- as.integer(z[2L]); finish <- start
    if (length(z) >= 3L && nzchar(z[3L])) {
      finish <- as.integer(z[3L]); if (finish < 100L) finish <- (start %/% 100L) * 100L + finish
    }
    list(start = start, end = finish,
         season = if (start == finish) as.character(start) else sprintf("%d/%02d", start, finish %% 100L),
         title = title)
  }

  split_matchup <- function(x, known) {
    # Some old cached pages were decoded with U+FFFD where RSSSF used an en dash.
    # In a fixture matchup that replacement character is the team separator.
    x <- clean(gsub("[\u0096–—−�]", "-", x))
    spaced <- regexpr("[[:space:]]+-[[:space:]]+", x, perl = TRUE)
    if (spaced[1L] > 0L) {
      home <- trimws(substr(x, 1L, spaced[1L] - 1L))
      away <- trimws(substr(x, spaced[1L] + attr(spaced, "match.length"), nchar(x)))
      return(c(home, away))
    }
    loc <- gregexpr("-", x, fixed = TRUE)[[1L]]
    if (identical(loc, -1L)) return(character())
    candidates <- lapply(loc, function(k) c(trimws(substr(x, 1L, k - 1L)), trimws(substr(x, k + 1L, nchar(x)))))
    candidates <- candidates[vapply(candidates, function(z) all(nzchar(z)), logical(1))]
    if (!length(candidates)) return(character())
    nk <- norm(known)
    value <- vapply(candidates, function(z) {
      nz <- norm(z)
      exact <- sum(nz %in% nk)
      partial <- sum(vapply(nz, function(v) any(nchar(v) >= 4L & (startsWith(nk, v) | startsWith(v, nk))), logical(1)))
      100 * exact + 10 * partial - abs(nchar(z[1L]) - nchar(z[2L])) / 100
    }, numeric(1))
    candidates[[which.max(value)]]
  }

  parse_page <- function(path, confed, fallback) {
    doc <- xml2::read_html(path); per <- period(doc, fallback)
    lines <- unlist(lapply(xml2::xml_find_all(doc, "//pre"), function(n) strsplit(xml2::xml_text(n), "\n", fixed = TRUE)[[1L]]))
    lines <- vapply(lines, clean, character(1))
    lines <- lines[nzchar(lines)]

    standings <- capture_standing <- lines[grepl("^[[:space:]]*[0-9]+[.]", lines)]
    standings <- sub("^[[:space:]]*[0-9]+[.]<?[^>]*>?", "", standings, perl = TRUE)
    standings <- sub("[[:space:]]+[0-9]+[[:space:]]+[0-9]+[[:space:]]+[0-9]+[[:space:]]+[0-9]+.*$", "", standings, perl = TRUE)
    standings <- trimws(sub("[[:space:]]*\\([^)]*\\)[[:space:]]*$", "", standings, perl = TRUE))
    country_codes <- c(
      "Afg","AIA","Alg","Ang","Ant","Arg","Aru","Aus","Bah","Bhr","Ban","Bdi","Bel","Ben","Ber","Bhu","Blz","Bol","Bot","Bra","Bru","Bur","BuF","BFa","Buf",
      "Cam","Can","CAf","Chd","Chi","Chn","Cmr","CoB","CoD","CoK","Col","Com","CRi","Cub",
      "CAI","Dji","Dom","DoR","Ecu","Egy","ElS","EqG","Esp","Eth","Gam","Gab","GBi","Gha","Gua","Gui","Guy","Hai","Hkg","Hon",
      # RSSSF has used more than one abbreviation for several associations
      # over the years (for example Ina/Idn for Indonesia).  Retain all
      # observed variants so knockout rows are split at the country codes,
      # rather than treating the second club and aggregate score as text.
      "Idn","Ina","Ind","Irn","Irq","IvC","Jam","Jpn","Jor","Ken","Kgz","Kyr","Kor","Kuw","Lao","Lbr","Lbn","Leb","Lby","Les",
      "Luc","Mac","Mad","Mas","May","Mly","Mdv","Mld","Mex","Mli","Mlw","Mng","Mon","Mor","Moz","Mtn","Mts","Mwi","Mya","Nam","Nep","Nga","Nig","Ngr","Nic","NKo",
      "NZl","Oma","Pak","Pan","Par","Per","Phi","PNG","PRi","Pur","Qat","Rwa","SAf","Saf","San","Sau","Sen","Sey","Sin","SKo","SLe","Som","Sri","SrL","SSu","StK","StL","STP","Sud","Sur","Swa","Syr",
      "Pal","Tan","Tch","Tha","Tjk","Taj","Tkm","Tog","Tri","Tun","UAE","Uga","Uru","USA","Uzb","Ven","Vie","Yem","Zam","Zan","Zim"
    )
    code_re <- paste0("(?:", paste(country_codes, collapse = "|"), ")")
    country_pattern <- paste0("^(.+?) (", code_re, ") (.+?) (", code_re, ") [0-9]+-[0-9]+")
    country_rows <- lines[grepl(country_pattern, lines, perl = TRUE)]
    country_teams <- unlist(lapply(country_rows, function(line) {
      z <- capture(country_pattern, line)
      if (length(z)) trimws(z[c(2L, 4L)]) else character()
    }))
    known <- unique(c(standings[nzchar(standings)], country_teams[nzchar(country_teams)]))

    games <- list(); stage <- ""; last_date <- as.Date(NA); pair_dates <- as.Date(c(NA, NA))
    add <- function(home, away, hs, as, date, raw, basis) {
      games[[length(games) + 1L]] <<- data.table::data.table(
        Confederation = confed, Season = per$season, Date = date,
        Home = clean(home), Away = clean(away), Score = paste0(hs, "-", as),
        Stage = stage, DateBasis = basis, RawLine = raw, SourceFile = normalizePath(path, winslash = "/")
      )
    }
    for (line in lines) {
      # A handful of RSSSF tables attach the association code directly to a
      # closing parenthesis, e.g. "Ceres (Negros)Phi".  Restore the separator
      # before applying the same general country-coded-row parser.
      line <- gsub(paste0("\\)(", code_re, ")\\b"), ") \\1", line, perl = TRUE)
      if (grepl("^(Group|Round|Qualifying|Preliminary|Playoff|First Round|Second Round|Quarterfinal|Semifinal|Final)", line, ignore.case = TRUE)) stage <- line
      # League/group standings contain score-shaped goal columns and must not
      # inherit the preceding fixture date.
      if (grepl("^[0-9]+[.]", line)) next
      dh <- capture("^\\[([A-Za-z]{3,9}) +([0-9]{1,2})(?:[^]]*)\\]$", line)
      if (length(dh)) {
        last_date <- score_date(dh[2], dh[3], per$start, per$end)
        pair_dates <- as.Date(c(NA, NA)); next
      }
      embedded_dh <- capture("^(.+?) \\[([A-Za-z]{3,9}) +([0-9]{1,2})(?:[^]]*)\\]$", line)
      if (length(embedded_dh)) {
        stage <- embedded_dh[2]; last_date <- score_date(embedded_dh[3], embedded_dh[4], per$start, per$end)
        pair_dates <- as.Date(c(NA, NA))
        next
      }
      single_pd <- capture("^\\(?([A-Za-z]{3,9}) +([0-9]{1,2})\\)?$", line)
      if (length(single_pd)) {
        last_date <- score_date(single_pd[2], single_pd[3], per$start, per$end)
        pair_dates <- as.Date(c(NA, NA)); next
      }
      pd <- capture("^\\(?([A-Za-z]{3,9})[.]? +([0-9]{1,2}) *(?:&|and|,) *(?:([A-Za-z]{3,9})[.]? +)?([0-9]{1,2})\\)?$", line)
      if (length(pd)) {
        pair_dates <- as.Date(c(score_date(pd[2], pd[3], per$start, per$end),
                                score_date(if (nzchar(pd[4])) pd[4] else pd[2], pd[5], per$start, per$end)))
        last_date <- pair_dates[1L]
        next
      }
      legs <- capture(paste0("^(.+?) (", code_re, ") (.+?) (", code_re,
                             ") ([0-9]+)-([0-9]+) ([0-9]+)-([0-9]+)(?: .*)?$"), line)
      if (length(legs) && all(!is.na(pair_dates))) {
        add(legs[2], legs[4], legs[6], legs[7], pair_dates[1], line, "paired_leg_dates_first")
        add(legs[4], legs[2], legs[9], legs[8], pair_dates[2], line, "paired_leg_dates_second")
        next
      }
      one_leg <- capture(paste0("^(.+?) (", code_re, ") (.+?) (", code_re,
                                ") ([0-9]+)-([0-9]+)(?: .*)?$"), line)
      one_leg_date <- if (!is.na(pair_dates[1])) pair_dates[1] else last_date
      if (length(one_leg) && !is.na(one_leg_date)) {
        add(one_leg[2], one_leg[4], one_leg[6], one_leg[7], one_leg_date, line,
            if (!is.na(pair_dates[1])) "paired_heading_single_result" else "single_heading_country_coded_result")
        next
      }
      z <- capture("^([A-Za-z]{3,9})[[:space:]]+([0-9]{1,2})[.:][[:space:]]*(.+?)[[:space:]]+([0-9]{1,2})-([0-9]{1,2})(?:[[:space:]]+[0-9]+-[0-9]+p)?(?:[a-z*].*)?$", line)
      if (length(z)) {
        d <- score_date(z[2], z[3], per$start, per$end); teams <- split_matchup(z[4], known)
        if (length(teams)) add(teams[1], teams[2], z[5], z[6], d, line, "month_day_prefix")
        last_date <- d; next
      }
      z <- capture("^([0-9]{1,2})[[:space:]]*-[[:space:]]*([0-9]{1,2})[[:space:]]+(.+?)[[:space:]]+([0-9]{1,2})-([0-9]{1,2})(?:[[:space:]]+[0-9]+-[0-9]+p)?(?:[[:space:]][a-z].*)?$", line)
      if (length(z)) {
        d <- numeric_date(z[2], z[3], per$start, per$end); teams <- split_matchup(z[4], known)
        if (length(teams)) add(teams[1], teams[2], z[5], z[6], d, line, "day_month_prefix")
        last_date <- d; next
      }
      # Traditional RSSSF layout: Home score Away. Read this before the
      # score-at-end form so penalty-shootout scores in notes cannot replace
      # the regulation result.
      score_tokens <- gregexpr("[0-9]{1,2}-[0-9]{1,2}", line, perl = TRUE)[[1L]]
      score_count <- if (identical(score_tokens[1L], -1L)) 0L else length(score_tokens)
      traditional <- capture("^(.+?)[[:space:]]+([0-9]{1,2})-([0-9]{1,2})[[:space:]]+(.+?)(?:[[:space:]]+\\[.*)?$", line)
      if (length(traditional) && !is.na(last_date) &&
          score_count == 1L &&
          !grepl("(?i)\\b(?:awd|awarded|n/p|cancelled|originally|abandoned|presumably|default score|walked off)\\b|^[xyz] ", line, perl = TRUE)) {
        add(traditional[2], traditional[5], traditional[3], traditional[4], last_date, line, "bracket_heading_home_score_away")
        next
      }
      z <- capture("^(.+?)[[:space:]]+([0-9]{1,2})-([0-9]{1,2})(?:[[:space:]][a-z].*)?$", line)
      if (length(z) && !is.na(last_date)) {
        teams <- split_matchup(z[2], known)
        if (length(teams)) add(teams[1], teams[2], z[3], z[4], last_date, line, "continued_previous_numeric_date")
        next
      }
    }
    ans <- data.table::rbindlist(games, fill = TRUE)
    if (!nrow(ans)) ans <- data.table::data.table(Confederation = confed, Season = per$season,
      Date = as.Date(character()), Home = character(), Away = character(), Score = character(),
      Stage = character(), DateBasis = character(), RawLine = character(), SourceFile = character())
    ans
  }

  samples <- data.table::data.table(
    Confederation = c("AFC", "CAF", "CONCACAF", "CONMEBOL", "OFC"),
    RelativeFile = c("tablesa/ascup2022.html", "tablesa/afcup2023.html",
                     "tablesc/cacups2025.html", "sacups/copa2024.html", "tableso/oceacup2023.html"),
    FallbackYear = c(2022L, 2023L, 2025L, 2024L, 2023L)
  )
  samples[, SourceFile := file.path(cache, RelativeFile)]
  if (full_run) {
    definitions <- data.table::data.table(
      Confederation = c("AFC", "CAF", "CONCACAF", "CONMEBOL", "OFC"),
      Folder = c("tablesa", "tablesa", "tablesc", "sacups", "tableso"),
      Pattern = c("^ascup(?:[0-9]{2}|[0-9]{4})[.]html$",
                  "^afcup(?:[0-9]{2}|[0-9]{4})[.]html$",
                  "^cacups(?:[0-9]{2}|[0-9]{4})[.]html$",
                  "^copa(?:[0-9]{2}|[0-9]{4})[.]html$",
                  "^oceacup(?:[0-9]{2}|[0-9]{4})[.]html$")
    )
    samples <- data.table::rbindlist(lapply(seq_len(nrow(definitions)), function(i) {
      d <- definitions[i]
      paths <- list.files(file.path(cache, d$Folder), pattern = d$Pattern,
                          full.names = TRUE, ignore.case = TRUE)
      digits <- sub("^.*?([0-9]{2}|[0-9]{4})[.]html$", "\\1", basename(paths), perl = TRUE)
      years <- as.integer(digits)
      years <- ifelse(nchar(digits) == 2L, ifelse(years <= 29L, 2000L + years, 1900L + years), years)
      data.table::data.table(Confederation = d$Confederation, SourceFile = paths, FallbackYear = years)
    }), fill = TRUE)
    min_year <- as.integer(Sys.getenv("WORLD_CONTINENTAL_MIN_YEAR", "2010"))
    max_year <- as.integer(Sys.getenv("WORLD_CONTINENTAL_MAX_YEAR", "2025"))
    samples <- samples[FallbackYear >= min_year & FallbackYear <= max_year]
    data.table::setorder(samples, Confederation, FallbackYear, SourceFile)
  }
  if (any(!file.exists(samples$SourceFile))) stop("One or more cached sample pages are missing")
  parsed <- lapply(seq_len(nrow(samples)), function(i) {
    message(sprintf("[%d/%d] %s: %s", i, nrow(samples), samples$Confederation[i], basename(samples$SourceFile[i])))
    tryCatch(parse_page(samples$SourceFile[i], samples$Confederation[i], samples$FallbackYear[i]),
             error = function(e) {
               z <- data.table::data.table(Confederation = samples$Confederation[i],
                 Season = as.character(samples$FallbackYear[i]), Date = as.Date(character()),
                 Home = character(), Away = character(), Score = character(), Stage = character(),
                 DateBasis = character(), RawLine = character(), SourceFile = character())
               attr(z, "parse_error") <- conditionMessage(e); z
             })
  })
  games <- data.table::rbindlist(parsed, fill = TRUE)
  audit <- data.table::rbindlist(lapply(seq_along(parsed), function(i) {
    x <- parsed[[i]]
    data.table::data.table(
      Confederation = samples$Confederation[i],
      Season = if (nrow(x)) x$Season[1L] else as.character(samples$FallbackYear[i]),
      SourceFile = normalizePath(samples$SourceFile[i], winslash = "/"),
      ParsedGames = nrow(x), DatedGames = sum(!is.na(x$Date)),
      DatedPercent = if (nrow(x)) round(100 * mean(!is.na(x$Date)), 2) else NA_real_,
      Error = if (is.null(attr(x, "parse_error"))) "" else attr(x, "parse_error")
    )
  }), fill = TRUE)
  data.table::fwrite(games, file.path(out, "parsed_games.csv"), na = "")
  data.table::fwrite(audit, file.path(out, "audit.csv"), na = "")
  print(audit)
  if (full_run) {
    print(audit[, .(Pages = .N, PageErrors = sum(nzchar(Error)),
                    ParsedGames = sum(ParsedGames), DatedGames = sum(DatedGames),
                    DatedPercent = if (sum(ParsedGames)) round(100 * sum(DatedGames) / sum(ParsedGames), 2) else NA_real_),
                by = Confederation])
  }
  message("Review: ", file.path(out, "audit.csv"))
  message("No downloads. Production master unchanged.")
  invisible(list(games = games, audit = audit))
}

run_world_continental_parser_check()
