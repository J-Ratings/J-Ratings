# Match-level reconciliation for the regional OpenFootball and RSSSF feeds.
merge_regional_sources <- function(master, openfootball_root, repo_dir) {
  base <- file.path(repo_dir, "EuropeanFootball", "pipeline_data")
  cutoff <- "2024-12-31"
  regional <- file.path(openfootball_root, "regional")
  files <- list.files(regional, pattern = "[.]txt$", recursive = TRUE)
  countries <- unique(master$Country)
  country_keys <- function(x) {
    gsub("[^a-z0-9]+", "-", tolower(stringi::stri_trans_general(x, "Latin-ASCII")))
  }
  country_map <- setNames(countries, country_keys(countries))
  competitions <- unique(master[c("Country", "CompetitionType", "Tier", "Competition")])
  candidates <- list()
  for (file in files) {
    season <- sub("_.*$", "", basename(file))
    if (!grepl("^(19|20)[0-9]{2}(-[0-9]{2})?$", season) || season < "2024") next
    code <- sub("^[^_]+_([^.]*)[.]txt$", "\\1", basename(file))
    folder <- basename(dirname(file))
    country <- unname(country_map[folder])
    tier <- suppressWarnings(as.integer(sub("^[a-z]+([12]).*$", "\\1", code)))
    type <- "league"
    if (code == "mls") {
      country <- "United States"
      tier <- 1L
    } else if (code %in% c("copal", "copas", "cafcl", "afl", "concacafcl")) {
      country <- switch(code, cafcl = "Africa", afl = "Africa",
                        concacafcl = "North America", "South America")
      type <- "continental"
    } else if (code %in% c("brcup", "ilcup")) {
      type <- "cup"
    }
    if (is.na(country) || (type == "league" && is.na(tier))) next
    existing <- competitions[competitions$Country == country &
      competitions$CompetitionType == type &
      !is.na(competitions$Tier) & competitions$Tier == tier, ]
    preferred <- paste0(gsub("-", "_", folder), "_top_flight")
    if (code == "mls") preferred <- "united_states_top_flight"
    competition <- if (preferred %in% existing$Competition) preferred else existing$Competition[1L]
    if (type == "continental") {
      competition <- switch(country, Africa = "caf_club_competitions",
        "North America" = "concacaf_club_competitions", "conmebol_club_competitions")
    } else if (type == "cup") {
      competition <- if (code == "brcup") "copa_do_brasil" else "israel_state_cup"
    } else if (!length(competition) || is.na(competition)) {
      competition <- paste0(gsub("-", "_", folder), if (tier == 1L) "_top_flight" else "_second_tier")
    }
    path <- file.path(regional, file)
    label <- sub("^=+\\s*", "", readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8"))
    label <- trimws(sub("\\s+[0-9]{4}([/-][0-9]{2,4})?\\s*$", "", label))
    games <- tryCatch(
      parse_comp_file(path, country, competition, type, label, tier,
                      season_str = gsub("-", "/", season, fixed = TRUE)),
      error = function(e) {
        warning("Regional OpenFootball file skipped: ", file, " | ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(games) || !nrow(games)) next
    games$SourceFile <- file
    candidates[[length(candidates) + 1L]] <- games
  }

  rsssf_file <- Sys.getenv("CLUB_RSSSF_MERGE_FILE")
  if (nzchar(rsssf_file)) {
    rsssf <- data.table::fread(rsssf_file, encoding = "UTF-8")
    needed <- c("Country", "Season", "Competition", "CompetitionType", "Tier",
                "League", "Date", "Home", "Away", "Score", "Result", "Source")
    if (!all(needed %in% names(rsssf))) stop("Invalid RSSSF merge input: ", rsssf_file)
    if ("Annotated" %in% names(rsssf)) rsssf <- rsssf[!is.na(Annotated) & !Annotated]
    rsssf <- rsssf[nzchar(Score) & !is.na(Score)]
    # Reuse the existing competition identifier for the same domestic tier.
    for (country in unique(rsssf$Country[rsssf$CompetitionType == "league" & rsssf$Tier == 1L])) {
      known <- competitions[competitions$Country == country &
        competitions$CompetitionType == "league" & competitions$Tier %in% 1L, ]
      if (!nrow(known)) next
      preferred <- paste0(gsub("-", "_", country_keys(country)), "_top_flight")
      id <- if (preferred %in% known$Competition) preferred else known$Competition[1L]
      rsssf[Country == country & CompetitionType == "league" & Tier == 1L,
            Competition := id]
    }
    candidates[[length(candidates) + 1L]] <- rsssf
  }
  if (!length(candidates)) return(master)
  incoming <- as.data.frame(data.table::rbindlist(candidates, fill = TRUE))
  incoming$Date <- as.character(incoming$Date)
  valid <- grepl("^\\d{4}-\\d{2}-\\d{2}$", incoming$Date) & incoming$Date > cutoff &
    !incoming$Home %in% c("", "N.N.", "TBD") & !incoming$Away %in% c("", "N.N.", "TBD") &
    !is.na(incoming$Home) & !is.na(incoming$Away)
  scored <- function(x) !is.na(x) & nzchar(trimws(x))
  incoming <- incoming[valid & (scored(incoming$Score) | incoming$Date >= as.character(Sys.Date())), ]
  if (!nrow(incoming)) return(master)
  for (col in c("SourceFile", "SourcePage")) {
    if (!col %in% names(master)) master[[col]] <- NA_character_
    if (!col %in% names(incoming)) incoming[[col]] <- NA_character_
  }

  aliases <- read.csv(file.path(base, "Reference", "team_aliases.csv"), encoding = "UTF-8")
  alias_keys <- paste(aliases$Country, aliases$SourceName, sep = "\r")
  map <- setNames(aliases$CanonicalName, alias_keys)
  source(file.path(repo_dir, "scripts/EuropeanFootball/club_identity_resolution.R"), local = TRUE)
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(stringi::stri_trans_general(x, "Latin-ASCII")))
  keys <- function(d) {
    home <- norm(resolve_alias_chain(d$Home, d$Country, map, character()))
    away <- norm(resolve_alias_chain(d$Away, d$Country, map, character()))
    # Domestic divisions can have source-specific competition and season names.
    competition <- ifelse(d$CompetitionType == "league" & !is.na(d$Tier),
                          paste0("league-", d$Tier), d$Competition)
    pair <- paste(d$Country, competition, home, away, sep = "\r")
    list(exact = paste(pair, d$Date, sep = "\r"), pair = pair,
         home = paste(d$Country, home, d$Date, sep = "\r"),
         away = paste(d$Country, away, d$Date, sep = "\r"), self = home == away)
  }
  recent <- which(!is.na(master$Date) & master$Date > cutoff)
  old <- keys(master[recent, , drop = FALSE])
  new <- keys(incoming)
  combined <- split(seq_len(nrow(incoming)), new$exact)
  previous <- split(recent, old$exact)
  pair_dates <- split(c(master$Date[recent], incoming$Date), c(old$pair, new$pair))
  occupied <- unique(c(old$home, old$away))
  candidate_clubs <- table(c(new$home[!duplicated(new$exact)], new$away[!duplicated(new$exact)]))
  conflict_rows <- integer()
  conflict_reasons <- character()
  add <- integer()
  score_key <- function(x) gsub("\\s+", "", sub("^(\\d+\\s*-\\s*\\d+).*$", "\\1", trimws(x)))
  for (group in combined) {
    i <- group[1L]
    old_rows <- previous[[new$exact[i]]]
    scores <- c(master$Score[old_rows], incoming$Score[group])
    if (length(unique(score_key(scores[scored(scores)]))) > 1L) {
      conflict_rows <- c(conflict_rows, group)
      conflict_reasons <- c(conflict_reasons,
        rep(paste("Different scores:", paste(unique(scores[scored(scores)]), collapse = " / ")), length(group)))
      next
    }
    best <- group[scored(incoming$Score[group])][1L]
    if (length(old_rows)) {
      if (!is.na(best) && !any(scored(master$Score[old_rows]))) {
        master[old_rows[1L], c("Result", "Score", "Source")] <-
          incoming[best, c("Result", "Score", "Source")]
        for (col in intersect(c("SourceFile", "SourcePage"), names(master))) {
          master[old_rows[1L], col] <- incoming[best, col]
        }
      }
      next
    }
    other_dates <- unique(pair_dates[[new$pair[i]]])
    distance <- abs(as.integer(as.Date(other_dates) - as.Date(incoming$Date[i])))
    if (new$self[i] || any(distance > 0 & distance <= 7) ||
        any(c(new$home[i], new$away[i]) %in% occupied) ||
        any(candidate_clubs[c(new$home[i], new$away[i])] > 1L)) {
      conflict_rows <- c(conflict_rows, i)
      conflict_reasons <- c(conflict_reasons, "Same club/date, nearby repeat, or self fixture")
      next
    }
    add <- c(add, if (is.na(best)) i else best)
  }
  for (col in setdiff(names(master), names(incoming))) incoming[[col]] <- NA
  report <- file.path(base, "Matches_Clean_Combined", "regional_source_conflicts.csv")
  if (length(conflict_rows)) {
    review <- incoming[conflict_rows, , drop = FALSE]
    review$ConflictReason <- conflict_reasons
    data.table::fwrite(review, report, na = "")
    warning(length(conflict_rows), " regional rows need review: ", report)
  } else if (file.exists(report)) {
    unlink(report)
  }
  cat("Additional regional fixtures and results:", length(add), "\n")
  rbind(master, incoming[add, names(master), drop = FALSE])
}
