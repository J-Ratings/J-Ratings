# Explicit shootouts count as full wins/losses throughout football Elo.
# Scores remain the actual match score; unresolved draws remain 0.5/0.5.
football_elo_decisions <- function(repo_dir, sport, dates, home, away, score,
                                   home_points, away_points) {
  started <- proc.time()[['elapsed']]
  drawn_rows <- which(!is.na(home_points) & !is.na(away_points) & home_points == .5 & away_points == .5)
  decided <- rep(FALSE, length(home_points))
  winner <- rep('', length(home_points))
  cat(sport, ': checking shootouts for', length(drawn_rows), 'draws out of', length(home_points), 'matches.\n')
  if (!length(drawn_rows)) return(list(home=home_points, away=away_points, decided=decided, winner=winner))
  key_name <- function(x) gsub("[^a-z0-9]", "", tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII")))
  teams <- jsonlite::fromJSON(file.path(repo_dir, sport, "data/teams.json"))
  aliases <- c(setNames(key_name(teams$id), key_name(teams$name)),
               setNames(key_name(teams$id), key_name(teams$id)))
  canonical <- function(x) {
    # Club names repeat across thousands of fixtures. Normalise each distinct
    # name once and expand with a vectorised match, rather than transliterating
    # and looking up every appearance separately.
    unique_names <- unique(as.character(x))
    raw <- key_name(unique_names)
    mapped <- unname(aliases[match(raw, names(aliases))])
    mapped[is.na(mapped)] <- raw[is.na(mapped)]
    mapped[match(as.character(x), unique_names)]
  }
  match_key <- function(d,h,a) paste(as.character(d), pmin(h,a), pmax(h,a), sep="|")
  h <- canonical(home[drawn_rows]); a <- canonical(away[drawn_rows])
  keys <- match_key(dates[drawn_rows],h,a)
  evidence <- list()
  index_path <- file.path(repo_dir, sport, "data/shootout-decisions.json")
  if (file.exists(index_path)) {
    index <- jsonlite::fromJSON(index_path, simplifyVector=FALSE)
    for (row in index) evidence[[match_key(row$date,row$teams[[1]],row$teams[[2]])]] <- row$winner
  }
  source_path <- file.path(repo_dir, sport, "pipeline_data/source/shootouts.csv")
  if (file.exists(source_path)) {
    source <- as.data.frame(readr::read_csv(source_path, col_types=readr::cols(.default="c"), locale=readr::locale(encoding="UTF-8"), show_col_types=FALSE))
    for (i in seq_len(nrow(source))) {
      sh <- canonical(source$home_team[i]); sa <- canonical(source$away_team[i]); sw <- canonical(source$winner[i])
      if (!sw %in% c(sh,sa)) stop("Invalid shootout winner in supplementary source.")
      key <- match_key(source$date[i],sh,sa)
      if (!is.null(evidence[[key]]) && evidence[[key]] != sw) stop("Conflicting shootout evidence: ",key)
      evidence[[key]] <- sw
    }
  }
  # One vectorised lookup replaces an R callback for every historical match.
  evidence_values <- unlist(evidence, use.names=FALSE)
  winners <- evidence_values[match(keys, names(evidence))]
  if (!length(evidence)) winners <- rep('', length(keys))
  winners[is.na(winners)] <- ''
  # A retained explicit score suffix is also direct evidence, independent of IDs.
  penalties <- stringr::str_match(as.character(score[drawn_rows]), "(?i)\\(\\s*(?:p|pens?\\.?|penalties)\\s+(\\d+)\\s*[-–]\\s*(\\d+)\\s*\\)")
  ph <- suppressWarnings(as.integer(penalties[,2])); pa <- suppressWarnings(as.integer(penalties[,3]))
  explicit <- !is.na(ph) & !is.na(pa) & ph != pa
  suffix_winners <- ifelse(ph > pa,h,a)
  conflict <- explicit & winners != "" & winners != suffix_winners
  if (any(conflict,na.rm=TRUE)) stop("Conflicting retained and supplementary shootout results.")
  winners[explicit] <- suffix_winners[explicit]
  resolved <- winners != '' & (winners == h | winners == a)
  rows <- drawn_rows[resolved]
  decided[rows] <- TRUE
  home_points[rows] <- as.numeric(winners[resolved] == h[resolved])
  away_points[rows] <- 1-home_points[rows]
  winner[rows] <- ifelse(home_points[rows] == 1, as.character(home[rows]), as.character(away[rows]))
  cat(sport, ": explicit shootout wins/losses applied to", sum(decided), "matches (",sum(decided & as.character(dates)<"2025-01-01"),"before 2025). Took", round(proc.time()[['elapsed']] - started, 2), "seconds.\n")
  list(home=home_points, away=away_points, decided=decided,
       winner=winner)
}
