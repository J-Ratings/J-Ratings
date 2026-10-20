# Explicit shootouts count as full wins/losses throughout football Elo.
# Scores remain the actual match score; unresolved draws remain 0.5/0.5.
football_elo_decisions <- function(repo_dir, sport, dates, home, away, score,
                                   home_points, away_points) {
  key_name <- function(x) gsub("[^a-z0-9]", "", tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII")))
  teams <- jsonlite::fromJSON(file.path(repo_dir, sport, "data/teams.json"))
  aliases <- c(setNames(key_name(teams$id), key_name(teams$name)),
               setNames(key_name(teams$id), key_name(teams$id)))
  canonical <- function(x) {
    raw <- key_name(x); mapped <- unname(aliases[raw]); mapped[is.na(mapped)] <- raw[is.na(mapped)]; mapped
  }
  match_key <- function(d,h,a) paste(as.character(d), pmin(h,a), pmax(h,a), sep="|")
  h <- canonical(home); a <- canonical(away)
  keys <- match_key(dates,h,a)
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
  winners <- unname(vapply(keys, function(key) if(is.null(evidence[[key]])) "" else evidence[[key]], character(1)))
  # A retained explicit score suffix is also direct evidence, independent of IDs.
  penalties <- stringr::str_match(as.character(score), "(?i)\\(\\s*(?:p|pens?\\.?|penalties)\\s+(\\d+)\\s*[-–]\\s*(\\d+)\\s*\\)")
  ph <- suppressWarnings(as.integer(penalties[,2])); pa <- suppressWarnings(as.integer(penalties[,3]))
  explicit <- !is.na(ph) & !is.na(pa) & ph != pa
  suffix_winners <- ifelse(ph > pa,h,a)
  conflict <- explicit & winners != "" & winners != suffix_winners
  if (any(conflict,na.rm=TRUE)) stop("Conflicting retained and supplementary shootout results.")
  winners[explicit] <- suffix_winners[explicit]
  drawn <- !is.na(home_points) & !is.na(away_points) & home_points == .5 & away_points == .5
  decided <- drawn & winners != "" & (winners == h | winners == a)
  home_points[decided] <- as.numeric(winners[decided] == h[decided])
  away_points[decided] <- 1-home_points[decided]
  cat(sport, ": explicit shootout wins/losses applied to", sum(decided), "matches (",sum(decided & as.character(dates)<"2025-01-01"),"before 2025).\n")
  list(home=home_points, away=away_points, decided=decided,
       winner=ifelse(winners==h,as.character(home),as.character(away)))
}
