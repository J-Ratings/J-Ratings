# Shared match facts from the saved 31st Samsung Cup catalogue.
read_samsung_profile_results <- function(root = getwd()) {
  data <- file.path(root, "Go/data")
  edition_path <- file.path(data, "tournaments/catalogue/wr-sc-31-.json")
  if (!file.exists(edition_path)) return(invisible(NULL))
  edition <- jsonlite::read_json(edition_path)
  stopifnot(identical(edition$source, "https://gotoeveryone.k2ss.info/news/wr/sc/31/"))
  profiles <- jsonlite::fromJSON(file.path(data, "players.json"))
  by_id <- setNames(profiles$name, profiles$id)
  alias <- c("Kim Myeonghoon" = "kim-myounghoon")
  additions <- list()
  for (g in edition$games) {
    if (!g$round %in% c("1st round", "2nd round") ||
        !g$date %in% c("2026-11-10", "2026-11-11", "2026-11-12", "2026-11-13") ||
        length(g$players) != 2L || !nzchar(g$winner) ||
        !grepl("^[BW]\\+", g$result)) next
    colours <- vapply(g$players, function(p) p$colour %||% "", "")
    if (!setequal(colours, c("B", "W"))) next
    players <- g$players[match(c("B", "W"), colours)]
    labels <- vapply(players, function(p) p$name, "")
    winner <- if (startsWith(g$result, "B+")) 1L else 2L
    if (!identical(g$winner, labels[[winner]])) stop("Conflicting Samsung Cup result: ", paste(labels, collapse=" vs "))
    ids <- vapply(players, function(p) p$id %||% "", "")
    mapped <- match(labels, names(alias))
    ids[!is.na(mapped)] <- unname(alias[mapped[!is.na(mapped)]])
    known <- ids %in% names(by_id)
    labels[known] <- unname(by_id[ids[known]])
    row <- list(date=g$date, era="2026", season=2026L, tournament=edition$name,
                black=labels[[1]], white=labels[[2]],
                result=if (winner == 1L) "1-0" else "0-1",
                source=edition$source)
    for (id in ids[known]) additions[[id]] <- c(additions[[id]], list(row))
  }
  additions
}

add_samsung_profile_results <- function(root = getwd()) {
  data <- file.path(root, "Go/data")
  additions <- read_samsung_profile_results(root)
  if (!length(additions)) return(invisible(NULL))
  source_url <- "https://gotoeveryone.k2ss.info/news/wr/sc/31/"
  key <- function(g) paste(g$date, min(g$black, g$white), max(g$black, g$white), sep="|")
  count <- 0L
  for (id in names(additions)) {
    path <- file.path(data, "games", paste0(id, ".json"))
    old <- if (file.exists(path)) jsonlite::read_json(path) else list()
    rows <- Filter(function(g) !identical(g$source, source_url), old)
    extra <- additions[[id]]
    keys <- vapply(extra, key, "")
    extra <- extra[!duplicated(keys) & !keys %in% vapply(rows, key, "")]
    combined <- c(rows, extra)
    if (!identical(old, combined))
      jsonlite::write_json(combined, path, auto_unbox=TRUE, digits=NA)
    count <- count + length(extra)
  }
  cat("Samsung Cup: ", count, " display-only results across ", length(additions),
      " player profiles (a game can appear on both profiles).\n", sep="")
  invisible(count)
}

merge_samsung_live_games <- function(games, canonical_name_map, root = getwd()) {
  facts <- unlist(read_samsung_profile_results(root), recursive=FALSE, use.names=FALSE)
  if (!length(facts)) return(games)
  lookup <- function(name) {
    ids <- names(canonical_name_map)[which(canonical_name_map == name)]
    if (length(ids) == 1L) ids[[1L]] else NA_character_
  }
  pair_key <- function(date, black, white)
    paste(date, pmin(black, white), pmax(black, white), sep="|")
  existing <- pair_key(games$Date, games$BlackID, games$WhiteID)
  pending <- list()
  seen <- character()
  held <- character()
  for (g in facts) {
    b <- lookup(g$black)
    w <- lookup(g$white)
    if (anyNA(c(b,w)) || identical(b,w)) {
      held <- union(held, paste(g$date, g$black, "vs", g$white))
      next
    }
    key <- pair_key(g$date, b, w)
    if (key %in% seen) next
    seen <- c(seen, key)
    result <- if (g$result == "1-0") "B" else "W"
    winner <- if (result == "B") b else w
    hits <- which(existing == key)
    if (length(hits)) {
      if (length(hits) != 1L || !identical(games$WinnerID[[hits[1L]]], winner))
        warning("Samsung Cup overlaps an ambiguous or conflicting GoRatings result: ", key)
      else games$Event[hits] <- g$tournament
      next
    }
    pending[[length(pending)+1L]] <- data.frame(
      Date=as.Date(g$date), BlackID=b, WhiteID=w, WinnerID=winner,
      Black=g$black, White=g$white, ResultCode=result, Event=g$tournament,
      BlackSourceID=b, WhiteSourceID=w, WinnerSourceID=winner,
      BlackSourceName=g$black, WhiteSourceName=g$white, SameSignatureSequence=1L,
      GameKey=paste0("K2SS|sc31|", key))
  }
  cat("Samsung Cup: ", length(pending), " games added to live Elo; ",
      length(held), " held for unresolved identities.\n", sep="")
  if (length(held)) cat(paste(held, collapse="\n"), "\n")
  dplyr::arrange(dplyr::bind_rows(games, dplyr::bind_rows(pending)),
                 Date, BlackID, WhiteID, ResultCode, SameSignatureSequence, GameKey)
}

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x
