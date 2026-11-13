# Display-only results from the saved 31st Samsung Cup catalogue.
add_samsung_profile_results <- function(root = getwd()) {
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
  key <- function(g) paste(g$date, g$black, g$white, sep="|")
  count <- 0L
  for (id in names(additions)) {
    path <- file.path(data, "games", paste0(id, ".json"))
    old <- if (file.exists(path)) jsonlite::read_json(path) else list()
    rows <- Filter(function(g) !identical(g$source, edition$source), old)
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

`%||%` <- function(x, fallback) if (is.null(x)) fallback else x
