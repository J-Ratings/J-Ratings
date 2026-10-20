# Standalone local preview export. No downloads or Elo changes.
local({
  library(xml2); library(jsonlite); library(data.table)
  year <- as.integer(Sys.getenv("GO_LANKE_YEAR", "2026"))
  stopifnot(year %in% c(2025L, 2026L))
  edition <- year - 2022L
  path <- sprintf("Go/pipeline_data/tournament_format_audit/cache/_news_wr_quzhou_lanke_%d_.html", edition)
  stopifnot(file.exists(path))
  doc <- read_html(path, encoding = "UTF-8")
  table <- xml_find_first(doc, "//table[contains(@class,'tournament-wide')]")
  stopifnot(!inherits(table, "xml_missing"))
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT")))
  clean <- function(x) trimws(sub("\\([^)]*\\)\\s*$", "", sub("^\\([a-z]+\\)", "", x)))
  profiles <- as.data.table(fromJSON("Go/data/players.json"))
  aliases <- fread("Go/pipeline_data/tournament_result_coverage/player_identity_review.csv", encoding = "UTF-8")
  aliases <- aliases[Family == "/news/wr/quzhou-lanke/" & !is.na(Canonical) & nzchar(Canonical)]
  person <- function(name) {
    k <- norm(clean(name)); matches <- unique(aliases[norm(SourceName) == k, Canonical])
    canonical <- if (length(matches) == 1L) matches else clean(name)
    idx <- which(norm(profiles$name) == norm(canonical))
    canonical <- if (length(idx) == 1L) profiles$name[idx] else clean(name)
    list(name = canonical, id = if (length(idx) == 1L) profiles$id[idx] else "",
      flag = if (length(idx) == 1L) profiles$flag[idx] else "",
      rating = if (length(idx) == 1L) profiles$rating[idx] else NULL)
  }
  trs <- xml_find_all(table, "./tr|./tbody/tr|./thead/tr")
  grid <- matrix(NA_integer_, length(trs), 7L); cells <- list()
  for (r in seq_along(trs)) {
    col <- 1L
    for (n in xml_find_all(trs[[r]], "./th|./td")) {
      while (col <= 7L && !is.na(grid[r,col])) col <- col + 1L
      span <- function(a) { z <- as.integer(xml_attr(n,a)); if (is.na(z)) 1L else z }
      rs <- span("rowspan"); cs <- span("colspan")
      stopifnot(col + cs - 1L <= 7L, r + rs - 1L <= nrow(grid))
      its <- trimws(xml_text(xml_find_all(n, ".//span[contains(@class,'tournament-result-item')]")))
      results <- its[grepl("^[BW]\\+", its)]
      winner <- if (length(results) == 1L && match(results,its) > 1L) clean(its[match(results,its)-1L]) else ""
      cells[[length(cells)+1L]] <- list(row=r,col=col,rs=rs,node=n,
        text=trimws(xml_text(n)),winner=winner,result=if(length(results)==1L) results else "",header=xml_name(n)=="th")
      grid[r:(r+rs-1L), col:(col+cs-1L)] <- length(cells); col <- col+cs
    }
  }
  matches <- list()
  for (i in seq_along(cells)) {
    cell <- cells[[i]]
    if (cell$header || cell$col == 1L) next
    prior <- unique(grid[cell$row:(cell$row+cell$rs-1L),cell$col-1L]); prior <- prior[!is.na(prior)]
    participants <- lapply(seq_along(prior), function(j) {
      prev <- cells[[prior[j]]]
      name <- if (prev$col==1L) clean(prev$text) else prev$winner
      if (!nzchar(name)) name <- paste("Winner of", if(prev$col==6L) "semifinal" else "previous match", j)
      person(name)
    })
    header <- cells[[grid[1,cell$col]]]$text
    date <- regmatches(header,regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",header))
    matches[[length(matches)+1L]] <- list(id=paste0("r",cell$col-1L,"-",i),round=cell$col-1L,
      row=cell$row-2L,span=cell$rs,players=participants,
      winner=if(nzchar(cell$winner)) person(cell$winner)$name else "",result=cell$result,
      date=if(length(date)) date else "",bye=length(participants)==1L,
      feedsFrom=as.list(paste0("r",cell$col-2L,"-",prior)))
  }
  final_index <- which(vapply(matches, function(m) m$round == 6L, logical(1)))
  final_cell <- cells[[which(vapply(cells, function(c) !c$header && c$col == 7L, logical(1)))]]
  series <- list()
  for (node in xml_find_all(final_cell$node, ".//span[contains(concat(' ',normalize-space(@class),' '),' match ')]")) {
    items <- trimws(xml_text(xml_find_all(node, "./span[contains(@class,'tournament-result-item')]")))
    dates <- items[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", items)]
    if (!length(dates)) next
    result_index <- which(grepl("^[BW]\\+", items))
    series[[length(series)+1L]] <- list(round=7L, players=matches[[final_index]]$players,
      date=dates[1L],result=if(length(result_index)) items[result_index[1L]] else "",
      winner=if(length(result_index)) person(items[result_index[1L]-1L])$name else "")
  }
  bracket_count <- length(matches)
  matches <- c(matches, series)
  history_cache <- new.env(parent = emptyenv())
  appearances <- rbindlist(lapply(matches, function(m) data.table(
    round = m$round, name = vapply(m$players, function(p) p$name, character(1)))))
  contradictions <- appearances[round <= 6L & !startsWith(name, "Winner of "), .N, by = .(round, name)][N > 1L]
  for (i in seq_along(matches)) {
    m <- matches[[i]]
    # The result prefix identifies the winner's stone colour.
    if (nzchar(m$result) && nzchar(m$winner) && length(m$players) == 2L) {
      winner_colour <- substr(m$result, 1L, 1L)
      if (winner_colour %in% c("B", "W")) {
        for (j in 1:2) m$players[[j]]$colour <- if (m$players[[j]]$name == m$winner) winner_colour else if (winner_colour == "B") "W" else "B"
      }
    }
    m$ratingBasis <- "unavailable"
    names <- vapply(m$players, function(p) p$name, character(1))
    m$identityConflict <- any(contradictions$round == m$round & contradictions$name %in% names)
    if (length(m$players) == 2L && nzchar(m$result)) {
      ids <- vapply(m$players, function(p) p$id, character(1))
      id <- ids[nzchar(ids)][1L]
      if (!is.na(id)) {
        if (!exists(id, history_cache, inherits = FALSE)) {
          file <- paste0("Go/data/games/", id, ".json")
          assign(id, if (file.exists(file)) as.data.table(fromJSON(file)) else data.table(), history_cache)
        }
        games <- get(id, history_cache)
        if (nrow(games) && !m$identityConflict) {
          candidates <- games[((black == names[1L] & white == names[2L]) |
            (black == names[2L] & white == names[1L])) &
            ((result == "1-0" & black == m$winner) | (result == "0-1" & white == m$winner)) &
            (if (winner_colour == "B") black == m$winner else white == m$winner)]
          hit <- candidates[date == m$date]
          m$historyMatch <- "exact-date"
          if (!nrow(hit)) {
            hit <- candidates[abs(as.integer(as.Date(date) - as.Date(m$date))) <= 7L]
            m$historyMatch <- "unique-nearby-date"
          }
          hit <- unique(hit[, .(date, black, white, blackElo, whiteElo)])
          if (nrow(hit) == 1L) {
            m$historyDate <- hit$date
            for (j in 1:2) m$players[[j]]$rating <- as.numeric(if (names[j] == hit$black) hit$blackElo else hit$whiteElo)
            m$ratingBasis <- "pre-game"
          }
        }
      }
      if (m$ratingBasis == "unavailable") for (j in 1:2) m$players[[j]]$rating <- NULL
    } else if (length(m$players) == 2L && all(vapply(m$players, function(p) !is.null(p$rating), logical(1)))) {
      m$ratingBasis <- "current"
    }
    if (m$ratingBasis != "unavailable") {
      probability <- 1 / (1 + 10 ^ ((m$players[[2]]$rating - m$players[[1]]$rating) / 400))
      m$players[[1]]$winPct <- round(100 * probability)
      m$players[[2]]$winPct <- 100 - m$players[[1]]$winPct
    }
    matches[[i]] <- m
  }
  series <- matches[seq.int(bracket_count+1L,length(matches))]
  matches <- matches[seq_len(bracket_count)]
  matches[[final_index]]$series <- series
  wins <- vapply(matches[[final_index]]$players, function(p) sum(vapply(series,function(g) g$winner == p$name,logical(1))),integer(1))
  matches[[final_index]]$seriesScores <- as.list(wins)
  matches[[final_index]]$winner <- if(any(wins >= 2L)) matches[[final_index]]$players[[which.max(wins)]]$name else ""
  for(j in seq_along(matches[[final_index]]$players)) {
    matches[[final_index]]$players[[j]]$rating <- NULL
    matches[[final_index]]$players[[j]]$winPct <- NULL
  }
  dir.create("Go/data/tournaments",recursive=TRUE,showWarnings=FALSE)
  payload <- list(name=paste0(edition,if(edition==3L) "rd" else "th"," Quzhou-Lanke Cup"),season=year,format="knockout",snapshot=as.character(file.info(path)$mtime),
    source=paste0("https://gotoeveryone.k2ss.info/news/wr/quzhou-lanke/",edition,"/"),slots=nrow(grid)-1L,
    rounds=list("First round","Round of 32","Round of 16","Quarterfinals","Semifinals","Final"),matches=matches,
    finalDates=lapply(series,function(g) g$date),note="Saved-source preview. Final is best of three; game three is conditional.")
  output <- paste0("Go/data/tournaments/lanke-",year,".json")
  write_json(payload,output,auto_unbox=TRUE,pretty=TRUE,null="null")
  cat("Saved",output,";",length(matches),"bracket entries. No ratings changed.\n")
})
