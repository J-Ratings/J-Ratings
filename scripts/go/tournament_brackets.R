# Source-geometry adapter for knockout tables. This is deliberately independent
# of tournament names: colspan/rowspan encode entrants, byes and feeder matches.
go_tournament_brackets <- function(url, family, rows, games, person, profiles,calendar_override=NULL) {
  library(xml2)
  link <- sub("https://gotoeveryone.k2ss.info", "", url, fixed=TRUE)
  path <- file.path("Go/pipeline_data/tournament_format_audit/cache",
                    paste0(gsub("[^a-zA-Z0-9]+", "_", link), ".html"))
  if (!file.exists(path)) return(list(brackets=list(), consumed=integer(), review="Saved source unavailable"))
  doc <- read_html(path)
  calendar_path <- "Go/pipeline_data/tournament_format_audit/calendar_events.csv"
  calendar <- if(file.exists(calendar_path)) fread(calendar_path) else data.table(Link=character(),Date=character())
  if(!is.null(calendar_override))calendar <- calendar_override
  clean <- function(x) trimws(sub("\\([^)]*\\)\\s*$", "", sub("^\\([a-z]+\\)", "", x)))
  items <- function(n) trimws(xml_text(xml_find_all(n, ".//span[contains(@class,'tournament-result-item')]")))
  identity_path <- "Go/pipeline_data/tournament_result_coverage/player_identity_review.csv"
  identity <- if(file.exists(identity_path)) fread(identity_path,encoding="UTF-8") else
    data.table(Family=character(),SourceName=character(),Canonical=character())
  identity <- identity[Family == family & !is.na(Canonical) & nzchar(Canonical)]
  player <- function(name) {
    name <- clean(name)
    canonical <- unique(identity[SourceName == name, Canonical])
    person(if(length(canonical)==1L) canonical else name, name)
  }
  # Preserve explicit home/visitor fixture rows; never infer teams from rosters.
  fixture_lists <- xml_find_all(doc,"//ul[contains(concat(' ',normalize-space(@class),' '),' al_winloss ')]")
  for(fi in seq_along(fixture_lists)) {
    ul <- fixture_lists[[fi]]
    team_a <- team_b <- fixture_score <- ""; board <- 0L
    heading <- trimws(xml_text(xml_find_first(ul,"preceding::*[self::h4][1]")))
    date <- regmatches(heading,regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",heading))
    for(tr in xml_find_all(ul,"./li")) {
      names <- trimws(xml_text(xml_find_all(tr,"./div[contains(@class,'al_name')]")))
      result <- trimws(xml_text(xml_find_first(tr,"./div[contains(@class,'al_result')]")))
      if(length(names)!=2L || is.na(result)) next
      if(grepl("^[0-9]+-[0-9]+$",result)) {team_a <- names[1L];team_b <- names[2L];fixture_score <- result;next}
      if(!nzchar(team_a) || !grepl("^[BW]\\+",result)) next
      board <- board+1L; participants <- unname(vapply(names,function(n) player(n)$name,""))
      hits <- which(vapply(games,function(g) identical(sort(vapply(g$players,function(p) p$name,"")),sort(participants)) &&
        g$result==result && (length(date)!=1L || g$date==date),TRUE))
      if(length(hits)!=1L) next
      games[[hits]]$fixture <- list(id=paste0("fixture-",fi),teamA=team_a,teamB=team_b,score=fixture_score,board=board)
      for(j in seq_along(games[[hits]]$players)) games[[hits]]$players[[j]]$team <- if(games[[hits]]$players[[j]]$name==participants[1L]) team_a else team_b
    }
  }
  score_rx <- "^[BW]\\+(R|T|[0-9]+(\\.[0-9]+)?)$"
  brackets <- list(); consumed <- integer(); review <- character()
  tables <- xml_find_all(doc, "//table[contains(@class,'tournament-')]")
  for (ti in seq_along(tables)) {
    table <- tables[[ti]]; cells <- list()
    trs <- xml_find_all(table, "./tr|./thead/tr|./tbody/tr|./tfoot/tr")
    grid <- matrix(NA_integer_, length(trs), 64L)
    valid <- TRUE
    for (r in seq_along(trs)) {
      col <- 1L
      for (n in xml_find_all(trs[[r]], "./td|./th")) {
        while(col<=64L && !is.na(grid[r,col])) col <- col+1L
        span <- function(a) {v <- suppressWarnings(as.integer(xml_attr(n,a))); if(is.na(v)) 1L else v}
        rs <- span("rowspan"); cs <- span("colspan")
        if(rs<1L || cs<1L || col+cs-1L>64L || r+rs-1L>nrow(grid)) {valid <- FALSE; break}
        cells[[length(cells)+1L]] <- list(node=n,row=r,col=col,rs=rs,cs=cs,
          text=trimws(xml_text(n)),items=items(n),header=xml_name(n)=="th")
        grid[r:(r+rs-1L),col:(col+cs-1L)] <- length(cells); col <- col+cs
      }
      if(!valid) break
    }
    if(!valid) {review <- c(review,paste("Invalid source spans in table",ti)); next}
    # Full-width empty rows split winners/losers or independent qualifying draws.
    table_width <- max(which(colSums(!is.na(grid))>0L))
    separators <- unique(vapply(Filter(function(c) !c$header && !nzchar(c$text) &&
      !length(c$items) && c$rs==1L && c$cs>=table_width-1L,cells),function(c) c$row,0L))
    segment <- function(r) sum(separators<r)+1L
    nodes <- list(); cell_nodes <- list()
    heading <- trimws(xml_text(xml_find_first(table,"preceding::*[self::h3][1]")))
    if(is.na(heading) || !nzchar(heading)) heading <- "Knockout"
    header_for <- function(cell,seg) {
      ids <- unique(na.omit(grid[,cell$col])); hs <- Filter(function(i) cells[[i]]$header && cells[[i]]$cs==1L,ids)
      if(!length(hs)) return(list(text=paste("Round",cell$col-1L),items=character()))
      above <- Filter(function(i) cells[[i]]$row<cell$row,hs)
      below <- Filter(function(i) cells[[i]]$row>cell$row,hs)
      # A bottom header describes the lower bracket in a split table.
      id <- if(seg>1L && length(below)) below[[1L]] else if(length(above)) tail(above,1L)[[1L]] else hs[[1L]]
      cells[[id]]
    }
    redraw_ids <- which(vapply(cells,function(c) !c$header &&
      grepl("^(re[- ]?lottery|re[- ]?draw|redraw)$",trimws(c$text),ignore.case=TRUE),TRUE))
    cell_order <- order(vapply(cells,function(x) x$col,0L),vapply(cells,function(x) x$row,0L))
    for (ci in cell_order) {
      cell <- cells[[ci]]
      if(cell$header || cell$col==1L || !length(cell$items)) next
      prior <- unique(na.omit(grid[cell$row:(cell$row+cell$rs-1L),cell$col-1L]))
      prior <- prior[!vapply(prior,function(i) cells[[i]]$header || !nzchar(cells[[i]]$text) && !length(cells[[i]]$items),TRUE)]
      after_redraw <- any(prior %in% redraw_ids)
      if(!after_redraw && length(prior)==1L && !is.null(cell_nodes[[as.character(prior)]])) {
        # A one-input source cell advances a player/bye without another game.
        if(!any(grepl(score_rx,cell$items))) cell_nodes[[as.character(ci)]] <- cell_nodes[[as.character(prior)]]
        next
      }
      if(!after_redraw && length(prior)!=2L) {
        if(length(prior)>2L) review <- c(review,paste("Ambiguous feeders in table",ti,"cell",ci))
        next
      }
      incoming <- if(after_redraw) {
        # A redraw is a pool, not two fixed feeder matches. Only an explicit
        # source result/pairing can identify its participants.
        links <- xml_attr(xml_find_all(cell$node,".//a[contains(@href,'/kifu/')]"),"href")
        evidence <- rows[KifuLink %in% links]
        pairs <- unique(evidence[,.(PlayerA,PlayerB,CanonicalA,CanonicalB)])
        pairs <- pairs[!is.na(PlayerA) & !is.na(PlayerB) & nzchar(PlayerA) & nzchar(PlayerB) & PlayerA!=PlayerB]
        named <- lapply(unique(cell$items),player)
        named <- named[vapply(named,function(p) nzchar(p$id),TRUE)]
        if(nrow(pairs)==1L) {
          list(list(player=person(pairs$CanonicalA,pairs$PlayerA),from=""),
               list(player=person(pairs$CanonicalB,pairs$PlayerB),from=""))
        } else if(length(named)==2L) {
          lapply(named,function(p) list(player=p,from=""))
        } else lapply(1:2,function(j) list(player=list(name="Draw pending",id="",flag=""),from=""))
      } else lapply(prior,function(i) {
        prev <- cells[[i]]
        from <- cell_nodes[[as.character(i)]]
        if(!is.null(from)) {
          m <- nodes[[from]]
          list(player=if(nzchar(m$winner)) player(m$winner) else list(name="Winner of previous match",id="",flag=""),from=m$id)
        } else if(!length(prev$items)) list(player=player(prev$text),from="")
        else NULL
      })
      if(any(vapply(incoming,is.null,TRUE))) {review <- c(review,paste("Unresolved feeder in table",ti,"cell",ci)); next}
      seg <- segment(cell$row); h <- header_for(cell,seg)
      if(any(separators>=cell$row & separators<cell$row+cell$rs)) seg <- length(separators)+2L
      round <- if(length(h$items)) h$items[1L] else h$text
      id <- paste0("t",ti,"-c",ci)
      participants <- lapply(incoming,`[[`,"player")
      feeds <- lapply(incoming,function(x) if(nzchar(x$from)) list(id=x$from,outcome="winner") else NULL)
      # Empty future cells retain their source pairing, without guessed ratings.
      m <- list(id=id,column=cell$col,row=cell$row,span=cell$rs,segment=seg,round=round,
        players=participants,feedsFrom=Filter(Negate(is.null),feeds),
        directEntrants=as.list(vapply(Filter(function(x) !nzchar(x$from),incoming),function(x) x$player$name,"")),
        winner="",date="",result="")
      m$redrawBefore <- after_redraw
      m$drawPending <- any(vapply(participants,function(p) p$name=="Draw pending",TRUE))
      match_nodes <- xml_find_all(cell$node,".//span[contains(concat(' ',normalize-space(@class),' '),' match ')]")
      if(length(match_nodes)) match_nodes <- Filter(function(n) {
        it <- items(n)
        length(it)>0L && (any(grepl(score_rx,it)) || any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",it)) || grepl("round|game",it[1L],ignore.case=TRUE))
      },as.list(match_nodes))
      is_series <- length(match_nodes)>1L
      if(!length(match_nodes)) match_nodes <- list(cell$node)
      series <- list()
      for(mn in match_nodes) {
        its <- items(mn); scores <- which(grepl(score_rx,its))
        winner <- if(length(scores)==1L && scores>1L) player(its[scores-1L])$name else ""
        result <- if(length(scores)==1L) its[scores] else ""
        kifu <- xml_attr(xml_find_first(mn,".//a[contains(@href,'/kifu/')]"),"href")
        sr <- if(!is.na(kifu)) rows[KifuLink==kifu] else rows[0]
        pn <- vapply(participants,function(x) x$name,"")
        hit <- which(vapply(games,function(g) identical(sort(vapply(g$players,function(x) x$name,"")),sort(pn)) &&
          g$winner==winner && g$result==result && (nrow(sr)!=1L || is.na(sr$Date) || !nzchar(sr$Date) || g$date==sr$Date),TRUE))
        g <- list(players=participants,winner=winner,result=result,date="",ratingBasis="unavailable")
        if(length(hit)==1L) {g <- games[[hit]]; consumed <- c(consumed,hit)} else {
          dates <- c(its,h$items); full <- unique(dates[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",dates)])
          if(nrow(sr)==1L && !is.na(sr$Date) && nzchar(sr$Date)) g$date <- sr$Date else if(length(full)==1L) g$date <- full else {
            md <- unique(dates[grepl("^[0-9]{2}-[0-9]{2}$",dates)])
            days <- unique(calendar[Link==link & substr(Date,6,10) %in% md,Date])
            if(length(days)==1L) g$date <- days
          }
          if(nzchar(result) && winner %in% pn) for(j in 1:2) g$players[[j]]$colour <- if(pn[j]==winner) substr(result,1,1) else if(substr(result,1,1)=="B") "W" else "B"
          if(!nzchar(result) && all(nzchar(vapply(participants,function(x) x$id,"")))) {
            for(j in 1:2) g$players[[j]]$rating <- profiles$rating[match(participants[[j]]$id,profiles$id)]
            ratings <- vapply(g$players,function(p) if(is.null(p$rating)) NA_real_ else as.numeric(p$rating),0.0)
            if(all(is.finite(ratings))) {
              g$players[[1]]$winPct <- round(100/(1+10^((ratings[2]-ratings[1])/400)))
              g$players[[2]]$winPct <- 100-g$players[[1]]$winPct; g$ratingBasis <- "current"
            }
          }
        }
        if(!nzchar(g$date)) {
          dates <- c(its,h$items); full <- unique(dates[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",dates)])
          if(length(full)==1L) g$date <- full else {
            md <- unique(dates[grepl("^[0-9]{2}-[0-9]{2}$",dates)])
            days <- unique(calendar[Link==link & substr(Date,6,10) %in% md,Date])
            if(length(days)==1L) g$date <- days
          }
        }
        if(nzchar(g$winner) && !g$winner %in% vapply(g$players,function(p) p$name,"")) {
          review <- c(review,paste("Winner contradicts feeders in table",ti,"cell",ci))
          g$winner <- ""; g$result <- ""
          for(j in seq_along(g$players)) {g$players[[j]]$rating <- NULL; g$players[[j]]$winPct <- NULL}
        }
        if(is_series) series[[length(series)+1L]] <- g else for(k in setdiff(names(g),c("stage","round"))) m[[k]] <- g[[k]]
      }
      if(is_series) {
        m$series <- series
        m$seriesScores <- lapply(participants,function(p) sum(vapply(series,function(g) g$winner==p$name,TRUE)))
        wins <- unlist(m$seriesScores)
        if(length(series)%%2L==1L && max(wins)>length(series)/2)
          m$winner <- participants[[which.max(wins)]]$name
        # An explicit aggregate winner takes precedence over the scheduled series.
        aggregate <- xml_find_all(cell$node,"./span[contains(@class,'tournament-result') and not(contains(concat(' ',normalize-space(@class),' '),' match '))]")
        ai <- if(length(aggregate)) items(aggregate[[1L]]) else character()
        if(length(ai) && player(ai[1L])$name %in% vapply(participants,function(x) x$name,"")) m$winner <- player(ai[1L])$name
      }
      nodes[[length(nodes)+1L]] <- m; cell_nodes[[as.character(ci)]] <- length(nodes)
    }
    if(!length(nodes)) next
    # Explicit named entries in a later region may be drops from the upper draw.
    # Only connect a unique, completed loss; a name alone never selects a game.
    for(j in seq_along(nodes)) {
      m <- nodes[[j]]
      if(m$segment>1L) for(p in Filter(function(p) p$name %in% unlist(m$directEntrants),m$players)) {
        candidates <- Filter(function(n) n$segment<m$segment && nzchar(n$winner) &&
          n$winner!=p$name && p$name %in% vapply(n$players,function(x) x$name,""),nodes)
        if(length(candidates)==1L && !any(vapply(m$feedsFrom,function(f) f$id==candidates[[1L]]$id,TRUE)))
          m$feedsFrom[[length(m$feedsFrom)+1L]] <- list(id=candidates[[1L]]$id,outcome="loser",player=p$name)
      }
      nodes[[j]] <- m
    }
    # A contradictory source draw must not acquire apparently authoritative Elo.
    appearances <- rbindlist(lapply(nodes,function(m) data.table(segment=m$segment,column=m$column,
      name=vapply(m$players,function(p) p$name,""))))
    conflicts <- appearances[!grepl("previous match|Draw pending",name),.N,by=.(segment,column,name)][N>1L]
    for(j in seq_along(nodes)) {
      m <- nodes[[j]]
      if(any(conflicts$segment==m$segment & conflicts$column==m$column &
             conflicts$name %in% vapply(m$players,function(p) p$name,""))) {
        m$identityConflict <- TRUE
        for(k in seq_along(m$players)) {m$players[[k]]$rating <- NULL; m$players[[k]]$winPct <- NULL}
        m$ratingBasis <- "unavailable"; nodes[[j]] <- m
      }
    }
    # Name each source region using its broad top/bottom header when available.
    for(seg in sort(unique(vapply(nodes,function(x) x$segment,0L)))) {
      ns <- Filter(function(m) m$segment==seg,nodes)
      broad <- Filter(function(c) c$header && c$cs>1L && grepl("tournament|bracket",c$text,ignore.case=TRUE),cells)
      label <- heading
      if(length(broad)) label <- broad[[min(seg,length(broad))]]$text
      if(seg>length(separators)+1L) label <- "Championship final"
      columns <- sort(unique(vapply(ns,function(x) x$column,0L)))
      for(j in seq_along(ns)) ns[[j]]$column <- match(ns[[j]]$column,columns)-1L
      brackets[[length(brackets)+1L]] <- list(id=paste0("t",ti,"-s",seg),name=label,type="knockout",matches=ns)
    }
  }
  # Published ranks and title-series scores outrank inferred knockout positions.
  standings <- list(); league_tables <- list()
  for(table in xml_find_all(doc,"//table[starts-with(@class,'league-') or starts-with(@class,'title-')]")) {
    label <- trimws(xml_text(xml_find_first(table,"preceding::*[self::h3][1]")))
    header <- trimws(xml_text(xml_find_all(table,"./tr[1]/td|./tr[1]/th|./tbody/tr[1]/td|./tbody/tr[1]/th")))
    if("Team" %in% header && "Rank" %in% header) {
      teams <- list()
      for(tr in xml_find_all(table,"./tr[position()>1]|./tbody/tr[position()>1]")) {
        values <- trimws(xml_text(xml_find_all(tr,"./td")))
        if(length(values)!=length(header)) next
        get <- function(name) {ix <- match(name,header); if(is.na(ix)) "" else values[ix]}
        teams[[length(teams)+1L]] <- list(name=get("Team"),rank=suppressWarnings(as.integer(get("Rank"))),
          points=get("Point"),boardWins=get("Wins"),record=get("Win-loss"))
      }
      if(length(teams))league_tables[[length(league_tables)+1L]] <- list(name=label,teams=teams)
      next
    }
    entries <- list()
    is_title <- startsWith(xml_attr(table,"class"),"title-")
    for(tr in xml_find_all(table,"./tr|./tbody/tr")) {
      td <- xml_find_all(tr,"./td"); if(length(td)<2L) next
      if(is_title) {
        p <- player(xml_text(td[[1L]]))
        wins <- sum(vapply(as.list(td[-1L]),function(c) grepl(score_rx,trimws(xml_text(c))),TRUE))
        entries[[length(entries)+1L]] <- list(player=p,wins=wins)
      } else {
        rank <- suppressWarnings(as.integer(trimws(xml_text(td[[1L]]))))
        if(is.na(rank)) next
        record <- trimws(xml_text(td)); record <- record[grepl("^[0-9]+-[0-9]+$",record)]
        wl <- if(length(record)==1L) as.integer(strsplit(record,"-",fixed=TRUE)[[1L]]) else c(NA_integer_,NA_integer_)
        entries[[length(entries)+1L]] <- list(player=player(xml_text(td[[2L]])),rank=rank,
          wins=if(is.na(wl[1L])) NULL else wl[1L],losses=if(is.na(wl[2L])) NULL else wl[2L],status="Published league position")
      }
    }
    if(is_title) {
      scheduled <- length(xml_find_all(table,"./tr[1]/th|./tbody/tr[1]/th"))-1L
      wins <- vapply(entries,function(e) e$wins,0L)
      if(length(entries)!=2L || scheduled<1L || max(wins)<=scheduled/2) next
      for(i in 1:2) {entries[[i]]$rank <- if(wins[i]==max(wins)) 1L else 2L
        entries[[i]]$losses <- wins[3L-i]; entries[[i]]$status <- if(entries[[i]]$rank==1L) "Champion" else "Runner-up"}
    }
    if(length(entries)) {
      section <- xml_find_first(table,"ancestor::section[1]")
      dates <- regmatches(xml_text(section),gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",xml_text(section)))[[1L]]
      standings[[length(standings)+1L]] <- list(name=label,entries=entries,asOf=if(length(dates)) max(dates) else "")
    }
  }
  list(brackets=brackets,consumed=unique(consumed),review=as.list(unique(review)),standings=standings,leagueTables=league_tables,games=games)
}
