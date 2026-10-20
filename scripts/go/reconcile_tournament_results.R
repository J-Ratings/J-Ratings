# Overlay unique, exact-date profile results onto saved draw slots. No downloads.
go_reconcile_draw <- function(matches,lookup) {
  review <- character();changed <- 0L
  known <- function(p)!is.null(p$name)&&nzchar(p$name)&&!grepl("previous match|Winner of|Loser of|Draw pending",p$name,ignore.case=TRUE)
  # Iterate to resolve feeder winners before finals, independent of source order.
  for(pass in seq_len(length(matches)+1L)) {
    progress <- FALSE
    for(i in seq_along(matches)) {
      m <- matches[[i]]; feeds <- m$feedsFrom
      placeholders <- which(!vapply(m$players,known,TRUE))
      if(length(placeholders) && length(feeds))for(j in placeholders) {
        fi <- if(length(feeds)==length(m$players))j else if(length(feeds)==1L&&length(placeholders)==1L)1L else NA_integer_
        if(is.na(fi))next
        f <- feeds[[fi]];id <- if(is.character(f))f else f$id
        at <- which(vapply(matches,function(n)identical(n$id,id),TRUE));if(length(at)!=1L)next
        parent <- matches[[at]];if(is.null(parent$winner)||!nzchar(parent$winner))next
        outcome <- if(is.list(f)&&!is.null(f$outcome))f$outcome else "winner"
        eligible <- Filter(function(p)known(p) && if(outcome=="loser")p$name!=parent$winner else p$name==parent$winner,parent$players)
        if(length(eligible)==1L) {
          # Feeder's previous rating/stone colour are not the next game's values.
          m$players[[j]] <- eligible[[1L]][intersect(c("name","id","flag"),names(eligible[[1L]]))]
          progress <- TRUE;changed <- changed+1L
        }
      }
      series <- length(m$series)>0L
      slots <- if(series)m$series else list(m)
      for(k in seq_along(slots)) {
        g <- slots[[k]]
        if(series)for(j in seq_along(g$players))if(!known(g$players[[j]])&&known(m$players[[j]]))g$players[[j]] <- m$players[[j]]
        if(!is.null(g$winner)&&nzchar(g$winner)){slots[[k]] <- g;next}
        if(length(g$players)!=2L||!all(vapply(g$players,known,TRUE))||is.null(g$date)||!nzchar(g$date)) {slots[[k]] <- g;next}
        hit <- lookup(g$date,g$players)
        if(length(hit)==1L) {
          result <- hit[[1L]]
          for(field in names(result))g[[field]] <- result[[field]]
          g$resultSource <- "GoRatings";g$ratingBasis <- "pre-game"
          slots[[k]] <- g;changed <- changed+1L;progress <- TRUE
        } else {
          slots[[k]] <- g
          if(length(hit)>1L)review <- unique(c(review,paste("Ambiguous same-day results:",g$date,paste(vapply(g$players,function(p)p$name,""),collapse=" / "))))
        }
      }
      if(series) {
        m$series <- slots
        m$seriesScores <- lapply(m$players,function(p)sum(vapply(slots,function(g)!is.null(g$winner)&&g$winner==p$name,TRUE)))
        wins <- unlist(m$seriesScores)
        if(length(wins)==2L&&max(wins)>length(slots)/2)m$winner <- m$players[[which.max(wins)]]$name
      } else m <- slots[[1L]]
      matches[[i]] <- m
    }
    if(!progress)break
  }
  list(matches=matches,changed=changed,review=review)
}

go_reconcile_tournaments <- function(links,catalogue_dir="Go/data/tournaments/catalogue",games_dir="Go/data/games") {
  library(jsonlite)
  index <- fromJSON(file.path(catalogue_dir,"index.json"),simplifyVector=FALSE)
  cache <- new.env(parent=emptyenv())
  lookup <- function(date,players) {
    # One player's history suffices. The result must identify the exact opponent
    # and date; repeated same-day games remain unresolved rather than guessed.
    id <- players[[1L]]$id;if(is.null(id)||!nzchar(id))id <- players[[2L]]$id
    if(is.null(id)||!nzchar(id))return(list())
    if(!exists(id,cache,inherits=FALSE)) {
      path <- file.path(games_dir,paste0(id,".json"))
      assign(id,if(file.exists(path))fromJSON(path,simplifyVector=FALSE) else list(),cache)
    }
    pn <- sort(vapply(players,function(p)p$name,""))
    hits <- Filter(function(g)g$date==date && identical(sort(c(g$black,g$white)),pn) && g$result %in% c("1-0","0-1"),get(id,cache))
    lapply(hits,function(h) {
      black_won <- h$result=="1-0"
      ps <- lapply(players,function(p) {
        p$colour <- if(p$name==h$black)"B" else "W"
        p$rating <- if(p$name==h$black)h$blackElo else h$whiteElo
        p$winPct <- if(p$name==h$black)h$blackWinPct else h$whiteWinPct;p
      })
      # The profile supplies a winner, not a resignation/margin. Never invent R.
      list(players=ps,winner=if(black_won)h$black else h$white,result=if(black_won)"B" else "W",
        date=date,historyDate=date,resultDetailUnavailable=TRUE)
    })
  }
  write <- function(value,path) {
    tmp <- paste0(path,".daily-part");write_json(value,tmp,auto_unbox=TRUE,null="null")
    backup <- paste0(path,".daily-previous")
    if(file.exists(backup))unlink(backup)
    if(file.exists(path)&&!file.rename(path,backup))stop("Cannot back up ",path)
    if(!file.rename(tmp,path)){if(file.exists(backup))file.rename(backup,path);stop("Cannot save ",path)}
  }
  updated <- character();review <- list()
  for(i in seq_along(index)) {
    entry <- index[[i]];path <- file.path(catalogue_dir,entry$file)
    d <- fromJSON(path,simplifyVector=FALSE)
    if(!sub("https://gotoeveryone.k2ss.info","",d$source,fixed=TRUE) %in% links)next
    changed <- 0L
    for(b in seq_along(d$brackets)) {
      resolved <- go_reconcile_draw(d$brackets[[b]]$matches,lookup)
      d$brackets[[b]]$matches <- resolved$matches;changed <- changed+resolved$changed
      review[[entry$file]] <- unique(c(review[[entry$file]],resolved$review))
      for(m in resolved$matches)for(g in if(length(m$series))m$series else list(m)) {
        if(is.null(g$resultSource)||g$resultSource!="GoRatings")next
        same <- which(vapply(d$games,function(old)identical(old$date,g$date)&&
          identical(sort(vapply(old$players,function(p)p$name,"")),sort(vapply(g$players,function(p)p$name,""))),TRUE))
        if(length(same)>1L)next
        g$stage <- d$brackets[[b]]$name;g$round <- m$round
        if(length(same)==1L) {at <- same; d$games[[at]] <- g} else {d$games[[length(d$games)+1L]] <- g;at <- length(d$games)}
        d$bracketGameIndices <- as.list(unique(c(unlist(d$bracketGameIndices),at-1L)))
      }
    }
    if(changed) {
      write(d,path);index[[i]]$games <- length(d$games);updated <- c(updated,entry$file)
      # The two original Lanke preview pages are still served by the local UI.
      year <- unname(c("wr-quzhou-lanke-3-.json"="2025","wr-quzhou-lanke-4-.json"="2026")[entry$file])
      if(length(year)==1L&&!is.na(year)) {
        preview_path <- file.path(dirname(catalogue_dir),paste0("lanke-",year,".json"))
        if(file.exists(preview_path)) {
          preview <- fromJSON(preview_path,simplifyVector=FALSE)
          preview$matches <- go_reconcile_draw(preview$matches,lookup)$matches
          write(preview,preview_path)
        }
      }
    }
  }
  if(length(updated))write(index,file.path(catalogue_dir,"index.json"))
  list(updated=updated,review=review)
}
