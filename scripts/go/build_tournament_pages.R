# Run from the repository root. Exports saved audit results; no network or Elo changes.
local({
  library(data.table); library(jsonlite)
  source("scripts/go/tournament_brackets.R")
  started <- proc.time()[[3]]
  out <- Sys.getenv("GO_TOURNAMENT_BUILD_OUTPUT", "Go/data/tournaments/catalogue")
  dir.create(out, recursive=TRUE, showWarnings=FALSE)
  source <- fread("Go/pipeline_data/tournament_result_coverage/source_results.csv", encoding="UTF-8")
  registry <- fread("Go/pipeline_data/tournament_format_audit/tournament_registry.csv", encoding="UTF-8")
  profiles <- as.data.table(fromJSON("Go/data/players.json"))
  norm <- function(x) gsub("[^a-z0-9]","",tolower(iconv(x,"UTF-8","ASCII//TRANSLIT")))
  profiles[, key := norm(name)]
  cache <- new.env(parent=emptyenv())
  person <- function(canonical,label) {
    found <- profiles[key == norm(if(!is.na(canonical) && nzchar(canonical)) canonical else label)]
    if(nrow(found)!=1L) return(list(name=label,id="",flag="",rating=NULL))
    list(name=found$name,id=found$id,flag=found$flag,rating=NULL)
  }
  history <- function(id) {
    if(!exists(id,cache,inherits=FALSE)) {
      path <- paste0("Go/data/games/",id,".json")
      assign(id,if(file.exists(path)) as.data.table(fromJSON(path)) else data.table(),cache)
    }
    get(id,cache)
  }
  pages <- unique(source[,.(SourceURL,Family,Tournament)])
  stopifnot(!anyDuplicated(pages$SourceURL))
  limit <- as.integer(Sys.getenv("GO_TOURNAMENT_BUILD_LIMIT", "0"))
  stopifnot(!is.na(limit),limit >= 0L)
  if(limit > 0L) pages <- head(pages,limit)
  catalogue <- list(); review <- list()
  for(i in seq_len(nrow(pages))) {
    page <- pages[i]; rows <- source[SourceURL == page$SourceURL]
    # Deduplicate repeated representations without merging separate games.
    rows <- unique(rows,by=c("Stage","Round","Date","PlayerA","PlayerB","Winner","Result"))
    games <- list()
    for(j in seq_len(nrow(rows))) {
      row <- rows[j]; a <- person(row$CanonicalA,row$PlayerA); b <- person(row$CanonicalB,row$PlayerB)
      winner <- person(row$CanonicalWinner,row$Winner)$name
      colour <- substr(row$Result,1,1)
      if(!colour %in% c("B","W") || !winner %in% c(a$name,b$name)) {
        review[[length(review)+1L]] <- data.table(SourceURL=page$SourceURL,RowID=row$RowID,Reason="Unresolved winner or stone colour")
        next
      }
      a$colour <- if(a$name==winner) colour else if(colour=="B") "W" else "B"
      b$colour <- if(b$name==winner) colour else if(colour=="B") "W" else "B"
      id <- if(nzchar(a$id)) a$id else b$id
      basis <- "unavailable"; history_date <- ""
      if(nzchar(id) && !is.na(row$Date) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",row$Date)) {
        h <- history(id)
        if(nrow(h)) {
          candidates <- h[((black==a$name & white==b$name)|(black==b$name & white==a$name)) &
            ((result=="1-0" & black==winner)|(result=="0-1" & white==winner)) &
            (if(colour=="B") black==winner else white==winner)]
          hit <- candidates[date==row$Date]
          if(!nrow(hit)) hit <- candidates[abs(as.integer(as.Date(date)-as.Date(row$Date)))<=7L]
          hit <- unique(hit[,.(date,black,white,blackElo,whiteElo)])
          if(nrow(hit)==1L) {
            a$rating <- as.numeric(if(a$name==hit$black) hit$blackElo else hit$whiteElo)
            b$rating <- as.numeric(if(b$name==hit$black) hit$blackElo else hit$whiteElo)
            a$winPct <- round(100/(1+10^((b$rating-a$rating)/400))); b$winPct <- 100-a$winPct
            basis <- "pre-game"; history_date <- hit$date
          }
        }
      }
      games[[length(games)+1L]] <- list(stage=row$Stage,round=row$Round,date=if(is.na(row$Date)) "" else row$Date,
        players=list(a,b),winner=winner,result=row$Result,ratingBasis=basis,historyDate=history_date)
    }
    slug <- gsub("[^a-z0-9]+","-",tolower(sub("https://gotoeveryone.k2ss.info/news/","",page$SourceURL)))
    format <- registry[Family==page$Family,Classification][1L]
    if(is.na(format)) format <- "Unclassified"
    target <- paste0(slug,".json")
    draw <- go_tournament_brackets(page$SourceURL,page$Family,rows,games,person,profiles)
    if(!is.null(draw$games))games <- draw$games
    write_json(list(name=page$Tournament,format=format,source=page$SourceURL,games=games,
      brackets=draw$brackets,bracketGameIndices=as.list(draw$consumed-1L),bracketReview=draw$review,sourceStandings=draw$standings,leagueTables=draw$leagueTables),
      file.path(out,target),auto_unbox=TRUE,pretty=FALSE,null="null")
    catalogue[[i]] <- list(name=page$Tournament,format=format,file=target,games=length(games),family=page$Family)
    if(i%%25L==0L || i==nrow(pages)) cat("Page",i,"/",nrow(pages),"\n")
  }
  write_json(catalogue,file.path(out,"index.json"),auto_unbox=TRUE,pretty=FALSE)
  if(out == "Go/data/tournaments/catalogue") source("scripts/go/build_tournament_links.R")
  if(length(review)) fwrite(rbindlist(review),"Go/pipeline_data/tournament_result_coverage/page_build_review.csv")
  cat("Built",length(catalogue),"edition pages from saved results. No downloads or ratings changes.\n")
  cat("Open http://localhost:8000/Go/tournaments/ after starting your local site.\nElapsed:",round(proc.time()[[3]]-started,1),"seconds\n")
})
