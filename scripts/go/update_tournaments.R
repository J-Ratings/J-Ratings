# Daily supplementary tournament update. No production game imports or Elo runs.
# Live fetching is deliberately disabled until GO_K2SS_PERMISSION_GRANTED=true.
# GO_TOURNAMENT_OFFLINE=true reuses saved pages for local verification.
go_tournament_request_wait <- function(last_finished,now=Sys.time()) {
  if(is.null(last_finished))return(0)
  max(0,3-as.numeric(difftime(now,last_finished,units="secs")))
}
go_update_tournaments <- function() {
  offline <- tolower(Sys.getenv("GO_TOURNAMENT_OFFLINE","false"))=="true"
  if(!offline && tolower(Sys.getenv("GO_K2SS_PERMISSION_GRANTED","false"))!="true") {
    cat("K2SS permission pending: no requests or tournament updates performed.\n")
    return(invisible(list(requests=0L,updated=0L)))
  }
  suppressPackageStartupMessages({library(data.table);library(jsonlite);library(xml2)})
  source("scripts/go/tournament_source_results.R")
  source("scripts/go/tournament_brackets.R")
  today <- as.Date(format(Sys.time(),tz="Europe/London",format="%Y-%m-%d"))
  override <- Sys.getenv("GO_TOURNAMENT_DATE", "")
  if(nzchar(override))today <- as.Date(override)
  stopifnot(!is.na(today))
  cache <- "Go/pipeline_data/tournament_format_audit/cache"
  out <- Sys.getenv("GO_TOURNAMENT_OUTPUT","Go/data/tournaments/catalogue")
  state_dir <- Sys.getenv("GO_TOURNAMENT_STATE_DIR","Go/pipeline_data/tournament_updates")
  dir.create(cache,recursive=TRUE,showWarnings=FALSE);dir.create(state_dir,recursive=TRUE,showWarnings=FALSE)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  write_atomic <- function(value,path) {
    temp <- paste0(path,".part");write_json(value,temp,auto_unbox=TRUE,null="null")
    if(file.exists(path)&&.Platform$OS.type=="windows") {
      backup <- paste0(path,".previous");if(file.exists(backup))unlink(backup)
      if(!file.rename(path,backup))stop("Cannot preserve previous file: ",path)
      if(!file.rename(temp,path)){file.rename(backup,path);stop("Cannot replace: ",path)}
      unlink(backup)
    } else if(!file.rename(temp,path))stop("Cannot replace: ",path)
  }
  requests <- 0L;last_finished <- NULL
  completed <- FALSE
  on.exit(if(!completed)write_atomic(list(date=as.character(today),status="failed",requests=requests,
    note="Run stopped. No network retries; previous catalogue/index retained."),file.path(state_dir,"latest-report.json")),add=TRUE)
  fetch <- function(url,path) {
    stopifnot(startsWith(url,"https://gotoeveryone.k2ss.info/"))
    if(offline){if(file.exists(path))return(TRUE);return(FALSE)}
    # A full three seconds after the preceding response, including slow ones.
    if(!is.null(last_finished)) {
      delay <- go_tournament_request_wait(last_finished)
      if(delay>0)Sys.sleep(delay)
    }
    requests <<- requests+1L;temp <- paste0(path,".part")
    on.exit({last_finished <<- Sys.time();if(file.exists(temp))unlink(temp)},add=TRUE)
    old_timeout <- getOption("timeout");options(timeout=60);on.exit(options(timeout=old_timeout),add=TRUE)
    # No retry loop. Any access/server error stops the run, preserving old files.
    status <- download.file(url,temp,method="libcurl",mode="wb",quiet=TRUE)
    if(status!=0L)stop("Source request failed; stopped without retry: ",url)
    if(grepl("\\.json$",path))fromJSON(temp,simplifyVector=FALSE) else read_html(temp)
    if(file.exists(path)&&.Platform$OS.type=="windows") {
      backup <- paste0(path,".previous");if(file.exists(backup))unlink(backup)
      if(!file.rename(path,backup))stop("Cannot preserve cache")
      if(!file.rename(temp,path)){file.rename(backup,path);stop("Cannot replace cache")};unlink(backup)
    } else if(!file.rename(temp,path))stop("Cannot save source page")
    TRUE
  }
  index_path <- file.path(out,"index.json")
  catalogue <- if(file.exists(index_path))fromJSON(index_path,simplifyVector=FALSE) else list()
  state_path <- file.path(state_dir,"state.json")
  state <- if(file.exists(state_path))fromJSON(state_path,simplifyVector=FALSE) else list()
  calendar_path <- Sys.getenv("GO_TOURNAMENT_CALENDAR","Go/pipeline_data/tournament_format_audit/calendar_events.csv")
  calendar <- if(file.exists(calendar_path))fread(calendar_path) else data.table(Date=character(),Label=character(),Link=character(),Family=character())
  calendar[,Date:=as.character(Date)]
  if(!"Family" %in% names(calendar))calendar[,Family:=sub("^(/news/[^/]+/[^/]+/).*","\\1",Link)]
  # Adjacent month only when the 7-day lookback/7-day lookahead crosses a boundary.
  months <- unique(format(seq(today-7,today+7,by="day"),"%Y/%m"))
  for(month in months) {
    path <- file.path(cache,paste0(gsub("/","_",month),".json"))
    if(!fetch(paste0("https://gotoeveryone.k2ss.info/json/schedules/",month,".json"),path))next
    days <- fromJSON(path,simplifyVector=FALSE);new <- list()
    for(day in days)for(item in day$items) {
      if(!is.null(item$link)&&grepl("^/news/[a-z-]+/[a-z0-9-]+/[a-z0-9-]+/$",item$link))
        new[[length(new)+1L]] <- data.table(Date=day$date,Label=item$name,Link=item$link,Family=sub("^(/news/[^/]+/[^/]+/).*","\\1",item$link))
    }
    calendar <- calendar[!startsWith(Date,gsub("/","-",month))]
    if(length(new))calendar <- unique(rbind(calendar,rbindlist(new),fill=TRUE))
  }
  fwrite(calendar,calendar_path)
  due <- unique(calendar[Date>=as.character(today-7)&Date<=as.character(today+7),Link])
  # Persist ongoing pages rather than scan every historical edition every day.
  ongoing <- names(state)[vapply(state,function(s)isTRUE(s$pending)&&!is.null(s$lastScheduled)&&
    as.Date(s$lastScheduled)>=today-30,TRUE)]
  queue <- unique(c(due,ongoing))
  subset <- Sys.getenv("GO_TOURNAMENT_LINKS","")
  if(nzchar(subset))queue <- strsplit(subset,",",fixed=TRUE)[[1L]]
  stopifnot(all(grepl("^/news/[a-z-]+/[a-z0-9-]+/[a-z0-9-]+/$",queue)))
  limit <- suppressWarnings(as.integer(Sys.getenv("GO_TOURNAMENT_MAX_PAGES","20")))
  stopifnot(!is.na(limit),limit>0L)
  # Overflow remains in state and is prioritised before new pages on the next run.
  deferred <- names(state)[vapply(state,function(s)isTRUE(s$deferred),TRUE)]
  if(!nzchar(subset))queue <- unique(c(deferred,queue))
  overflow <- if(length(queue)>limit)queue[(limit+1L):length(queue)] else character()
  for(link in overflow)state[[link]] <- list(deferred=TRUE,pending=TRUE,lastScheduled=as.character(today))
  queue <- head(queue,limit)
  profiles <- as.data.table(fromJSON("Go/data/players.json"))
  norm <- function(s)gsub("[^a-z0-9]","",tolower(iconv(s,"UTF-8","ASCII//TRANSLIT")))
  profiles[,key:=norm(name)]
  aliases <- data.table(NameKey=character(),name=character())
  map_path <- "Go/pipeline_data/processed/goratings_player_map.csv"
  if(file.exists(map_path)) {
    map <- fread(map_path)
    if(all(c("goratings_name","historical_name") %in% names(map))) {
      aliases <- unique(map[!is.na(historical_name)&nzchar(historical_name),.(NameKey=norm(goratings_name),name=historical_name)])
      aliases <- aliases[,if(uniqueN(name)==1L).SD[1L],by=NameKey]
    }
  }
  person <- function(name) {
    k <- norm(name);found <- profiles[key==k]
    if(nrow(found)!=1L){alias <- aliases[NameKey==k,name];if(length(alias)==1L)found <- profiles[key==norm(alias)]}
    if(nrow(found)!=1L)return(list(name=name,id="",flag="",rating=NULL))
    list(name=found$name,id=found$id,flag=found$flag,rating=NULL)
  }
  histories <- new.env(parent=emptyenv())
  enrich <- function(g) {
    if(length(g$players)!=2L||!nzchar(g$winner))return(g)
    for(i in 1:2)g$players[[i]]$colour <- if(g$players[[i]]$name==g$winner)substr(g$result,1,1) else if(substr(g$result,1,1)=="B")"W" else "B"
    if(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",g$date))return(g)
    id <- g$players[[1L]]$id;if(!nzchar(id))id <- g$players[[2L]]$id
    if(!nzchar(id))return(g)
    if(!exists(id,histories,inherits=FALSE)) {
      path <- paste0("Go/data/games/",id,".json")
      assign(id,if(file.exists(path))as.data.table(fromJSON(path)) else data.table(),histories)
    }
    h <- get(id,histories);if(!nrow(h))return(g)
    pn <- vapply(g$players,function(p)p$name,"")
    candidates <- h[((black==pn[1L]&white==pn[2L])|(black==pn[2L]&white==pn[1L])) &
      ((result=="1-0"&black==g$winner)|(result=="0-1"&white==g$winner)) &
      (if(startsWith(g$result,"B"))black==g$winner else white==g$winner)]
    hit <- candidates[date==g$date];if(!nrow(hit))hit <- candidates[abs(as.integer(as.Date(date)-as.Date(g$date)))<=7L]
    hit <- unique(hit[,.(date,black,white,blackElo,whiteElo)])
    if(nrow(hit)==1L) {
      for(i in 1:2)g$players[[i]]$rating <- as.numeric(if(pn[i]==hit$black)hit$blackElo else hit$whiteElo)
      g$players[[1L]]$winPct <- round(100/(1+10^((g$players[[2L]]$rating-g$players[[1L]]$rating)/400)))
      g$players[[2L]]$winPct <- 100-g$players[[1L]]$winPct;g$historyDate <- hit$date;g$ratingBasis <- "pre-game"
    }
    g
  }
  report <- list(date=as.character(today),offline=offline,requests=0L,updated=list(),review=list(),deferred=as.list(overflow))
  for(link in queue) {
    path <- file.path(cache,paste0(gsub("[^a-zA-Z0-9]+","_",link),".html"))
    url <- paste0("https://gotoeveryone.k2ss.info",link)
    if(!fetch(url,path)){report$review[[link]] <- "Saved page unavailable";next}
    family <- sub("^(/news/[^/]+/[^/]+/).*","\\1",link)
    parsed <- go_source_results(path,url,family,calendar,person)
    rows <- parsed$rows;games <- list()
    for(i in seq_len(nrow(rows))) {
      row <- rows[i];a <- person(row$PlayerA);b <- person(row$PlayerB)
      g <- list(stage=row$Stage,round=row$Round,date=row$Date,players=list(a,b),winner=row$Winner,result=row$Result,ratingBasis="unavailable")
      games[[length(games)+1L]] <- enrich(g)
    }
    adapter_person <- function(canonical,label)person(if(!is.na(canonical)&&nzchar(canonical))canonical else label)
    draw <- go_tournament_brackets(url,family,rows,games,adapter_person,profiles,calendar_override=calendar)
    doc <- read_html(path);name <- trimws(xml_text(xml_find_first(doc,"//main//h2|//article//h2")))
    file <- paste0(gsub("[^a-z0-9]+","-",tolower(sub("^/news/","",link))),".json")
    old <- Filter(function(e)e$file==file,catalogue)
    if(length(old)&&length(games)<old[[1L]]$games) {
      report$review[[link]] <- list(reason="Result count fell: preserving previous edition for review",old=old[[1L]]$games,new=length(games));next
    }
    if(!length(games)&&!length(draw$brackets)&&!length(draw$leagueTables)) {
      report$review[[link]] <- "Unsupported or empty edition: preserving previous data";next
    }
    format <- if(length(old))old[[1L]]$format else if(length(draw$leagueTables))"Team league" else if(length(draw$brackets))"Knockout stages" else "Mixed stages"
    payload <- list(name=name,format=format,source=url,games=draw$games,brackets=draw$brackets,
      bracketGameIndices=as.list(draw$consumed-1L),bracketReview=draw$review,sourceStandings=draw$standings,leagueTables=draw$leagueTables)
    write_atomic(payload,file.path(out,file))
    catalogue <- Filter(function(e)e$file!=file,catalogue)
    catalogue[[length(catalogue)+1L]] <- list(name=name,format=format,file=file,games=length(games),family=family)
    pending <- length(draw$leagueTables)>0L || any(vapply(unlist(lapply(draw$brackets,function(b)b$matches),recursive=FALSE),function(m)!nzchar(m$winner),TRUE))
    scheduled <- calendar[Link==link,Date]
    state[[link]] <- list(pending=pending,deferred=FALSE,lastScheduled=if(length(scheduled))max(scheduled) else as.character(today),lastChecked=as.character(today))
    report$updated[[length(report$updated)+1L]] <- file
    unknown <- unique(unlist(lapply(draw$games,function(g)vapply(Filter(function(p)!nzchar(p$id),g$players),function(p)p$name,""))))
    if(length(parsed$review)||length(draw$review)||length(unknown)||!length(old))report$review[[link]] <-
      list(newEdition=!length(old),unresolvedPlayers=as.list(unknown),results=parsed$review,layout=draw$review)
  }
  write_atomic(catalogue,index_path);write_atomic(state,state_path)
  report$requests <- requests;write_atomic(report,file.path(state_dir,"latest-report.json"))
  if(length(report$updated)) {
    old_output <- Sys.getenv("GO_TOURNAMENT_LINK_OUTPUT",unset=NA_character_)
    Sys.setenv(GO_TOURNAMENT_LINK_OUTPUT=out)
    on.exit(if(is.na(old_output))Sys.unsetenv("GO_TOURNAMENT_LINK_OUTPUT") else Sys.setenv(GO_TOURNAMENT_LINK_OUTPUT=old_output),add=TRUE)
    source("scripts/go/build_tournament_links.R")
  }
  cat("Tournament update:",length(report$updated),"editions;",requests,"requests;",length(report$review),"review items. No Elo/import changes.\n")
  completed <- TRUE
  invisible(report)
}
if(!identical(Sys.getenv("GO_TOURNAMENT_FUNCTIONS_ONLY"),"true"))go_update_tournaments()
