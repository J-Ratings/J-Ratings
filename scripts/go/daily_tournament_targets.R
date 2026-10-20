# Pure local selection. No network calls or pipeline writes.
go_daily_targets <- function(today=as.Date(format(Sys.time(),tz="Europe/London",format="%Y-%m-%d")),
  catalogue_dir="Go/data/tournaments/catalogue",
  calendar_file="Go/pipeline_data/tournament_format_audit/calendar_events.csv",
  homepage_file="Go/pipeline_data/goratings/goratings_players_homepage.csv",
  map_file="Go/pipeline_data/processed/goratings_player_map.csv", max_profiles=80L,
  tournament_links=Sys.getenv("GO_DAILY_TOURNAMENT_LINKS","")) {
  library(data.table);library(jsonlite)
  if(!file.exists(homepage_file))stop("Daily mode needs the homepage metadata saved by the weekly run.")
  if(!file.exists(file.path(catalogue_dir,"index.json")))stop("Missing tournament catalogue; daily mode will not fall back to a broad scrape.")
  home <- fread(homepage_file,colClasses=c(player_id="character"))
  norm <- function(x)gsub("[^a-z0-9]","",tolower(iconv(x,"UTF-8","ASCII//TRANSLIT")))
  home[,name_key:=norm(player_name)]
  aliases <- if(file.exists(map_file))fread(map_file,colClasses=c(goratings_id="character")) else data.table()
  calendar <- if(file.exists(calendar_file))fread(calendar_file) else data.table(Link=character(),Date=character())
  calendar[,Date:=as.character(Date)]
  catalogue <- fromJSON(file.path(catalogue_dir,"index.json"),simplifyVector=FALSE)
  candidates <- list();active <- character();unknown <- character();undated <- character()
  scope <- if(nzchar(tournament_links))trimws(strsplit(tournament_links,",",fixed=TRUE)[[1L]]) else character()
  add <- function(p,file,date,reason) {
    if(is.null(p$name)||!nzchar(p$name)||grepl("previous match|Draw pending|Winner of|Loser of",p$name,ignore.case=TRUE))return()
    hit <- home[name_key==norm(p$name)]
    if(nrow(hit)!=1L && nrow(aliases) && all(c("historical_name","goratings_id") %in% names(aliases))) {
      ids <- unique(aliases[norm(historical_name)==norm(p$name),goratings_id]); hit <- home[player_id %in% ids]
    }
    if(nrow(hit)!=1L){unknown <<- unique(c(unknown,p$name));return()}
    candidates[[length(candidates)+1L]] <<- data.table(player_id=hit$player_id,file=file,date=date,reason=reason)
  }
  for(entry in catalogue) {
    path <- file.path(catalogue_dir,entry$file);if(!file.exists(path))next
    d <- fromJSON(path,simplifyVector=FALSE)
    link <- sub("https://gotoeveryone.k2ss.info","",d$source,fixed=TRUE)
    if(length(scope) && !link %in% scope)next
    scheduled <- calendar[Link==link & Date>=as.character(today-7) & Date<=as.character(today),Date]
    found <- FALSE
    matches <- unlist(lapply(d$brackets,function(b)b$matches),recursive=FALSE)
    for(m in matches)for(g in if(length(m$series))m$series else list(m)) {
      if(!is.null(g$winner)&&nzchar(g$winner))next
      date <- if(is.null(g$date))"" else g$date
      due <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",date) && date>=as.character(today-7) && date<=as.character(today)
      if(!due) {
        if(date=="" && length(scheduled))undated <- unique(c(undated,link))
        next
      }
      before <- length(candidates)
      for(p in g$players)add(p,entry$file,date,"unresolved scheduled match")
      found <- found || length(candidates)>before
    }
    # Never turn a calendar entry into a request for the entire entrant field.
    # Missing pairing/date coverage is left to structure refresh and weekly sweep.
    if(found)active <- unique(c(active,link))
  }
  detail <- if(length(candidates))unique(rbindlist(candidates)) else data.table(player_id=character(),file=character(),date=character(),reason=character())
  detail[,priority:=match(reason,c("unresolved scheduled match","undated match in scheduled tournament","recent tournament schedule"))]
  priority <- if(nrow(detail))detail[,.(priority=min(priority),last_scheduled=max(date)),by=player_id][order(priority,-xtfrm(last_scheduled),player_id)] else
    data.table(player_id=character(),priority=integer(),last_scheduled=character())
  if(is.na(max_profiles)||max_profiles<1L)stop("max_profiles must be positive")
  ids <- head(priority$player_id,max_profiles)
  selected <- home[match(ids,player_id),setdiff(names(home),"name_key"),with=FALSE]
  selected[,scrape_reason:="daily tournament target"]
  list(profiles=as.data.frame(selected),details=detail,links=active,
    deferred=setdiff(priority$player_id,ids),unknown=unknown,undatedTournaments=undated,date=as.character(today))
}
