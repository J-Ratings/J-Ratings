# Short offline checks. Does not fetch source pages or run the Elo pipeline.
local({
  old <- Sys.getenv(c("GO_TOURNAMENT_FUNCTIONS_ONLY","GO_K2SS_PERMISSION_GRANTED","GO_TOURNAMENT_OFFLINE",
    "GO_TOURNAMENT_OUTPUT","GO_TOURNAMENT_STATE_DIR","GO_TOURNAMENT_CALENDAR","GO_TOURNAMENT_DATE","GO_TOURNAMENT_LINKS"),unset=NA_character_)
  on.exit(for(name in names(old))if(is.na(old[[name]]))Sys.unsetenv(name) else do.call(Sys.setenv,setNames(list(old[[name]]),name)),add=TRUE)
  Sys.setenv(GO_TOURNAMENT_FUNCTIONS_ONLY="true",GO_K2SS_PERMISSION_GRANTED="false",GO_TOURNAMENT_OFFLINE="false")
  source("scripts/go/update_tournaments.R")
  blocked <- go_update_tournaments()
  stopifnot(blocked$requests==0L,blocked$updated==0L)
  now <- as.POSIXct("2026-10-13 12:00:00",tz="UTC")
  stopifnot(go_tournament_request_wait(NULL,now)==0,
    go_tournament_request_wait(now,now)==3,
    go_tournament_request_wait(now-1,now)==2,
    go_tournament_request_wait(now-4,now)==0)
  library(data.table);library(xml2);library(jsonlite)
  source("scripts/go/tournament_source_results.R")
  cache <- "Go/pipeline_data/tournament_format_audit/cache"
  # Saved pages are optional private test inputs; never download missing fixtures.
  examples <- c("/news/cn/al/28/","/news/wr/quzhou-lanke/4/","/news/tw/umc/2/")
  calendar_path <- "Go/pipeline_data/tournament_format_audit/calendar_events.csv"
  calendar <- if(file.exists(calendar_path))fread(calendar_path) else data.table(Link=character(),Date=character())
  person <- function(name)list(name=name,id="",flag="",rating=NULL)
  for(link in examples) {
    path <- file.path(cache,paste0(gsub("[^a-zA-Z0-9]+","_",link),".html"))
    if(!file.exists(path))next
    results <- go_source_results(path,paste0("https://gotoeveryone.k2ss.info",link),
      sub("^(/news/[^/]+/[^/]+/).*","\\1",link),calendar,person)
    stopifnot(nrow(results$rows)>0L,all(results$rows$Winner %in% c(results$rows$PlayerA,results$rows$PlayerB)))
    if(link=="/news/cn/al/28/")stopifnot(nrow(results$rows)==478L)
    if(link=="/news/tw/umc/2/")stopifnot(results$rows[Stage=="Title match",.N]==6L)
    cat("Saved parser:",link,";",nrow(results$rows),"results\n")
  }
  if(file.exists(file.path(cache,"_news_wr_quzhou_lanke_4_.html"))&&file.exists(calendar_path)) {
    test_dir <- tempfile("go-tournament-update-");dir.create(test_dir)
    on.exit(unlink(test_dir,recursive=TRUE),add=TRUE)
    test_calendar <- file.path(test_dir,"calendar.csv");file.copy(calendar_path,test_calendar)
    Sys.setenv(GO_TOURNAMENT_OFFLINE="true",GO_TOURNAMENT_DATE="2026-10-13",
      GO_TOURNAMENT_LINKS="/news/wr/quzhou-lanke/4/",GO_TOURNAMENT_OUTPUT=file.path(test_dir,"catalogue"),
      GO_TOURNAMENT_STATE_DIR=file.path(test_dir,"state"),GO_TOURNAMENT_CALENDAR=test_calendar)
    updated <- go_update_tournaments()
    stopifnot(updated$requests==0L,length(updated$updated)==1L)
    edition <- fromJSON(file.path(test_dir,"catalogue/wr-quzhou-lanke-4-.json"),simplifyVector=FALSE)
    stopifnot(edition$name=="4th Quzhou-Lanke Cup",length(edition$brackets)>0L,length(edition$games)==51L)
    stopifnot(file.exists(file.path(test_dir,"catalogue/game-links.json")))
  }
  cat("Passed: permission gate, three-second spacing, available saved-page parser fixtures.\n")
})
