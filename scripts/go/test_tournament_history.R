# Short synthetic offline discovery/resume check; no website/data changes.
local({
  repo <- normalizePath(getwd(),winslash="/")
  vars <- c("GO_HISTORY_FUNCTIONS_ONLY","GO_HISTORY_OFFLINE","GO_HISTORY_START_MONTH","GO_HISTORY_END_MONTH","GO_TOURNAMENT_OUTPUT","GO_K2SS_PERMISSION_GRANTED")
  old <- Sys.getenv(vars,unset=NA_character_)
  sandbox <- tempfile("go-history-check-"); dir.create(sandbox)
  on.exit({setwd(repo); unlink(sandbox,recursive=TRUE)
    for(v in vars)if(is.na(old[[v]]))Sys.unsetenv(v) else do.call(Sys.setenv,setNames(list(old[[v]]),v))},add=TRUE)
  Sys.setenv(GO_HISTORY_FUNCTIONS_ONLY="true",GO_HISTORY_OFFLINE="true")
  Sys.setenv(GO_TOURNAMENT_OUTPUT="Go/data/tournaments/catalogue")
  Sys.unsetenv(c("GO_HISTORY_START_MONTH","GO_HISTORY_END_MONTH"))
  source("scripts/go/import_tournament_history.R")
  dir.create(file.path(sandbox,"scripts/go"),recursive=TRUE)
  for(f in c("update_tournaments.R","tournament_source_results.R","tournament_source_document.R","tournament_brackets.R","build_tournament_links.R","tournament_last_played.R"))
    file.copy(file.path("scripts/go",f),file.path(sandbox,"scripts/go",f))
  fixture <- file.path(repo,"Go/pipeline_data/tournament_format_audit/cache/_news_wr_quzhou_lanke_4_.html")
  setwd(sandbox)
  cache <- "Go/pipeline_data/tournament_format_audit/cache"
  dir.create(cache,recursive=TRUE)
  writeLines('<html><a href="/schedules/2000/">2000</a></html>',file.path(cache,"history_schedules_.html"))
  writeLines('<html></html>',file.path(cache,"history_schedules_2000_.html"))
  for(m in 1:12)writeLines('[]',file.path(cache,sprintf("2000_%02d.json",m)))
  go_import_tournament_history()
  progress <- readRDS("Go/pipeline_data/tournament_history/progress.rds")
  stopifnot(length(progress$months)==12L,length(progress$schedules)==2L)
  go_import_tournament_history()
  resumed <- readRDS("Go/pipeline_data/tournament_history/progress.rds")
  stopifnot(identical(progress,resumed))
  # Mock server errors: skip a single broken edition and stop at three in a row.
  broken <- paste0("/news/test/broken/",1:3,"/")
  progress$edition_queue <- broken
  saveRDS(progress,"Go/pipeline_data/tournament_history/progress.rds")
  Sys.setenv(GO_HISTORY_OFFLINE="false",GO_K2SS_PERMISSION_GRANTED="true")
  calls <- 0L; waits <- numeric()
  fake_download <- function(...) {
    calls <<- calls+1L
    warning("HTTP status was '500 Internal Server Error'")
    stop("mock server failure")
  }
  failure <- tryCatch(go_import_tournament_history(download=fake_download,
    wait=function(seconds)waits <<- c(waits,seconds)),error=identity)
  stopifnot(inherits(failure,"error"),grepl("Three consecutive",conditionMessage(failure)),calls==3L,
    length(waits)==2L,all(waits>0),length(readRDS("Go/pipeline_data/tournament_history/progress.rds")$server_errors)==3L)
  saveRDS(resumed,"Go/pipeline_data/tournament_history/progress.rds")
  Sys.setenv(GO_HISTORY_OFFLINE="true")
  if(file.exists(fixture)) {
    file.copy(fixture,file.path(cache,basename(fixture)))
    dir.create("Go/data",recursive=TRUE,showWarnings=FALSE)
    file.copy(file.path(repo,"Go/data/players.json"),"Go/data/players.json")
    data.table::fwrite(data.table::data.table(Date="2026-04-14",Label="Lanke",Link="/news/wr/quzhou-lanke/4/",
      Family="/news/wr/quzhou-lanke/"),"Go/pipeline_data/tournament_history/calendar_events.csv")
    go_import_tournament_history()
    payload <- jsonlite::fromJSON("Go/data/tournaments/catalogue/wr-quzhou-lanke-4-.json",simplifyVector=FALSE)
    stopifnot(length(payload$games)>0L,length(payload$brackets)>0L)
    cat("Saved edition parsed and exported in isolated temporary catalogue.\n")
  }
  cat("Historical archive discovery and resume passed offline.\n")
})
