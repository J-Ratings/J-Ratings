# Preview by default. GO_DAILY_APPLY=true explicitly enables requests and writes.
# Does not fetch K2SS, commit, push, or change workflow configuration.
go_run_daily_results <- function() {
  source("scripts/go/daily_tournament_targets.R")
  date <- as.Date(Sys.getenv("GO_DAILY_DATE",format(Sys.time(),tz="Europe/London",format="%Y-%m-%d")))
  target <- go_daily_targets(today=date,max_profiles=as.integer(Sys.getenv("GO_DAILY_MAX_PROFILES","80")))
  print(target$profiles[,intersect(c("player_name","player_id","scrape_reason"),names(target$profiles)),drop=FALSE])
  cat("Daily targets:",nrow(target$profiles),"profiles;",length(target$deferred),"deferred;",length(target$unknown),"unmapped names.\n")
  if(!identical(tolower(Sys.getenv("GO_DAILY_APPLY","false")),"true")) {
    cat("Preview only: no requests, pipeline writes, or rating changes.\n")
    return(invisible(target))
  }
  if(!nrow(target$profiles))return(invisible(target))
  vars <- c("GORATINGS_SCRAPE_MODE","GO_TOURNAMENT_LINK_OUTPUT")
  old <- Sys.getenv(vars,unset=NA_character_)
  on.exit(for(v in vars)if(is.na(old[[v]]))Sys.unsetenv(v) else do.call(Sys.setenv,setNames(list(old[[v]]),v)),add=TRUE)
  root <- "Go/pipeline_data/tournament_updates/daily-results"
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  jsonlite::write_json(target,file.path(root,"selection.json"),auto_unbox=TRUE,null="null",pretty=TRUE)
  Sys.setenv(GORATINGS_SCRAPE_MODE="daily")
  # Preserve the canonical observation merge, identity mapping, and Elo stages.
  # The weekly scrape later merges the same games through stage 05's deduper.
  for(script in c("04_scrape_goratings.R","05_build_games.R","06_build_player_map.R",
    "07_calculate_live_elo.R","08_build_combined_ratings.R","09_build_combined_history.R","02_write_json.R"))
    source(file.path("scripts/go",script),local=new.env(parent=globalenv()))
  source("scripts/go/reconcile_tournament_results.R")
  report <- go_reconcile_tournaments(target$links)
  Sys.setenv(GO_TOURNAMENT_LINK_OUTPUT="Go/data/tournaments/catalogue")
  source("scripts/go/build_tournament_links.R")
  jsonlite::write_json(report,file.path(root,"reconciliation.json"),auto_unbox=TRUE,null="null",pretty=TRUE)
  cat("Daily results complete. No Git operations or workflow changes.\n")
  invisible(report)
}
if(!identical(Sys.getenv("GO_DAILY_FUNCTIONS_ONLY"),"true"))go_run_daily_results()
