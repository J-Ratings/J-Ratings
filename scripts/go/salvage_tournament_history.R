# Reprocess failed/legacy editions using saved HTML only. Run from repo root.
# No permission flag required: offline is forced for the entire operation.
local({
  root <- "Go/pipeline_data/tournament_history"
  checkpoint <- file.path(root,"progress.rds")
  if(!file.exists(checkpoint))stop("Complete the historical download first.")
  library(jsonlite)
  candidates <- character()
  for(path in list.files(root,pattern="^batch-.*report\\.json$",full.names=TRUE)) {
    report <- fromJSON(path,simplifyVector=FALSE)
    for(link in names(report$review)) {
      item <- report$review[[link]]
      reason <- if(is.character(item))item else if(!is.null(item$reason))item$reason else ""
      if(grepl("Unsupported or empty|Result count fell",reason))candidates <- c(candidates,link)
    }
  }
  candidates <- unique(candidates)
  state <- readRDS(checkpoint)
  candidates <- intersect(candidates,state$editions)
  if(!length(candidates)){cat("No failed saved editions to reprocess.\n");return(invisible(NULL))}
  backup <- file.path(root,"progress-before-legacy-salvage.rds")
  if(!file.exists(backup))file.copy(checkpoint,backup)
  # Queue only once: rerunning after interruption must retain finished batches.
  plan <- file.path(root,"legacy-salvage-queue.rds")
  if(!file.exists(plan)) {
    state$built <- setdiff(state$built,candidates)
    tmp <- paste0(checkpoint,".salvage-part");saveRDS(state,tmp)
    previous <- paste0(checkpoint,".previous")
    if(file.exists(previous))unlink(previous)
    if(!file.rename(checkpoint,previous))stop("Cannot preserve checkpoint")
    if(!file.rename(tmp,checkpoint)){file.rename(previous,checkpoint);stop("Cannot save salvage queue")}
    saveRDS(candidates,plan)
  } else candidates <- readRDS(plan)
  vars <- c("GO_HISTORY_OFFLINE","GO_HISTORY_FUNCTIONS_ONLY")
  old <- Sys.getenv(vars,unset=NA_character_)
  on.exit(for(v in vars)if(is.na(old[[v]]))Sys.unsetenv(v) else do.call(Sys.setenv,setNames(list(old[[v]]),v)),add=TRUE)
  Sys.setenv(GO_HISTORY_OFFLINE="true",GO_HISTORY_FUNCTIONS_ONLY="true")
  source("scripts/go/import_tournament_history.R")
  cat("Reprocessing",length(candidates),"saved editions; no website requests.\n")
  go_import_tournament_history()
})
