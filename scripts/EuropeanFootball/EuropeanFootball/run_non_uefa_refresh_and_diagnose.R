# Long, offline review run. Fresh extraction in a separate output folder,
# followed by reconciliation with the current production master.
run_non_uefa_refresh <- function() {
  root<-normalizePath(getwd(),winslash="/",mustWork=TRUE)
  folder<-"RSSSF_World_Refresh_Review"
  out<-file.path(root,"EuropeanFootball/pipeline_data/Manual_Sources",folder)
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  envs<-c("WORLD_AUDIT_COUNTRIES","WORLD_AUDIT_MIN_YEAR","WORLD_AUDIT_MAX_YEAR",
    "WORLD_AUDIT_RESUME","WORLD_AUDIT_RECHECK_STRUCTURAL","WORLD_AUDIT_CONFEDERATIONS",
    "WORLD_AUDIT_OUTPUT_FOLDER","WORLD_AUDIT_COMPLETION_BEEP","NON_UEFA_SITUATION_INPUT_FOLDER")
  old<-Sys.getenv(envs,unset=NA_character_)
  on.exit({
    for(i in seq_along(envs)) {
      if(is.na(old[i])) Sys.unsetenv(envs[i]) else do.call(Sys.setenv,setNames(list(old[i]),envs[i]))
    }
    if(interactive() && requireNamespace("beepr",quietly=TRUE)) try(beepr::beep(),silent=TRUE)
  },add=TRUE)
  Sys.unsetenv("WORLD_AUDIT_COUNTRIES")
  Sys.setenv(WORLD_AUDIT_MIN_YEAR="2010",WORLD_AUDIT_MAX_YEAR="2025",
    WORLD_AUDIT_RESUME="0",WORLD_AUDIT_RECHECK_STRUCTURAL="0",
    WORLD_AUDIT_CONFEDERATIONS="AFC,CAF,CONCACAF,CONMEBOL,OFC",
    WORLD_AUDIT_OUTPUT_FOLDER=folder,WORLD_AUDIT_COMPLETION_BEEP="0",
    NON_UEFA_SITUATION_INPUT_FOLDER=folder)
  cat("PART 1/2: Fresh cached-page parsing, 2010-2025, all five non-UEFA confederations.\n")
  cat("Separate review output; no downloads or production changes. Progress is printed per page.\n")
  engine<-new.env(parent=globalenv())
  sys.source(file.path(root,"scripts/EuropeanFootball/rsssf_world_audit.R"),envir=engine)
  cat("\nPART 2/2: Comparing fresh domestic extraction with current master.\n")
  cat("Continental comparison uses the existing specialist continental extraction.\n")
  diagnose<-new.env(parent=globalenv())
  sys.source(file.path(root,"scripts/EuropeanFootball/audit_non_uefa_current_situation.R"),envir=diagnose)
  cat("\nBoth stages finished. Review: ",file.path(out,"current_situation"),"\n",sep="")
  cat("Do not run 02/03 yet: production data has not changed.\n")
}
run_non_uefa_refresh()
