# Recheck only material batches whose previous standings read failed.
# Saved fixtures + local HTML; no reparsing of fixtures or production changes.
run_failed_standings_recheck <- function() {
  variables<-c("NON_UEFA_VALIDATION_OUTPUT","NON_UEFA_ALLOW_ROSTER_NEW_CLUBS",
    "NON_UEFA_FAILED_STANDINGS_ONLY")
  old<-Sys.getenv(variables,unset=NA_character_)
  on.exit({for(i in seq_along(variables)) {
    if(is.na(old[i])) Sys.unsetenv(variables[i]) else
      do.call(Sys.setenv,setNames(list(old[i]),variables[i]))
  }},add=TRUE)
  Sys.setenv(NON_UEFA_VALIDATION_OUTPUT="failed_standings_recheck",
    NON_UEFA_ALLOW_ROSTER_NEW_CLUBS="1",NON_UEFA_FAILED_STANDINGS_ONLY="1")
  source("scripts/EuropeanFootball/validate_non_uefa_recovery_batches.R")
}
run_failed_standings_recheck()
