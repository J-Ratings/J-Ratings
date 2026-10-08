# Reuse saved fixtures and local standings; preserve the previously imported
# validation files. No downloads, production writes, or new global aliases.
run_non_uefa_roster_recheck <- function() {
  old <- Sys.getenv("NON_UEFA_VALIDATION_OUTPUT",unset=NA_character_)
  on.exit(if(is.na(old)) Sys.unsetenv("NON_UEFA_VALIDATION_OUTPUT") else
    Sys.setenv(NON_UEFA_VALIDATION_OUTPUT=old),add=TRUE)
  Sys.setenv(NON_UEFA_VALIDATION_OUTPUT="roster_recheck")
  source("scripts/EuropeanFootball/validate_non_uefa_recovery_batches.R")
}
run_non_uefa_roster_recheck()
