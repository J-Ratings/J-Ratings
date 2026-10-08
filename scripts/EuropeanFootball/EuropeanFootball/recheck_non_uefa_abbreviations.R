# Check saved material batches with expanded season-local abbreviation evidence.
# Does not change aliases or the production master; leaves prior previews intact.
run_non_uefa_abbreviation_recheck <- function() {
  old<-Sys.getenv("NON_UEFA_VALIDATION_OUTPUT",unset=NA_character_)
  on.exit(if(is.na(old)) Sys.unsetenv("NON_UEFA_VALIDATION_OUTPUT") else
    Sys.setenv(NON_UEFA_VALIDATION_OUTPUT=old),add=TRUE)
  Sys.setenv(NON_UEFA_VALIDATION_OUTPUT="abbreviation_recheck")
  source("scripts/EuropeanFootball/validate_non_uefa_recovery_batches.R")
}
run_non_uefa_abbreviation_recheck()
