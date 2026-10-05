# Import the roster recheck preview through the same checked, backed-up importer.
# APPLY_VALIDATED_NON_UEFA_RECOVERY = "1" enables production writes.
run_non_uefa_roster_import <- function() {
  old <- Sys.getenv("NON_UEFA_IMPORT_INPUT",unset=NA_character_)
  on.exit(if(is.na(old)) Sys.unsetenv("NON_UEFA_IMPORT_INPUT") else
    Sys.setenv(NON_UEFA_IMPORT_INPUT=old),add=TRUE)
  Sys.setenv(NON_UEFA_IMPORT_INPUT="roster_recheck")
  source("scripts/EuropeanFootball/import_validated_non_uefa_recovery.R")
}
run_non_uefa_roster_import()
