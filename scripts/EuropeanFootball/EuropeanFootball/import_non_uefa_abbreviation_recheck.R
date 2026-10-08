# Import only the saved abbreviation recheck fixtures that passed validation.
# Set APPLY_VALIDATED_NON_UEFA_RECOVERY = "1" to apply; default is a dry run.
run_non_uefa_abbreviation_import <- function() {
  old <- Sys.getenv("NON_UEFA_IMPORT_INPUT", unset=NA_character_)
  on.exit(if(is.na(old)) Sys.unsetenv("NON_UEFA_IMPORT_INPUT") else
    Sys.setenv(NON_UEFA_IMPORT_INPUT=old), add=TRUE)
  Sys.setenv(NON_UEFA_IMPORT_INPUT="abbreviation_recheck")
  source("scripts/EuropeanFootball/import_validated_non_uefa_recovery.R")
}
run_non_uefa_abbreviation_import()
