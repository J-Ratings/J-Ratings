# Import the saved, validated grouped-stage batch with a production backup.
# APPLY_VALIDATED_NON_UEFA_RECOVERY = "1" enables writing; default is preview.
run_grouped_recovery_import <- function() {
  old<-Sys.getenv("NON_UEFA_IMPORT_INPUT",unset=NA_character_)
  on.exit(if(is.na(old)) Sys.unsetenv("NON_UEFA_IMPORT_INPUT") else
    Sys.setenv(NON_UEFA_IMPORT_INPUT=old),add=TRUE)
  Sys.setenv(NON_UEFA_IMPORT_INPUT="grouped_stage_validation")
  source("scripts/EuropeanFootball/import_validated_non_uefa_recovery.R")
}
run_grouped_recovery_import()
