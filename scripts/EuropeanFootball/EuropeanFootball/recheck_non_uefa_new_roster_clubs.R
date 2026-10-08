# Review-only admission of source clubs absent from the existing master.
# Requires unique roster membership, a passing season structure, at least four
# dates, and no close existing-name evidence. No aliases or master writes.
run_new_roster_club_recheck <- function() {
  variables<-c("NON_UEFA_VALIDATION_OUTPUT","NON_UEFA_ALLOW_ROSTER_NEW_CLUBS")
  old<-Sys.getenv(variables,unset=NA_character_)
  on.exit({
    for(i in seq_along(variables)) {
      if(is.na(old[i])) Sys.unsetenv(variables[i]) else
        do.call(Sys.setenv,setNames(list(old[i]),variables[i]))
    }
  },add=TRUE)
  Sys.setenv(NON_UEFA_VALIDATION_OUTPUT="new_roster_club_recheck",NON_UEFA_ALLOW_ROSTER_NEW_CLUBS="1")
  source("scripts/EuropeanFootball/validate_non_uefa_recovery_batches.R")
}
run_new_roster_club_recheck()
