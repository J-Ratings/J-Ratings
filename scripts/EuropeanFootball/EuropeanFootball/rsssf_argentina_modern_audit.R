# Rebuild modern Argentine top-flight candidates from existing cached RSSSF pages.
# No downloads and no production changes.

root <- normalizePath(Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
                      winslash = "/", mustWork = TRUE)

old_env <- Sys.getenv(c("ARGENTINA_AUDIT_MIN_YEAR", "ARGENTINA_AUDIT_MAX_YEAR",
                        "ARGENTINA_AUDIT_COUNTRIES", "ARGENTINA_AUDIT_RESUME"), unset = NA_character_)
on.exit({
  keys <- names(old_env)
  for (i in seq_along(keys)) {
    if (is.na(old_env[[i]])) Sys.unsetenv(keys[[i]]) else do.call(Sys.setenv, setNames(list(old_env[[i]]), keys[[i]]))
  }
}, add = TRUE)

Sys.setenv(
  ARGENTINA_AUDIT_MIN_YEAR = "2010",
  ARGENTINA_AUDIT_MAX_YEAR = "2025",
  ARGENTINA_AUDIT_COUNTRIES = "Argentina",
  ARGENTINA_AUDIT_RESUME = "0",
  RSSSF_OFC_FUNCTIONS_ONLY = "1"
)
source(file.path(root, "scripts/EuropeanFootball/rsssf_ofc_audit.R"), encoding = "UTF-8")
Sys.unsetenv("RSSSF_OFC_FUNCTIONS_ONLY")

cfg <- data.table::data.table(
  Country = "Argentina",
  Confederation = "CONMEBOL",
  Directory = "tablesa",
  FilePattern = "^arg20(?:1[0-9]|2[0-6])[a-z]?[.]html$",
  Competition = "Argentina Top Flight",
  CompetitionType = "league"
)

run_rsssf_ofc_audit(
  config_override = cfg,
  audit_name = "Argentina modern",
  output_folder = "RSSSF_Argentina_Modern_Audit",
  env_prefix = "ARGENTINA_AUDIT"
)
