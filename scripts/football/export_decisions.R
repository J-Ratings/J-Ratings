# Metadata-only post-export pass. Does not read or invalidate Elo checkpoints.
enrich_football_export_decisions <- function(repo_dir, sport) {
  python <- Sys.which("python")
  if (!nzchar(python)) python <- Sys.which("python3")
  if (!nzchar(python)) stop("Python is required to export football shootout metadata.")
  script <- file.path(repo_dir, "scripts", "football", "export_decisions.py")
  status <- system2(python, c(shQuote(script), shQuote(sport)))
  if (!identical(status, 0L)) stop("Football shootout metadata export failed.")
}
