# Atomic directory creation lets only one process scrape from this checkout.
go_with_goratings_lock <- function(expr, root = getwd()) {
  parent <- file.path(root, "Go", "pipeline_data", "goratings")
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  path <- file.path(parent, ".scrape-lock")
  if (!dir.create(path, showWarnings = FALSE)) {
    if (!dir.exists(path)) stop("Cannot create GoRatings scrape lock: ", path)
    stop("A GoRatings scrape is already running in this checkout. No requests made. ",
         "If a previous process was killed, confirm it has stopped before removing: ", path)
  }
  on.exit({
    # On Windows, R requires recursive=TRUE even for an empty directory.
    # Check that this is still the exact lock directory and contains no files.
    if (!identical(normalizePath(path, winslash = "/", mustWork = FALSE), path) ||
        length(list.files(path, all.files = TRUE, no.. = TRUE)) ||
        unlink(path, recursive = TRUE, expand = FALSE) != 0L)
      warning("Could not remove GoRatings scrape lock: ", path)
  }, add = TRUE)
  force(expr)
}
