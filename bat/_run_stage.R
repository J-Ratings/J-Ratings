# Fresh R process per stage; normal .Renviron loading is retained for credentials.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[1])) stop("Specify an existing pipeline script.")
cat("Started:", format(Sys.time()), "\n")
source(args[1], local = globalenv())
cat("Finished:", format(Sys.time()), "\n")
