# Fresh process for one pipeline stage; used by run_02_03_logged.ps1.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=1L || !args[1] %in% c("02","03")) stop("Specify stage 02 or 03.")
invisible(Sys.setlocale("LC_ALL","English_United Kingdom.utf8"))
suppressPackageStartupMessages(library(data.table))
setDTthreads(1L)
cat("Stage ",args[1]," started: ",format(Sys.time()),"\n",sep="")
cat("R: ",R.version.string,"; data.table threads: ",getDTthreads(),"\n",sep="")
print(gc())
script <- if(args[1]=="02") "02_calculate_elo.R" else "03_write_json.R"
source(file.path("scripts/EuropeanFootball",script))
cat("Stage ",args[1]," completed: ",format(Sys.time()),"\n",sep="")
