# Repair only oversized team fields made from repeated quotation marks.
# All fixtures, dates, scores and other fields are preserved, with a backup.
run_quote_repair <- function() {
  suppressPackageStartupMessages(library(data.table))
  root<-normalizePath(getwd(),winslash="/",mustWork=TRUE)
  path<-file.path(root,"EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
  d<-fread(path,encoding="UTF-8",showProgress=FALSE)
  q<-intToUtf8(34)
  report<-list()
  for(column in c("Home","Away")) {
    rows<-which(grepl(q,d[[column]],fixed=TRUE))
    if(!length(rows)) next
    old<-d[[column]][rows]
    fixed<-trimws(gsub(q,"",old,fixed=TRUE))
    if(any(nchar(fixed)>120L | !nzchar(fixed)))
      stop("Oversized name is not a simple quote expansion; no changes made.")
    report[[length(report)+1L]]<-data.table(Row=rows,Country=d$Country[rows],Season=d$Season[rows],
      Field=column,OldLength=nchar(old),RestoredName=fixed)
    set(d,i=rows,j=column,value=fixed)
  }
  if(!length(report)) { cat("No oversized quote expansions; master unchanged.\n");return(invisible(NULL)) }
  report<-rbindlist(report)
  stamp<-format(Sys.time(),"%Y%m%d_%H%M%S")
  backup<-sub("[.]csv$",paste0("_before_quote_repair_",stamp,".csv"),path)
  if(!file.copy(path,backup,overwrite=FALSE)) stop("Could not back up master.")
  temporary<-paste0(path,".quote_repair.tmp")
  fwrite(d,temporary,na="")
  check<-fread(temporary,encoding="UTF-8",showProgress=FALSE)
  if(nrow(check)!=nrow(d) || !identical(names(check),names(d)) ||
     !identical(check$Home,d$Home) || !identical(check$Away,d$Away))
    stop("Round-trip validation failed; production master unchanged.")
  if(!file.copy(temporary,path,overwrite=TRUE)) stop("Could not install repaired master; backup: ",backup)
  unlink(temporary)
  fwrite(report,file.path(dirname(path),paste0("quote_repair_",stamp,".csv")))
  cat("Restored ",nrow(report)," team fields across ",uniqueN(report$Row)," matches.\n",sep="")
  cat("Master rows unchanged: ",nrow(d),". Backup: ",backup,"\n",sep="")
  print(unique(report[,.(Country,RestoredName)]))
}
run_quote_repair()


