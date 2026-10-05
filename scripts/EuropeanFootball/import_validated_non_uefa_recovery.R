# Import only the saved VALIDATED_IMPORT_PREVIEW; never reparse or infer aliases.
# Set APPLY_VALIDATED_NON_UEFA_RECOVERY = "1" to apply. Default is a dry run.
run_validated_non_uefa_import <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if (interactive() && requireNamespace("beepr", quietly=TRUE))
    try(beepr::beep(), silent=TRUE), add=TRUE)
  root <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  folder <- file.path(base, "Manual_Sources/RSSSF_World_Refresh_Review",
    Sys.getenv("NON_UEFA_IMPORT_INPUT", "batch_validation"))
  output <- file.path(folder, "import")
  dir.create(output, recursive=TRUE, showWarnings=FALSE)
  master_path <- file.path(base, "Matches_Clean_Combined/european_football_all_matches.csv")
  cat("[1/4] Reading validated preview and current master...\n")
  fresh <- fread(file.path(folder, "validated_import_preview.csv"), encoding="UTF-8")
  batches <- fread(file.path(folder, "batch_summary.csv"), encoding="UTF-8")
  master <- fread(master_path, encoding="UTF-8", showProgress=FALSE)
  columns <- names(master)
  if ("MasterRow" %in% columns && !"MasterRow" %in% names(fresh)) {
    last_id <- suppressWarnings(max(as.numeric(master$MasterRow),na.rm=TRUE))
    if (!is.finite(last_id)) last_id <- 0
    fresh[,MasterRow:=last_id+seq_len(.N)]
  }
  if (!all(columns %in% names(fresh))) stop("Preview is missing production columns.")
  if (!nrow(fresh) || anyNA(fresh$Decision) ||
      any(fresh$Decision != "VALIDATED_IMPORT_PREVIEW")) stop("Unexpected preview decisions.")
  counts <- merge(fresh[,.(PreviewRows=.N),by=.(Country,Season)],
    batches[ValidatedAdditions>0,.(Country,Season,ValidatedAdditions)],
    by=c("Country","Season"), all=TRUE)
  if (anyNA(counts) || any(counts$PreviewRows != counts$ValidatedAdditions))
    stop("Preview counts disagree with the validation summary. Revalidate first.")
  fresh[,Date:=as.IDate(Date)]
  master[,Date:=as.IDate(Date)]
  if (anyNA(fresh[,.(Country,Date,Home,Away,Score,Result,CompetitionType,Tier)]) ||
      any(!nzchar(fresh$Home) | !nzchar(fresh$Away)) ||
      any(fresh$CompetitionType != "league" | fresh$Tier != 1L) ||
      any(!grepl("^[0-9]+-[0-9]+$",fresh$Score))) stop("Invalid preview fixtures.")
  scores <- tstrsplit(fresh$Score,"-",fixed=TRUE)
  result <- fifelse(as.integer(scores[[1]])>as.integer(scores[[2]]),"1-0",
    fifelse(as.integer(scores[[1]])<as.integer(scores[[2]]),"0-1","0.5-0.5"))
  if (any(fresh$Result != result)) stop("Score and result disagree.")
  cat("[2/4] Checking approved identities, duplicates and club schedules...\n")
  cmap <- c("Hongkong"="Hong Kong","Macao"="Macau","East Timor"="Timor-Leste",
    "Congo-Brazzaville"="Congo","Congo-Kinshasa"="DR Congo","Guinea Bissau"="Guinea-Bissau",
    "French Guyana"="French Guiana","US Virgin Islands"="United States Virgin Islands",
    "Surinam"="Suriname","Fiji (clubs)"="Fiji","Fiji (districts)"="Fiji","Fiji (national)"="Fiji",
    "Vanuatu (PVFL)"="Vanuatu","Vanuatu (VFFCL)"="Vanuatu")
  country <- function(x) { h<-unname(cmap[x]); x[!is.na(h)]<-h[!is.na(h)]; x }
  aliases <- fread(file.path(base,"Reference/team_aliases.csv"),encoding="UTF-8")
  aliases[,Country:=country(Country)]
  aliases <- aliases[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,
    by=.(Country,SourceName)]
  amap <- setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  canonical <- function(c,x) {
    for (i in 1:10) { h<-unname(amap[paste(c,x,sep="\r")]); use<-!is.na(h)&h!=x
      if (!any(use)) break
      x[use]<-h[use]
    }; x
  }
  norm <- function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  identities <- function(d) data.table(Country=country(d$Country),Date=d$Date,
    H=norm(canonical(country(d$Country),d$Home)),
    A=norm(canonical(country(d$Country),d$Away)),Score=d$Score)
  dom <- master[CompetitionType=="league"]
  old <- identities(dom)
  ids <- identities(fresh)
  if (any(ids$H==ids$A)) stop("Preview contains a self fixture after approved aliases.")
  key <- function(d) paste(d$Country,d$Date,d$H,d$A,d$Score,sep="\r")
  if (anyDuplicated(key(ids))) stop("Preview contains duplicate fixtures after approved aliases.")
  present <- key(ids) %chin% key(old)
  cat("  Already present: ",sum(present),"; proposed additions: ",sum(!present),"\n",sep="")
  fresh <- fresh[!present]
  ids <- ids[!present]
  schedules <- function(d) rbindlist(list(d[,.(Country,Date,Club=H)],d[,.(Country,Date,Club=A)]))
  new_schedule <- schedules(ids)
  old_schedule <- schedules(old)
  schedule_key <- function(d) paste(d$Country,d$Date,d$Club,sep="\r")
  clashes <- new_schedule[schedule_key(new_schedule) %chin% schedule_key(old_schedule)]
  within <- new_schedule[,.(Fixtures=.N),by=.(Country,Date,Club)][Fixtures>1L]
  fwrite(clashes,file.path(output,"existing_schedule_conflicts.csv"))
  fwrite(within,file.path(output,"candidate_schedule_conflicts.csv"))
  if (nrow(clashes) || nrow(within)) stop("New club/date conflicts found; master unchanged. See import conflict reports.")
  additions <- fresh[,..columns]
  overview <- additions[,.(AddedRows=.N,FirstDate=min(Date),LastDate=max(Date)),by=.(Country,Season)]
  setorder(overview,Country,Season)
  fwrite(overview,file.path(output,"import_summary.csv"),na="")
  fwrite(additions,file.path(output,"import_preview.csv"),na="")
  cat("[3/4] Import ready: ",nrow(additions)," matches in ",nrow(overview)," country/seasons.\n",sep="")
  if (!identical(Sys.getenv("APPLY_VALIDATED_NON_UEFA_RECOVERY"),"1")) {
    cat("Dry run complete. Production master unchanged.\n")
    return(invisible(overview))
  }
  if (!nrow(additions)) { cat("All validated fixtures are already present; nothing changed.\n"); return(invisible(overview)) }
  cat("[4/4] Backing up and writing the master...\n")
  stamp <- format(Sys.time(),"%Y%m%d_%H%M%S")
  backup <- sub("[.]csv$",paste0("_before_validated_non_uefa_recovery_",stamp,".csv"),master_path)
  temporary <- paste0(master_path,".validated_recovery.tmp")
  if (!file.copy(master_path,backup,overwrite=FALSE)) stop("Backup creation failed; master unchanged.")
  combined <- rbindlist(list(master,additions),use.names=TRUE)
  if(any(grepl(intToUtf8(34),combined$Home,fixed=TRUE) |
         grepl(intToUtf8(34),combined$Away,fixed=TRUE)))
    stop("Quoted team names need normalisation before a full-master rewrite; master unchanged.")
  fwrite(combined,temporary,na="")
  check <- fread(temporary,showProgress=FALSE)
  if (nrow(check)!=nrow(master)+nrow(additions) || !identical(names(check),columns))
    stop("Temporary master verification failed; master unchanged.")
  tail_rows <- tail(check,nrow(additions))
  tail_rows[,Date:=as.IDate(Date)]
  if (!identical(key(identities(tail_rows)),key(ids))) stop("Written additions failed verification; master unchanged.")
  if (!file.copy(temporary,master_path,overwrite=TRUE)) stop("Could not install verified master. Backup: ",backup)
  unlink(temporary)
  fwrite(additions,file.path(output,paste0("applied_additions_",stamp,".csv")),na="")
  cat("Production master updated: ",nrow(master)," -> ",nrow(combined)," rows.\n",sep="")
  cat("Backup: ",backup,"\nReport: ",output,"\n",sep="")
}
run_validated_non_uefa_import()


