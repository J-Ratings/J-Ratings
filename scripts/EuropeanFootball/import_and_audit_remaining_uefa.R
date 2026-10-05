# Import the seven reviewed league batches, then audit all other selected UEFA
# cached seasons from 2010 onwards. New audit candidates are never auto-imported.
# Run from the repository root. APPLY_VERIFIED_UEFA_RECOVERY=1 enables import.
run_import_and_audit_remaining_uefa <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive() && requireNamespace("beepr",quietly=TRUE))
    try(beepr::beep(),silent=TRUE),add=TRUE)
  root <- normalizePath(getwd(),winslash="/",mustWork=TRUE)
  base <- file.path(root,"EuropeanFootball/pipeline_data")
  review <- file.path(base,"Manual_Sources/UEFA/cached_page_recovery")
  master_path <- file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv")
  alias_path <- file.path(base,"Reference/team_aliases.csv")
  out <- file.path(base,"Manual_Sources/UEFA/full_modern_recovery")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/3] Validating the seven reviewed league batches...\n")
  g <- fread(file.path(review,"reviewed_league_candidates.csv"))
  expected <- data.table(Country=c("Belarus","Faroe Islands","Georgia","Iceland","Kazakhstan","Sweden","Ukraine"),
    Season=c(rep("2025",6),"2022/23"),Expected=c(240L,135L,180L,162L,182L,240L,240L))
  counts <- g[,.(Actual=.N),by=.(Country,Season)]
  check <- merge(expected,counts,by=c("Country","Season"),all=TRUE)
  if(anyNA(check) || any(check$Expected!=check$Actual)) stop("Reviewed batch counts changed; inspect before importing.")
  g[,Date:=as.IDate(Date)]
  if(anyNA(g$Date) || any(!g$Result %chin% c("1-0","0-1","0.5-0.5"))) stop("Invalid reviewed dates/results.")
  m <- fread(master_path,showProgress=FALSE)
  cols <- names(m)
  a <- fread(alias_path)
  # The inspected Ukraine page uses these two spellings for the same club.
  target <- a[Country=="Ukraine" & SourceName=="Zorya",unique(CanonicalName)]
  if(length(target)!=1L) stop("Expected one approved canonical name for Ukraine/Zorya.")
  existing <- a[Country=="Ukraine" & SourceName=="Zoria",unique(CanonicalName)]
  if(length(existing) && any(existing!=target)) stop("Existing Zoria alias conflicts with reviewed mapping.")
  a <- unique(rbindlist(list(a,data.table(Country="Ukraine",SourceName="Zoria",CanonicalName=target)),fill=TRUE))
  safe <- a[,if(uniqueN(CanonicalName)==1L) .(CanonicalName=CanonicalName[1]) else NULL,by=.(Country,SourceName)]
  amap <- setNames(safe$CanonicalName,paste(safe$Country,safe$SourceName,sep="\r"))
  canonical <- function(country,x) {
    for(i in 1:10) { hit <- unname(amap[paste(country,x,sep="\r")]); use <- !is.na(hit)&hit!=x
      if(!any(use)) break; x[use]<-hit[use] }
    x
  }
  norm <- function(x) gsub("[^a-z0-9]","",tolower(stringi::stri_trans_general(x,"Latin-ASCII")))
  key <- function(d,score=TRUE) paste(d$Country,as.IDate(d$Date),norm(canonical(d$Country,d$Home)),
    norm(canonical(d$Country,d$Away)),if(score) d$Score else "",sep="\r")
  g[,`:=`(Home=canonical(Country,Home),Away=canonical(Country,Away))]
  if(any(g$Home==g$Away) || anyDuplicated(key(g))) stop("Duplicate/self fixtures in reviewed candidates.")
  domestic <- m[CompetitionType=="league" & Tier==1L & Country %in% expected$Country]
  hit <- match(key(g,FALSE),key(domestic,FALSE))
  if(any(!is.na(hit) & g$Score!=domestic$Score[hit])) stop("Candidate conflicts with an existing score.")
  add <- g[!key(g) %chin% key(domestic)]
  # Do not introduce an additional league fixture for a club on the same day.
  appearances <- function(d) c(paste(d$Country,as.IDate(d$Date),norm(canonical(d$Country,d$Home)),sep="\r"),
    paste(d$Country,as.IDate(d$Date),norm(canonical(d$Country,d$Away)),sep="\r"))
  if(any(appearances(add) %chin% appearances(domestic)) || anyDuplicated(appearances(add)))
    stop("Potential same-day double booking; inspect before importing.")
  # Preserve the production competition identifiers and labels for each league.
  metadata <- domestic[, .SD[which.max(as.integer(as.IDate(Date)))],by=Country,
                       .SDcols=c("Competition","League","Date")]
  if(!all(add$Country %in% metadata$Country)) stop("Missing production league metadata.")
  rows <- copy(add)
  for(country in unique(rows$Country)) {
    meta <- metadata[Country==country]
    rows[Country==country,`:=`(Competition=meta$Competition[1],League=meta$League[1])]
  }
  rows[,`:=`(CompetitionType="league",Tier=1L,Source="rsssf",DateApprox=FALSE,
             HomeAssociation=Country,AwayAssociation=Country)]
  for(col in setdiff(cols,names(rows))) rows[,(col):=NA]
  rows <- rows[,..cols]
  print(rows[,.(Additions=.N),by=.(Country,Season)])
  cat("[2/3] ",nrow(rows)," validated additions.\n",sep="")
  if(identical(Sys.getenv("APPLY_VERIFIED_UEFA_RECOVERY"),"1")) {
    stamp <- format(Sys.time(),"%Y%m%d_%H%M%S")
    if(nrow(rows)) {
      combined <- rbindlist(list(m,rows),use.names=TRUE)
      backup <- sub("[.]csv$",paste0("_before_verified_uefa_recovery_",stamp,".csv"),master_path)
      alias_backup <- sub("[.]csv$",paste0("_before_verified_uefa_recovery_",stamp,".csv"),alias_path)
      if(!file.copy(master_path,backup) || !file.copy(alias_path,alias_backup)) stop("Backup failed.")
      temp <- paste0(master_path,".verified.tmp")
      fwrite(combined,temp,na="")
      if(nrow(fread(temp,select="Country",showProgress=FALSE))!=nrow(combined)) stop("Temporary master validation failed.")
      # Alias write precedes master so interruption cannot leave imported variants
      # without their approved mapping. Both originals have backups above.
      fwrite(a,alias_path,na="")
      if(!file.copy(temp,master_path,overwrite=TRUE)) stop("Could not install validated master.")
      unlink(temp)
      fwrite(rows,file.path(out,paste0("imported_matches_",stamp,".csv")),na="")
      cat("Master updated: ",nrow(m)," -> ",nrow(combined),". Backup: ",backup,"\n",sep="")
    } else cat("Verified matches already present; import skipped.\n")
  } else cat("Preview: set APPLY_VERIFIED_UEFA_RECOVERY=1 to import the reviewed batches.\n")
  if(identical(Sys.getenv("RUN_UEFA_FULL_AUDIT"),"0")) {
    cat("Full audit skipped by RUN_UEFA_FULL_AUDIT=0.\n")
    return(invisible(NULL))
  }
  cat("[3/3] Auditing every cached selected UEFA season from 2010 onwards...\n")
  manifest <- fread(file.path(base,"Manual_Sources/UEFA/online_page_discovery/season_page_inventory.csv"))
  manifest <- manifest[LocalCached==TRUE & !Country %chin% c("England","France","Germany","Italy","Portugal")]
  fwrite(manifest,file.path(out,"audit_manifest.csv"))
  old <- Sys.getenv("UEFA_RECOVERY_FUNCTIONS_ONLY",unset=NA_character_)
  on.exit(if(is.na(old)) Sys.unsetenv("UEFA_RECOVERY_FUNCTIONS_ONLY") else
    Sys.setenv(UEFA_RECOVERY_FUNCTIONS_ONLY=old),add=TRUE)
  Sys.setenv(UEFA_RECOVERY_FUNCTIONS_ONLY="1")
  engine <- new.env(parent=globalenv())
  sys.source(file.path(root,"scripts/EuropeanFootball/audit_uefa_cached_recovery_pages.R"),envir=engine)
  engine$run_uefa_cached_recovery(manifest_path=file.path(out,"audit_manifest.csv"),
                                 output_folder="UEFA/full_modern_recovery",completion_beep=FALSE)
  cat("Finished. Inspect full_modern_recovery/season_comparison.csv before importing further candidates.\n")
}
run_import_and_audit_remaining_uefa()
