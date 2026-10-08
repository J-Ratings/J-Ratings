# Applies only a saved validation preview whose inputs have not changed.
# Default dry run. To apply: Sys.setenv(APPLY_2025_26_RECOVERY="1").
import_target_fixtures <- function() {
  suppressPackageStartupMessages(library(data.table))
  on.exit(if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE),add=TRUE)
  folder<-"EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26/import_validation"
  cat("[1/4] Checking validation fingerprints...\n")
  manifest<-fread(file.path(folder,"validation_manifest.csv"))
  if(any(!file.exists(manifest$Path))||any(unname(tools::md5sum(manifest$Path))!=manifest$MD5))
    stop("Validation inputs changed. Run validate_2025_26_matched_fixtures.R again before importing.")
  master_path<-manifest$Path[1]
  m<-fread(master_path,encoding="UTF-8",showProgress=FALSE)
  p<-fread(file.path(folder,"validated_import_preview.csv"),encoding="UTF-8")
  if(!nrow(p)||any(p$Validation!="VALIDATED_IMPORT_PREVIEW"))stop("Preview is empty or contains held rows")
  schema<-names(m)
  if(!all(schema %in% names(p)))stop("Master schema differs from preview")
  if(anyNA(p[,.(Country,Season,Date,Home,Away,Score,Result,Competition,Tier)])||
    any(nchar(p$Home)>200L|nchar(p$Away)>200L|p$Home==p$Away)||
    any(p$CompetitionType!="league"|p$Tier!=1L))stop("Invalid import preview")
  last<-suppressWarnings(max(as.numeric(m$MasterRow),na.rm=TRUE));if(!is.finite(last))last<-0
  p[,MasterRow:=last+seq_len(.N)]
  add<-p[,schema,with=FALSE]
  cat("[2/4] Validated additions:",nrow(add),"in",uniqueN(paste(add$Country,add$Season)),"country/seasons.\n")
  if(Sys.getenv("APPLY_2025_26_RECOVERY","0")!="1") {
    cat("Dry run only. Set APPLY_2025_26_RECOVERY=1 to import. Master unchanged.\n");return(invisible(p))
  }
  cat("[3/4] Writing and verifying temporary master...\n")
  tmp<-paste0(master_path,".2025_26_tmp.csv")
  fwrite(rbindlist(list(m,add),use.names=TRUE),tmp,na="",quote="auto")
  checked<-fread(tmp,encoding="UTF-8",showProgress=FALSE)
  if(nrow(checked)!=nrow(m)+nrow(add)||!identical(names(checked),schema))stop("Temporary master verification failed; production untouched")
  expected<-add[,.(Home,Away,Score,Date,Competition)]
  actual<-tail(checked,nrow(add))[,.(Home,Away,Score,Date,Competition)]
  for(col in names(expected))if(!identical(as.character(expected[[col]]),as.character(actual[[col]])))
    stop("Round-trip verification failed for ",col,"; production untouched")
  cat("[4/4] Backing up and replacing master...\n")
  backup<-sub("[.]csv$",paste0("_before_2025_26_recovery_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv"),master_path)
  if(!file.copy(master_path,backup,overwrite=FALSE))stop("Backup failed; production untouched")
  # Windows can reject overwriting an existing large file. Move the original
  # aside before installing the verified file, restoring it if installation fails.
  # Give each run its own retained original. Failed cleanup of an older run
  # must not block a fresh import or cause that recovery file to be overwritten.
  displaced<-tempfile(pattern="retained_master_",tmpdir=dirname(master_path),fileext=".csv")
  if(!file.rename(master_path,displaced))
    stop("Cannot move master aside. Close applications holding the CSV and retry. Production unchanged. Backup: ",backup)
  if(!file.rename(tmp,master_path)) {
    restored<-file.rename(displaced,master_path)
    stop("Cannot install verified master. Original restored: ",restored,". Backup: ",backup,
      "; original retained at: ",displaced)
  }
  if(unlink(displaced)!=0L||file.exists(displaced))
    cat("Original retained because cleanup failed:",displaced,"\n")
  fwrite(p,file.path(folder,"applied_additions.csv"))
  cat("Production master updated:",nrow(m),"->",nrow(checked),"rows.\nBackup:",normalizePath(backup,winslash="/"),"\n")
  cat("More held batches remain; do not run 02/03 until recovery review is finished.\n")
}
import_target_fixtures()
