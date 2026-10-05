# Restore metadata discarded by older 01 versions, without replacing current games.
local({
  library(data.table)
  root <- normalizePath(Sys.getenv('J_RATINGS_REPO',getwd()),winslash='/',mustWork=TRUE)
  folder <- file.path(root,'EuropeanFootball/pipeline_data/Matches_Clean_Combined')
  path <- file.path(folder,'european_football_all_matches.csv')
  candidates <- list.files(folder,pattern='before_verified_dates_.*[.]csv$',full.names=TRUE)
  if(!length(candidates)) stop('No metadata-bearing backup found')
  backup_source <- tail(sort(candidates),1)
  fields <- c('Country','Competition','Date','Home','Away','Score')
  info <- names(fread(backup_source,nrows=0))
  metadata <- intersect(c('HomeAssociation','AwayAssociation','SourcePage','SourceFile','Stage','DateApprox'),info)
  if(!all(c('HomeAssociation','AwayAssociation') %in% metadata)) stop('Backup lacks associations')
  message('[1/3] Reading associations and provenance from ',basename(backup_source))
  old <- fread(backup_source,select=c(fields,metadata),encoding='UTF-8')
  old <- old[!duplicated(old,by=fields)]
  message('[2/3] Restoring matching current rows...')
  m <- fread(path,encoding='UTF-8')
  for(nm in metadata) {
    if(!nm %in% names(m)) m[, (nm):=old[[nm]][NA_integer_]]
    # Join only matching fixtures; never import backup games or alter scores.
    m[old,on=fields,(nm):=get(paste0('i.',nm))]
  }
  cwc <- m[Competition=='fifa_club_world_cup' & Date>='2025-01-01' & Date<'2026-01-01']
  stopifnot(nrow(cwc)==63L,all(!is.na(cwc$HomeAssociation) & nzchar(cwc$HomeAssociation)),
    all(!is.na(cwc$AwayAssociation) & nzchar(cwc$AwayAssociation)))
  message('[3/3] Backing up and writing restored master...')
  backup <- file.path(folder,paste0('european_football_all_matches_before_metadata_restore_',format(Sys.time(),'%Y%m%d_%H%M%S'),'.csv'))
  if(!file.copy(path,backup)) stop('Backup failed')
  tmp <- paste0(path,'.metadata_restore.tmp')
  fwrite(m,tmp,na='')
  if(!file.copy(tmp,path,overwrite=TRUE)) stop('Replacement failed; backup: ',backup)
  unlink(tmp)
  cat('Metadata restored; all 63 Club World Cup rows have participant associations. Backup:',backup,'\n')
  if(interactive()) try(beepr::beep(),silent=TRUE)
})
