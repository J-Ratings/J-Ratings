# Two source-verified date corrections; preserve games and scores.
local({
  library(data.table)
  root <- normalizePath(Sys.getenv('J_RATINGS_REPO',getwd()),winslash='/',mustWork=TRUE)
  folder <- file.path(root,'EuropeanFootball/pipeline_data/Matches_Clean_Combined')
  path <- file.path(folder,'european_football_all_matches.csv')
  m <- fread(path,encoding='UTF-8'); m[,Date:=as.character(Date)]
  a <- which(m$Country=='Africa' & m$Competition=='caf_club_competitions' &
    m$Date=='2010-10-17' & m$Home=='FUS Rabat' & m$Away=='CS Sfaxien' & m$Score=='0-0' &
    grepl('Final',m$Stage))
  b <- which(m$Country=='Argentina' & m$Competition=='argentina_top_flight' &
    m$Date=='2019-10-31' & m$Home=='CA Lan\u00fas' & m$Away=='CA Boca Juniors' & m$Score=='1-2' &
    grepl('Round 1:',m$Stage,fixed=TRUE))
  if(length(a)>1L || length(b)>1L) stop('Ambiguous repair targets')
  if(!length(a) && !length(b)) {cat('Verified date corrections already applied.\n');return(invisible(NULL))}
  if(length(a)) m[a,Date:='2010-11-28']
  if(length(b)) m[b,`:=`(Date='2020-10-31',Season='2020/21')]
  tmp <- paste0(path,'.verified_date_repair.tmp')
  fwrite(m,tmp,na='')
  check <- fread(tmp,encoding='UTF-8')
  stopifnot(nrow(check)==nrow(m))
  backup <- file.path(folder,paste0('european_football_all_matches_before_verified_dates_',format(Sys.time(),'%Y%m%d_%H%M%S'),'.csv'))
  if(!file.copy(path,backup)) stop('Backup failed')
  if(!file.copy(tmp,path,overwrite=TRUE)) stop('Replacement failed; backup: ',backup)
  unlink(tmp)
  cat('Corrected',length(a)+length(b),'dates; no games removed. Backup:',backup,'\n')
})
