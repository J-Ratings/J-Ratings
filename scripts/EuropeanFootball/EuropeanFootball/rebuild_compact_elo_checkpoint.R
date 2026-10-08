# Reuse completed calculations; do not run historical Elo again.
local({
  library(data.table)
  root<-normalizePath(Sys.getenv('J_RATINGS_REPO',getwd()),winslash='/',mustWork=TRUE)
  folder<-file.path(root,'EuropeanFootball/pipeline_data/Elo')
  cutoff<-as.Date('2024-12-31')
  cat('[1/3] Reading completed pass histories...\n')
  a<-fread(file.path(folder,'football_elo_game_history_pass1.csv'),encoding='UTF-8')
  b<-fread(file.path(folder,'football_elo_game_history.csv'),encoding='UTF-8')
  a[,Date:=as.Date(Date)];b[,Date:=as.Date(Date)]
  state_from_history<-function(x) {
    x<-x[Date<=cutoff]
    apps<-rbindlist(list(
      x[,.(key=HomeKey,rating=HomeRating_After,games=HomeGamesAfter,league=League,tier=Tier,date=Date,entry=HomeStartRating)],
      x[,.(key=AwayKey,rating=AwayRating_After,games=AwayGamesAfter,league=League,tier=Tier,date=Date,entry=AwayStartRating)]))
    setorder(apps,key,games)
    first<-apps[!duplicated(key)];last<-apps[!duplicated(key,fromLast=TRUE)]
    named<-function(values,keys)setNames(as.list(values),keys)
    list(ratings=named(last$rating,last$key),games=named(last$games,last$key),
      first_league=named(first$league,first$key),first_tier=named(first$tier,first$key),
      first_date=named(first$date,first$key),entry_rating=named(first$entry,first$key))
  }
  cat('[2/3] Recovering cutoff state from recorded ratings and game counts...\n')
  s1<-state_from_history(a);s2<-state_from_history(b)
  checkpoint<-list(version=1L,cutoff=cutoff,created=Sys.time(),historical_input=NULL,
    pass1_state=s1,pass2_state=s2,pass1_history=a[Date<=cutoff],pass2_history=b[Date<=cutoff],
    frozen_retro_map=unlist(s2$entry_rating, use.names=TRUE))
  path<-file.path(folder,'checkpoint_2024_12_31.rds');tmp<-paste0(path,'.compact.tmp')
  saveRDS(checkpoint,tmp,compress=FALSE)
  stopifnot(length(checkpoint$pass1_state$ratings)>0,identical(names(s1$ratings),names(s2$ratings)))
  cat('[3/3] Archiving oversized checkpoint and installing compact one...\n')
  archive<-paste0(path,'.before_compaction_',format(Sys.time(),'%Y%m%d_%H%M%S'))
  if(file.exists(path) && !file.rename(path,archive))stop('Cannot archive checkpoint')
  if(!file.rename(tmp,path)) {if(file.exists(archive))file.rename(archive,path);stop('Cannot install checkpoint')}
  export_path<-file.path(folder,'json_export_checkpoint_2024_12_31.rds')
  if(file.exists(export_path)) {
    export<-readRDS(export_path)
    if(identical(export$version,1L)) {
      export$elo_hash<-unname(tools::md5sum(path))
      saveRDS(export,export_path,compress=FALSE)
      cat('Existing 03 historical cache retained.\n')
    }
  }
  cat('Compact checkpoint saved. Historical ratings preserved from completed CSVs; no Elo recalculation.\n')
  if(interactive())try(beepr::beep(),silent=TRUE)
})
