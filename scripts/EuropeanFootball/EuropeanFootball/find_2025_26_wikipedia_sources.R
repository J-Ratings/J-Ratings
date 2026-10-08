# Find season-page candidates for the already extracted dated archives.
# Search results are suggestions, not proof of correct competition or completeness.
find_target_wikipedia <- function() {
  suppressPackageStartupMessages({library(data.table);library(jsonlite)})
  started<-Sys.time()
  delay<-suppressWarnings(as.numeric(Sys.getenv("WIKIPEDIA_SEARCH_DELAY","10")))
  if(!is.finite(delay)||delay<1.5)stop("WIKIPEDIA_SEARCH_DELAY must be at least 1.5 seconds")
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  base<-"EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26"
  out<-file.path(base,"wikipedia_discovery");cache<-file.path(out,"cache")
  dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  checks<-fread(file.path(base,"source_discovery/dated_source_checks.csv"))
  queue<-fread(file.path(base,"source_discovery/recovery_queue.csv"))
  units<-unique(checks[Status=="DATED_RESULTS_EXTRACTABLE_NOT_YET_MATCHED",.(Country,TargetSeason)])
  units<-merge(units,queue[Tier==1L,.(Country,LeagueLabels,SeasonStyle)],by="Country",all.x=TRUE)
  # Split annual stages (Apertura/Clausura), playoffs and regional leagues must
  # be checked against the complete season page rather than assumed equivalent.
  records<-list();failures<-list();blocked<-FALSE;consecutive_failures<-0L;new_requests<-0L
  cat("Finding Wikipedia season candidates for",nrow(units),"countries...\n")
  for(i in seq_len(nrow(units))) {
    c<-units[i]
    season<-if(c$SeasonStyle=="calendar")"2025"else "2025\u201326"
    labels<-trimws(strsplit(c$LeagueLabels,";",fixed=TRUE)[[1]])
    label<-if(length(labels)&&nzchar(labels[1]))labels[1]else paste(c$Country,"football league")
    query<-paste(season,label)
    path<-file.path(cache,paste0(gsub("[^A-Za-z0-9]","_",c$Country),".json"))
    cat(sprintf("  [%d/%d] %s (%s)\n",i,nrow(units),c$Country,season))
    download_warnings<-character()
    tryCatch({
      if(!file.exists(path)) {
        Sys.sleep(delay)
        new_requests<-new_requests+1L
        url<-paste0("https://en.wikipedia.org/w/api.php?action=query&list=search&format=json&maxlag=5&srlimit=5&srsearch=",URLencode(query,reserved=TRUE))
        old<-options(timeout=30)
        tryCatch(withCallingHandlers(
          download.file(url,paste0(path,".part"),mode="wb",quiet=TRUE,method="libcurl",
            headers=c("User-Agent"="J-Ratings-Season-Audit/1.0 (local football data research)")),
          warning=function(w){download_warnings<<-c(download_warnings,conditionMessage(w));invokeRestart("muffleWarning")}),finally=options(old))
        response<-fromJSON(paste0(path,".part"))
        if(!is.null(response$error))stop(paste(response$error$code,response$error$info))
        if(is.null(response$query$search))stop("Wikipedia search response unavailable")
        if(!file.rename(paste0(path,".part"),path))stop("Could not save search cache")
      }
      response<-fromJSON(path)
      hits<-as.data.table(response$query$search)
      if(!nrow(hits))stop("No search candidates")
      consecutive_failures<-0L
      for(j in seq_len(nrow(hits))) {
        title<-hits$title[j]
        # Only flags: actual page content and club rosters still need checking.
        has_target<-if(c$SeasonStyle=="calendar")grepl("2025",title,fixed=TRUE)&&!grepl("2026",title,fixed=TRUE)else grepl("2025[\u2013-](26|2026)",title)
        records[[length(records)+1L]]<-data.table(Country=c$Country,TargetSeason=c$TargetSeason,
          Query=query,Rank=j,Title=title,URL=paste0("https://en.wikipedia.org/wiki/",URLencode(gsub(" ","_",title,fixed=TRUE),reserved=TRUE)),
          TitleContainsTargetSeason=has_target,Decision="PAGE_SCOPE_AND_RESULT_TABLE_REVIEW")
      }
    },error=function(e){
      detail<-paste(c(conditionMessage(e),download_warnings),collapse=" | ")
      failures[[length(failures)+1L]]<<-data.table(Country=c$Country,Query=query,Error=detail)
      consecutive_failures<<-consecutive_failures+1L
      cat("    Search held:",detail,"\n")
      if(grepl("403|429|maxlag|rate.limit",detail,ignore.case=TRUE)||consecutive_failures>=3L)blocked<<-TRUE
      if(file.exists(paste0(path,".part")))unlink(paste0(path,".part"))
    })
    if(length(records))fwrite(rbindlist(records),file.path(out,"season_page_candidates.csv"))
    if(length(failures))fwrite(rbindlist(failures),file.path(out,"search_errors.csv"))
    if(blocked){cat("Access restriction or repeated search failures; stopping and retaining cached successes.\n");break}
  }
  if(!length(failures))fwrite(data.table(Country=character(),Query=character(),Error=character()),file.path(out,"search_errors.csv"))
  cat("New requests:",new_requests,"(successful cached searches were reused).\n")
  cat("Countries searched:",i,"; candidate pages:",length(records),"; search errors:",length(failures),"\n")
  cat("No fixtures imported or aliases changed. Report:",normalizePath(out,winslash="/"),"\n")
}
find_target_wikipedia()
