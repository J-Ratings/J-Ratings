# Source discovery only: sequential cached public pages, no imports or aliases.
check_target_sources <- function() {
  suppressPackageStartupMessages({library(data.table);library(xml2);library(rvest)})
  started <- Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  out <- "EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26/source_discovery"
  cache <- file.path(out,"cache");dir.create(cache,recursive=TRUE,showWarnings=FALSE)
  queue <- fread(file.path(out,"recovery_queue.csv"))
  delay <- suppressWarnings(as.numeric(Sys.getenv("SEASON_SOURCE_DELAY", "1.5")))
  if(!is.finite(delay))stop("SEASON_SOURCE_DELAY must be numeric")
  delay <- max(1,delay)
  old_options <- options(timeout=30);on.exit(options(old_options),add=TRUE)
  blocked <- FALSE; requests <- 0L
  fetch <- function(url,name) {
    path <- file.path(cache,paste0(name,".html"))
    if(file.exists(path)&&file.info(path)$size>1000) return(read_html(path))
    if(blocked) stop("Host blocked this run; stopping requests rather than retrying")
    Sys.sleep(delay);requests <<- requests+1L
    tmp <- paste0(path,".part")
    tryCatch({download.file(url,tmp,quiet=TRUE,mode="wb",method="libcurl")
      doc <- read_html(tmp)
      if(grepl("Access Denied|Too Many Requests|captcha",xml_text(doc),ignore.case=TRUE)) {
        blocked <<- TRUE;stop("Access restriction; host requests stopped")
      }
      if(!file.rename(tmp,path))stop("Could not save source cache")
      doc
    },error=function(e){if(file.exists(tmp))unlink(tmp)
      if(grepl("403|429|Access restriction",conditionMessage(e)))blocked <<- TRUE
      stop(conditionMessage(e))})
  }
  cat("[1/3] Discovering competition links from public directories...\n")
  directories <- c("europa","asien","afrika","amerika","ozeanien")
  found <- list(); errors <- list()
  for(region in directories) for(page in 1:4) {
    if(blocked)break
    cat("  Directory:",region,"page",page,"\n")
    name <- if(page==1L)region else paste0(region,"_",page)
    url <- paste0("https://www.transfermarkt.co.uk/wettbewerbe/",region,if(page>1L)paste0("?page=",page)else "")
    doc <- tryCatch(fetch(url,name),error=function(e){errors[[length(errors)+1L]]<<-data.table(URL=url,Error=conditionMessage(e));NULL})
    if(is.null(doc))next
    rows <- xml_find_all(doc,"//tr[.//img[contains(@class,'flaggenrahmen')] and .//a[contains(@href,'/startseite/wettbewerb/')]]")
    if(!length(rows))break
    for(row in rows) {
      link <- xml_find_first(row,".//a[contains(@href,'/startseite/wettbewerb/') and @title]")
      if(is.na(xml_attr(link,"href")))next
      country <- xml_attr(xml_find_first(row,".//img[contains(@class,'flaggenrahmen')]"),"alt")
      country <- switch(country,"Ireland"="Republic of Ireland","Korea, South"="South Korea",
        "United States of America"="United States","Czech Republic"="Czechia",country)
      found[[length(found)+1L]] <- data.table(Country=country,CompetitionLabel=xml_attr(link,"title"),
        Path=xml_attr(link,"href"),DirectoryURL=url)
    }
    # Directory search is bounded; countries not listed remain source research.
    more <- xml_find_all(doc,paste0("//a[contains(@href,'page=",page+1L,"') and contains(@class,'pagination') ]"))
    if(!length(more))break
  }
  catalog <- if(length(found))unique(rbindlist(found),by="Path")else data.table(Country=character(),CompetitionLabel=character(),Path=character(),DirectoryURL=character())
  fwrite(catalog,file.path(out,"discovered_competitions.csv"))
  cat("[2/3] Checking season selectors and dated played results...\n")
  # Only top-flight units with an established season convention are selected
  # automatically. Multiple competitions per country stay explicit candidates.
  candidates <- merge(catalog,queue[Tier==1L&SeasonStyle %in% c("calendar","split"),
    .(Country,Tier,SeasonStyle,TargetSeason,TargetCompletedMatches)],by="Country",allow.cartesian=TRUE)
  results <- list()
  for(i in seq_len(nrow(candidates))) {
    c <- candidates[i];code <- sub(".*/wettbewerb/","",c$Path)
    cat(sprintf("  [%d/%d] %s: %s\n",i,nrow(candidates),c$Country,c$CompetitionLabel))
    result <- data.table(c,SourceURL="",SelectedLabel="",DatedPlayedResults=0L,
      FirstDate="",LastDate="",Status="SOURCE_REVIEW",Error="")
    tryCatch({
      home <- fetch(paste0("https://www.transfermarkt.co.uk",c$Path),paste0("competition_",code))
      home_text <- gsub("[[:space:]\u00a0]+"," ",xml_text(home))
      if(!grepl("League level: First Tier",home_text,fixed=TRUE))stop("Candidate is not confirmed as a first-tier league")
      expected <- if(c$SeasonStyle=="calendar")"2025"else "25/26"
      opts <- xml_find_all(home,"//select[@name='saison_id']/option")
      labels <- trimws(xml_text(opts));hit <- which(labels==expected)
      if(length(hit)!=1L)stop("Target season selector not uniquely available")
      season_id <- xml_attr(opts[[hit]],"value");result$SelectedLabel <- labels[hit]
      url <- paste0("https://www.transfermarkt.co.uk",sub("/startseite/","/gesamtspielplan/",c$Path),"/saison_id/",season_id)
      result$SourceURL <- url
      doc <- fetch(url,paste0("schedule_",code,"_",season_id))
      selected <- xml_text(xml_find_first(doc,"//select[@name='saison_id']/option[@selected]"))
      if(is.na(selected)||trimws(selected)!=expected)stop("Downloaded schedule season label differs from target")
      text <- gsub("[[:space:]\u00a0]+"," ",xml_text(doc))
      if(!grepl("League level: First Tier",text,fixed=TRUE))stop("First-tier status not confirmed on schedule")
      fixtures <- list()
      for(tab in xml_find_all(doc,"//table[.//a[contains(@class,'ergebnis-link')]]")) {
        date <- NA_character_
        for(row in xml_find_all(tab,".//tbody/tr")) {
          dl <- xml_attr(xml_find_first(row,".//a[contains(@href,'/datum/')]"),"href")
          if(!is.na(dl))date <- sub(".*/datum/","",dl)
          score <- trimws(xml_text(xml_find_first(row,".//a[contains(@class,'ergebnis-link')]")))
          if(!is.na(score)&&grepl("^[0-9]+:[0-9]+$",score))fixtures[[length(fixtures)+1L]]<-data.table(Date=date,Score=score)
        }
      }
      f <- if(length(fixtures))rbindlist(fixtures)else data.table(Date=character(),Score=character())
      dates <- as.Date(f$Date)
      lower <- as.Date("2025-01-01");upper<-if(c$SeasonStyle=="calendar")as.Date("2025-12-31")else as.Date("2026-12-31")
      if(anyNA(dates)||any(dates<lower|dates>upper))stop("Missing or out-of-target-year dates; review required")
      result$DatedPlayedResults<-nrow(f)
      if(nrow(f)){result$FirstDate<-as.character(min(dates));result$LastDate<-as.character(max(dates))}
      result$Status<-if(nrow(f))"DATED_RESULTS_EXTRACTABLE_NOT_YET_MATCHED"else "NO_PLAYED_RESULTS_EXTRACTED"
    },error=function(e){set(result,j="Error",value=conditionMessage(e));set(result,j="Status",value="SOURCE_REVIEW")})
    results[[i]]<-result
    fwrite(rbindlist(results,fill=TRUE),file.path(out,"dated_source_checks.csv"))
    if(blocked){cat("Host restricted access. Saved progress; no further requests this run.\n");break}
  }
  if(!length(results))fwrite(data.table(Country=character(),Status=character()),file.path(out,"dated_source_checks.csv"))
  if(length(errors))fwrite(rbindlist(errors),file.path(out,"download_errors.csv"))
  coverage <- copy(queue)
  coverage[,DirectoryCandidates:=0L]
  for(country in unique(catalog$Country))coverage[Country==country&Tier==1L,DirectoryCandidates:=nrow(catalog[Country==country])]
  coverage[SourceStatus=="NOT_CHECKED"&Tier==1L&SeasonStyle %in% c("calendar","split"),
    SourceStatus:=fifelse(DirectoryCandidates>0L,"CANDIDATE_SOURCE_REVIEW","NOT_LISTED_IN_BOUNDED_SEARCH")]
  if(length(results)) {
    checked <- rbindlist(results,fill=TRUE)
    for(country in unique(checked[Status=="DATED_RESULTS_EXTRACTABLE_NOT_YET_MATCHED",Country])) {
      urls <- paste(checked[Country==country&Status=="DATED_RESULTS_EXTRACTABLE_NOT_YET_MATCHED",SourceURL],collapse="; ")
      coverage[Country==country&Tier==1L&SourceStatus!="PILOT_192_SCORES_MATCHED",
        `:=`(SourceStatus="DATED_SOURCE_FOUND_MATCHING_PENDING",SourceURL=urls)]
    }
  }
  fwrite(coverage,file.path(out,"source_coverage_queue.csv"),na="")
  cat("[3/3] Source check complete. New downloads:",requests,"\n")
  if(length(results))print(rbindlist(results)[,.N,by=Status])
  cat("Not listed does not mean unavailable. Mixed seasons and lower tiers remain separate research.\n")
  cat("No imports or identity changes. Report:",normalizePath(out,winslash="/"),"\n")
}
check_target_sources()
