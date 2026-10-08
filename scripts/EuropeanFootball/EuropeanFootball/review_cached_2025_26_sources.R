# Offline extraction and source triage. Never imports matches or changes aliases.
review_cached_target_sources <- function() {
  suppressPackageStartupMessages({library(data.table);library(xml2);library(rvest);library(stringi)})
  started <- Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  base <- "EuropeanFootball/pipeline_data"
  input <- file.path(base,"Manual_Sources/Season_2025_26/source_discovery")
  out <- file.path(base,"Manual_Sources/Season_2025_26/cached_source_review")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  checks <- fread(file.path(input,"dated_source_checks.csv"))
  checks[,Code:=sub(".*/wettbewerb/","",Path)]
  cat("[1/4] Diagnosing held sources using cached pages only...\n")
  for(i in which(checks$Status=="SOURCE_REVIEW")) {
    p <- file.path(input,"cache",paste0("competition_",checks$Code[i],".html"))
    reason <- "Cached page unavailable; download warning requires review"
    if(file.exists(p)) {
      doc <- read_html(p);txt<-gsub("[[:space:]\u00a0]+"," ",xml_text(doc))
      if(!grepl("League level: First Tier",txt,fixed=TRUE))reason<-"Not confirmed as first-tier league (often lower division or cup)" else {
        labels <- trimws(xml_text(xml_find_all(doc,"//select[@name='saison_id']/option")))
        expected<-if(checks$SeasonStyle[i]=="calendar")"2025"else "25/26"
        reason<-if(sum(labels==expected)!=1L)"Target season not uniquely available"else "Schedule extraction or date check requires review"
      }
    }
    set(checks,i=i,j="ReviewReason",value=reason)
  }
  fwrite(checks,file.path(out,"source_triage.csv"))
  ready<-checks[Status=="DATED_RESULTS_EXTRACTABLE_NOT_YET_MATCHED"]
  cat("[2/4] Extracting played fixtures with source labels and match IDs...\n")
  all<-list()
  for(i in seq_len(nrow(ready))) {
    c<-ready[i];sid<-sub(".*/saison_id/","",c$SourceURL)
    doc<-read_html(file.path(input,"cache",paste0("schedule_",c$Code,"_",sid,".html")))
    for(tab in xml_find_all(doc,"//table[.//a[contains(@class,'ergebnis-link')]]")) {
      date<-NA_character_
      heading<-trimws(xml_text(xml_find_first(tab,"preceding::div[contains(@class,'content-box-headline')][1]")))
      for(row in xml_find_all(tab,".//tbody/tr")) {
        dl<-xml_attr(xml_find_first(row,".//a[contains(@href,'/datum/')]"),"href")
        if(!is.na(dl))date<-sub(".*/datum/","",dl)
        sc<-xml_find_first(row,".//a[contains(@class,'ergebnis-link')]");score<-trimws(xml_text(sc))
        if(is.na(score)||!grepl("^[0-9]+:[0-9]+$",score))next
        links<-xml_find_all(row,".//a[contains(@href,'/spielplan/verein/')]")
        names<-unique(xml_attr(links,"title"));ids<-unique(sub(".*/verein/([0-9]+).*","\\1",xml_attr(links,"href")))
        if(length(names)!=2L||anyNA(names)||length(ids)!=2L)stop("Club axis extraction failed: ",c$SourceURL)
        all[[length(all)+1L]]<-data.table(Country=c$Country,Season=c$TargetSeason,
          CompetitionLabel=c$CompetitionLabel,Code=c$Code,Date=date,Home=names[1],Away=names[2],
          HomeSourceID=ids[1],AwaySourceID=ids[2],Score=sub(":","-",score,fixed=TRUE),
          SourceRound=heading,MatchID=xml_attr(sc,"id"),SourceURL=paste0("https://www.transfermarkt.co.uk",xml_attr(sc,"href")))
      }
    }
    if(i%%10L==0L||i==nrow(ready))cat("  Extracted",i,"/",nrow(ready),"competition pages\n")
  }
  games<-rbindlist(all)
  if(anyNA(as.Date(games$Date)))stop("Missing dates in extracted played matches")
  if(anyDuplicated(games[,paste(Code,MatchID)]))stop("Repeated match IDs in extracted source; review required")
  cat("[3/4] Comparing with current master using existing aliases only...\n")
  aliases<-fread(file.path(base,"Reference/team_aliases.csv"))
  source("scripts/EuropeanFootball/club_identity_resolution.R",local=TRUE)
  local_map<-setNames(aliases$CanonicalName,paste(aliases$Country,aliases$SourceName,sep="\r"))
  regional_map<-setNames(character(),character())
  norm<-function(x,country)gsub("[^a-z0-9]","",tolower(stri_trans_general(resolve_alias_chain(x,country,local_map,regional_map),"Latin-ASCII")))
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),
    select=c("Country","Tier","CompetitionType","Date","Home","Away","Score"))
  m<-m[CompetitionType=="league"&Tier==1&Date>="2025-01-01"&Date<="2026-12-31"]
  m[,Key:=paste(Country,Date,norm(Home,Country),norm(Away,Country),gsub(":","-",Score,fixed=TRUE),sep="|")]
  games[,Key:=paste(Country,Date,norm(Home,Country),norm(Away,Country),Score,sep="|")]
  games[,Comparison:=fifelse(Key %chin% m$Key,"MATCHES_MASTER_USING_EXISTING_ALIASES","NOT_MATCHED_TO_MASTER_NOT_APPROVED_MISSING")]
  games[,ScopeDecision:="NEEDS_WIKIPEDIA_AND_STAGE_VALIDATION"]
  games[grepl("Gozo",CompetitionLabel,ignore.case=TRUE),ScopeDecision:="REGIONAL_LEAGUE_SCOPE_REVIEW"]
  fwrite(games,file.path(out,"dated_fixture_candidates.csv"))
  summary<-games[,.(ExtractedPlayed=.N,MasterMatched=sum(Comparison=="MATCHES_MASTER_USING_EXISTING_ALIASES"),
    Unmatched=sum(Comparison!="MATCHES_MASTER_USING_EXISTING_ALIASES"),FirstDate=min(Date),LastDate=max(Date)),
    by=.(Country,Code,CompetitionLabel)]
  fwrite(summary,file.path(out,"competition_summary.csv"))
  fwrite(unique(games[,.(Country,Code,CompetitionLabel,Name=Home,SourceID=HomeSourceID)])[order(Country,Code,Name)],file.path(out,"home_club_inventory.csv"))
  cat("[4/4] Offline review overview:\n")
  print(checks[Status=="SOURCE_REVIEW",.N,by=ReviewReason])
  cat("Usable pages:",nrow(ready),"; countries:",uniqueN(ready$Country),"; played fixture candidates:",nrow(games),"\n")
  print(games[,.N,by=Comparison])
  cat("Source stages may be incomplete; these counts are not season coverage percentages.\n")
  cat("No downloads, master changes or aliases. Report:",normalizePath(out,winslash="/"),"\n")
}
review_cached_target_sources()
