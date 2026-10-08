# Offline snapshot of played league results by season and source, as of 5 Oct 2026.
local({
  suppressPackageStartupMessages(library(data.table))
  out<-"EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26/source_recency"
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("Reading dated, completed league results...\n")
  m<-fread("EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv",
    select=c("Country","Competition","CompetitionType","Tier","League","Season","Date","Score","Source","SourcePage","SourceFile"),showProgress=FALSE)
  m[,Date:=as.IDate(Date)]
  m<-m[CompetitionType=="league" & !is.na(Date) & Date<=as.IDate("2026-10-05") & grepl("^[0-9]+-[0-9]+$",Score)]
  m[,SourceGroup:=fcase(tolower(Source)=="rsssf","RSSSF",
    grepl("rsssf",Source,ignore.case=TRUE),"Wikipedia + RSSSF hybrid",
    grepl("wikipedia_transfermarkt",Source,ignore.case=TRUE),"Wikipedia + dated source",default="Other / original pipeline")]
  detail<-m[,.(PlayedMatches=.N,FirstDate=min(Date),LastDate=max(Date),
    SourceLabels=paste(sort(unique(Source)),collapse="; ")),
    by=.(Country,Competition,Tier,League,Season,SourceGroup)]
  setorder(detail,Country,Tier,Competition,-LastDate)
  fwrite(detail,file.path(out,"all_league_seasons_by_source.csv"))
  recent<-detail[LastDate>=as.IDate("2024-01-01")]
  fwrite(recent,file.path(out,"recent_league_seasons_by_source.csv"))
  latest<-m[,.(LatestPlayedDate=max(Date),LatestSeason=Season[which.max(Date)],
    LatestLeagueLabel=League[which.max(Date)],LatestSource=Source[which.max(Date)],
    RSSSFLatestDate=if(any(SourceGroup=="RSSSF"))max(Date[SourceGroup=="RSSSF"]) else as.IDate(NA),
    RSSSFLatestSeason=if(any(SourceGroup=="RSSSF"))Season[which.max(fifelse(SourceGroup=="RSSSF",as.integer(Date),-Inf))] else ""),
    by=.(Country,Competition,Tier)]
  fwrite(latest,file.path(out,"latest_league_coverage.csv"))
  print(m[Date>=as.IDate("2025-01-01"),.N,by=.(SourceGroup,Source)][order(-N)])
  print(latest[Tier==1 & Country %chin% c("England","Spain","Italy","Germany","France","Brazil","Argentina","China","Norway","Croatia","Japan","United States","Mexico","Portugal","Belgium","Russia"),
    .(Country,LatestLeagueLabel,LatestSeason,LatestPlayedDate,RSSSFLatestSeason,RSSSFLatestDate)])
  cat("Saved snapshot:",normalizePath(out,winslash="/"),"\n")
})
