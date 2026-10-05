# Offline presence audit. No downloads or production changes.
# Calendar leagues target 2025; split-year leagues target 2025/26.
audit_target_season <- function() {
  suppressPackageStartupMessages(library(data.table))
  started<-Sys.time()
  on.exit({cat("Elapsed:",round(as.numeric(difftime(Sys.time(),started,units="secs"))),"seconds\n")
    if(interactive()&&requireNamespace("beepr",quietly=TRUE))try(beepr::beep(),silent=TRUE)},add=TRUE)
  root<-normalizePath(getwd(),winslash="/",mustWork=TRUE)
  base<-file.path(root,"EuropeanFootball/pipeline_data")
  out<-file.path(base,"Manual_Sources/Global/season_2025_26_presence")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  cat("[1/4] Reading master and league reference...\n")
  m<-fread(file.path(base,"Matches_Clean_Combined/european_football_all_matches.csv"),encoding="UTF-8")
  seeds<-fread(file.path(base,"Reference/non_uefa_country_seeds.csv"))
  # Recover standard country spelling without altering the source master.
  country_map<-setNames(seeds$Country,tolower(seeds$Country))
  fix_country<-function(x) {z<-unname(country_map[tolower(trimws(x))]);u<-!is.na(z);x[u]<-z[u];x}
  m[,Country:=fix_country(Country)]
  clean<-function(x) {
    x<-trimws(as.character(x));x<-gsub("[–—-]","/",x)
    short<-grepl("^[0-9]{4}/[0-9]{2}$",x)
    x[short]<-paste0(substr(x[short],1,5),substr(x[short],1,2),substr(x[short],6,7))
    x
  }
  m[,`:=`(SeasonClean=clean(Season),Date=as.IDate(Date),Tier=suppressWarnings(as.integer(Tier)))]
  m[,StartYear:=suppressWarnings(as.integer(substr(SeasonClean,1,4))) ]
  m[,SeasonStyle:=fifelse(grepl("^[0-9]{4}$",SeasonClean),"calendar",
    fifelse(grepl("^[0-9]{4}/[0-9]{4}$",SeasonClean),"split","unknown"))]
  m[,Played:=!is.na(Date)&!is.na(Result)&nzchar(trimws(Result))&!is.na(Score)&
    grepl("^[0-9]+[[:space:]]*[-:][[:space:]]*[0-9]+$",trimws(Score))]
  league<-m[CompetitionType=="league" & !is.na(Tier)&!is.na(Country)&nzchar(Country)]
  # Country/tier rollup joins generic RSSSF and named Wikipedia competition keys.
  units<-unique(rbind(seeds[,.(Country,Tier=as.integer(Tier))],
    league[StartYear>=2023 & StartYear<=2025,.(Country,Tier)]))
  units<-merge(units,unique(seeds[,.(Country,Confederation)]),by="Country",all.x=TRUE)
  cat("[2/4] Inferring calendar versus split-year seasons...\n")
  history<-unique(league[StartYear>=2020 & StartYear<=2025 & SeasonStyle!="unknown",
    .(Country,Tier,SeasonClean,StartYear,SeasonStyle)])
  reports<-list()
  for(i in seq_len(nrow(units))) {
    unit<-units[i];hist<-history[Country==unit$Country&Tier==unit$Tier]
    if(nrow(hist))hist<-hist[StartYear>=max(StartYear)-2L]
    styles<-hist[,.(N=.N),by=SeasonStyle][order(-N)]
    style<-if(!nrow(styles))"unknown" else if(nrow(styles)>1L)"mixed_review" else styles$SeasonStyle[1]
    target<-if(style=="calendar")"2025" else if(style=="split")"2025/2026" else "2025 OR 2025/2026 (review)"
    all<-league[Country==unit$Country&Tier==unit$Tier]
    rows<-all[SeasonClean %chin% if(style=="calendar")"2025" else if(style=="split")"2025/2026" else c("2025","2025/2026")]
    played<-rows[Played==TRUE];upcoming<-rows[Played==FALSE & !is.na(Date)]
    previous<-all[Played==TRUE & StartYear<=2025]
    reports[[i]]<-data.table(unit,SeasonStyle=style,TargetSeason=target,
      TargetCompletedMatches=nrow(played),TargetDatedUnplayedRows=nrow(upcoming),
      TargetTeams=uniqueN(c(played$Home,played$Away)),
      FirstTargetMatch=if(nrow(played))as.character(min(played$Date)) else NA_character_,
      LastTargetMatch=if(nrow(played))as.character(max(played$Date)) else NA_character_,
      LatestRecordedSeason=if(nrow(previous))previous[order(-Date),Season][1] else NA_character_,
      LeagueLabels=paste(sort(unique(all[StartYear>=2023 & StartYear<=2025,League])),collapse="; "),
      TargetCompetitionKeys=paste(sort(unique(rows$Competition)),collapse="; "),
      Status=if(style %chin% c("unknown","mixed_review"))"SEASON_CONVENTION_REVIEW" else
        if(nrow(played))"PRESENT_COMPLETENESS_NOT_ESTABLISHED" else
        if(nrow(upcoming))"FIXTURES_ONLY_NO_RESULTS" else "MISSING_TARGET_SEASON")
    if(i%%25L==0L||i==nrow(units))cat("  League country/tier units:",i,"/",nrow(units),"\n")
  }
  report<-rbindlist(reports);setorder(report,Confederation,Country,Tier)
  cat("[3/4] Writing individual competition detail and comparison seasons...\n")
  detail<-league[SeasonClean %chin% c("2025","2025/2026"),
    .(CompletedMatches=sum(Played),DatedUnplayedRows=sum(!Played & !is.na(Date)),
      LeagueLabels=paste(sort(unique(League)),collapse="; "),
      Sources=paste(sort(unique(Source)),collapse="; ")),
    by=.(Country,Tier,Competition,Season=SeasonClean)]
  comparison<-league[StartYear>=2023 & StartYear<=2025,
    .(CompletedMatches=sum(Played),DatedUnplayedRows=sum(!Played&!is.na(Date))),
    by=.(Country,Tier,Season,SeasonStyle)]
  fwrite(report,file.path(out,"league_status.csv"),na="")
  fwrite(report[Status %chin% c("MISSING_TARGET_SEASON","FIXTURES_ONLY_NO_RESULTS")],file.path(out,"missing_leagues.csv"),na="")
  fwrite(report[Status=="SEASON_CONVENTION_REVIEW"],file.path(out,"season_conventions_to_review.csv"),na="")
  fwrite(detail,file.path(out,"target_competition_detail.csv"),na="")
  fwrite(comparison,file.path(out,"recent_season_counts.csv"),na="")
  summary<-report[,.(Leagues=.N),by=.(Confederation,Status)]
  fwrite(summary,file.path(out,"summary.csv"),na="")
  cat("[4/4] Target-season overview:\n");print(summary)
  cat("\nMissing or fixtures-only:\n");print(report[Status %chin% c("MISSING_TARGET_SEASON","FIXTURES_ONLY_NO_RESULTS"),
    .(Country,Tier,TargetSeason,LatestRecordedSeason,Status)])
  writeLines(c("Scope: existing covered league country/tier units, plus seed-reference tiers; not all leagues on Earth.",
    "2025 is the calendar-year equivalent. 2026 and 2026/27 are excluded.",
    "Present means at least one completed dated match, NOT a complete season or a percentage threshold.",
    "Generic RSSSF and named Wikipedia competition keys are rolled up by country/tier.",
    "Parallel regional leagues at the same tier must be checked separately in competition detail.",
    "Mixed or unknown season conventions are held for review, not automatically called missing.",
    "Recent-season counts help spot partial seasons but are not expected-fixture denominators.",
    "No downloads, parsing, master writes, Elo calculations, or JSON regeneration."),file.path(out,"READ_ME.txt"))
  cat("\nProduction unchanged. Report:",out,"\n")
}
audit_target_season()
