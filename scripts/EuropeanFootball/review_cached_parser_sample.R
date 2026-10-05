# Offline review of 30 weak seasons; never replaces audit, locks or master.
run_cached_parser_sample <- function() {
  tictoc::tic("Broad cached parser review")
  on.exit({tictoc::toc(); try(suppressWarnings(beepr::beep()),silent=TRUE)},add=TRUE)
  root <- Sys.getenv("J_RATINGS_REPO","C:/Users/stjuk/Documents/GitHub/J-Ratings")
  previous <- Sys.getenv("FOUR_LEAGUES_FUNCTIONS_ONLY",unset=NA_character_)
  on.exit(if(is.na(previous)) Sys.unsetenv("FOUR_LEAGUES_FUNCTIONS_ONLY") else
    Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY=previous),add=TRUE)
  Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY="1")
  engine <- new.env(parent=globalenv())
  source(file.path(root,"scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R"),local=engine,encoding="UTF-8")
  library(data.table)
  base <- file.path(root,"EuropeanFootball/pipeline_data")
  folder <- file.path(base,"Manual_Sources/Wikipedia_RSSSF_Alias_Audit")
  out <- file.path(folder,"broad_parser_review")
  dir.create(out,showWarnings=FALSE)
  audit <- fread(file.path(folder,"season_audit.csv"))
  coverage <- fread(file.path(folder,"parser_coverage_by_season.csv"))
  candidates <- coverage[StartYear>=2010 & CoveragePercent<95 & NoRSSSFFixturesGames==0 & RemainingFailures>=5]
  candidates <- candidates[order(-RemainingFailures)][,.SD[seq_len(min(.N,2L))],by=Country]
  candidates <- candidates[order(-RemainingFailures)][seq_len(min(.N,30L))]
  selection_path <- file.path(out,"selection.csv")
  if(file.exists(selection_path)) candidates <- fread(selection_path) else fwrite(candidates,selection_path)
  summaries <- list(); traces <- list(); failures <- list()
  inspect_only <- Sys.getenv("PARSER_REVIEW_INSPECT_ONLY","0")=="1"
  for(i in seq_len(nrow(candidates))) {
    c <- candidates[i]
    a <- audit[Country==c$Country & Season==c$Season][1L]
    key <- paste(c$Country,gsub("/","-",c$Season),sep="_")
    season_dir <- file.path(folder,key)
    w <- fread(file.path(season_dir,"all_wikipedia_games.csv"))
    old_r <- fread(file.path(season_dir,"rsssf_evidence.csv"))
    old_map <- fread(file.path(season_dir,"team_map.csv"))
    name <- paste0(gsub("[^A-Za-z0-9._-]","_",URLdecode(a$RSSSF)),".html")
    paths <- c(file.path(base,"Source/rsssf/all/pages",sub("https://www.rsssf.org/","",a$RSSSF,fixed=TRUE)),
      file.path(base,"Manual_Sources",c("Wikipedia_RSSSF_Alias_Audit/cache",
        "Wikipedia_RSSSF_Remaining_UEFA_Leagues/cache","Wikipedia_RSSSF_Four_Leagues/cache"),name))
    path <- paths[file.exists(paths)][1L]
    stopifnot(!is.na(path))
    doc <- xml2::read_html(path)
    lines <- unlist(strsplit(paste(xml2::xml_text(xml2::xml_find_all(doc,"//pre")),collapse="\n"),"\n",fixed=TRUE))
    eligible <- w[!DateStatus %in% c("matched","team_identity_unresolved")]
    # Prioritise different failure types, using existing approved/resolved identities.
    examples <- eligible[,head(.SD,2L),by=DateStatus]
    for(j in seq_len(nrow(examples))) {
      ex <- examples[j]
      home_names <- old_map[!is.na(Wikipedia) & Wikipedia==ex$Home,RSSSF]
      away_names <- old_map[!is.na(Wikipedia) & Wikipedia==ex$Away,RSSSF]
      same <- old_r[RHome %in% home_names & RAway %in% away_names & Score==ex$Score]
      raw_hit <- which(vapply(lines,function(line)
        any(vapply(home_names,function(n) grepl(n,line,fixed=TRUE),logical(1))) &&
        any(vapply(away_names,function(n) grepl(n,line,fixed=TRUE),logical(1))),logical(1)))
      context <- unique(unlist(lapply(head(raw_hit,3L),function(k) seq.int(max(1L,k-3L),min(length(lines),k+1L)))))
      traces[[length(traces)+1L]] <- data.table(Country=c$Country,Season=c$Season,
        Home=ex$Home,Away=ex$Away,Score=ex$Score,Status=ex$DateStatus,Stage=ex$Stage,
        RSSSF=a$RSSSF,LocalPath=path,ParsedExactRows=nrow(same),
        ParsedDates=paste(unique(same$Date),collapse=" | "),
        ParsedPhases=if("RSSSFPhase" %in% names(same)) paste(unique(same$RSSSFPhase),collapse=" | ") else "",
        HTMLContext=paste(lines[context],collapse="\n"))
    }
    if(inspect_only) next
    r <- engine$rsssf_games(doc,a$StartYear,a$StartYear+as.integer(grepl("/",a$Season,fixed=TRUE)))
    result <- engine$attach_dates(w,r,season_start=a$StartYear)
    before <- sum(w$DateStatus=="matched"); after <- sum(result$games$DateStatus=="matched")
    changed <- which(w$DateStatus=="matched" & result$games$DateStatus=="matched" & as.character(w$Date)!=as.character(result$games$Date))
    summaries[[i]] <- data.table(Country=c$Country,Season=c$Season,WikipediaGames=nrow(w),
      Before=before,After=after,Gain=after-before,ChangedExistingDates=length(changed),
      BeforePercent=round(100*before/nrow(w),2),AfterPercent=round(100*after/nrow(w),2))
    fwrite(result$games,file.path(out,paste0(key,"_games.csv")))
    fwrite(r,file.path(out,paste0(key,"_rsssf.csv")))
    print(summaries[[i]])
  }
  fwrite(rbindlist(traces,fill=TRUE),file.path(out,"html_examples.csv"))
  if(length(summaries)) fwrite(rbindlist(summaries),file.path(out,"comparison.csv"))
  cat("Review saved to",out,"\n")
}
run_cached_parser_sample()
