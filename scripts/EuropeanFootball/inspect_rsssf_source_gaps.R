# Bounded offline inspection of 2010+ seasons with no extracted RSSSF fixtures.
# No web requests, alias changes, audit replacement or production changes.
inspect_source_gaps <- function() {
  tictoc::tic("Inspect cached RSSSF source gaps")
  on.exit({tictoc::toc(); try(suppressWarnings(beepr::beep()),silent=TRUE)},add=TRUE)
  root <- Sys.getenv("J_RATINGS_REPO","C:/Users/stjuk/Documents/GitHub/J-Ratings")
  engine <- new.env(parent=globalenv())
  old <- Sys.getenv("FOUR_LEAGUES_FUNCTIONS_ONLY",unset=NA_character_)
  on.exit(if(is.na(old)) Sys.unsetenv("FOUR_LEAGUES_FUNCTIONS_ONLY") else
    Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY=old),add=TRUE)
  Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY="1")
  sys.source(file.path(root,"scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R"),engine)
  base <- file.path(root,"EuropeanFootball/pipeline_data")
  out <- file.path(base,"Manual_Sources/Wikipedia_RSSSF_Alias_Audit")
  audit <- data.table::fread(file.path(out,"season_audit.csv"))
  cases <- audit[StartYear>=2010 & WikipediaGames>0 & RSSSFResults==0]
  rows <- lapply(seq_len(nrow(cases)),function(i) {
    a <- cases[i]
    name <- paste0(gsub("[^A-Za-z0-9._-]","_",URLdecode(a$RSSSF)),".html")
    paths <- c(file.path(base,"Source/rsssf/all/pages",sub("https://www.rsssf.org/","",a$RSSSF,fixed=TRUE)),
      file.path(base,"Manual_Sources",c("Wikipedia_RSSSF_Alias_Audit/cache",
        "Wikipedia_RSSSF_Remaining_UEFA_Leagues/cache","Wikipedia_RSSSF_Four_Leagues/cache"),name))
    paths <- unique(paths[file.exists(paths)])
    if(!length(paths)) return(data.table::data.table(Country=a$Country,Season=a$Season,
      WikipediaGames=a$WikipediaGames,RSSSF=a$RSSSF,Assessment="no_local_page"))
    # Inspect every available copy, not only the bulk-download copy.
    data.table::rbindlist(lapply(paths,function(path) {
      r <- engine$rsssf_games(xml2::read_html(path),a$StartYear,
        a$StartYear+as.integer(grepl("/",a$Season,fixed=TRUE)))
      data.table::data.table(Country=a$Country,Season=a$Season,WikipediaGames=a$WikipediaGames,
        RSSSF=a$RSSSF,LocalPath=path,FixtureRows=nrow(r),DatedRows=sum(!is.na(r$Date)),
        Assessment=attr(r,"source_assessment"))
    }),fill=TRUE)
  })
  report <- data.table::rbindlist(rows,fill=TRUE)
  data.table::fwrite(report,file.path(out,"source_gap_inspection.csv"))
  print(report[,.(Country,Season,WikipediaGames,FixtureRows,DatedRows,Assessment)])
  cat("Inspected",nrow(cases),"seasons;",sum(cases$WikipediaGames),"Wikipedia games.\n")
}
inspect_source_gaps()
