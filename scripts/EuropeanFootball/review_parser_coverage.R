# Read saved audit outputs only: no downloads, parsing rebuild or production writes.
review_parser_coverage <- function() {
  tictoc::tic("Saved parser coverage review")
  on.exit({tictoc::toc(); try(suppressWarnings(beepr::beep()),silent=TRUE)},add=TRUE)
  library(data.table)
  root <- Sys.getenv("J_RATINGS_REPO","C:/Users/stjuk/Documents/GitHub/J-Ratings")
  folder <- file.path(root,"EuropeanFootball/pipeline_data/Manual_Sources/Wikipedia_RSSSF_Alias_Audit")
  w <- fread(file.path(folder,"all_wikipedia_games.csv"),select=c("Country","Season","DateStatus"))
  w[, StartYear:=as.integer(substr(Season,1,4))]
  w[, Period:=ifelse(StartYear>=2010,"2010 onward","Before 2010")]
  metrics <- function(z) {
    total <- nrow(z); dated <- sum(z$DateStatus=="matched")
    identities <- sum(z$DateStatus=="team_identity_unresolved")
    no_fixtures <- sum(z$DateStatus=="rsssf_fixtures_not_extracted")
    eligible <- total-identities
    data.table(WikipediaGames=total,DatedGames=dated,UnresolvedIdentityGames=identities,
      NoRSSSFFixturesGames=no_fixtures,GamesExcludingUnresolvedIdentities=eligible,
      CoveragePercent=if(total) round(100*dated/total,2) else NA_real_,
      CoverageExcludingUnresolvedIdentities=if(eligible) round(100*dated/eligible,2) else NA_real_,
      RemainingFailures=eligible-dated)
  }
  seasons <- w[,metrics(.SD),by=.(Country,Season,StartYear,Period)]
  countries <- w[,metrics(.SD),by=.(Country,Period)]
  periods <- w[,metrics(.SD),by=Period]
  fwrite(seasons,file.path(folder,"parser_coverage_by_season.csv"))
  fwrite(countries,file.path(folder,"parser_coverage_by_country.csv"))
  fwrite(periods,file.path(folder,"parser_coverage_by_period.csv"))
  audit <- fread(file.path(folder,"season_audit.csv"))
  if("SourceAssessment" %in% names(audit)) {
    fwrite(audit[WikipediaGames>0 & RSSSFResults==0,
      .(Country,Season,WikipediaGames,RSSSF,SourceAssessment,ExtractionReview)],
      file.path(folder,"source_layout_review.csv"))
  }
  print(periods)
  print(countries[Period=="2010 onward",.(Country,CoveragePercent,CoverageExcludingUnresolvedIdentities,
    UnresolvedIdentityGames,NoRSSSFFixturesGames,RemainingFailures)][order(Country)])
  print(w[StartYear>=2010,.N,by=DateStatus][order(-N)])
  shortlist <- seasons[StartYear>=2010 & NoRSSSFFixturesGames==0 & RemainingFailures>=10][order(-RemainingFailures)]
  print(shortlist[,.(Country,Season,CoverageExcludingUnresolvedIdentities,RemainingFailures)][1:min(.N,35L)])
}
review_parser_coverage()
