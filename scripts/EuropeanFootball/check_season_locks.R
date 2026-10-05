# Small offline check. Only temporary test files are written.
tictoc::tic("Season lock checks")
Sys.setenv(FOUR_LEAGUES_FUNCTIONS_ONLY="1")
source("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")
check_locks <- function() {
  out <- tempfile("season_lock_test_")
  dir.create(out)
  on.exit(unlink(out,recursive=TRUE),add=TRUE)
  folder <- file.path(out,"Example_2024")
  dir.create(folder)
  games <- data.table(Home="Alpha",Away="Beta",Score="1-0",
    Date=as.Date("2024-04-07"),DateStatus="matched")
  fwrite(games,file.path(folder,"all_wikipedia_games.csv"))
  fwrite(data.table(RHome="Alpha",RAway="Beta"),file.path(folder,"rsssf_evidence.csv"))
  fwrite(data.table(RSSSF="Alpha",Wikipedia="Alpha"),file.path(folder,"team_map.csv"))
  row <- data.table(Country="Example",Season="2024",StartYear=2024L,
    WikipediaGames=1L,DatedGames=1L,Error="",ExtractionReview="",RSSSF="example")
  stopifnot(!freeze_season(out,copy(row)[,DatedGames:=0L]))
  stopifnot(!freeze_season(out,copy(row)[,ExtractionReview:="Review missing stages"]))
  stopifnot(!freeze_season(out,copy(row)[,`:=`(WikipediaGames=100L,DatedGames=95L)]))
  stopifnot(freeze_season(out,row))
  saved <- read_season_locks(out)
  stopifnot(nrow(saved)==1L,identical(saved$Error,""))
  # Changing working output must not change the protected snapshot.
  games[,Score:="2-0"]
  fwrite(games,file.path(folder,"all_wikipedia_games.csv"))
  stopifnot(freeze_season(out,row))
  snapshot <- file.path(out,"locked_seasons/Example_2024/all_wikipedia_games.csv")
  stopifnot(fread(snapshot)$Score=="1-0")
  fwrite(games,snapshot)
  stopifnot(inherits(try(read_season_locks(out),silent=TRUE),"try-error"))
  cat("Season locks: threshold, review exclusion, immutable copy and integrity checks passed.\n")
}
check_locks()
tictoc::toc()
tryCatch(suppressWarnings(beepr::beep()),error=function(e) message("Checks finished; sound unavailable."))
