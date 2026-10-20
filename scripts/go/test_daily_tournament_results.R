# Fast isolated checks. No requests, production writes, or Elo runs.
local({
  library(data.table);library(jsonlite)
  source("scripts/go/daily_tournament_targets.R")
  source("scripts/go/reconcile_tournament_results.R")
  tmp <- tempfile("daily-go-test-");dir.create(tmp);on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  dir.create(file.path(tmp,"catalogue"));dir.create(file.path(tmp,"games"))
  p <- function(name)list(name=name,id=tolower(name),flag="kr")
  a <- p("Alpha");b <- p("Beta");c <- p("Gamma");d <- p("Delta")
  placeholder <- list(name="Winner of previous match",id="",flag="")
  sf1 <- list(id="s1",round="Semifinals",players=list(a,b),winner="",result="",date="2026-10-12",feedsFrom=list())
  sf2 <- list(id="s2",round="Semifinals",players=list(c,d),winner="",result="",date="2026-10-12",feedsFrom=list())
  final <- list(id="f",round="Final",players=list(placeholder,placeholder),winner="",result="",date="",
    feedsFrom=list(list(id="s1",outcome="winner"),list(id="s2",outcome="winner")),
    series=lapply(c("2026-10-13","2026-10-15","2026-10-16"),function(date)list(players=list(placeholder,placeholder),winner="",result="",date=date)))
  # Final deliberately precedes semifinals: propagation must use IDs, not order.
  matches <- list(final,sf2,sf1)
  link <- "/news/test/cup/1/"
  index <- list(list(name="1st Test Cup",family="/news/test/cup/",file="test.json",games=0L))
  index[[2L]] <- list(name="2nd Other Cup",family="/news/test/other/",file="other.json",games=1L)
  write_json(index,file.path(tmp,"catalogue/index.json"),auto_unbox=TRUE)
  payload <- list(name="1st Test Cup",source=paste0("https://gotoeveryone.k2ss.info",link),games=list(),
    brackets=list(list(name="Main draw",matches=matches)),bracketGameIndices=list())
  write_json(payload,file.path(tmp,"catalogue/test.json"),auto_unbox=TRUE)
  other <- payload;other$source <- "https://gotoeveryone.k2ss.info/news/test/other/2/"
  other$games <- list(list(players=list(p("Unrelated"),a),date="2026-10-13",winner="Alpha"))
  undated <- sf1;undated$date <- "";undated$players <- list(p("Unrelated"),a)
  other$brackets[[1L]]$matches <- list(undated)
  write_json(other,file.path(tmp,"catalogue/other.json"),auto_unbox=TRUE)
  home <- data.table(player_id=as.character(1:5),player_name=c("Alpha","Beta","Gamma","Delta","Unrelated"),
    profile_url=paste0("https://www.goratings.org/en/players/",1:5,".html"))
  fwrite(home,file.path(tmp,"home.csv"))
  fwrite(data.table(Link=c(link,"/news/test/other/2/"),Date="2026-10-13"),file.path(tmp,"calendar.csv"))
  select <- function(cap=80L)go_daily_targets(today=as.Date("2026-10-14"),catalogue_dir=file.path(tmp,"catalogue"),
    calendar_file=file.path(tmp,"calendar.csv"),homepage_file=file.path(tmp,"home.csv"),map_file=file.path(tmp,"missing.csv"),max_profiles=cap)
  target <- select();stopifnot(setequal(target$profiles$player_name,c("Alpha","Beta","Gamma","Delta")),!"5" %in% target$profiles$player_id)
  capped <- select(2L);stopifnot(nrow(capped$profiles)==2L,length(capped$deferred)==2L)
  stopifnot("/news/test/other/2/" %in% target$undatedTournaments)
  scoped <- go_daily_targets(today=as.Date("2026-10-14"),catalogue_dir=file.path(tmp,"catalogue"),calendar_file=file.path(tmp,"calendar.csv"),
    homepage_file=file.path(tmp,"home.csv"),map_file=file.path(tmp,"missing.csv"),tournament_links="/news/test/other/2/")
  stopifnot(nrow(scoped$profiles)==0L)
  game <- function(date,black,white,result)list(date=date,black=black,white=white,result=result,
    blackElo=3500,whiteElo=3400,blackWinPct=64,whiteWinPct=36)
  h1 <- game("2026-10-12","Alpha","Beta","1-0")
  h2 <- game("2026-10-12","Gamma","Delta","1-0")
  h3 <- game("2026-10-13","Alpha","Gamma","1-0")
  write_json(list(h1,h3),file.path(tmp,"games/alpha.json"),auto_unbox=TRUE)
  write_json(list(h2),file.path(tmp,"games/gamma.json"),auto_unbox=TRUE)
  report <- go_reconcile_tournaments(link,catalogue_dir=file.path(tmp,"catalogue"),games_dir=file.path(tmp,"games"))
  stopifnot(identical(report$updated,"test.json"))
  result <- fromJSON(file.path(tmp,"catalogue/test.json"),simplifyVector=FALSE)
  f <- result$brackets[[1L]]$matches[[1L]]
  stopifnot(identical(vapply(f$players,function(p)p$name,""),c("Alpha","Gamma")),
    f$series[[1L]]$winner=="Alpha",f$series[[1L]]$result=="B",f$winner=="",
    f$series[[1L]]$players[[1L]]$rating==3500,f$series[[1L]]$players[[1L]]$colour=="B",
    identical(unlist(f$seriesScores),c(1L,0L)),length(result$games)==3L)
  repeat_report <- go_reconcile_tournaments(link,catalogue_dir=file.path(tmp,"catalogue"),games_dir=file.path(tmp,"games"))
  stopifnot(!length(repeat_report$updated))
  ambiguous <- go_reconcile_draw(list(sf1),function(date,ps)list(h1,h1))
  stopifnot(ambiguous$matches[[1L]]$winner=="",length(ambiguous$review)==1L)
  completed <- sf1;completed$winner <- "Beta";completed$result <- "W+R"
  preserved <- go_reconcile_draw(list(completed),function(...)stop("A completed source result must not be queried"))
  stopifnot(preserved$matches[[1L]]$winner=="Beta",preserved$matches[[1L]]$result=="W+R")
  cat("Daily targets/cap, exact-date overlay, feeder propagation, series, preserved ratings, idempotence and ambiguity checks passed.\n")
})
