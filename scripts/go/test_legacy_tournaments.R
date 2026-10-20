# Short, offline checks for the legacy HTML adapter. No catalogue writes.
local({
  library(data.table); library(xml2)
  source("scripts/go/tournament_source_results.R")
  source("scripts/go/tournament_brackets.R")
  calendar <- data.table(Link=character(),Date=character())
  person <- function(n)list(name=n,id="",flag="",rating=NULL)
  fixture <- tempfile(fileext=".html");on.exit(unlink(fixture),add=TRUE)
  writeLines(c('<html><h3>Tournament table</h3><TABLE class="table_two">',
    '<TR><TH>Players</TH><TH>Final</TH></TR>',
    '<TR><TD>Alpha(9d)</TD><TD rowspan="2">',
    '1st round<br/>Alpha<br/><a href="/kifu/test/1">B+R</a><br/>2010-01-01<br/><br/>',
    '2nd round<br/>Beta<br/><a href="/kifu/test/2">W+R</a><br/>2010-01-02<br/><br/>',
    '3rd round<br/>Alpha<br/><a href="/kifu/test/3">B+1.5</a><br/>2010-01-03<br/>Alpha<br/>won by 2-1',
    '</TD></TR><TR><TD>Beta(9d)</TD></TR></TABLE></html>'),fixture)
  x <- go_source_results(fixture,"https://gotoeveryone.k2ss.info/news/test/series/1/","/news/test/series/",calendar,person)
  stopifnot(nrow(x$rows)==3L,identical(x$rows$Winner,c("Alpha","Beta","Alpha")),
    identical(x$rows$Date,c("2010-01-01","2010-01-02","2010-01-03")),
    all(x$rows$PlayerA=="Alpha"),all(x$rows$PlayerB=="Beta"),
    length(unique(x$rows$KifuLink))==3L)
  # Actual saved pages contain source typos; retain review rather than inventing
  # corrected names or feeder pairings. Every recovered result must have a winner.
  examples <- c("/news/wr/lg/23/","/news/wr/ig/8/","/news/jp/meijin/42/",
    "/news/jp/kisei/41/","/news/wr/sc/24/","/news/wr/ml/3/")
  for(link in examples) {
    path <- file.path("Go/pipeline_data/tournament_format_audit/cache",paste0(gsub("[^a-zA-Z0-9]+","_",link),".html"))
    if(!file.exists(path))next
    family <- sub("^(/news/[^/]+/[^/]+/).*","\\1",link)
    parsed <- go_source_results(path,paste0("https://gotoeveryone.k2ss.info",link),family,calendar,person)
    stopifnot(nrow(parsed$rows)>0L,all(parsed$rows$Winner %in% c(parsed$rows$PlayerA,parsed$rows$PlayerB)))
    draw <- go_tournament_brackets(paste0("https://gotoeveryone.k2ss.info",link),family,parsed$rows,
      list(),function(c,l)person(l),data.table(),calendar_override=calendar)
    stopifnot(length(draw$brackets)+length(draw$standings)>0L)
    cat(link,":",nrow(parsed$rows),"results;",length(draw$brackets),"draws;",length(draw$standings),"standings.\n")
  }
  cat("Legacy results, series dates/SGFs, and source geometry checks passed.\n")
})
