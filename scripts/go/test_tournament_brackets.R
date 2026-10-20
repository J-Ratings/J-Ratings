# Regression checks for distinct source draw structures, after either build.
local({
  library(jsonlite)
  read <- function(file) fromJSON(file.path("Go/data/tournaments/catalogue",file),simplifyVector=FALSE)
  myeongin <- read("kr-mi-49-.json")
  stopifnot(length(myeongin$brackets)==3L)
  upper <- myeongin$brackets[[1L]]; lower <- myeongin$brackets[[2L]]; final <- myeongin$brackets[[3L]]
  stopifnot(upper$name=="Winner's tournament",lower$name=="Loser's tournament",
    length(upper$matches)==15L,length(lower$matches)==14L,length(final$matches)==1L)
  stopifnot("3rd round" %in% vapply(lower$matches,function(m) m$round,""),
    "Quarterfinals" %in% vapply(lower$matches,function(m) m$round,""))
  # Both bracket champions feed the championship series; it remains unfinished.
  stopifnot(length(final$matches[[1L]]$feedsFrom)==2L,final$matches[[1L]]$winner=="",
    length(final$matches[[1L]]$series)==3L)
  stopifnot(length(myeongin$bracketGameIndices)==length(myeongin$games))
  lanke <- read("wr-quzhou-lanke-4-.json")
  nodes <- lanke$brackets[[1L]]$matches
  # Late entrants/byes make one-feeder games alongside ordinary two-feeder games.
  feeders <- vapply(nodes,function(m) length(m$feedsFrom),0L)
  stopifnot(any(feeders==1L),any(feeders==2L),any(vapply(nodes,function(m) length(m$series)==3L,TRUE)))
  agon <- read("cn-agon-26-.json")
  stopifnot(length(agon$brackets)>0L)
  for(file in c("wr-ml-6-.json","wr-cl-16-.json")) {
    draw <- read(file)$brackets[[1L]]$matches
    semis <- Filter(function(m) m$round=="Semifinals",draw)
    final <- Filter(function(m) m$round=="Final",draw)
    stopifnot(length(semis)==2L,length(final)==1L,
      all(vapply(semis,function(m) isTRUE(m$redrawBefore)&&isTRUE(m$drawPending)&&length(m$feedsFrom)==0L,TRUE)),
      length(final[[1L]]$feedsFrom)==2L)
  }
  # Validate the complete exported graph for dangling edges and cycles.
  files <- list.files("Go/data/tournaments/catalogue",pattern="\\.json$",full.names=TRUE)
  checked <- 0L
  for(path in files) {
    data <- fromJSON(path,simplifyVector=FALSE)
    if(!length(data$brackets)) next
    nodes <- unlist(lapply(data$brackets,function(b) b$matches),recursive=FALSE)
    ids <- vapply(nodes,function(m) m$id,""); stopifnot(!anyDuplicated(ids))
    visit <- function(id,path=character()) {
      stopifnot(id %in% ids,!id %in% path)
      m <- nodes[[match(id,ids)]]
      for(f in m$feedsFrom) visit(f$id,c(path,id))
    }
    for(id in ids) visit(id)
    checked <- checked+1L
  }
  cat("Passed: Myeongin split draw and final, Lanke byes and series, Agon compact draw,",checked,"edition graphs.\n")
})
