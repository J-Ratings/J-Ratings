# Small link index from already-built pages; no downloads or ratings changes.
local({
  library(jsonlite)
  base <- Sys.getenv("GO_TOURNAMENT_LINK_OUTPUT","Go/data/tournaments/catalogue")
  catalogue <- fromJSON(file.path(base,"index.json"),simplifyVector=FALSE)
  links <- list()
  for(page in catalogue) {
    data <- fromJSON(file.path(base,page$file),simplifyVector=FALSE)
    for(game in data$games) {
      date <- if(!is.null(game$historyDate) && nzchar(game$historyDate)) game$historyDate else game$date
      if(!nzchar(date)||length(game$players)!=2L) next
      players <- sort(vapply(game$players,function(p) p$name,character(1)))
      key <- paste(date,players[1],players[2],game$winner,sep="|")
      previous <- links[[key]]
      files <- unique(c(previous,page$file))
      links[[key]] <- files
    }
  }
  links <- lapply(links,function(files) if(length(files)==1L) files else NULL)
  links <- links[!vapply(links,is.null,logical(1))]
  write_json(links,file.path(base,"game-links.json"),auto_unbox=TRUE)
  cat("Built",length(links),"unambiguous game-to-tournament links.\n")
})
