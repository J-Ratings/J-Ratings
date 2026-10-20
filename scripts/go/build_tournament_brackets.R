# Refresh layouts from saved pages without repeating the Elo/history matching.
# Normal build_tournament_pages.R also generates this data automatically.
local({
  library(data.table); library(jsonlite)
  source("scripts/go/tournament_brackets.R")
  base <- "Go/data/tournaments/catalogue"
  catalogue <- fromJSON(file.path(base,"index.json"))
  selected <- Sys.getenv("GO_BRACKET_FILES", "")
  if(nzchar(selected)) catalogue <- catalogue[catalogue$file %in% strsplit(selected,",",fixed=TRUE)[[1L]],]
  source_rows <- fread("Go/pipeline_data/tournament_result_coverage/source_results.csv",encoding="UTF-8")
  profiles <- as.data.table(fromJSON("Go/data/players.json"))
  norm <- function(x) gsub("[^a-z0-9]","",tolower(iconv(x,"UTF-8","ASCII//TRANSLIT")))
  profiles[,key:=norm(name)]
  person <- function(canonical,label) {
    found <- profiles[key==norm(canonical)]
    if(nrow(found)!=1L) return(list(name=label,id="",flag="",rating=NULL))
    list(name=found$name,id=found$id,flag=found$flag,rating=NULL)
  }
  count <- 0L; matches <- 0L; issues <- list()
  for(i in seq_len(nrow(catalogue))) {
    entry <- catalogue[i,]; path <- file.path(base,entry$file)
    data <- fromJSON(path,simplifyVector=FALSE)
    draw <- go_tournament_brackets(data$source,entry$family,source_rows[SourceURL==data$source],data$games,person,profiles)
    nodes <- unlist(lapply(draw$brackets,function(b) b$matches),recursive=FALSE)
    ids <- vapply(nodes,function(m) m$id,"")
    stopifnot(!anyDuplicated(ids),all(draw$consumed>=1L & draw$consumed<=length(data$games)))
    for(m in nodes) {
      stopifnot(length(m$players)==2L)
      for(f in m$feedsFrom) stopifnot(f$id %in% ids,f$id!=m$id)
      if(nzchar(m$winner)) stopifnot(m$winner %in% vapply(m$players,function(p) p$name,""))
    }
    data$brackets <- draw$brackets; data$bracketGameIndices <- as.list(draw$consumed-1L); data$bracketReview <- draw$review
    data$sourceStandings <- draw$standings
    data$leagueTables <- draw$leagueTables
    if(!is.null(draw$games))data$games <- draw$games
    write_json(data,path,auto_unbox=TRUE,null="null")
    if(length(draw$brackets)) count <- count+1L
    matches <- matches+length(nodes)
    if(length(draw$review)) issues[[length(issues)+1L]] <- data.table(File=entry$file,Issue=unlist(draw$review))
    if(i%%50L==0L) cat("Layout",i,"/",nrow(catalogue),"\n")
  }
  review_path <- "Go/pipeline_data/tournament_result_coverage/bracket_review.csv"
  updated <- if(length(issues)) rbindlist(issues) else data.table(File=character(),Issue=character())
  if(nzchar(selected) && file.exists(review_path)) updated <- rbind(
    fread(review_path)[!File %in% catalogue$file],updated)
  fwrite(updated,review_path)
  cat("Connected layouts:",count,"editions;",matches,"matches. Saved pages only.\n")
})
