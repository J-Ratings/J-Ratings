# Completed games only: scheduled dates and unplayed series games never count.
go_tournament_last_played <- function(payload,today=format(Sys.time(),tz="Europe/London",format="%Y-%m-%d")) {
  dates <- character()
  collect <- function(g) {
    if(isTRUE(g$bye)||is.null(g$winner)||!nzchar(g$winner)||is.null(g$result)||!nzchar(g$result)||is.null(g$date))return()
    date <- g$date
    if(length(date)==1L && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",date) && !is.na(as.Date(date)) && date<=as.character(today))
      dates <<- c(dates,date)
  }
  for(g in payload$games)collect(g)
  for(b in payload$brackets)for(m in b$matches) {
    if(length(m$series))for(g in m$series)collect(g) else collect(m)
  }
  if(length(dates))max(dates) else NULL
}
