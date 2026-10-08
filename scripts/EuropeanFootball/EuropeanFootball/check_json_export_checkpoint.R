# Isolated ranking continuation test. No production files are read or written.
local({
  suppressPackageStartupMessages({library(dplyr);library(tibble)})
  lines <- readLines('scripts/EuropeanFootball/03_write_json.R',warn=FALSE)
  first <- which(lines=='ranking_events <- hist_long %>%')[1]
  last <- which(grepl('^write_json_retry\\($',lines) & seq_along(lines)>first)[1]-1L
  code <- parse(text=lines[first:last])
  run <- function(checkpoint=NULL) {
    hist_long <- tibble(team=c('A','B','A','B','C'),
      date=as.Date(c('2024-01-01','2024-01-01','2025-01-02','2025-01-02','2025-01-03')),
      rating=c(2000,1990,2005,1985,2010))
    team_country_for_rankings <- tibble(team=c('A','B','C'),country='X')
    name_to_id <- c(A='a',B='b',C='c')
    TOP_TEAMS_START <- as.Date('2000-01-01');TOP_TEAMS_N<-5L;TOP_TEAMS_ACTIVE_DAYS<-365L
    EXPORT_CUTOFF <- as.Date('2024-12-31');asof_date<-as.Date('2025-01-03')
    export_checkpoint <- checkpoint
    for(expr in code) eval(expr)
    list(table=top_teams_tbl,ranking=ranking_boundary)
  }
  full <- run()
  resumed <- run(list(ranking=full$ranking))
  stopifnot(identical(full$table,resumed$table))
  cat('PASS: frozen country rankings plus resumed events exactly match a full export.\n')
})
