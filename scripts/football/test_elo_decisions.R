repo <- normalizePath(getwd(),winslash="/")
source(file.path(repo,"scripts/football/elo_decisions.R"))
international <- football_elo_decisions(repo,"InternationalFootball",
  as.Date(c("2022-12-18","1994-07-17","1900-01-01")),
  c("Argentina","Brazil","Argentina"),c("France","Italy","France"),
  c("3-3","0-0","0-0"),rep(.5,3),rep(.5,3))
stopifnot(identical(international$home,c(1,1,.5)),identical(international$away,c(0,0,.5)))
club <- football_elo_decisions(repo,"EuropeanFootball",as.Date(c("2019-02-05","1900-01-01")),
  c("Borussia Dortmund","Example A"),c("Werder Bremen","Example B"),
  c("3-3 (p 2-4)","1-1"),c(.5,.5),c(.5,.5))
stopifnot(identical(club$home,c(0,.5)),identical(club$away,c(1,.5)))
cat("Shootout Elo fixtures passed; unresolved draws remain draws.\n")
