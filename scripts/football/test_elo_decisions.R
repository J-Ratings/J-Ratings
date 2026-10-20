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

# Isolated evidence: reversed home/away orientation, aliases, unknown draws,
# non-draws, conflicting evidence, and an empty evidence file.
fixture <- tempfile('shootout-fixture-')
dir.create(file.path(fixture, 'Test/data'), recursive=TRUE)
on.exit_cleanup <- function() unlink(fixture, recursive=TRUE)
jsonlite::write_json(data.frame(id=c('a','b'), name=c('Alpha','Beta')),
  file.path(fixture,'Test/data/teams.json'), dataframe='rows')
index_path <- file.path(fixture,'Test/data/shootout-decisions.json')
jsonlite::write_json(list(list(date='2026-01-01', teams=c('a','b'), winner='a')), index_path, auto_unbox=TRUE)
test <- football_elo_decisions(fixture,'Test',as.Date(rep('2026-01-01',4)),
  c('Alpha','Beta','Alpha','Alpha'), c('Beta','Alpha','Beta','Beta'),
  c('1-1','1-1','2-0','0-0'),c(.5,.5,1,.5),c(.5,.5,0,.5))
stopifnot(identical(test$home,c(1,0,1,1)), identical(test$away,c(0,1,0,0)),
          identical(test$winner[c(1,2,4)],rep('Alpha',3)))
conflict <- try(football_elo_decisions(fixture,'Test',as.Date('2026-01-01'),
  'Alpha','Beta','1-1 (p 2-4)',.5,.5), silent=TRUE)
stopifnot(inherits(conflict,'try-error'))
jsonlite::write_json(list(),index_path)
empty <- football_elo_decisions(fixture,'Test',as.Date('2026-01-01'),
  'Alpha','Beta','1-1',.5,.5)
stopifnot(identical(empty$home,.5), !empty$decided)
no_draw <- football_elo_decisions(fixture,'Test',as.Date('2026-01-01'),
  'Alpha','Beta','2-0',1,0)
stopifnot(identical(no_draw$home,1), !no_draw$decided)
on.exit_cleanup()
cat('Shootout orientation, conflict, empty-evidence and non-draw checks passed.\n')

# Verify the Club score-parser optimisation against the original row-wise path.
expressions <- parse(file.path(repo,'scripts/EuropeanFootball/02_calculate_elo.R'))
parser <- Filter(function(e) is.call(e) && identical(e[[1]], as.name('<-')) &&
  identical(e[[2]], as.name('result_to_scores')), as.list(expressions))
eval(parser[[1]])
score_examples <- rep(c('0-0','2-1','1-3','0.5-0.5',NA,'','bad','2-2 (p 3-4)'),100)
old_scores <- t(vapply(score_examples,result_to_scores,numeric(2)))
unique_results <- unique(score_examples)
unique_scores <- t(vapply(unique_results,result_to_scores,numeric(2)))
new_scores <- unique_scores[match(score_examples,unique_results),,drop=FALSE]
stopifnot(identical(unname(old_scores),unname(new_scores)))
cat('Distinct-score parsing matches original results exactly.\n')
