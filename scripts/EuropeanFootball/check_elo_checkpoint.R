# Small isolated continuation test: no production reads, writes or completion sound.
local({
  library(data.table)
  code <- parse('scripts/EuropeanFootball/02_calculate_elo.R')
  for (expr in code) {
    if (is.call(expr) && identical(expr[[1]], as.name('<-')) &&
        as.character(expr[[2]]) %in% c('run_elo','expected_score')) eval(expr)
  }
  K_NORMAL <- 20; K_NEW <- 20; K_NEW_GAMES <- 100L
  K_SAME_CONFED <- 40; K_INTERCONFED <- 60
  CHECKPOINT_DATE <- as.Date('2024-12-31')
  x <- data.table(HomeKey=c('X\rA','X\rB','X\rA'), AwayKey=c('X\rB','X\rC','X\rC'),
    League='Test', Tier=1L, SeedRatingForTier=2000,
    Date=as.Date(c('2024-12-20','2025-01-02','2025-01-03')),
    HomeScore=c(1,0.5,0), AwayScore=c(0,0.5,1), KClass='domestic')
  for (mode in c('seed','retro')) {
    retro <- list('X\rA'=2010,'X\rB'=1990,'X\rC'=2005)
    full <- run_elo(x, entry_mode=mode, retro_start_map=retro)
    resumed <- run_elo(x[Date>CHECKPOINT_DATE], entry_mode=mode, retro_start_map=retro,
      initial_state=full$boundary_state, frozen_history=full$dt[Date<=CHECKPOINT_DATE])
    stopifnot(isTRUE(all.equal(full$dt,resumed$dt)), isTRUE(all.equal(full$final,resumed$final)))
  }
  cat('PASS: resumed seed and retro runs exactly match uninterrupted runs, including a newly appearing club.\n')
})
