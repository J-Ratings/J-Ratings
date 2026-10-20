# Read-only player coverage analysis; uses existing audit outputs, no downloads.
local({
  library(data.table)
  library(jsonlite)
  out <- "Go/pipeline_data/tournament_result_coverage"
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(iconv(x, "UTF-8", "ASCII//TRANSLIT")))
  sig <- function(d, a, b, w) paste(d, pmin(norm(a), norm(b)), pmax(norm(a), norm(b)), norm(w), sep = "|")
  players <- as.data.table(fromJSON("Go/data/players.json"))
  meta <- fromJSON("Go/data/meta.json")
  cutoff <- paste0(as.integer(substr(meta$asof, 1, 4)) - 2L, substr(meta$asof, 5, 10))
  players <- players[!is.na(last_date) & last_date >= cutoff]
  setorder(players, -rating, name)
  players[, LiveRank := .I]
  players[, NameKey := norm(name)]
  source <- fread(file.path(out, "source_results.csv"))
  covered <- unique(source[Comparison == "MATCHED_LOCAL_HISTORY", Signature])
  history <- fread("Go/pipeline_data/live_Elo/combined_game_history.csv",
    select = c("Date", "Black", "White", "ResultCode"))
  history[, Date := as.character(Date)]
  history <- history[Date >= "2022-03-01" & Date <= meta$asof & grepl("^[BW](\\+|$)", ResultCode)]
  history[, Signature := sig(Date, Black, White, ifelse(startsWith(ResultCode, "B"), Black, White))]
  history <- unique(history, by = "Signature")
  history[, Covered := Signature %in% covered]
  apps <- rbind(history[, .(NameKey = norm(Black), Date, Signature, Covered)],
    history[, .(NameKey = norm(White), Date, Signature, Covered)])
  count <- apps[, .(ExistingGames = .N, CoveredGames = sum(Covered),
    Existing2026 = sum(substr(Date, 1, 4) == "2026"),
    Covered2026 = sum(Covered & substr(Date, 1, 4) == "2026")), by = NameKey]
  result <- merge(players[, .(LiveRank, name, rating, last_date, NameKey)], count, by = "NameKey", all.x = TRUE)
  for (col in c("ExistingGames", "CoveredGames", "Existing2026", "Covered2026")) set(result, which(is.na(result[[col]])), col, 0L)
  result[, CoveragePercent := round(100 * CoveredGames / pmax(1L, ExistingGames), 1)]
  result[, Coverage2026Percent := round(100 * Covered2026 / pmax(1L, Existing2026), 1)]
  setorder(result, LiveRank)
  result[, RankBand := cut(LiveRank, c(0, 10, 30, 50, 100, 200, 500, Inf),
    labels = c("1-10", "11-30", "31-50", "51-100", "101-200", "201-500", "501+"))]
  bands <- result[, .(Players = .N, PlayersWithCoveredGames = sum(CoveredGames > 0),
    ExistingGames = sum(ExistingGames), CoveredGames = sum(CoveredGames),
    Existing2026 = sum(Existing2026), Covered2026 = sum(Covered2026)), by = RankBand]
  bands[, CoveragePercent := round(100 * CoveredGames / pmax(1L, ExistingGames), 1)]
  bands[, Coverage2026Percent := round(100 * Covered2026 / pmax(1L, Existing2026), 1)]
  fwrite(result, file.path(out, "coverage_by_live_player.csv"))
  fwrite(bands, file.path(out, "coverage_by_live_rank_band.csv"))
  cat("Live homepage ranking as of", meta$asof, "; two-year activity cutoff", cutoff, "\n")
  print(bands)
  print(result[LiveRank <= 30, .(LiveRank, name, rating, ExistingGames, CoveredGames, CoveragePercent, Coverage2026Percent)])
  cat("Live players:", nrow(result), "; with at least one covered game:", sum(result$CoveredGames > 0), "\n")
  cat("Lowest-rated covered live player:\n")
  print(tail(result[CoveredGames > 0, .(LiveRank, name, rating, CoveredGames)], 1))
  cat("Counts are player appearances: a game contributes to both players. No production changes.\n")
})
