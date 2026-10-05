# Replace Argentina's incomplete RSSSF domestic history with the corrected
# cached-page rebuild. Run rsssf_argentina_modern_audit.R first.

suppressPackageStartupMessages({
  library(data.table)
  library(tictoc)
  library(beepr)
})

tictoc::tic("Replace Argentina RSSSF domestic history")
on.exit({tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE)}, add = TRUE)

root <- normalizePath(Sys.getenv(
  "J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"
), winslash = "/", mustWork = TRUE)
master_path <- file.path(
  root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv"
)
source_path <- file.path(
  root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_Argentina_Modern_Audit/all_dated_rsssf_games.csv"
)
if (!file.exists(master_path) || !file.exists(source_path))
  stop("Run rsssf_argentina_modern_audit.R first; required input is missing.")

master <- fread(master_path, encoding = "UTF-8")
src <- fread(source_path, encoding = "UTF-8")
master_cols <- names(master)

# Argentina uses calendar and split seasons. Filter by the actual match date.
src[, Date := as.IDate(Date)]
src <- src[
  Country == "Argentina" & CompetitionType == "league" &
    Date >= as.IDate("2010-01-01") & Date <= as.IDate("2025-12-31") &
    !Annotated & Result %chin% c("1-0", "0.5-0.5", "0-1") &
    grepl("^[0-9]+-[0-9]+$", Score)
]
if (!nrow(src)) stop("The corrected Argentina audit contains no eligible dated matches.")

replacement <- src[, .(
  Season, Country = "Argentina", Competition = "argentina_top_flight",
  CompetitionType = "league", Tier = 1L,
  League = "Argentine Primera Division", Date, Home, Away, Result, Score,
  Source = "rsssf", SourcePage, Stage, DateApprox = FALSE, SourceFile
)]
for (nm in setdiff(master_cols, names(replacement))) replacement[, (nm) := NA]
replacement <- replacement[, ..master_cols]
replacement <- unique(replacement, by = c("Date", "Home", "Away", "Score", "Competition"))

if (replacement[Home == Away, .N]) stop("Corrected Argentina rows contain self-matches.")
if (replacement[is.na(Date) | !nzchar(Home) | !nzchar(Away), .N])
  stop("Corrected Argentina rows contain missing required fields.")

replace_row <- master$Country == "Argentina" & master$Source == "rsssf" &
  master$CompetitionType == "league"
removed <- master[replace_row]
result <- rbindlist(list(master[!replace_row], replacement), use.names = TRUE)
result <- unique(result, by = c("Date", "Home", "Away", "Score", "Competition", "Source"))
setorder(result, Date, Country, Home, Away)

cat("Argentina domestic RSSSF replacement:\n")
print(data.table(
  Version = c("Removed old rows", "Added corrected rows"),
  Matches = c(nrow(removed), nrow(replacement)),
  FirstDate = c(as.character(min(as.IDate(removed$Date))), as.character(min(replacement$Date))),
  LastDate = c(as.character(max(as.IDate(removed$Date))), as.character(max(replacement$Date)))
))
print(replacement[, .(Matches = .N, FirstDate = min(Date), LastDate = max(Date)), by = Season])

if (identical(Sys.getenv("ARGENTINA_IMPORT_DRY_RUN"), "1")) {
  message("Dry run: production master unchanged.")
} else {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup <- sub("[.]csv$", paste0("_before_argentina_", stamp, ".csv"), master_path)
  if (!file.copy(master_path, backup)) stop("Could not create master backup.")
  tmp <- paste0(master_path, ".tmp")
  fwrite(result, tmp, na = "")
  check <- fread(tmp, encoding = "UTF-8")
  actual <- check[Country == "Argentina" & Source == "rsssf" & CompetitionType == "league", .N]
  if (nrow(check) != nrow(result) || actual != nrow(replacement)) {
    unlink(tmp)
    stop("Written-file verification failed; production master unchanged. Backup: ", backup)
  }
  if (!file.copy(tmp, master_path, overwrite = TRUE))
    stop("Could not replace production master. Backup: ", backup)
  unlink(tmp)
  message("Production master updated. Backup: ", backup)
}
