# Add the audited RSSSF-only non-UEFA domestic and continental history to the
# production master. Only normalized-exact Wikipedia aliases are accepted;
# fuzzy suggestions remain review-only.
#
# Preview without writing:
# Sys.setenv(NON_UEFA_IMPORT_DRY_RUN="1")
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/import_non_uefa_rsssf_world.R")

suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
  library(tictoc)
  library(beepr)
})

tictoc::tic("Import non-UEFA RSSSF history")
on.exit({tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE)}, add = TRUE)

root <- normalizePath(Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
                      winslash = "/", mustWork = TRUE)
base <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit")
master_path <- file.path(root, "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv")
alias_path <- file.path(root, "EuropeanFootball/pipeline_data/Reference/team_aliases.csv")
wiki_path <- Sys.getenv("NON_UEFA_WIKIPEDIA_NAMES",
  "C:/Users/stjuk/Downloads/non_uefa_wikipedia_team_names_2025_26.csv")
domestic_path <- file.path(base, "all_rsssf_games.csv")
continental_path <- file.path(base, "continental_rebuild/parsed_games.csv")
review_dir <- file.path(base, "team_identity_inventory")
paths <- c(master_path, alias_path, wiki_path, domestic_path, continental_path,
           file.path(review_dir, "domestic_alias_review.csv"),
           file.path(review_dir, "continental_alias_review.csv"))
if (any(!file.exists(paths))) stop("Missing input(s):\n", paste(paths[!file.exists(paths)], collapse = "\n"))

country_alias <- c(
  "Hongkong"="Hong Kong", "Macao"="Macau", "East Timor"="Timor-Leste",
  "Congo-Brazzaville"="Congo", "Congo-Kinshasa"="DR Congo",
  "Guinea Bissau"="Guinea-Bissau", "French Guyana"="French Guiana",
  "US Virgin Islands"="United States Virgin Islands", "Surinam"="Suriname",
  "Fiji (clubs)"="Fiji", "Vanuatu (PVFL)"="Vanuatu", "Vanuatu (VFFCL)"="Vanuatu"
)
map_country <- function(x) {
  x <- trimws(as.character(x)); hit <- x %chin% names(country_alias)
  x[hit] <- unname(country_alias[x[hit]]); x
}
slug <- function(x) {
  x <- tolower(stri_trans_general(as.character(x), "Latin-ASCII"))
  x <- gsub("[^a-z0-9]+", "_", x); gsub("(^_+|_+$)", "", x)
}
result_from_score <- function(x) {
  z <- tstrsplit(x, "-", fixed = TRUE)
  a <- as.integer(z[[1L]]); b <- as.integer(z[[2L]])
  fifelse(a > b, "1-0", fifelse(a < b, "0-1", "0.5-0.5"))
}

wiki <- fread(wiki_path, encoding = "UTF-8")
official <- unique(wiki[, .(Confederation, Country=Association, League)])
if (official[, uniqueN(League), by=Country][V1 > 1L, .N]) stop("Wikipedia input has multiple leagues for an association")
official <- official[, .(Confederation=Confederation[1L], League=League[1L]), by=Country]

dom <- fread(domestic_path, encoding = "UTF-8")
dom[, Country := map_country(Country)]
dom <- dom[CompetitionType == "league" & Country %chin% official$Country &
             as.integer(StartYear) >= 2010L & as.integer(StartYear) <= 2025L &
             !is.na(Date) & Result %chin% c("1-0","0.5-0.5","0-1")]
# Reject century/date corruption while allowing both calendar seasons and
# cross-year seasons. StartYear is supplied by the audited source manifest.
dom[, MatchYear := as.integer(substr(as.character(Date), 1L, 4L))]
bad_domestic_dates <- dom[is.na(StartYear) | is.na(MatchYear) |
                            MatchYear < as.integer(StartYear) - 1L |
                            MatchYear > as.integer(StartYear) + 1L]
if (nrow(bad_domestic_dates)) {
  message("Excluded ", nrow(bad_domestic_dates), " domestic rows with dates outside their season window.")
}
dom <- dom[!is.na(StartYear) & !is.na(MatchYear) &
             MatchYear >= as.integer(StartYear) - 1L & MatchYear <= as.integer(StartYear) + 1L]
dom[, MatchYear := NULL]
dom <- official[dom, on="Country"]
dom[, `:=`(Competition=paste0(slug(Country), "_top_flight"), CompetitionType="league",
           Tier=1L, Source="rsssf")]

cont <- fread(continental_path, encoding = "UTF-8")
cont <- cont[!is.na(Date) & nzchar(Home) & nzchar(Away) & grepl("^[0-9]+-[0-9]+$", Score)]
cont[, `:=`(
  Country=fcase(Confederation=="AFC","Asia", Confederation=="CAF","Africa",
                Confederation=="CONCACAF","North America", Confederation=="CONMEBOL","South America",
                Confederation=="OFC","Oceania"),
  Competition=fcase(Confederation=="AFC","afc_club_competitions", Confederation=="CAF","caf_club_competitions",
                    Confederation=="CONCACAF","concacaf_club_competitions", Confederation=="CONMEBOL","conmebol_club_competitions",
                    Confederation=="OFC","ofc_champions_league"),
  CompetitionType="continental", Tier=NA_integer_,
  League=fcase(Confederation=="AFC","AFC Club Competitions", Confederation=="CAF","CAF Club Competitions",
               Confederation=="CONCACAF","CONCACAF Club Competitions", Confederation=="CONMEBOL","CONMEBOL Club Competitions",
               Confederation=="OFC","OFC Champions League"),
  Result=result_from_score(Score), Source="rsssf", SourcePage=basename(SourceFile), DateApprox=FALSE
)]

master <- fread(master_path, encoding = "UTF-8")
master_cols <- names(master)
make_rows <- function(x) {
  z <- x[, .(Season, Country, Competition, CompetitionType, Tier, League, Date=as.IDate(Date),
             Home, Away, Result, Score, Source, SourcePage,
             Stage=if ("Stage" %in% names(x)) Stage else "", DateApprox,
             SourceFile=if ("SourceFile" %in% names(x)) SourceFile else "")]
  for (nm in setdiff(master_cols, names(z))) z[, (nm) := NA]
  z[, ..master_cols]
}
new_rows <- rbindlist(list(make_rows(dom), make_rows(cont)), use.names=TRUE)
new_rows <- unique(new_rows, by=c("Date","Home","Away","Score","CompetitionType"))
if (new_rows[Home == Away, .N]) stop("Proposed import contains self-matches")

# Preserve only manually reviewed normalized-exact aliases.
read_exact <- function(name) fread(file.path(review_dir, name), encoding="UTF-8")[
  SuggestedDecision == "accept_normalised_exact" & CandidateRank == 1L,
  .(Country=Association, SourceName=RSSSFName, CanonicalName=WikipediaCandidate)
]
alias_add <- unique(rbindlist(list(read_exact("domestic_alias_review.csv"),
                                  read_exact("continental_alias_review.csv"))))
alias_add <- alias_add[nzchar(Country) & nzchar(SourceName) & nzchar(CanonicalName) & SourceName != CanonicalName]
aliases <- fread(alias_path, encoding="UTF-8")
conflicts <- rbindlist(list(aliases[,.(Country,SourceName,CanonicalName)], alias_add))[
  , .(N=uniqueN(CanonicalName)), by=.(Country,SourceName)][N > 1L]
if (nrow(conflicts)) stop("Alias conflicts found; production unchanged")
aliases_out <- unique(rbindlist(list(aliases, alias_add), fill=TRUE), by=c("Country","SourceName"))

existing_key <- do.call(paste, c(master[, .(Date,Home,Away,Score,CompetitionType)], sep="\r"))
new_key <- do.call(paste, c(new_rows[, .(Date,Home,Away,Score,CompetitionType)], sep="\r"))
new_rows <- new_rows[!new_key %chin% existing_key]
result <- rbindlist(list(master, new_rows), use.names=TRUE)
setorder(result, Date, Country, Home, Away)

cat("\nProposed non-UEFA import:\n")
print(new_rows[, .(Matches=.N, FirstDate=min(as.IDate(Date)), LastDate=max(as.IDate(Date))),
               by=.(CompetitionType, Country)][order(CompetitionType,Country)])
cat("\nRows:", nrow(master), "->", nrow(result), "| aliases:", nrow(aliases), "->", nrow(aliases_out), "\n")
if (Sys.getenv("NON_UEFA_IMPORT_DRY_RUN") == "1") {
  message("Dry run: production files unchanged."); quit(save="no")
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
master_backup <- sub("[.]csv$", paste0("_before_non_uefa_",stamp,".csv"), master_path)
alias_backup <- sub("[.]csv$", paste0("_before_non_uefa_",stamp,".csv"), alias_path)
if (!file.copy(master_path, master_backup) || !file.copy(alias_path, alias_backup)) stop("Could not create backups")
tmp_master <- paste0(master_path,".tmp"); tmp_alias <- paste0(alias_path,".tmp")
fwrite(result,tmp_master,na=""); fwrite(aliases_out,tmp_alias,na="",bom=TRUE)
if (nrow(fread(tmp_master,select="Date")) != nrow(result)) stop("Master verification failed")
file.copy(tmp_master,master_path,overwrite=TRUE); file.copy(tmp_alias,alias_path,overwrite=TRUE)
unlink(c(tmp_master,tmp_alias))
message("Production master and exact alias registry updated.")
message("Master backup: ",master_backup)
message("Alias backup: ",alias_backup)
