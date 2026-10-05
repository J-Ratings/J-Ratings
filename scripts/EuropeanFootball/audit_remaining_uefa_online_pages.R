# Discovery-only audit of RSSSF top-flight pages for UEFA leagues.
#
# Scope: 2010/11 through 2024/25 (2010 through 2025 for calendar leagues),
# excluding England, France, Germany, Italy and Portugal.
#
# This script reads two RSSSF decade index pages. It does not download fixture
# pages, parse new matches, alter the production master, or calculate Elo.

suppressPackageStartupMessages({
  library(data.table)
  library(xml2)
  library(rvest)
})

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
master_path <- file.path(
  root,
  "EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv"
)
out_dir <- file.path(
  root,
  "EuropeanFootball/pipeline_data/Manual_Sources/UEFA/online_page_discovery"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

config <- fread(text = "Country;Prefix;Calendar
Albania;tablesa/alba;FALSE
Andorra;tablesa/ando;FALSE
Armenia;tablesa/arme;FALSE
Austria;tableso/oost;FALSE
Azerbaijan;tablesa/azer;FALSE
Belarus;tablesw/witr;TRUE
Belgium;tablesb/belg;FALSE
Bosnia and Herzegovina;tablesb/bih;FALSE
Bulgaria;tablesb/bulg;FALSE
Croatia;tablesk/kroa;FALSE
Cyprus;tablesc/cyp;FALSE
Czechia;tablest/tsje;FALSE
Denmark;tablesd/den;FALSE
Estonia;tablese/est;TRUE
Faroe Islands;tablesf/far;TRUE
Finland;tablesf/fin;TRUE
Georgia;tablesg/geor;TRUE
Gibraltar;tablesg/gib;FALSE
Greece;tablesg/grk;FALSE
Hungary;tablesh/hong;FALSE
Iceland;tablesi/ijs;TRUE
Israel;tablesi/isra;FALSE
Kazakhstan;tablesk/kaz;TRUE
Kosovo;tablesk/kosovo;FALSE
Latvia;tablesl/let;TRUE
Lithuania;tablesl/lito;TRUE
Luxembourg;tablesl/lux;FALSE
Malta;tablesm/malt;FALSE
Moldova;tablesm/mold;FALSE
Montenegro;tablesm/monteg;FALSE
Netherlands;tablesn/ned;FALSE
North Macedonia;tablesn/nmkd;FALSE
Northern Ireland;tablesn/nil;FALSE
Norway;tablesn/noo;TRUE
Poland;tablesp/pol;FALSE
Republic of Ireland;tablesi/ier;TRUE
Romania;tablesr/roem;FALSE
Russia;tablesr/rus;FALSE
San Marino;tabless/sanm;FALSE
Scotland;tabless/scot;FALSE
Serbia;tabless/serv;FALSE
Slovakia;tabless/slow;FALSE
Slovenia;tabless/slov;FALSE
Spain;tabless/span;FALSE
Sweden;tablesz/zwed;TRUE
Switzerland;tablesz/zwit;FALSE
Turkey;tablest/turk;FALSE
Ukraine;tableso/oekr;FALSE
Wales;tablesw/wal;FALSE", sep = ";", colClasses = c(Calendar = "logical"))

# Historical alternative used on RSSSF for North Macedonia.
prefixes <- rbind(
  config[, .(Country, Prefix, Calendar)],
  config[Country == "North Macedonia", .(Country, Prefix = "tablesf/fyrom", Calendar)]
)

normalise_href <- function(x) {
  x <- sub("^https?://(www\\.)?rsssf\\.(org|com)/", "", x, ignore.case = TRUE)
  x <- sub("^/+", "", x)
  while (grepl("^\\.\\./", x)) x <- sub("^\\.\\./", "", x)
  sub("[?#].*$", "", x)
}

read_index <- function(url) {
  cat("Reading RSSSF index: ", url, "\n", sep = "")
  doc <- read_html(url)
  nodes <- html_elements(doc, "a[href]")
  data.table(
    Label = trimws(html_text2(nodes)),
    Href = vapply(html_attr(nodes, "href"), normalise_href, character(1))
  )
}

index_urls <- c(
  "https://www.rsssf.org/resultsp2010.html",
  "https://www.rsssf.org/resultsp2020.html"
)
online <- read_index(index_urls[1])
Sys.sleep(0.5)
online <- rbind(online, read_index(index_urls[2]), fill = TRUE)
online <- unique(online[nzchar(Href)])

match_one_prefix <- function(country, prefix, calendar) {
  escaped <- gsub("([][{}()+*^$|\\?.])", "\\\\\\1", prefix)
  z <- online[grepl(paste0("^", escaped, "(?:[0-9]{2}|[0-9]{4})\\.html$"), Href,
                    ignore.case = TRUE)]
  if (!nrow(z)) return(NULL)
  z[, Suffix := sub(paste0("^", escaped), "", Href, ignore.case = TRUE)]
  z[, Suffix := sub("\\.html$", "", Suffix, ignore.case = TRUE)]
  z[, PageYear := suppressWarnings(as.integer(Suffix))]
  z[nchar(Suffix) == 2L, PageYear := fifelse(PageYear <= 29L, 2000L + PageYear, 1900L + PageYear)]
  z[, `:=`(Country = country, Prefix = prefix, Calendar = calendar)]
  z[PageYear >= 2010L & PageYear <= 2025L]
}

cat("Matching the remaining UEFA league page names...\n")
found <- rbindlist(Map(match_one_prefix, prefixes$Country, prefixes$Prefix, prefixes$Calendar),
                   fill = TRUE)
if (!nrow(found)) {
  stop("No matching league pages were found in the RSSSF indexes; check index layout or network response.")
}
found[, OnlineURL := paste0("https://www.rsssf.org/", Href)]

cat("Indexing local RSSSF HTML files...\n")
local_roots <- c(
  file.path(root, "EuropeanFootball/pipeline_data/Source/rsssf/all"),
  file.path(root, "EuropeanFootball/pipeline_data/Source/rsssf/uefa_raw"),
  file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources")
)
local_roots <- local_roots[dir.exists(local_roots)]
local_files <- unlist(lapply(local_roots, list.files, pattern = "\\.html$", recursive = TRUE,
                             full.names = TRUE, ignore.case = TRUE), use.names = FALSE)
local_norm <- tolower(gsub("\\\\", "/", local_files))

find_local <- function(href) {
  h <- tolower(href)
  encoded <- gsub("/", "_", h, fixed = TRUE)
  hit <- which(endsWith(local_norm, paste0("/", h)) |
                 grepl(paste0(encoded, "(?:\\.html)?$"), local_norm))
  if (length(hit)) normalizePath(local_files[hit[1]], winslash = "/", mustWork = FALSE) else NA_character_
}
found[, LocalFile := vapply(Href, find_local, character(1))]
found[, LocalCached := !is.na(LocalFile)]

cat("Reading the production master...\n")
stopifnot(file.exists(master_path))
m <- fread(master_path, select = c("Season", "Country", "CompetitionType", "Tier", "Date",
                                    "Home", "Away", "Result"), showProgress = FALSE)
m <- m[CompetitionType == "league" & Tier == 1L & nzchar(Home) & nzchar(Away) & nzchar(Result)]
m[, PageYear := suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", Season)))]
m[grepl("^[0-9]{4}/[0-9]{2}$", Season), PageYear := PageYear + 1L]
m[is.na(PageYear), PageYear := as.integer(format(as.IDate(Date), "%Y"))]
m[, Country := fcase(
  Country == "Türkiye", "Turkey",
  default = as.character(Country)
)]
master_counts <- m[Country %in% config$Country & PageYear >= 2010L & PageYear <= 2025L,
                   .(MasterMatches = .N), by = .(Country, PageYear)]

# Make one row for every requested country-season, including seasons for which
# RSSSF has no matching link in its decade indexes.
targets <- rbindlist(lapply(seq_len(nrow(config)), function(i) {
  yrs <- if (isTRUE(config$Calendar[i])) 2010:2025 else 2011:2025
  data.table(Country = config$Country[i], Calendar = config$Calendar[i], PageYear = yrs)
}))

found_year <- found[, .(
  OnlinePageListed = TRUE,
  OnlineLabels = paste(sort(unique(Label[nzchar(Label)])), collapse = " | "),
  OnlineURLs = paste(sort(unique(OnlineURL)), collapse = " | "),
  OnlineHrefs = paste(sort(unique(Href)), collapse = " | "),
  LocalCached = any(LocalCached),
  LocalFiles = paste(sort(unique(LocalFile[!is.na(LocalFile)])), collapse = " | ")
), by = .(Country, PageYear)]

audit <- merge(targets, found_year, by = c("Country", "PageYear"), all.x = TRUE)
audit <- merge(audit, master_counts, by = c("Country", "PageYear"), all.x = TRUE)
audit[is.na(OnlinePageListed), OnlinePageListed := FALSE]
audit[is.na(LocalCached), LocalCached := FALSE]
audit[is.na(MasterMatches), MasterMatches := 0L]
audit[, Season := fifelse(Calendar, as.character(PageYear),
                          sprintf("%d/%02d", PageYear - 1L, PageYear %% 100L))]
audit[, Status := fcase(
  OnlinePageListed & !LocalCached & MasterMatches == 0L, "ONLINE_NOT_CACHED_AND_NO_MASTER_GAMES",
  OnlinePageListed & !LocalCached, "ONLINE_NOT_CACHED",
  OnlinePageListed & LocalCached & MasterMatches == 0L, "CACHED_BUT_NO_MASTER_GAMES",
  !OnlinePageListed & MasterMatches == 0L, "NO_INDEX_PAGE_AND_NO_MASTER_GAMES",
  !OnlinePageListed, "NO_INDEX_PAGE_BUT_MASTER_HAS_GAMES",
  default = "CACHED_AND_REPRESENTED"
)]
setcolorder(audit, c("Country", "Season", "PageYear", "Calendar", "MasterMatches",
                     "OnlinePageListed", "LocalCached", "Status", "OnlineLabels",
                     "OnlineURLs", "OnlineHrefs", "LocalFiles"))
setorder(audit, Country, PageYear)

summary <- audit[, .(
  SeasonsChecked = .N,
  MasterSeasons = sum(MasterMatches > 0L),
  OnlinePagesListed = sum(OnlinePageListed),
  CachedPages = sum(LocalCached),
  OnlineNotCached = sum(OnlinePageListed & !LocalCached),
  OnlineNotCachedAndNoMasterGames = sum(OnlinePageListed & !LocalCached & MasterMatches == 0L),
  CachedButNoMasterGames = sum(OnlinePageListed & LocalCached & MasterMatches == 0L),
  NoIndexPageAndNoMasterGames = sum(!OnlinePageListed & MasterMatches == 0L)
), by = Country]
setorder(summary, Country)

opportunities <- audit[Status %chin% c(
  "ONLINE_NOT_CACHED_AND_NO_MASTER_GAMES",
  "ONLINE_NOT_CACHED",
  "CACHED_BUT_NO_MASTER_GAMES"
)]

fwrite(audit, file.path(out_dir, "season_page_inventory.csv"))
fwrite(summary, file.path(out_dir, "country_summary.csv"))
fwrite(opportunities, file.path(out_dir, "recovery_opportunities.csv"))

cat("\nRemaining UEFA online-page discovery summary:\n")
print(summary[OnlineNotCached > 0L | CachedButNoMasterGames > 0L])
cat("\nPotential recovery seasons: ", nrow(opportunities), "\n", sep = "")
cat("Online pages were only discovered, not downloaded. Production master unchanged.\n")
cat("Review: ", file.path(out_dir, "recovery_opportunities.csv"), "\n", sep = "")
