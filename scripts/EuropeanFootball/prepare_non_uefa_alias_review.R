# Build reviewable RSSSF -> Wikipedia team-name suggestions for non-UEFA clubs.
# Read-only with respect to production data: suggestions are never auto-applied.

suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
  library(tictoc)
  library(beepr)
})

tictoc::tic("Non-UEFA alias review preparation")
on.exit({tictoc::toc(); try(suppressWarnings(beepr::beep()), silent = TRUE)}, add = TRUE)

root <- normalizePath(Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
                      winslash = "/", mustWork = TRUE)
wiki_path <- Sys.getenv(
  "NON_UEFA_WIKIPEDIA_NAMES",
  "C:/Users/stjuk/Downloads/non_uefa_wikipedia_team_names_2025_26.csv"
)
base <- file.path(root, "EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit")
domestic_path <- file.path(base, "all_rsssf_games.csv")
continental_path <- file.path(base, "continental_rebuild/parsed_games.csv")
out <- file.path(base, "team_identity_inventory")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
for (p in c(wiki_path, domestic_path, continental_path)) if (!file.exists(p)) stop("Missing input: ", p)

normalise <- function(x) {
  x <- stri_trans_general(trimws(as.character(x)), "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  # Club/legal suffixes add little identity evidence and vary heavily by source.
  x <- gsub("\\b(?:fc|cf|sc|ac|afc|fk|sk|bk|cd|club|football|futbol|futebol|soccer)\\b", " ", x, perl = TRUE)
  gsub("[^a-z0-9]+", "", x)
}
tokens <- function(x) {
  x <- tolower(stri_trans_general(as.character(x), "Latin-ASCII"))
  unique(Filter(nzchar, strsplit(gsub("[^a-z0-9]+", " ", x), " +")[[1L]]))
}
similarity <- function(a, b) {
  na <- normalise(a); nb <- normalise(b)
  if (!nzchar(na) || !nzchar(nb)) return(0)
  edit <- 1 - as.numeric(adist(na, nb)) / max(nchar(na), nchar(nb))
  ta <- tokens(a); tb <- tokens(b)
  jac <- if (length(union(ta, tb))) length(intersect(ta, tb)) / length(union(ta, tb)) else 0
  containment <- if (nchar(na) >= 4L && nchar(nb) >= 4L && (grepl(na, nb, fixed = TRUE) || grepl(nb, na, fixed = TRUE))) 1 else 0
  round(max(edit, 0.72 * edit + 0.28 * jac, 0.88 * containment + 0.12 * edit), 4)
}

wiki <- fread(wiki_path, encoding = "UTF-8", na.strings = c("", "NA"))
needed <- c("Confederation", "Association", "League", "WikipediaTeam", "TeamListSeason", "CoverageStatus")
missing <- setdiff(needed, names(wiki))
if (length(missing)) stop("Wikipedia-name file lacks: ", paste(missing, collapse = ", "))
wiki <- unique(wiki[!is.na(WikipediaTeam) & nzchar(trimws(WikipediaTeam)), .(
  Confederation, Association, League, WikipediaTeam = trimws(WikipediaTeam),
  TeamListSeason, CoverageStatus
)])
wiki[, NormalisedWikipedia := normalise(WikipediaTeam)]
wiki[, WikipediaTokens := lapply(WikipediaTeam, tokens)]

country_alias <- c(
  "Hongkong" = "Hong Kong", "Macao" = "Macau", "East Timor" = "Timor-Leste",
  "Congo-Brazzaville" = "Congo", "Congo-Kinshasa" = "DR Congo",
  "Guinea Bissau" = "Guinea-Bissau", "French Guyana" = "French Guiana",
  "US Virgin Islands" = "United States Virgin Islands", "Surinam" = "Suriname",
  "Fiji (clubs)" = "Fiji", "Vanuatu (PVFL)" = "Vanuatu", "Vanuatu (VFFCL)" = "Vanuatu"
)

dom <- fread(domestic_path, select = c("Country", "Confederation", "StartYear", "Home", "Away", "CompetitionType"),
             encoding = "UTF-8", na.strings = c("", "NA"))
dom <- dom[CompetitionType == "league"]
dom[, Association := fifelse(Country %chin% names(country_alias), unname(country_alias[Country]), Country)]
available <- unique(wiki$Association)
dom <- dom[Association %chin% available]
latest <- dom[, .(StartYear = max(as.integer(StartYear), na.rm = TRUE)), by = .(Confederation, Association)]
dom <- dom[latest, on = .(Confederation, Association, StartYear)]
dom_names <- unique(rbindlist(list(
  dom[, .(Confederation, Association, RSSSFName = trimws(Home), StartYear)],
  dom[, .(Confederation, Association, RSSSFName = trimws(Away), StartYear)]
)))[nzchar(RSSSFName)]
dom_names[, NormalisedRSSSF := normalise(RSSSFName)]

similarities <- function(source_name, pool) {
  na <- normalise(source_name)
  nb <- pool$NormalisedWikipedia
  if (!nzchar(na) || !length(nb)) return(rep(0, length(nb)))
  edit <- 1 - as.numeric(adist(na, nb)) / pmax(nchar(na), nchar(nb))
  ta <- tokens(source_name)
  jac <- vapply(pool$WikipediaTokens, function(tb) {
    u <- union(ta, tb)
    if (length(u)) length(intersect(ta, tb)) / length(u) else 0
  }, numeric(1))
  containment <- as.numeric(nchar(na) >= 4L & nchar(nb) >= 4L &
    (grepl(na, nb, fixed = TRUE) | vapply(nb, function(x) grepl(x, na, fixed = TRUE), logical(1))))
  round(pmax(edit, 0.72 * edit + 0.28 * jac, 0.88 * containment + 0.12 * edit), 4)
}

rank_candidates <- function(source_names, candidate_pool, within_association = TRUE, top_n = 3L) {
  rows <- vector("list", nrow(source_names))
  for (i in seq_len(nrow(source_names))) {
    if (i %% 250L == 0L) message("  Compared ", i, "/", nrow(source_names), " team names")
    src <- source_names[i]
    pool <- if (within_association)
      candidate_pool[Confederation == src$Confederation & Association == src$Association] else candidate_pool
    if (!nrow(pool)) next
    exact <- pool[NormalisedWikipedia == src$NormalisedRSSSF]
    if (nrow(exact) == 1L) {
      exact[, `:=`(RSSSFName = src$RSSSFName, CandidateRank = 1L, Similarity = 1,
                   SuggestedDecision = "accept_normalised_exact")]
      rows[[i]] <- exact
      next
    }
    pool <- copy(pool)
    pool[, Similarity := similarities(src$RSSSFName, pool)]
    setorder(pool, -Similarity, WikipediaTeam)
    pool <- head(pool, top_n)
    pool[, `:=`(RSSSFName = src$RSSSFName, CandidateRank = seq_len(.N))]
    pool[, SuggestedDecision := fifelse(
      CandidateRank == 1L & Similarity >= 0.92, "review_strong",
      fifelse(CandidateRank == 1L & Similarity >= 0.78, "review_possible", "review_weak")
    )]
    rows[[i]] <- pool
  }
  rbindlist(rows, fill = TRUE)
}

domestic_review_path <- file.path(out, "domestic_alias_review.csv")
reuse_domestic <- Sys.getenv("NON_UEFA_REUSE_DOMESTIC", "1") == "1" && file.exists(domestic_review_path)
if (reuse_domestic) {
  message("Reusing completed domestic alias review (domestic input is unchanged)...")
  domestic_review <- fread(domestic_review_path, encoding = "UTF-8", na.strings = c("", "NA"))
} else {
  message("Comparing current domestic RSSSF names within association...")
  domestic_review <- rank_candidates(dom_names, wiki, within_association = TRUE)
  domestic_review <- domestic_review[, .(
    Confederation, Association, League, TeamListSeason, CoverageStatus,
    RSSSFName, WikipediaCandidate = WikipediaTeam, CandidateRank, Similarity, SuggestedDecision
  )]
  fwrite(domestic_review, domestic_review_path, bom = TRUE)
}

continental <- fread(continental_path, select = c("Confederation", "Home", "Away"),
                     encoding = "UTF-8", na.strings = c("", "NA"))
continental_names <- unique(rbindlist(list(
  continental[, .(Confederation, RSSSFName = trimws(Home))],
  continental[, .(Confederation, RSSSFName = trimws(Away))]
)))[nzchar(RSSSFName)]
continental_names[, `:=`(Association = "", NormalisedRSSSF = normalise(RSSSFName))]
message("Comparing continental RSSSF names against Wikipedia names in the same confederation...")
continental_review <- rbindlist(lapply(unique(continental_names$Confederation), function(cf) {
  rank_candidates(continental_names[Confederation == cf], wiki[Confederation == cf],
                  within_association = FALSE)
}), fill = TRUE)
continental_review <- continental_review[, .(
  Confederation, Association, League, TeamListSeason, CoverageStatus,
  RSSSFName, WikipediaCandidate = WikipediaTeam, CandidateRank, Similarity, SuggestedDecision
)]
fwrite(continental_review, file.path(out, "continental_alias_review.csv"), bom = TRUE)

summary <- rbindlist(list(
  domestic_review[, .(Scope = "domestic", Suggestions = .N,
                      SourceTeams = uniqueN(RSSSFName), ExactAccepted = uniqueN(RSSSFName[SuggestedDecision == "accept_normalised_exact"]),
                      StrongReview = uniqueN(RSSSFName[SuggestedDecision == "review_strong"]))],
  continental_review[, .(Scope = "continental", Suggestions = .N,
                         SourceTeams = uniqueN(RSSSFName), ExactAccepted = uniqueN(RSSSFName[SuggestedDecision == "accept_normalised_exact"]),
                         StrongReview = uniqueN(RSSSFName[SuggestedDecision == "review_strong"]))]
))
fwrite(summary, file.path(out, "alias_review_summary.csv"), bom = TRUE)
print(summary)
message("Review files written to: ", out)
message("No aliases applied. Production master unchanged.")
