# Add conservative, globally-derived historical team aliases.
#
# The existing Wikipedia-backed aliases are treated as canonical anchors. A
# previously unseen RSSSF spelling is added only when it has one unambiguous
# canonical target in the same country and there is no evidence that the two
# names represented different teams.
#
# Dry run:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/apply_safe_global_team_aliases.R")
#
# Apply accepted aliases (a timestamped backup is made first):
# Sys.setenv(APPLY_SAFE_GLOBAL_ALIASES = "1")
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/apply_safe_global_team_aliases.R")

suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
})

root <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = TRUE
)

base <- file.path(root, "EuropeanFootball/pipeline_data")
master_path <- file.path(base, "Matches_Clean_Combined/european_football_all_matches.csv")
alias_path <- file.path(base, "Reference/team_aliases.csv")
out_dir <- file.path(base, "Manual_Sources/Team_Identity_Audit/safe_global_aliases")
apply_changes <- identical(Sys.getenv("APPLY_SAFE_GLOBAL_ALIASES", "0"), "1")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (path in c(master_path, alias_path)) {
  if (!file.exists(path)) stop("Required file not found: ", path)
}

normalise_name <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("['`.]", "", x)
  x <- gsub("[-_/()]", " ", x)
  x <- gsub("\\b(?:fc|fk|sk|nk|if|ifk|bk|ks|ac|afc|cf|ff|as|ss)\\b", " ", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x))
}

expand_abbreviations <- function(x) {
  x <- normalise_name(x)
  replacements <- c(
    "t" = "town", "c" = "city", "u" = "united", "utd" = "united",
    "co" = "county", "n" = "nomads", "r" = "rovers", "w" = "wanderers",
    "ath" = "athletic"
  )
  words <- strsplit(x, " ", fixed = TRUE)
  vapply(words, function(z) {
    hit <- z %chin% names(replacements)
    z[hit] <- replacements[z[hit]]
    paste(z, collapse = " ")
  }, character(1))
}

generic_words <- c(
  "town", "city", "united", "county", "nomads", "rovers", "wanderers",
  "athletic", "academy", "club", "football", "sporting", "association"
)

root_key <- function(x) {
  words <- strsplit(expand_abbreviations(x), " ", fixed = TRUE)
  vapply(words, function(z) paste(z[!z %chin% generic_words], collapse = " "), character(1))
}

generic_tokens <- function(x) {
  z <- strsplit(expand_abbreviations(x), " ", fixed = TRUE)[[1L]]
  intersect(z, generic_words)
}

similarity <- function(a, b) {
  if (!nzchar(a) || !nzchar(b)) return(0)
  1 - adist(a, b)[1L] / max(nchar(a), nchar(b))
}

master <- fread(master_path, encoding = "UTF-8", na.strings = c("", "NA"))
aliases <- fread(alias_path, encoding = "UTF-8", na.strings = c("", "NA"))

required_master <- c("Country", "CompetitionType", "Season", "Date", "Home", "Away")
required_alias <- c("Country", "SourceName", "CanonicalName")
if (length(setdiff(required_master, names(master)))) {
  stop("Master file is missing required columns: ", paste(setdiff(required_master, names(master)), collapse = ", "))
}
if (length(setdiff(required_alias, names(aliases)))) {
  stop("Alias file is missing required columns: ", paste(setdiff(required_alias, names(aliases)), collapse = ", "))
}

aliases[, `:=`(
  Country = trimws(as.character(Country)),
  SourceName = trimws(as.character(SourceName)),
  CanonicalName = trimws(as.character(CanonicalName))
)]
aliases <- unique(aliases[
  !is.na(Country) & nzchar(Country) &
    !is.na(SourceName) & nzchar(SourceName) &
    !is.na(CanonicalName) & nzchar(CanonicalName),
  .(Country, SourceName, CanonicalName)
])

alias_conflicts <- aliases[, .(CanonicalNames = uniqueN(CanonicalName)), by = .(Country, SourceName)][CanonicalNames > 1L]
if (nrow(alias_conflicts)) stop("Existing alias file contains conflicting country/source mappings.")

domestic <- master[
  CompetitionType == "league" & !is.na(Country) & nzchar(Country) &
    !is.na(Date) & nzchar(Date) & !is.na(Home) & nzchar(Home) & !is.na(Away) & nzchar(Away),
  .(
    Country = trimws(as.character(Country)),
    Season = as.character(Season),
    Date = as.IDate(Date),
    Home = trimws(as.character(Home)),
    Away = trimws(as.character(Away))
  )
]

appearances <- rbindlist(list(
  domestic[, .(Country, Season, Date, RawName = Home)],
  domestic[, .(Country, Season, Date, RawName = Away)]
))
appearances <- appearances[!is.na(RawName) & nzchar(RawName)]

inventory <- appearances[, .(
  Games = .N,
  Seasons = uniqueN(Season),
  FirstDate = min(Date),
  LastDate = max(Date)
), by = .(Country, RawName)]
inventory[, `:=`(
  Normalised = normalise_name(RawName),
  Expanded = expand_abbreviations(RawName),
  Root = root_key(RawName)
)]

existing_keys <- paste(aliases$Country, aliases$SourceName, sep = "\r")
inventory[, ExistingAlias := paste(Country, RawName, sep = "\r") %chin% existing_keys]
unresolved <- inventory[ExistingAlias == FALSE]

# Only country-specific aliases are anchors for domestic names. "Europe" and
# other confederation placeholders cannot safely identify a domestic club.
anchors <- aliases[Country != "Europe"]
anchors[, `:=`(
  SourceNormalised = normalise_name(SourceName),
  SourceExpanded = expand_abbreviations(SourceName),
  SourceRoot = root_key(SourceName),
  CanonicalNormalised = normalise_name(CanonicalName),
  CanonicalExpanded = expand_abbreviations(CanonicalName),
  CanonicalRoot = root_key(CanonicalName)
)]

name_dates <- unique(appearances[, .(Country, RawName, Date)])

has_identity_collision <- function(country, raw_name, canonical_name) {
  group_names <- unique(c(
    canonical_name,
    anchors[Country == country & CanonicalName == canonical_name, SourceName]
  ))
  group_names <- setdiff(group_names, raw_name)
  if (!length(group_names)) return(FALSE)

  direct <- domestic[
    Country == country &
      ((Home == raw_name & Away %chin% group_names) |
       (Away == raw_name & Home %chin% group_names)),
    .N
  ] > 0L
  if (direct) return(TRUE)

  raw_dates <- name_dates[Country == country & RawName == raw_name, unique(Date)]
  target_dates <- name_dates[Country == country & RawName %chin% group_names, unique(Date)]
  length(intersect(raw_dates, target_dates)) > 0L
}

candidate_rows <- vector("list", nrow(unresolved))
rejected_rows <- vector("list", nrow(unresolved))

for (i in seq_len(nrow(unresolved))) {
  row <- unresolved[i]
  country_anchors <- anchors[Country == row$Country]
  if (!nrow(country_anchors)) next

  # Score each canonical club using every Wikipedia-backed source variant that
  # already points to it. Exact normalisation is strongest, followed by a
  # suffix/abbreviation chain, then a very close spelling match.
  scored <- country_anchors[, {
    exact <- row$Normalised == CanonicalNormalised | row$Normalised %chin% SourceNormalised
    suffix_chain <- FALSE
    if (nzchar(row$Root) && nchar(row$Root) >= 4L && length(strsplit(row$Expanded, " ", fixed = TRUE)[[1L]]) >= 2L) {
      same_root <- row$Root == CanonicalRoot | row$Root %chin% SourceRoot
      suffix_evidence <- length(intersect(
        generic_tokens(row$RawName),
        unique(unlist(lapply(c(CanonicalName, SourceName), generic_tokens), use.names = FALSE))
      )) > 0L
      suffix_chain <- any(same_root) && suffix_evidence
    }

    comparison_names <- unique(c(CanonicalNormalised, SourceNormalised))
    fuzzy_scores <- vapply(comparison_names, function(z) similarity(row$Normalised, z), numeric(1))
    fuzzy <- nchar(row$Normalised) >= 7L && max(fuzzy_scores, na.rm = TRUE) >= 0.94

    list(
      Exact = any(exact),
      SuffixChain = any(suffix_chain),
      FuzzyScore = max(fuzzy_scores, na.rm = TRUE)
    )
  }, by = CanonicalName]

  scored[, MethodRank := fifelse(Exact, 3L, fifelse(SuffixChain, 2L, fifelse(FuzzyScore >= 0.94, 1L, 0L)))]
  scored <- scored[MethodRank > 0L][order(-MethodRank, -FuzzyScore, CanonicalName)]
  if (!nrow(scored)) next

  best_rank <- scored$MethodRank[1L]
  best <- scored[MethodRank == best_rank]
  if (best_rank == 1L) {
    best_score <- best$FuzzyScore[1L]
    runner_up <- if (nrow(scored) > 1L) scored$FuzzyScore[2L] else 0
    best <- best[FuzzyScore == max(FuzzyScore)]
    if (nrow(best) != 1L || best_score - runner_up < 0.08) {
      rejected_rows[[i]] <- data.table(
        Country = row$Country, SourceName = row$RawName,
        Reason = "fuzzy_match_not_unique", CandidateNames = paste(scored$CanonicalName[1:min(3L, .N)], collapse = " | ")
      )
      next
    }
  } else if (nrow(best) != 1L) {
    rejected_rows[[i]] <- data.table(
      Country = row$Country, SourceName = row$RawName,
      Reason = "multiple_canonical_targets", CandidateNames = paste(best$CanonicalName, collapse = " | ")
    )
    next
  }

  target <- best$CanonicalName[1L]
  if (identical(row$RawName, target)) next
  if (has_identity_collision(row$Country, row$RawName, target)) {
    rejected_rows[[i]] <- data.table(
      Country = row$Country, SourceName = row$RawName,
      Reason = "same_date_or_head_to_head_collision", CandidateNames = target
    )
    next
  }

  method <- c("fuzzy_unique", "historical_suffix_chain", "normalised_exact")[best_rank]
  candidate_rows[[i]] <- data.table(
    Country = row$Country,
    SourceName = row$RawName,
    CanonicalName = target,
    Method = method,
    Games = row$Games,
    Seasons = row$Seasons,
    FirstDate = row$FirstDate,
    LastDate = row$LastDate,
    FuzzyScore = round(best$FuzzyScore[1L], 4)
  )
}

candidates <- rbindlist(candidate_rows, fill = TRUE)
rejected <- rbindlist(rejected_rows, fill = TRUE)
if (!nrow(candidates)) {
  candidates <- data.table(
    Country = character(), SourceName = character(), CanonicalName = character(),
    Method = character(), Games = integer(), Seasons = integer(), FirstDate = as.IDate(character()),
    LastDate = as.IDate(character()), FuzzyScore = numeric()
  )
}
if (!nrow(rejected)) {
  rejected <- data.table(Country = character(), SourceName = character(), Reason = character(), CandidateNames = character())
}

candidates <- unique(candidates, by = c("Country", "SourceName"))
setorder(candidates, Country, CanonicalName, SourceName)
setorder(rejected, Country, SourceName)

candidate_conflicts <- candidates[, .(CanonicalNames = uniqueN(CanonicalName)), by = .(Country, SourceName)][CanonicalNames > 1L]
if (nrow(candidate_conflicts)) stop("Generated candidates contain conflicting targets; nothing was changed.")

summary <- candidates[, .(
  Aliases = .N,
  GamesRepresented = sum(Games),
  Exact = sum(Method == "normalised_exact"),
  HistoricalChains = sum(Method == "historical_suffix_chain"),
  Fuzzy = sum(Method == "fuzzy_unique")
), by = Country][order(-Aliases, Country)]

fwrite(candidates, file.path(out_dir, "accepted_safe_aliases.csv"), bom = TRUE)
fwrite(rejected, file.path(out_dir, "rejected_ambiguous_or_colliding.csv"), bom = TRUE)
fwrite(summary, file.path(out_dir, "country_summary.csv"), bom = TRUE)

cat("Unresolved domestic source names checked:", nrow(unresolved), "\n")
cat("Safe aliases accepted:", nrow(candidates), "\n")
cat("Represented match appearances:", sum(candidates$Games), "\n")
cat("Rejected ambiguous/colliding names:", nrow(rejected), "\n")
print(candidates[, .N, by = Method][order(Method)])
cat("Review files:", out_dir, "\n")

barry_check <- candidates[Country == "Wales" & SourceName %chin% c("Barry T", "Barry Town")]
if (nrow(barry_check)) {
  cat("\nBarry chain detected:\n")
  print(barry_check[, .(Country, SourceName, CanonicalName, Method)])
}

if (!apply_changes) {
  cat("\nDry run only. Set APPLY_SAFE_GLOBAL_ALIASES=1 and source this script again to apply.\n")
} else if (!nrow(candidates)) {
  cat("No new safe aliases to add.\n")
} else {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- file.path(dirname(alias_path), paste0("team_aliases_before_safe_global_", stamp, ".csv"))
  if (!file.copy(alias_path, backup_path, overwrite = FALSE)) {
    stop("Could not create alias backup; nothing was changed.")
  }

  updated <- rbindlist(list(
    aliases[, .(Country, SourceName, CanonicalName)],
    candidates[, .(Country, SourceName, CanonicalName)]
  ))
  updated <- unique(updated, by = c("Country", "SourceName"))
  setorder(updated, Country, SourceName)

  post_conflicts <- updated[, .(CanonicalNames = uniqueN(CanonicalName)), by = .(Country, SourceName)][CanonicalNames > 1L]
  if (nrow(post_conflicts)) stop("Post-merge alias conflict detected; original alias file was not overwritten.")

  fwrite(updated, alias_path, bom = TRUE)
  cat("\nAdded safe global aliases:", nrow(candidates), "\n")
  cat("Updated alias file:", alias_path, "\n")
  cat("Backup:", backup_path, "\n")
  cat("Next: run 02_calculate_elo.R, then 03_write_json.R.\n")
}
