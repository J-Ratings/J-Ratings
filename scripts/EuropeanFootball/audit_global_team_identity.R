# Review every domestic club identity in the master dataset before applying
# aliases. This script never changes the master CSV, aliases, Elo files, or
# site data.
#
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/audit_global_team_identity.R")

required <- c("data.table", "stringi", "tictoc", "beepr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required package(s): ", paste(missing, collapse = ", "))

library(data.table)

audit_global_team_identity <- function() {
  tictoc::tic("Global team identity audit")
  on.exit({
    tictoc::toc()
    try(suppressWarnings(beepr::beep()), silent = TRUE)
  }, add = TRUE)

  root <- normalizePath(
    Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
    winslash = "/", mustWork = TRUE
  )
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  master_path <- file.path(base, "Matches_Clean_Combined/european_football_all_matches.csv")
  aliases_path <- file.path(base, "Reference/team_aliases.csv")
  out_dir <- file.path(base, "Manual_Sources/Team_Identity_Audit")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  normalise_name <- function(x) {
    x <- trimws(as.character(x))
    x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
    x <- gsub("&", " and ", x, fixed = TRUE)
    x <- gsub("['`.]", "", x)
    x <- gsub("[-_/()]", " ", x)
    x <- gsub("\\b(?:fc|fk|sk|nk|if|ifk|bk|ks|ac|afc|cf|ff|as|ss)\\b", " ", x, perl = TRUE)
    trimws(gsub("\\s+", " ", x))
  }

  # This key expands common RSSSF suffix abbreviations. It is deliberately
  # review-only: a shared key suggests a relationship; it never merges clubs.
  abbreviation_key <- function(x) {
    x <- normalise_name(x)
    x <- gsub("\\b(?:t)\\b", "town", x, perl = TRUE)
    x <- gsub("\\b(?:c)\\b", "city", x, perl = TRUE)
    x <- gsub("\\b(?:u|utd)\\b", "united", x, perl = TRUE)
    x <- gsub("\\b(?:n)\\b", "nomads", x, perl = TRUE)
    x <- gsub("\\b(?:co)\\b", "county", x, perl = TRUE)
    x <- gsub("\\b(?:r)\\b", "rovers", x, perl = TRUE)
    trimws(gsub("\\s+", " ", x))
  }

  # A looser key catches historical shortening/renaming chains such as Barry,
  # Barry T, and Barry Town. It is evidence for human review, never a rule for
  # automatically joining clubs.
  root_key <- function(x) {
    x <- abbreviation_key(x)
    x <- gsub("\\b(?:town|city|united|county|nomads|rovers|wanderers|athletic|academy)\\b", " ", x, perl = TRUE)
    trimws(gsub("\\s+", " ", x))
  }

  master <- fread(master_path, encoding = "UTF-8")
  aliases <- fread(aliases_path, encoding = "UTF-8", na.strings = c("", "NA"))
  stopifnot(all(c("Country", "SourceName", "CanonicalName") %in% names(aliases)))

  domestic <- master[
    CompetitionType == "league" & !is.na(Country) & nzchar(Country) &
      Country != "Europe" & !is.na(Date) & nzchar(Date),
    .(Country, Season, Date = as.IDate(Date), Home, Away)
  ]
  appearances <- rbindlist(list(
    domestic[, .(Country, Season, Date, RawName = Home)],
    domestic[, .(Country, Season, Date, RawName = Away)]
  ))
  appearances[, RawName := trimws(as.character(RawName))]
  appearances <- appearances[!is.na(RawName) & nzchar(RawName)]
  appearances[, `:=`(
    NormalisedName = normalise_name(RawName),
    AbbreviationKey = abbreviation_key(RawName),
    RootKey = root_key(RawName)
  )]
  appearances <- appearances[nzchar(NormalisedName)]

  aliases[, `:=`(
    Country = trimws(as.character(Country)),
    SourceName = trimws(as.character(SourceName)),
    CanonicalName = trimws(as.character(CanonicalName))
  )]
  aliases <- unique(aliases[
    !is.na(Country) & nzchar(Country) & !is.na(SourceName) & nzchar(SourceName) &
      !is.na(CanonicalName) & nzchar(CanonicalName),
    .(Country, RawName = SourceName, ExistingCanonicalName = CanonicalName)
  ], by = c("Country", "RawName"))

  inventory <- appearances[, .(
    Appearances = .N,
    Seasons = uniqueN(Season),
    SeasonSet = list(unique(Season)),
    FirstDate = min(Date),
    LastDate = max(Date),
    FirstSeason = Season[which.min(Date)],
    LastSeason = Season[which.max(Date)]
  ), by = .(Country, RawName, NormalisedName, AbbreviationKey, RootKey)]
  inventory <- aliases[inventory, on = .(Country, RawName)]
  inventory[is.na(ExistingCanonicalName), ExistingCanonicalName := ""]

  make_groups <- function(key_column, candidate_type, require_distinct_normalised = FALSE) {
    groups <- inventory[, {
      variants <- .SD[order(-Appearances, RawName)]
      alias_names <- unique(variants$ExistingCanonicalName[nzchar(variants$ExistingCanonicalName)])
      list(
        VariantCount = .N,
        NormalisedVariantCount = uniqueN(variants$NormalisedName),
        Appearances = sum(variants$Appearances),
        FirstDate = min(variants$FirstDate),
        LastDate = max(variants$LastDate),
        RawVariants = paste(variants$RawName, collapse = " | "),
        ExistingCanonicalNames = paste(alias_names, collapse = " | "),
        HasOverlappingSeasons = anyDuplicated(unlist(variants$SeasonSet, use.names = FALSE)) > 0L
      )
    }, by = c("Country", key_column)]
    setnames(groups, key_column, "CandidateKey")
    if (require_distinct_normalised) groups <- groups[NormalisedVariantCount > 1L]
    groups[, CandidateType := candidate_type]
    groups[VariantCount > 1L]
  }

  exact <- make_groups("NormalisedName", "typography_or_prefix", FALSE)
  abbrev <- make_groups("AbbreviationKey", "abbreviation_expansion", TRUE)
  roots <- make_groups("RootKey", "historical_name_chain", TRUE)

  candidates <- rbindlist(list(exact, abbrev, roots), fill = TRUE)
  candidates <- unique(candidates, by = c("Country", "CandidateType", "CandidateKey"))
  candidates[, ReviewPriority := fifelse(
    CandidateType == "typography_or_prefix" & !HasOverlappingSeasons, "high",
    fifelse(CandidateType == "abbreviation_expansion" & !HasOverlappingSeasons, "medium", "manual")
  )]
  setcolorder(candidates, c(
    "ReviewPriority", "CandidateType", "Country", "CandidateKey", "VariantCount",
    "NormalisedVariantCount", "Appearances", "FirstDate", "LastDate",
    "HasOverlappingSeasons", "RawVariants", "ExistingCanonicalNames"
  ))
  setorder(candidates, ReviewPriority, Country, CandidateType, -Appearances)

  # A country-level view shows where a review will have the greatest impact.
  country_summary <- candidates[, .(
    CandidateGroups = .N,
    HighPriority = sum(ReviewPriority == "high"),
    MediumPriority = sum(ReviewPriority == "medium"),
    ManualReview = sum(ReviewPriority == "manual"),
    AffectedAppearances = sum(Appearances)
  ), by = Country][order(-CandidateGroups, Country)]

  inventory_output <- copy(inventory)[, SeasonSet := NULL]
  fwrite(inventory_output, file.path(out_dir, "all_domestic_team_names.csv"), na = "")
  fwrite(candidates, file.path(out_dir, "identity_merge_candidates.csv"), na = "")
  fwrite(country_summary, file.path(out_dir, "country_summary.csv"), na = "")

  cat("Domestic raw identities:", nrow(inventory), "\n")
  cat("Review candidates:", nrow(candidates), "\n")
  print(country_summary)
  message("Review files written to: ", out_dir)
  message("No aliases, matches, Elo files, or site data were changed.")
  invisible(list(inventory = inventory, candidates = candidates, summary = country_summary))
}

audit_global_team_identity()
