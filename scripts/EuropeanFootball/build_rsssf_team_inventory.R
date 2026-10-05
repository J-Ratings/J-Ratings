# Build a review-only inventory of every club appearing in the RSSSF-rebuilt
# UEFA domestic leagues. No production data or aliases are changed.
#
# Run from anywhere:
# source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/build_rsssf_team_inventory.R")

required <- c("data.table", "stringi", "tictoc", "beepr")
missing_packages <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install these packages first: ", paste(missing_packages, collapse = ", "))
}

library(data.table)

build_rsssf_team_inventory <- function() {
  tictoc::tic("RSSSF team identity inventory")
  on.exit({
    tictoc::toc()
    try(suppressWarnings(beepr::beep()), silent = TRUE)
  }, add = TRUE)

  root <- normalizePath(
    Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
    winslash = "/", mustWork = TRUE
  )
  base <- file.path(root, "EuropeanFootball/pipeline_data")
  rebuild_dir <- file.path(base, "Manual_Sources/RSSSF_UEFA_Rebuild")
  master_path <- file.path(base, "Matches_Clean_Combined/european_football_all_matches.csv")
  audit_path <- file.path(rebuild_dir, "season_audit.csv")
  aliases_path <- file.path(base, "Reference/team_aliases.csv")
  output_dir <- file.path(rebuild_dir, "team_identity_inventory")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.exists(master_path), file.exists(audit_path), file.exists(aliases_path))

  # Conservative source-name normalisation. It removes typography and common
  # football-club prefixes, but does not perform substring matching or merge
  # fuzzy names. Those remain review candidates in a separate file.
  team_key <- function(x) {
    x <- trimws(as.character(x))
    x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
    x <- gsub("&", " and ", x, fixed = TRUE)
    x <- gsub("['`.]", "", x)
    x <- gsub("[-_/()]", " ", x)
    x <- gsub("\\b(?:fc|fk|sk|nk|if|ifk|bk|ks|ac|afc|cf|ff|as|ss)\\b", " ", x, perl = TRUE)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  master <- fread(master_path, encoding = "UTF-8")
  audit <- fread(audit_path, encoding = "UTF-8")
  aliases <- fread(aliases_path, encoding = "UTF-8", na.strings = c("", "NA"))
  countries <- sort(unique(audit$Country))
  if (length(countries) != 38L) stop("Expected 38 RSSSF rebuild countries.")

  domestic <- master[
    Country %chin% countries & CompetitionType == "league" & as.integer(Tier) == 1L,
    .(Country, League, Season, Date = as.IDate(Date), Home, Away)
  ]
  if (!nrow(domestic)) stop("No Tier 1 domestic rows found for the RSSSF countries.")

  appearances <- rbindlist(list(
    domestic[, .(Country, League, Season, Date, RawName = Home)],
    domestic[, .(Country, League, Season, Date, RawName = Away)]
  ))
  appearances[, RawName := trimws(RawName)]
  appearances <- appearances[!is.na(RawName) & nzchar(RawName)]
  appearances[, NormalisedName := team_key(RawName)]
  appearances <- appearances[nzchar(NormalisedName)]

  aliases[, `:=`(
    Country = trimws(as.character(Country)),
    SourceName = trimws(as.character(SourceName)),
    CanonicalName = trimws(as.character(CanonicalName))
  )]
  aliases <- unique(aliases[
    Country %chin% countries & !is.na(SourceName) & nzchar(SourceName) &
      !is.na(CanonicalName) & nzchar(CanonicalName),
    .(Country, RawName = SourceName, ExistingCanonicalName = CanonicalName)
  ], by = c("Country", "RawName"))

  inventory <- appearances[, .(
    Games = .N,
    Seasons = uniqueN(Season),
    FirstDate = min(Date),
    LastDate = max(Date),
    FirstSeason = Season[which.min(Date)],
    LastSeason = Season[which.max(Date)]
  ), by = .(Country, League, RawName, NormalisedName)]
  inventory <- aliases[inventory, on = .(Country, RawName)]
  inventory[, ExistingCanonicalName := fifelse(
    is.na(ExistingCanonicalName) | !nzchar(ExistingCanonicalName), "", ExistingCanonicalName
  )]

  group_summary <- inventory[, {
    ranked <- .SD[order(-Games, -as.integer(LastDate), RawName)]
    canonical_aliases <- unique(ExistingCanonicalName[nzchar(ExistingCanonicalName)])
    list(
      VariantCount = .N,
      TotalGames = sum(Games),
      SuggestedDisplayName = ranked$RawName[1L],
      ExistingCanonicalNames = paste(canonical_aliases, collapse = " | "),
      RawVariants = paste(ranked$RawName, collapse = " | "),
      MergeStatus = fifelse(
        .N == 1L, "single_name",
        fifelse(length(canonical_aliases) == 1L, "already_mapped_to_one_canonical_name",
               "normalisation_merge_candidate")
      )
    )
  }, by = .(Country, League, NormalisedName)]

  inventory <- group_summary[inventory, on = .(Country, League, NormalisedName)]
  setcolorder(inventory, c(
    "Country", "League", "SuggestedDisplayName", "MergeStatus", "VariantCount",
    "RawVariants", "ExistingCanonicalNames", "RawName", "NormalisedName",
    "ExistingCanonicalName", "Games", "Seasons", "FirstSeason", "LastSeason",
    "FirstDate", "LastDate", "TotalGames"
  ))
  setorder(inventory, Country, League, SuggestedDisplayName, RawName)

  # Review-only spelling neighbours: similar normalised names within one
  # country, never automatically merged. Requiring the same first character,
  # compatible length and edit distance <= 2 keeps this list manageable.
  groups <- unique(group_summary[, .(
    Country, League, NormalisedName, SuggestedDisplayName, VariantCount,
    TotalGames, RawVariants, MergeStatus
  )])
  spelling_reviews <- lapply(split(groups, groups$Country), function(x) {
    x <- as.data.table(x)
    if (nrow(x) < 2L) return(NULL)
    pairs <- combn(seq_len(nrow(x)), 2L)
    out <- vector("list", ncol(pairs))
    k <- 0L
    for (j in seq_len(ncol(pairs))) {
      left <- x[pairs[1L, j]]
      right <- x[pairs[2L, j]]
      a <- left$NormalisedName
      b <- right$NormalisedName
      if (!nzchar(a) || !nzchar(b) || substr(a, 1L, 1L) != substr(b, 1L, 1L)) next
      if (abs(nchar(a) - nchar(b)) > 2L) next
      distance <- as.integer(adist(a, b))
      if (distance > 2L) next
      k <- k + 1L
      out[[k]] <- data.table(
        Country = left$Country,
        LeftName = left$SuggestedDisplayName,
        RightName = right$SuggestedDisplayName,
        LeftNormalisedName = a,
        RightNormalisedName = b,
        EditDistance = distance,
        LeftVariants = left$RawVariants,
        RightVariants = right$RawVariants,
        ReviewStatus = "manual_review_required"
      )
    }
    rbindlist(out[seq_len(k)], fill = TRUE)
  })
  spelling_reviews <- rbindlist(spelling_reviews, fill = TRUE)
  if (!nrow(spelling_reviews)) {
    spelling_reviews <- data.table(
      Country = character(), LeftName = character(), RightName = character(),
      LeftNormalisedName = character(), RightNormalisedName = character(),
      EditDistance = integer(), LeftVariants = character(), RightVariants = character(),
      ReviewStatus = character()
    )
  }
  setorder(spelling_reviews, Country, EditDistance, LeftName, RightName)

  exact_groups <- group_summary[VariantCount > 1L]
  setorder(exact_groups, Country, SuggestedDisplayName)
  country_summary <- inventory[, .(
    RawNames = .N,
    NormalisedGroups = uniqueN(NormalisedName),
    ExactNormalisationGroups = uniqueN(NormalisedName[VariantCount > 1L]),
    Games = sum(Games),
    FirstDate = min(FirstDate),
    LastDate = max(LastDate)
  ), by = .(Country, League)]
  setorder(country_summary, Country, League)

  fwrite(inventory, file.path(output_dir, "all_teams_by_league.csv"), na = "")
  fwrite(exact_groups, file.path(output_dir, "normalisation_merge_groups.csv"), na = "")
  fwrite(spelling_reviews, file.path(output_dir, "possible_spelling_duplicates.csv"), na = "")
  fwrite(country_summary, file.path(output_dir, "league_team_summary.csv"), na = "")

  print(country_summary)
  message(
    "Wrote ", nrow(inventory), " raw team-name rows across ", nrow(country_summary),
    " leagues. Exact normalisation groups: ", nrow(exact_groups),
    ". Spelling-review pairs: ", nrow(spelling_reviews), "."
  )
  message("Review ", output_dir, ". No aliases or production data were changed.")
  invisible(list(
    inventory = inventory, exact_groups = exact_groups,
    spelling_reviews = spelling_reviews, summary = country_summary
  ))
}

build_rsssf_team_inventory()
