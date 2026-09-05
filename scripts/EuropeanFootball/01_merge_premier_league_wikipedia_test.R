options(stringsAsFactors = FALSE)

script_start_time <- Sys.time()

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

season_folder <- Sys.getenv(
  "OPENFOOTBALL_SEASON",
  unset = {
    today <- Sys.Date()
    y <- as.integer(format(today, "%Y"))
    m <- as.integer(format(today, "%m"))
    start_year <- if (m >= 7L) y else y - 1L
    sprintf("%04d-%02d", start_year, (start_year + 1L) %% 100L)
  }
)

season_label <- gsub("-", "/", season_folder, fixed = TRUE)

wiki_file <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Source",
  "wikipedia",
  "premier_league",
  season_folder,
  "page.html"
)

combined_file <- file.path(
  repo_dir,
  "EuropeanFootball",
  "pipeline_data",
  "Matches_Clean_Combined",
  "european_football_all_matches.csv"
)

if (!file.exists(wiki_file)) {
  stop(
    "Wikipedia Premier League page not found:\n",
    wiki_file,
    "\nRun 00_download_premier_league_wikipedia_test.R first."
  )
}

if (!file.exists(combined_file)) {
  stop("Combined match CSV not found:\n", combined_file)
}

if (!requireNamespace("xml2", quietly = TRUE)) {
  stop("Package 'xml2' is required. Install it once with install.packages('xml2').")
}

clean_text <- function(x) {
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("\\[[0-9]+\\]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

club_key <- function(x) {
  x <- clean_text(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)

  # Wikipedia article-title forms -> OpenFootball-style bare club names.
  # Examples:
  #   Arsenal F.C.        -> arsenal
  #   Hull City A.F.C.    -> hull city
  #   AFC Bournemouth     -> bournemouth
  x <- gsub("^a\\s*f\\s*c\\s+", "", x, perl = TRUE)
  x <- gsub("^afc\\s+", "", x, perl = TRUE)
  x <- gsub("\\s+a\\s*f\\s*c$", "", x, perl = TRUE)
  x <- gsub("\\s+afc$", "", x, perl = TRUE)
  x <- gsub("\\s+f\\s*c$", "", x, perl = TRUE)
  x <- gsub("\\s+fc$", "", x, perl = TRUE)
  x <- gsub("\\s+football club$", "", x, perl = TRUE)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

result_code <- function(home_goals, away_goals) {
  if (home_goals > away_goals) {
    "1-0"
  } else if (home_goals < away_goals) {
    "0-1"
  } else {
    "0.5-0.5"
  }
}

cell_team_name <- function(cell) {
  link <- xml2::xml_find_first(cell, ".//a")

  if (!inherits(link, "xml_missing")) {
    title <- xml2::xml_attr(link, "title")
    if (!is.na(title) && nzchar(title)) {
      return(clean_text(title))
    }
    return(clean_text(xml2::xml_text(link)))
  }

  clean_text(xml2::xml_text(cell))
}

parse_wikipedia_results_matrix <- function(html_path) {
  doc <- xml2::read_html(html_path, encoding = "UTF-8")
  tables <- xml2::xml_find_all(doc, "//table")

  result_table <- NULL

  for (tbl in tables) {
    first_row <- xml2::xml_find_first(tbl, ".//tr[1]")
    if (inherits(first_row, "xml_missing")) next

    first_text <- clean_text(xml2::xml_text(first_row))

    # The Premier League results matrix has a Home/Away corner heading.
    if (
      grepl("Home", first_text, ignore.case = TRUE) &&
      grepl("Away", first_text, ignore.case = TRUE)
    ) {
      cells <- xml2::xml_find_all(first_row, "./th|./td")
      if (length(cells) >= 21L) {
        result_table <- tbl
        break
      }
    }
  }

  if (is.null(result_table)) {
    stop(
      "Could not find the Premier League Home/Away results matrix in the cached Wikipedia page."
    )
  }

  header_cells <- xml2::xml_find_all(
    xml2::xml_find_first(result_table, ".//tr[1]"),
    "./th|./td"
  )

  away_names <- vapply(
    header_cells[-1],
    cell_team_name,
    character(1)
  )

  rows <- list()
  k <- 1L

  score_pat <- "^\\s*([0-9]+)\\s*[\\-\u2013\u2014]\\s*([0-9]+)\\s*$"

  trs <- xml2::xml_find_all(result_table, ".//tr[position()>1]")

  for (tr in trs) {
    cells <- xml2::xml_find_all(tr, "./th|./td")
    if (length(cells) < 2L) next

    home_name <- cell_team_name(cells[[1]])
    if (!nzchar(home_name)) next

    n_away <- min(length(away_names), length(cells) - 1L)

    for (j in seq_len(n_away)) {
      score_text <- clean_text(xml2::xml_text(cells[[j + 1L]]))

      if (!grepl(score_pat, score_text, perl = TRUE)) next

      hg <- as.integer(sub(score_pat, "\\1", score_text, perl = TRUE))
      ag <- as.integer(sub(score_pat, "\\2", score_text, perl = TRUE))

      rows[[k]] <- data.frame(
        HomeWiki = home_name,
        AwayWiki = away_names[j],
        HomeKey = club_key(home_name),
        AwayKey = club_key(away_names[j]),
        Result = result_code(hg, ag),
        Score = sprintf("%d-%d", hg, ag),
        stringsAsFactors = FALSE
      )

      k <- k + 1L
    }
  }

  if (length(rows) == 0L) {
    stop("Premier League results matrix was found, but no completed scores were parsed.")
  }

  out <- do.call(rbind, rows)

  pair_key <- paste(out$HomeKey, out$AwayKey, sep = "||")

  if (anyDuplicated(pair_key)) {
    dup <- out[
      duplicated(pair_key) | duplicated(pair_key, fromLast = TRUE),
      ,
      drop = FALSE
    ]
    cat("\nDuplicate Wikipedia home/away pairs:\n")
    print(dup, row.names = FALSE)
    stop("Wikipedia Premier League matrix produced duplicate home/away pairs.")
  }

  out
}

cat("Mode: Premier League Wikipedia merge only\n")
cat("Season:", season_folder, "\n")
cat("Wikipedia cache:", wiki_file, "\n")
cat("Combined CSV:", combined_file, "\n\n")

wiki <- parse_wikipedia_results_matrix(wiki_file)

cat("Wikipedia completed matches parsed:", nrow(wiki), "\n")

all_df <- read.csv(
  combined_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c()
)

required_cols <- c(
  "Season", "Country", "Competition", "CompetitionType", "Tier", "League",
  "Date", "Home", "Away", "Result", "Score", "Source"
)

missing_cols <- setdiff(required_cols, names(all_df))
if (length(missing_cols) > 0L) {
  stop(
    "Combined CSV is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

pl_idx <- which(
  all_df$Season == season_label &
    all_df$Country == "England" &
    all_df$Competition == "premier_league"
)

if (length(pl_idx) == 0L) {
  stop("No ", season_label, " Premier League rows exist in the combined CSV.")
}

pl <- all_df[pl_idx, , drop = FALSE]
pl_home_key <- club_key(pl$Home)
pl_away_key <- club_key(pl$Away)
pl_pair_key <- paste(pl_home_key, pl_away_key, sep = "||")

if (anyDuplicated(pl_pair_key)) {
  dup <- pl[
    duplicated(pl_pair_key) | duplicated(pl_pair_key, fromLast = TRUE),
    ,
    drop = FALSE
  ]
  cat("\nDuplicate current Premier League pairs in combined CSV:\n")
  print(dup[, c("Date", "Home", "Away", "Result", "Score"), drop = FALSE])
  stop("Current combined CSV has duplicate Premier League home/away pairs.")
}

wiki_pair_key <- paste(wiki$HomeKey, wiki$AwayKey, sep = "||")
pos <- match(wiki_pair_key, pl_pair_key)

if (any(is.na(pos))) {
  unmatched <- wiki[is.na(pos), , drop = FALSE]

  cat("\nWikipedia completed results not present in combined CSV:\n")
  print(
    unmatched[
      ,
      c("HomeWiki", "AwayWiki", "HomeKey", "AwayKey", "Score"),
      drop = FALSE
    ],
    row.names = FALSE
  )

  stop(
    "Some Wikipedia Premier League results cannot be matched to the current combined CSV. ",
    "Refusing to alter canonical Elo input."
  )
}

target_idx <- pl_idx[pos]

existing_result <- trimws(as.character(all_df$Result[target_idx]))
existing_score <- trimws(as.character(all_df$Score[target_idx]))
existing_result[is.na(existing_result)] <- ""
existing_score[is.na(existing_score)] <- ""

conflict <- (
  (existing_result != "" & existing_result != wiki$Result) |
  (existing_score != "" & existing_score != wiki$Score)
)

if (any(conflict)) {
  conflict_rows <- data.frame(
    Date = all_df$Date[target_idx[conflict]],
    Home = all_df$Home[target_idx[conflict]],
    Away = all_df$Away[target_idx[conflict]],
    ExistingResult = existing_result[conflict],
    ExistingScore = existing_score[conflict],
    WikipediaResult = wiki$Result[conflict],
    WikipediaScore = wiki$Score[conflict],
    stringsAsFactors = FALSE
  )

  cat("\nConflicting Premier League results:\n")
  print(conflict_rows, row.names = FALSE)

  stop(
    "OpenFootball/combined data conflicts with Wikipedia. ",
    "Refusing to alter canonical Elo input."
  )
}

newly_completed <- sum(existing_result == "" & existing_score == "")
already_completed <- nrow(wiki) - newly_completed

# Keep the existing fixture date from OpenFootball/combined CSV.
# Wikipedia supplies the score/result only.
all_df$Result[target_idx] <- wiki$Result
all_df$Score[target_idx] <- wiki$Score
all_df$Source[target_idx] <- ifelse(
  existing_result != "" | existing_score != "",
  "openfootball+wikipedia",
  "wikipedia"
)

pl_after <- all_df[
  all_df$Season == season_label &
    all_df$Country == "England" &
    all_df$Competition == "premier_league",
  ,
  drop = FALSE
]

completed_after <- sum(
  trimws(as.character(pl_after$Result)) != "" &
    trimws(as.character(pl_after$Score)) != ""
)

# Atomic write: write temporary file, then replace the combined CSV.
tmp_file <- paste0(combined_file, ".tmp")

write.csv(
  all_df,
  tmp_file,
  row.names = FALSE,
  na = ""
)

if (!file.exists(tmp_file) || file.info(tmp_file)$size == 0) {
  stop("Temporary combined CSV was not written correctly.")
}

if (!file.copy(tmp_file, combined_file, overwrite = TRUE)) {
  stop("Could not replace combined CSV after successful QA.")
}

file.remove(tmp_file)

cat(
  "\nPremier League Wikipedia merge QA:\n",
  "  Existing current-season PL rows: ", length(pl_idx), "\n",
  "  Wikipedia completed matches: ", nrow(wiki), "\n",
  "  Matched completed matches: ", nrow(wiki), "\n",
  "  Newly completed from Wikipedia: ", newly_completed, "\n",
  "  Already completed and verified: ", already_completed, "\n",
  "  Completed PL rows after merge: ", completed_after, "\n",
  "  Unmatched Wikipedia results: 0\n",
  "  Conflicting scores: 0\n",
  sep = ""
)

cat("\nUpdated:", combined_file, "\n")

cat(
  "\n01 PL Wikipedia merge elapsed time: ",
  round(as.numeric(difftime(Sys.time(), script_start_time, units = "secs")), 1),
  " seconds\n",
  sep = ""
)
