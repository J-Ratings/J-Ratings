# ============================================================
# Write Rugby Union tournament JSON for the website
#
# Input:
#   RugbyUnion/pipeline_data/Matches/rugby_union_results_master.csv
#
# Outputs:
#   RugbyUnion/data/tournaments/index.json
#   RugbyUnion/data/tournaments/<tournament>/editions.json
#   RugbyUnion/data/tournaments/<tournament>/<edition>.json
#
# This script does NOT calculate or alter Elo ratings.
# It only organises canonical Rugby Union match data into
# tournament-edition JSON for the website.
# ============================================================

library(readr)
library(dplyr)
library(jsonlite)
library(stringi)

options(stringsAsFactors = FALSE)

get_repo_dir <- function() {
  github_workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")

  if (nzchar(github_workspace) && dir.exists(github_workspace)) {
    return(normalizePath(github_workspace, winslash = "/", mustWork = TRUE))
  }

  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  if (
    dir.exists(file.path(wd, "RugbyUnion")) &&
    dir.exists(file.path(wd, "scripts"))
  ) {
    return(wd)
  }

  stop(
    "Could not find repo root. Run this script from the J-Ratings repo root, ",
    "or set GITHUB_WORKSPACE."
  )
}

repo_dir <- get_repo_dir()

input_csv <- file.path(
  repo_dir,
  "RugbyUnion", "pipeline_data", "Matches",
  "rugby_union_results_master.csv"
)

base_out <- file.path(
  repo_dir,
  "RugbyUnion", "data", "tournaments"
)

dir.create(base_out, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_csv)) {
  stop("Missing Rugby Union master CSV: ", input_csv)
}

normalise_team_name <- function(x) {
  x0 <- trimws(as.character(x))
  x0 <- sub(" Rugby$", "", x0)

  team_map <- c(
    "All Blacks" = "New Zealand",
    "Western Samoa" = "Samoa",
    "West Germany" = "Germany",
    "USA" = "United States",
    "UAE" = "United Arab Emirates",
    "Korea" = "South Korea"
  )

  ifelse(
    x0 %in% names(team_map),
    unname(team_map[x0]),
    x0
  )
}

slug <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

write_json_compact <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  write_json(
    x,
    path,
    auto_unbox = TRUE,
    pretty = FALSE,
    na = "null"
  )
}

results <- read_csv(
  input_csv,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

required_cols <- c(
  "date", "home_team", "away_team",
  "home_score", "away_score",
  "result", "competition"
)

missing_cols <- setdiff(required_cols, names(results))

if (length(missing_cols) > 0) {
  stop(
    "Master CSV is missing columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

results <- results %>%
  transmute(
    date = as.Date(trimws(date)),
    home_team = normalise_team_name(home_team),
    away_team = normalise_team_name(away_team),
    home_score = suppressWarnings(as.integer(home_score)),
    away_score = suppressWarnings(as.integer(away_score)),
    result = trimws(as.character(result)),
    competition = trimws(as.character(competition))
  ) %>%
  filter(
    !is.na(date),
    home_team != "",
    away_team != "",
    competition != ""
  ) %>%
  mutate(
    result = if_else(
      tolower(result) == "draw",
      "Draw",
      normalise_team_name(result)
    )
  )

make_teams <- function(games) {
  team_names <- sort(unique(c(games$home_team, games$away_team)))

  lapply(team_names, function(tm) {
    list(
      id = slug(tm),
      name = tm,
      flag = ""
    )
  })
}

make_matches <- function(games) {
  lapply(seq_len(nrow(games)), function(j) {
    row <- games[j, ]

    played <- !is.na(row$home_score) && !is.na(row$away_score)

    list(
      date = format(row$date, "%Y-%m-%d"),
      home = slug(row$home_team),
      away = slug(row$away_team),
      homeScore = if (played) as.integer(row$home_score) else NA_integer_,
      awayScore = if (played) as.integer(row$away_score) else NA_integer_,
      status = if (played) "completed" else "scheduled"
    )
  })
}

write_family <- function(
  tournament_id,
  family_name,
  competition_names,
  edition_mode = c("calendar", "december-next-year"),
  preferred_competition = NULL
) {
  edition_mode <- match.arg(edition_mode)

  x <- results %>%
    filter(competition %in% competition_names) %>%
    mutate(
      edition = as.integer(format(date, "%Y"))
    )

  if (edition_mode == "december-next-year") {
    x <- x %>%
      mutate(
        edition = edition + if_else(
          as.integer(format(date, "%m")) == 12L,
          1L,
          0L
        )
      )
  }

  if (!is.null(preferred_competition)) {
    for (edition_text in names(preferred_competition)) {
      edition_num <- as.integer(edition_text)
      preferred <- preferred_competition[[edition_text]]

      x <- x %>%
        filter(
          edition != edition_num |
            competition == preferred
        )
    }
  }

  # Protect against duplicate rows where feeds use different labels
  # but resolve to the same canonical date/home/away fixture.
  x <- x %>%
    arrange(date, home_team, away_team, competition) %>%
    distinct(date, home_team, away_team, .keep_all = TRUE)

  if (nrow(x) == 0) {
    warning("No rows found for tournament family: ", tournament_id)
    return(invisible(NULL))
  }

  out_dir <- file.path(base_out, tournament_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  editions <- sort(unique(x$edition))
  edition_rows <- vector("list", length(editions))

  for (i in seq_along(editions)) {
    edition_year <- editions[[i]]

    games <- x %>%
      filter(edition == edition_year) %>%
      arrange(date, home_team, away_team)

    display_name <- names(sort(table(games$competition), decreasing = TRUE))[[1]]

    teams <- make_teams(games)
    matches <- make_matches(games)

    all_completed <- all(vapply(
      matches,
      function(m) identical(m$status, "completed"),
      logical(1)
    ))

    edition_json <- list(
      tournamentId = tournament_id,
      name = family_name,
      editionId = as.character(edition_year),
      event = paste(display_name, edition_year),
      competitionName = display_name,
      status = if (all_completed) "complete" else "incomplete",
      teams = teams,
      matches = matches,
      summary = list(
        matches = nrow(games),
        teams = length(teams)
      )
    )

    write_json_compact(
      edition_json,
      file.path(out_dir, paste0(edition_year, ".json"))
    )

    edition_rows[[i]] <- list(
      id = as.character(edition_year),
      label = as.character(edition_year),
      event = paste(display_name, edition_year),
      status = edition_json$status
    )

    cat(
      "Wrote ", family_name, " ", edition_year,
      " (", nrow(games), " matches)\n",
      sep = ""
    )
  }

  write_json_compact(
    rev(edition_rows),
    file.path(out_dir, "editions.json")
  )

  invisible(NULL)
}

# ------------------------------------------------------------
# Rugby World Cup
# ------------------------------------------------------------

RWC_FINAL_YEARS <- c(
  1987L, 1991L, 1995L, 1999L, 2003L,
  2007L, 2011L, 2015L, 2019L, 2023L
)

rwc <- results %>%
  filter(
    competition == "Rugby World Cup",
    as.integer(format(date, "%Y")) %in% RWC_FINAL_YEARS
  ) %>%
  mutate(edition = as.integer(format(date, "%Y"))) %>%
  arrange(date, home_team, away_team)

if (nrow(rwc) == 0) {
  stop("No completed Rugby World Cup final-tournament matches found.")
}

rwc_out <- file.path(base_out, "rugby-world-cup")
dir.create(rwc_out, recursive = TRUE, showWarnings = FALSE)

rwc_edition_rows <- vector("list", length(RWC_FINAL_YEARS))

for (i in seq_along(RWC_FINAL_YEARS)) {
  edition_year <- RWC_FINAL_YEARS[[i]]

  games <- rwc %>%
    filter(edition == edition_year) %>%
    arrange(date, home_team, away_team)

  if (nrow(games) == 0) {
    stop("No Rugby World Cup rows found for edition ", edition_year)
  }

  teams <- make_teams(games)
  matches <- make_matches(games)

  final_row <- games[nrow(games), ]

  champion_name <- if (final_row$home_score > final_row$away_score) {
    final_row$home_team
  } else {
    final_row$away_team
  }

  runner_up_name <- if (champion_name == final_row$home_team) {
    final_row$away_team
  } else {
    final_row$home_team
  }

  display_fallback_matches <- list()

  # The master CSV is missing Argentina 63-3 Namibia from the 2007
  # final tournament. Keep canonical matches untouched and expose this
  # separately as a display-only historical fallback.
  if (edition_year == 2007L) {
    display_fallback_matches <- list(
      list(
        date = "2007-09-22",
        home = "argentina",
        away = "namibia",
        homeScore = 63L,
        awayScore = 3L,
        status = "completed",
        sourceDataMissing = TRUE
      )
    )
  }

  edition_json <- list(
    tournamentId = "rugby-world-cup",
    name = "Rugby World Cup",
    editionId = as.character(edition_year),
    event = paste("Rugby World Cup", edition_year),
    status = "complete",
    teams = teams,
    matches = matches,
    displayFallbackMatches = display_fallback_matches,
    summary = list(
      championId = slug(champion_name),
      runnerUpId = slug(runner_up_name),
      matches = nrow(games)
    )
  )

  write_json_compact(
    edition_json,
    file.path(rwc_out, paste0(edition_year, ".json"))
  )

  rwc_edition_rows[[i]] <- list(
    id = as.character(edition_year),
    label = as.character(edition_year),
    event = paste("Rugby World Cup", edition_year),
    status = "complete"
  )

  cat(
    "Wrote Rugby World Cup ", edition_year,
    " (", nrow(games), " matches)\n",
    sep = ""
  )
}

write_json_compact(
  rev(rwc_edition_rows),
  file.path(rwc_out, "editions.json")
)

# ------------------------------------------------------------
# Main recurring Rugby Union championships
# ------------------------------------------------------------

write_family(
  tournament_id = "six-nations",
  family_name = "Six Nations Championship",
  competition_names = c(
    "Home Nations Championship",
    "Five Nations Championship",
    "Six Nations Championship"
  ),
  edition_mode = "december-next-year"
)

write_family(
  tournament_id = "rugby-championship",
  family_name = "The Rugby Championship",
  competition_names = c(
    "Tri Nations",
    "The Rugby Championship",
    "Rugby Championship"
  ),
  preferred_competition = list(
    "2025" = "Rugby Championship"
  )
)

write_family(
  tournament_id = "pacific-nations-cup",
  family_name = "Pacific Nations Cup",
  competition_names = c(
    "IRB Pacific 5 Nations",
    "IRB Pacific Nations Cup",
    "World Rugby Pacific Nations Cup",
    "Pacific Nations Cup"
  ),
  preferred_competition = list(
    "2025" = "Pacific Nations Cup"
  )
)

write_family(
  tournament_id = "rugby-europe-championship",
  family_name = "Rugby Europe Championship",
  competition_names = c(
    "Rugby Europe Championship"
  )
)

tournament_index <- list(
  list(
    id = "rugby-world-cup",
    name = "Rugby World Cup",
    renderer = "group_knockout"
  ),
  list(
    id = "six-nations",
    name = "Six Nations Championship",
    renderer = "league"
  ),
  list(
    id = "rugby-championship",
    name = "The Rugby Championship",
    renderer = "league"
  ),
  list(
    id = "pacific-nations-cup",
    name = "Pacific Nations Cup",
    renderer = "league"
  ),
  list(
    id = "rugby-europe-championship",
    name = "Rugby Europe Championship",
    renderer = "league"
  )
)

write_json_compact(
  tournament_index,
  file.path(base_out, "index.json")
)

cat("\nDone.\n")
cat("Tournament JSON directory: ", base_out, "\n", sep = "")
