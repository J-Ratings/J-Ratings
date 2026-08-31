library(readr)
library(dplyr)
library(lubridate)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
repo_dir <- normalizePath(
  Sys.getenv(
    "J_RATINGS_REPO",
    "C:/Users/stjuk/Documents/GitHub/J-Ratings"
  ),
  winslash = "/",
  mustWork = TRUE
)

source_dir <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "source"
)

processed_dir <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "processed"
)

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

all_matches_file <- file.path(source_dir, "all_matches.csv")
output_file <- file.path(processed_dir, "results_with_winner.csv")

tournament_registry_file <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "reference",
  "tournament_registry.csv"
)

tournament_override_file <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "reference",
  "tournament_edition_overrides.csv"
)

tournament_date_range_file <- file.path(
  repo_dir,
  "InternationalFootball",
  "pipeline_data",
  "reference",
  "tournament_edition_date_ranges.csv"
)

tournament_summary_file <- file.path(
  processed_dir,
  "tournament_name_summary.csv"
)

if (!file.exists(all_matches_file)) {
  stop("Missing all_matches file: ", all_matches_file)
}

# -----------------------------
# Helpers
# -----------------------------
parse_mixed_date <- function(x) {
  x_chr <- trimws(as.character(x))
  
  parsed <- case_when(
    is.na(x_chr) | x_chr == "" ~ as.Date(NA),
    grepl("^[0-9]+$", x_chr) ~ as.Date(as.numeric(x_chr), origin = "1899-12-30"),
    grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x_chr) ~ dmy(x_chr),
    TRUE ~ ymd(x_chr)
  )
  
  as.Date(parsed)
}

clean_score_num <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null", "na")] <- NA_character_
  suppressWarnings(as.integer(x_chr))
}

normalise_team_name <- function(x) {
  x0 <- trimws(as.character(x))
  
  team_map <- c(
    "Åland" = "Åland Islands",
    
    # Match new data-source names to existing site asset names
    "Czechia" = "Czech Republic",
    "Ireland" = "Republic of Ireland",
    "China" = "China PR",
    
    # Spelling / diacritics
    "Curacao" = "Curaçao",
    "Reunion" = "Réunion",
    "Sao Tome and Principe" = "São Tomé and Príncipe",
    "São Tome and Principe" = "São Tomé and Príncipe",
    "Saint Barthelemy" = "Saint Barthélemy",
    
    # Common abbreviations
    "St Vincent & Grenadines" = "Saint Vincent and the Grenadines",
    "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines",
    "Saint Vincent & Grenadines" = "Saint Vincent and the Grenadines",
    
    # Territory / country naming
    "US Virgin Islands" = "United States Virgin Islands",
    "Macao" = "Macau",
    "Eastern Samoa" = "American Samoa",
    "East Timor" = "Timor-Leste"
  )
  
  ifelse(x0 %in% names(team_map), unname(team_map[x0]), x0)
}


# -----------------------------
# Automatic source duplicate resolution
# -----------------------------
#
# Some source errors duplicate the same match on the wrong date. We resolve
# these automatically rather than maintaining one-off exclusions.
#
# Candidate duplicates must match on teams, score and tournament (plus country
# and neutral when available). Each candidate date is then scored by how many
# matches from the same tournament occur nearby. An isolated copy loses to a
# copy embedded in the real tournament cluster.
#
# If two copies are both independently well-supported (for example a genuine
# repeat fixture in different editions), both are retained automatically.

resolve_source_duplicates <- function(df, window_days = 45L, min_support = 8L, support_margin = 5L) {
  if (nrow(df) == 0L) return(df)
  
  signature_cols <- c(
    "home_team", "away_team", "home_score", "away_score", "tournament"
  )
  
  optional_cols <- intersect(c("country", "neutral"), names(df))
  signature_cols <- c(signature_cols, optional_cols)
  
  sig <- do.call(
    paste,
    c(
      lapply(df[signature_cols], function(x) ifelse(is.na(x), "<NA>", as.character(x))),
      sep = "||"
    )
  )
  
  duplicate_sigs <- unique(sig[duplicated(sig) | duplicated(sig, fromLast = TRUE)])
  
  if (length(duplicate_sigs) == 0L) {
    return(df)
  }
  
  keep <- rep(TRUE, nrow(df))
  auto_removed <- list()
  removed_n <- 0L
  
  for (s in duplicate_sigs) {
    idx <- which(sig == s)
    
    # Same-date exact duplicates: keep the first deterministically.
    if (length(unique(df$date[idx])) == 1L) {
      if (length(idx) > 1L) {
        keep[idx[-1L]] <- FALSE
        removed_n <- removed_n + length(idx) - 1L
      }
      next
    }
    
    support <- vapply(
      idx,
      function(i) {
        sum(
          df$tournament == df$tournament[i] &
            !is.na(df$date) &
            abs(as.integer(df$date - df$date[i])) <= window_days,
          na.rm = TRUE
        )
      },
      integer(1)
    )
    
    ord <- order(support, df$date[idx], decreasing = TRUE)
    best_pos <- ord[1L]
    best_support <- support[best_pos]
    second_support <- if (length(ord) >= 2L) support[ord[2L]] else -Inf
    
    # Remove the weaker copies only when one date is clearly embedded in a
    # tournament cluster and the alternatives are materially less supported.
    if (
      best_support >= min_support &&
      best_support >= second_support + support_margin
    ) {
      winner_idx <- idx[best_pos]
      loser_idx <- setdiff(idx, winner_idx)
      
      keep[loser_idx] <- FALSE
      
      removed_n <- removed_n + length(loser_idx)
      auto_removed[[length(auto_removed) + 1L]] <- tibble(
        kept_date = df$date[winner_idx],
        removed_date = df$date[loser_idx],
        home_team = df$home_team[winner_idx],
        away_team = df$away_team[winner_idx],
        score = paste0(df$home_score[winner_idx], "-", df$away_score[winner_idx]),
        tournament = df$tournament[winner_idx],
        kept_support = best_support,
        removed_support = support[match(loser_idx, idx)]
      )
    }
  }
  
  if (removed_n > 0L) {
    cat("Automatically removed source duplicate rows:", removed_n, "\n")
    
    removed_tbl <- bind_rows(auto_removed)
    if (nrow(removed_tbl) > 0L) {
      print(removed_tbl, n = Inf)
    }
  } else {
    cat("Automatically removed source duplicate rows: 0\n")
  }
  
  df[keep, , drop = FALSE]
}

# -----------------------------
# Tournament recognition
# -----------------------------
#
# The source already supplies a tournament name. This layer does NOT try to
# infer tournament format. It only maps selected source tournament names to a
# stable tournament family and separates repeated editions by time gaps.
#
# Unrecognised source names are deliberately left blank rather than guessed.

load_tournament_registry <- function() {
  if (!file.exists(tournament_registry_file)) {
    stop(
      "Missing tournament registry: ", tournament_registry_file,
      "\nCreate it before running 01_combine_results.R."
    )
  }
  
  registry <- read_csv(
    tournament_registry_file,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    transmute(
      source_tournament = trimws(as.character(source_tournament)),
      tournament_id = trimws(as.character(tournament_id)),
      display_name = trimws(as.character(display_name)),
      edition_gap_days = suppressWarnings(as.integer(edition_gap_days)),
      enabled = case_when(
        is.logical(enabled) ~ enabled,
        tolower(trimws(as.character(enabled))) %in% c("true", "1", "yes", "y") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(
      enabled,
      source_tournament != "",
      tournament_id != "",
      display_name != ""
    ) %>%
    mutate(
      edition_gap_days = if_else(
        is.na(edition_gap_days) | edition_gap_days < 1L,
        180L,
        edition_gap_days
      )
    )
  
  if (anyDuplicated(registry$source_tournament)) {
    stop("tournament_registry.csv has duplicate source_tournament values.")
  }
  
  registry
}

load_tournament_overrides <- function() {
  if (!file.exists(tournament_override_file)) {
    return(tibble(
      tournament_id = character(),
      detected_edition_id = character(),
      edition_id = character(),
      event_display = character()
    ))
  }
  
  overrides <- read_csv(
    tournament_override_file,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) %>%
    transmute(
      tournament_id = trimws(as.character(tournament_id)),
      detected_edition_id = trimws(as.character(detected_edition_id)),
      edition_id = trimws(as.character(edition_id)),
      event_display = trimws(as.character(event_display))
    ) %>%
    filter(
      tournament_id != "",
      detected_edition_id != "",
      edition_id != ""
    )
  
  if (anyDuplicated(paste(overrides$tournament_id, overrides$detected_edition_id, sep = "||"))) {
    stop("tournament_edition_overrides.csv has duplicate tournament/edition override keys.")
  }
  
  overrides
}


load_tournament_date_ranges <- function() {
  if (!file.exists(tournament_date_range_file)) {
    return(tibble(
      tournament_id = character(),
      start_date = as.Date(character()),
      end_date = as.Date(character()),
      edition_id = character(),
      event_display = character()
    ))
  }
  
  ranges <- read_csv(
    tournament_date_range_file,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8"),
    col_types = cols(.default = col_character())
  ) %>%
    transmute(
      tournament_id = trimws(as.character(tournament_id)),
      start_date = parse_mixed_date(start_date),
      end_date = parse_mixed_date(end_date),
      edition_id = trimws(as.character(edition_id)),
      event_display = trimws(as.character(event_display))
    ) %>%
    filter(
      tournament_id != "",
      !is.na(start_date),
      !is.na(end_date),
      edition_id != ""
    )
  
  if (any(ranges$end_date < ranges$start_date)) {
    stop("tournament_edition_date_ranges.csv contains an end_date before start_date.")
  }
  
  if (nrow(ranges) > 1L) {
    overlap_check <- ranges %>%
      arrange(tournament_id, start_date, end_date) %>%
      group_by(tournament_id) %>%
      mutate(previous_end = lag(end_date)) %>%
      ungroup() %>%
      filter(!is.na(previous_end), start_date <= previous_end)
    
    if (nrow(overlap_check) > 0L) {
      print(overlap_check)
      stop("tournament_edition_date_ranges.csv contains overlapping ranges for a tournament.")
    }
  }
  
  ranges
}

recognise_tournament_editions <- function(df, registry, overrides, date_ranges) {
  recognised <- df %>%
    left_join(registry, by = c("tournament" = "source_tournament")) %>%
    arrange(tournament_id, date, home_team, away_team) %>%
    group_by(tournament_id) %>%
    mutate(
      previous_date = lag(date),
      new_edition = case_when(
        is.na(tournament_id) ~ FALSE,
        row_number() == 1L ~ TRUE,
        is.na(previous_date) ~ TRUE,
        as.integer(date - previous_date) > edition_gap_days ~ TRUE,
        TRUE ~ FALSE
      ),
      edition_number = if_else(
        is.na(tournament_id),
        NA_integer_,
        cumsum(new_edition)
      )
    ) %>%
    ungroup()
  
  edition_lookup <- recognised %>%
    filter(!is.na(tournament_id), !is.na(edition_number)) %>%
    group_by(tournament_id, display_name, edition_number) %>%
    summarise(
      edition_start = min(date),
      edition_end = max(date),
      .groups = "drop"
    ) %>%
    mutate(
      detected_edition_id = format(edition_start, "%Y")
    ) %>%
    left_join(
      overrides,
      by = c("tournament_id", "detected_edition_id")
    ) %>%
    mutate(
      edition_id = if_else(
        is.na(edition_id) | edition_id == "",
        detected_edition_id,
        edition_id
      ),
      event = if_else(
        !is.na(event_display) & event_display != "",
        event_display,
        paste(display_name, edition_id)
      )
    ) %>%
    select(
      tournament_id,
      edition_number,
      edition_id,
      event,
      edition_start,
      edition_end
    )
  
  recognised_out <- recognised %>%
    left_join(
      edition_lookup,
      by = c("tournament_id", "edition_number")
    ) %>%
    mutate(
      tournament_id = if_else(is.na(tournament_id), NA_character_, tournament_id),
      edition_id = if_else(is.na(edition_id), NA_character_, edition_id),
      event = if_else(is.na(event), NA_character_, event)
    )
  
  # Explicit date ranges override automatic gap-based edition labels for
  # competitions that span multiple calendar years, such as Nations Leagues.
  if (nrow(date_ranges) > 0L) {
    for (i in seq_len(nrow(date_ranges))) {
      rule <- date_ranges[i, ]
      
      idx <- !is.na(recognised_out$tournament_id) &
        recognised_out$tournament_id == rule$tournament_id &
        recognised_out$date >= rule$start_date &
        recognised_out$date <= rule$end_date
      
      if (any(idx)) {
        recognised_out$edition_id[idx] <- rule$edition_id
        
        if (!is.na(rule$event_display) && rule$event_display != "") {
          recognised_out$event[idx] <- rule$event_display
        } else {
          recognised_out$event[idx] <- paste(recognised_out$display_name[idx], rule$edition_id)
        }
      }
    }
  }
  
  recognised_out %>%
    select(-display_name, -edition_gap_days, -enabled, -previous_date, -new_edition, -edition_number,
           -edition_start, -edition_end)
}

# -----------------------------
# Load source data
# -----------------------------
all_matches <- read_csv(
  all_matches_file,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

required_cols <- c(
  "date",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "tournament"
)

missing_cols <- setdiff(required_cols, names(all_matches))

if (length(missing_cols) > 0) {
  stop(
    "all_matches.csv is missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

# -----------------------------
# Clean results
# -----------------------------
today <- Sys.Date()

results <- all_matches %>%
  mutate(
    date = parse_mixed_date(date),
    home_team = normalise_team_name(home_team),
    away_team = normalise_team_name(away_team),
    home_score = clean_score_num(home_score),
    away_score = clean_score_num(away_score),
    tournament = trimws(as.character(tournament))
  ) %>%
  filter(
    !is.na(date),
    date <= today,
    !is.na(home_score),
    !is.na(away_score),
    home_team != "",
    away_team != "",
    tournament != ""
  )

results <- resolve_source_duplicates(results)

tournament_registry <- load_tournament_registry()
tournament_overrides <- load_tournament_overrides()
tournament_date_ranges <- load_tournament_date_ranges()

results <- recognise_tournament_editions(
  results,
  tournament_registry,
  tournament_overrides,
  tournament_date_ranges
)

# Diagnostic inventory of every source tournament name. This is deliberately
# written even for unrecognised names so the registry can be expanded safely.
tournament_name_summary <- results %>%
  group_by(tournament) %>%
  summarise(
    matches = n(),
    first_date = min(date),
    last_date = max(date),
    tournament_id = first(tournament_id),
    .groups = "drop"
  ) %>%
  mutate(
    recognised = !is.na(tournament_id) & tournament_id != ""
  ) %>%
  arrange(desc(matches), tournament)

write_csv(tournament_name_summary, tournament_summary_file)

# -----------------------------
# Build downstream results file
# -----------------------------
out <- results %>%
  mutate(
    result = case_when(
      home_score > away_score ~ home_team,
      away_score > home_score ~ away_team,
      TRUE ~ "Draw"
    ),
    score = paste0(home_score, "-", away_score),
    date = format(date, "%Y-%m-%d")
  ) %>%
  select(
    date,
    home_team,
    away_team,
    result,
    score,
    tournament,
    tournament_id,
    edition_id,
    event
  ) %>%
  arrange(date, tournament, home_team, away_team)

# -----------------------------
# Sanity checks
# -----------------------------
if (nrow(out) == 0) {
  stop("No usable rows were produced from all_matches.csv.")
}

bad_results <- out %>%
  filter(
    result != "Draw",
    result != home_team,
    result != away_team
  )

if (nrow(bad_results) > 0) {
  print(bad_results)
  stop("Some result labels do not match home_team, away_team, or Draw.")
}

duplicate_rows <- out %>%
  count(date, home_team, away_team, score, tournament, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_rows) > 0) {
  cat(
    "Automatically collapsing exact duplicate output match rows:",
    nrow(duplicate_rows),
    "\n"
  )
  
  print(duplicate_rows, n = min(20, nrow(duplicate_rows)))
  
  out <- out %>%
    distinct(
      date,
      home_team,
      away_team,
      score,
      tournament,
      .keep_all = TRUE
    )
}

# -----------------------------
# Write output
# -----------------------------
write_csv(out, output_file)

cat("Wrote:", output_file, "\n")
cat("Rows:", nrow(out), "\n")
cat("Latest date:", max(out$date, na.rm = TRUE), "\n")
cat("Recognised tournament matches:", sum(!is.na(out$tournament_id)), "\n")
cat("Tournament-name inventory:", tournament_summary_file, "\n")

# Basic penalty sanity check: 2022 World Cup final should remain a draw
check_row <- out %>%
  filter(
    home_team == "Argentina",
    away_team == "France",
    date == "2022-12-18"
  )

if (nrow(check_row) > 0) {
  print(check_row)
}