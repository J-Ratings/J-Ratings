# ============================================================
# Athletics 100m Elo ratings - two-pass retro-start test script
#
# Reads:
#   world-athletics_all-time-top-lists.csv
#
# Filters:
#   - Men's 100m
#   - Outdoor Senior - Men
#   - date >= 2000-01-01
#
# Race grouping:
#   - date + venue + event + category + race_section
#   - race_section is extracted from pos_raw:
#       1qf4 -> qf4
#       2sf1 -> sf1
#       1f2  -> f2
#       1    -> main
#
# Outputs:
#   - athletics_100m_game_history.csv
#   - athletics_100m_final_ratings.csv
#   - athletics_100m_peak_ratings.csv
#
# Notes:
#   - This dataset is a toplist dataset, not a full race-result feed.
#   - Race fields may still be incomplete if slower athletes are missing.
# ============================================================

library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tibble)
library(ggplot2)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------
input_file <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/archive (4)/world-athletics_all-time-top-lists.csv"

out_dir <- "C:/Users/stjuk/OneDrive/Desktop/Baduk/Go-Go-Ratings/archive (4)/athletics_elo_outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

history_file <- file.path(out_dir, "athletics_100m_game_history.csv")
final_file   <- file.path(out_dir, "athletics_100m_final_ratings.csv")
peak_file    <- file.path(out_dir, "athletics_100m_peak_ratings.csv")

# -----------------------------
# Filters
# -----------------------------
EVENT_FILTER <- "Men's 200m"
CATEGORY_FILTER <- "Outdoor Senior - Men"

RUN_START_DATE <- as.Date("1990-01-01")
RUN_END_DATE   <- as.Date("2026-12-31")

# -----------------------------
# Elo settings
# -----------------------------
START_R <- 1500
K_NORMAL <- 10

RETRO_RACES_N <- 40L
MIN_VALID_RETRO_COMPARISONS <- 20L
MIN_VALID_RETRO_WEIGHT <- 10

MIN_RACE_SIZE <- 3L
MIN_RACES_FOR_TABLE <- 5L

# -----------------------------
# Helpers
# -----------------------------
expected_score <- function(Ra, Rb) {
  1 / (1 + 10 ^ ((Rb - Ra) / 400))
}

opponent_weight <- function(n, cap = 10L, floor = 0.10) {
  n <- pmin(as.numeric(n), cap)
  floor + (1 - floor) * (n / cap)
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  x[x == ""] <- NA_character_
  x
}

parse_wa_date <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  
  d <- suppressWarnings(dmy(x))
  
  bad <- is.na(d)
  if (any(bad)) {
    d[bad] <- suppressWarnings(ymd(x[bad]))
  }
  
  as.Date(d)
}

parse_position <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  suppressWarnings(as.integer(str_extract(x, "^\\d+")))
}

parse_race_section <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  
  section <- str_replace(x, "^\\d+", "")
  section[is.na(section) | section == ""] <- "main"
  
  section
}

normalise_mark <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

make_athlete_id <- function(name, nat, dob) {
  paste(
    clean_text(name),
    str_trim(as.character(nat)),
    str_trim(as.character(dob)),
    sep = "|"
  )
}

safe_get_col <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) {
    df[[col]]
  } else {
    rep(default, nrow(df))
  }
}

# -----------------------------
# Load data
# -----------------------------
raw <- read_csv(
  input_file,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

required_cols <- c(
  "event", "category", "mark", "competitor", "nat",
  "date_of_birth", "pos", "date", "venue"
)

missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

df <- raw %>%
  transmute(
    all_time_rank = safe_get_col(raw, "all_time_rank"),
    results_score = suppressWarnings(as.numeric(safe_get_col(raw, "results_score"))),
    event = clean_text(event),
    category = clean_text(category),
    event_rank = suppressWarnings(as.integer(safe_get_col(raw, "event_rank"))),
    mark = normalise_mark(mark),
    competitor = clean_text(competitor),
    nat = str_trim(as.character(nat)),
    date_of_birth = str_trim(as.character(date_of_birth)),
    pos_raw = str_trim(as.character(pos)),
    position = parse_position(pos),
    race_section = parse_race_section(pos),
    date = parse_wa_date(date),
    venue = clean_text(venue),
    age = suppressWarnings(as.numeric(safe_get_col(raw, "age"))),
    wind = suppressWarnings(as.numeric(safe_get_col(raw, "wind"))),
    athlete_id = make_athlete_id(competitor, nat, date_of_birth)
  ) %>%
  filter(
    event == EVENT_FILTER,
    category == CATEGORY_FILTER,
    !is.na(date),
    date >= RUN_START_DATE,
    date <= RUN_END_DATE,
    !is.na(position),
    !is.na(race_section),
    !is.na(competitor),
    competitor != "",
    !is.na(venue),
    venue != "",
    !is.na(athlete_id),
    athlete_id != ""
  )

cat("Rows after event/date filter:", nrow(df), "\n")
cat("Date range:", as.character(min(df$date)), "to", as.character(max(df$date)), "\n")
cat("Unique athletes:", n_distinct(df$athlete_id), "\n\n")

# -----------------------------
# Build race IDs
# -----------------------------
df <- df %>%
  mutate(
    race_id = paste(date, venue, event, category, race_section, sep = "|")
  ) %>%
  group_by(race_id) %>%
  mutate(
    race_size_raw = n(),
    duplicate_athlete_in_race = duplicated(athlete_id) | duplicated(athlete_id, fromLast = TRUE)
  ) %>%
  ungroup() %>%
  filter(
    race_size_raw >= MIN_RACE_SIZE,
    !duplicate_athlete_in_race
  ) %>%
  group_by(race_id) %>%
  mutate(race_size = n()) %>%
  ungroup() %>%
  filter(race_size >= MIN_RACE_SIZE) %>%
  arrange(date, race_id, position, competitor)

cat("Usable rows:", nrow(df), "\n")
cat("Usable races:", n_distinct(df$race_id), "\n")
cat("Mean race size:", round(mean(df$race_size), 2), "\n")
cat("Race size summary:\n")
print(summary(df$race_size))
cat("\n")

# -----------------------------
# Elo runner
# -----------------------------
run_race_elo <- function(results_df,
                         entry_mode = c("flat", "retro"),
                         retro_start_map = NULL,
                         pass_label = "Pass") {
  
  entry_mode <- match.arg(entry_mode)
  
  races <- split(results_df, results_df$race_id)
  n_races <- length(races)
  
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Running ", pass_label, "\n", sep = "")
  cat("Races:", n_races, "\n")
  
  start_time <- Sys.time()
  
  ratings <- list()
  races_cnt <- list()
  first_date <- list()
  entry_rating <- list()
  
  history_rows <- vector("list", nrow(results_df))
  row_i <- 0L
  
  for (r_i in seq_along(races)) {
    
    race <- races[[r_i]] %>%
      arrange(position, competitor)
    
    race_date <- race$date[1]
    race_id <- race$race_id[1]
    race_venue <- race$venue[1]
    race_event <- race$event[1]
    race_category <- race$category[1]
    race_section <- race$race_section[1]
    race_size <- nrow(race)
    
    athlete_ids <- race$athlete_id
    
    for (athlete_id in athlete_ids) {
      if (is.null(ratings[[athlete_id]])) {
        
        start_rating <- START_R
        
        if (
          entry_mode == "retro" &&
          !is.null(retro_start_map) &&
          athlete_id %in% names(retro_start_map)
        ) {
          start_rating <- as.numeric(retro_start_map[[athlete_id]])
        }
        
        ratings[[athlete_id]] <- start_rating
        races_cnt[[athlete_id]] <- 0L
        first_date[[athlete_id]] <- race_date
        entry_rating[[athlete_id]] <- start_rating
      }
    }
    
    old_ratings <- sapply(athlete_ids, function(x) as.numeric(ratings[[x]]))
    old_counts  <- sapply(athlete_ids, function(x) as.integer(races_cnt[[x]]))
    
    changes <- numeric(race_size)
    expected_totals <- numeric(race_size)
    actual_totals <- numeric(race_size)
    weight_totals <- numeric(race_size)
    
    for (i in seq_len(race_size)) {
      
      player_rating <- old_ratings[i]
      player_pos <- race$position[i]
      
      actual_sum <- 0
      expected_sum <- 0
      weight_sum <- 0
      
      for (j in seq_len(race_size)) {
        
        if (i == j) next
        
        opponent_rating <- old_ratings[j]
        opponent_count <- old_counts[j]
        opponent_pos <- race$position[j]
        
        w <- opponent_weight(opponent_count, cap = 10L, floor = 0.10)
        
        actual <- if (player_pos < opponent_pos) {
          1
        } else if (player_pos == opponent_pos) {
          0.5
        } else {
          0
        }
        
        expected <- expected_score(player_rating, opponent_rating)
        
        actual_sum <- actual_sum + (w * actual)
        expected_sum <- expected_sum + (w * expected)
        weight_sum <- weight_sum + w
      }
      
      changes[i] <- if (weight_sum > 0) {
        K_NORMAL * (actual_sum - expected_sum)
      } else {
        0
      }
      
      expected_totals[i] <- expected_sum
      actual_totals[i] <- actual_sum
      weight_totals[i] <- weight_sum
    }
    
    for (i in seq_len(race_size)) {
      
      athlete_id <- athlete_ids[i]
      
      rating_before <- old_ratings[i]
      races_before <- old_counts[i]
      rating_after <- rating_before + changes[i]
      races_after <- races_before + 1L
      
      ratings[[athlete_id]] <- rating_after
      races_cnt[[athlete_id]] <- races_after
      
      row_i <- row_i + 1L
      
      history_rows[[row_i]] <- tibble(
        date = race_date,
        race_id = race_id,
        venue = race_venue,
        event = race_event,
        category = race_category,
        race_section = race_section,
        race_size = race_size,
        
        athlete_id = athlete_id,
        competitor = race$competitor[i],
        nat = race$nat[i],
        date_of_birth = race$date_of_birth[i],
        age = race$age[i],
        
        mark = race$mark[i],
        wind = race$wind[i],
        pos_raw = race$pos_raw[i],
        position = race$position[i],
        
        first_appearance = races_before == 0L,
        entry_rating = as.numeric(entry_rating[[athlete_id]]),
        
        races_before = races_before,
        k = K_NORMAL,
        rating_before = rating_before,
        
        actual_total = actual_totals[i],
        expected_total = expected_totals[i],
        weight_total = weight_totals[i],
        
        rating_change = changes[i],
        rating_after = rating_after,
        races_after = races_after
      )
    }
    
    if (r_i == 1L || r_i %% 25L == 0L || r_i == n_races) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      pct <- 100 * r_i / n_races
      races_per_sec <- r_i / max(elapsed, 0.001)
      remaining <- (n_races - r_i) / max(races_per_sec, 0.001)
      
      cat(sprintf(
        "Processed %d/%d races (%.1f%%) | %.1f races/sec | elapsed %.1fs | ETA %.1fs\n",
        r_i, n_races, pct, races_per_sec, elapsed, remaining
      ))
      flush.console()
    }
  }
  
  game_history <- bind_rows(history_rows[seq_len(row_i)]) %>%
    arrange(date, race_id, position, competitor)
  
  players <- names(ratings)
  
  final_table <- tibble(
    athlete_id = players,
    rating = as.numeric(unlist(ratings[players])),
    races = as.integer(unlist(races_cnt[players])),
    first_date = as.Date(as.numeric(unlist(first_date[players])), origin = "1970-01-01"),
    entry_rating = as.numeric(unlist(entry_rating[players]))
  ) %>%
    left_join(
      game_history %>%
        arrange(athlete_id, date) %>%
        group_by(athlete_id) %>%
        summarise(
          competitor = last(competitor),
          nat = last(nat),
          date_of_birth = last(date_of_birth),
          last_date = max(date, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "athlete_id"
    ) %>%
    mutate(
      rating = round(rating, 0),
      entry_rating = round(entry_rating, 1),
      is_public_table = races >= MIN_RACES_FOR_TABLE
    ) %>%
    select(
      athlete_id,
      competitor,
      nat,
      date_of_birth,
      rating,
      races,
      first_date,
      last_date,
      entry_rating,
      is_public_table
    ) %>%
    arrange(desc(rating), competitor)
  
  list(
    history = game_history,
    final = final_table
  )
}

# -----------------------------
# Build retro start ratings from Pass 1
# -----------------------------
build_retro_start_map <- function(pass1_history,
                                  n_races = RETRO_RACES_N,
                                  min_valid_comparisons = MIN_VALID_RETRO_COMPARISONS,
                                  min_valid_weight = MIN_VALID_RETRO_WEIGHT,
                                  fallback_center = START_R) {
  
  solve_perf <- function(opp_ratings, scores, weights,
                         lower = 500, upper = 3000) {
    
    ok <- is.finite(opp_ratings) & is.finite(scores) & is.finite(weights) & weights > 0
    
    opp_ratings <- opp_ratings[ok]
    scores <- scores[ok]
    weights <- weights[ok]
    
    if (length(scores) == 0) return(fallback_center)
    
    actual_sum <- sum(weights * scores)
    weight_sum <- sum(weights)
    
    if (actual_sum <= 0) {
      return(max(lower, min(upper, min(opp_ratings) - 800)))
    }
    
    if (actual_sum >= weight_sum) {
      return(max(lower, min(upper, max(opp_ratings) + 800)))
    }
    
    f <- function(R) {
      sum(weights * expected_score(R, opp_ratings)) - actual_sum
    }
    
    flo <- f(lower)
    fhi <- f(upper)
    
    if (!is.finite(flo) || !is.finite(fhi) || flo * fhi > 0) {
      avg_opp <- weighted.mean(opp_ratings, weights)
      p <- actual_sum / weight_sum
      p <- min(0.999, max(0.001, p))
      approx_r <- avg_opp + 400 * log10(p / (1 - p))
      return(max(lower, min(upper, approx_r)))
    }
    
    uniroot(f, lower = lower, upper = upper, tol = 1e-8)$root
  }
  
  athlete_races <- pass1_history %>%
    arrange(athlete_id, date, race_id) %>%
    group_by(athlete_id) %>%
    mutate(athlete_race_index = row_number()) %>%
    ungroup() %>%
    filter(athlete_race_index <= n_races) %>%
    select(
      race_id,
      date,
      athlete_id,
      position,
      athlete_race_index
    )
  
  opponents <- pass1_history %>%
    select(
      race_id,
      opponent_id = athlete_id,
      opponent_position = position,
      opponent_rating = rating_before,
      opponent_races_before = races_before
    )
  
  comparisons <- athlete_races %>%
    inner_join(opponents, by = "race_id") %>%
    filter(athlete_id != opponent_id) %>%
    mutate(
      score = case_when(
        position < opponent_position ~ 1,
        position == opponent_position ~ 0.5,
        TRUE ~ 0
      ),
      weight = opponent_weight(opponent_races_before, cap = 10L, floor = 0.10)
    ) %>%
    filter(
      is.finite(opponent_rating),
      is.finite(score),
      is.finite(weight),
      weight > 0
    )
  
  perf <- comparisons %>%
    group_by(athlete_id) %>%
    summarise(
      valid_comparisons = n(),
      valid_weight = sum(weight),
      retro_start = solve_perf(opponent_rating, score, weight),
      .groups = "drop"
    ) %>%
    filter(
      valid_comparisons >= min_valid_comparisons,
      valid_weight >= min_valid_weight,
      is.finite(retro_start)
    )
  
  cat("\nRetro start ratings built:", nrow(perf), "athletes\n")
  cat("Retro comparison summary:\n")
  print(summary(perf$valid_comparisons))
  cat("Retro weight summary:\n")
  print(summary(perf$valid_weight))
  
  setNames(as.list(perf$retro_start), perf$athlete_id)
}

# -----------------------------
# Run Pass 1
# -----------------------------
pass1 <- run_race_elo(
  results_df = df,
  entry_mode = "flat",
  retro_start_map = NULL,
  pass_label = "Pass 1 - flat starts"
)

retro_start_map <- build_retro_start_map(
  pass1_history = pass1$history,
  n_races = RETRO_RACES_N
)

# -----------------------------
# Run Pass 2
# -----------------------------
pass2 <- run_race_elo(
  results_df = df,
  entry_mode = "retro",
  retro_start_map = retro_start_map,
  pass_label = "Pass 2 - retro starts"
)

game_history <- pass2$history
final_table <- pass2$final

# -----------------------------
# Peak ratings
# -----------------------------
peak_table <- game_history %>%
  arrange(athlete_id, date, race_id) %>%
  group_by(athlete_id) %>%
  slice_max(order_by = rating_after, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    athlete_id = athlete_id,
    competitor = competitor,
    nat = nat,
    date_of_birth = date_of_birth,
    peak_rating = round(rating_after, 0),
    peak_date = date,
    peak_venue = venue,
    peak_race_id = race_id,
    races_at_peak = races_after
  ) %>%
  left_join(
    final_table %>%
      select(athlete_id, current_rating = rating, total_races = races, last_date),
    by = "athlete_id"
  ) %>%
  mutate(
    is_public_table = total_races >= MIN_RACES_FOR_TABLE
  ) %>%
  arrange(desc(peak_rating), competitor)

# -----------------------------
# Write outputs
# -----------------------------
write_csv(game_history, history_file)
write_csv(final_table, final_file)
write_csv(peak_table, peak_file)

cat("\nDone.\n")
cat("History:", history_file, "\n")
cat("Final ratings:", final_file, "\n")
cat("Peak ratings:", peak_file, "\n\n")

cat("Final table rows:", nrow(final_table), "\n")
cat("Public table rows, min races =", MIN_RACES_FOR_TABLE, ":", sum(final_table$is_public_table), "\n\n")

cat("Highest current rating:\n")
print(final_table %>% slice_head(n = 10))

cat("\nHighest peak ratings:\n")
print(peak_table %>% slice_head(n = 10))

# -----------------------------
# Diagnostic: race size distribution after race-section split
# -----------------------------
cat("\nRace size distribution:\n")
print(table(df$race_size))

# -----------------------------
# Diagnostic: Usain Bolt Beijing 2008 quarter-final grouping
# -----------------------------
cat("\nBolt Beijing 2008 check:\n")

bolt_race <- df %>%
  filter(
    competitor == "Usain BOLT",
    date >= as.Date("2008-08-01"),
    date <= as.Date("2008-08-31")
  ) %>%
  arrange(date) %>%
  slice(1)

if (nrow(bolt_race) > 0) {
  bolt_race_id <- bolt_race$race_id[1]
  
  print(
    df %>%
      filter(race_id == bolt_race_id) %>%
      arrange(position, competitor) %>%
      select(
        competitor,
        nat,
        mark,
        wind,
        pos_raw,
        position,
        race_section,
        race_size,
        date,
        venue
      )
  )
} else {
  cat("No Bolt race found in August 2008.\n")
}

# -----------------------------
# ggplot2 chart - selected athletes + top 10 / top 100 average
# -----------------------------
plot_athletes <- c(
  "Usain BOLT",
  "Justin GATLIN",
  "Yohan BLAKE",
  "Asafa POWELL",
  "Noah LYLES"
)

plot_history <- game_history %>%
  filter(competitor %in% plot_athletes) %>%
  arrange(competitor, date, race_id) %>%
  group_by(competitor, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

daily_latest <- game_history %>%
  arrange(date, race_id) %>%
  group_by(date, athlete_id) %>%
  slice_tail(n = 1) %>%
  ungroup()

top_averages <- daily_latest %>%
  group_by(date) %>%
  arrange(desc(rating_after), .by_group = TRUE) %>%
  summarise(
    top_10_avg = mean(head(rating_after, 10), na.rm = TRUE),
    top_100_avg = mean(head(rating_after, 100), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c(top_10_avg, top_100_avg),
    names_to = "series",
    values_to = "rating_after"
  ) %>%
  mutate(
    series = case_when(
      series == "top_10_avg" ~ "Top 10 average",
      series == "top_100_avg" ~ "Top 100 average",
      TRUE ~ series
    )
  )

if (nrow(plot_history) == 0L) {
  warning("No rows found for selected athletes. Check name spelling/capitalisation in competitor column.")
} else {
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = plot_history,
      ggplot2::aes(x = date, y = rating_after, colour = competitor),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = plot_history,
      ggplot2::aes(x = date, y = rating_after, colour = competitor),
      size = 1.5
    ) +
    ggplot2::geom_line(
      data = top_averages,
      ggplot2::aes(x = date, y = rating_after, colour = series),
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = "100m Elo rating history",
      subtitle = "Selected athletes with top 10 and top 100 average ratings",
      x = "Date",
      y = "Elo rating",
      colour = "Series"
    ) +
    ggplot2::theme_minimal()

  print(p)
}

######################################################################


# Pick one athlete/date to inspect
target_athlete <- "Usain BOLT"
target_date <- as.Date("2009-08-16")

target_row <- game_history %>%
  filter(
    competitor == target_athlete,
    date == target_date
  ) %>%
  slice(1)

target_race_id <- target_row$race_id[1]
target_athlete_id <- target_row$athlete_id[1]

race <- game_history %>%
  filter(race_id == target_race_id) %>%
  arrange(position, competitor)

player <- race %>%
  filter(athlete_id == target_athlete_id) %>%
  slice(1)

pairwise_breakdown <- race %>%
  filter(athlete_id != target_athlete_id) %>%
  transmute(
    opponent = competitor,
    opponent_nat = nat,
    opponent_position = position,
    opponent_pos_raw = pos_raw,
    opponent_rating_before = rating_before,
    opponent_races_before = races_before,
    opponent_weight = opponent_weight(races_before),
    player_position = player$position[1],
    player_rating_before = player$rating_before[1],
    actual = case_when(
      player$position[1] < position ~ 1,
      player$position[1] == position ~ 0.5,
      TRUE ~ 0
    ),
    expected = expected_score(player$rating_before[1], rating_before),
    weighted_actual = opponent_weight * actual,
    weighted_expected = opponent_weight * expected,
    contribution = weighted_actual - weighted_expected
  ) %>%
  mutate(
    contribution_to_rating_change =
      K_NORMAL * contribution
  ) %>%
  arrange(desc(contribution_to_rating_change))

print(pairwise_breakdown)

cat("\nRace summary:\n")
cat("Athlete:", player$competitor[1], "\n")
cat("Race:", target_race_id, "\n")
cat("Position:", player$pos_raw[1], "\n")
cat("Rating before:", round(player$rating_before[1], 1), "\n")
cat("Actual total:", round(sum(pairwise_breakdown$weighted_actual), 3), "\n")
cat("Expected total:", round(sum(pairwise_breakdown$weighted_expected), 3), "\n")
cat("Weight total:", round(sum(pairwise_breakdown$opponent_weight), 3), "\n")
cat("Rating change:", round(sum(pairwise_breakdown$contribution_to_rating_change), 3), "\n")
cat("Rating after:", round(player$rating_after[1], 1), "\n")








