# scripts/england_football/03_write_json.R

library(dplyr)
library(readr)
library(jsonlite)
library(stringi)
library(stringr)

options(stringsAsFactors = FALSE)

# -----------------------------
# Paths
# -----------------------------

repo_dir <- normalizePath(
  Sys.getenv("J_RATINGS_REPO", "C:/Users/stjuk/OneDrive/Documents/GitHub/J-Ratings"),
  winslash = "/",
  mustWork = FALSE
)

src_dir <- file.path(
  repo_dir,
  "EnglishFootball",
  "pipeline_data",
  "Elo"
)

base_data_dir <- file.path(repo_dir, "EnglishFootball", "data")
history_out   <- file.path(base_data_dir, "history")
games_out     <- file.path(base_data_dir, "games")

dir.create(base_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(history_out,   recursive = TRUE, showWarnings = FALSE)
dir.create(games_out,     recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Helpers
# -----------------------------

slug <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

tier_to_division <- function(tier, fallback_league = NULL) {
  out <- dplyr::case_when(
    tier == 1L ~ "Premier League",
    tier == 2L ~ "Championship",
    tier == 3L ~ "League 1",
    tier == 4L ~ "League 2",
    TRUE ~ NA_character_
  )
  
  if (!is.null(fallback_league)) {
    out[is.na(out)] <- as.character(fallback_league[is.na(out)])
  }
  
  out
}

season_label <- function(d) {
  d <- as.Date(d)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  
  start_year <- ifelse(m >= 7L, y, y - 1L)
  end_short <- substr(as.character(start_year + 1L), 3, 4)
  
  paste0(start_year, "-", end_short)
}

# -----------------------------
# Load CSVs
# -----------------------------

final_csv <- file.path(src_dir, "football_elo_final_ratings.csv")
hist_csv  <- file.path(src_dir, "football_elo_game_history.csv")

if (!file.exists(final_csv)) stop("Missing file: ", final_csv)
if (!file.exists(hist_csv))  stop("Missing file: ", hist_csv)

final <- read_csv(final_csv, show_col_types = FALSE)
ghist <- read_csv(hist_csv, show_col_types = FALSE)

# -----------------------------
# Validate columns
# -----------------------------

required_final <- c("Team", "Rating", "Games")
required_hist <- c(
  "League", "Tier", "Date", "Home", "Away", "Result", "Score",
  "HomeRating_Before", "AwayRating_Before",
  "HomeRating_After", "AwayRating_After"
)

miss_final <- setdiff(required_final, names(final))
miss_hist  <- setdiff(required_hist, names(ghist))

if (length(miss_final) > 0) {
  stop("Missing columns in final CSV: ", paste(miss_final, collapse = ", "))
}

if (length(miss_hist) > 0) {
  stop("Missing columns in history CSV: ", paste(miss_hist, collapse = ", "))
}

# -----------------------------
# Prepare history
# -----------------------------

ghist <- ghist %>%
  mutate(
    Date   = as.Date(Date),
    Home   = trimws(as.character(Home)),
    Away   = trimws(as.character(Away)),
    League = trimws(as.character(League)),
    Score  = trimws(as.character(Score)),
    Tier   = as.integer(Tier)
  ) %>%
  filter(!is.na(Date), Home != "", Away != "")

asof_date <- max(ghist$Date, na.rm = TRUE)

# -----------------------------
# meta.json
# -----------------------------

meta <- list(
  asof = format(asof_date, "%Y-%m-%d"),
  games = nrow(ghist)
)

write_json(
  meta,
  file.path(base_data_dir, "meta.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote meta.json (asof =", meta$asof, ")\n")

# -----------------------------
# Current tier/division per team
# -----------------------------

team_last <- bind_rows(
  ghist %>% transmute(team = Home, Date, Tier, League),
  ghist %>% transmute(team = Away, Date, Tier, League)
) %>%
  filter(!is.na(Date), team != "") %>%
  arrange(team, Date) %>%
  group_by(team) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    name = team,
    last_played = as.Date(Date),
    tier = as.integer(Tier),
    division = tier_to_division(as.integer(Tier), League)
  )

# -----------------------------
# teams.json
# -----------------------------

teams_all <- final %>%
  transmute(
    name = trimws(as.character(Team)),
    rating = as.integer(round(Rating)),
    games = as.integer(Games)
  ) %>%
  left_join(team_last, by = "name") %>%
  mutate(
    id = slug(name)
  ) %>%
  select(id, name, rating, games, last_played, tier, division) %>%
  arrange(desc(rating), name)

if (anyDuplicated(teams_all$id)) {
  teams_all <- teams_all %>%
    group_by(id) %>%
    mutate(
      n = row_number(),
      id = if_else(n == 1L, id, paste0(id, "-", n - 1L))
    ) %>%
    ungroup() %>%
    select(-n)
}

cutoff_date <- as.Date(asof_date) - 365L

teams_tbl <- teams_all %>%
  filter(!is.na(last_played) & last_played >= cutoff_date) %>%
  select(id, name, rating, games, tier, division) %>%
  arrange(desc(rating), name)

write_json(
  teams_tbl,
  file.path(base_data_dir, "teams.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat(
  "Wrote teams.json (n =",
  nrow(teams_tbl),
  ", cutoff =",
  format(cutoff_date, "%Y-%m-%d"),
  ")\n"
)

name_to_id <- setNames(teams_all$id, teams_all$name)

# -----------------------------
# Long rating history
# -----------------------------

hist_long <- bind_rows(
  ghist %>%
    transmute(
      team = Home,
      date = Date,
      season = season_label(Date),
      rating = HomeRating_After,
      tier = as.integer(Tier),
      division = tier_to_division(as.integer(Tier), League)
    ),
  ghist %>%
    transmute(
      team = Away,
      date = Date,
      season = season_label(Date),
      rating = AwayRating_After,
      tier = as.integer(Tier),
      division = tier_to_division(as.integer(Tier), League)
    )
) %>%
  filter(!is.na(rating), team != "") %>%
  arrange(team, date) %>%
  group_by(team, date) %>%
  slice_tail(n = 1) %>%
  ungroup()

# -----------------------------
# season_starts.json
# -----------------------------

season_starts_long <- bind_rows(
  ghist %>%
    transmute(
      team = Home,
      date = Date,
      season = season_label(Date),
      elo = HomeRating_Before,
      tier = as.integer(Tier),
      division = tier_to_division(as.integer(Tier), League)
    ),
  ghist %>%
    transmute(
      team = Away,
      date = Date,
      season = season_label(Date),
      elo = AwayRating_Before,
      tier = as.integer(Tier),
      division = tier_to_division(as.integer(Tier), League)
    )
) %>%
  filter(team != "", !is.na(date), !is.na(elo), !is.na(season)) %>%
  arrange(team, season, date) %>%
  group_by(team, season) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    id = unname(name_to_id[team]),
    elo = as.integer(round(elo))
  ) %>%
  filter(!is.na(id)) %>%
  transmute(
    season = as.character(season),
    id = as.character(id),
    name = as.character(team),
    elo = as.integer(elo),
    tier = as.integer(tier),
    division = as.character(division)
  ) %>%
  arrange(desc(season), desc(elo), name)

write_json(
  season_starts_long,
  file.path(base_data_dir, "season_starts.json"),
  auto_unbox = TRUE,
  pretty = FALSE
)

cat("Wrote season_starts.json (rows =", nrow(season_starts_long), ")\n")

# -----------------------------
# Per-team rating history JSON
# -----------------------------

n_hist_written <- 0L

for (tm in names(name_to_id)) {
  id <- name_to_id[[tm]]
  
  df <- hist_long %>%
    filter(team == tm) %>%
    arrange(date) %>%
    transmute(
      date = format(date, "%Y-%m-%d"),
      season = as.character(season),
      rating = as.integer(round(rating)),
      tier = as.integer(tier),
      division = as.character(division)
    )
  
  if (nrow(df) > 0) {
    write_json(
      df,
      file.path(history_out, paste0(id, ".json")),
      auto_unbox = TRUE,
      pretty = FALSE
    )
    
    n_hist_written <- n_hist_written + 1L
  }
}

cat("Wrote rating history files:", n_hist_written, "\n")

# -----------------------------
# Per-team games JSON
# -----------------------------

n_games_written <- 0L

for (tm in names(name_to_id)) {
  id <- name_to_id[[tm]]
  
  df <- ghist %>%
    filter(Home == tm | Away == tm) %>%
    arrange(desc(Date)) %>%
    mutate(
      season = season_label(Date),
      delta_num = case_when(
        Home == tm ~ HomeRating_After - HomeRating_Before,
        Away == tm ~ AwayRating_After - AwayRating_Before,
        TRUE ~ NA_real_
      ),
      delta = ifelse(
        !is.na(delta_num),
        sprintf("%+0.1f", round(delta_num, 1)),
        NA_character_
      ),
      division = tier_to_division(as.integer(Tier), League)
    ) %>%
    transmute(
      date = format(Date, "%Y-%m-%d"),
      season = as.character(season),
      league = as.character(League),
      tier = as.integer(Tier),
      division = as.character(division),
      home = as.character(Home),
      homeElo = as.integer(round(HomeRating_Before)),
      away = as.character(Away),
      awayElo = as.integer(round(AwayRating_Before)),
      result = as.character(Result),
      score = as.character(Score),
      delta
    )
  
  if (nrow(df) > 0) {
    write_json(
      df,
      file.path(games_out, paste0(id, ".json")),
      auto_unbox = TRUE,
      pretty = FALSE
    )
    
    n_games_written <- n_games_written + 1L
  }
}

cat("Wrote games files:", n_games_written, "\n")
cat("Done.\n")