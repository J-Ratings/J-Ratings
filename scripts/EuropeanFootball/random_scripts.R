suppressPackageStartupMessages({
  library(rvest)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

master_file <- paste0(
  "C:/Users/stjuk/Documents/GitHub/J-Ratings/",
  "EuropeanFootball/pipeline_data/Matches_Clean_Combined/",
  "european_football_all_matches.csv"
)

wiki_url <- "https://en.wikipedia.org/wiki/2025%E2%80%9326_Scottish_Premiership"

clean_score <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "–|—|−", "-")
  x <- str_extract(x, "\\d+\\s*-\\s*\\d+")
  str_replace_all(x, "\\s+", "")
}

page <- read_html(wiki_url)

tables <- page %>%
  html_elements("table") %>%
  html_table(
    fill = TRUE,
    convert = FALSE
  )

candidate_ids <- which(
  vapply(
    tables,
    function(tb) {
      nrow(tb) == 12L &&
        ncol(tb) >= 13L
    },
    logical(1)
  )
)

if (length(candidate_ids) == 0L) {
  stop("No 12-team Scottish Premiership results matrix found.")
}

candidate_scores <- vapply(
  candidate_ids,
  function(i) {
    sum(
      !is.na(
        clean_score(
          unlist(
            tables[[i]],
            use.names = FALSE
          )
        )
      )
    )
  },
  integer(1)
)

table_id <- candidate_ids[
  which.max(candidate_scores)
]

tb <- tables[[table_id]]

if (
  nrow(tb) != 12L ||
  ncol(tb) != 13L
) {
  stop("Unexpected Scottish Premiership results-matrix dimensions.")
}

wiki_teams <- as.character(
  tb[[1]]
) %>%
  str_replace_all("\\[[^]]+\\]", "") %>%
  str_squish()

headers <- names(tb)[-1]

diagonal_check <- lapply(
  seq_len(12L),
  function(r) {
    
    values <- as.character(
      tb[r, -1]
    )
    
    non_scores <- which(
      is.na(
        clean_score(values)
      )
    )
    
    tibble(
      Row = r,
      Team = wiki_teams[r],
      Header = if (
        length(non_scores) == 1L
      ) {
        headers[non_scores]
      } else {
        paste(
          headers[non_scores],
          collapse = ", "
        )
      },
      NonScoreCells = length(non_scores)
    )
  }
) %>%
  bind_rows()

rows <- list()

for (r in seq_len(12L)) {
  
  home <- wiki_teams[r]
  
  for (j in seq_len(12L)) {
    
    away <- wiki_teams[j]
    
    if (home == away) {
      next
    }
    
    score <- clean_score(
      tb[[j + 1L]][r]
    )
    
    if (is.na(score)) {
      next
    }
    
    rows[[length(rows) + 1L]] <- tibble(
      Home = home,
      Away = away,
      Score = score
    )
  }
}

wiki_results <- bind_rows(rows)

master <- read_csv(
  master_file,
  show_col_types = FALSE
)

scotland <- master %>%
  filter(
    Country == "Scotland",
    Competition == "premiership",
    Season == "2025/26"
  )

wiki_team_check <- tibble(
  WikipediaTeam = sort(
    unique(
      c(
        wiki_results$Home,
        wiki_results$Away
      )
    )
  )
)

master_team_check <- tibble(
  MasterTeam = sort(
    unique(
      c(
        scotland$Home,
        scotland$Away
      )
    )
  )
)

check <- tibble(
  WikipediaRows = nrow(wiki_results),
  WikipediaTeams = n_distinct(c(
    wiki_results$Home,
    wiki_results$Away
  )),
  MasterRows = nrow(scotland),
  MasterTeams = n_distinct(c(
    scotland$Home,
    scotland$Away
  )),
  MasterScored = sum(
    !is.na(scotland$Score) &
      scotland$Score != ""
  ),
  MasterBlank = sum(
    is.na(scotland$Score) |
      scotland$Score == ""
  )
)

print(
  check,
  row.names = FALSE
)

print(
  diagonal_check,
  n = Inf,
  row.names = FALSE
)

print(
  wiki_team_check,
  n = Inf,
  row.names = FALSE
)

print(
  master_team_check,
  n = Inf,
  row.names = FALSE
)