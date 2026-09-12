library(data.table)
library(rvest)
library(stringr)
library(stringi)
library(tictoc)
library(beepr)

tic("Ukraine 2025/26 update")

ROOT <- "C:/Users/stjuk/Documents/GitHub/J-Ratings"

master_path <- file.path(
  ROOT,
  "EuropeanFootball",
  "pipeline_data",
  "Matches_Clean_Combined",
  "european_football_all_matches.csv"
)

out_dir <- file.path(
  ROOT,
  "EuropeanFootball",
  "pipeline_data",
  "Manual_Sources",
  "Ukraine"
)

audit_dir <- file.path(
  out_dir,
  "audits"
)

backup_dir <- file.path(
  ROOT,
  "EuropeanFootball",
  "pipeline_data",
  "Matches_Clean_Combined",
  "backups"
)

candidate_path <- file.path(
  out_dir,
  "ukraine_premier_league_2025_26_candidate.csv"
)

audit_path <- file.path(
  audit_dir,
  "ukraine_premier_league_2025_26_audit.csv"
)

wiki_url <- "https://en.wikipedia.org/wiki/2025%E2%80%9326_Ukrainian_Premier_League"
rsssf_url <- "https://www.rsssf.org/tableso/oekr2026.html"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

success <- FALSE
error_message <- NULL
backup_path <- NA_character_
wiki_n <- NA_integer_
rsssf_n <- NA_integer_
matched_n <- NA_integer_
master_before <- NA_integer_
master_after <- NA_integer_

clean_text <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("[–—−]", "-", x)
  x <- gsub("\\[[^]]*\\]", "", x)
  x <- trimws(gsub("\\s+", " ", x))
  x
}

canon <- function(x) {
  x <- clean_text(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
  
  out <- x
  
  out[grepl("^dynamo", x)] <- "dynamo_kyiv"
  out[grepl("^epicenter|^epitsentr", x)] <- "epicenter"
  out[grepl("^karpaty", x)] <- "karpaty_lviv"
  out[grepl("^kolos", x)] <- "kolos_kovalivka"
  out[grepl("^kryvbas", x)] <- "kryvbas_kryvyi_rih"
  out[grepl("^kudrivka", x)] <- "kudrivka"
  out[grepl("^lnz", x)] <- "lnz_cherkasy"
  out[grepl("metalist.*1925|metalist.?25", x)] <- "metalist_1925"
  out[grepl("^obolon", x)] <- "obolon_kyiv"
  out[grepl("^oleksandriya|^olexandrija|^olexandri", x)] <- "oleksandriya"
  out[grepl("^polissya|^polissja", x)] <- "polissya_zhytomyr"
  out[grepl("^poltava$|^sc poltava", x)] <- "sc_poltava"
  out[grepl("^rukh|^ruch", x)] <- "rukh_lviv"
  out[grepl("^shakhtar|^sachtar", x)] <- "shakhtar_donetsk"
  out[grepl("^veres", x)] <- "veres_rivne"
  out[grepl("^zorya|^zorja", x)] <- "zorya_luhansk"
  
  out
}

parse_score <- function(x) {
  x <- clean_text(x)
  
  m <- str_match(
    x,
    "(\\d+)\\s*-\\s*(\\d+)"
  )
  
  ifelse(
    is.na(m[, 1]),
    NA_character_,
    paste0(m[, 2], "-", m[, 3])
  )
}

score_result <- function(score) {
  m <- str_match(
    score,
    "^(\\d+)-(\\d+)$"
  )
  
  hg <- as.integer(m[, 2])
  ag <- as.integer(m[, 3])
  
  fifelse(
    hg > ag,
    "H",
    fifelse(
      hg < ag,
      "A",
      "D"
    )
  )
}

tryCatch({
  
  # --------------------------------------------------
  # Wikipedia: fixture identity + results
  # --------------------------------------------------
  
  wiki_doc <- read_html(wiki_url)
  
  wiki_tables <- html_table(
    wiki_doc,
    fill = TRUE
  )
  
  matrix_candidates <- Filter(
    function(x) {
      nrow(x) == 16L &&
        ncol(x) == 17L &&
        grepl(
          "Home",
          names(x)[1],
          ignore.case = TRUE
        )
    },
    wiki_tables
  )
  
  if (length(matrix_candidates) != 1L) {
    stop(
      paste0(
        "Could not uniquely identify Wikipedia results matrix. Candidates found: ",
        length(matrix_candidates)
      )
    )
  }
  
  mat <- as.data.table(
    matrix_candidates[[1]]
  )
  
  row_team <- clean_text(
    mat[[1]]
  )
  
  away_team <- c(
    "Dynamo Kyiv",
    "Epitsentr Kamianets-Podilskyi",
    "Karpaty Lviv",
    "Kolos Kovalivka",
    "Kryvbas Kryvyi Rih",
    "Kudrivka",
    "LNZ Cherkasy",
    "Metalist 1925 Kharkiv",
    "Obolon Kyiv",
    "Oleksandriya",
    "Polissya Zhytomyr",
    "SC Poltava",
    "Rukh Lviv",
    "Shakhtar Donetsk",
    "Veres Rivne",
    "Zorya Luhansk"
  )
  
  wiki_matches <- rbindlist(
    lapply(
      seq_len(nrow(mat)),
      function(i) {
        vals <- unlist(
          mat[
            i,
            2:17,
            with = FALSE
          ],
          use.names = FALSE
        )
        
        data.table(
          Home = row_team[i],
          Away = away_team,
          RawScore = vals
        )
      }
    )
  )
  
  wiki_matches[
    ,
    Score := parse_score(RawScore)
  ]
  
  wiki_matches <- wiki_matches[
    !is.na(Score)
  ]
  
  wiki_matches[
    ,
    `:=`(
      HomeCanon = canon(Home),
      AwayCanon = canon(Away),
      Result = score_result(Score)
    )
  ]
  
  wiki_n <- nrow(wiki_matches)
  
  if (wiki_n != 240L) {
    stop(
      paste0(
        "Wikipedia produced ",
        wiki_n,
        " played matches; expected 240."
      )
    )
  }
  
  if (
    anyDuplicated(
      wiki_matches[
        ,
        paste(
          HomeCanon,
          AwayCanon,
          sep = "|||"
        )
      ]
    )
  ) {
    stop(
      "Wikipedia contains duplicate canonical home/away fixtures."
    )
  }
  
  # --------------------------------------------------
  # RSSSF: dates only
  # --------------------------------------------------
  
  rsssf_doc <- read_html(
    rsssf_url
  )
  
  rsssf_text <- html_text2(
    html_element(
      rsssf_doc,
      "body"
    )
  )
  
  rsssf_lines <- str_split(
    rsssf_text,
    "\n"
  )[[1]]
  
  rsssf_lines <- trimws(
    rsssf_lines
  )
  
  rsssf_lines <- rsssf_lines[
    rsssf_lines != ""
  ]
  
  section_start <- grep(
    "^VBet Premier League 2025/26$",
    rsssf_lines,
    ignore.case = TRUE
  )[1]
  
  section_end <- grep(
    "^Vbet Ukrainian Cup 2025/26$",
    rsssf_lines,
    ignore.case = TRUE
  )
  
  section_end <- section_end[
    section_end > section_start
  ]
  
  if (
    is.na(section_start) ||
    length(section_end) == 0L
  ) {
    stop(
      "Could not isolate rendered RSSSF Premier League section."
    )
  }
  
  section <- rsssf_lines[
    section_start:(section_end[1] - 1L)
  ]
  
  round1 <- grep(
    "^Round 1\\b",
    section,
    ignore.case = TRUE
  )[1]
  
  if (is.na(round1)) {
    stop(
      "Could not find RSSSF Round 1."
    )
  }
  
  section <- section[
    round1:length(section)
  ]
  
  month_lookup <- c(
    Jan = 1L,
    Feb = 2L,
    Mar = 3L,
    Apr = 4L,
    May = 5L,
    Jun = 6L,
    Jul = 7L,
    Aug = 8L,
    Sep = 9L,
    Oct = 10L,
    Nov = 11L,
    Dec = 12L
  )
  
  current_year <- 2025L
  previous_month <- NA_integer_
  current_date <- as.IDate(NA)
  
  rsssf_rows <- list()
  rsssf_i <- 0L
  
  for (line in section) {
    
    date_match <- str_match(
      line,
      "^\\[([A-Z][a-z]{2})\\s+(\\d{1,2})\\]$"
    )
    
    if (!is.na(date_match[1, 1])) {
      
      month_name <- date_match[1, 2]
      month_num <- unname(
        month_lookup[
          month_name
        ]
      )
      
      day_num <- as.integer(
        date_match[1, 3]
      )
      
      if (
        !is.na(previous_month) &&
        previous_month >= 11L &&
        month_num <= 3L
      ) {
        current_year <- current_year + 1L
      }
      
      current_date <- as.IDate(
        sprintf(
          "%04d-%02d-%02d",
          current_year,
          month_num,
          day_num
        )
      )
      
      previous_month <- month_num
      
      next
    }
    
    match_line <- str_match(
      line,
      "^(.+?)\\s{2,}(\\d+)\\s*-\\s*(\\d+)\\s{2,}(.+?)$"
    )
    
    if (
      is.na(match_line[1, 1]) ||
      is.na(current_date)
    ) {
      next
    }
    
    home <- clean_text(
      match_line[1, 2]
    )
    
    away <- clean_text(
      match_line[1, 5]
    )
    
    score <- paste0(
      match_line[1, 3],
      "-",
      match_line[1, 4]
    )
    
    rsssf_i <- rsssf_i + 1L
    
    rsssf_rows[[rsssf_i]] <- data.table(
      Date = current_date,
      RSSSFHome = home,
      RSSSFAway = away,
      RSSSFScore = score
    )
  }
  
  if (length(rsssf_rows) == 0L) {
    stop(
      "No RSSSF match rows parsed."
    )
  }
  
  rsssf <- rbindlist(
    rsssf_rows,
    fill = TRUE
  )
  
  rsssf[
    ,
    `:=`(
      HomeCanon = canon(RSSSFHome),
      AwayCanon = canon(RSSSFAway)
    )
  ]
  
  valid_clubs <- unique(
    c(
      wiki_matches$HomeCanon,
      wiki_matches$AwayCanon
    )
  )
  
  # Restrict RSSSF rows to clubs participating in the
  # 2025/26 Ukrainian Premier League according to Wikipedia.
  # Newly promoted clubs are therefore included automatically.
  rsssf <- rsssf[
    HomeCanon %chin% valid_clubs &
      AwayCanon %chin% valid_clubs
  ]
  
  rsssf[
    ,
    MatchKey := paste(
      HomeCanon,
      AwayCanon,
      RSSSFScore,
      sep = "|||"
    )
  ]
  
  wiki_matches[
    ,
    MatchKey := paste(
      HomeCanon,
      AwayCanon,
      Score,
      sep = "|||"
    )
  ]
  
  rsssf_n <- nrow(rsssf)
  
  if (rsssf_n != 240L) {
    fwrite(
      rsssf,
      audit_path
    )
    
    stop(
      paste0(
        "RSSSF produced ",
        rsssf_n,
        " Premier League match rows; expected 240. Audit written."
      )
    )
  }
  
  if (anyDuplicated(rsssf$MatchKey)) {
    stop(
      "RSSSF contains duplicate canonical home/away/score keys."
    )
  }
  
  # --------------------------------------------------
  # Match Wikipedia results to RSSSF dates
  # --------------------------------------------------
  
  candidate <- merge(
    wiki_matches,
    rsssf[
      ,
      .(
        MatchKey,
        Date,
        RSSSFHome,
        RSSSFAway
      )
    ],
    by = "MatchKey",
    all.x = TRUE
  )
  
  unmatched <- candidate[
    is.na(Date)
  ]
  
  matched_n <- sum(
    !is.na(candidate$Date)
  )
  
  if (nrow(unmatched) > 0L) {
    fwrite(
      unmatched,
      audit_path
    )
    
    stop(
      paste0(
        nrow(unmatched),
        " Wikipedia matches lacked an RSSSF date. Audit written."
      )
    )
  }
  
  candidate[
    ,
    `:=`(
      Season = "2025/26",
      Country = "Ukraine",
      Competition = "ukrainian_premier_league",
      CompetitionType = "league",
      Tier = 1L,
      League = "Ukrainian Premier League",
      FixtureSource = "wikipedia",
      ResultSource = "wikipedia",
      DateSource = "rsssf"
    )
  ]
  
  candidate <- candidate[
    ,
    .(
      Season,
      Country,
      Competition,
      CompetitionType,
      Tier,
      League,
      Date,
      Home,
      Away,
      Result,
      Score,
      FixtureSource,
      ResultSource,
      DateSource
    )
  ]
  
  setorder(
    candidate,
    Date,
    Home,
    Away
  )
  
  if (nrow(candidate) != 240L) {
    stop(
      "Final candidate does not contain exactly 240 rows."
    )
  }
  
  fwrite(
    candidate,
    candidate_path
  )
  
  # --------------------------------------------------
  # Import
  # --------------------------------------------------
  
  master <- fread(
    master_path,
    encoding = "UTF-8"
  )
  
  master[
    ,
    Date := as.IDate(Date)
  ]
  
  existing <- master[
    Country == "Ukraine" &
      Competition == "ukrainian_premier_league" &
      Season == "2025/26"
  ]
  
  if (nrow(existing) > 0L) {
    stop(
      paste0(
        "Master already contains ",
        nrow(existing),
        " Ukraine 2025/26 rows."
      )
    )
  }
  
  candidate_master <- candidate[
    ,
    .(
      Season,
      Country,
      Competition,
      CompetitionType,
      Tier,
      League,
      Date,
      Home,
      Away,
      Result,
      Score,
      Source = "Wikipedia + RSSSF",
      SourcePage = NA,
      Stage = NA,
      DateApprox = FALSE,
      SourceFile = NA
    )
  ]
  
  candidate_keys <- candidate_master[
    ,
    paste(
      Date,
      Home,
      Away,
      sep = "|||"
    )
  ]
  
  existing_keys <- master[
    ,
    paste(
      Date,
      Home,
      Away,
      sep = "|||"
    )
  ]
  
  overlap_n <- sum(
    candidate_keys %chin% existing_keys
  )
  
  if (overlap_n > 0L) {
    stop(
      paste0(
        "Candidate unexpectedly overlaps ",
        overlap_n,
        " existing master rows."
      )
    )
  }
  
  backup_path <- file.path(
    backup_dir,
    paste0(
      "european_football_all_matches_before_ukraine_2025_26_import_",
      format(
        Sys.time(),
        "%Y%m%d_%H%M%S"
      ),
      ".csv"
    )
  )
  
  if (
    !file.copy(
      master_path,
      backup_path,
      overwrite = FALSE
    )
  ) {
    stop(
      "Could not create master backup."
    )
  }
  
  master_before <- nrow(master)
  
  combined <- rbindlist(
    list(
      master,
      candidate_master
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  setorder(
    combined,
    Date,
    Country,
    Competition,
    Home,
    Away
  )
  
  master_after <- nrow(combined)
  
  if (master_after != master_before + 240L) {
    stop(
      "Unexpected master row count after append."
    )
  }
  
  fwrite(
    combined,
    master_path
  )
  
  verify <- fread(
    master_path,
    encoding = "UTF-8"
  )
  
  verify_ukraine <- verify[
    Country == "Ukraine" &
      Competition == "ukrainian_premier_league" &
      Season == "2025/26"
  ]
  
  if (nrow(verify_ukraine) != 240L) {
    stop(
      "Post-write Ukraine verification failed."
    )
  }
  
  # --------------------------------------------------
  # Rebuild Elo + JSON quietly
  # --------------------------------------------------
  
  invisible(
    capture.output(
      source(
        file.path(
          ROOT,
          "scripts",
          "EuropeanFootball",
          "02_calculate_elo.R"
        )
      )
    )
  )
  
  invisible(
    capture.output(
      source(
        file.path(
          ROOT,
          "scripts",
          "EuropeanFootball",
          "03_write_json.R"
        )
      )
    )
  )
  
  success <- TRUE
  
}, error = function(e) {
  error_message <<- conditionMessage(e)
})

elapsed <- toc(
  quiet = TRUE
)

if (success) {
  
  beep(1)
  
  final_output <- paste0(
    "Ukraine 2025/26 update PASS\n\n",
    "Wikipedia results:   ", wiki_n, "\n",
    "RSSSF dated matches:  ", rsssf_n, "\n",
    "Matched:              ", matched_n, "\n",
    "Master before:        ", master_before, "\n",
    "Master after:         ", master_after, "\n",
    "Rows added:           240\n\n",
    "Candidate:\n",
    candidate_path,
    "\n\nBackup:\n",
    backup_path,
    "\n\nElo and JSON rebuilt.\n\n",
    "Elapsed: ",
    round(
      elapsed$toc - elapsed$tic,
      2
    ),
    " seconds"
  )
  
} else {
  
  beep(2)
  
  final_output <- paste0(
    "Ukraine 2025/26 update FAILED\n\n",
    "Error: ",
    error_message,
    "\n\n",
    "Wikipedia rows: ",
    ifelse(is.na(wiki_n), "not reached", wiki_n),
    "\n",
    "RSSSF rows: ",
    ifelse(is.na(rsssf_n), "not reached", rsssf_n),
    "\n",
    "Matched rows: ",
    ifelse(is.na(matched_n), "not reached", matched_n),
    "\n\n",
    "Audit if created:\n",
    audit_path,
    "\n\nBackup: ",
    ifelse(
      is.na(backup_path),
      "not created",
      backup_path
    ),
    "\n\nElapsed: ",
    round(
      elapsed$toc - elapsed$tic,
      2
    ),
    " seconds"
  )
}

cat(final_output, "\n")