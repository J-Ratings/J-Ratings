suppressPackageStartupMessages({
  library(data.table)
  library(tictoc)
  library(beepr)
})

options(
  error = function() {
    try(beepr::beep(), silent = TRUE)
  }
)

tic()

root <- "C:/Users/stjuk/Documents/GitHub/J-Ratings"

master_path <- file.path(
  root,
  "EuropeanFootball",
  "pipeline_data",
  "Matches_Clean_Combined",
  "european_football_all_matches.csv"
)

import_path <- file.path(
  root,
  "EuropeanFootball",
  "pipeline_data",
  "Manual_Sources",
  "Denmark",
  "denmark_superliga_1991_92_to_2025_26.csv"
)

alias_path <- file.path(
  root,
  "EuropeanFootball",
  "pipeline_data",
  "Reference",
  "team_aliases.csv"
)

backup_dir <- "C:/Users/stjuk/Documents/GitHub/Miscellaneous J-Ratings Backup"

dir.create(
  backup_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

stopifnot(
  file.exists(master_path),
  file.exists(import_path),
  file.exists(alias_path)
)

master <- fread(
  master_path,
  encoding = "UTF-8"
)

denmark <- fread(
  import_path,
  encoding = "UTF-8"
)

aliases <- fread(
  alias_path,
  encoding = "UTF-8"
)

required_import_cols <- c(
  "Country",
  "Competition",
  "CompetitionType",
  "Tier",
  "League",
  "Season",
  "Date",
  "Home",
  "Away",
  "Result",
  "Score",
  "Source"
)

missing_import_cols <- setdiff(
  required_import_cols,
  names(denmark)
)

if (length(missing_import_cols) > 0L) {
  stop(
    paste(
      "Import file is missing columns:",
      paste(missing_import_cols, collapse = ", ")
    )
  )
}

required_alias_cols <- c(
  "Country",
  "SourceName",
  "CanonicalName"
)

missing_alias_cols <- setdiff(
  required_alias_cols,
  names(aliases)
)

if (length(missing_alias_cols) > 0L) {
  stop(
    paste(
      "Alias file is missing columns:",
      paste(missing_alias_cols, collapse = ", ")
    )
  )
}

denmark[, Date := as.IDate(Date)]

if (
  any(is.na(denmark$Date)) ||
  any(is.na(denmark$Home) | denmark$Home == "") ||
  any(is.na(denmark$Away) | denmark$Away == "") ||
  any(is.na(denmark$Result) | denmark$Result == "") ||
  any(is.na(denmark$Score) | denmark$Score == "")
) {
  stop("Denmark import contains blank/invalid Elo-required fields.")
}

if (any(denmark$Season == "1991")) {
  stop("The intentionally excluded spring-1991 season is present.")
}

valid_results <- c(
  "1-0",
  "0-1",
  "0.5-0.5"
)

bad_results <- denmark[
  !(Result %chin% valid_results)
]

if (nrow(bad_results) > 0L) {
  stop("Denmark import contains invalid Result values.")
}

import_dup <- denmark[
  ,
  .N,
  by = .(
    Date,
    Home,
    Away,
    Competition
  )
][
  N > 1L
]

if (nrow(import_dup) > 0L) {
  stop("Duplicate fixture keys exist inside the Denmark import.")
}

master_rows_before <- nrow(master)

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

backup_path <- file.path(
  backup_dir,
  paste0(
    "european_football_all_matches_before_denmark_import_",
    timestamp,
    ".csv"
  )
)

fwrite(
  master,
  backup_path,
  bom = TRUE
)

# Compare against existing master rows using the exact source fixture key.
master_compare <- copy(master)

if (!inherits(master_compare$Date, "IDate")) {
  master_compare[, Date := as.IDate(Date)]
}

key_cols <- c(
  "Date",
  "Home",
  "Away",
  "Competition"
)

master_compare[
  ,
  fixture_key := do.call(
    paste,
    c(
      .SD,
      sep = "\r"
    )
  ),
  .SDcols = key_cols
]

denmark[
  ,
  fixture_key := do.call(
    paste,
    c(
      .SD,
      sep = "\r"
    )
  ),
  .SDcols = key_cols
]

existing_keys <- master_compare$fixture_key

already_present <- denmark[
  fixture_key %chin% existing_keys
]

new_rows <- denmark[
  !(fixture_key %chin% existing_keys)
]

# If an exact key already exists, its Score/Result must agree.
if (nrow(already_present) > 0L) {

  existing_match <- master_compare[
    already_present,
    on = "fixture_key",
    nomatch = 0L,
    allow.cartesian = TRUE
  ]

  conflicts <- existing_match[
    as.character(Score) != as.character(i.Score) |
      as.character(Result) != as.character(i.Result)
  ]

  if (nrow(conflicts) > 0L) {
    stop(
      paste(
        "Existing master rows conflict with",
        nrow(conflicts),
        "Denmark import row(s). No write performed."
      )
    )
  }
}

denmark[, fixture_key := NULL]
new_rows[, fixture_key := NULL]

# Align the import to the master schema without dropping any master columns.
missing_in_import <- setdiff(
  names(master),
  names(new_rows)
)

for (nm in missing_in_import) {
  new_rows[, (nm) := NA]
}

extra_import_cols <- setdiff(
  names(new_rows),
  names(master)
)

if (length(extra_import_cols) > 0L) {
  new_rows[
    ,
    (extra_import_cols) := NULL
  ]
}

setcolorder(
  new_rows,
  names(master)
)

combined <- rbindlist(
  list(
    master,
    new_rows
  ),
  use.names = TRUE,
  fill = TRUE
)

# Final duplicate/conflict check for the imported competition.
combined_check <- copy(combined)
combined_check[, Date := as.IDate(Date)]

denmark_check <- combined_check[
  Country == "Denmark" &
    Competition == "danish_superliga"
]

duplicate_keys_after <- denmark_check[
  ,
  .N,
  by = .(
    Date,
    Home,
    Away,
    Competition
  )
][
  N > 1L
]

if (nrow(duplicate_keys_after) > 0L) {
  stop("Duplicate Denmark fixture keys would exist after import. No write performed.")
}

fwrite(
  combined,
  master_path,
  bom = TRUE
)

# Alias audit only: do NOT auto-edit aliases.
teams <- sort(
  unique(
    c(
      denmark$Home,
      denmark$Away
    )
  )
)

denmark_aliases <- aliases[
  Country == "Denmark"
]

resolved_teams <- unique(
  denmark_aliases[
    SourceName %chin% teams,
    SourceName
  ]
)

unresolved_teams <- setdiff(
  teams,
  resolved_teams
)

season_counts <- denmark[
  ,
  .N,
  by = Season
][
  order(Season)
]

toc_result <- toc(
  quiet = TRUE
)

seconds <- round(
  as.numeric(
    toc_result$toc -
      toc_result$tic
  ),
  2
)

final_output <- paste(
  "DENMARK SUPERLIGA IMPORT",
  paste0("Master rows before: ", master_rows_before),
  paste0("Import rows supplied: ", nrow(denmark)),
  paste0("Already present: ", nrow(already_present)),
  paste0("Rows added: ", nrow(new_rows)),
  paste0("Master rows after: ", nrow(combined)),
  paste0("Backup: ", backup_path),
  "",
  "SEASON COUNTS",
  paste(
    capture.output(
      print(
        season_counts,
        nrows = Inf
      )
    ),
    collapse = "\n"
  ),
  "",
  paste0(
    "Exact Denmark SourceName aliases already present: ",
    length(resolved_teams),
    "/",
    length(teams)
  ),
  paste0(
    "Unresolved Denmark SourceNames: ",
    if (length(unresolved_teams) == 0L) {
      "none"
    } else {
      paste(unresolved_teams, collapse = ", ")
    }
  ),
  "",
  "NOTE: aliases were audited only and were not modified.",
  paste0("Seconds: ", seconds),
  sep = "\n"
)

print(final_output)

beep()
