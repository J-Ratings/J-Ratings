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

staging_path <- file.path(
  root,
  "EuropeanFootball",
  "pipeline_data",
  "Manual_Sources",
  "Russia",
  "betexplorer_staging",
  "russia_topflight_combined_staging.csv"
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

# ------------------------------------------------------------
# Read
# ------------------------------------------------------------

master <- fread(
  master_path,
  encoding = "UTF-8"
)

rus <- fread(
  staging_path,
  encoding = "UTF-8"
)

aliases <- fread(
  alias_path,
  encoding = "UTF-8"
)

master_rows_before <- nrow(master)
import_rows_supplied <- nrow(rus)

# ------------------------------------------------------------
# Basic staging checks
# ------------------------------------------------------------

if (import_rows_supplied != 8503L) {
  stop(
    paste0(
      "Expected 8,503 Russia staging rows but found ",
      import_rows_supplied,
      "."
    )
  )
}

required_staging <- c(
  "Source",
  "Country",
  "Competition",
  "CompetitionType",
  "Tier",
  "League",
  "Season",
  "Date",
  "HomeRaw",
  "AwayRaw",
  "Score",
  "Result"
)

missing_staging <- setdiff(
  required_staging,
  names(rus)
)

if (length(missing_staging) > 0L) {
  stop(
    paste(
      "Russia staging missing required columns:",
      paste(
        missing_staging,
        collapse = ", "
      )
    )
  )
}

rus[
  ,
  Date := as.IDate(Date)
]

if (rus[is.na(Date), .N] > 0L) {
  stop(
    "Russia staging contains invalid or missing dates."
  )
}

if (
  rus[
    is.na(HomeRaw) |
    trimws(HomeRaw) == "" |
    is.na(AwayRaw) |
    trimws(AwayRaw) == "",
    .N
  ] > 0L
) {
  stop(
    "Russia staging contains blank team names."
  )
}

if (
  rus[
    is.na(Score) |
    trimws(Score) == "",
    .N
  ] > 0L
) {
  stop(
    "Russia staging contains blank Score values."
  )
}

if (
  rus[
    is.na(Result) |
    trimws(Result) == "",
    .N
  ] > 0L
) {
  stop(
    "Russia staging contains blank Result values."
  )
}

if (
  rus[
    CompetitionType != "league",
    .N
  ] > 0L
) {
  stop(
    "Russia staging contains non-league CompetitionType rows."
  )
}

if (
  rus[
    Tier != 1L,
    .N
  ] > 0L
) {
  stop(
    "Russia staging contains rows that are not Tier 1."
  )
}

if (
  rus[
    Country != "Russia",
    .N
  ] > 0L
) {
  stop(
    "Russia staging contains non-Russia rows."
  )
}

# ------------------------------------------------------------
# Detect alias columns
# ------------------------------------------------------------

alias_names_lower <- tolower(
  names(aliases)
)

find_alias_col <- function(candidates) {
  
  idx <- which(
    alias_names_lower %in%
      tolower(candidates)
  )
  
  if (length(idx) == 0L) {
    return(NA_character_)
  }
  
  names(aliases)[idx[1L]]
}

alias_col <- find_alias_col(
  c(
    "Alias",
    "TeamAlias",
    "Raw",
    "RawName",
    "SourceName",
    "TeamRaw"
  )
)

canonical_col <- find_alias_col(
  c(
    "Canonical",
    "CanonicalName",
    "Team",
    "TeamCanonical",
    "StandardName"
  )
)

if (is.na(alias_col)) {
  stop(
    "Could not identify alias column in team_aliases.csv."
  )
}

if (is.na(canonical_col)) {
  stop(
    "Could not identify canonical column in team_aliases.csv."
  )
}

# ------------------------------------------------------------
# Build alias lookup
# ------------------------------------------------------------

lookup <- aliases[
  ,
  .(
    Alias = trimws(
      as.character(
        get(alias_col)
      )
    ),
    Canonical = trimws(
      as.character(
        get(canonical_col)
      )
    )
  )
]

lookup <- lookup[
  !is.na(Alias) &
    nzchar(Alias) &
    !is.na(Canonical) &
    nzchar(Canonical)
]

lookup_check <- lookup[
  ,
  .(
    CanonicalCount = uniqueN(Canonical),
    Canonical = paste(
      sort(
        unique(Canonical)
      ),
      collapse = " | "
    )
  ),
  by = Alias
]

raw_names <- sort(
  unique(
    c(
      trimws(rus$HomeRaw),
      trimws(rus$AwayRaw)
    )
  )
)

russia_lookup <- lookup_check[
  Alias %chin% raw_names
]

unresolved <- setdiff(
  raw_names,
  russia_lookup$Alias
)

if (length(unresolved) > 0L) {
  stop(
    paste(
      "Unresolved Russia aliases:",
      paste(
        unresolved,
        collapse = ", "
      )
    )
  )
}

ambiguous <- russia_lookup[
  CanonicalCount != 1L
]

if (nrow(ambiguous) > 0L) {
  stop(
    paste(
      "Ambiguous Russia aliases:",
      paste(
        ambiguous$Alias,
        collapse = ", "
      )
    )
  )
}

alias_map <- setNames(
  russia_lookup$Canonical,
  russia_lookup$Alias
)

# ------------------------------------------------------------
# Canonicalise teams
# ------------------------------------------------------------

rus[
  ,
  Home := unname(
    alias_map[
      trimws(HomeRaw)
    ]
  )
]

rus[
  ,
  Away := unname(
    alias_map[
      trimws(AwayRaw)
    ]
  )
]

if (
  rus[
    is.na(Home) |
    is.na(Away),
    .N
  ] > 0L
) {
  stop(
    "Canonicalisation produced missing Home/Away names."
  )
}

# ------------------------------------------------------------
# Required master fields
# ------------------------------------------------------------

required_master <- c(
  "Source",
  "Country",
  "Competition",
  "CompetitionType",
  "Tier",
  "League",
  "Season",
  "Date",
  "Home",
  "Away",
  "Score",
  "Result"
)

missing_master <- setdiff(
  required_master,
  names(master)
)

if (length(missing_master) > 0L) {
  stop(
    paste(
      "Master missing required columns:",
      paste(
        missing_master,
        collapse = ", "
      )
    )
  )
}

master[
  ,
  Date := as.IDate(Date)
]

# ------------------------------------------------------------
# Build import table using master schema
# ------------------------------------------------------------

import <- copy(rus)

# Raw source names are not master fields.
drop_from_import <- intersect(
  c(
    "HomeRaw",
    "AwayRaw",
    "HomeGoals",
    "AwayGoals",
    "Stage",
    "SourceURL",
    "DateRaw",
    "ScoreRaw"
  ),
  names(import)
)

if (length(drop_from_import) > 0L) {
  import[
    ,
    (drop_from_import) := NULL
  ]
}

# Add any master columns absent from staging.
for (col in setdiff(
  names(master),
  names(import)
)) {
  import[
    ,
    (col) := NA
  ]
}

# Discard staging-only columns that are not part of the master.
extra_import_cols <- setdiff(
  names(import),
  names(master)
)

if (length(extra_import_cols) > 0L) {
  import[
    ,
    (extra_import_cols) := NULL
  ]
}

setcolorder(
  import,
  names(master)
)

# ------------------------------------------------------------
# Fixture key
# ------------------------------------------------------------

key_cols <- c(
  "Country",
  "Competition",
  "Season",
  "Date",
  "Home",
  "Away"
)

make_fixture_key <- function(dt) {
  
  do.call(
    paste,
    c(
      dt[
        ,
        ..key_cols
      ],
      sep = "|"
    )
  )
}

master[
  ,
  fixture_key := make_fixture_key(master)
]

import[
  ,
  fixture_key := make_fixture_key(import)
]

# ------------------------------------------------------------
# Internal Russia duplicate audit
# ------------------------------------------------------------

internal_dupes <- import[
  ,
  .N,
  by = fixture_key
][
  N > 1L
]

if (nrow(internal_dupes) > 0L) {
  
  duplicate_rows <- import[
    fixture_key %chin%
      internal_dupes$fixture_key
  ]
  
  cat(
    "\nDUPLICATE FIXTURES INSIDE RUSSIA IMPORT\n"
  )
  
  print(
    duplicate_rows[
      ,
      .(
        Country,
        Competition,
        Season,
        Date,
        Home,
        Away,
        Score,
        Result
      )
    ],
    nrows = Inf
  )
  
  stop(
    paste0(
      "Russia import contains ",
      nrow(internal_dupes),
      " duplicate fixture keys."
    )
  )
}

# ------------------------------------------------------------
# Existing master matches
# ------------------------------------------------------------

already_present <- import[
  fixture_key %chin%
    master$fixture_key
]

# ------------------------------------------------------------
# Conflict check
# ------------------------------------------------------------

conflicts <- data.table()

if (nrow(already_present) > 0L) {
  
  master_overlap <- master[
    fixture_key %chin%
      already_present$fixture_key,
    .(
      fixture_key,
      MasterScore = Score,
      MasterResult = Result
    )
  ]
  
  import_overlap <- already_present[
    ,
    .(
      fixture_key,
      ImportScore = Score,
      ImportResult = Result
    )
  ]
  
  overlap_compare <- merge(
    import_overlap,
    master_overlap,
    by = "fixture_key",
    allow.cartesian = TRUE
  )
  
  conflicts <- overlap_compare[
    ImportScore != MasterScore |
      ImportResult != MasterResult
  ]
}

if (nrow(conflicts) > 0L) {
  
  cat(
    "\nCONFLICTING EXISTING FIXTURES\n"
  )
  
  print(
    conflicts,
    nrows = Inf
  )
  
  stop(
    paste0(
      nrow(conflicts),
      " Russia fixture conflicts found. Master was NOT modified."
    )
  )
}

# ------------------------------------------------------------
# Rows genuinely to add
# ------------------------------------------------------------

to_add <- import[
  !fixture_key %chin%
    master$fixture_key
]

already_present_n <- nrow(already_present)
rows_to_add <- nrow(to_add)

# ------------------------------------------------------------
# Sanity checks
# ------------------------------------------------------------

if (
  already_present_n +
  rows_to_add !=
  import_rows_supplied
) {
  stop(
    "Import accounting does not balance."
  )
}

# ------------------------------------------------------------
# Backup master outside repo
# ------------------------------------------------------------

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

backup_path <- file.path(
  backup_dir,
  paste0(
    "european_football_all_matches_before_Russia_",
    timestamp,
    ".csv"
  )
)

master_for_backup <- copy(master)

master_for_backup[
  ,
  fixture_key := NULL
]

fwrite(
  master_for_backup,
  backup_path,
  bom = TRUE
)

if (!file.exists(backup_path)) {
  stop(
    "Master backup was not created."
  )
}

# ------------------------------------------------------------
# Append
# ------------------------------------------------------------

master[
  ,
  fixture_key := NULL
]

to_add[
  ,
  fixture_key := NULL
]

combined <- rbindlist(
  list(
    master,
    to_add
  ),
  use.names = TRUE,
  fill = TRUE
)

expected_after <- master_rows_before +
  rows_to_add

if (nrow(combined) != expected_after) {
  stop(
    "Unexpected master row count after append."
  )
}

# ------------------------------------------------------------
# Final duplicate audit
# ------------------------------------------------------------

combined[
  ,
  fixture_key := make_fixture_key(combined)
]

final_dupes <- combined[
  ,
  .N,
  by = fixture_key
][
  N > 1L
]

# Only stop if the import introduced duplicate keys involving Russia.
russia_keys <- to_add[
  ,
  make_fixture_key(to_add)
]

new_duplicate_keys <- final_dupes[
  fixture_key %chin%
    russia_keys
]

if (nrow(new_duplicate_keys) > 0L) {
  stop(
    paste0(
      "Russia import would create ",
      nrow(new_duplicate_keys),
      " duplicate fixture keys. Master was NOT written."
    )
  )
}

combined[
  ,
  fixture_key := NULL
]

# ------------------------------------------------------------
# Write master
# ------------------------------------------------------------

fwrite(
  combined,
  master_path,
  bom = TRUE
)

# ------------------------------------------------------------
# Read-back verification
# ------------------------------------------------------------

verify <- fread(
  master_path,
  encoding = "UTF-8"
)

if (nrow(verify) != expected_after) {
  stop(
    paste0(
      "Read-back verification failed. Expected ",
      expected_after,
      " rows but found ",
      nrow(verify),
      "."
    )
  )
}

# ------------------------------------------------------------
# Russia verification
# ------------------------------------------------------------

russia_master_rows <- verify[
  Country == "Russia" &
    Competition == "russian_premier_league"
]

if (nrow(russia_master_rows) != import_rows_supplied) {
  
  stop(
    paste0(
      "Expected ",
      import_rows_supplied,
      " Russia league rows in master after import but found ",
      nrow(russia_master_rows),
      "."
    )
  )
}

# ------------------------------------------------------------
# Final output
# ------------------------------------------------------------

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
  "RUSSIA MASTER IMPORT COMPLETE",
  "",
  paste0(
    "Master rows before: ",
    master_rows_before
  ),
  paste0(
    "Import rows supplied: ",
    import_rows_supplied
  ),
  paste0(
    "Already present: ",
    already_present_n
  ),
  paste0(
    "Rows added: ",
    rows_to_add
  ),
  paste0(
    "Master rows after: ",
    nrow(verify)
  ),
  "",
  paste0(
    "Russia league rows now in master: ",
    nrow(russia_master_rows)
  ),
  paste0(
    "Russian raw names resolved: ",
    length(raw_names),
    "/",
    length(raw_names)
  ),
  paste0(
    "Import fixture conflicts: ",
    nrow(conflicts)
  ),
  paste0(
    "Import duplicate fixture keys: ",
    nrow(internal_dupes)
  ),
  "",
  paste0(
    "Backup: ",
    backup_path
  ),
  paste0(
    "Updated master: ",
    master_path
  ),
  "",
  "Russia has been imported into the master.",
  "Elo has NOT been recalculated by this script.",
  paste0(
    "Seconds: ",
    seconds
  ),
  sep = "\n"
)

cat(
  final_output,
  "\n"
)

beep()