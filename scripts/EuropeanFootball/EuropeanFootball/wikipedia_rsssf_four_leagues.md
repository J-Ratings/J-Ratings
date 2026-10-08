# Four-league historical rebuild

Run from the J-Ratings repository root in R/RStudio:

```r
source("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")
```

Dependencies: `data.table`, `xml2`, `rvest`, `stringi`. The script does not install packages or change the production master. `J_RATINGS_REPO` can override the repository location.

It discovers season pages from RSSSF's historical indexes and works backwards, including historical league names. It does not stop at the first missing season. Wikipedia provides the games and scores; RSSSF provides dates only. Football-Data is never read. Existing raw RSSSF pages are reused when available; otherwise only the required pages are fetched and cached. This script does not invoke the bulk RSSSF pipeline.

For a smaller run, set start-year bounds before sourcing:

```r
Sys.setenv(FOUR_LEAGUES_MIN_YEAR = 2000,
           FOUR_LEAGUES_MAX_YEAR = 2000,
           FOUR_LEAGUES_COUNTRIES = "Poland,Norway,Sweden,Romania")
source("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")
```

Unset those three variables with `Sys.unsetenv()` to restore the historical sweep (1900 through seasons starting in 2025). Calendar and split-year seasons are taken from the RSSSF page title. Season indexes and downloadable pages can have gaps: the earliest match recovered is evidence of coverage, not proof that older records do not exist.

Outputs are under `EuropeanFootball/pipeline_data/Manual_Sources/Wikipedia_RSSSF_Four_Leagues/`:

- `season_audit.csv`: extracted games, dated games, source URLs and errors for each attempted season.
- `dated_candidate.csv`: Wikipedia games with unambiguous matching RSSSF dates.
- `all_wikipedia_games.csv`, `unresolved.csv`: retain unresolved fixtures instead of inventing dates.
- `coverage_summary.csv`: earliest dated season found per country in this run.
- Country/season folders: raw RSSSF match evidence, Wikipedia matrix counts, team mappings and unresolved rows.
- `team_aliases.csv`: optional reviewed mappings with columns `Country,RSSSF,Wikipedia`; rerun after adding a correspondence.

Aggregate outputs describe the latest run; per-season folders from previous runs remain available. Narrow test runs overwrite aggregate summaries, so use the full bounds when building the final candidate.

Supported layouts include Wikipedia result matrices, matrices beside league standings, and simple individual match reports; RSSSF bracketed date headings, named-month dates, numeric dates and inline dates. Split-season phase information helps distinguish repeated fixtures. Exact normalized names and reviewed aliases seed a season-specific identity map. Whole-token comparisons (including punctuation-separated components, initials and a terminal possessive s) propose shortened names. Proposals require at least three matching results against two opponents, 80% agreement and a two-result lead over the next candidate. Unrelated spellings require at least six matches against four opponents, 90% agreement and a three-result lead. Unique token candidates may supply provisional opponent evidence when every source team is abbreviated, but do not become accepted identities without result checks. Identity collisions and tied proposals stay unresolved. No substring or edit-distance matching is used, and there are no built-in club-specific exceptions. Scores must agree, and competing dates remain unresolved.

When RSSSF omits the year, split-year seasons use July–December in the start year and January–June in the end year. For July–September in the COVID-extended 2019/20 season, the parser now checks the nearest originally dated matches before and after the fixture within the same phase. If the available neighboring evidence agrees on one year, it assigns that year; conflicting or absent evidence stays unresolved. Explicit years take precedence. The per-season `rsssf_evidence.csv` includes `DateHeading` and `DateBasis` for inspecting this inference. Other unusual season calendars require review.

To retry only Poland 2019/20 after this fix:

```r
Sys.setenv(FOUR_LEAGUES_COUNTRIES = "Poland",
           FOUR_LEAGUES_MIN_YEAR = 2019,
           FOUR_LEAGUES_MAX_YEAR = 2019)
source("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")
Sys.unsetenv(c("FOUR_LEAGUES_COUNTRIES", "FOUR_LEAGUES_MIN_YEAR", "FOUR_LEAGUES_MAX_YEAR"))
```

This focused retry overwrites the aggregate audit/candidate with Poland 2019/20 only; previous per-season folders remain intact. The script beeps on completion when `beepr` is installed.

`all_extracted_games_dated` means every *extracted* Wikipedia game has a date. It is not a claim of complete season coverage. Unsupported tables, awarded/annotated results, multi-leg aggregate tables, and unusual historical date layouts need review. Individual reports with the same pair and score as an extracted matrix entry are treated as another presentation of that fixture. Repeated fixtures and playoff structures should therefore be checked before import. The production dataset remains unchanged until candidate coverage is reviewed.


## Updated team and annotation handling

- Trailing match notes no longer overwrite date headings. A final result marked `remaining 75'` uses the date of completion; the original note is retained. Unrecognized annotations stay unresolved.
- `team_map.csv` now records the suggested identity, supporting results, results compared, distinct supporting opponents and runner-up support.
- `team_match_candidates.csv` records the competing candidates for each matching pass.
- `team_identity_unresolved` distinguishes identity failures from score disagreements and missing dates.
- Season audits now include counts of RSSSF results, dated RSSSF results and unresolved source names. A page containing only standings cannot supply regular-season match dates.

Run all countries/seasons (clear any prior focused-test settings):

```r
Sys.unsetenv(c("FOUR_LEAGUES_COUNTRIES", "FOUR_LEAGUES_MIN_YEAR",
              "FOUR_LEAGUES_MAX_YEAR", "FOUR_LEAGUES_FUNCTIONS_ONLY"))
source("scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R")
```

This revision has not been executed by the assistant; inspect the new audit before importing anything into the production master.
