# J-Ratings European historical data: RSSSF-only rebuild handover

## Current objective

Rebuild historical domestic top-flight data for the additional UEFA leagues using RSSSF for both the result and the date. This replaces the experimental method that took results from Wikipedia and dates from RSSSF.

The historical rebuild ends at **2024/25**. The planned checkpoint is the end of **2025/26**. From that point onward, the intended process is Wikipedia results plus the user's pre-made fixture-date files, because RSSSF may update slowly. Historical rows only need to be made accurate once and then frozen in the production master CSV.

The user has made a separate backup copy of the complete J-Ratings folder. The obsolete `worldwide_trial` directory was deleted to recover disk space.

## Repository and main files

Repository root:

`C:/Users/stjuk/Documents/GitHub/J-Ratings`

Current RSSSF-only rebuild script:

`C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/rsssf_uefa_rebuild.R`

Shared RSSSF parsing functions loaded by that script:

`C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/wikipedia_rsssf_four_leagues.R`

Cached bulk RSSSF pages, checked first:

`C:/Users/stjuk/Documents/GitHub/J-Ratings/EuropeanFootball/pipeline_data/Source/rsssf/all/pages`

The script also checks the older manual-source caches when a page is absent from the bulk cache. It performs **no web downloads**.

RSSSF-only audit output directory:

`C:/Users/stjuk/Documents/GitHub/J-Ratings/EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_UEFA_Rebuild`

Production match master, which this audit script deliberately does not change:

`C:/Users/stjuk/Documents/GitHub/J-Ratings/EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv`

Downstream scripts:

- Elo calculation: `C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/02_calculate_elo.R`
- Website JSON: `C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/03_write_json.R`

Former Wiki-plus-RSSSF audit and candidate data remain here for reference:

`C:/Users/stjuk/Documents/GitHub/J-Ratings/EuropeanFootball/pipeline_data/Manual_Sources/Wikipedia_RSSSF_Alias_Audit`

The old importer is:

`C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/import_remaining_uefa_leagues.R`

Do not use that importer for the new RSSSF-only output without reviewing and adapting it. The data source and matching assumptions have changed.

## How to run the current audit

For a complete run in R:

```r
Sys.unsetenv(c(
  "RSSSF_REBUILD_COUNTRIES",
  "RSSSF_REBUILD_MIN_YEAR",
  "RSSSF_REBUILD_MAX_YEAR"
))

source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/rsssf_uefa_rebuild.R")
```

The script uses `tictoc` and sounds `beepr::beep()` at completion. It reads local files only, so there is no `Sys.sleep()` and no risk of burdening RSSSF or Wikipedia.

For a small test:

```r
Sys.setenv(
  RSSSF_REBUILD_COUNTRIES = "Hungary,Cyprus",
  RSSSF_REBUILD_MIN_YEAR = "2015",
  RSSSF_REBUILD_MAX_YEAR = "2024"
)

source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/rsssf_uefa_rebuild.R")
```

Unset those variables before a full run. A filtered run overwrites the combined audit outputs with the filtered set.

## Important design decisions

- RSSSF supplies the home team, away team, score and date. Wikipedia is not read by this script.
- The old `WikipediaGames` value is retained only as an independent reference count and is called `ExpectedGames` in the new report. A percentage over 100 does not mean Wikipedia was parsed during the run.
- Every requested season is reparsed on every run. There are currently no 95% locks and no resume mechanism. The user explicitly asked not to lock seasons yet because this is a new extraction method.
- Exact repeated fixtures are retained. `SourceFixtureId` includes the source line so two matches with the same teams and score are not silently collapsed.
- The parser isolates the first RSSSF `<h4>` competition block. This prevents it from mixing the top flight with lower divisions, cups, regional leagues, or a later duplicate “Details” section.
- A separately named `prorel` block is excluded because it is a cross-tier promotion/relegation playoff. Internal championship and relegation groups belonging to the top flight remain included.
- The extended 2019/20 season has a special general date rule that uses numbered-round halves to disambiguate bare month/day headings after the COVID interruption.

The first structural test gave these representative improvements:

- Georgia 2024: 1,366 dated rows before isolation, 182 after it, against a 180-game reference.
- Lithuania 2022: 424 before, 182 after, against a 181-game reference.
- Russia 2022/23: duplicated historical output was reduced to 242 dated rows, against a 240-game reference.
- Andorra, Romania and Sweden also produced plausible totals in the targeted test.

Small totals just above 100% can be legitimate reference-count differences or top-flight playoffs. The current safety threshold marks a season as a suspicious overcount only when extracted RSSSF results exceed `ExpectedGames` by more than 5%.

## Audit outputs and how to read them

The main files in `RSSSF_UEFA_Rebuild` are:

- `season_audit.csv`: one row per country-season, with detailed counts, status, source path, isolation method and top-flight heading.
- `coverage_summary.csv`: country totals and counts of seasons in each quality group.
- `coverage_summary_2010_onwards.csv`: the same country summary restricted to the priority period from 2010 onward.
- `all_rsssf_games.csv`: every extracted result, including rows whose dates are unresolved.
- `all_dated_rsssf_games.csv`: every extracted row with a date, including suspicious seasons.
- `dated_candidate.csv`: dated rows excluding seasons quarantined for obvious overcounts or season errors.
- `quarantined_overcounts.csv`: dated rows from suspicious seasons, kept for inspection rather than discarded.
- `unresolved.csv`: extracted results whose date could not be resolved.
- `source_manifest.csv`: the country-seasons and RSSSF pages selected for the run.
- Per-season directories such as `Hungary_2019-20/rsssf_games.csv` and `unresolved.csv`.

Key `season_audit.csv` fields:

- `RSSSFResults`: result rows extracted from the isolated RSSSF top-flight block.
- `RSSSFDatedResults`: extracted result rows with a resolved date.
- `CoveragePercent`: `RSSSFDatedResults / ExpectedGames`. This compares RSSSF extraction with the old independent season count.
- `DatedShareOfRSSSFPercent`: `RSSSFDatedResults / RSSSFResults`. This directly measures how completely the parser assigned dates to the RSSSF results it found.
- `CountDifference`: extracted results minus expected results.
- `QualityStatus`: `at_least_95`, `below_95`, `suspicious_overcount`, `no_reference_count`, or `season_error`.
- `TopFlightHeading`: the RSSSF heading selected as the top-flight block. Review this when a total is implausible.

`CoveragePercent` and `DatedShareOfRSSSFPercent` answer different questions. A high dated share with low reference coverage usually means the selected block was parsed cleanly but the page or block did not contain all expected top-flight games. A low dated share means the parser found score lines but failed to associate dates. A value far above 100% indicates unwanted competitions or duplicate sections were probably included.

In the country summaries, `CoveragePercent` uses only seasons that actually have an `ExpectedGames` reference. This prevents countries with many unreferenced seasons from displaying misleading totals above 100%. `GoodShareOfSafeSeasonsPercent` excludes season errors and quarantined overcounts from its denominator.

## Immediate next steps

1. Run the complete RSSSF-only audit and inspect `season_audit.csv` and `coverage_summary.csv`.
2. Review `suspicious_overcount` seasons first. Check `TopFlightHeading` and the cached HTML structure to see whether the first `<h4>` rule selected too much or the wrong competition.
3. Review `below_95` seasons by separating extraction failures from genuinely incomplete RSSSF pages. `DatedShareOfRSSSFPercent` helps distinguish these.
4. Improve general parser rules from a varied sample. Avoid one-off club-name fixes unless a reusable rule cannot solve the layout.
5. Once the RSSSF-only historical output is satisfactory, create or adapt an importer that replaces the relevant countries in the production master with the reviewed dated rows. Back up the master immediately before import.
6. Check team identity continuity across RSSSF seasons. RSSSF-only removes the cross-source Wikipedia/RSSSF alias problem, but a club may still change spelling across RSSSF pages and accidentally become two Elo identities.
7. Only after import, run `02_calculate_elo.R`, then `03_write_json.R`, and inspect the website.

Do not infer parser quality from a country-wide aggregate alone. Review yearly results, especially from 2010 onward. Older years can tolerate more gaps, but obvious parsing errors should still be corrected when a general fix is available.
