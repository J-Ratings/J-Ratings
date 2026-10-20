# Supplementary tournament updates

## Recover legacy pages from the historical cache

```r
source("scripts/go/salvage_tournament_history.R")
```

This forces offline mode and requeues saved editions previously reported as empty,
unsupported or reduced in result count. It adapts BR-separated legacy knockout,
series and league cells in memory, preserving SGF links and source spans. It never
downloads or guesses a year for a month/day-only date. Source contradictions remain
review items. Previous larger exports remain protected by the count-drop guard.
Rerun the same command to resume; the salvage plan is created once. Existing batch
reports are replaced as requeued batches finish. The original checkpoint is kept
as `progress-before-legacy-salvage.rds`. The game-link index is rebuilt once at the
end of a historical run instead of once per batch.

Run `source("scripts/go/test_legacy_tournaments.R")` for short offline series and
saved-page checks. The synthetic series fixture checks explicit dates, opponents,
winners and SGF links; saved regressions cover LG, Ing, Meijin, Kisei, Samsung and
MLILY where those HTML files are locally available.

## One-off historical import (local only)

Run from the repository root after the source owner has approved historical
downloading and reuse:

```r
Sys.setenv(GO_K2SS_PERMISSION_GRANTED = "true")
source("scripts/go/import_tournament_history.R")
```

Do not set that flag while permission is pending. Without it the script exits
before any requests or writes. This may take hours; run it yourself in RStudio.
Interrupting and running the same command resumes from cached pages/checkpoints.
The importer follows advertised timetable archive years/months, then same-family
edition navigation. It does not assume March 2022 is the beginning. A full three
seconds elapses after each response before the next request. There are no network
retries; 404 URLs are recorded, individual HTTP 5xx failures are deferred to a
later run, and three consecutive server errors stop the run. Access/network
failures still stop immediately. Deferred pages are listed in
`deferred-server-errors.json` and retried only when you start another run.

The local catalogue under `Go/data/tournaments/catalogue` is expanded in batches.
The script does not push, enable workflows, import rating games or recalculate Elo.
Reports/checkpoints are in `Go/pipeline_data/tournament_history`; inspect
`summary.json`, `missing-cache.csv` and all `batch-*-report.json` review entries.
Processed editions include unsupported layouts; a finished run does not guarantee
all tournaments rendered successfully. Verify the discovered first/last calendar
against the source timetable before claiming full coverage.

If archive navigation is only rendered by JavaScript, the importer may need a
verified explicit range rather than HTML links. Set both `GO_HISTORY_START_MONTH`
and `GO_HISTORY_END_MONTH` to `YYYY-MM` values matching the source archive. No
unverified start year is supplied by default.

`GO_HISTORY_OFFLINE=true` uses saved pages only. For a safe short check, run
`source("scripts/go/test_tournament_history.R")`; it uses synthetic archive
fixtures in a temporary folder and makes no requests. To rebuild previously
processed editions after parser changes, remove only the `built` vector from
`progress.rds`; retain the cache and other progress fields.

The broad GoRatings pipeline imports results and calculates Elo weekly. The K2SS
structure updater only updates tournament displays and links; it never calculates
Elo. A separate targeted daily GoRatings runner below imports confirmed results
through the existing canonical game merge and Elo stages. Workflows remain paused.

## Targeted daily GoRatings results

From the repository root, preview the selection without network requests or writes:

```r
Sys.unsetenv("GO_DAILY_APPLY")
source("scripts/go/run_daily_tournament_results.R")
```

To explicitly run the selected profile requests and the existing rating pipeline:

```r
Sys.setenv(GO_DAILY_APPLY = "true")
source("scripts/go/run_daily_tournament_results.R")
Sys.unsetenv("GO_DAILY_APPLY")
```

Allow several minutes for a busy day and the downstream Elo/export stages. The
runner does not fetch K2SS, enable Actions, commit or push. Run the separate
permission-gated structure updater when new source structure is available.

Daily selection uses cached weekly homepage IDs and local identity aliases. It
checks only unresolved explicitly dated games within the preceding seven days.
It never expands a calendar entry to the entire entrant field or treats an undated
slot as a game played today. Undated tournaments are reported for structure review.
Future dates outside that window and unrelated recently active players do not
trigger requests. Each player is selected at most once per run. The default cap
is 80 profiles (`GO_DAILY_MAX_PROFILES`); deferred IDs and unmapped names are
reported, not silently treated as checked. `GO_DAILY_DATE=YYYY-MM-DD` supports
offline selection checks for another date. No selection can find a tournament
absent from our saved dated draw; the weekly sweep catches that.

To check only a particular edition, set `GO_DAILY_TOURNAMENT_LINKS` to its source
path, for example `/news/wr/quzhou-lanke/4/`. Comma-separated paths are supported.
Unset it afterward to return to checking all recent dated pending matches.

`GORATINGS_SCRAPE_MODE=daily` adds this selection to stage 04. It does not request
or overwrite homepage metadata, checkpoints observations after every profile,
waits three seconds after each response, and stops on request/parser failures
without retries. Weekly/full selection remains available. Stages 05–09 and 02
then retain their existing identity reconciliation, duplicate handling and Elo
calculation. Reading both opponents or later doing the weekly sweep must not add
the same game twice; this stays the responsibility of the existing stage 05 merge.

After export, `reconcile_tournament_results.R` fills only unique exact-date,
exact-opponent results into pending saved bracket slots, propagates known feeder
winners/losers, updates series scores, and refreshes both catalogue and original
Lanke preview snapshots. A completed source result is preserved. Same-day multiple
games remain unresolved, and month/day-only dates are never assigned a guessed
year. GoRatings profiles provide winner/colour but not necessarily the winning
margin; the overlay records only `B` or `W`, marks the missing detail and retains
GoRatings provenance. It never fabricates a resignation. Current review reports
are saved under `Go/pipeline_data/tournament_updates/daily-results/`.

Short offline checks: `source("scripts/go/test_daily_tournament_results.R")`.

## Permission gate

Live requests fail closed unless `GO_K2SS_PERMISSION_GRANTED=true`. The Actions
workflow also checks the repository variable of the same name before checkout,
package setup or any source requests. Leave it unset while permission is pending.
Enable it only after the owner has agreed to the actual frequency/reuse, and
adjust this implementation to any conditions in that reply. Schedules only run
after the workflow is merged into the repository's default branch.

## Daily selection and request behaviour

- Read this month's calendar; read an adjacent month only when the seven-day
  lookback/lookahead crosses the month boundary.
- Refresh relevant editions and recently scheduled ongoing draws. Carry deferred
  pages into the next run. At most 20 tournament pages per run by default.
- Wait at least three seconds **after a response finishes** before another request.
  Fetch sequentially. No network retry loop; stop on HTTP/access/server failures.
- Validate responses before replacing cached pages. Never fetch player profiles
  from K2SS, nor crawl every historical edition to initialise the daily updater.
- Use local identities and histories conservatively; unique exact matches first,
  then unique matches within seven days with the same winner and stone colours.
  Retain the source date. Source results can display before weekly histories catch
  up, with ratings absent until a match is available.
- Export review reports for unknown layouts/identities, new editions, missing
  source pages and falling result counts. Preserve the previous edition if a new
  parse contains fewer supported games.

The source cache, calendar and ongoing state persist through the Actions cache.
The published catalogue is versioned under `Go/data/tournaments/catalogue` once
publication is authorised. Existing locally generated snapshots are not needed
to initialise a current-month run, and should remain local until reuse is agreed.

## Local checks

Run `source("scripts/go/test_tournament_updates.R")` for short offline checks.
Running `source("scripts/go/update_tournaments.R")` with the permission flag unset
performs no network requests or writes.

For saved-page testing, use `GO_TOURNAMENT_OFFLINE=true`, optionally
`GO_TOURNAMENT_DATE=2026-10-13` and
`GO_TOURNAMENT_LINKS=/news/wr/quzhou-lanke/4/`. This writes tournament output, so
use `GO_TOURNAMENT_OUTPUT` and `GO_TOURNAMENT_STATE_DIR` overrides for an isolated
preview. `GO_TOURNAMENT_CALENDAR` can similarly isolate the calendar CSV.

Keep the source attribution on tournament pages. Review the report rather than
silently adding aliases or inventing connections from unresolved source data.
