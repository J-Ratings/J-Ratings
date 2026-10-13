# Supplementary tournament updates

The existing GoRatings pipeline imports results and calculates Elo weekly. The
separate daily workflow only updates tournament displays and game-to-event links.
Neither the daily script nor its workflow calls the production import/Elo stages.

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
