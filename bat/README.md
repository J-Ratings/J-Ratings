# Local update launchers

## Git lock recovery

If GitHub Desktop reports an existing lock, fully close Codex first, then run
`Repair_Git_Lock.bat`. It does not invoke Git. It refuses recovery while any
Git process is running, while the lock is recent, or when exclusive access fails.
It preserves a stale lock as `.git/index.lock.stale-TIMESTAMP`, rather than
deleting its contents. Retry the commit in GitHub Desktop afterward. This is a
recovery tool, not a fix for the application leaving the lock behind.

## Data updates

Double-click a `.bat` file. The launcher finds the repository automatically,
finds Rscript, runs stages in order and keeps the window open afterward.
Logs go in `bat/logs/`. A failed stage stops that run; do not assume its exports
are current. After fixing the error, rerun the launcher.

These launchers do not commit, push, deploy, build the public site, or enable
scheduled jobs. Run one update at a time, including updates started in RStudio.
The launchers prevent simultaneous runs of other launchers in this folder.

| Launcher | Purpose |
| --- | --- |
| Go_Quick_Update.bat | Daily targeted GoRatings checks for recent dated pending matches in saved tournament draws; updates Elo, JSON and reconciles known draws. Can miss new tournaments absent from the saved draw. |
| Go_Weekly_Update.bat | Normal broader GoRatings update: new and recently active profiles, followed by ratings and JSON. |
| Go_Full_Update.bat | Deep refresh of every selected/listed GoRatings profile; requires typing FULL. Much slower; not a full GoGoD historical rebuild. |
| Club_Football_Update.bat | Refresh configured OpenFootball sources, parse, calculate ratings and export. Historical source caches/checkpoints are reused when valid. Does not import RSSSF review candidates. |
| International_Football_Update.bat | Download Kaggle results and shootout evidence, combine, calculate and export. |
| Snooker_Update.bat | Refresh recent lookups and matches, calculate and export. Preserves the pipeline's request-delay settings. |
| Rugby_Update.bat | API merge using configured years (2025 onwards by default), calculate and export. |
| *_Recalculate_Only.bat | Recalculate/export from saved inputs without running source downloads. Go also rebuilds its local game merge and identity map. Does not force a historical checkpoint reset. |

## Setup

R and the packages used by the existing pipelines must already be installed.
Python and the Kaggle CLI must also be available for the football stages that
use them. If automatic R discovery fails, set `J_RATINGS_RSCRIPT` to the full
path of your preferred `Rscript.exe`.

Launchers use fresh R processes, so credentials set only with `Sys.setenv()`
inside an existing RStudio session are not inherited. Use your existing user
`.Renviron` or Windows environment settings:

- International Football: Kaggle credentials (`KAGGLE_API_TOKEN` or existing CLI setup).
- Snooker: `SNOOKER_API_HEADER`.
- Rugby: `THESPORTSDB_API_KEY` (the existing pipeline falls back to its public key if absent).

Keep secrets out of the BAT files and repository. Existing request-delay settings
are retained. The normal Snooker pipeline defaults to a 70-second request pause.

No launcher downloads K2SS tournament structures or resets historical Elo
checkpoints. Those are separate maintenance operations, rather than routine
update alternatives.
