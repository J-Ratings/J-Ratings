# Explicit football shootout decisions

`export_decisions.py` is a metadata-only post-export step called by both football
`03_write_json.R` scripts. It can also be run independently:

```powershell
python scripts/football/export_decisions.py all
```

It enriches existing `data/games` and `data/tournaments` JSON with `decision`,
`winner`, and (when recorded) `homePenalties` / `awayPenalties`. It does not change
`score`, `result`, Elo, rating histories, or either Club Football checkpoint.
Only drawn, completed matches are eligible. Conflicting evidence is skipped.
Date and both teams must match; home/away orientation is handled explicitly.

Evidence comes from retained explicit `(p H-A)` score suffixes, verified structure
decisions, or a supplementary `pipeline_data/source/shootouts.csv` containing
`date,home_team,away_team,winner`. No winner is inferred from later-round entrants.
Without evidence, a tied score remains a draw and a final remains unresolved.

The Club master CSV is also checked for retained shootouts, including historical
cup fixtures not yet represented in the published Elo exports. A compact
`data/shootout-decisions.json` preserves the decision index for either sport.
Unchanged JSON files are cached by modification time and size so subsequent
metadata passes do not repeatedly parse the full historical profile archive.

International Football's downloader refreshes the small supplementary file from
[Mart Jürisoo's international results dataset](https://github.com/martj42/international_results/blob/master/shootouts.csv).
This file supplies shootout winners, not necessarily shootout scores; scores must
never be invented. The normal match-results source is unchanged. A failed refresh
retains the previously validated local file.

The separate, verified 2022 final decision also contains the 4–2 shootout score,
supported by [FIFA](https://www.fifa.com/en/tournaments/mens/worldcup/articles/argentina-france-2022-final-greatest-games).

`scripts/football/elo_decisions.R` applies explicit shootout decisions before either Elo calculation: a winner receives 1 and the loser 0. Draws without evidence remain 0.5/0.5. The actual score is preserved. Club checkpoints are tagged `shootout-wins-v1`; older draw-based checkpoints are rebuilt rather than reused.

For the one-off historical migration, run `source("scripts/football/rebuild_shootout_ratings.R")` from the repository root. This backs up the existing Club checkpoints, forces full Club history, exports Club JSON, then rebuilds and exports International Football. No downloads or Git actions are performed. This is a long user-run job.

`core/football-decisions.js` is the shared display rule for both sports. Profiles
show “Win (pens)” / “Loss (pens)” and colour the team's drawn score accordingly.
Tournament progression, final standings W/D/L totals, profiles and rebuilt Elo now use the explicit shootout winner. Ratings and aggregate profiles remain on the previous policy until the migration finishes.
