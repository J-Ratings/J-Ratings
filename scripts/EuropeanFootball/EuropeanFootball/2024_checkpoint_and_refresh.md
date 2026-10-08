# Club football checkpoint and source refresh

The original 02 implementation is preserved as `02_calculate_elo_before_2024_checkpoint.R`.

Run normal 02 once to create `EuropeanFootball/pipeline_data/Elo/checkpoint_2024_12_31.rds`.
That first run is a full two-pass calculation. The checkpoint contains unrounded
ratings, counts, entry metadata, both historical match histories, and the frozen
retro entry ratings for clubs existing at the cutoff. Later runs calculate matches
dated 2025-01-01 onward and append their results to the stored historical histories.
03 now also creates `json_export_checkpoint_2024_12_31.rds` after a successful export.
It reuses historical country ranking snapshots and skips existing history/game files
for clubs with no post-cutoff results or fixtures. Clubs with recent activity still
receive complete files, preserving the existing website format. Current summaries
and season manifests are regenerated. The full history CSV is still read; therefore
this is not a constant-time export. Its first run remains a full export.
The original 03 is preserved as `03_write_json_before_2024_checkpoint.R`.
Missing files are rebuilt, and changed Elo checkpoints or club IDs invalidate the
export cache. Country assignment changes rebuild country ranking snapshots.

Pre-2025 input changes produce a warning and do not change the saved history.
Do not delete or overwrite the checkpoint in normal use. Back it up and include it
when transferring the pipeline to GitHub Actions. To intentionally rebuild all
history, use the archived script separately; replacing the checkpoint is a deliberate
maintenance operation, not an automatic update.

The club workflow runs weekly. The separately named world-football workflow is
for international football, not club leagues.

The monthly RSSSF workflow is review-only. It refreshes the current linked page and
recent year variants for modelled top flights without a working OpenFootball route.
Requests are sequential, 1.5 seconds apart; access restrictions stop the run.
Years at or before the frozen cutoff are not targeted separately. In 2026, the
rolling three-year window is clipped to 2025-2026, plus a linked 2026/27 page if present.
This avoids spending requests on frozen history. From 2027 it covers three recent years.
Lower divisions and continental competitions are not included in this first refresher.

Reports and post-cutoff candidate matches are saved as a GitHub Actions artifact.
No matches are automatically imported, and no ratings are changed by that job.
Review and validate candidates before importing them and running 02/03. Download or
parser failure is not proof that a league lacks results. The excluded-country list
is explicit in the script and can be overridden with RSSSF_REFRESH_EXCLUDE_COUNTRIES.

The source-refresh runtime has not yet been measured. Expect several minutes for
downloads, plus parsing time depending on page size; the workflow allows 90 minutes.
The checkpoint changes were syntax-checked and tested using a small isolated
two-pass continuation example. A full production run is intentionally left to the user.
