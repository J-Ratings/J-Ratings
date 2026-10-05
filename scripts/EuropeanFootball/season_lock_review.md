# Coverage locks and non-alias review, 21 September 2026

Normal audits now preserve seasons with raw DatedGames/WikipediaGames > 0.95,
no Error or ExtractionReview flag, consistent saved counts, nonmissing dates
within the season years, and no repeated home/away/date fixtures or self-matches.
Exactly 95% remains eligible for parsing. This does not certify that Wikipedia
has every season fixture. Existing extraction warnings prevent automatic locking.

The next audit creates immutable copies of the saved games, RSSSF evidence,
team map and audit row in locked_seasons/. Checksums detect changed snapshots.
locked_seasons.csv is the manifest. RESUME=0 recomputes only unlocked seasons;
USE_LOCKS=0 explicitly bypasses snapshots for comparison without deleting them.
The production master is not changed. Keep snapshots when changing aliases;
their original evidence remains available for a later reviewed revision.

## Sample findings

- Albania 2018/19: an `awd` record cleared the date for ordinary matches below
  it. Preserving that heading increases dated matches from 120/180 (66.67%) to
  167/180 (92.78%). Awarded outcomes themselves are not converted to played games.
- Moldova 2013/14: the same fault; 153/203 (75.37%) becomes 182/203 (89.66%).
- Kosovo 2012/13: many headings give two days, e.g. `Round 2 [Aug 25,26]`.
  They do not establish individual match dates. Remains 58/198.
- Romania 2010/11: the cached league fixture listing ends after Round 18;
  Round 19 is unplayed placeholders followed by the final standings. Remains
  160/306. A full final table is not a full dated fixture list.
- Wales 2010/11: the cached page contains only part of the dated schedule;
  later championship/relegation sections are standings. Remains 66/192.
- Wales 2011/12 (source and saved map inspection): Bangor/Neath FC/Newtown FC
  remain unresolved even though Bangor City/Neath/Newtown occur and resolve.
  A zero count of completely absent Wikipedia identities therefore does not
  mean all fixture-name variants resolved. Alias logic was not changed.
- Hungary 2019/20: undated July–September fixtures involve ambiguous COVID
  season years and conflicting neighbouring anchors, including a postponed May
  match listed under Round 1. No guessed year was introduced; remains 158/198.

Synthetic checks cover awarded/postponed records followed by normal fixtures,
ambiguous two-day headings, lock threshold, immutable copies and corruption
detection. The existing 23 cached-season regression cases were also run;
no matched-game counts regressed. Full audit and production import are left
for the user. The audit retains its elapsed timer and completion beep.

```r
Sys.unsetenv(c("ALIAS_AUDIT_COUNTRIES", "ALIAS_AUDIT_MIN_YEAR", "ALIAS_AUDIT_MAX_YEAR"))
Sys.setenv(ALIAS_AUDIT_RESUME="0", ALIAS_AUDIT_USE_LOCKS="1",
           ALIAS_AUDIT_CACHE_ONLY="1")
source("C:/Users/stjuk/Documents/GitHub/J-Ratings/scripts/EuropeanFootball/wikipedia_rsssf_alias_audit.R")
```

This run uses local pages only and preserves protected seasons in the combined
audit/candidate files. It cannot download or sleep between web requests.

## Date parser review, 22 September 2026

The date-heading recogniser now requires calendar month names or numeric date
syntax. Previously `[Ivanovski 65]`, `[Glisic 7]` and similar scorer notes
erased the actual date. Multi-day round windows still remain unresolved.
Venue and attendance notes no longer invalidate otherwise dated matches;
awards, annulments, postponements and score-changing notes remain unresolved.
Explicit two-digit years are accepted only when unique within the season.
Numeric match dates followed by venue prose are not inherited by the next
fixture. Standings rows containing numeric club names remain excluded.

Latest saved audit versus local regression checks:

| Season | Before | After |
|---|---:|---:|
| North Macedonia 2010/11 | 161/198 | 193/198 |
| North Macedonia 2011/12 | 160/198 | 194/198 |
| North Macedonia 2012/13 | 161/198 | 196/198 |
| Slovakia 2018/19 | 151/192 | 181/192 |
| Slovakia 2019/20 | 132/162 | 155/162 |

All 29 cached-season checks passed without reduced matched counts. This is a
sample, not a projected full-audit improvement. Source annotations and identity
rules were not loosened to accept awarded/annulled outcomes or unapproved aliases.

`inspect_rsssf_source_gaps.R` checks all available cached copies for the 26
2010+ seasons previously returning zero fixtures (5,447 Wikipedia games).
None yielded dated league fixture rows. Manual inspections include Poland,
Norway, Hungary, Iceland, Serbia, Bulgaria, Georgia, Israel, Romania, Cyprus,
and the Faroe Islands: the league sections contain standings/undated grids;
cup dates do not establish league dates. A parser-only solution cannot be
assumed for this block. The saved report is `source_gap_inspection.csv`.

The full rerun also writes `SourceAssessment` in the season audit and
`source_layout_review.csv`, distinguishing undated grids, no detected fixture
rows/date headings, and date headings with an unresolved fixture layout. These
are diagnostics, not additional lock criteria or independent completeness proof.
Keep the same cache-only rerun command above; protected seasons remain intact.
