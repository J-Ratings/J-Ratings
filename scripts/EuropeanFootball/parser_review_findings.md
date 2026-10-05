# Offline parser review

## Additional sparse-table validation: 21 September 2026

Both fixture layouts now reject club fields containing standalone matrix dashes,
embedded score cells or tab-separated columns. Attached hyphens in club names
remain valid. This closes the remaining Cyprus 2019/20 false-identity problem:
the cached check now matches 120/138 (86.96%), versus 51/138 in the full audit.
Sixteen dates, one repeated fixture and one score disagreement remain unresolved.
The 14-season cached regression suite passed again in 15 seconds. No full audit,
downloads, production changes or manual alias approvals were performed.

## Follow-up: five failure samples

Inspected Romania 1991/92, Slovakia 2007/08, Cyprus 2019/20, Iceland 2024,
and Wales 2024/25. General fixes exclude multi-score summaries and matrix
placeholders from the trailing-score fixture format, recognize Cup 1/16 finals
and Slovak pohar headings, retain dates across malformed brace scorer notes,
and avoid treating Pen-y-Bont as a penalty annotation. Championship conference
headings now share the championship phase classification.

The 14-season offline regression suite passed in 15 seconds. These five samples
changed respectively from 42 to 306 of 306, 83 to 196 of 198, 51 to 82 of 138,
160 to 162 of 162, and 171 to 189 of 195 dated games. Cyprus still needs review;
its gain does not establish season completeness. No manual aliases were added.
Full audit and production data were not rebuilt.

Ten cached seasons were checked after the fixture-layout, repeated-result,
Wikipedia identity, and competition-boundary corrections on 20 September 2026.

| Season | Dated Wikipedia games | Coverage |
|---|---:|---:|
| Iceland 2024 | 160/162 | 98.77% |
| Belarus 2024 | 240/241 | 99.59% |
| Cyprus 2007/08 | 213/218 | 97.71% |
| Russia 2024/25 | 240/240 | 100% |
| Albania 2024/25 | 180/180 | 100% |
| Croatia 2024/25 | 179/180 | 99.44% |
| Croatia 2011/12 | 231/240 | 96.25% |
| Finland 2024 | 165/165 | 100% |
| Finland 2011 | 0/198 | 0% |
| Lithuania 2024 | 179/181 | 98.90% |

These are extracted-game matching rates, not independent completeness checks.
No production data or full-season audit was overwritten by the diagnostic.

## Changes

- A year/season prefix no longer hides a cup section boundary.
- Results-matrix rows containing several scores are excluded from fixture parsing.
- Multiple RSSSF sections can resolve spellings independently against Wikipedia
  results. Conflicting identities for the same raw name remain unresolved.
- Explicit attendance metadata after a round date is stripped before date parsing.
- Identical repeated home/away/score records are assigned the complete set of
  distinct RSSSF dates only when the counts agree exactly within the phase and
  no matching RSSSF row lacks a date. Duplicate RSSSF evidence is deduplicated.
  This establishes date/result tuples, not an association between an individual
  Wikipedia cell's stage label and its assigned date. The method is explicitly
  recorded as `complete_identical_fixture_date_set`.
- The alternative `Home - Away 2:1 (24. VII. 2011.)` format is supported,
  including Roman-numeral months and round dates overridden for one match.
- Ordinal position suffixes such as `(5th)` are removed from team identities.
- Different Wikipedia labels pointing to one club article are consolidated.
  Labels with conflicting article targets, or shown playing one another, are
  excluded. Original Wikipedia labels are retained in separate evidence fields.
- Lithuanian cup/lower-division headings and additional competition boundaries
  are recognized. Prose mentioning another division does not end the league.
- Alias reports include unmatched Wikipedia game counts and RSSSF appearance
  counts. No-fixture pages are flagged separately from missing team identities.
- A blank Approved field in the alias template is no longer treated as approval.

## Source limitation

The cached Finland 2011 page contains a league cross-table without league
fixture dates. Dates elsewhere on that page belong to the cup. Zero league
dates extracted from this page cannot be diagnosed as an alias failure.
The earlier claim that all 2010-12 zero-result seasons were parser failures was
too broad. Croatia 2011/12, by contrast, contained a recoverable fixture format.

Additional unmatched games are saved in
`EuropeanFootball/pipeline_data/Manual_Sources/Wikipedia_RSSSF_Alias_Audit/parser_checks/unresolved_examples.csv`.
The diagnostic also saves season-specific team maps and comparison.csv.

## Rebuild

Use `ALIAS_AUDIT_RESUME=0` and `ALIAS_AUDIT_CACHE_ONLY=1` for the next full
audit so retained earlier results do not mask these fixes. Network access is
disabled in that mode. The audit creates a backup of its previous season audit.
Missing cached pages remain reviewable errors; they are not proof of absent
source data. New approved aliases should trigger another recomputation.
