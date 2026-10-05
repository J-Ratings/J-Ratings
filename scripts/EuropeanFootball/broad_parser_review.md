# Cached parser review — 22 September 2026

Scope: 30 seasons starting in 2010 or later, selected for substantial unresolved
non-identity counts, with at most two seasons per country. Inspect cached source
excerpts and reparse the selected seasons only. No downloads, production imports,
alias edits or lock changes. The sample reuses the saved Wikipedia fixtures;
the regression checks also re-extract Wikipedia tables.

## General fixes

- Accept a complete date heading with its opening bracket missing.
- Read the actual date in `Nov 8; postponed from Sep 9,10`; do not accept
  `postponed to` as the original playing date.
- Accept a completed match explicitly marked only `replay`. Awarded, abandoned,
  annulled and mixed administrative notes remain unresolved.
- Preserve championship/relegation information in named anchors inside PRE
  blocks, including translated visible headings. Use phase evidence for unique
  Wikipedia fixtures as well as repeated ones when that phase exists in RSSSF.
- Use explicit Wikipedia match/round ranges only after at least five unique
  dated fixtures validate the correspondence, with no contradictions anywhere
  in that season. Northern Ireland demonstrated why unconditional round-range
  matching is unsafe: the two sources can number the schedule differently.
- Recognise additional cup/lower-division headings, including Kauss, Karikas,
  Pohar, Kuboku and Esiliiga. A cup fixture must not supply a league date.
- Remove Wikipedia superscript footnotes before reading scores: `0-3` with
  footnote `1` must not become `0-31`.

## Findings in the selected sample

| Country / seasons | Source evidence / remaining limitation |
|---|---|
| Romania 2010/11 | League round list is incomplete; a final table does not supply dates for its missing fixtures. |
| Kosovo 2011/12, 2012/13 | Many round dates are ranges such as Aug 25,26. Explicit validated round ranges recover some repeated fixtures; exact days remain unknown for others. |
| Wales 2010/11, 2011/12 | Incomplete earlier round listing, changing club labels within the source, and malformed `Nov 25]` heading. The heading is fixed; identity variants are deferred. |
| Latvia 2011, 2025 | League result grids without a corresponding dated league schedule. Dates below include cup/playoff material; cup contamination is now excluded. |
| Slovakia 2010/11, 2011/12 | Undated league grids and dated cup material further down. Translated cup boundary now recognised. |
| Hungary 2019/20 | Ambiguous summer years across the COVID extension, repeated fixtures, and inconsistent round-range correspondence. No blanket year or round inference. |
| Russia 2015/16, 2022/23 | Remaining examples include club-label differences and a direct score disagreement (Ufa–Spartak: Wikipedia 3-0, source 3-1). No score substitution. |
| Lithuania 2010, 2019 | Annulled/awarded results, including matches explicitly not played. These remain withheld. |
| Estonia 2025 | Undated league material followed by dated cup material. Cup heading now recognised. |
| Andorra 2022/23 | Repeated same-score fixtures, source/Wikipedia orientation discrepancies and awarded scores. Do not pick an arbitrary date. |
| Andorra 2014/15 | Translated phase headings concealed regular/playoff distinctions; named anchors provide explicit phase evidence. |
| Moldova 2013/14 | Repeated score where one occurrence was played and another awarded. Cycle labels alone do not prove the occurrence date. |
| San Marino 2010/11, 2011/12 | Undated regular-season grids alongside dated playoffs/cup matches. A playoff between the same clubs is not the missing regular-season fixture. |
| Armenia 2015/16, 2020/21 | Withdrawals, awarded results and repeated scores across portions of the season; no administrative date invention. |
| Cyprus 2015/16, 2019/20 | Group A/B headings do not explicitly identify championship/relegation; repeated fixtures and some year ambiguity remain. No universal A/B assumption. |
| Azerbaijan 2010/11, 2021/22 | Undated league grids in the earlier page; later examples include changed team labels. Identity work deferred. |
| Albania 2013/14, 2018/19 | Awarded matches and superscript footnotes concatenated into score digits. Footnote extraction fixed; awards remain withheld. |
| North Macedonia 2024/25 | Examples include awarded 3-0 results for clubs that did not show up. Not evidence of a missed played match. |
| Northern Ireland 2022/23 | Rescheduling text and a completed replay blocked real dates. Both formats fixed. |

## Measured result

On the 30 saved Wikipedia fixture sets: 30 additional dated games, one old
cup-derived assignment removed, and zero changed dates among retained matches.
Northern Ireland 2022/23 improves from 94.83% to 98.28%; Andorra 2014/15 from
87.50% to 95%; Wales 2011/12 from 65.62% to 68.75%.

This is a modest recovery, not evidence of a large full-audit improvement.
Separately, re-extracting Albania 2018/19 fixes footnote-corrupted scores and
exposes a played/awarded same-score ambiguity (Kastrioti–Kamza); dated coverage
there falls by one rather than arbitrarily choosing which occurrence to date.
The audit's non-identity bucket is not equivalent to recoverable parser errors:
it includes absent dates, administrative outcomes, score disagreements and
partially mapped source naming variants. These findings do not prove that all
non-alias parser problems have been solved.

Evidence and per-season comparisons are saved under
`EuropeanFootball/pipeline_data/Manual_Sources/Wikipedia_RSSSF_Alias_Audit/broad_parser_review/`.
The reproducible offline sample is `review_cached_parser_sample.R`.
