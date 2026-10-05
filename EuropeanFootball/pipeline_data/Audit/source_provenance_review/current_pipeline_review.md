# Club Football pipeline review — 5 October 2026

## Verified current behavior

The weekly workflow runs 00_download_openfootball_current.R, 01_parse_openfootball.R, 02_calculate_elo.R, and 03_write_json.R. These names conceal a multi-source pipeline.

Stage 00 refreshes configured OpenFootball files and current Wikipedia pages for the Premier League, Europa League, Conference League, FA Cup, EFL Cup, Copa del Rey, Coppa Italia and Coupe de France. It requires the committed Schochastics snapshot.

Stage 01 parses configured OpenFootball and Wikipedia competitions, adds historical Schochastics rows, overlays current Premier League scores from Wikipedia, and merges into the saved master. A freshly parsed country/competition/season replaces existing rows for that key. Other master rows persist. Source choice is configured by competition rather than automatic OpenFootball-to-RSSSF fallback.

Stage 02 calculates from european_football_all_matches.csv and uses a frozen checkpoint through 2024-12-31 when present. Changes to pre-checkpoint inputs cannot simply be assumed to update ratings.

Stage 03 writes website JSON. The separate monthly RSSSF workflow refreshes and parses candidate pages for review only; it does not import to the master or recalculate ratings.

## Source conclusions

- Mixed Wikipedia result/date imports still exist in the ratings history. Some newer native-source imports replaced historical portions, but no blanket replacement is proven.
- The current cup/Europa/Conference parser uses Wikipedia directly. It is not an OpenFootball-date plus Wikipedia-score adapter. The current Premier League overlay is an explicit active mixed-source adapter.
- wikipedia_transfermarkt is a separately validated 2025/26 recovery import. Wikipedia results were matched to dated fixture records using club identities and score. The applied master retains Transfermarkt match URLs. It is not refreshed by the weekly pipeline.
- Denmark has 6,859 Weltfussball-labelled Elo matches. Its validation notes confirm a manual source import. RSSSF Denmark 2025/26 at https://www.rsssf.org/tablesd/den2026.html has final league tables and the European playoff result, but no regular league match listing; it cannot alone replace that entire season. Older seasons need individual coverage review.
- Russia has 240 BetExplorer-labelled Elo matches, all 2025/26. RSSSF Russia 2025/26 at https://www.rsssf.org/tablesr/rus2026.html contains dated league rounds and is a viable replacement candidate, subject to parsing, full-season coverage checks, and club identity/date/score reconciliation. Historical Russian RSSSF imports already exist through 2024/25.
- No finding in this review establishes that use of BetExplorer or Weltfussball is prohibited; replacement is a source preference, not a legal conclusion.

## Proposed streamlined design (not implemented)

1. Separate frozen historical inputs from live updates so a routine run cannot reintroduce or replace historical sources unintentionally.
2. Maintain one explicit source policy per competition and season: primary, permitted fallback, exact-date provider if needed, and completeness expectations.
3. Prefer complete dated OpenFootball results where available; use RSSSF where complete dated coverage has been verified. Keep documented Wikipedia/official-source exceptions where neither covers the competition adequately.
4. Retain useful validated historical data until a replacement has matching coverage. Preserve original result/date provenance separately instead of changing mixed labels without evidence.
5. Prepare a Russia 2025/26 RSSSF replacement preview first. Audit Denmark season-by-season; do not delete the manual backfill in advance.
6. Treat source disagreements as review items rather than silently overwriting results. Rebuild checkpoints explicitly if accepted historical changes require recalculating ratings.

No master, checkpoint, rating, or website-data changes were made during this review.
