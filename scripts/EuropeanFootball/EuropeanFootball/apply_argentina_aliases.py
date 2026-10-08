"""Apply reviewed Argentina RSSSF name mappings to the Elo alias registry."""
import csv
from datetime import datetime
from pathlib import Path
import shutil

ROOT = Path(__file__).resolve().parents[2]
REVIEW = ROOT / 'EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_Argentina_Modern_Audit/argentina_alias_review.csv'
REGISTRY = ROOT / 'EuropeanFootball/pipeline_data/Reference/team_aliases.csv'

with REVIEW.open(encoding='utf-8-sig', newline='') as f:
    proposed = list(csv.DictReader(f))
if not proposed or any(r['Status'] != 'mapped' or not r['CanonicalName'] for r in proposed):
    raise SystemExit('Alias review still has unresolved Argentina names.')
with REGISTRY.open(encoding='utf-8-sig', newline='') as f:
    existing = list(csv.DictReader(f))
    fields = list(existing[0])

entries = {(r['Country'], r['SourceName']): r for r in existing}
for r in proposed:
    if r['SourceName'] == r['CanonicalName']:
        continue
    entries[('Argentina', r['SourceName'])] = dict(Country='Argentina',
        SourceName=r['SourceName'], CanonicalName=r['CanonicalName'])
ordered = sorted(entries.values(), key=lambda r: (r['Country'], r['SourceName']))
backup = REGISTRY.with_name('team_aliases_before_argentina_' + datetime.now().strftime('%Y%m%d_%H%M%S') + '.csv')
shutil.copy2(REGISTRY, backup)
temp = REGISTRY.with_suffix('.csv.tmp')
with temp.open('w', encoding='utf-8-sig', newline='') as f:
    w = csv.DictWriter(f, fields)
    w.writeheader()
    w.writerows(ordered)
temp.replace(REGISTRY)
print('Argentina aliases:', sum(r['Country']=='Argentina' for r in ordered))
print('Backup:', backup)
