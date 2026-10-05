"""Import the reconciled, dated FIFA Club World Cup results into the master.

The supplied Wikipedia result list defines which matches belong; cached RSSSF
pages provide dates. Association columns disambiguate club identities for Elo.
"""
import csv
from collections import Counter
from datetime import datetime
import os
from pathlib import Path
import shutil

ROOT = Path(__file__).resolve().parents[2]
BASE = ROOT / 'EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit/club_world_cup_import'
RECON = BASE / 'reconciliation.csv'
TEAM_MAP = BASE / 'team_mapping_review.csv'
MASTER = ROOT / 'EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv'
ALIASES = ROOT / 'EuropeanFootball/pipeline_data/Reference/team_aliases.csv'

# Choices checked against club identities in the existing Elo file. In
# particular Atlético/MG must never be confused with Atlético/PR, and these
# Argentina labels must join the consolidated domestic identities.
OVERRIDES = {
    'Al Ahly': 'Ahly', 'Al-Ain': 'Ain', 'Al-Jazira': 'Jazeera',
    'Al-Nassr': 'Nasr', 'Al-Sadd': 'Sadd',
    'Atlético Mineiro': 'Atlético/MG',
    'Boca Juniors': 'Boca Juniors', 'Estudiantes': 'Estudiantes (LP)',
    'River Plate': 'River Plate', 'San Lorenzo': 'San Lorenzo',
    'Moghreb Tétouan': 'MA Tétouan',
    'South Melbourne': 'South Melbourne', 'Sydney FC': 'Sydney FC',
}


def rows(path):
    with path.open(encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))


def main():
    recon = rows(RECON)
    mapping = {r['ClubWorldCupName']: r for r in rows(TEAM_MAP)}
    aliases = {(r['Country'], r['SourceName']): r['CanonicalName'] for r in rows(ALIASES)}
    if len(recon) != 223 or any(r['MatchStatus'] != 'matched' for r in recon):
        raise SystemExit('The 223 supplied results must all reconcile before import.')
    if len(mapping) != 86 or any(not r['Country'] for r in mapping.values()):
        raise SystemExit('Club-to-association mapping is incomplete.')
    master_fields = None
    existing = []
    first_domestic = {}
    with MASTER.open(encoding='utf-8-sig', newline='') as f:
        reader = csv.DictReader(f)
        master_fields = list(reader.fieldnames)
        for r in reader:
            existing.append(r)
            if r['CompetitionType'] == 'league' and r['Date']:
                for field in ('Home', 'Away'):
                    key = (r['Country'], aliases.get((r['Country'], r[field]), r[field]))
                    if key not in first_domestic or r['Date'] < first_domestic[key]:
                        first_domestic[key] = r['Date']
    fields = list(dict.fromkeys(master_fields + ['HomeAssociation', 'AwayAssociation']))
    proposed = []
    for r in recon:
        # The RSSSF page supplies the actual home/away order, which can differ
        # from the user's result inventory in the 2000 group-stage entries.
        # Resolve RSSSF names to the user's labels by comparing the two sides.
        from prepare_club_world_cup_import import namescore
        uhome, uaway = r['Team 1'], r['Team 2']
        direct = namescore(uhome, r['RSSSFHome']) + namescore(uaway, r['RSSSFAway'])
        reverse = namescore(uhome, r['RSSSFAway']) + namescore(uaway, r['RSSSFHome'])
        home_name, away_name = (uhome, uaway) if direct >= reverse else (uaway, uhome)
        home, away = mapping[home_name], mapping[away_name]
        home_canonical = OVERRIDES.get(home_name, home['EloName'])
        away_canonical = OVERRIDES.get(away_name, away['EloName'])
        if not home_canonical or not away_canonical:
            raise SystemExit('No canonical name for ' + home_name + ' / ' + away_name)
        row = {x: '' for x in fields}
        row.update(Season=r['Year'], Country='World', Competition='fifa_club_world_cup',
                   CompetitionType='continental', League='FIFA Club World Cup',
                   Date=r['Date'], Home=home_canonical, Away=away_canonical,
                   Result='1-0' if int(r['HG']) > int(r['AG']) else '0-1' if int(r['HG']) < int(r['AG']) else '0.5-0.5',
                   Score=f"{r['HG']}-{r['AG']}", Source='wikipedia_rsssf',
                   SourcePage=r['SourcePage'], Stage=r['Stage'], DateApprox='FALSE',
                   SourceFile=str(ROOT / 'EuropeanFootball/pipeline_data/Source/rsssf/all/pages/tablesf' / r['SourcePage']),
                   HomeAssociation=home['Country'], AwayAssociation=away['Country'])
        proposed.append(row)
    if len({(r['Date'], r['HomeAssociation'], r['Home'], r['AwayAssociation'], r['Away'], r['Score']) for r in proposed}) != len(proposed):
        raise SystemExit('Duplicate Club World Cup match after identity mapping.')
    coverage = Counter()
    for r in proposed:
        if r['Date'] >= first_domestic.get((r['HomeAssociation'], r['Home']), '9999-12-31') and \
           r['Date'] >= first_domestic.get((r['AwayAssociation'], r['Away']), '9999-12-31'):
            coverage[r['Season']] += 1
    print('Supplied dated matches:', len(proposed))
    print('Estimated Elo-eligible matches using raw domestic names:', sum(coverage.values()))
    print('Eligible by year:', dict(sorted(coverage.items())))
    target = BASE / 'import_preview.csv'
    with target.open('w', encoding='utf-8', newline='') as f:
        w = csv.DictWriter(f, fields)
        w.writeheader()
        w.writerows(proposed)
    if os.getenv('CLUB_WORLD_CUP_IMPORT_DRY_RUN') == '1':
        print('Dry run: master unchanged.')
        return
    retained = [r for r in existing if r.get('Competition') != 'fifa_club_world_cup']
    stamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    backup = MASTER.with_name(MASTER.stem + '_before_club_world_cup_' + stamp + '.csv')
    shutil.copy2(MASTER, backup)
    temp = MASTER.with_suffix('.csv.tmp')
    with temp.open('w', encoding='utf-8-sig', newline='') as f:
        w = csv.DictWriter(f, fields)
        w.writeheader()
        w.writerows(retained)
        w.writerows(proposed)
    temp.replace(MASTER)
    print('Master Club World Cup matches:', len(proposed))
    print('Backup:', backup)


if __name__ == '__main__':
    main()
