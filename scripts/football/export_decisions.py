"""Add explicit shootout metadata to existing exports, without running Elo.

Reads retained '(p H-A)' scores, verified structure decisions, and optional
pipeline_data/source/shootouts.csv (date,home_team,away_team,winner).
Never infers a winner from advancement or changes score/result/rating fields.
"""
import argparse
import csv
import hashlib
import json
import re
import unicodedata
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def token(value):
    return re.sub(r'[^a-z0-9]', '', unicodedata.normalize('NFKD', str(value)).encode('ascii', 'ignore').decode().lower())


def score_pair(row):
    if row.get('homeScore') is not None and row.get('awayScore') is not None:
        return row['homeScore'], row['awayScore']
    match = re.match(r'^(\d+)\s*[-–]\s*(\d+)', str(row.get('score', '')))
    return tuple(map(int, match.groups())) if match else None


def explicit_decision(row):
    pair = score_pair(row)
    if not pair or pair[0] != pair[1] or row.get('upcoming') or row.get('played') is False:
        return None
    home, away = row.get('home'), row.get('away')
    if not home or not away:
        return None
    ph, pa = row.get('homePenalties'), row.get('awayPenalties')
    if ph is None or pa is None:
        match = re.search(r'\(\s*(?:p|pens?\.?|penalties)\s+(\d+)\s*[-–]\s*(\d+)\s*\)', str(row.get('score', '')), re.I)
        if match:
            ph, pa = map(int, match.groups())
    if ph is not None and pa is not None and int(ph) != int(pa):
        return {'winner': home if int(ph) > int(pa) else away,
                'homePenalties': int(ph), 'awayPenalties': int(pa), 'decision': 'penalties'}
    if row.get('decision') == 'penalties' and row.get('winner') in (home, away):
        return {'winner': row['winner'], 'decision': 'penalties'}
    return None


def records(value):
    if isinstance(value, dict):
        if 'date' in value and 'home' in value and 'away' in value:
            yield value
        for child in value.values():
            yield from records(child)
    elif isinstance(value, list):
        for child in value:
            yield from records(child)


def refresh(sport):
    base = ROOT / sport
    cache_path = base / 'pipeline_data/reference/shootout_export_cache.json'
    cache = json.loads(cache_path.read_text(encoding='utf-8')) if cache_path.exists() else {}
    cached_files = cache.get('files', {})
    next_files = {}
    def signature(path):
        stat = path.stat()
        return [stat.st_mtime_ns, stat.st_size]
    teams = json.loads((base / 'data/teams.json').read_text(encoding='utf-8'))
    aliases = {token(t[field]): token(t['id']) for t in teams for field in ('name', 'id')}
    keyname = lambda name: aliases.get(token(name), token(name))
    decisions, conflicts = {}, set()

    def add(row, decision):
        h, a = keyname(row['home']), keyname(row['away'])
        key = (row['date'], *sorted((h, a)))
        winner = keyname(decision['winner'])
        if winner not in (h, a):
            return
        entry = {'winner': winner, 'decision': 'penalties'}
        if 'homePenalties' in decision:
            entry['penalties'] = {h: decision['homePenalties'], a: decision['awayPenalties']}
        if key in decisions and (decisions[key]['winner'] != winner or
                ('penalties' in entry and 'penalties' in decisions[key] and entry['penalties'] != decisions[key]['penalties'])):
            conflicts.add(key)
        else:
            decisions[key] = {**decisions.get(key, {}), **entry}

    paths = sorted((base / 'data/games').glob('*.json')) + sorted((base / 'data/tournaments').glob('*/*.json'))
    # Collect evidence first; the opponent's profile may contain the same match.
    for path in paths:
        relative = str(path.relative_to(base))
        previous = cached_files.get(relative, {})
        if previous.get('signature') == signature(path):
            evidence = previous.get('evidence', [])
        else:
            evidence = []
            raw = path.read_text(encoding='utf-8')
            candidate = '(p ' in raw or 'penalt' in raw.lower() or '"decision"' in raw or '"homePenalties"' in raw
            for row in records(json.loads(raw)) if candidate else []:
                decision = explicit_decision(row)
                if decision:
                    evidence.append(dict(date=row['date'], home=row['home'], away=row['away'], **decision))
        next_files[relative] = dict(signature=signature(path), evidence=evidence)
        for row in evidence:
            add(row, row)
    # The Club master also retains cup shootouts not present in older exports.
    master = base / 'pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv'
    if master.exists():
        with master.open(encoding='utf-8-sig', newline='') as handle:
            for original in csv.DictReader(handle):
                row = dict(date=original['Date'], home=original['Home'], away=original['Away'], score=original['Score'])
                decision = explicit_decision(row)
                if decision:
                    add(row, decision)
    for path in (base / 'data/tournament-structure').glob('*/*.json'):
        structure = json.loads(path.read_text(encoding='utf-8'))
        for rnd in structure.get('knockoutRounds', []):
            for match in rnd.get('matches', []):
                if match.get('decision') == 'penalties' and match.get('winner') in match.get('teams', []):
                    add(dict(date=match['date'], home=match['teams'][0], away=match['teams'][1]), match)
    source = base / 'pipeline_data/source/shootouts.csv'
    if source.exists():
        with source.open(encoding='utf-8-sig', newline='') as handle:
            for row in csv.DictReader(handle):
                add(dict(date=row['date'], home=row['home_team'], away=row['away_team']), dict(winner=row['winner']))
    updated = matches = 0
    decision_hash = hashlib.sha256(repr((sorted((key, repr(value)) for key, value in decisions.items()), sorted(conflicts))).encode()).hexdigest()
    participants = {team for key in decisions for team in key[1:]}
    for path in paths:
        relative = str(path.relative_to(base))
        if (cache.get('decision_hash') == decision_hash and
                cached_files.get(relative, {}).get('signature') == signature(path)):
            continue
        if path.parent.name == 'games' and keyname(path.stem) not in participants:
            continue
        value = json.loads(path.read_text(encoding='utf-8'))
        changed = False
        for row in records(value):
            pair = score_pair(row)
            if not pair or pair[0] != pair[1] or row.get('upcoming') or row.get('played') is False:
                continue
            h, a = keyname(row['home']), keyname(row['away'])
            key = (row['date'], *sorted((h, a)))
            decision = decisions.get(key)
            if key in conflicts:
                for field in ('decision', 'winner', 'homePenalties', 'awayPenalties'):
                    if field in row:
                        del row[field]
                        changed = True
                continue
            if not decision:
                continue
            fields = {'decision': 'penalties', 'winner': row['home'] if decision['winner'] == h else row['away']}
            if 'penalties' in decision:
                fields.update(homePenalties=decision['penalties'][h], awayPenalties=decision['penalties'][a])
            if any(row.get(k) != v for k, v in fields.items()):
                row.update(fields)
                changed = True
                matches += 1
        if changed:
            temporary = path.with_suffix('.json.tmp')
            temporary.write_text(json.dumps(value, ensure_ascii=False, separators=(',', ':')) + '\n', encoding='utf-8')
            temporary.replace(path)
            next_files[relative]['signature'] = signature(path)
            updated += 1
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    cache_path.write_text(json.dumps(dict(decision_hash=decision_hash, files=next_files), ensure_ascii=False), encoding='utf-8')
    index = [dict(date=key[0], teams=list(key[1:]), **value) for key, value in sorted(decisions.items()) if key not in conflicts]
    (base / 'data/shootout-decisions.json').write_text(json.dumps(index, ensure_ascii=False, separators=(',', ':'))+'\n', encoding='utf-8')
    print(f'{sport}: {len(decisions)-len(conflicts)} explicit shootouts; {updated} JSON files / {matches} records enriched; {len(conflicts)} conflicts skipped. No Elo or score changes.')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('sport', choices=['EuropeanFootball', 'InternationalFootball', 'all'], default='all', nargs='?')
    args = parser.parse_args()
    for sport in (['EuropeanFootball', 'InternationalFootball'] if args.sport == 'all' else [args.sport]):
        refresh(sport)
