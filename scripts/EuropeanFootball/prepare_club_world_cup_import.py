"""Reconcile the user's Club World Cup results with dates in cached RSSSF pages.

This is an offline preparation step. It writes candidates and a review report;
it never alters the production match file.
"""

import csv
import difflib
import html
from html.parser import HTMLParser
from pathlib import Path
import re
import unicodedata

ROOT = Path(__file__).resolve().parents[2]
USER = Path.home() / 'Downloads' / 'fifa_club_world_cup_all_matches_2000_2025.csv'
PAGES = ROOT / 'EuropeanFootball/pipeline_data/Source/rsssf/all/pages/tablesf'
OUT = ROOT / 'EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_World_Audit/club_world_cup_import'
FINAL = ROOT / 'EuropeanFootball/pipeline_data/Elo/football_elo_final_ratings.csv'

# Club World Cup entrants, checked against the participant country printed on
# the RSSSF tournament pages. These explicit associations disambiguate names
# shared by clubs in several countries.
COUNTRIES = {
    'AC Milan': 'Italy', 'AS Pirae': 'Tahiti', 'Adelaide United': 'Australia',
    'Al Ahly': 'Egypt', 'Al-Ahli Dubai': 'United Arab Emirates', 'Al-Ain': 'United Arab Emirates',
    'Al-Duhail': 'Qatar', 'Al-Hilal': 'Saudi Arabia', 'Al-Ittihad': 'Saudi Arabia',
    'Al-Jazira': 'United Arab Emirates', 'Al-Nassr': 'Saudi Arabia', 'Al-Sadd': 'Qatar',
    'Al-Wahda': 'United Arab Emirates', 'América': 'Mexico', 'Atlante': 'Mexico',
    'Atlético Madrid': 'Spain', 'Atlético Mineiro': 'Brazil', 'Atlético Nacional': 'Colombia',
    'Auckland City': 'New Zealand', 'Barcelona': 'Spain', 'Bayern Munich': 'Germany',
    'Benfica': 'Portugal', 'Boca Juniors': 'Argentina', 'Borussia Dortmund': 'Germany',
    'Botafogo': 'Brazil', 'Chelsea': 'England', 'Corinthians': 'Brazil', 'Cruz Azul': 'Mexico',
    'ES Sétif': 'Algeria', 'Espérance de Tunis': 'Tunisia', 'Estudiantes': 'Argentina',
    'Flamengo': 'Brazil', 'Fluminense': 'Brazil', 'Gamba Osaka': 'Japan', 'Grêmio': 'Brazil',
    'Guadalajara': 'Mexico', 'Guangzhou Evergrande': 'China', 'Hekari United': 'Papua New Guinea',
    'Hienghène Sport': 'New Caledonia', 'Inter Miami': 'United States', 'Inter Milan': 'Italy',
    'Internacional': 'Brazil', 'Jeonbuk Hyundai Motors': 'South Korea', 'Juventus': 'Italy',
    'Kashima Antlers': 'Japan', 'Kashiwa Reysol': 'Japan', 'LDU Quito': 'Ecuador',
    'León': 'Mexico', 'Liverpool': 'England', 'Los Angeles FC': 'United States',
    'Mamelodi Sundowns': 'South Africa', 'Manchester City': 'England',
    'Manchester United': 'England', 'Moghreb Tétouan': 'Morocco', 'Monterrey': 'Mexico',
    'Necaxa': 'Mexico', 'Pachuca': 'Mexico', 'Palmeiras': 'Brazil',
    'Paris Saint-Germain': 'France', 'Pohang Steelers': 'South Korea', 'Porto': 'Portugal',
    'Raja Casablanca': 'Morocco', 'Real Madrid': 'Spain', 'Red Bull Salzburg': 'Austria',
    'River Plate': 'Argentina', 'San Lorenzo': 'Argentina', 'Sanfrecce Hiroshima': 'Japan',
    'Santos': 'Brazil', 'Saprissa': 'Costa Rica', 'Seattle Sounders': 'United States',
    'Seongnam Ilhwa Chunma': 'South Korea', 'Sepahan': 'Iran', 'South Melbourne': 'Australia',
    'Sydney FC': 'Australia', 'São Paulo': 'Brazil', 'TP Mazembe': 'DR Congo',
    'Team Wellington': 'New Zealand', 'Tigres UANL': 'Mexico', 'Ulsan HD': 'South Korea',
    'Ulsan Hyundai': 'South Korea', 'Urawa Red Diamonds': 'Japan',
    'Vasco da Gama': 'Brazil', 'Waitakere United': 'New Zealand',
    'Western Sydney Wanderers': 'Australia', 'Wydad Casablanca': 'Morocco',
    'Étoile du Sahel': 'Tunisia',
}


class Pres(HTMLParser):
    def __init__(self):
        super().__init__()
        self.inside = False
        self.lines = []
        self.buf = []

    def handle_starttag(self, tag, attrs):
        if tag == 'pre':
            self.inside = True
        elif self.inside and tag == 'br':
            self.buf.append('\n')

    def handle_endtag(self, tag):
        if tag == 'pre':
            self.inside = False
            self.buf.append('\n')

    def handle_data(self, data):
        if self.inside:
            self.buf.append(data)

    def finish(self):
        return ''.join(self.buf).splitlines()


def norm(s):
    s = unicodedata.normalize('NFKD', s)
    s = ''.join(c for c in s if not unicodedata.combining(c))
    s = s.lower().replace('ü', 'u')
    s = s.replace('internazionale', 'intermilan')
    s = re.sub(r'\b(?:fc|cf|sc|ac|club|football|futebol|de|da|do)\b', ' ', s)
    return re.sub('[^a-z0-9]+', '', s)


def namescore(a, b):
    a, b = norm(a), norm(b)
    if a == b:
        return 1.0
    if len(a) >= 5 and len(b) >= 5 and (a in b or b in a):
        return 0.9
    return difflib.SequenceMatcher(None, a, b).ratio()


def source_lines(path):
    p = Pres()
    p.feed(path.read_bytes().decode('latin-1'))
    return [html.unescape(x).strip() for x in p.finish()]


def parse_page(path, year):
    games = []
    current = None
    for raw in source_lines(path):
        line = re.sub(r'\s+', ' ', raw).strip()
        if not line:
            continue
        # Most editions put dd-mm-yy on the fixture line. 2000 uses a
        # "Jan 5:" heading shared by multiple fixtures.
        dated_iso = re.match(r'^(20\d{2})-(\d{1,2})-(\d{1,2})\b\s*(.*)$', line)
        dated = re.match(r'^(\d{1,2})\s*-\s*(\d{1,2})\s*-\s*(\d{2,4})\b\s*(.*)$', line)
        if dated_iso:
            yr, month, day, line = dated_iso.groups()
            current = f'{int(yr):04d}-{int(month):02d}-{int(day):02d}'
        elif dated:
            day, month, shortyear, line = dated.groups()
            yr = int(shortyear)
            yr = yr + (2000 if yr < 80 else 1900) if yr < 100 else yr
            current = f'{yr:04d}-{int(month):02d}-{int(day):02d}'
        else:
            month_name = re.match(r'^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+(\d{1,2})\s*:\s*(.*)$', line, re.I)
            if month_name:
                mon, day, line = month_name.groups()
                month = 'jan feb mar apr may jun jul aug sep oct nov dec'.split().index(mon.lower()) + 1
                current = f'{year:04d}-{month:02d}-{int(day):02d}'
        if not current:
            continue
        # Score in the middle, sometimes followed by a venue or [pen] note.
        mid = re.match(r'^(?:Match\s+\d+\.?\s+)?(.+?)\s+(\d{1,2})\s*-\s*(\d{1,2})\s+(.+)$', line, re.I)
        if mid and re.search('[A-Za-z]', mid.group(4)) and not mid.group(4).startswith('[') and ' - ' not in mid.group(1):
            home, hg, ag, away = mid.groups()
            away = re.split(r'\s{2,}|\s*\[', away, 1)[0].strip()
            games.append(dict(Year=str(year), Date=current, RSSSFHome=home.strip(), RSSSFAway=away,
                              HG=int(hg), AG=int(ag), SourcePage=path.name, RawLine=raw))
            continue
        # 2000: Home - Away 1-0, optionally a penalty score afterwards.
        tail = re.match(r'^(.+?)\s+-\s+(.+?)\s+(\d{1,2})\s*-\s*(\d{1,2})(?:\s|$)', line)
        if tail:
            home, away, hg, ag = tail.groups()
            games.append(dict(Year=str(year), Date=current, RSSSFHome=home.strip(), RSSSFAway=away.strip(),
                              HG=int(hg), AG=int(ag), SourcePage=path.name, RawLine=raw))
    return games


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    with USER.open(encoding='utf-8-sig', newline='') as f:
        given = list(csv.DictReader(f))
    pages = {}
    for p in PAGES.glob('fifa-wcc*.html'):
        if '-det' in p.stem:
            continue
        suffix = p.stem.replace('fifa-wcc', '')
        year = int(suffix)
        year = 2000 + year if year < 100 else year
        pages[year] = p
    parsed = [game for year, p in sorted(pages.items()) for game in parse_page(p, year)]
    used = set()
    joined = []
    for index, row in enumerate(given):
        match = re.match(r'^\s*(\d+)\D+(\d+)', row['Score'])
        if not match:
            joined.append(dict(**row, MatchStatus='unreadable_user_score'))
            continue
        hg, ag = map(int, match.groups())
        candidates = []
        for j, game in enumerate(parsed):
            if j in used or game['Year'] != row['Year']:
                continue
            if (game['HG'], game['AG']) == (hg, ag):
                strength = (namescore(row['Team 1'], game['RSSSFHome']) +
                            namescore(row['Team 2'], game['RSSSFAway'])) / 2
                candidates.append((strength, j))
            if (game['HG'], game['AG']) == (ag, hg):
                strength = (namescore(row['Team 1'], game['RSSSFAway']) +
                            namescore(row['Team 2'], game['RSSSFHome'])) / 2
                candidates.append((strength - .02, j))
        candidates.sort(reverse=True)
        if candidates and candidates[0][0] >= .73 and (len(candidates) == 1 or candidates[0][0] - candidates[1][0] >= .08):
            score, j = candidates[0]
            used.add(j)
            joined.append(dict(row | parsed[j], MatchStatus='matched', NameScore=round(score, 3)))
        else:
            joined.append(dict(**row, MatchStatus='needs_review', BestScore=round(candidates[0][0], 3) if candidates else ''))
    fields = list(dict.fromkeys(k for row in joined for k in row))
    with (OUT / 'reconciliation.csv').open('w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fields)
        w.writeheader()
        w.writerows(joined)
    with FINAL.open(encoding='utf-8-sig', newline='') as f:
        ratings = list(csv.DictReader(f))
    mapping = []
    for team in sorted({x for r in given for x in (r['Team 1'], r['Team 2'])}):
        country = COUNTRIES.get(team)
        choices = [(namescore(team, r['Team']), r) for r in ratings if r.get('Country') == country]
        choices.sort(key=lambda x: x[0], reverse=True)
        best = choices[0] if choices else (0, {})
        second = choices[1] if len(choices) > 1 else (0, {})
        mapping.append(dict(ClubWorldCupName=team, Country=country or '',
                            EloName=best[1].get('Team', ''), EloKey=best[1].get('TeamKey', ''),
                            Similarity=round(best[0], 3), RunnerUp=second[1].get('Team', ''),
                            RunnerUpSimilarity=round(second[0], 3),
                            Status='strong' if best[0] >= .85 and best[0]-second[0] >= .08 else 'review'))
    with (OUT / 'team_mapping_review.csv').open('w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, mapping[0].keys())
        w.writeheader()
        w.writerows(mapping)
    from collections import Counter
    print('User matches:', len(given), 'RSSSF date/score rows:', len(parsed))
    print('Reconciliation:', dict(Counter(row['MatchStatus'] for row in joined)))
    print('Unmatched RSSSF rows:', len(parsed) - len(used))
    print('Team mapping:', dict(Counter(row['Status'] for row in mapping)))
    for row in mapping:
        if row['Status'] != 'strong':
            print('TEAM REVIEW', row['ClubWorldCupName'], row['Country'], row['EloName'], row['Similarity'])
    for row in joined:
        if row['MatchStatus'] != 'matched':
            print('REVIEW', row['Year'], row['Team 1'], row['Score'], row['Team 2'], row.get('BestScore', ''))


if __name__ == '__main__':
    main()
