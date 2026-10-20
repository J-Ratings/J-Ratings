"""Local bracket audit only. No website, ratings or prediction writes.

Run: python scripts/go/test_lanke_2026.py
Uses the saved single-page snapshot; refresh explicitly with --download.
"""
import argparse
import csv
import json
import re
import unicodedata
import urllib.request
from datetime import datetime, timezone
from html.parser import HTMLParser
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'Go/pipeline_data/tournament_tests/lanke_2026'
URL = 'https://gotoeveryone.k2ss.info/news/wr/quzhou-lanke/4/'


class Bracket(HTMLParser):
    def __init__(self):
        super().__init__()
        self.active = False
        self.rows = []
        self.cell = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == 'table' and 'tournament-wide' in attrs.get('class', ''):
            self.active = True
        if not self.active:
            return
        if tag == 'tr':
            self.rows.append([])
        if tag in ('td', 'th'):
            self.cell = {'text': [], 'rowspan': int(attrs.get('rowspan', 1)),
                         'colspan': int(attrs.get('colspan', 1)), 'result': ''}
            self.rows[-1].append(self.cell)
        if tag == 'a' and self.cell is not None:
            self.cell['link'] = attrs.get('href', '')

    def handle_data(self, data):
        if self.active and self.cell is not None and data.strip():
            self.cell['text'].append(data.strip())

    def handle_endtag(self, tag):
        if tag in ('td', 'th'):
            self.cell = None
        if tag == 'table':
            self.active = False


def clean(text):
    text = re.sub(r'^\([a-z]+\)', '', text).strip()
    return re.sub(r'\([^)]*\)$', '', text).strip()


def key(text):
    return ''.join(c for c in unicodedata.normalize('NFKD', text).casefold()
                   if c.isalnum() and not unicodedata.combining(c))


def main():
    args = argparse.ArgumentParser()
    args.add_argument('--download', action='store_true')
    args = args.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    source = OUT / 'source.html'
    if args.download:
        request = urllib.request.Request(URL, headers={'User-Agent': 'J-Ratings local tournament test'})
        with urllib.request.urlopen(request, timeout=30) as response:
            source.write_bytes(response.read())
    if not source.exists():
        raise SystemExit('No snapshot. Run once with --download.')
    parser = Bracket()
    parser.feed(source.read_text(encoding='utf-8'))
    assert len(parser.rows) == 49, 'Unexpected main bracket size; review format.'
    headers = [' '.join(c['text']) for c in parser.rows[0]]
    assert len(headers) == 7 and 'Semifinals' in headers[5], 'Unexpected columns.'
    grid = {}
    cells = []
    for r, row in enumerate(parser.rows[1:]):
        col = 0
        for cell in row:
            while (r, col) in grid:
                col += 1
            cell.update(row=r, column=col)
            cells.append(cell)
            for rr in range(r, r + cell['rowspan']):
                for cc in range(col, col + cell['colspan']):
                    assert (rr, cc) not in grid, 'Overlapping bracket cells.'
                    grid[rr, cc] = cell
            col += cell['colspan']
    profiles = json.loads((ROOT / 'Go/data/players.json').read_text(encoding='utf-8'))
    names = {}
    for p in profiles:
        names.setdefault(key(p['name']), []).append(p)
    entrants = []
    for cell in cells:
        if cell['column'] == 0:
            name = clean(' '.join(cell['text']))
            matches = names.get(key(name), [])
            entrants.append({'source_name': name, 'profile_id': matches[0]['id'] if len(matches) == 1 else '',
                             'status': 'MATCHED' if len(matches) == 1 else 'UNMATCHED_OR_AMBIGUOUS'})
    fixtures = []
    for cell in cells:
        col = cell['column']
        if col == 0 or col == 6:
            continue  # byes and best-of-three final are not individual result cells
        incoming = []
        seen = set()
        for r in range(cell['row'], min(48, cell['row'] + cell['rowspan'])):
            prior = grid.get((r, col - 1))
            if prior is None or id(prior) in seen:
                continue
            seen.add(id(prior))
            if prior['column'] == 0:
                incoming.append(clean(' '.join(prior['text'])))
            elif prior['text'] and any(re.fullmatch(r'[BW]\+.+', t) for t in prior['text']):
                incoming.append(clean(prior['text'][0]))
        result = next((t for t in cell['text'] if re.fullmatch(r'[BW]\+.+', t)), '')
        winner = clean(cell['text'][0]) if result else ''
        date = re.search(r'2026-\d{2}-\d{2}', headers[col])
        ids = [names.get(key(n), []) for n in incoming]
        valid = len(incoming) == 2 and all(len(m) == 1 for m in ids)
        if result and winner not in incoming:
            valid = False
        fixtures.append({'tournament_id': 'quzhou-lanke', 'edition': 4, 'season': 2026,
                         'tournament_name': '4th Quzhou-Lanke Cup', 'format': 'knockout',
                         'round': re.sub(r'2026-.*', '', headers[col]).strip(),
                         'date': date.group() if date else '', 'players': incoming,
                         'profile_ids': [m[0]['id'] if len(m) == 1 else '' for m in ids],
                         'winner': winner, 'result': result,
                         'status': 'COMPLETED' if valid and result else 'SCHEDULED_DATE_ONLY' if valid else 'HELD_REVIEW',
                         'pairing_evidence': 'bracket_progression', 'source_url': URL,
                         'public_prediction_ready': False})
    payload = {'checked_at_utc': datetime.now(timezone.utc).isoformat(),
               'note': 'Local test. Bracket progression requires format confirmation; date-only games are not open predictions. Final games conditional on semifinal winners and series score.',
               'entrants': entrants, 'fixtures': fixtures,
               'conditional_final_dates': ['2026-10-13', '2026-10-15', '2026-10-16']}
    (OUT / 'review.json').write_text(json.dumps(payload, indent=2, ensure_ascii=False), encoding='utf-8')
    with (OUT / 'player_matching.csv').open('w', encoding='utf-8', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=['source_name', 'profile_id', 'status'])
        writer.writeheader()
        writer.writerows(entrants)
    print(f"Players matched: {sum(bool(e['profile_id']) for e in entrants)}/{len(entrants)}")
    for f in fixtures:
        if f['round'] == 'Semifinals' or f['status'] == 'HELD_REVIEW':
            print(f"{f['round']} {f['date']}: {' vs '.join(f['players'])} [{f['status']}]")
    print(f'Local review: {OUT / "review.json"}')


if __name__ == '__main__':
    main()
