"""Build an explicit Argentina RSSSF name review from the corrected audit."""
import csv
from pathlib import Path
import re
import unicodedata

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_Argentina_Modern_Audit/all_dated_rsssf_games.csv'
OUT = ROOT / 'EuropeanFootball/pipeline_data/Manual_Sources/RSSSF_Argentina_Modern_Audit/argentina_alias_review.csv'


def plain(s):
    s = unicodedata.normalize('NFKD', s).lower()
    return ''.join(c for c in s if not unicodedata.combining(c))


def canonical(name):
    s = plain(name)
    if 'crucero del norte' in s: return 'Crucero del Norte'
    if 'independiente rivadavia' in s: return 'Independiente Rivadavia'
    if 'san martin' in s:
        if s == 'ca san martin': return 'San Martín (San Juan)'  # 2017/18 top flight
        if any(t in s for t in ('san juan', '(sj)', '(s j)')): return 'San Martín (San Juan)'
        if any(t in s for t in ('tucuman', '(sm', 'club atletico san martin')): return 'San Martín (Tucumán)'
        return None
    if 'boca juniors' in s: return 'Boca Juniors'
    if 'river plate' in s: return 'River Plate'
    if 'san lorenzo' in s: return 'San Lorenzo'
    if 'estudiantes' in s: return 'Estudiantes (LP)'
    if 'racing' in s: return 'Racing'
    if 'independiente' in s: return 'Independiente'
    if 'gimnasia' in s: return 'Gimnasia y Esgrima (LP)'
    if 'godoy cruz' in s: return 'Godoy Cruz'
    if 'defensa y justicia' in s: return 'Defensa y Justicia'
    if 'argentinos juniors' in s: return 'Argentinos Juniors'
    if 'newell' in s: return "Newell's Old Boys"
    if 'velez sarsfield' in s: return 'Vélez Sarsfield'
    if 'atletido de rafaela' in s or 'atletico de rafaela' in s or 'atletico rafaela' in s:
        return 'Atlético de Rafaela'
    for token, target in (
        ('rosario central', 'Rosario Central'), ('central cordoba', 'Central Córdoba'),
        ('atletico tucuman', 'Atlético Tucumán'), ('atl. tucuman', 'Atlético Tucumán'),
        ('talleres', 'Talleres'), ('huracan', 'Huracán'), ('banfield', 'Banfield'),
        ('lanus', 'Lanús'), ('colon', 'Colón'), ('union', 'Unión'),
        ('barracas central', 'Barracas Central'), ('platense', 'Platense'),
        ('aldosivi', 'Aldosivi'), ('belgrano', 'Belgrano'),
        ('sarmiento', 'Sarmiento'), ('tigre', 'Tigre'),
        ('arsenal', 'Arsenal'), ('olimpo', 'Olimpo'),
        ('quilmes', 'Quilmes'), ('all boys', 'All Boys'),
        ('patronato', 'Patronato'), ('riestra', 'Deportivo Riestra'),
        ('temperley', 'Temperley'), ('chacarita', 'Chacarita Juniors'),
        ('nueva chicago', 'Nueva Chicago'), ('instituto', 'Instituto'),
    ):
        if token in s: return target
    return None


def main():
    counts = {}
    with SOURCE.open(encoding='utf-8-sig', newline='') as f:
        for row in csv.DictReader(f):
            for field in ('Home', 'Away'):
                name = row[field]
                counts[name] = counts.get(name, 0) + 1
    report = []
    for name, games in sorted(counts.items()):
        target = canonical(name)
        report.append(dict(SourceName=name, CanonicalName=target or '', Appearances=games,
                           Status='mapped' if target else 'review'))
    with OUT.open('w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, report[0])
        w.writeheader()
        w.writerows(report)
    print('Names:', len(report), 'mapped:', sum(r['Status']=='mapped' for r in report))
    for row in report:
        if row['Status']=='review':print('REVIEW',row)


if __name__ == '__main__':main()
