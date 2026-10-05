import csv, pathlib, re
p=pathlib.Path(__file__).resolve().parents[2]/'EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26/openfootball_update_history'
rows=list(csv.DictReader((p/'current_routes.csv').open(encoding='utf-8-sig')))
for r in rows:
    f=p/(r['Country']+'_'+r['League'].replace(' ','_').replace('.','')+'.txt')
    if not f.exists(): continue
    header=''; last=''
    for line in f.read_text(encoding='utf-8').splitlines():
        if re.match(r'^\s*(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)\s+[A-Z][a-z]{2}\s+\d',line): header=line.strip()
        if re.search(r'\b\d+\s*-\s*\d+\b',line) and not line.lstrip().startswith(('#','=')): last=header
    r['LastScoredDateHeader']=last
    print(r['Country'],r['League'],last)
with (p/'current_routes.csv').open('w',newline='',encoding='utf-8-sig') as f:
    w=csv.DictWriter(f,fieldnames=list(rows[0])); w.writeheader(); w.writerows(rows)
