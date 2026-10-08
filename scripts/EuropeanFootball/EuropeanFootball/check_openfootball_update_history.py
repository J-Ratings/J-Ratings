"""Read-only check of the current-season routes configured in stage 00."""
import concurrent.futures, csv, datetime, json, pathlib, re, urllib.request, urllib.error

ROOT = pathlib.Path(__file__).resolve().parents[2]
OUT = ROOT / 'EuropeanFootball/pipeline_data/Manual_Sources/Season_2025_26/openfootball_update_history'
OUT.mkdir(parents=True, exist_ok=True)
JOBS = []
for country, repo, leagues in [
    ('England','england',[('Premier League','1-premierleague.txt'),('Championship','2-championship.txt'),('League One','3-league1.txt'),('League Two','4-league2.txt'),('National League','5-nationalleague.txt')]),
    ('Spain','espana',[('La Liga','1-liga.txt'),('Segunda Division','2-liga2.txt')]),
    ('Italy','italy',[('Serie A','1-seriea.txt'),('Serie B','2-serieb.txt')]),
    ('Germany','deutschland',[('Bundesliga','1-bundesliga.txt'),('2. Bundesliga','2-bundesliga2.txt')]),
    ('Austria','austria',[('Bundesliga','1-bundesliga.txt')]),
    ('Belgium','belgium',[('Pro League','be1.txt')])]:
    for league, name in leagues: JOBS.append((country,league,repo,'2026-27/'+name))
for country,folder,league,suffix in [
    ('France','france','Ligue 1','fr1'),('France','france','Ligue 2','fr2'),
    ('Portugal','portugal','Primeira Liga','pt1'),('Netherlands','netherlands','Eredivisie','nl1'),
    ('Switzerland','switzerland','Super League','ch1'),('Scotland','scotland','Premiership','sco1'),
    ('Turkey','turkey','Super Lig','tr1'),('Greece','greece','Super League','gr1'),
    ('Czechia','czech-republic','First League','cz1'),('Ukraine','ukraine','Premier League','ua1')]:
    JOBS.append((country,league,'europe',folder+'/2026-27_'+suffix+'.txt'))

def get(url):
    req=urllib.request.Request(url,headers={'User-Agent':'J-Ratings-read-only-source-audit','Accept':'application/vnd.github+json'})
    return urllib.request.urlopen(req,timeout=12).read().decode('utf-8')

def check(job):
    country,league,repo,path=job
    row=dict(Country=country,League=league,URL=f'https://github.com/openfootball/{repo}/blob/master/{path}',Status='',LastCommit='',RecentCommitDates='',MedianGapDays='',ScoredLines=0,LastScoredDateHeader='',Error='')
    try:
        txt=get(f'https://raw.githubusercontent.com/openfootball/{repo}/master/{path}')
        (OUT / (country+'_'+league.replace(' ','_').replace('.','')+'.txt')).write_text(txt,encoding='utf-8')
        header=''
        for line in txt.splitlines():
            if re.match(r'^\s*(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)\s+[A-Z][a-z]{2}\s+\d',line): header=line.strip()
            if re.search(r'\b\d+\s*-\s*\d+\b',line) and not line.lstrip().startswith(('#','=')):
                row['ScoredLines']+=1; row['LastScoredDateHeader']=header
        row['Status']='CURRENT_FILE_WITH_SCORES' if row['ScoredLines'] else 'CURRENT_FILE_NO_SCORES'
    except urllib.error.HTTPError as e:
        row['Status']='CURRENT_FILE_MISSING' if e.code==404 else 'REQUEST_ERROR'; row['Error']=str(e)
    except Exception as e: row['Status']='REQUEST_ERROR'; row['Error']=str(e)
    try:
        commits=json.loads(get(f'https://api.github.com/repos/openfootball/{repo}/commits?path={path}&per_page=8'))
        dates=sorted(set(c['commit']['committer']['date'][:10] for c in commits),reverse=True)
        row['LastCommit']=dates[0] if dates else ''
        row['RecentCommitDates']='; '.join(dates)
        ds=[datetime.date.fromisoformat(d) for d in dates]
        gaps=sorted((a-b).days for a,b in zip(ds,ds[1:]))
        if gaps: row['MedianGapDays']=gaps[len(gaps)//2]
    except Exception as e: row['Error']+='; history: '+str(e)
    return row

if __name__=='__main__':
    results=[]
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
        for n,row in enumerate(pool.map(check,JOBS),1):
            results.append(row)
            print(f"[{n}/23] {row['Country']} {row['League']}: {row['Status']}; last edit {row['LastCommit']}; score lines {row['ScoredLines']}",flush=True)
    with (OUT/'current_routes.csv').open('w',newline='',encoding='utf-8-sig') as f:
        writer=csv.DictWriter(f,fieldnames=list(results[0]));writer.writeheader();writer.writerows(results)
    print('Report:',OUT,flush=True)
