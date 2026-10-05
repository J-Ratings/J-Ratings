"""Stream out corrupt repeated quote decorations, preserving CSV structure."""
import csv, os, pathlib, sys, datetime
ROOT=pathlib.Path(__file__).resolve().parents[2]
path=ROOT/'EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv'
tmp=path.with_name(path.name+'.compact.tmp')
size=path.stat().st_size; done=0; quotes=0; runs=0; removed=0
def emit_run(out,n):
    global runs,removed
    if n>=16:
        replacement=b'"' if n%2 else b'""'
        out.write(replacement);runs+=1;removed+=n-len(replacement)
    else: out.write(b'"'*n)
print('[1/3] Streaming quote repair; master unchanged until validation.',flush=True)
import re
with path.open('rb') as src,tmp.open('wb') as out:
    while True:
        block=src.read(4*1024*1024)
        if not block:break
        for token in re.finditer(b'"+|[^"]+',block):
            value=token.group()
            if value[0]==34:quotes+=len(value)
            else:
                if quotes:emit_run(out,quotes);quotes=0
                out.write(value)
        done+=len(block)
        if done//(512*1024*1024)!=(done-len(block))//(512*1024*1024):
            print(f'  Read {done/size:.0%}; compact output {out.tell()/1024**2:.1f} MB',flush=True)
    if quotes:emit_run(out,quotes)
print('[2/3] Validating repaired CSV...',flush=True)
csv.field_size_limit(50_000_000)
count=0
with tmp.open(encoding='utf-8-sig',newline='') as f:
    reader=csv.reader(f);header=next(reader)
    for row in reader:
        if len(row)!=len(header):raise RuntimeError(f'CSV field count mismatch at row {count+2}; master unchanged')
        count+=1
        if count and count%250000==0:print(f'  Validated {count:,} rows',flush=True)
if not count:raise RuntimeError('Empty repaired file')
if not runs:
    tmp.unlink();print('No expanded quote runs found; unchanged.');sys.exit(0)
print(f'[3/3] Validated {count:,} rows; removed {removed:,} quote bytes from {runs} runs.',flush=True)
# Rename preserves the original as a backup without copying another 7 GB.
stamp=datetime.datetime.now().strftime('%Y%m%d_%H%M%S')
backup=path.with_name(path.stem+'_before_stream_quote_compaction_'+stamp+'.csv')
path.rename(backup)
try:os.replace(tmp,path)
except Exception:
    backup.rename(path);raise
print('Master compacted. Backup:',backup,flush=True)
