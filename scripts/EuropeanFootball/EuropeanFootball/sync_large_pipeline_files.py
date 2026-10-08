"""Pack or restore required pipeline inputs in Git-sized, checked gzip parts."""
import gzip, hashlib, json, pathlib, shutil, sys, tempfile
ROOT = pathlib.Path(__file__).resolve().parents[2]
STORE = ROOT / 'EuropeanFootball/pipeline_data/versioned_inputs'
FILES = ['EuropeanFootball/pipeline_data/Matches_Clean_Combined/european_football_all_matches.csv',
         'EuropeanFootball/pipeline_data/Elo/checkpoint_2024_12_31.rds']
LIMIT = 40 * 1024 * 1024

def digest(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for block in iter(lambda: f.read(4*1024*1024), b''): h.update(block)
    return h.hexdigest()

mode = sys.argv[1]
STORE.mkdir(parents=True, exist_ok=True)
manifest_path = STORE / 'manifest.json'
if mode == 'pack':
    manifest = []
    for relative in FILES:
        source = ROOT / relative
        print('Packing', relative, flush=True)
        with tempfile.TemporaryDirectory() as folder:
            archive = pathlib.Path(folder) / 'input.gz'
            with source.open('rb') as f, gzip.open(archive, 'wb', compresslevel=1) as z:
                shutil.copyfileobj(f, z, 4*1024*1024)
            parts = []
            with archive.open('rb') as f:
                for index in range(10000):
                    block = f.read(LIMIT)
                    if not block: break
                    name = source.name + '.part%04d.gz' % index
                    target = STORE / name
                    if not target.exists() or target.read_bytes() != block: target.write_bytes(block)
                    parts.append({'name':name, 'sha256':hashlib.sha256(block).hexdigest()})
        manifest.append({'path':relative,'sha256':digest(source),'parts':parts})
        print('  Saved', len(parts), 'parts', flush=True)
    manifest_path.write_text(json.dumps(manifest, indent=2) + '\n')
elif mode == 'restore':
    for item in json.loads(manifest_path.read_text()):
        target = ROOT / item['path']
        print('Restoring', item['path'], flush=True)
        target.parent.mkdir(parents=True, exist_ok=True)
        with tempfile.TemporaryDirectory() as folder:
            archive = pathlib.Path(folder) / 'input.gz'
            with archive.open('wb') as out:
                for part in item['parts']:
                    source = STORE / part['name']
                    if digest(source) != part['sha256']: raise RuntimeError('Corrupt part: '+part['name'])
                    with source.open('rb') as f: shutil.copyfileobj(f, out)
            temporary = target.with_name(target.name + '.restore.tmp')
            with gzip.open(archive, 'rb') as z, temporary.open('wb') as out:
                shutil.copyfileobj(z, out, 4*1024*1024)
            if digest(temporary) != item['sha256']: raise RuntimeError('Restored hash mismatch')
            temporary.replace(target)
else: raise ValueError('Use pack or restore')
