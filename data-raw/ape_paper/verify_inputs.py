#!/usr/bin/env python3
"""Scientific data contract checks, independent of ggexon rendering."""
from pathlib import Path
import csv,json,hashlib
ROOT=Path(__file__).resolve().parent

def table(p):return list(csv.DictReader(p.open(),delimiter='\t'))
reports=[]
availability=table(ROOT/'results/data_availability.tsv')
for case in ['mhc']:
    folder=ROOT/'results'/case
    windows=table(folder/'windows.tsv');byid={w['id']:w for w in windows}
    nfeatures=0
    for w in windows:
        a,z=int(w['start0']),int(w['end0'])
        seq=''.join((folder/(w['id']+'.fa')).read_text().splitlines()[1:]).upper()
        assert len(seq)==z-a,(case,w['id'],'FASTA length')
        assert not set(seq)-set('ACGTN'),(case,w['id'],'FASTA bases')
        rows=[r.split('\t') for r in (folder/(w['id']+'.gff3')).read_text().splitlines() if not r.startswith('#')]
        assert rows
        for r in rows:
            assert r[0]==w['chrom'] and int(r[3])<=int(r[4])
            assert int(r[3])<=z and int(r[4])>a,(case,w['id'],'gene outside window',r)
        nfeatures+=len(rows)
        signal=table(folder/(w['id']+'.methylation.tsv'))
        if not signal:
            assert any(r['example']==case and r['individual']==w['id'] and r['assay']=='methylation' and r['status']=='unavailable' for r in availability), (case,w['id'],'Undocumented missing assay')
        for r in signal:
            assert a<int(r['xmin'])<=int(r['xmax'])<=z
            assert 0<=float(r['coverage'])<=1 and int(r['observed_bases'])>0
    links=table(folder/'links.tsv');assert links
    for r in links:
        for prefix in ['t','q']:
            w=byid[r[prefix+'species']]
            assert r[prefix+'chr']==w['chrom']
            assert int(w['start0'])<int(r[prefix+'start'])<=int(r[prefix+'end'])<=int(w['end0'])
        assert r['strand'] in ['+','-'] and 0<=float(r['identity'])<=100
        assert 0<int(r['nmatch'])<=int(r['alen'])
    features=table(folder/'features.tsv')
    for r in features:
        w=byid[r['track']]
        assert r['chr']==w['chrom'] and int(w['start0'])<int(r['xmin'])<=int(r['xmax'])<=int(w['end0'])
    reports.append(dict(example=case,genomic_windows=len(windows),gff_records=nfeatures,nucleotide_links=len(links),feature_intervals=len(features),checks='PASS'))

for p in ROOT.glob('manifest-*.json'):
    for record in json.loads(p.read_text()):
        f=ROOT/record['file'];assert f.exists()
        assert hashlib.sha256(f.read_bytes()).hexdigest()==record['sha256'],f

(ROOT/'results/verification.json').write_text(json.dumps(dict(status='PASS',examples=reports,coordinate_convention='Plotting TSV/GFF: 1-based closed; raw BED/PAF: 0-based half-open'),indent=2)+'\n')
for r in reports:print(r)
print('PASS: all signal, feature, sequence, link-coordinate and manifest checks')
