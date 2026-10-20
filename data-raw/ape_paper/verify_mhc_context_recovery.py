#!/usr/bin/env python3
"""Independent source/GFF/native-coordinate/DNA and fixed-layout recovery audit."""
from pathlib import Path
import collections,csv,hashlib,json
import parasail
R=Path(__file__).resolve().parent;B=R/'results/mhc_continuous';P=B/'gene_models_with_projected_context';O=B/'gene_models_with_recovered_context'
def rd(p):return list(csv.DictReader(p.open(),delimiter='\t'))
w={r['track']:r for r in rd(B/'windows.tsv')};oldm=rd(P/'models.tsv');olde=rd(P/'exons.tsv');m=rd(O/'models.tsv');e=rd(O/'exons.tsv')
assert [{k:r[k] for k in oldm[0]} for r in m[:len(oldm)]]==oldm
assert [{k:r[k] for k in olde[0]} for r in e[:len(olde)]]==olde
new=m[len(oldm):];newe=e[len(olde):];assert len(new)==5 and len(newe)==15
manifest=json.loads((O/'recovery_manifest.json').read_text())
for f,h in manifest['input_hashes'].items():assert hashlib.sha256(Path(f).read_bytes()).hexdigest()==h
seq={t:''.join((B/(t+'.fa')).read_text().splitlines()[1:]).upper() for t in w}
byid={r['model_id']:r for r in m};ebyid=collections.defaultdict(list)
for r in e:ebyid[r['model_id']].append(r)
def spliced(mid):
    r=byid[mid];win=w[r['track']];ss=seq[r['track']]
    if win['orientation']=='-':ss=ss.translate(str.maketrans('ACGTN','TGCAN'))[::-1]
    s=''.join(ss[int(ex['start0'])-int(win['start0']):int(ex['end0'])-int(win['start0'])] for ex in sorted(ebyid[mid],key=lambda ex:int(ex['start0'])))
    return s if r['strand']=='+' else s.translate(str.maketrans('ACGTN','TGCAN'))[::-1]
qc=[]
for c in rd(O/'recovered-models.tsv'):
    mid=f'{c["target"]}:recovered:{c["gene_symbol"]}';r=byid[mid];src=byid[r['projection_source_model_id']];win=w[r['track']]
    assert src['species_id']==r['species_id'] and src['track']!=r['track']
    assert src['strand']==r['strand'] and src['gene_symbol']==r['gene_symbol']
    assert min(float(r['projection_coverage']),float(r['projection_identity']),float(r['recovery_global_identity']))>=.95
    assert c['expected_start0'] and int(c['expected_start0'])-5000<=int(r['chromStart'])<int(r['chromEnd'])<=int(c['expected_end0'])+5000
    gff=Path(r['recovery_raw_gff']);assert hashlib.sha256(gff.read_bytes()).hexdigest()==c['raw_gff_sha256']
    blocks=[]
    for l in gff.read_text().splitlines():
        if l.startswith('#') or not l:continue
        f=l.split('\t');assert f[0]==r['track'];a,b=int(f[3])-1,int(f[4])
        if win['orientation']=='+':a,b=a+int(win['start0']),b+int(win['start0']);st=f[6]
        else:a,b=int(win['end0'])-b,int(win['end0'])-a;st='+' if f[6]=='-' else '-'
        if f[2]=='gene':assert (a,b,st)==(int(r['chromStart']),int(r['chromEnd']),r['strand'])
        if f[2]=='exon':blocks.append((a,b,st))
    assert sorted(set(blocks))==sorted((int(x['start0']),int(x['end0']),x['strand']) for x in ebyid[mid])
    sizes=list(map(int,r['blockSizes'].strip(',').split(',')));offsets=list(map(int,r['blockStarts'].strip(',').split(',')));a=int(r['chromStart'])
    assert [(x,y) for x,y,st in sorted(set(blocks))]==[(a+o,a+o+n) for o,n in zip(offsets,sizes)]
    q,t=spliced(src['model_id']),spliced(mid);aln=parasail.nw_stats_striped_32(q,t,5,1,parasail.matrix_create('ACGTN',2,-3));assert not aln.saturated
    identity=aln.matches/aln.length;assert identity>=.95 and abs(identity-float(r['recovery_global_identity']))<1e-12
    qc.append(dict(track=r['track'],gene_symbol=r['gene_symbol'],independent_global_identity=identity,native_start0=r['chromStart'],native_end0=r['chromEnd']))
audit=rd(O/'recovery-audit.tsv');assert len(audit)==len({(r['track'],r['gene_symbol']) for r in audit})==14
assert sum(r['status']=='recovered_strict_alternative_model' for r in audit)==5
assert sum(r['status']=='unresolved_complete_model' for r in audit)==9
F=B/'figures';archive=F/'before-unresolved-recovery'
for f in ['mhc-anchors.tsv','mhc-head-row-template.tsv','native-axis-qc.tsv','common-bp-scale-qc.tsv','native-geometry-qc.tsv','link-qc.tsv']:
    assert rd(F/f)==rd(archive/f),f
assert len(rd(F/'plotted-gene-arrows.tsv'))==len(rd(archive/'plotted-gene-arrows.tsv'))+5
report=dict(status='PASS',unchanged_existing_models=len(oldm),unchanged_existing_exons=len(olde),recovered_alternative_models=len(new),new_exons=len(newe),remaining_unresolved=9,all_cases_audited=14,all_curated_head_anchors_rows_and_native_axes_unchanged=True,recoveries=qc)
(O/'verification.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
