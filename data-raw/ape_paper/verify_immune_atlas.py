#!/usr/bin/env python3
"""Independent checks of source-derived coordinates, counts and exported facets."""
import csv,json,hashlib,collections
from pathlib import Path
R=Path(__file__).resolve().parent;B=R/'results';O=B/'immune_atlas'
def rd(p):
    with p.open() as f:return list(csv.DictReader(f,delimiter='\t'))
g=rd(B/'igtr/paper_genes.tsv');w=rd(B/'igtr/loci.tsv');a=rd(B/'igtr/alignments.tsv');sv=rd(B/'igtr/sv_blocks.tsv')
windows={(z['locus'],z['track_id']):z for z in w}
assert len(g)==3738 and len(w)==54 and len(a)==370 and len(sv)==961
for z in g:
    q=windows[z['locus'],z['track_id']];start,end=int(z['start']),int(z['end']);length=int(q['length'])
    assert 1<=start<=end<=length
    expected=(start,end) if q['display_orientation']=='1' else (length-end+1,length-start+1)
    assert expected==(int(z['display_start']),int(z['display_end']))
    assert int(z['genomic_start'])-int(q['genomic_start0'])==start
    assert int(z['genomic_end'])-int(q['genomic_start0'])==end
for s in json.loads((R/'source_metadata/igtr/manifest.json').read_text())['sources']:
    p=R/'source_metadata/igtr'/s['path'];assert hashlib.sha256(p.read_bytes()).hexdigest()==s['sha256'],p
ids=['PPA1','PPA2','HSA1','GGO1','GGO2','PAB1','PAB2','PPY1','PPY2']
for z in a:assert abs(ids.index(z['track1'])-ids.index(z['track2']))==1
m=rd(B/'mhc_full/genes_ed3_scope.tsv');common=[z for z in m if z['track'].replace('h','') in ids]
assert len(m)==269 and len(common)==191
loci=['IGH','IGK','IGL','TRA','TRB','TRG','MHC_I','MHC_II']
for locus in loci:
    shown=rd(O/f'{locus}.plotted-genes.tsv');layout=rd(O/f'{locus}.layout-qc.tsv')
    expect=[z for z in (common if locus.startswith('MHC') else g) if z['locus']==locus]
    assert len(shown)==len(expect),(locus,len(shown),len(expect))
    assert [z['track'] for z in layout if z['panel_type']=='annotation']==ids
    assert set(z['panel_type'] for z in layout)<= {'annotation','link'}
    assert sum(z['panel_type']=='link' for z in layout)==(0 if locus.startswith('MHC') else 8)
    for ext in ['pdf','png']:assert (O/f'{locus}.{ext}').stat().st_size>1000
    f=O/f'{locus}.lane-qc.tsv'
    if f.exists():
        for z in rd(f):assert z['owner_match']=='TRUE' and z['below_gene']=='TRUE'
link_total=0
for locus in loci[:6]:
    for z in rd(O/f'{locus}.link-qc.tsv'):
        assert z['owner_match']=='TRUE' and z['input_blocks']==z['rendered_blocks']
        link_total+=int(z['rendered_blocks'])
assert link_total==len(a)
for locus in ['MHC_I','MHC_II']:
    layout=rd(O/f'{locus}_all_species.layout-qc.tsv');assert len(layout)==13
    assert len(rd(O/f'{locus}_all_species.plotted-genes.tsv'))==sum(z['locus']==locus for z in m)
for z in rd(O/'functional_gene_counts.tsv'):
    if z['group'].startswith('MHC'):
        expected=sum(r['track'].replace('h','')==z['track'] and r['locus']==z['group'].replace(' ','_') and r['functionality']=='functional' for r in m)
    elif z['track']=='HSA1':assert z['count']=='NA';continue
    else:
        families={'TRAV','TRAV/DV','TRDV'} if z['group']=='TRAV + TRDV' else {z['group']}
        expected=sum(r['track_id']==z['track'] and r['gene_family'] in families and r['functional']=='Functional' for r in g)
    assert float(z['count'])==expected,z
for stem in ['immune_combined','functional_gene_counts']:
    for ext in ['pdf','png']:assert (O/f'{stem}.{ext}').stat().st_size>1000
# Dedicated removed examples must not survive in the active project.
assert not (B/'adcyap1').exists() and not (B/'duplication').exists()
report=dict(status='PASS',igtr_genes=len(g),mhc_genes=len(m),common_mhc_genes=len(common),alignment_blocks=len(a),sv_blocks=len(sv),genomic_facets=8,nominal_loci=9,common_annotation_panels=72)
(O/'verification.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
