#!/usr/bin/env python3
"""Audit the compact example against curated inputs, sequences and full-plot geometry."""
from pathlib import Path
from collections import Counter
import ast, csv, hashlib, json, re
import numpy as np
ROOT=Path(__file__).resolve().parent
B=ROOT/'results/mhc_compact'; F=B/'figures'
SOURCE=ROOT/'results/mhc_continuous'; FULL=SOURCE/'figures'
def rd(p):
    with p.open() as handle: return list(csv.DictReader(handle,delimiter='\t'))
manifest=json.loads((B/'selection.json').read_text())
tracks=manifest['tracks_top_to_bottom']; assert tracks==['SSYh1','GGOh2','GGOh1','HSAh1']
w=rd(B/'windows.tsv'); byid={r['track']:r for r in w}
raw=[r for r in rd(ROOT/'results/mhc_full/genes.tsv') if r['track'] in tracks]
plotted=rd(F/'plotted-mhc-genes.tsv'); assert len(raw)==len(plotted)==97
identity=lambda r: tuple(r[k] for k in ['track','gene_name','start','end','strand','functionality'])
assert list(map(identity,raw))==list(map(identity,plotted))
for g in plotted:
    window=byid[g['track']]; a,b=int(g['start'])-1,int(g['end']); lo,hi=int(window['start0']),int(window['end0'])
    expected=(a-lo,b-lo) if window['orientation']=='+' else (hi-b,hi-a)
    assert tuple(map(float,[g['xmin'],g['xmax']]))==expected
    assert float(g['true_midpoint'])==sum(expected)/2
    assert float(g['xmax'])-float(g['xmin'])==b-a
    assert (g['label'].endswith('*'))==(g['functionality']=='pseudogene')
# Retained rows must reproduce the full example's curated geometry and context.
for name,keys in [
 ('plotted-mhc-genes.tsv',['track','gene_name','start','end','strand','functionality','xmin','xmax','true_midpoint','head_row']),
 ('plotted-gene-arrows.tsv',['track','transcripts','xmin','xmax','strand','annotation_evidence']),
 ('plotted-native-mhc-arrows.tsv',['track','gene_name','native_start0','native_end0','xmin','xmax','strand','ortholog_fill']),
 ('native-axis-qc.tsv',['track','native_tick','display_bp']),
 ('vr-lane-qc.tsv',['track','region','ymin','ymax'])]:
    signature=lambda r: tuple(r[k] for k in keys)
    assert Counter(signature(r) for r in rd(F/name))==Counter(signature(r) for r in rd(FULL/name) if r['track'] in tracks),name
scales=rd(F/'common-bp-scale-qc.tsv'); assert len({float(r['bp_per_axis_unit']) for r in scales})==1
assert (FULL/'mhc_continuous.pdf').is_file()
assert hashlib.sha256((FULL/'mhc_continuous.pdf').read_bytes()).hexdigest()=='4be79d91440e471a5275b419bd9b56711b6238b51315daefa636a85f7928d81b'
layout=rd(F/'layout-qc.tsv')
assert [r['track'] for r in layout if r['panel_type']=='annotation']==tracks
assert sum(r['panel_type']=='link' for r in layout)==3
links=rd(B/'links.tsv'); assert len(links)==manifest['ribbon_count']==252
assert {(r['tspecies'],r['qspecies']) for r in links}==set(zip(tracks,tracks[1:]))
for row in rd(F/'link-qc.tsv'): assert row['input_blocks']==row['rendered_blocks']
# Recheck every direct PAF CIGAR against the exact display-oriented cached DNA.
sequences={t:''.join((SOURCE/(t+'.fa')).read_text().splitlines()[1:]) for t in tracks}
raw_alignments=reverse_alignments=aligned_bp=0
for alignment in manifest['alignments']:
    p=B/alignment['file']; assert hashlib.sha256(p.read_bytes()).hexdigest()==alignment['sha256']
    for role in ['target','query']:
        assert hashlib.sha256((SOURCE/(alignment[role]+'.fa')).read_bytes()).hexdigest()==alignment[role+'_fasta_sha256']
    for line in p.read_text().splitlines():
        f=line.split('\t');q=sequences[f[0]][int(f[2]):int(f[3])];t=sequences[f[5]][int(f[7]):int(f[8])]
        if f[4]=='-': q=q.translate(str.maketrans('ACGTN','TGCAN'))[::-1]; reverse_alignments+=1
        cigar=next(z[5:] for z in f[12:] if z.startswith('cg:Z:')); ops=re.findall(r'(\d+)([MID=X])',cigar)
        assert ''.join(n+op for n,op in ops)==cigar
        qi=ti=matches=0
        for n,op in ops:
            n=int(n)
            if op in 'M=X':
                qa=np.frombuffer(q[qi:qi+n].encode(),dtype=np.uint8);ta=np.frombuffer(t[ti:ti+n].encode(),dtype=np.uint8)
                assert len(qa)==len(ta)==n
                matches+=int(((qa==ta)&(qa!=ord('N'))).sum())
                if op=='=': assert np.all(qa==ta)
                if op=='X': assert np.all((qa!=ta)|(qa==ord('N')))
                qi+=n;ti+=n
            elif op=='I': qi+=n
            else: ti+=n
        assert qi==len(q) and ti==len(t) and matches==int(f[9])
        raw_alignments+=1; aligned_bp+=len(t)
# Reuse only the established independent geometric predicates, without running
# the full-figure verifier's top-level assertions against this smaller dataset.
module=ast.parse((ROOT/'verify_mhc_continuous.py').read_text())
geometry=ast.Module(body=[n for n in module.body if isinstance(n,ast.FunctionDef) and n.name in ['proper_cross','hits_open_rect']],type_ignores=[])
exec(compile(geometry,'established_geometry_predicates','exec'))
heads=rd(F/'mhc-lollipop-layout.tsv'); template={r['source_gene_key']:int(r['head_row']) for r in rd(F/'mhc-head-row-template.tsv')}
style=rd(F/'style-qc.tsv')[0]; tol=1e-6; crossings=intersections=0
for device in ['pdf','png']:
    hs=[h for h in heads if h['device']==device]; assert len(hs)==97
    for h in hs:
        assert int(h['row'])==template[h['source_gene_key']]
        assert float(h['left_mm'])>=0 and float(h['right_mm'])<=float(h['panel_width_mm'])+tol
        assert float(h['top_mm'])<=float(h['panel_height_mm'])+tol
        assert float(h['label_width_mm'])+2*float(style['padding_mm'])+float(style['tip_mm'])<=float(h['head_width_mm'])+tol
    for row in range(1,7):
        offsets=[float(h['head_y_mm'])-float(h['anchor_y_mm']) for h in hs if int(h['row'])==row]
        assert max(offsets)-min(offsets)<tol
    for track in tracks:
        group=[h for h in hs if h['track']==track]
        paths=[[(float(h['anchor_x_mm']),float(h['anchor_y_mm'])),(float(h['anchor_x_mm']),float(h['tick_top_mm'])),
                (float(h['channel_x_mm']),float(h['fan_top_mm'])),(float(h['channel_x_mm']),float(h['head_y_mm'])),
                (float(h['left_mm']),float(h['head_y_mm']))] for h in group]
        for i,path in enumerate(paths):
            for j,h in enumerate(group):
                if i==j: continue
                margin=(float(style['head_linewidth_mm'])+float(style['stem_linewidth_mm']))/2
                rect=(float(h['left_mm'])-margin,float(h['right_mm'])+margin,float(h['bottom_mm'])-margin,float(h['top_mm'])+margin)
                for a,b in zip(path,path[1:]): intersections+=hits_open_rect(a,b,rect)
            for other in paths[i+1:]:
                for a,b in zip(path,path[1:]):
                    for c,d in zip(other,other[1:]): crossings+=proper_cross(a,b,c,d)
        for i,a in enumerate(group):
            for b in group[i+1:]:
                xo=min(float(a['right_mm']),float(b['right_mm']))-max(float(a['left_mm']),float(b['left_mm']))
                yo=min(float(a['top_mm']),float(b['top_mm']))-max(float(a['bottom_mm']),float(b['bottom_mm']))
                assert min(xo,yo)+float(style['head_linewidth_mm'])<=tol
assert crossings==intersections==0,(crossings,intersections)
assert b'/Subtype /Image' not in (F/'mhc_continuous.pdf').read_bytes()
report=dict(status='PASS',species=3,annotation_tracks=4,link_panels=3,mhc_heads=97,native_mhc_arrows=97,
            ribbons=len(links),raw_alignments_audited=raw_alignments,reverse_alignments=reverse_alignments,aligned_bp_audited=aligned_bp,
            native_genomic_spans_unchanged=True,context_genes_preserved=True,shared_six_head_rows=True,
            common_bp_scale=True,leader_crossings=crossings,leader_other_head_intersections=intersections,
            head_overlaps=False,vector_pdf=True,approved_full_pdf_unchanged=True)
(F/'verification.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
