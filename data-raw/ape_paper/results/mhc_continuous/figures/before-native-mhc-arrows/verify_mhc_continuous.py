#!/usr/bin/env python3
"""Independent native-coordinate, sequence-alignment and rendering contracts."""
from pathlib import Path
import csv,json,re,hashlib
import numpy as np
ROOT=Path(__file__).resolve().parent;B=ROOT/'results/mhc_continuous';F=B/'figures'
def rd(p):return list(csv.DictReader(p.open(),delimiter='\t'))
w=rd(B/'windows.tsv');byid={z['track']:z for z in w};assert len(w)==len(byid)==13
seq={}
for z in w:
 s=''.join((B/(z['track']+'.fa')).read_text().splitlines()[1:]);seq[z['track']]=s
 assert len(s)==int(z['length'])==int(z['end0'])-int(z['start0'])
 assert z['orientation'] in ['+','-']
links=rd(B/'links.tsv');assert len(links)==624;pair_counts={}
for z in links:
 t,q=byid[z['tspecies']],byid[z['qspecies']]
 assert 1<=int(z['tstart'])<=int(z['tend'])<=int(t['length'])
 assert 1<=int(z['qstart'])<=int(z['qend'])<=int(q['length'])
 pair_counts[z['tspecies'],z['qspecies']]=pair_counts.get((z['tspecies'],z['qspecies']),0)+1
assert set(pair_counts)==set(zip(byid,list(byid)[1:])),pair_counts
# Reconstruct every original PAF CIGAR against the source-extracted sequences.
n_align=0;n_reverse=0;total_bp=0
for p in B.glob('*.paf'):
 for line in p.read_text().splitlines():
  f=line.split('\t');q=seq[f[0]][int(f[2]):int(f[3])];t=seq[f[5]][int(f[7]):int(f[8])]
  if f[4]=='-':q=q.translate(str.maketrans('ACGTN','TGCAN'))[::-1];n_reverse+=1
  cigar=next(z[5:] for z in f[12:] if z.startswith('cg:Z:'));ops=re.findall(r'(\d+)([MID=X])',cigar)
  assert ''.join(n+op for n,op in ops)==cigar
  qi=ti=matches=0
  for n,op in ops:
   n=int(n)
   if op in 'M=X':
    qa=np.frombuffer(q[qi:qi+n].encode(),dtype=np.uint8);ta=np.frombuffer(t[ti:ti+n].encode(),dtype=np.uint8)
    assert len(qa)==len(ta)==n
    same=(qa==ta)&(qa!=ord('N'));matches+=int(same.sum())
    if op=='=':assert np.all(qa==ta)
    if op=='X':assert np.all((qa!=ta)|(qa==ord('N')))
    qi+=n;ti+=n
   elif op=='I':qi+=n
   else:ti+=n
  assert qi==len(q) and ti==len(t),(p,'CIGAR extent')
  assert matches==int(f[9]),(p,'matches',matches,f[9])
  n_align+=1;total_bp+=len(t)
# Every curated source record survives native translation/reflection, including shared region gap.
raw=rd(ROOT/'results/mhc_full/genes.tsv');plotted=rd(F/'plotted-mhc-genes.tsv');assert len(raw)==len(plotted)==285
assert [(z['track'],z['gene_name'],z['start'],z['end'],z['strand'],z['functionality']) for z in raw]==[(z['track'],z['gene_name'],z['start'],z['end'],z['strand'],z['functionality']) for z in plotted]
for z in plotted:
 a,b=int(z['start'])-1,int(z['end']);window=byid[z['track']];lo,hi=int(window['start0']),int(window['end0'])
 expected=(a-lo,b-lo) if window['orientation']=='+' else (hi-b,hi-a)
 assert expected==(float(z['xmin']),float(z['xmax']))
 assert float(z['xmax'])-float(z['xmin'])==b-a
 if z['functionality']=='pseudogene':assert z['kind']=='Pseudogene' and z['label'].endswith('*')
 expected_strand=z['strand'] if window['orientation']=='+' else ('-' if z['strand']=='+' else '+')
 assert z['display_oriented_strand']==expected_strand and float(z['true_midpoint'])==sum(expected)/2
 assert z['gene_label']==z['label']
lay=rd(F/'layout-qc.tsv');assert [z['track'] for z in lay if z['panel_type']=='annotation']==list(byid)
assert sum(z['panel_type']=='link' for z in lay)==12 and not any(z['panel_type']=='coverage' for z in lay)
scales=rd(F/'common-bp-scale-qc.tsv');assert len({float(z['bp_per_axis_unit']) for z in scales})==1
geometry=rd(F/'native-geometry-qc.tsv');assert len(geometry)==13
assert all(abs(float(z['built_units_per_native_bp'])-1)<1e-12 and abs(float(z['intercept']))<1e-6 and float(z['max_affine_error'])<1e-6 for z in geometry)
for z in rd(F/'link-qc.tsv'):assert z['input_blocks']==z['rendered_blocks']==str(pair_counts[z['target'],z['query']])
cov=rd(F/'annotation-coverage.tsv');missing=[z['track'] for z in cov if int(z['source_gene_models'])==0]
assert missing==[]
projected_tracks={z['track'] for z in cov if z['annotation_evidence']=='same-species Liftoff projection'}
assert projected_tracks=={'SSYh2','PABh2','PPYh2','GGOh2','PPAh1','PTRh2'}
assert all(int(z['context_gene_arrows'])>0 for z in cov)
assert len({z['track'] for z in cov if z['annotation_evidence']=='released annotation'})==7
exons=rd(F/'source-representative-exons.tsv');assert {z['track'] for z in exons}==set(byid)
for z in exons:
 window=byid[z['track']];lo,hi=int(window['start0']),int(window['end0']);a=max(int(z['start0']),lo);b=min(int(z['end0']),hi)
 assert float(z['xmax'])-float(z['xmin'])==b-a>0
arrows=rd(F/'plotted-gene-arrows.tsv')
assert len({z['transcripts'] for z in arrows})==len(arrows)
assert all(z['kind']=='Other gene' and z['source']=='source representative context model' for z in arrows)
assert len(arrows)==sum(z['native_mhc_model']=='FALSE' for z in rd(F/'representative-models.tsv'))
assert all(z['annotation_evidence']==('same-species Liftoff projection' if z['track'] in projected_tracks else 'released annotation') for z in arrows)
assert all(float(z['projection_coverage'])>=.95 and float(z['projection_identity'])>=.95 and z['projection_source_model_id'] for z in arrows if z['track'] in projected_tracks)
for z in arrows:
 window=byid[z['track']];a=max(int(z['native_start0']),int(window['start0']));b=min(int(z['native_end0']),int(window['end0']))
 assert float(z['xmax'])-float(z['xmin'])==b-a>0
excluded=rd(F/'excluded-native-mhc-models.tsv')
assert not ({z['transcripts'] for z in arrows}&{z['model_id'] for z in excluded})
assert len(excluded)+len(arrows)==len(rd(F/'representative-models.tsv'))
for z in excluded:
 assert z['native_mhc_model']=='TRUE'
 assert z['kind']!='Other gene' or z['duplicate_curated_rows']
vr_lanes=rd(F/'vr-lane-qc.tsv');assert len({float(z['ymin']) for z in vr_lanes})==len({float(z['ymax']) for z in vr_lanes})==1
assert all(float(z['ymax'])<0 for z in vr_lanes)
assert all(float(z['y'])<min(float(r['ymin']) for r in vr_lanes) for z in rd(F/'class-spans.tsv'))
for z in rd(B/'discordant_regions.tsv'):
 window=byid[z['track']];assert int(window['start0'])<=int(z['start0'])<int(z['end0'])<=int(window['end0'])
 assert int(z['display_end0'])-int(z['display_start0'])==int(z['end0'])-int(z['start0'])
axes=rd(F/'native-axis-qc.tsv');assert {z['track'] for z in axes}==set(byid)
for z in axes:
 window=byid[z['track']];position=float(z['display_bp']);native=float(z['native_tick'])
 assert 0<=position<=int(window['length'])
 expected=position+int(window['start0'])+1 if window['orientation']=='+' else int(window['end0'])-position
 assert expected==native and z['chr']==window['chr']
style=rd(F/'style-qc.tsv')[0];assert style['gene_geom']=='geom_gene' and style['mhc_geom']=='geom_gene_lollipop' and style['vr_shared_y']=='TRUE'
assert style['strip_side']=='left' and style['panel_border']=='FALSE' and style['strip_background']=='FALSE'
# Every enlarged head is matched to an unchanged midpoint from the curated table.
anchors=rd(F/'mhc-anchors.tsv');assert len(anchors)==285
palette={}
for line in (ROOT/'source_metadata/mhc_continuous/colors.txt').read_text().splitlines():
 name,rgb=line.split();palette[name]='#'+''.join(f'{round(float(v)):02x}' for v in rgb.split(',')[:3])
for a,z in zip(anchors,plotted):
 assert a['track']==z['track'] and a['label']==z['label'] and a['strand']==z['display_oriented_strand']
 assert float(a['x'])==float(z['true_midpoint']) and float(a['y'])==0
 assert a['fill'].lower()==palette['p' if z['kind']=='Pseudogene' else z['kind']]
layout=rd(F/'mhc-lollipop-layout.tsv')
assert set(z['device'] for z in layout)=={'pdf','png'}
# Draw-time geometry is checked independently on each physical output device.
def proper_cross(a,b,c,d):
 def orient(p,q,r):return (q[0]-p[0])*(r[1]-p[1])-(q[1]-p[1])*(r[0]-p[0])
 return orient(a,b,c)*orient(a,b,d)<-1e-12 and orient(c,d,a)*orient(c,d,b)<-1e-12

def hits_open_rect(a,b,rect):
 # Liang-Barsky clipping against the open, outline-expanded rectangle.
 low,high=0.,1.
 for pos,delta,mn,mx in [(a[0],b[0]-a[0],rect[0],rect[1]),(a[1],b[1]-a[1],rect[2],rect[3])]:
  if abs(delta)<1e-12:
   if pos<=mn+1e-8 or pos>=mx-1e-8:return False
  else:
   t1,t2=(mn-pos)/delta,(mx-pos)/delta
   low=max(low,min(t1,t2));high=min(high,max(t1,t2))
   if high-low<=1e-9:return False
 return high-low>1e-9

assert style['head_layout']=='columns'
assert style['head_rows']=='fixed by source gene key'
template=rd(F/'mhc-head-row-template.tsv')
row_by_key={z['source_gene_key']:int(z['head_row']) for z in template}
assert len(row_by_key)==len(template)==41
assert set(row_by_key)=={z['source_gene_key'] for z in plotted}
assert set(row_by_key.values())==set(range(1,7))
assert float(style['context_top_y'])==0 and all(float(z['ymin'])==float(style['context_bottom_y']) for z in arrows)
assert max(float(z['ymax']) for z in vr_lanes)<float(style['context_bottom_y'])
height_qc=rd(F/'annotation-height-qc.tsv');assert {z['track'] for z in height_qc}==set(byid)
prior=rd(F/'before-tracing-polish/mhc-lollipop-layout.tsv')
prior_lookup={(z['device'],z['track'],z['label'],float(z['genomic_x'])):z for z in prior}
leader_head_intersections=leader_crossings=0
head_widths={};max_rows={};tol=1e-6
for device in ['pdf','png']:
 heads=[z for z in layout if z['device']==device];assert len(heads)==285,(device,len(heads))
 assert {z['track'] for z in heads}==set(byid)
 assert len({round(float(z['head_width_mm']),6) for z in heads})==1
 assert len({round(float(z['head_height_mm']),6) for z in heads})==1
 assert all(abs(float(z['head_height_mm'])-4)<tol for z in heads)
 head_widths[device]=float(heads[0]['head_width_mm']);max_rows[device]=max(int(z['row']) for z in heads)
 lookup={(z['track'],z['label'],float(z['true_midpoint'])):z for z in plotted}
 assert len(lookup)==285
 for h in heads:
  key=(h['track'],h['label'],float(h['genomic_x']));assert key in lookup,key
  original=lookup.pop(key)
  expected_dir=1 if original['display_oriented_strand']=='+' else -1
  assert float(h['direction'])==expected_dir
  assert h['head_layout']=='columns'
  assert h['row_is_fixed']=='TRUE'
  assert h['source_gene_key']==original['source_gene_key']
  assert int(h['row'])==row_by_key[h['source_gene_key']]==int(original['head_row'])
  old=prior_lookup[(device,h['track'],h['label'],float(h['genomic_x']))]
  assert abs(float(old['anchor_x_mm'])/float(old['panel_width_mm'])-float(h['anchor_x_mm'])/float(h['panel_width_mm']))<1e-7
  base_color=palette['p' if original['kind']=='Pseudogene' else original['kind']]
  dark='#'+''.join(f'{round(int(base_color[k:k+2],16)*.6):02X}' for k in [1,3,5])
  assert h['stem_colour']==dark,(h['label'],h['stem_colour'],dark)
  assert abs(float(h['tick_top_mm'])-float(h['anchor_y_mm'])-float(style['anchor_tick_mm']))<tol
  assert abs(float(h['fan_top_mm'])-float(h['tick_top_mm'])-float(style['fan_height_mm']))<tol
  assert float(h['channel_x_mm'])<float(h['left_mm'])
  assert float(h['fan_top_mm'])<float(h['bottom_mm'])
  assert abs(float(h['head_x_mm'])-float(h['anchor_x_mm']))<=float(style['max_shift_mm'])+tol
  assert float(h['left_mm'])>=float(style['head_linewidth_mm'])/2-tol and float(h['right_mm'])<=float(h['panel_width_mm'])-float(style['head_linewidth_mm'])/2+tol
  assert float(h['bottom_mm'])>=float(h['anchor_y_mm'])+1.5-tol
  assert float(h['top_mm'])<=float(h['panel_height_mm'])-float(style['head_linewidth_mm'])/2+tol
  assert abs(float(h['right_mm'])-float(h['left_mm'])-float(h['head_width_mm']))<tol
  assert abs(float(h['top_mm'])-float(h['bottom_mm'])-float(h['head_height_mm']))<tol
  assert float(h['label_width_mm'])+2*float(style['padding_mm'])+float(style['tip_mm'])<=float(h['head_width_mm'])+tol
 assert not lookup
 # Same keys, statuses and copy occurrences use one shared row; every row has
 # one physical offset above the true anchor across all haplotype panels.
 for gene_key,expected_row in row_by_key.items():
  assert {int(h['row']) for h in heads if h['source_gene_key']==gene_key}=={expected_row}
 for row in range(1,7):
  offsets=[float(h['head_y_mm'])-float(h['anchor_y_mm']) for h in heads if int(h['row'])==row]
  assert max(offsets)-min(offsets)<tol,(device,row,offsets)
 for track in byid:
  hs=[h for h in heads if h['track']==track]
  ordered=sorted(hs,key=lambda h:int(h['display_order']))
  assert [int(h['display_order']) for h in ordered]==list(range(1,len(hs)+1))
  assert all(float(a['anchor_x_mm'])<=float(b['anchor_x_mm'])+tol for a,b in zip(ordered,ordered[1:]))
  assert all(float(a['channel_x_mm'])<float(b['channel_x_mm']) for a,b in zip(ordered,ordered[1:]))
  for col in {h['column'] for h in hs}:
   group=[h for h in ordered if h['column']==col]
   assert len({round(float(h['head_x_mm']),6) for h in group})==1
   assert all(float(a['head_y_mm'])>float(b['head_y_mm']) for a,b in zip(group,group[1:]))
  paths=[[(float(h['anchor_x_mm']),float(h['anchor_y_mm'])),
          (float(h['anchor_x_mm']),float(h['tick_top_mm'])),
          (float(h['channel_x_mm']),float(h['fan_top_mm'])),
          (float(h['channel_x_mm']),float(h['head_y_mm'])),
          (float(h['left_mm']),float(h['head_y_mm']))] for h in hs]
  for i,path in enumerate(paths):
   for j,other in enumerate(hs):
    if i==j:continue
    margin=(float(style['head_linewidth_mm'])+float(style['stem_linewidth_mm']))/2
    rect=(float(other['left_mm'])-margin,float(other['right_mm'])+margin,
          float(other['bottom_mm'])-margin,float(other['top_mm'])+margin)
    for a,b in zip(path,path[1:]):leader_head_intersections+=hits_open_rect(a,b,rect)
   for other in paths[i+1:]:
    for a,b in zip(path,path[1:]):
     for c,d in zip(other,other[1:]):leader_crossings+=proper_cross(a,b,c,d)
  for i,a in enumerate(hs):
   for b in hs[i+1:]:
    x_overlap=min(float(a['right_mm']),float(b['right_mm']))-max(float(a['left_mm']),float(b['left_mm']))
    y_overlap=min(float(a['top_mm']),float(b['top_mm']))-max(float(a['bottom_mm']),float(b['bottom_mm']))
    assert min(x_overlap,y_overlap)+float(style['head_linewidth_mm'])<=tol,(device,track,a['label'],b['label'],'overlapping head outlines')
    if a['row']!=b['row'] and x_overlap>0:
     assert -y_overlap>=float(style['head_row_gap_mm'])+float(style['head_linewidth_mm'])-tol
  for row in {h['row'] for h in hs}:
   row_heads=sorted([h for h in hs if h['row']==row],key=lambda h:float(h['genomic_x']))
   for a,b in zip(row_heads,row_heads[1:]):
    assert float(a['head_x_mm'])<=float(b['head_x_mm'])+tol
    assert float(b['left_mm'])-float(a['right_mm'])>=float(style['head_gap_mm'])+float(style['head_linewidth_mm'])-tol
assert leader_crossings==leader_head_intersections==0,(leader_crossings,leader_head_intersections)
for ext in ['pdf','png']:assert (F/f'mhc_continuous.{ext}').stat().st_size>1000
assert b'/Subtype /Image' not in (F/'mhc_continuous.pdf').read_bytes()
report=dict(status='PASS',native_tracks=13,source_exon_tracks=13,released_annotation_tracks=7,projected_context_tracks=sorted(projected_tracks),curated_only_tracks=missing,curated_mhc_genes=285,representative_models=sum(int(z['source_gene_models']) for z in cov),display_ribbons=len(links),raw_alignments_audited=n_align,reverse_alignments=n_reverse,aligned_target_bp_audited=total_bp,common_bp_scale=True,vector_pdf=True,lollipop_heads=285,context_native_gene_arrows=len(arrows),head_width_mm_by_device=head_widths,max_head_rows_by_device=max_rows,head_overlap=False,heads_inside_panels=True,labels_inside_heads=True,max_shift_mm=float(style['max_shift_mm']),ordered_columns=True,leader_crossings=leader_crossings,leader_other_head_intersections=leader_head_intersections,genomic_anchors_unchanged=True,shared_gene_rows=True,shared_row_count=6,source_gene_keys_with_fixed_rows=len(row_by_key))
(F/'verification.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
