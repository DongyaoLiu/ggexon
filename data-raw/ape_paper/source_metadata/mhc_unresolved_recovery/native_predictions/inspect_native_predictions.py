#!/usr/bin/env python3
"""Audit native Augustus candidates against CIGAR-resolved same-species synteny.
No labels or gene models are changed. All intervals in exported evidence are BED0.
"""
from pathlib import Path
import csv, json, re
ROOT=Path(__file__).resolve().parents[3]
R=ROOT/'results/mhc_continuous'
N=ROOT/'source_metadata/mhc_context_search/ncbi'
OUT=Path(__file__).resolve().parent
read=lambda p:list(csv.DictReader(p.open(),delimiter='\t'))
windows={x['track']:x for x in read(R/'windows.tsv')}
models=read(R/'figures/before-alt-context/representative-models.tsv')
byid={x['model_id']:x for x in models}
failed=[x for x in read(R/'gene_models_with_projected_context/projection-audit.tsv') if x['status']!='accepted']
probe={x['track']:x for x in json.loads((N/'genark_native_annotation_probe.json').read_text())}
def native(track,a,b):
 w=windows[track]
 return (int(w['end0'])-b,int(w['end0'])-a) if w['orientation']=='-' else (int(w['start0'])+a,int(w['start0'])+b)
def display(track,a,b):
 w=windows[track]
 return (int(w['end0'])-b,int(w['end0'])-a) if w['orientation']=='-' else (a-int(w['start0']),b-int(w['start0']))
def union(xs):
 out=[]
 for a,b in sorted(xs):
  if out and a<=out[-1][1]:out[-1]=(out[-1][0],max(b,out[-1][1]))
  elif b>a:out.append((a,b))
 return out
def overlap(xs,ys):return sum(max(0,min(b,d)-max(a,c)) for a,b in union(xs) for c,d in union(ys))
def exons(m):
 a=int(m['chromStart'])
 return [(a+s,a+s+n) for s,n in zip(map(int,m['blockStarts'].strip(',').split(',')),map(int,m['blockSizes'].strip(',').split(',')))]
def paf_segments(source,target):
 p=next(p for p in R.glob('*.paf') if set(p.stem.split('__'))=={source,target})
 out=[]
 for ln,l in enumerate(p.read_text().splitlines(),1):
  f=l.split('\t');q,ql,qs,qe,orient,t,tl,ts,te=f[:9]
  if orient!='+': continue # inverted minor/repetitive alignments are not trusted anchors
  cig=next(x[5:] for x in f[12:] if x.startswith('cg:Z:'))
  qi,ti=int(qs),int(ts)
  for ns,op in re.findall(r'(\d+)([MIDNSHP=X])',cig):
   n=int(ns)
   if op in '=XM':
    a,b,c,d=(qi,qi+n,ti,ti+n) if q==source else (ti,ti+n,qi,qi+n)
    out.append((a,b,c,d,op,ln,int(f[11])))
   if op in '=XMI':qi+=n
   if op in '=XMDN':ti+=n
 return p.name,out
beds={}
for track in {f['track'] for f in failed}:
 arr=[]
 for l in (N/f'{track}.genark.augustus.bed').read_text().splitlines():
  f=l.split('\t');m=dict(zip(['chrom','chromStart','chromEnd','name','score','strand','thickStart','thickEnd','itemRgb','blockCount','blockSizes','blockStarts'],f))
  m['exons']=exons(m);m['gene_id']=f[12];m['cds_start_stat']=f[13];m['cds_end_stat']=f[14]
  arr.append(m)
 beds[track]=arr
records=[]
for fail in failed:
 src=byid[fail['source_model_id']];track=fail['track'];source=fail['source_track']
 p,segs=paf_segments(source,track)
 sx=exons(src);dx=[display(source,a,b) for a,b in sx];projected=[];matched=[];identical=[]
 for a,b in dx:
  for s,e,t,u,op,ln,mapq in segs:
   l,h=max(a,s),min(b,e)
   if l<h:
    na,nb=native(track,t+l-s,t+h-s);projected.append((na,nb));matched.append((l,h))
    if op=='=': identical.append((l,h))
 px=union(projected)
 # Closest aligned flanks define the search locus even for source exons fully in a gap.
 lo=min(a for a,b in dx);hi=max(b for a,b in dx)
 left=max((s for s in segs if s[1]<=lo),key=lambda x:x[1],default=None)
 right=min((s for s in segs if s[0]>=hi),key=lambda x:x[0],default=None)
 bracket=[]
 if left: bracket.append(native(track,left[3]-1,left[3])[0])
 if right: bracket.append(native(track,right[2],right[2]+1)[0])
 span=(min(a for a,b in px),max(b for a,b in px)) if px else (min(bracket),max(bracket)+1) if bracket else (0,0)
 sxlen=sum(b-a for a,b in sx)
 expected_strand=src['strand'] # region displays share orientation within each species
 candidates=[]
 for m in beds[track]:
  a,b=int(m['chromStart']),int(m['chromEnd'])
  if b<span[0]-25000 or a>span[1]+25000:continue
  ov=overlap(px,m['exons']);clen=sum(b-a for a,b in m['exons']);dist=max(0,span[0]-b,a-span[1])
  c=dict(m);c.update(projected_exonic_overlap_bp=ov,projected_fraction=ov/max(1,sum(b-a for a,b in px)),candidate_fraction=ov/max(1,clen),distance_to_projected_span_bp=dist,same_strand=m['strand']==expected_strand)
  candidates.append(c)
 candidates.sort(key=lambda c:(c['same_strand'],c['projected_exonic_overlap_bp'],-c['distance_to_projected_span_bp']),reverse=True)
 tr=next(t for t in probe[track]['tracks'] if t['name']=='augustus')
 records.append(dict(track=track,gene_symbol=fail['gene_symbol'],source_track=source,source_model_id=fail['source_model_id'],source_native_chrom=src['chrom'],source_native_start0=int(src['chromStart']),source_native_end0=int(src['chromEnd']),source_strand=src['strand'],source_exons_native_bed0=sx,source_exonic_bp=sxlen,target_native_chrom=probe[track]['genbank_chrom'],target_browser_chrom=probe[track]['browser_chrom'],paf_file=p,syntenic_search_start0=span[0],syntenic_search_end0=span[1],closest_aligned_flanks_native_bed0=bracket,paf_projected_exons_native_bed0=px,paf_source_exonic_coverage=len([]) if not matched else sum(b-a for a,b in union(matched))/sxlen,paf_source_identical_fraction=sum(b-a for a,b in union(identical))/sxlen,source_url=tr['url'],candidates=candidates))
(OUT/'native_candidate_evidence.json').write_text(json.dumps(records,indent=2)+'\n')
fields=['track','gene_symbol','source_track','source_model_id','source_exonic_bp','paf_source_exonic_coverage','paf_source_identical_fraction','target_native_chrom','syntenic_search_start0','syntenic_search_end0','projected_start0','projected_end0','candidate_rank','prediction_id','prediction_gene_id','candidate_start0','candidate_end0','strand','blockCount','exons_native_bed0','same_strand','projected_exonic_overlap_bp','projected_fraction','candidate_fraction','distance_to_projected_span_bp','cds_start_stat','cds_end_stat','source_url']
with (OUT/'native_candidate_evidence.tsv').open('w') as f:
 w=csv.DictWriter(f,fields,delimiter='\t');w.writeheader()
 for r in records:
  for rank,c in enumerate(r['candidates'],1):
   row={k:r[k] for k in fields if k in r};px=r['paf_projected_exons_native_bed0'];row.update(projected_start0=min((a for a,b in px),default=''),projected_end0=max((b for a,b in px),default=''),candidate_rank=rank,prediction_id=c['name'],prediction_gene_id=c['gene_id'],candidate_start0=c['chromStart'],candidate_end0=c['chromEnd'],exons_native_bed0=json.dumps(c['exons'],separators=(',',':')))
   row.update({k:c[k] for k in fields if k in c});w.writerow(row)
for r in records:
 print(r['track'],r['gene_symbol'],'source exonic',r['source_exonic_bp'],'mapped',round(r['paf_source_exonic_coverage'],4),'identity',round(r['paf_source_identical_fraction'],4))
 for c in r['candidates'][:5]: print('  ',c['name'],c['chromStart'],c['chromEnd'],c['strand'],c['blockCount'],'overlap',c['projected_exonic_overlap_bp'],'projected_frac',round(c['projected_fraction'],3),'cand_frac',round(c['candidate_fraction'],3))
# Check whether a prediction spans exons homologous to multiple named source genes.
for r in records:
 _,segs=paf_segments(r['source_track'],r['track'])
 for c in r['candidates']:
  neighbors=[]
  da,db=display(r['track'],int(c['chromStart']),int(c['chromEnd']))
  local=[s for s in segs if s[3]>=da and s[2]<=db]
  for m in models:
   if m['track']!=r['source_track']:continue
   projected_m=[]
   for na,nb in exons(m):
    a,b=display(m['track'],na,nb)
    for s,e,t,u,op,ln,mapq in local:
     l,h=max(a,s),min(b,e)
     if l<h:projected_m.append(native(r['track'],t+l-s,t+h-s))
   n=overlap(c['exons'],projected_m)
   if n:neighbors.append(dict(gene_symbol=m['gene_symbol'],source_model_id=m['model_id'],exonic_overlap_bp=n))
  c['overlapping_source_gene_exons']=sorted(neighbors,key=lambda x:x['exonic_overlap_bp'],reverse=True)
(OUT/'native_candidate_evidence.json').write_text(json.dumps(records,indent=2)+'\n')
for r in records:
 for c in r['candidates']:
  if c['projected_exonic_overlap_bp'] or (r['gene_symbol']=='OR2J2'):
   print(r['track'],r['gene_symbol'],c['name'],'source gene overlaps',c['overlapping_source_gene_exons'])
