#!/usr/bin/env python3
"""Independent checks of original fields, raw GFF coordinates and exon DNA.

Requires the same isolated Liftoff runtime for parasail. The sequence comparison
re-extracts spliced DNA from source/target regional FASTAs and globally aligns it
independently of Liftoff's genomic alignment. Results are diagnostic; acceptance
uses the documented Liftoff exon coverage and identity thresholds.
"""
from pathlib import Path
import collections,csv,hashlib,json
from urllib.parse import unquote
import parasail
R=Path(__file__).resolve().parent;B=R/'results/mhc_continuous';P=B/'gene_models_with_projected_context'
def rd(p):return list(csv.DictReader(p.open(),delimiter='\t'))
def rev(s):return s.translate(str.maketrans('ACGTN','TGCAN'))[::-1]
w={r['track']:r for r in rd(B/'windows.tsv')}
m=rd(P/'models.tsv');e=rd(P/'exons.tsv');oldm=rd(B/'gene_models/models.tsv');olde=rd(B/'gene_models/exons.tsv')
assert [{k:r[k] for k in oldm[0]} for r in m[:len(oldm)]]==oldm
assert [{k:r[k] for k in olde[0]} for r in e[:len(olde)]]==olde
assert all(r['annotation_evidence']=='released annotation' for r in m[:len(oldm)]+e[:len(olde)])
seq={t:''.join((B/(t+'.fa')).read_text().splitlines()[1:]).upper() for t in w}
exon_by_model=collections.defaultdict(list)
for r in e:exon_by_model[r['model_id']].append(r)
byid={r['model_id']:r for r in m}
def spliced(mid):
 r=byid[mid];win=w[r['track']];blocks=[]
 for ex in exon_by_model[mid]:
  a,b=int(ex['start0']),int(ex['end0'])
  x,y=(a-int(win['start0']),b-int(win['start0'])) if win['orientation']=='+' else (int(win['end0'])-b,int(win['end0'])-a)
  assert 0<=x<y<=len(seq[r['track']])
  blocks.append((x,y))
 s=''.join(seq[r['track']][a:b] for a,b in sorted(blocks))
 display_strand=r['strand'] if win['orientation']=='+' else ('-' if r['strand']=='+' else '+')
 return s if display_strand=='+' else rev(s)
manifest=json.loads((P/'projection_manifest.json').read_text());audit=rd(P/'projection-audit.tsv')
projected=[r for r in m if r['annotation_evidence']=='same-species Liftoff projection'];diagnostics=[];native_exons=0
for run in manifest['runs']:
 target,source=run['target'],run['source'];d=P/target;assert run['target_window']==w[target] and run['source_window']==w[source]
 for tag in ['target','source']:
  assert run[f'{tag}_fasta_sha256']==hashlib.sha256((B/(run[tag]+'.fa')).read_bytes()).hexdigest()
 assert run['source_gff_sha256']==hashlib.sha256((d/'source.gff3').read_bytes()).hexdigest()
 assert run['tools']['direct_url']['vcs_info']['commit_id']=='94d6073ee2400c438d181a67d3af1e362197d839'
 source_by_gid=json.loads((d/'source_models.json').read_text());raw_exons=collections.defaultdict(list);raw_genes={};parents={}
 for line in (d/'projected.gff3').read_text().splitlines():
  if not line or line.startswith('#'):continue
  f=line.split('\t');assert f[0]==target
  attrs={unquote(k):unquote(v) for k,v in (a.split('=',1) for a in f[8].split(';') if '=' in a)}
  a,b=int(f[3])-1,int(f[4]);win=w[target]
  if win['orientation']=='+':a,b=a+int(win['start0']),b+int(win['start0']);strand=f[6]
  else:a,b=int(win['end0'])-b,int(win['end0'])-a;strand='-' if f[6]=='+' else '+'
  if f[2]=='gene':raw_genes[attrs['ID']]=(a,b,strand,attrs)
  elif f[2] in ('mRNA','transcript'):parents[attrs['ID']]=attrs['Parent']
  elif f[2]=='exon':
   for parent in attrs['Parent'].split(','):raw_exons[parents[parent]].append((a,b,strand))
 these=[r for r in projected if r['track']==target];assert len(these)==run['accepted']
 assert run['accepted']+run['not_transferred']==run['source_context_genes']
 for r in these:
  gid=r['name'];a,b,strand,attrs=raw_genes[gid]
  assert (int(r['chromStart']),int(r['chromEnd']),r['strand'])==(a,b,strand)
  assert float(attrs['coverage'])==float(r['projection_coverage'])>=.95
  assert float(attrs['sequence_ID'])==float(r['projection_identity'])>=.95
  assert not r['gene_symbol'].startswith(('HLA-','Patr-','Papa-','Gogo-','Poab-','Popy-','Sysy-'))
  assert r['projection_source_model_id']==source_by_gid[gid]['model_id']
  actual=sorted((int(ex['start0']),int(ex['end0']),ex['strand']) for ex in exon_by_model[r['model_id']])
  assert actual==sorted(set(raw_exons[gid]));native_exons+=len(actual)
  sizes=[int(v) for v in r['blockSizes'].strip(',').split(',')];starts=[int(v) for v in r['blockStarts'].strip(',').split(',')]
  assert actual==[(a+x,a+x+n,strand) for x,n in zip(starts,sizes)]
  q,t=spliced(r['projection_source_model_id']),spliced(r['model_id'])
  aln=parasail.nw_stats_striped_32(q,t,5,1,parasail.matrix_create('ACGTN',2,-3))
  assert not aln.saturated
  diagnostics.append(dict(track=target,gene_symbol=r['gene_symbol'],model_id=r['model_id'],source_exonic_bp=len(q),target_exonic_bp=len(t),global_matches=aln.matches,global_alignment_columns=aln.length,global_spliced_identity=aln.matches/aln.length))
assert len(projected)==sum(r['status']=='accepted' for r in audit)==956
assert len(audit)==970 and sum(r['status']!='accepted' for r in audit)==14
assert {r['track'] for r in m}==set(w)
assert all(r['global_spliced_identity']>=.95 for r in diagnostics)
with (P/'independent-spliced-DNA-qc.tsv').open('w') as h:
 writer=csv.DictWriter(h,fieldnames=list(diagnostics[0]),delimiter='\t');writer.writeheader();writer.writerows(diagnostics)
report=dict(status='PASS',unchanged_released_models=len(oldm),unchanged_released_exons=len(olde),projected_models=len(projected),projected_exons_native_coordinates_verified=native_exons,unresolved_source_models=14,tracks_with_context=13,minimum_independent_global_spliced_identity=min(r['global_spliced_identity'] for r in diagnostics))
report['minimum_global_spliced_identity_by_track']={t:min(r['global_spliced_identity'] for r in diagnostics if r['track']==t) for t in {r['track'] for r in diagnostics}}
(P/'verification.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
