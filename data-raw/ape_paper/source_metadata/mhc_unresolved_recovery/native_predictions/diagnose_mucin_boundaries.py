#!/usr/bin/env python3
"""Evidence-only endpoint mapping preserving intra-exon indels; no model export."""
from pathlib import Path
import csv,json,re,parasail
OUT=Path(__file__).resolve().parent
ROOT=OUT.parents[2];R=ROOT/'results/mhc_continuous'
read=lambda p:list(csv.DictReader(p.open(),delimiter='\t'))
wins={x['track']:x for x in read(R/'windows.tsv')}
original=json.loads((OUT/'native_candidate_evidence.json').read_text())
targets={('PABh2','MUC21'),('PPYh2','MUC21'),('GGOh2','MUC21'),('GGOh2','MUC22')}
def display(track,a,b):
 w=wins[track];return (int(w['end0'])-b,int(w['end0'])-a) if w['orientation']=='-' else (a-int(w['start0']),b-int(w['start0']))
def native(track,a,b):
 w=wins[track];return (int(w['end0'])-b,int(w['end0'])-a) if w['orientation']=='-' else (int(w['start0'])+a,int(w['start0'])+b)
def fasta(track):return ''.join(l.strip() for l in (R/f'{track}.fa').read_text().splitlines() if not l.startswith('>')).upper()
def pafrows(path,source):
 rows=[]
 for ln,l in enumerate(path.read_text().splitlines(),1):
  f=l.split('\t')
  if f[4]!='+':continue
  qi,ti=int(f[2]),int(f[7]);segs=[];ops=[]
  for ns,op in re.findall(r'(\d+)([MIDNSHP=X])',next(x[5:] for x in f[12:] if x.startswith('cg:Z:'))):
   n=int(ns);s,t=(qi,ti) if f[0]==source else (ti,qi)
   sop=op if f[0]==source else {'I':'D','D':'I'}.get(op,op)
   if op in '=XM':segs.append((s,s+n,t,t+n,op))
   ops.append((s,t,n,sop))
   if op in '=XMI':qi+=n
   if op in '=XMDN':ti+=n
  rows.append(dict(line=ln,mapq=int(f[11]),segments=segs,ops=ops,source_start=int(f[2]) if f[0]==source else int(f[7]),source_end=int(f[3]) if f[0]==source else int(f[8])))
 return rows
def mapbase(x,segs):
 for a,b,c,d,op in segs:
  if a<=x<b:return c+x-a
 return None
reports=[];exonrows=[]
for r in original:
 if (r['track'],r['gene_symbol']) not in targets:continue
 source,target=r['source_track'],r['track'];sseq,tseq=fasta(source),fasta(target)
 rows=pafrows(R/r['paf_file'],source);mapped=[];sourceparts=[];targetparts=[]
 for num,(na,nb) in enumerate(r['source_exons_native_bed0'],1):
  a,b=display(source,na,nb)
  candidates=[p for p in rows if p['source_start']<=a and p['source_end']>=b]
  candidates.sort(key=lambda p:(p['mapq'],sum(max(0,min(b,e)-max(a,s)) for s,e,t,u,op in p['segments'])),reverse=True)
  p=candidates[0] if candidates else None
  left=right=None;ld=rd=None
  if p:
   for d in range(11):
    if left is None:
     q=mapbase(a+d,p['segments'])
     if q is not None:left=q;ld=d
    if right is None:
     q=mapbase(b-1-d,p['segments'])
     if q is not None:right=q+1;rd=d
  exact=left is not None and right is not None and ld==rd==0
  er=dict(track=target,gene_symbol=r['gene_symbol'],exon_number_native_order=num,source_start0=na,source_end0=nb,source_exon_length=b-a,paf_line=p['line'] if p else None,start_boundary_inward_offset_bp=ld,end_boundary_inward_offset_bp=rd,both_boundaries_exact=exact)
  if left is not None and right is not None:
   ta,tb=native(target,left,right)
   exsource=sseq[a:b];extarget=tseq[left:right]
   sourceparts.append(exsource);targetparts.append(extarget)
   cov=sum(max(0,min(b,e)-max(a,s)) for s,e,t,u,op in p['segments'])
   ide=sum(max(0,min(b,e)-max(a,s)) for s,e,t,u,op in p['segments'] if op=='=')
   internal_source_deleted=sum(min(a2+n,b)-max(a2,a) for a2,t,n,op in p['ops'] if op=='I' and a2<b and a2+n>a)
   internal_target_inserted=sum(n for a2,t,n,op in p['ops'] if op=='D' and a<a2<b)
   er.update(target_start0=ta,target_end0=tb,target_exon_length=right-left,length_delta_bp=right-left-(b-a),paf_aligned_source_bases=cov,paf_exact_source_bases=ide,internal_source_deleted_bp=internal_source_deleted,internal_target_inserted_bp=internal_target_inserted)
   mapped.append((ta,tb))
  exonrows.append(er)
 sr=''.join(sourceparts);tr=''.join(targetparts)
 result=parasail.nw_trace_striped_32(sr,tr,5,1,parasail.matrix_create('ACGTN',2,-3))
 aq,ar=result.traceback.query,result.traceback.ref
 assert aq.replace('-','')==sr and ar.replace('-','')==tr
 stats=dict(exact_matches=sum(x==y and x!='-' for x,y in zip(aq,ar)),mismatches=sum(x!=y and x!='-' and y!='-' for x,y in zip(aq,ar)),source_gap_bp=aq.count('-'),target_gap_bp=ar.count('-'),alignment_columns=len(aq))
 exs=[e for e in exonrows if e['track']==target and e['gene_symbol']==r['gene_symbol']]
 report=dict(track=target,gene_symbol=r['gene_symbol'],source_track=source,source_model_id=r['source_model_id'],target_native_chrom=r['target_native_chrom'],source_native_exons=r['source_exons_native_bed0'],target_native_exons=mapped,target_native_start0=min(a for a,b in mapped),target_native_end0=max(b for a,b in mapped),target_native_strand=r['source_strand'],source_exon_count=len(r['source_exons_native_bed0']),mapped_exon_count=len(mapped),exact_endpoint_count=sum(int(e['start_boundary_inward_offset_bp']==0)+int(e['end_boundary_inward_offset_bp']==0) for e in exs),full_exact_boundary_confidence=all(e['both_boundaries_exact'] for e in exs),source_spliced_length=len(sr),target_spliced_length=len(tr),source_paf_aligned_fraction=sum(e.get('paf_aligned_source_bases',0) for e in exs)/len(sr),target_length_delta_bp=len(tr)-len(sr),global_alignment_parameters=dict(algorithm='parasail.nw_trace_striped_32',match=2,mismatch=-3,gap_open=5,gap_extend=1),global_alignment_stats=stats,global_identity_including_all_gap_columns=stats['exact_matches']/stats['alignment_columns'],source_base_pairing_fraction=(len(sr)-stats['target_gap_bp'])/len(sr),source_exact_match_fraction=stats['exact_matches']/len(sr),cigar=result.cigar.decode.decode(),interpretation='Endpoint-mapped same-species syntenic candidate; indels inside an exon remain inside that exon. Not evidence of precise target splice use or expression; not accepted as a strict95 transfer.')
 reports.append(report)
 name=f'{target}.{r["gene_symbol"]}'
 (OUT/f'{name}.endpoint_spliced.fa').write_text(f'>{name}.source_{source}\n{sr}\n>{name}.target_native_candidate\n{tr}\n')
 (OUT/f'{name}.global_alignment.txt').write_text(aq+'\n'+ar+'\n')
(OUT/'mucin_exact_boundary_diagnostics.json').write_text(json.dumps(reports,indent=2)+'\n')
with (OUT/'mucin_exact_boundary_exons.tsv').open('w') as f:
 fields=list(dict.fromkeys(k for x in exonrows for k in x));w=csv.DictWriter(f,fields,delimiter='\t');w.writeheader();w.writerows(exonrows)
for r in reports:print(r['track'],r['gene_symbol'],r['target_native_exons'],'exact',r['exact_endpoint_count'],'lengths',r['source_spliced_length'],r['target_spliced_length'],'global identity',r['global_identity_including_all_gap_columns'],'stats',r['global_alignment_stats'])
# Cross-species RefSeq alignment supports the locus, not native RNA expression.
metadata=read(ROOT/'source_metadata/mhc_context_search/ncbi/genark_xenoRefGene_mhc_accession_metadata.tsv')
for r in reports:
 w=wins[r['track']];tseq=fasta(r['track']);seqs=[]
 for a,b in r['target_native_exons']:
  da,db=display(r['track'],a,b);seqs.append((da,db))
 r['target_intron_boundary_dinucleotides']=[dict(intron_start0=r['target_native_exons'][i][1],intron_end0=r['target_native_exons'][i+1][0],donor=tseq[seqs[i][1]:seqs[i][1]+2],acceptor=tseq[seqs[i+1][0]-2:seqs[i+1][0]]) for i in range(len(seqs)-1)]
 humans=[m for m in metadata if m['track']==r['track'] and m['source_gene_symbols']==r['gene_symbol'] and m['source_organism']=='Homo sapiens']
 comparisons=[]
 for m in humans:
  hx=[(int(m['start0'])+s,int(m['start0'])+s+n) for s,n in zip(map(int,m['block_starts'].strip(',').split(',')),map(int,m['block_sizes'].strip(',').split(',')))]
  overlap=sum(max(0,min(b,d)-max(a,c)) for a,b in r['target_native_exons'] for c,d in hx)
  comparisons.append(dict(refseq_accession=m['refseq_accession'],native_alignment_start0=int(m['start0']),native_alignment_end0=int(m['end0']),strand=m['strand'],source_organism=m['source_organism'],native_alignment_blocks=hx,gene_span_start_delta_candidate_minus_human=r['target_native_start0']-int(m['start0']),gene_span_end_delta_candidate_minus_human=r['target_native_end0']-int(m['end0']),human_exonic_bp=sum(b-a for a,b in hx),human_aligned_exonic_overlap_with_candidate_bp=overlap,human_aligned_exonic_overlap_fraction=overlap/sum(b-a for a,b in hx),candidate_exon_starts_matching_a_human_block_start=[a for a,b in r['target_native_exons'] if a in {c for c,d in hx}],candidate_exon_ends_matching_a_human_block_end=[b for a,b in r['target_native_exons'] if b in {d for c,d in hx}],caution='Human RefSeq native-target sequence alignment is locus evidence. It is not an ape target-haplotype RNA observation; block splits in repeats need not be true introns.'))
 r['human_refseq_locus_comparison']=comparisons
(OUT/'mucin_exact_boundary_diagnostics.json').write_text(json.dumps(reports,indent=2)+'\n')
for r in reports:
 print(r['track'],r['gene_symbol'],'splice motifs',r['target_intron_boundary_dinucleotides'],'human',r['human_refseq_locus_comparison'])
for r in reports:
 tseq=fasta(r['track']);a,b=r['target_native_exons'][1];da,db=display(r['track'],a,b);s=tseq[da:db]
 scores=sorted([(sum(x==y for x,y in zip(s,s[k:]))/(len(s)-k),k) for k in range(15,181)],reverse=True)
 r['target_middle_exon_repeat_periodicity']=dict(method='Fraction of matching DNA bases at each shift15..180bp across complete middle exon. Descriptive evidence of sequence periodicity only.',top_shifts=[dict(lag_bp=k,base_match_fraction=v) for v,k in scores[:6]],lag45_base_match_fraction=sum(x==y for x,y in zip(s,s[45:]))/(len(s)-45))
(OUT/'mucin_exact_boundary_diagnostics.json').write_text(json.dumps(reports,indent=2)+'\n')
