#!/usr/bin/env python3
"""Reconstruct a continuous full MHC comparison from auditable bounded inputs.
The 5.309336Mb CHM13 interval is the union of published VR intervals and the
ZNF311–GRM4 landmark genes. It is not an assertion of original ED3c cut points.
"""
from pathlib import Path
import csv,json,re,hashlib,concurrent.futures,subprocess,argparse,sys
from acquire import RemoteTwoBit
from mhc_continuous_utils import clip_block,display_interval,split_cigar
ROOT=Path(__file__).resolve().parent;RAW=ROOT/'source_metadata/mhc_continuous';OUT=ROOT/'results/mhc_continuous';OUT.mkdir(exist_ok=True)
HUMAN_LO,HUMAN_HI=28669878,33979214
ORDER=['SSYh2','SSYh1','PABh2','PABh1','PPYh2','PPYh1','GGOh2','GGOh1','PPAh2','PPAh1','PTRh2','PTRh1','HSAh1']
SAMPLE={'PTR':'mPanTro3','PPA':'mPanPan1','GGO':'mGorGor1','PPY':'mPonPyg2','PAB':'mPonAbe1','SSY':'mSymSyn1'}
NAMES={'HSA':'Human CHM13','PTR':'Chimpanzee','PPA':'Bonobo','GGO':'Gorilla','PPY':'Bornean orangutan','PAB':'Sumatran orangutan','SSY':'Siamang'}
def rd(p):return list(csv.DictReader(p.open(),delimiter='\t'))
def write(p,rows,fields):
 with p.open('w') as h:
  w=csv.DictWriter(h,fieldnames=fields,delimiter='\t',extrasaction='ignore');w.writeheader();w.writerows(rows)
def merge_intervals(values):
 out=[]
 for a,b in sorted(values):
  if out and a<=out[-1][1]:out[-1]=(out[-1][0],max(b,out[-1][1]))
  else:out.append((a,b))
 return out

def prepare():
 if not (RAW/'chain_sources.json').exists():subprocess.run([sys.executable,str(ROOT/'fetch_mhc_continuous_chains.py')],check=True)
 source=json.loads((RAW/'chain_sources.json').read_text())
 if any(not (RAW/(d['shortLabel'].replace('#','_')+suffix)).exists() for d in source for suffix in ('.chains.bed','.links.bed')):subprocess.run([sys.executable,str(ROOT/'fetch_mhc_continuous_chains.py')],check=True)
 curated=rd(ROOT/'results/mhc_full/genes.tsv')
 blocks=[];windows=[];sources={}
 for track in ORDER:
  species=track[:3];hap=int(track[-1]);gg=[z for z in curated if z['track']==track];chrom=gg[0]['chr']
  if track=='HSAh1':
   lo,hi,ori=HUMAN_LO,HUMAN_HI,'+';url='https://s3-us-west-2.amazonaws.com/human-pangenomics/T2T/browser/CHM13/chm13v2.0.2bit'
  else:
   sample=SAMPLE[species];ending=('mat' if hap==1 else 'pat') if species in ('PPA','GGO') else f'hap{hap}'
   name=sample+'_'+ending;cfg=next(d for d in source if d['shortLabel'].replace('#','_')==name);sources[track]=cfg
   chains={}
   for line in (RAW/(name+'.chains.bed')).read_text().splitlines():
    f=line.split('\t');chains[f[3]]=dict(strand=f[5],qchr=f[7],qsize=int(f[8]))
   bb=[]
   for idx,line in enumerate((RAW/(name+'.links.bed')).read_text().splitlines(),1):
    f=line.split('\t');c=chains[f[3]]
    if c['qchr']!=chrom:continue
    a,b=int(f[1]),int(f[2]);q=int(f[4]);n=b-a
    q0,q1=(q,q+n) if c['strand']=='+' else (c['qsize']-q-n,c['qsize']-q)
    clipped=clip_block(a,b,q0,q1,c['strand'],HUMAN_LO,HUMAN_HI)
    if clipped:
     a,b,q0,q1=clipped;bb.append(dict(track=track,t0=a,t1=b,q0=q0,q1=q1,strand=c['strand'],chain_id=f[3],source_row=idx))
   assert bb,track
   strand_coverage={strand:sum(z['t1']-z['t0'] for z in bb if z['strand']==strand) for strand in ['+','-']}
   ori=max(strand_coverage,key=strand_coverage.get);lo=min(z['q0'] for z in bb);hi=max(z['q1'] for z in bb)
   blocks+=bb;url=cfg['otherTwoBitUrl']
  windows.append(dict(track=track,species_id=species,haplotype=f'h{hap}',label=NAMES[species]+('' if species=='HSA' else f' h{hap}'),chr=chrom,start0=lo,end0=hi,length=hi-lo,orientation=ori,twoBit_url=url))
  assert all(lo<=int(z['start'])-1 and int(z['end'])<=hi for z in gg),(track,'curated gene outside orthologous window')
 write(OUT/'windows.tsv',windows,['track','species_id','haplotype','label','chr','start0','end0','length','orientation','twoBit_url'])
 write(OUT/'human_anchor_blocks.tsv',blocks,['track','t0','t1','q0','q1','strand','chain_id','source_row'])
 # Project exactly chain-supported portions. These are shared comparative findings,
 # not independent measurements of ape-specific discordance.
 regions=rd(RAW/'paper_c/Supplementary_Table_XI_59_exact.tsv')[:3];features=[];summary=[]
 for w in windows:
  for r in regions:
   a,b=int(r['Start']),int(r['End']);vr=r['Region name']
   if w['track']=='HSAh1':intervals=[(a,b)];ref_coverage=b-a;kind='published CHM13 interval'
   else:
    mapped=[];ref=[]
    for z in blocks:
     if z['track']!=w['track']:continue
     clipped=clip_block(z['t0'],z['t1'],z['q0'],z['q1'],z['strand'],a,b)
     if clipped:
      x,y,q0,q1=clipped;mapped.append((q0,q1));ref.append((x,y))
    intervals=merge_intervals(mapped);ref_coverage=sum(y-x for x,y in merge_intervals(ref));kind='chain-projected shared annotation'
   for x,y in intervals:
    ds,de=display_interval(x,y,w['start0'],w['end0'],w['orientation'])
    features.append(dict(track=w['track'],chr=w['chr'],start0=x,end0=y,display_start0=ds,display_end0=de,region=vr,source_start=a,source_end=b,evidence=kind))
   summary.append(dict(track=w['track'],region=vr,mapped_reference_bp=ref_coverage,source_bp=b-a,coverage_fraction=ref_coverage/(b-a),segments=len(intervals)))
 write(OUT/'discordant_regions.tsv',features,['track','chr','start0','end0','display_start0','display_end0','region','source_start','source_end','evidence'])
 write(OUT/'discordant_projection_qc.tsv',summary,['track','region','mapped_reference_bp','source_bp','coverage_fraction','segments'])
 manifest=dict(reference=dict(chr='chr6',start0=HUMAN_LO,end0=HUMAN_HI,length=HUMAN_HI-HUMAN_LO),window_definition='Reconstruction: union of TableXI59 VR1–3 and CHM13RefSeq ZNF311–GRM4 landmark gene spans; original ED3c extraction endpoints unavailable. This interval rounds to5.31Mb but is not an exact reproduction.',source_query=dict(chr='chr6',start0=28000000,end0=34000000),coordinate_convention='BED0. TableXI59 indexing unstated; operationally treat End−Start as half-open to match reported sizes. Preserve original values.',sources=sources,raw_files=[dict(file=str(p.relative_to(ROOT)),sha256=hashlib.sha256(p.read_bytes()).hexdigest()) for p in sorted(RAW.glob('*.bed'))],limitations=['Released general exon models cover one haplotype per ape species. Additional same-species context projections, where generated, are documented separately in gene_models_with_projected_context/projection_manifest.json.','Projected variable regions label shared comparative findings and preserve chain gaps.','Adjacent nucleotide alignments regenerated using minimap2 on extracted exact native sequences, not recovered original ED3c plot data.'])
 (OUT/'manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
 return windows

def fetch_sequences(windows):
 jobs={}
 for w in windows:jobs.setdefault(w['twoBit_url'],[]).append(w)
 def task(item):
  url,ws=item;pending=[w for w in ws if not (OUT/(w['track']+'.fa')).exists()]
  if not pending:return
  obj=RemoteTwoBit(url)
  for w in pending:
   seq=obj.sequence(w['chr'],w['start0'],w['end0']).upper()
   if w['orientation']=='-':seq=seq.translate(str.maketrans('ACGTN','TGCAN'))[::-1]
   assert len(seq)==w['length'];(OUT/(w['track']+'.fa')).write_text('>'+w['track']+'\n'+'\n'.join(seq[i:i+80] for i in range(0,len(seq),80))+'\n')
   print('FASTA',w['track'],len(seq),flush=True)
 with concurrent.futures.ThreadPoolExecutor(max_workers=3) as ex:list(ex.map(task,jobs.items()))

def align(windows):
 pairs=list(zip(windows,windows[1:]));all_blocks=[]
 def task(pair):
  t,q=pair;stem=t['track']+'__'+q['track'];p=OUT/(stem+'.paf')
  cmd=['minimap2','-x','asm20','-c','--eqx','--secondary=no','-t','2',str(OUT/(t['track']+'.fa')),str(OUT/(q['track']+'.fa'))]
  if not p.exists():
   tmp=p.with_suffix('.paf.tmp')
   with tmp.open('w') as h,(OUT/(stem+'.log')).open('w') as log:subprocess.run(cmd,stdout=h,stderr=log,check=True)
   tmp.rename(p)
  (OUT/(stem+'.command.json')).write_text(json.dumps(cmd,indent=2)+'\n');out=[]
  for row,line in enumerate(p.read_text().splitlines(),1):
   f=line.split('\t');qs,qe,ts,te=map(int,[f[2],f[3],f[7],f[8]]);identity=int(f[9])/int(f[10])
   if identity<.70:continue
   tags={v[:2]:v[5:] for v in f[12:]};qlen=int(f[1]);strand=f[4];oq0=qs if strand=='+' else qlen-qe
   for block,(a,b,c,d) in enumerate(split_cigar(tags['cg'],ts,oq0,strand),1):
    if min(b-a,d-c)<500:continue
    c,d=(c,d) if strand=='+' else (qlen-d,qlen-c)
    assert 0<=a<b<=t['length'] and 0<=c<d<=q['length']
    out.append(dict(tspecies=t['track'],qspecies=q['track'],tchr='MHC',qchr='MHC',tstart=a+1,tend=b,qstart=c+1,qend=d,strand=strand,identity=identity*100,source_paf=p.name,source_row=row,cigar_block=block))
  print('ALIGN',stem,len(out),flush=True);return out
 with concurrent.futures.ThreadPoolExecutor(max_workers=3) as ex:
  for out in ex.map(task,pairs):all_blocks+=out
 write(OUT/'links.tsv',all_blocks,['tspecies','qspecies','tchr','qchr','tstart','tend','qstart','qend','strand','identity','source_paf','source_row','cigar_block'])
 print('TOTAL LINKS',len(all_blocks),flush=True)

if __name__=='__main__':
 p=argparse.ArgumentParser();p.add_argument('--windows-only',action='store_true');args=p.parse_args();w=prepare()
 if not args.windows_only:
  fetch_sequences(w);align(w)
  manifest=json.loads((OUT/'manifest.json').read_text())
  manifest['sequence_files']=[dict(track=z['track'],file=z['track']+'.fa',length=z['length'],chr=z['chr'],start0=z['start0'],end0=z['end0'],orientation=z['orientation'],sha256=hashlib.sha256((OUT/(z['track']+'.fa')).read_bytes()).hexdigest()) for z in w]
  manifest['alignment_files']=[dict(file=p.name,sha256=hashlib.sha256(p.read_bytes()).hexdigest()) for p in sorted(OUT.glob('*.paf'))]
  (OUT/'manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
 print('DONE',len(w),'continuous windows',flush=True)
