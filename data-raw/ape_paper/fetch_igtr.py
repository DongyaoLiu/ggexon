#!/usr/bin/env python3
"""Acquire pinned paper IG/TR sources and prepare transparent locus-local tables."""
import csv, hashlib, io, json, re, time
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import requests
ROOT=Path(__file__).resolve().parent
RAW=ROOT/'source_metadata/igtr'; OUT=ROOT/'results/igtr'
REPO='yana-safonova/primate_t2t_igtr'
SUP='https://media.springernature.com/original/springer-static/esm/art%3A10.1038%2Fs41586-025-08816-3/MediaObjects/41586_2025_8816_MOESM5_ESM.csv'
LOCI=['IGH','IGK','IGL','TRA','TRB','TRG']
MAP={'human_T2T':('HSA1','Human','1'),'mPanPan1_mat':('PPA1','Bonobo','1'),'mPanPan1_pat':('PPA2','Bonobo','2'),'mGorGor1_mat':('GGO1','Gorilla','1'),'mGorGor1_pat':('GGO2','Gorilla','2'),'mPonAbe1_hap1':('PAB1','Sumatran orangutan','1'),'mPonAbe1_hap2':('PAB2','Sumatran orangutan','2'),'mPonPyg2_hap1':('PPY1','Bornean orangutan','1'),'mPonPyg2_hap2':('PPY2','Bornean orangutan','2')}
ORDER={v[0]:i+1 for i,v in enumerate(MAP.values())}
def get(url):
 for i in range(3):
  try:
   r=requests.get(url,timeout=45);r.raise_for_status();return r.content
  except requests.RequestException:
   if i==2:raise
   time.sleep(i+1)
def table(path):return list(csv.DictReader(path.open(),delimiter='\t' if path.suffix in ['.tsv','.txt'] else ','))
def write(name,rows,fields=None):
 if not rows and not fields:return
 with (OUT/name).open('w') as f:
  w=csv.DictWriter(f,fieldnames=fields or list(rows[0]),delimiter='\t');w.writeheader();w.writerows(rows)
def main():
 RAW.mkdir(parents=True,exist_ok=True);OUT.mkdir(parents=True,exist_ok=True)
 pin=RAW/'repository_tree.json'
 if pin.exists():tree=json.loads(pin.read_text())
 else:
  tree=json.loads(get(f'https://api.github.com/repos/{REPO}/git/trees/main?recursive=1'));pin.write_text(json.dumps(tree,indent=2))
 sha=tree['sha'];base=f'https://raw.githubusercontent.com/{REPO}/{sha}/'
 paths=[x['path'] for x in tree['tree'] if x['type']=='blob' and (x['path'].startswith(('configs/','data_primate_gene_positions/','data_human_t2t_gene_positions/','data_locus_alignments/','data_SV_block_positions/','data_primate_igtrloci_fasta/','aux_scripts/')) or x['path'] in ['README.md','visualize_genome_diagram.py','data_primate_igtrloci_locus_config.csv'])]
 def download(p):
  dst=RAW/'repo'/p
  if not dst.exists():dst.parent.mkdir(parents=True,exist_ok=True);dst.write_bytes(get(base+p))
 with ThreadPoolExecutor(max_workers=8) as pool:list(pool.map(download,paths))
 if not (RAW/'supplementary_data_1.csv').exists():(RAW/'supplementary_data_1.csv').write_bytes(get(SUP))
 manifest=[{'path':str(p.relative_to(RAW)),'bytes':p.stat().st_size,'sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'url':SUP if p.name=='supplementary_data_1.csv' else base+str(p.relative_to(RAW/'repo'))} for p in RAW.rglob('*') if p.is_file() and (p.is_relative_to(RAW/'repo') or p.name=='supplementary_data_1.csv')]
 (RAW/'manifest.json').write_text(json.dumps({'repository_commit':sha,'sources':manifest},indent=2))
 cfgrows=table(RAW/'repo/data_primate_igtrloci_locus_config.csv');cfgidx={(r['Species']+'_'+r['Haplotype'],r['Locus']):r for r in cfgrows}
 genes=[];loci=[];links=[];blocks=[]
 for locus in LOCI:
  cfg=dict(line.strip().split(None,1) for line in (RAW/f'repo/configs/config_{locus}.txt').read_text().splitlines() if line.strip())
  names=cfg['GenomeNames'].split(',');lens=list(map(int,cfg['Lengths'].split(',')));orient=list(map(int,cfg['Strands'].split(',')));facet='TRA/TRD' if locus=='TRA' else locus
  for name,length,ori,gfile,bfile in zip(names,lens,orient,cfg['GeneFiles'].split(','),cfg['Units'].split(',')):
   track,species,hap=MAP[name];gfile=gfile.replace('gene_positinos','gene_positions');gf=RAW/'repo'/gfile
   cr=cfgidx.get((name,locus))
   fasta=RAW/'repo'/('data_primate_igtrloci_fasta/'+cr['Fname'] if cr else 'data_human_t2t_gene_positions/human_'+locus+'.fasta')
   lines=fasta.read_text().splitlines();seq=''.join(lines[1:]).upper();header=lines[0]
   bounds=re.search(r'START(?:_POS)?:([0-9]+)\|END(?:_POS)?:([0-9]+)',header);offset=int(bounds.group(1));contig=header[1:].split('|')[0]
   loci.append(dict(track_id=track,species=species,haplotype=hap,locus=locus,facet=facet,length=length,display_orientation=ori,track_order=ORDER[track],source_track=name,contig=contig,genomic_start0=offset,genomic_end0=int(bounds.group(2)),fasta_length=len(seq),source_file='repo/configs/config_'+locus+'.txt'))
   for idx,r in enumerate(table(gf)):
    s=r['Sequence'].upper();native=int(r['LocusPos']);expected=int(r['Pos'])-offset;strand=r['Strand'];forward=s if strand!='-' else s.translate(str.maketrans('ACGT','TGCA'))[::-1]
    candidates=[expected,expected-1,expected+1,native];pos=next((p for p in candidates if seq[p:p+len(s)]==forward),None)
    if pos is None and strand in ['NA','']:
     pos=next((p for p in candidates if seq[p:p+len(s)]==s),None)
    verified=pos is not None
    if not verified:pos=native
    start,end=pos+1,pos+len(s)
    ds,de=(start,end) if ori==1 else (length-end+1,length-start+1)
    genes.append(dict(track_id=track,species=species,haplotype=hap,locus=locus,facet=facet,gene_type=r['GeneType'],source_label=r.get('Label') or f'{contig}_{r["Pos"]}',contig=contig,genomic_pos_source=r['Pos'],locus_pos_source=native,start=start,end=end,display_start=ds,display_end=de,strand=strand,productive=r['Productive'] or 'unknown',is_species_specific=r['IsSpeciesSpecific'],sequence_length=len(s),sequence_verified=verified,locus_pos_corrected=pos!=native,source_row=idx+1,source_file='repo/'+gfile))
   for r in table(RAW/'repo'/bfile):
    a,b=int(r['Start']),int(r['End']);st,en=a+1,b;ds,de=(st,en) if ori==1 else (length-en+1,length-st+1)
    blocks.append(dict(track_id=track,locus=locus,facet=facet,start=st,end=en,display_start=ds,display_end=de,color=r['Color'],source_file='repo/'+bfile,annotation='source SV block'))
  for i,lf in enumerate(cfg['RangeFiles'].split(',')):
   for j,r in enumerate(table(RAW/'repo'/lf)):
    vals={}
    for k in [1,2]:
     a,b=int(r[f'G{k}_start']),int(r[f'G{k}_end']);ln=lens[i+k-1];o=orient[i+k-1];vals.update({f'start{k}':a,f'end{k}':b,f'display_start{k}':a if o==1 else ln-a+1,f'display_end{k}':b if o==1 else ln-b+1})
    links.append(dict(locus=locus,facet=facet,track1=MAP[names[i]][0],track2=MAP[names[i+1]][0],identity=float(r['PI']),source_file='repo/'+lf,source_row=j+1,**vals))
 write('genes.tsv',genes);write('loci.tsv',loci);write('alignments.tsv',links);write('sv_blocks.tsv',blocks)
 counts=[]
 for loc in loci:
  gs=[g for g in genes if g['track_id']==loc['track_id'] and g['locus']==loc['locus']]
  for gt in ['V','D','J']:
   sub=[g for g in gs if g['gene_type']==gt]
   counts.append(dict(track_id=loc['track_id'],species=loc['species'],locus=loc['locus'],facet=loc['facet'],gene_type=gt,source_rows=len(sub),unique_intervals=len(set((g['start'],g['end']) for g in sub)),species_specific_rows=sum(g['is_species_specific']=='True' for g in sub),productive_true_rows=sum(g['productive']=='True' for g in sub),productive_false_rows=sum(g['productive']=='False' for g in sub),productive_unknown_rows=sum(g['productive']=='unknown' for g in sub)))
 write('counts.tsv',counts)
 supp=table(RAW/'supplementary_data_1.csv')
 keep=['index','species','haplotype','locus','Source','gene_type','Contig','Strand','digger_start','digger_end','igdetective_start','igdetective_end','gene_start','gene_end','functional','notes','likelihood','digger_SeqLength','igdetective_SeqLength']
 write('supplementary_gene_annotations.tsv',[{k:r[k] for k in keep} for r in supp])
 # Supplementary final boundaries are chromosome 1-based closed; retain exact values.
 spprefix={'gorilla':'GGO','bonobo':'PPA','sumatran orangutan':'PAB','bornean orangutan':'PPY'}
 locusidx={(r['track_id'],r['locus']):r for r in loci}
 paper=[]
 for r in supp:
  species=' '.join(r['species'].split());hap=r['haplotype'].strip();track=spprefix[species]+('1' if hap in ['mat','hap1'] else '2');locus='TRA' if r['locus']=='TRA/D' else r['locus'];lc=locusidx[track,locus];gt=r['gene_type'];gtype='V' if 'V' in gt else 'D' if gt.endswith('D') else 'J' if gt.endswith('J') else 'unknown'
  a,b=sorted([int(float(r['gene_start'])),int(float(r['gene_end']))]);st,en=a-lc['genomic_start0'],b-lc['genomic_start0'];ori=lc['display_orientation'];ln=lc['length'];ds,de=(st,en) if ori==1 else (ln-en+1,ln-st+1)
  ip=r['igdetective_start'];matches=[]
  if ip:
   ip=int(float(ip));matches=[g for g in genes if g['track_id']==track and g['locus']==locus and g['gene_type']==gtype and abs(int(g['genomic_pos_source'])-ip)<=1]
  flags=set(g['is_species_specific'] for g in matches);flag=next(iter(flags)) if len(flags)==1 else 'unknown'
  paper.append(dict(track_id=track,species=lc['species'],haplotype=lc['haplotype'],locus=locus,facet=lc['facet'],gene_family=gt or 'unclassified',gene_type=gtype,contig=r['Contig'],genomic_start=a,genomic_end=b,start=st,end=en,display_start=ds,display_end=de,strand=r['Strand'],functional=r['functional'],is_species_specific=flag,specificity_evidence='repository IgDetective position and type within 1 bp' if flag!='unknown' else 'unavailable',source_id=r['index'],source_file='supplementary_data_1.csv',annotation_source=r['Source'],within_locus=st>=1 and en<=ln))
 # Human is absent from Supplementary Data1; distinguish its repository annotation.
 for g in genes:
  if g['track_id']!='HSA1':continue
  lc=locusidx['HSA1',g['locus']]
  paper.append(dict(track_id='HSA1',species=g['species'],haplotype='1',locus=g['locus'],facet=g['facet'],gene_family=g['locus']+g['gene_type'],gene_type=g['gene_type'],contig=g['contig'],genomic_start=lc['genomic_start0']+g['start'],genomic_end=lc['genomic_start0']+g['end'],start=g['start'],end=g['end'],display_start=g['display_start'],display_end=g['display_end'],strand=g['strand'],functional='unknown',is_species_specific=g['is_species_specific'],specificity_evidence='repository IsSpeciesSpecific',source_id=g['source_row'],source_file=g['source_file'],annotation_source='repository human T2T annotation; Productive='+g['productive'],within_locus=g['start']>=1 and g['end']<=lc['length']))
 # Human delta and shared TRA/DV annotations exist in the supplied NCBI BED,
 # although the plotting CSV omits nine delta segments. BED is 0-based half-open.
 bedfile='repo/aux_scripts/ncbi_bed_files/TRA_annotation.BED'
 lc=locusidx['HSA1','TRA']
 for r in csv.reader((RAW/bedfile).open(),delimiter='\t'):
  name=r[3]
  if not (name.startswith(('TRDV','TRDD','TRDJ')) or (name.startswith('TRAV') and 'DV' in name)):continue
  a,b=int(r[1])+1,int(r[2]);family='TRAV/DV' if name.startswith('TRAV') else name[:4];gt='V' if 'V' in family else family[-1]
  matches=[g for g in paper if g['track_id']=='HSA1' and g['locus']=='TRA' and a<=g['genomic_start']<=b]
  if matches:
   for g in matches:g['gene_family']=family;g['annotation_source']+='; family='+name+' from NCBI BED'
   continue
  st,en=a-lc['genomic_start0'],b-lc['genomic_start0']
  paper.append(dict(track_id='HSA1',species='Human',haplotype='1',locus='TRA',facet='TRA/TRD',gene_family=family,gene_type=gt,contig=r[0],genomic_start=a,genomic_end=b,start=st,end=en,display_start=st,display_end=en,strand=r[5],functional='unknown',is_species_specific='unknown',specificity_evidence='unavailable',source_id=name,source_file=bedfile,annotation_source='repository-supplied NCBI BED gene interval',within_locus=st>=1 and en<=lc['length']))
 write('paper_genes.tsv',paper)
 pc=[]
 for lc in loci:
  group=[p for p in paper if p['track_id']==lc['track_id'] and p['locus']==lc['locus']]
  for gt in sorted(set(p['gene_family'] for p in group)):
   sub=[p for p in group if p['gene_family']==gt];pc.append(dict(track_id=lc['track_id'],locus=lc['locus'],facet=lc['facet'],gene_family=gt,count=len(sub),functional=sum(p['functional']=='Functional' for p in sub),orf=sum(p['functional']=='ORF' for p in sub),unknown=sum(p['functional']=='unknown' for p in sub)))
 write('paper_counts.tsv',pc)
 qc={'source_gene_rows':len(genes),'loci':len(loci),'alignment_rows':len(links),'sv_block_rows':len(blocks),'verified_sequences':sum(g['sequence_verified'] for g in genes),'unverified_sequences':sum(not g['sequence_verified'] for g in genes),'corrected_locus_positions':sum(g['locus_pos_corrected'] for g in genes),'supplementary_rows':len(supp)}
 (OUT/'qc.json').write_text(json.dumps(qc,indent=2));print(json.dumps(qc),flush=True)
if __name__=='__main__':main()
