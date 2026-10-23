#!/usr/bin/env python3
"""Build real-source locus extracts for the integrated MHC detail example."""
from acquire import *
import argparse

HUMAN_BBI='https://s3-us-west-2.amazonaws.com/human-pangenomics/T2T/browser/CHM13/bbi/'
HUMAN_GENES='https://s3-us-west-2.amazonaws.com/human-pangenomics/T2T/CHM13/browser/bbi/chm13v2.0_RefSeq_Liftoff_v5.2.bb'
HUMAN_2BIT='https://s3-us-west-2.amazonaws.com/human-pangenomics/T2T/browser/CHM13/chm13v2.0.2bit'
PAN='https://genomeark.s3.amazonaws.com/species/Pan_troglodytes/mPanTro3/assembly_curated/'
PAN_METH=PAN+'epi/mPanTro3_v2.0.ONT_Guppy6.3.8_5mC.winnowmap_modkit_5mC_CpG_v1.0.bw'
HUMAN_METH='https://s3-us-west-2.amazonaws.com/human-pangenomics/T2T/CHM13/assemblies/annotation/regulation/chm13v2.0_CHM13_CpG_ont_guppy3.6.0_nanopolish0.13.2.bw'

def fasta_window(w,folder,twobits):
    out=folder/(w['id']+'.fa')
    if not out.exists():
        if w['twoBit'] not in twobits: twobits[w['twoBit']]=RemoteTwoBit(w['twoBit'])
        seq=twobits[w['twoBit']].sequence(w['chrom'],w['start0'],w['end0'])
        out.write_text('>'+w['id']+'\n'+'\n'.join(seq[i:i+80] for i in range(0,len(seq),80))+'\n')
    record(w['twoBit'],out,chrom=w['chrom'],start0=w['start0'],end0=w['end0'],assembly=w['assembly'],coordinate_system='0-based half-open source; FASTA local window')

def feature_rows(rows,w,kind):
    return [dict(track=w['id'],chr=w['chrom'],xmin=max(a,w['start0'])+1,xmax=min(z,w['end0']),kind=kind,label=rest.split('\t')[0]) for a,z,rest in rows if a<w['end0'] and z>w['start0']]

def save_windows(folder,windows):
    (folder/'windows.json').write_text(json.dumps(windows,indent=2)+'\n')
    write_tsv(folder/'windows.tsv',windows,['id','label','assembly','chrom','start0','end0'])

def mhc():
    folder=DATA/'mhc';folder.mkdir(exist_ok=True)
    windows=[dict(id='human',label='Human · CHM13',assembly='T2T-CHM13v2.0',chrom='chr6',start0=32270000,end0=32465000,twoBit=HUMAN_2BIT,gene=HUMAN_GENES,sd=HUMAN_BBI+'segDups_2024.bb'),dict(id='chimp_h1',label='Chimpanzee · haplotype 1',assembly='mPanTro3_v2.0',chrom='chr5_hap1_hsa6',start0=38225000,end0=38520000,twoBit=PAN+'mPanTro3_v2.0.2bit',gene=PAN+'gene/mPanTro3_refGene.bb',sd=PAN+'repeats/mPanTro3_v2.0.SD_v1.0.bb'),dict(id='chimp_h2',label='Chimpanzee · haplotype 2',assembly='mPanTro3_v2.0',chrom='chr5_hap2_hsa6',start0=36975000,end0=37270000,twoBit=PAN+'mPanTro3_v2.0.2bit',gene=PAN+'gene/mPanTro3_refGene.bb',sd=PAN+'repeats/mPanTro3_v2.0.SD_v1.0.bb')]
    manual=list(csv.DictReader((META/'MHC_manual_functionality_bed0.tsv').open(),delimiter='\t'))
    features=[];twobits={}
    for w in windows:
        print('MHC',w['id'],flush=True);fasta_window(w,folder,twobits)
        raw=[] if w['id']=='chimp_h2' else bb_entries(w['gene'],w['chrom'],w['start0'],w['end0'],folder/(w['id']+'.genes.bed'))
        models=genes_from_bed(raw,w['chrom'],w['id'])
        if w['id']=='human':models=[m for m in models if re.search(r'HLA-DR',m['symbol'])]
        else:
            curated=[m for m in manual if m['chrom']==w['chrom'] and 'DR' in m['gene'] and int(m['start'])<w['end0'] and int(m['end'])>w['start0']]
            keep=[]
            for m in models:
                overlaps=[c for c in curated if min(m['end'],int(c['end']))-max(m['start'],int(c['start']))>0.5*min(m['end']-m['start'],int(c['end'])-int(c['start']))]
                if overlaps:
                    c=max(overlaps,key=lambda c:min(m['end'],int(c['end']))-max(m['start'],int(c['start'])))
                    m['symbol']=c['gene'];keep.append(m)
            # Multiple RefSeq LOC transcripts can overlap same manually curated gene: retain one.
            models=[]
            for m in sorted(keep,key=lambda m:sum(z-a for a,z in m['exons']),reverse=True):
                if m['symbol'] not in [x['symbol'] for x in models]:models.append(m)
            for c in curated:features.append(dict(track=w['id'],chr=w['chrom'],xmin=int(c['start'])+1,xmax=int(c['end']),kind=c['functionality'],label=c['gene']))
        if w['id']=='chimp_h2':
            lines=['##gff-version 3']
            for k,c in enumerate(curated):
                lines.append(f"{w['chrom']}\tmanual_MHC\tgene\t{int(c['start'])+1}\t{c['end']}\t.\t{c['strand']}\t.\tID=manual_{k};gene_id=manual_{k};gene_name={c['gene']};Name={c['gene']}")
            (folder/(w['id']+'.gff3')).write_text('\n'.join(lines)+'\n')
        else:
            assert models, f'No MHC exon models for {w["id"]}'
            gff_write(folder/(w['id']+'.gff3'),models,w['chrom'])
        sd=bb_entries(w['sd'],w['chrom'],w['start0'],w['end0'],folder/(w['id']+'.sd.bed'))
        features.extend(feature_rows(sd,w,'Segmental duplication'))
        signal_extract(HUMAN_METH if w['id']=='human' else PAN_METH,w['chrom'],w['start0'],w['end0'],folder/(w['id']+'.methylation.tsv'),bin_bp=1000)
    save_windows(folder,windows)
    write_tsv(folder/'features.tsv',features,['track','chr','xmin','xmax','kind','label'])
    run_alignment(folder,windows,minlen=250,identity=75);finalize(folder.name)

if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('example',choices=['mhc']);args=parser.parse_args()
    globals()[args.example]()
