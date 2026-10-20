#!/usr/bin/env python3
"""Same-species Liftoff context transfer; all outputs retain inferred provenance.

Input primary context representatives are the archived pre-integration renderer
selection. Curated MHC models are never transferred or altered. Regional FASTAs
are exactly the sequences used for the plotted nucleotide ribbons. Native BED0
coordinates are reconstructed after mapping, including reversed siamang tracks.
"""
from pathlib import Path
import argparse,collections,csv,hashlib,json,subprocess,sys,shutil
from urllib.parse import quote,unquote
ROOT=Path(__file__).resolve().parent
B=ROOT/'results/mhc_continuous'
PAIRS={'SSYh2':'SSYh1','PABh2':'PABh1','PPYh2':'PPYh1','GGOh2':'GGOh1','PPAh1':'PPAh2','PTRh2':'PTRh1'}
EVIDENCE='same-species Liftoff projection'
MIN_COVERAGE=MIN_IDENTITY=.95

def rd(path):
    return list(csv.DictReader(path.open(),delimiter='\t'))

def wr(path,rows,fields):
    with path.open('w',newline='') as h:
        w=csv.DictWriter(h,fieldnames=fields,delimiter='\t',lineterminator='\n',extrasaction='ignore');w.writeheader();w.writerows(rows)

def eligible_context(row):
    return row['native_mhc_model']=='FALSE' and not row['gene_symbol'].startswith(('HLA-','Patr-','Papa-','Gogo-','Poab-','Popy-','Sysy-'))

def native_span(a,b,strand,window):
    lo,hi=int(window['start0']),int(window['end0'])
    if not (1<=a<=b<=hi-lo) or strand not in ('+','-'):
        raise ValueError(f'Invalid regional interval: {a},{b},{strand}')
    if window['orientation']=='+':return lo+a-1,lo+b,strand
    if window['orientation']=='-':return hi-b,hi-a+1,'-' if strand=='+' else '+'
    raise ValueError('Invalid window orientation')

def parse_liftoff(text,window,sources):
    features=[]
    for line in text.splitlines():
        if not line or line.startswith('#'):continue
        f=line.split('\t')
        if len(f)!=9 or f[0]!=window['track']:raise ValueError('Unexpected Liftoff chromosome/format')
        attr={unquote(k):unquote(v) for k,v in (s.split('=',1) for s in f[8].split(';') if '=' in s)}
        a,b,strand=native_span(int(f[3]),int(f[4]),f[6],window)
        features.append(dict(type=f[2],a=a,b=b,strand=strand,attr=attr))
    genes={f['attr']['ID']:f for f in features if f['type']=='gene'}
    transcripts={f['attr']['ID']:f['attr']['Parent'] for f in features if f['type'] in ('mRNA','transcript')}
    children=collections.defaultdict(list)
    for f in features:
        if f['type']=='exon':
            for parent in f['attr']['Parent'].split(','):
                children[transcripts.get(parent,parent)].append(f)
    models=[];exons=[];audit=[]
    for gid,g in genes.items():
        if gid not in sources:raise ValueError(f'Unknown projected source ID {gid}')
        src=sources[gid];a,b=g['a'],g['b'];attr=g['attr'];cov=float(attr['coverage']);ident=float(attr['sequence_ID'])
        accepted=(cov>=MIN_COVERAGE and ident>=MIN_IDENTITY and attr.get('partial_mapping','False')!='True' and attr.get('low_identity','False')!='True')
        rec=dict(track=window['track'],source_track=src['track'],source_model_id=src['model_id'],gene_symbol=src['gene_symbol'],coverage=cov,sequence_identity=ident,status='accepted' if accepted else 'below_threshold',native_start0=a,native_end0=b,strand=g['strand'])
        audit.append(rec)
        if not accepted:continue
        blocks=sorted(set((f['a'],f['b']) for f in children[gid]))
        if not blocks:raise ValueError(f'{gid}: accepted model lacks exons')
        if any(not (a<=x<y<=b) for x,y in blocks):raise ValueError(f'{gid}: exon outside gene')
        if any(f['strand']!=g['strand'] for f in children[gid]):raise ValueError(f'{gid}: inconsistent strand')
        mid=f'{window["track"]}:liftoff:{gid}'
        common=dict(track=window['track'],species_id=window['species_id'],haplotype=window['haplotype'],chr=window['chr'],annotation_chr=window['chr'],orientation=window['orientation'],model_id=mid,gene_symbol=src['gene_symbol'],name2=src['gene_symbol'],gene_name=src['gene_symbol'],gene_name2=src['gene_symbol'],gene_type=src['gene_type'],source_url=src['source_url'],source_track='Liftoff_context',annotation_evidence=EVIDENCE,projection_source_model_id=src['model_id'],projection_source_track=src['track'],projection_coverage=cov,projection_identity=ident)
        models.append(dict(common,chrom=window['chr'],chromStart=a,chromEnd=b,name=gid,score=0,strand=g['strand'],thickStart=a,thickEnd=a,itemRgb='0',blockCount=len(blocks),blockSizes=''.join(f'{y-x},' for x,y in blocks),blockStarts=''.join(f'{x-a},' for x,y in blocks)))
        for i,(x,y) in enumerate(blocks,1):exons.append(dict(common,exon_index=i,start0=x,end0=y,strand=g['strand'],cds_start0=x,cds_end0=x,has_cds_overlap=False))
    for gid,src in sources.items():
        if gid not in genes:audit.append(dict(track=window['track'],source_track=src['track'],source_model_id=src['model_id'],gene_symbol=src['gene_symbol'],coverage='',sequence_identity='',status='unmapped_or_below_threshold',native_start0='',native_end0='',strand=''))
    return models,exons,audit

def main():
    p=argparse.ArgumentParser(description=__doc__);p.add_argument('--liftoff',required=True);args=p.parse_args()
    out=B/'gene_models_with_projected_context';out.mkdir(exist_ok=True)
    source_file=B/'figures/before-alt-context/representative-models.tsv'
    source_rows=rd(source_file)
    excluded=[r for r in source_rows if r['track'] in PAIRS.values() and r['native_mhc_model']=='FALSE' and not eligible_context(r)]
    wr(out/'excluded-mhc-symbols.tsv',excluded,list(source_rows[0]))
    reps=[r for r in source_rows if eligible_context(r) and r['track'] in PAIRS.values()]
    windows={r['track']:r for r in rd(B/'windows.tsv')}
    models=rd(B/'gene_models/models.tsv');exons=rd(B/'gene_models/exons.tsv')
    for r in models+exons:r.update(annotation_evidence='released annotation',projection_source_model_id='',projection_source_track='',projection_coverage='',projection_identity='')
    by_model=collections.defaultdict(list)
    for e in exons:by_model[e['model_id']].append(e)
    tool_python=Path(args.liftoff).resolve().parent/'python'
    tool_info=json.loads(subprocess.check_output([str(tool_python),'-c',"import importlib.metadata as m,json;d=m.distribution('Liftoff');print(json.dumps(dict(version=d.version,direct_url=json.loads(d.read_text('direct_url.json') or '{}'))))"],text=True))
    tool_info['minimap2_version']=subprocess.check_output(['minimap2','--version'],text=True).strip()
    tool_info['minimap2_sha256']=hashlib.sha256(Path(shutil.which('minimap2')).read_bytes()).hexdigest()
    audits=[];runs=[]
    for target,source in PAIRS.items():
        d=out/target;d.mkdir(exist_ok=True);w=windows[source];sources={f'ctx{i:04d}':r for i,r in enumerate([r for r in reps if r['track']==source],1)}
        gff=['##gff-version 3']
        for gid,r in sources.items():
            a,b=int(r['chromStart']),int(r['chromEnd']);strand=r['strand']
            def regional(x,y):
                return (x-int(w['start0'])+1,y-int(w['start0'])) if w['orientation']=='+' else (int(w['end0'])-y+1,int(w['end0'])-x)
            start,end=regional(a,b)
            if not 1<=start<=end<=int(w['length']):raise ValueError('Source gene crosses regional FASTA boundary')
            if w['orientation']=='-':strand='-' if strand=='+' else '+'
            def line(kind,x,y,attr):return '\t'.join(map(str,[source,'CAT_context',kind,x,y,'.',strand,'.',attr]))
            gff.extend([line('gene',start,end,f'ID={gid};Name={quote(r["gene_symbol"])}'),line('mRNA',start,end,f'ID={gid}.t1;Parent={gid}')])
            for i,e in enumerate(by_model[r['model_id']],1):
                x,y=regional(int(e['start0']),int(e['end0']));gff.append(line('exon',x,y,f'ID={gid}.e{i};Parent={gid}.t1'))
        gp=d/'source.gff3';gp.write_text('\n'.join(gff)+'\n');(d/'source_models.json').write_text(json.dumps(sources,indent=2)+'\n')
        result=d/'projected.gff3'
        cmd=[str(Path(args.liftoff).resolve()),str(B/(target+'.fa')),str(B/(source+'.fa')),'-g',str(gp),'-o',str(result),'-u',str(d/'unmapped.txt'),'-dir',str(d/'intermediates'),'-p','2','-a',str(MIN_COVERAGE),'-s',str(MIN_IDENTITY),'-exclude_partial']
        stamp=dict(command=cmd,tools=tool_info,source_window=windows[source],target_window=windows[target],source_gff_sha256=hashlib.sha256(gp.read_bytes()).hexdigest(),target_fasta_sha256=hashlib.sha256((B/(target+'.fa')).read_bytes()).hexdigest(),source_fasta_sha256=hashlib.sha256((B/(source+'.fa')).read_bytes()).hexdigest())
        cache=d/'command.json'
        if result.exists() and (not cache.exists() or json.loads(cache.read_text())!=stamp):raise ValueError('Existing projection has different inputs; choose fresh output')
        if not result.exists():
            with (d/'liftoff.log').open('w') as h:subprocess.run(cmd,stdout=h,stderr=h,check=True)
            cache.write_text(json.dumps(stamp,indent=2)+'\n')
        mm,ee,aa=parse_liftoff(result.read_text(),windows[target],sources);models+=mm;exons+=ee;audits+=aa
        run=dict(target=target,source=source,source_context_genes=len(sources),accepted=len(mm),not_transferred=len(sources)-len(mm),**stamp);runs.append(run)
        print(json.dumps({k:run[k] for k in ['target','source','source_context_genes','accepted','not_transferred']}),flush=True)
    extra=['annotation_evidence','projection_source_model_id','projection_source_track','projection_coverage','projection_identity']
    model_fields=list(rd(B/'gene_models/models.tsv')[0])+extra;exon_fields=list(rd(B/'gene_models/exons.tsv')[0])+extra
    wr(out/'models.tsv',models,model_fields);wr(out/'exons.tsv',exons,exon_fields)
    wr(out/'projection-audit.tsv',audits,list(audits[0]))
    manifest=dict(method='Same-species context-only Liftoff; no extra-copy search; no functional status inferred',minimum_coverage=MIN_COVERAGE,minimum_identity=MIN_IDENTITY,coordinate_system='native BED0; source and target regional FASTAs are display-oriented, reflected back once for siamang',source_representatives=str(source_file),source_representatives_sha256=hashlib.sha256(source_file.read_bytes()).hexdigest(),runs=runs,limitations=['Projected models are inferred, not released consensus annotations.','Only the bounded MHC interval is searched; unmapped models are not gene absence.','Novel or haplotype-specific context genes absent from the primary source cannot be discovered.','One source representative per annotated context locus; exons transferred without CDS or functionality inference.'])
    (out/'projection_manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')

if __name__=='__main__':main()
