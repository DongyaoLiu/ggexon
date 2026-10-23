#!/usr/bin/env python3
"""Range-query exon models for continuous MHC windows; never fetch whole genomes.

Usage: python fetch_mhc_continuous_genes.py --windows windows.tsv --out DIR
Window fields: track,species_id,haplotype,chr,start0,end0,orientation (BED0).
Default CAT models are used where available, with CHM13 RefSeq for human.
Missing haplotype annotations are retained as explicit empty queries in manifest.
Native genomic coordinates and strands are retained regardless of orientation.
"""
from __future__ import annotations
import argparse
import collections
import concurrent.futures
import csv
import hashlib
import json
from pathlib import Path
import re
import pyBigWig

ROOT = Path(__file__).resolve().parent
METADATA = ROOT / 'source_metadata/mhc_continuous/annotation_sources.json'
BED_FIELDS = ['chrom','chromStart','chromEnd','name','score','strand','thickStart','thickEnd','itemRgb','blockCount','blockSizes','blockStarts']


def tsv(path, fields, rows):
    with path.open('w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter='\t', lineterminator='\n', extrasaction='ignore')
        w.writeheader()
        w.writerows(rows)


def extract(window, source, out):
    track = window['track']
    rec = next(h for h in source['haplotypes'] if h['track'] == track)
    if not rec['has_chromosome_models']:
        return [], [], {'track':track,'source_url':source['url'],'source_track':source['track_name'],'browser_chr':window['chr'],'start0':int(window['start0']),'end0':int(window['end0']),'n_models':0,'n_exons':0,'missing_annotation':True,'reason':'Browser source has no chromosome models for this haplotype; not biological absence.'}
    if window['chr'] != rec['browser_chrom']:
        raise ValueError(f"{track}: unexpected browser chromosome {window['chr']}")
    chrom = rec['annotation_chrom']
    lo, hi = int(window['start0']), int(window['end0'])
    assert 0 <= lo < hi
    rawpath = out / 'raw' / f'{track}.{source["track_name"]}.bed'
    url = source['url']
    if rawpath.exists():
        lines = rawpath.read_text().splitlines()
        cache = json.loads(rawpath.with_suffix('.json').read_text())
        if (cache['source_url'], cache['chrom'], cache['start0'], cache['end0']) != (url, chrom, lo, hi):
            raise ValueError(f'{track}: cached query differs; choose a new output directory')
    else:
        with pyBigWig.open(url) as b:
            entries = b.entries(chrom, lo, hi) or []
        lines = ['\t'.join([chrom, str(a), str(z), rest]) for a, z, rest in entries]
        rawpath.write_text('\n'.join(lines) + ('\n' if lines else ''))
        rawpath.with_suffix('.json').write_text(json.dumps({'source_url': url,'chrom': chrom,'browser_chrom': window['chr'],'start0':lo,'end0':hi},indent=2)+'\n')
    # Fields beyond BED12 are named by the actual autosql schema.
    schema_fields = re.findall(r'^\s*(?:\w+(?:\[[^\]]+\])?)\s+(\w+)\s*;', source['schema'], flags=re.M)
    models, exons = [], []
    for i, line in enumerate(lines, 1):
        values = line.split('\t')
        assert len(values) >= 12, (track, line)
        extra = dict(zip(schema_fields, values))
        bid = dict(zip(BED_FIELDS, values[:12]))
        count = int(bid['blockCount'])
        sizes = [int(x) for x in bid['blockSizes'].strip(',').split(',')]
        starts = [int(x) for x in bid['blockStarts'].strip(',').split(',')]
        assert count == len(sizes) == len(starts)
        model_id = f'{track}:{i}:{bid["name"]}'
        symbol = extra.get('name2') or extra.get('geneName2') or extra.get('geneName') or bid['name']
        common = dict(track=track,species_id=window['species_id'],haplotype=window['haplotype'],chr=window['chr'],annotation_chr=chrom,orientation=window.get('orientation','+'),model_id=model_id,gene_symbol=symbol,name2=extra.get('name2',''),gene_name=extra.get('geneName',''),gene_name2=extra.get('geneName2',''),gene_type=extra.get('geneType',extra.get('type','')),source_url=url,source_track=source['track_name'])
        models.append(dict(common,**bid))
        for n,(offset,size) in enumerate(zip(starts,sizes),1):
            a = int(bid['chromStart']) + offset
            z = a + size
            assert int(bid['chromStart']) <= a < z <= int(bid['chromEnd'])
            exons.append(dict(common,exon_index=n,start0=a,end0=z,strand=bid['strand'],cds_start0=min(z,max(a,int(bid['thickStart']))),cds_end0=max(min(z,max(a,int(bid['thickStart']))),min(z,int(bid['thickEnd']))),has_cds_overlap=max(a,int(bid['thickStart'])) < min(z,int(bid['thickEnd']))))
    return models,exons,{'track':track,'source_url':url,'source_track':source['track_name'],'annotation_chr':chrom,'browser_chr':window['chr'],'start0':lo,'end0':hi,'n_models':len(models),'n_exons':len(exons),'raw_file':str(rawpath),'raw_sha256':hashlib.sha256(rawpath.read_bytes()).hexdigest()}


def extract_job(job):
    window, source, out = job
    return extract(window, source, out)


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--windows',type=Path,required=True)
    p.add_argument('--out',type=Path,required=True)
    p.add_argument('--ape-source',choices=['catGene','catRefGene','refGene'],default='catRefGene')
    p.add_argument('--workers',type=int,default=4)
    args = p.parse_args()
    metadata = json.loads(METADATA.read_text())
    windows = list(csv.DictReader(args.windows.open(),delimiter='\t'))
    assert len({w['track'] for w in windows}) == len(windows), 'Expected one continuous window per track'
    args.out.mkdir(parents=True,exist_ok=True)
    (args.out/'raw').mkdir(exist_ok=True)
    jobs=[]
    for w in windows:
        kind = 'refGene' if w['species_id']=='HSA' else args.ape_source
        source = next(s for s in metadata['sources'] if s['species_id']==w['species_id'] and s['track_name']==kind)
        jobs.append((w,source,args.out))
    with concurrent.futures.ProcessPoolExecutor(args.workers) as pool:
        results = list(pool.map(extract_job,jobs))
    models = [r for result in results for r in result[0]]
    exons = [r for result in results for r in result[1]]
    common = ['track','species_id','haplotype','chr','annotation_chr','orientation','model_id','gene_symbol','name2','gene_name','gene_name2','gene_type','source_url','source_track']
    tsv(args.out/'models.tsv',common+BED_FIELDS,models)
    tsv(args.out/'exons.tsv',common+['exon_index','start0','end0','strand','cds_start0','cds_end0','has_cds_overlap'],exons)
    manifest={'window_file':str(args.windows.resolve()),'window_sha256':hashlib.sha256(args.windows.read_bytes()).hexdigest(),'coordinate_system':'All coordinates native BED0. Exons/models retain full native span, including segments beyond query boundaries; orientation is presentation metadata only.','annotation_source_metadata':str(METADATA),'queries':[r[2] for r in results],'n_models':len(models),'n_exons':len(exons),'cautions':metadata['cautions']}
    (args.out/'query_manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
    print(json.dumps({'n_tracks':len(windows),'n_models':len(models),'n_exons':len(exons)}))


if __name__=='__main__':
    main()
