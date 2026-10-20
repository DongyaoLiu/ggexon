#!/usr/bin/env python3
"""Acquire small MHC curated source tables and normalize gene spans.

Run with the conda Python (requests installed). Uses cached sources by default;
--refresh re-fetches pinned GitHub content. No sequence downloads or alignments.
Outputs genes.tsv and genes_ed3_scope.tsv use 1-based closed coordinates.
"""
from __future__ import annotations
import argparse
import collections
import csv
import datetime
import hashlib
import json
from pathlib import Path
import shutil
import requests

ROOT = Path(__file__).resolve().parent
RAW = ROOT / 'source_metadata' / 'mhc_full'
OUT = ROOT / 'results' / 'mhc_full'
REPO = 'https://api.github.com/repos/marbl/T2T-Browser'
FIELDS = ['species_id', 'haplotype', 'track', 'locus', 'chr', 'start', 'end', 'strand', 'gene_name', 'functionality', 'source']
SPECIES = [('mPanTro3', 'PTR', 'Pan troglodytes'), ('mPanPan1', 'PPA', 'Pan paniscus'), ('mGorGor1', 'GGO', 'Gorilla gorilla'), ('mPonAbe1', 'PAB', 'Pongo abelii'), ('mPonPyg2', 'PPY', 'Pongo pygmaeus'), ('mSymSyn1', 'SSY', 'Symphalangus syndactylus')]
ED3_HUMAN = set('HLA-A HLA-B HLA-C HLA-E HLA-F HLA-G HLA-H HLA-J HLA-DRA HLA-DRB1 HLA-DRB5 HLA-DQA1 HLA-DQB1 HLA-DQA2 HLA-DQB2 HLA-DPA1 HLA-DPB1 HLA-DMA HLA-DMB HLA-DOA HLA-DOB'.split())
REFSEQ_URL = 'https://s3-us-west-2.amazonaws.com/human-pangenomics/T2T/CHM13/browser/bbi/chm13v2.0_RefSeq_Liftoff_v5.2.bb'


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def download(url, path, refresh):
    if refresh or not path.exists():
        r = requests.get(url, timeout=90)
        r.raise_for_status()
        path.write_bytes(r.content)
    return {'file': str(path.relative_to(ROOT)), 'url': url, 'bytes': path.stat().st_size, 'sha256': sha(path)}


def write_tsv(path, fields, rows):
    with path.open('w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=fields, delimiter='\t', lineterminator='\n')
        w.writeheader()
        w.writerows(rows)


def locus(name):
    # MHC-I includes the ape Gogo-OKO locus and named human HLA pseudogenes.
    suffix = name.split('-', 1)[1]
    return 'MHC_II' if suffix.startswith(('DR', 'DQ', 'DP', 'DM', 'DO')) else 'MHC_I'


def summaries(genes, suffix=''):
    windows, counts = [], []
    groups = collections.defaultdict(list)
    for g in genes:
        groups[(g['species_id'], g['haplotype'], g['track'], g['locus'], g['chr'])].append(g)
    for key, gs in groups.items():
        row = dict(zip(['species_id', 'haplotype', 'track', 'locus', 'chr'], key))
        windows.append(dict(row, start=min(g['start'] for g in gs), end=max(g['end'] for g in gs), n_genes=len(gs), boundary_definition='observed curated gene span'))
        c = collections.Counter(g['functionality'] for g in gs)
        counts.append(dict(row, functional=c['functional'], pseudogene=c['pseudogene'], total=len(gs)))
    write_tsv(OUT / f'windows{suffix}.tsv', ['species_id', 'haplotype', 'track', 'locus', 'chr', 'start', 'end', 'n_genes', 'boundary_definition'], windows)
    write_tsv(OUT / f'counts{suffix}.tsv', ['species_id', 'haplotype', 'track', 'locus', 'chr', 'functional', 'pseudogene', 'total'], counts)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--refresh', action='store_true')
    args = ap.parse_args()
    RAW.mkdir(parents=True, exist_ok=True)
    OUT.mkdir(parents=True, exist_ok=True)
    revision_path = RAW / 'github_revision.json'
    if not revision_path.exists():
        r = requests.get(REPO + '/commits/main', timeout=60)
        r.raise_for_status()
        revision_path.write_text(json.dumps({'sha': r.json()['sha']}, indent=2) + '\n')
    revision = json.loads(revision_path.read_text())['sha']
    prefix = f'https://raw.githubusercontent.com/marbl/T2T-Browser/{revision}/src/MHC/'
    sources = [download(prefix + 'README.md', RAW / 'README.upstream.md', args.refresh)]
    genes = []
    for sample, species_id, scientific_name in SPECIES:
        filename = sample + '_MHC_manual_v0.1.txt'
        url = prefix + filename
        sources.append(download(url, RAW / filename, args.refresh))
        sources.append(download(prefix + sample + '_MHC_manual_v0.1.bed', RAW / (sample + '_MHC_manual_v0.1.bed'), args.refresh))
        original_rows = list(csv.reader((RAW / filename).open(), delimiter='\t'))
        bed_rows = list(csv.reader((RAW / (sample + '_MHC_manual_v0.1.bed')).open(), delimiter='\t'))
        bed_keys = {(r[0], int(r[1]), int(r[2]), r[3], r[5]) for r in bed_rows}
        assert len(bed_keys) == len(original_rows)
        for row in original_rows:
            chrom, name, biotype, hap, start, end, strand = row
            start, end = int(start), int(end)
            assert start > 0 and end >= start and strand in ('+', '-')
            assert (chrom, start - 1, end, name, strand) in bed_keys, row
            assert biotype in ('Coding gene', 'Pseudogene')
            haplotype = {'One': 'h1', 'Two': 'h2'}[hap]
            genes.append(dict(zip(FIELDS, [species_id, haplotype, species_id + haplotype, locus(name), chrom, start, end, strand, name, 'functional' if biotype == 'Coding gene' else 'pseudogene', url])))
    # Existing interval extract has explicit RefSeq geneType in column 20.
    # Archive the entire small MHC extract and use only HLA coding/pseudogene rows.
    original = ROOT / 'source_metadata' / 'CHM13_MHC_gene_models.bigGenePred.bed'
    human = RAW / original.name
    if not human.exists() or args.refresh:
        shutil.copyfile(original, human)
    sources.append({'file': str(human.relative_to(ROOT)), 'url': REFSEQ_URL, 'bytes': human.stat().st_size, 'sha256': sha(human), 'acquisition': 'Archived existing regional bigGenePred extract; not a genome-wide download. BED0 geneType is field20.'})
    human_rows = collections.defaultdict(list)
    excluded = []
    for line in human.read_text().splitlines():
        r = line.split('\t')
        name, biotype = r[17], r[19]
        if not name.startswith('HLA-'):
            continue
        if biotype != 'protein_coding' and 'pseudogene' not in biotype:
            excluded.append({'gene_name': name, 'geneType': biotype})
            continue
        human_rows[(name, r[0], r[5], 'functional' if biotype == 'protein_coding' else 'pseudogene')].append(r)
    for (name, chrom, strand, functionality), rows in human_rows.items():
        genes.append(dict(zip(FIELDS, ['HSA', 'h1', 'HSAh1', locus(name), chrom, min(int(r[1]) for r in rows) + 1, max(int(r[2]) for r in rows), strand, name, functionality, REFSEQ_URL])))
    order = {s: n for n, s in enumerate(['HSA', 'PPA', 'GGO', 'PAB', 'PPY', 'PTR', 'SSY'])}
    genes.sort(key=lambda r: (order[r['species_id']], r['haplotype'], r['locus'], r['chr'], r['start'], r['end']))
    scope = [r for r in genes if r['species_id'] != 'HSA' or r['gene_name'] in ED3_HUMAN]
    assert {r['gene_name'] for r in scope if r['species_id'] == 'HSA'} == ED3_HUMAN
    assert len({r['track'] for r in genes}) == 13
    assert len(genes) == len({tuple(r[k] for k in FIELDS[:-1]) for r in genes})
    write_tsv(OUT / 'genes.tsv', FIELDS, genes)
    write_tsv(OUT / 'genes_ed3_scope.tsv', FIELDS, scope)
    bed0 = [dict(g, start=g['start'] - 1) for g in genes]
    write_tsv(OUT / 'genes.bed0.tsv', FIELDS, bed0)
    summaries(genes)
    summaries(scope, '_ed3_scope')
    metadata = {
        'retrieved': datetime.date.today().isoformat(), 'paper': 'https://www.nature.com/articles/s41586-025-08816-3',
        'github_revision': revision, 'coordinate_system': 'genes.tsv, genes_ed3_scope.tsv and windows*.tsv: 1-based closed; genes.bed0.tsv and upstream .bed: 0-based half-open; upstream .txt: 1-based closed',
        'conversion': 'Ape .txt preserved start/end; verified every record matches upstream BED start=original_start-1,end=original_end. Human RefSeq BED0 starts incremented by1; ends unchanged.',
        'functionality': 'functional means manual Coding gene or RefSeq protein_coding; pseudogene means explicit manual Pseudogene or RefSeq geneType containing pseudogene. No functionality inferred from color. Functional is an annotation category, not experimental validation.',
        'class_assignment': 'DR/DQ/DP/DM/DO symbol suffixes: MHC_II; remaining curated ape loci and HLA symbols: MHC_I. Gogo-OKO is MHC_I.',
        'human_aggregation': 'Transcript models collapsed by gene symbol/chromosome/strand/status; output spans are union of transcript model starts and ends.',
        'scope': 'All6ape curated manual tables; human CHM13 RefSeq HLA coding/pseudogene regional records. Separate genes_ed3_scope.tsv restricts human to8classI loci (A/B/C/E/F/G/H/J) and13classII coding loci, matching requested ED3 overview scope; allape curated labels retained.',
        'human_ed3_scope_symbols': sorted(ED3_HUMAN), 'human_excluded_noncoding_HLA_symbols': sorted({r['gene_name'] for r in excluded}),
        'species': {sid: {'sample': sample, 'scientific_name': name, 'assembly': sample + '_v2.0'} for sample, sid, name in SPECIES},
        'cautions': [
            'Counts represent source-curated genes, not genome-wide equal-ascertainment gene inventories. Ape manual tables omit some minor HLA-class pseudogene counterparts that RefSeq annotates in human.',
            'The absence of a gene label in these sources does not establish biological absence in an ape haplotype.',
            'Windows are the min/max observed selected gene spans for each class and track, not complete locus or synteny boundaries. No sequence alignment or genome-wide survey was performed.',
            'Manual gene spans approximate putative start-to-end-codon intervals; human RefSeq transcript-union spans may include untranslated regions. Their widths are not directly equivalent gene-length measurements.',
            'Some upstream manual browser records incorporate CAT-supported or renamed loci beyond populated manual cells in supplementary57/58; original browser tables are the source of truth for these outputs.'
        ], 'sources': sources,
        'outputs': {p.name: {'sha256': sha(p), 'bytes': p.stat().st_size} for p in sorted(OUT.glob('*.tsv'))},
        'verification': {'n_genes_full': len(genes), 'n_genes_ed3_scope': len(scope), 'n_tracks': 13, 'all_ape_records_match_bed0': True, 'unique_gene_records': True}
    }
    metadata['species']['HSA'] = {'sample': 'CHM13', 'scientific_name': 'Homo sapiens', 'assembly': 'T2T-CHM13v2.0'}
    (RAW / 'sources.json').write_text(json.dumps(metadata, indent=2) + '\n')
    print(json.dumps(metadata['verification'], indent=2))


if __name__ == '__main__':
    main()
