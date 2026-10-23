#!/usr/bin/env python3
"""Extract unchanged ENCODE ENCFF607FEG signal for human AS3MT (GRCh38).

Python standard library only. Downloads 23,466 bytes with HTTP Range requests,
not the entire 129,625,014-byte source file. Exact byte locations below are
specific to the source artifact whose ENCODE full-file MD5 is
9320986050d387767f17042dcb2ea07f. The full-file MD5 cannot be checked from ranges.

Example (from the repository root):
  python3 data-raw/as3mt/prepare-rnaseq-coverage.py --out-dir /tmp/as3mt-reproduced \
    --verify-tsv inst/extdata/as3mt/human-rnaseq-ENCFF607FEG.tsv

--cached-dir /tmp reuses the previously downloaded source bytes without network.
"""
import argparse
import csv
import datetime
import hashlib
import json
import math
from pathlib import Path
import struct
import urllib.request
import zlib

SOURCE_URL = 'https://encode-public.s3.amazonaws.com/2022/05/13/c0826239-672c-4d5f-a32b-bd30b5c59e26/ENCFF607FEG.bigWig'
SOURCE_SIZE = 129625014
SOURCE_MD5 = '9320986050d387767f17042dcb2ea07f'
REGION_START, REGION_END = 102869470, 102901899  # 1-based closed
RANGES = [
    ('header', 0, 2549, None),
    ('rtree_root', 81482931, 81491170, 'as3mt-index-root.bin'),
    ('rtree_leaf', 81562891, 81571086, 'as3mt-index-leaf.bin'),
    ('signal_block', 10310187, 10314666, 'as3mt-data-block.bin'),
]


def fetch_range(start, end):
    request = urllib.request.Request(SOURCE_URL, headers={'Range': f'bytes={start}-{end}'})
    with urllib.request.urlopen(request, timeout=60) as response:
        content_range = response.headers.get('Content-Range')
        if response.status != 206 or content_range != f'bytes {start}-{end}/{SOURCE_SIZE}':
            raise RuntimeError(f'Unexpected range response: {response.status}, {content_range!r}')
        result = response.read(end - start + 2)
    if len(result) != end - start + 1:
        raise RuntimeError('Incorrect byte-range length')
    return result


def extract(block):
    data = zlib.decompress(block)
    chrom_id, block_start, block_end, step, span, kind, reserved, count = struct.unpack_from('<IIIIIBBH', data)
    assert (chrom_id, block_start, block_end, kind, count) == (1, 102863516, 102919707, 1, 1024)
    assert len(data) == 24 + 12 * count
    rows = []
    for index in range(count):
        start0, end0, score = struct.unpack_from('<IIf', data, 24 + 12 * index)
        if start0 < REGION_END and end0 > REGION_START - 1:
            rows.append(('chr10', max(start0 + 1, REGION_START), min(end0, REGION_END), score))
    assert len(rows) == 352
    assert sum(end - start + 1 for _, start, end, _ in rows) == 11224
    return rows


def verify(rows, path):
    with path.open() as handle:
        observed = [(r['seqnames'], int(r['start']), int(r['end']), float(r['score']))
                    for r in csv.DictReader(handle, delimiter='\t')]
    assert len(rows) == len(observed), (len(rows), len(observed))
    for expected, actual in zip(rows, observed):
        assert expected[:3] == actual[:3] and math.isclose(expected[3], actual[3], rel_tol=1e-13, abs_tol=1e-13), (expected, actual)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out-dir', type=Path, required=True)
    parser.add_argument('--cached-dir', type=Path, help='Use existing /tmp/as3mt-*.bin files; skips header fetch because exact source header has already been inspected.')
    parser.add_argument('--verify-tsv', type=Path)
    args = parser.parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)
    chunks = {}
    for label, start, end, cached_name in RANGES:
        if args.cached_dir:
            if cached_name is None:
                continue
            chunk = (args.cached_dir / cached_name).read_bytes()
            assert len(chunk) == end - start + 1
        else:
            chunk = fetch_range(start, end)
        chunks[label] = chunk
    if 'header' in chunks:
        header = struct.unpack_from('<IHHQQQHHQQIQ', chunks['header'])
        assert (header[0], header[3], header[4], header[5]) == (0x888FFC26, 344, 2550, 81482931)
        # Chromosome B+ tree entry at key chr10 must carry id 1.
        tree = chunks['header'][344:]
        magic, block_size, key_size, value_size, item_count, reserved = struct.unpack_from('<IIIIQQ', tree)
        leaf, reserved, count = struct.unpack_from('<BBH', tree, 32)
        assert magic == 0x78CA8C91 and leaf == 1 and value_size == 8
        chromosomes = {}
        for i in range(count):
            offset = 36 + i * (key_size + value_size)
            name = tree[offset:offset+key_size].rstrip(b'\x00').decode()
            chromosomes[name] = struct.unpack_from('<II', tree, offset+key_size)
        assert chromosomes['chr10'] == (1, 133797422)
    # Verify both relevant R-tree nodes identify the fetched locus block.
    root = chunks['rtree_root']
    root_header = struct.unpack_from('<IIQIIIIQII', root)
    assert root_header[0] == 0x2468ACE0
    leaf, reserved, count = struct.unpack_from('<BBH', root, 48)
    assert leaf == 0
    relevant_children = []
    for i in range(count):
        a,b,c,d,offset = struct.unpack_from('<IIIIQ', root, 52 + i * 24)
        if (a,b) < (1,REGION_END) and (c,d) > (1,REGION_START-1):
            relevant_children.append(offset)
    assert relevant_children == [81562891]
    leaf_data = chunks['rtree_leaf']
    leaf, reserved, count = struct.unpack_from('<BBH', leaf_data)
    assert leaf == 1
    relevant_blocks = []
    for i in range(count):
        a,b,c,d,offset,size = struct.unpack_from('<IIIIQQ', leaf_data, 4 + i * 32)
        if (a,b) < (1,REGION_END) and (c,d) > (1,REGION_START-1):
            relevant_blocks.append((offset,size))
    assert relevant_blocks == [(10310187,4480)]
    rows = extract(chunks['signal_block'])
    output = args.out_dir / 'human-rnaseq-ENCFF607FEG.tsv'
    with output.open('w', newline='') as handle:
        writer = csv.writer(handle, delimiter='\t', lineterminator='\n')
        writer.writerow(['seqnames','start','end','score'])
        writer.writerows(rows)
    if args.verify_tsv:
        verify(rows, args.verify_tsv)
    provenance = {
        'accession':'ENCFF607FEG', 'experiment_accession':'ENCSR394HJK',
        'assembly':'GRCh38', 'seqnames':'chr10', 'start':REGION_START, 'end':REGION_END,
        'coordinate_system':'1-based closed', 'gene':'AS3MT', 'gene_strand':'+',
        'source_url':SOURCE_URL, 'source_metadata_url':'https://www.encodeproject.org/files/ENCFF607FEG/?format=json',
        'experiment_url':'https://www.encodeproject.org/experiments/ENCSR394HJK/',
        'biosample_summary':'Homo sapiens dorsolateral prefrontal cortex tissue male adult (83 years)',
        'assay':'total RNA-seq', 'biological_replicate':1,
        'output_type':'plus strand signal of unique reads',
        'score_handling':'Source float32 values unchanged; no normalization, interpolation, binning, or synthetic signal added.',
        'score_units':'Not established from inspected file metadata; do not label raw read depth.',
        'expected_full_file_md5':SOURCE_MD5, 'full_file_md5_verified':False,
        'source_full_file_size':SOURCE_SIZE,
        'byte_ranges':[{'name':name,'start':start,'end':end} for name,start,end,_ in RANGES],
        'fetched_chunk_sha256':{name:hashlib.sha256(data).hexdigest() for name,data in chunks.items()},
        'n_intervals':len(rows), 'n_covered_bases':sum(end-start+1 for _,start,end,_ in rows),
        'min_score':min(row[3] for row in rows), 'max_score':max(row[3] for row in rows),
        'verification':'Matching source R-tree nodes and bedGraph block structure validated; ' + ('all intervals/scores matched against ' + str(args.verify_tsv) if args.verify_tsv else 'no external TSV comparison requested'),
        'prepared_utc':datetime.datetime.now(datetime.timezone.utc).isoformat(),
        'caveats':'Single adult human DLPFC sample, separate from the AS3MT paper organoid conditions. Coverage alone cannot establish exon-skipping junctions or isoform abundance.'
    }
    (args.out_dir / 'human-rnaseq-ENCFF607FEG-preparation.json').write_text(json.dumps(provenance, indent=2)+'\n')
    print(f'Extracted {len(rows)} source intervals to {output}')
    if args.verify_tsv:
        print(f'All coordinates and scores matched {args.verify_tsv}')

if __name__ == '__main__':
    main()
