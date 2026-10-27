#!/usr/bin/env python3
"""Select informative MHC haplotypes and directly align the compact track chain."""
from pathlib import Path
from collections import Counter
import csv, hashlib, json, shutil, subprocess
from mhc_continuous_utils import split_cigar

ROOT = Path(__file__).resolve().parent
SOURCE = ROOT / 'results/mhc_continuous'
OUT = ROOT / 'results/mhc_compact'
# A distant ape with matching head profiles adds breadth without a duplicate row;
# gorilla contributes both gene-content and functional-status differences.
TRACKS = ['SSYh1', 'GGOh2', 'GGOh1', 'HSAh1']

def read(path):
    with path.open() as handle:
        return list(csv.DictReader(handle, delimiter='\t'))

def write(path, rows, fields=None):
    with path.open('w') as handle:
        writer = csv.DictWriter(handle, fieldnames=fields or list(rows[0]), delimiter='\t', lineterminator='\n')
        writer.writeheader(); writer.writerows(rows)

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def head(row):
    return row['gene_name'].split('-', 1)[-1], row['functionality'], row['strand']

def main():
    OUT.mkdir(exist_ok=True)
    windows = read(SOURCE / 'windows.tsv')
    byid = {row['track']: row for row in windows}
    genes = read(ROOT / 'results/mhc_full/genes.tsv')
    audit, differences = [], []
    for species in dict.fromkeys(row['species_id'] for row in windows):
        profiles = {}
        counts = {}
        for hap in ('h1', 'h2'):
            track = species + hap
            rows = sorted((g for g in genes if g['track'] == track), key=lambda g: int(g['start']))
            profiles[hap] = [head(g) for g in rows]
            counts[hap] = len(rows)
        available = bool(profiles['h1'] and profiles['h2'])
        same = available and profiles['h1'] == profiles['h2']
        audit.append(dict(species_id=species, h1_heads=counts['h1'], h2_heads=counts['h2'],
                          paired_haplotypes_available=available, identical_ordered_head_profiles=same,
                          selected_tracks=','.join(t for t in TRACKS if t.startswith(species))))
        if available:
            a, b = Counter(profiles['h1']), Counter(profiles['h2'])
            for hap, delta in [('h1', a-b), ('h2', b-a)]:
                for (key, status, strand), copies in delta.items():
                    differences.append(dict(species_id=species, excess_in=hap, source_gene_key=key,
                                            functionality=status, native_strand=strand, excess_copies=copies))
    assert next(a for a in audit if a['species_id']=='SSY')['identical_ordered_head_profiles']
    assert not next(a for a in audit if a['species_id']=='GGO')['identical_ordered_head_profiles']
    assert {t[:3] for t in TRACKS} == {'HSA','GGO','SSY'}
    write(OUT / 'candidate-haplotype-audit.tsv', audit)
    write(OUT / 'haplotype-head-differences.tsv', differences)
    write(OUT / 'windows.tsv', [byid[t] for t in TRACKS])
    write(OUT / 'genes.tsv', [g for g in genes if g['track'] in TRACKS])
    all_blocks, provenance = [], []
    fields = ['tspecies','qspecies','tchr','qchr','tstart','tend','qstart','qend','strand','identity','source_paf','source_row','cigar_block']
    for target, query in zip(TRACKS, TRACKS[1:]):
        stem = target + '__' + query
        paf = OUT / (stem + '.paf')
        cached = SOURCE / paf.name
        command = ['minimap2','-x','asm20','-c','--eqx','--secondary=no','-t','2',
                   str(SOURCE/(target+'.fa')), str(SOURCE/(query+'.fa'))]
        for track in (target,query):
            assert (SOURCE/(track+'.fa')).exists(), f'Missing cached regional sequence: {track}'
        if not paf.exists():
            if cached.exists():
                shutil.copyfile(cached, paf)
            else:
                temporary = paf.with_suffix('.paf.tmp')
                with temporary.open('w') as handle, (OUT/(stem+'.log')).open('w') as log:
                    subprocess.run(command, stdout=handle, stderr=log, check=True)
                temporary.rename(paf)
        (OUT/(stem+'.command.json')).write_text(json.dumps(command,indent=2)+'\n')
        for rownum,line in enumerate(paf.read_text().splitlines(),1):
            f=line.split('\t'); qs,qe,ts,te=map(int,[f[2],f[3],f[7],f[8]])
            identity=int(f[9])/int(f[10])
            if identity < .70: continue
            tags={v[:2]:v[5:] for v in f[12:]}; qlen=int(f[1]); strand=f[4]
            oq0=qs if strand=='+' else qlen-qe
            for block,(a,b,c,d) in enumerate(split_cigar(tags['cg'],ts,oq0,strand),1):
                if min(b-a,d-c)<500: continue
                c,d=(c,d) if strand=='+' else (qlen-d,qlen-c)
                assert 0<=a<b<=int(byid[target]['length']) and 0<=c<d<=int(byid[query]['length'])
                all_blocks.append(dict(tspecies=target,qspecies=query,tchr='MHC',qchr='MHC',
                  tstart=a+1,tend=b,qstart=c+1,qend=d,strand=strand,identity=identity*100,
                  source_paf=paf.name,source_row=rownum,cigar_block=block))
        provenance.append(dict(target=target, query=query, file=paf.name, sha256=sha(paf),
          source='existing direct alignment' if cached.exists() else 'new direct minimap2 alignment',
          target_fasta_sha256=sha(SOURCE/(target+'.fa')),query_fasta_sha256=sha(SOURCE/(query+'.fa')),
          command=command))
        print(stem, sum(z['tspecies']==target and z['qspecies']==query for z in all_blocks), 'ribbons', flush=True)
    write(OUT/'links.tsv', all_blocks, fields)
    manifest=dict(tracks_top_to_bottom=TRACKS, species=['Human','Gorilla','Siamang'],
      selection='Human CHM13 baseline; gorilla has head identity/content/status differences; siamang has identical ordered heads and is represented by h1.',
      human_limitation='Only CHM13 is available; no second human haplotype is inferred.',
      equivalence='Ordered gene identity, copy multiplicity, source functional status and strand; native coordinate shifts alone do not require a second haplotype.',
      biological_scope='Differences describe the sampled assemblies and curated source annotations, not fixed species-wide differences or confirmed gene absence.',
      common_display_span_bp=max(int(w['length']) for w in windows),
      alignments=provenance, ribbon_count=len(all_blocks))
    (OUT/'selection.json').write_text(json.dumps(manifest,indent=2)+'\n')
    print('PASS: three species; four native tracks; direct alignments; candidate profiles audited.')

if __name__=='__main__': main()
