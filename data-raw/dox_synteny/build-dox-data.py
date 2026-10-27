#!/usr/bin/env python3
"""Prepare the three-species Dox example using public, assembly-matched inputs.

Python standard library, LASTZ, and minimap2 are required. RNA-seq headers
and compatible locus windows are fetched with verified HTTP byte ranges.
Original source intervals/scores are retained. The two flanking gene models
are the same D. melanogaster RefSeq transcripts mapped onto each assembly by
UCSC GenArk; these are explicitly comparative models, not native annotations.
Dox coding exons are mapped from the published CDS with 100% identity.
The incompatible Dsim BigWigs are recorded but excluded. Dsim signal instead
comes from a verified complete-library remapping prepared by
remap-dsim-rnaseq.py. The two outer species retain deposited signal.
"""

import argparse
import concurrent.futures
import csv
import gzip
import hashlib
import json
from pathlib import Path
import io
import re
import shutil
import struct
import subprocess
import time
import urllib.error
import urllib.parse
import urllib.request
import zlib
import zipfile
import xml.etree.ElementTree as ET


LOCI = [
    dict(species="Dmau", name="D. mauritiana", assembly="ASM438214v1",
         accession="GCF_004382145.1", chrom="NC_046672.1", start=9389704,
         end=9438333, sample="GSM5611558", geo_group="GSM5611nnn"),
    dict(species="Dsim", name="D. simulans", assembly="ASM438218v1",
         accession="GCA_004382185.1", chrom="CM015611.1", start=9233164,
         end=9276944, sample="GSM7039340", geo_group="GSM7039nnn"),
    dict(species="Dsech", name="D. sechellia", assembly="ASM438219v1",
         accession="GCF_004382195.1", chrom="NC_045954.1", start=9396969,
         end=9444821, sample="GSM5611560", geo_group="GSM5611nnn"),
]
TRANSCRIPTS = {"NM_132346.3": "CG42797", "NM_132348.3": "Ptpmeg2"}
PAPER = "https://doi.org/10.1038/s41559-021-01592-z"
SUPPLEMENT = ("https://media.springernature.com/original/springer-static/esm/"
              "art%3A10.1038%2Fs41559-021-01592-z/MediaObjects/"
              "41559_2021_1592_MOESM2_ESM.xlsx")


def fetch(url, cache, start=None, size=None):
    key = hashlib.sha256(f"{url}:{start}:{size}".encode()).hexdigest()
    path = cache / key
    if path.exists():
        return path.read_bytes()
    headers = {"User-Agent": "ggexon-Dox-visualization/1.0", "Accept-Encoding": "identity"}
    if start is not None:
        headers["Range"] = f"bytes={start}-{start + size - 1}"
    for attempt in range(4):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=headers), timeout=45) as r:
                if start is not None:
                    content_range = r.headers.get("Content-Range", "")
                    expected = f"bytes {start}-{start + size - 1}/"
                    if r.status != 206 or not content_range.startswith(expected):
                        raise RuntimeError(f"Server did not honor byte range: {r.status} {content_range}")
                data = r.read(size + 1 if size else -1)
                if size and len(data) != size:
                    raise RuntimeError(f"Incorrect byte count: expected {size}, received {len(data)}")
            path.write_bytes(data)
            return data
        except (urllib.error.URLError, TimeoutError):
            if attempt == 3:
                raise
            time.sleep(1 + attempt)


class BigWigWindow:
    """Read the exact bedGraph/variableStep/fixedStep records in a remote locus."""

    def __init__(self, url, cache):
        self.url, self.cache, self.ranges = url, cache, []
        h = struct.unpack("<IHHQQQHHQQIQ", self.read(0, 64))
        if h[0] != 0x888FFC26:
            raise ValueError("Not a little-endian BigWig file")
        self.tree_offset, self.index_offset, self.compressed = h[3], h[5], h[10] != 0

    def read(self, start, size):
        data = fetch(self.url, self.cache, start, size)
        self.ranges.append(dict(start=start, size=size, sha256=hashlib.sha256(data).hexdigest()))
        return data

    def chromosomes(self):
        magic, _, key_size, value_size, _, _ = struct.unpack("<IIIIQQ", self.read(self.tree_offset, 32))
        if magic != 0x78CA8C91 or value_size != 8:
            raise ValueError("Unexpected chromosome B+ tree")
        result = {}

        def visit(offset):
            leaf, _, count = struct.unpack("<BBH", self.read(offset, 4))
            node = self.read(offset + 4, count * (key_size + 8))
            for i in range(count):
                pos = i * (key_size + 8)
                name = node[pos:pos + key_size].rstrip(b"\0").decode()
                if leaf:
                    result[name] = struct.unpack_from("<II", node, pos + key_size)
                else:
                    visit(struct.unpack_from("<Q", node, pos + key_size)[0])

        visit(self.tree_offset + 32)
        return result

    def intervals(self, chrom_id, start0, end0):
        header = struct.unpack("<IIQIIIIQII", self.read(self.index_offset, 48))
        if header[0] != 0x2468ACE0:
            raise ValueError("Unexpected BigWig interval R-tree")
        blocks = set()

        def overlaps(a, b, c, d):
            return (a, b) < (chrom_id, end0) and (c, d) > (chrom_id, start0)

        def visit(offset):
            leaf, _, count = struct.unpack("<BBH", self.read(offset, 4))
            width = 32 if leaf else 24
            node = self.read(offset + 4, count * width)
            for i in range(count):
                a, b, c, d, child = struct.unpack_from("<IIIIQ", node, i * width)
                if overlaps(a, b, c, d):
                    if leaf:
                        blocks.add((child, struct.unpack_from("<Q", node, i * width + 24)[0]))
                    else:
                        visit(child)

        visit(self.index_offset + 48)
        result = []
        for offset, size in sorted(blocks):
            raw = self.read(offset, size)
            raw = zlib.decompress(raw) if self.compressed else raw
            pos = 0
            while pos < len(raw):
                chrom, section_start, _, step, span, kind, _, count = struct.unpack_from("<IIIIIBBH", raw, pos)
                pos += 24
                for i in range(count):
                    if kind == 1:
                        a, b, score = struct.unpack_from("<IIf", raw, pos)
                        pos += 12
                    elif kind == 2:
                        a, score = struct.unpack_from("<If", raw, pos)
                        b = a + span
                        pos += 8
                    elif kind == 3:
                        a = section_start + i * step
                        b = a + span
                        score, = struct.unpack_from("<f", raw, pos)
                        pos += 4
                    else:
                        raise ValueError(f"Unknown BigWig section type {kind}")
                    if chrom == chrom_id and a < end0 and b > start0:
                        result.append((max(a, start0) + 1, min(b, end0), score))
            if pos != len(raw):
                raise ValueError("Incorrect decoded block length")
        result.sort()
        if any(a[1] >= b[0] for a, b in zip(result, result[1:])):
            raise ValueError("Source BigWig records overlap")
        return result


def api(locus, endpoint, **params):
    acc = locus["accession"]
    digits = acc.split("_")[1].split(".")[0]
    hub = f"https://hgdownload.soe.ucsc.edu/hubs/{acc[:3]}/{digits[:3]}/{digits[3:6]}/{digits[6:9]}/{acc}/hub.txt"
    params = dict(hubUrl=hub, genome=acc, chrom=locus["chrom"],
                  start=locus["start"] - 1, end=locus["end"], **params)
    return f"https://api.genome.ucsc.edu/getData/{endpoint}?" + urllib.parse.urlencode(params)


def write_tsv(path, rows, fields=None):
    if fields is None:
        fields = list(rows[0])
    opener = gzip.open(path, "wt", newline="") if path.suffix == ".gz" else path.open("w", newline="")
    with opener as h:
        w = csv.DictWriter(h, fields, delimiter="\t", lineterminator="\n")
        w.writeheader()
        w.writerows(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    root = Path(__file__).resolve().parents[2]
    parser.add_argument("--out-dir", type=Path, default=root / "inst/extdata/dox_synteny")
    parser.add_argument("--cache-dir", type=Path, default=Path(__file__).resolve().parent / "downloads")
    args = parser.parse_args()
    destination, cache = args.out_dir, args.cache_dir
    out = cache / "prepared"
    out.mkdir(parents=True, exist_ok=True)
    cache.mkdir(parents=True, exist_ok=True)
    genes, features, coverage, sources, genomes, chromosome_lengths = [], [], [], [], {}, {}
    for locus in LOCI:
        species = locus["species"]
        seq_url = api(locus, "sequence")
        payload = json.loads(fetch(seq_url, cache))
        dna = payload["dna"].upper()
        if len(dna) != locus["end"] - locus["start"] + 1:
            raise ValueError("Incorrect genome window length")
        genomes[species] = dna
        (out / f"{species}.fa").write_text(f">{species}\n" + "\n".join(dna[i:i+80] for i in range(0, len(dna), 80)) + "\n")
        anno_url = api(locus, "track", track="xenoRefGene")
        annotation_response = json.loads(fetch(anno_url, cache))
        anno = annotation_response["xenoRefGene"]
        chromosome_lengths[species] = annotation_response["chromSize"]
        for tx, name in TRANSCRIPTS.items():
            records = [r for r in anno if r["name"].split(".")[0] == tx.split(".")[0]]
            if len(records) != 1:
                raise ValueError(f"Expected exactly one {tx} mapping in {species}")
            r = records[0]
            gene = dict(species=species, seqnames="X", gene_id=f"{species}_{name}",
                        gene_name=name, transcript_id=f"{species}_{tx}",
                        start=r["chromStart"]+1, end=r["chromEnd"], strand=r["strand"],
                        model_source="UCSC GenArk mapped RefSeq", source_transcript=r["name"])
            genes.append(gene)
            for i, (size, offset) in enumerate(zip(r["blockSizes"].strip(",").split(","), r["chromStarts"].strip(",").split(","))):
                a, b = r["chromStart"] + int(offset), r["chromStart"] + int(offset) + int(size)
                features.append(dict(**gene, type="exon", feature_start=a+1, feature_end=b, exon_number=i+1))
                c, d = max(a, r["thickStart"]), min(b, r["thickEnd"])
                if c < d:
                    features.append(dict(**gene, type="CDS", feature_start=c+1, feature_end=d, exon_number=i+1))
        sources.append(dict(species=species, kind="genome_and_mapped_annotation",
                            sequence_url=seq_url, annotation_url=anno_url,
                            range_requests_verified=True))
        print(f"{species}: {len(dna):,} genomic bases and two mapped flank models", flush=True)

    # Map the published spliced CDS rather than assuming that the paper's
    # coordinate labels refer to the deposited GenBank assembly.
    with zipfile.ZipFile(io.BytesIO(fetch(SUPPLEMENT, cache))) as z:
        ns = {"m": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}
        ss = ["".join(e.itertext()) for e in ET.fromstring(z.read("xl/sharedStrings.xml")).findall("m:si", ns)]
        for row in ET.fromstring(z.read("xl/worksheets/sheet1.xml")).findall(".//m:row", ns):
            values = []
            for cell in row.findall("m:c", ns):
                v = cell.find("m:v", ns)
                value = "" if v is None else v.text
                values.append(ss[int(value)] if cell.get("t") == "s" else value)
            if values and values[0] == "Dox":
                cds = re.sub("[^ACGT]", "", values[3].split("(")[0])
                break
        else:
            raise ValueError("Published Dox CDS not found")
    (out / "Dox-published-CDS.fa").write_text(">Dox_CDS\n" + cds + "\n")
    cds_mapping = subprocess.check_output(["minimap2", "-c", "-x", "splice", "-k9", "-w5",
        "-m10", "-s20", "-G10000", "--splice-flank=no", str(out / "Dsim.fa"),
        str(out / "Dox-published-CDS.fa")], text=True, stderr=subprocess.DEVNULL)
    mappings = [r.split("\t") for r in cds_mapping.splitlines()]
    full = [r for r in mappings if int(r[2]) == 0 and int(r[3]) == len(cds) and
            int(r[9]) == len(cds) and int(r[10]) == len(cds) and int(r[11]) >= 30]
    if len(full) != 1:
        raise ValueError("Dox CDS must map uniquely, end-to-end, with 100% identity")
    r = full[0]
    dox_gene = dict(species="Dsim", seqnames="X", gene_id="Dsim_Dox", gene_name="Dox",
        transcript_id="Dsim_Dox_published_CDS", start=int(r[7])+LOCI[1]["start"],
        end=int(r[8])+LOCI[1]["start"]-1, strand=r[4],
        model_source="Published CDS, uniquely mapped end-to-end with 100% identity; UTR not reconstructed",
        source_transcript="Supplementary Table 1, row Dox, nucleotide CDS")
    genes.append(dox_gene)
    cigar = next(t[5:] for t in r[12:] if t.startswith("cg:Z:"))
    position = dox_gene["start"]
    exon_number = 0
    for length, operation in re.findall(r"(\d+)([A-Z=])", cigar):
        length = int(length)
        if operation == "M":
            exon_number += 1
            for feature_type in ["exon", "CDS"]:
                features.append(dict(**dox_gene, type=feature_type, feature_start=position,
                    feature_end=position+length-1, exon_number=exon_number))
            position += length
        elif operation == "N":
            position += length
        else:
            raise ValueError("Dox CDS mapping must contain only exact exons and introns")

    def get_signal(job):
        locus, strand, direction = job
        species, sample = locus["species"], locus["sample"]
        url = (f"https://ftp.ncbi.nlm.nih.gov/geo/samples/{locus['geo_group']}/{sample}/suppl/"
               f"{sample}_{species}_RNA_rep1.{direction}.bw")
        bw = BigWigWindow(url, cache)
        chromosomes = bw.chromosomes()
        key = "X" if "X" in chromosomes else "chrX"
        if key not in chromosomes:
            raise ValueError(f"X chromosome missing from {sample}: {list(chromosomes)}")
        if chromosomes[key][1] != chromosome_lengths[species]:
            if species != "Dsim":
                raise ValueError(f"RNA-seq chromosome length does not match the {species} assembly")
            print(f"{species} {strand}: excluded because source X length {chromosomes[key][1]} differs from assembly {chromosome_lengths[species]}", flush=True)
            return [], dict(species=species, kind="RNA-seq", sample=sample, strand=strand,
                url=url, source_seqname=key, chromosome_length=chromosomes[key][1],
                expected_chromosome_length=chromosome_lengths[species], included=False,
                reason="Reference mismatch; exact source genome could not be retrieved; no coordinate shift inferred",
                range_requests_verified=True, byte_ranges=bw.ranges)
        rows = bw.intervals(chromosomes[key][0], locus["start"]-1, locus["end"])
        if not rows:
            raise ValueError(f"No source coverage for {species} {strand}")
        signal = [dict(species=species, seqnames="X", start=a, end=b, strand=strand,
                       score=score, sample=sample) for a, b, score in rows]
        provenance = dict(species=species, kind="RNA-seq", sample=sample, strand=strand,
                          url=url, source_seqname=key, chromosome_length=chromosomes[key][1],
                          n_intervals=len(rows), included=True, range_requests_verified=True, byte_ranges=bw.ranges,
                          score_handling="Source float32 scores unchanged; intervals clipped at window boundaries only")
        print(f"{species} {strand}: {len(rows):,} exact source intervals", flush=True)
        return signal, provenance

    jobs = [(l, strand, direction) for l in LOCI for strand, direction in [("+", "Forward"), ("-", "Reverse")]]
    with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
        for signal, provenance in pool.map(get_signal, jobs):
            coverage.extend(signal)
            sources.append(provenance)

    remap_dir = cache / "remapping"
    remap_file = remap_dir / "Dsim-remapped-coverage.tsv.gz"
    remap_qc = remap_dir / "Dsim-remapping-qc.json"
    if not remap_file.exists() or not remap_qc.exists():
        raise RuntimeError("Prepare verified Dsim coverage first: python3 data-raw/dox_synteny/remap-dsim-rnaseq.py --download")
    qc = json.loads(remap_qc.read_text())
    if (qc["assembly"] != LOCI[1]["accession"] or qc["chrom"] != LOCI[1]["chrom"] or
        qc["start"] != LOCI[1]["start"] or qc["end"] != LOCI[1]["end"] or
        qc["chromosome_length"] != chromosome_lengths["Dsim"] or
        not qc["independent_depth_verified"] or not qc["full_reference_dictionary_verified"] or
        qc["primary_records"] != 2 * qc["read_pairs"] or qc["min_mapq"] < 20 or
        qc["coverage_md5"] != hashlib.md5(remap_file.read_bytes()).hexdigest() or
        qc["locus_sequence_sha256"] != hashlib.sha256(genomes["Dsim"].encode()).hexdigest()):
        raise ValueError("Dsim remapping does not match the exact plotted locus or provenance")
    with gzip.open(remap_file, "rt") as inp:
        remapped = list(csv.DictReader(inp, delimiter="\t"))
    for row in remapped:
        row["start"], row["end"], row["score"] = int(row["start"]), int(row["end"]), int(row["score"])
        if (row["species"] != "Dsim" or row["sample"] != qc["sample"] or row["seqnames"] != "X" or
            not LOCI[1]["start"] <= row["start"] <= row["end"] <= LOCI[1]["end"] or
            row["strand"] not in ("+", "-") or row["score"] == 0 or
            (row["score"] > 0) != (row["strand"] == "+")):
            raise ValueError("Invalid remapped coverage interval")
    coverage.extend(remapped)
    for strand in ("+", "-"):
        sources.append(dict(species="Dsim", kind="RNA-seq remapped", sample=qc["sample"],
            run=qc["run"], strand=strand, included=True, source_seqname=qc["chrom"],
            chromosome_length=qc["chromosome_length"],
            url="https://www.ebi.ac.uk/ena/browser/view/" + qc["run"],
            n_intervals=sum(r["strand"] == strand for r in remapped),
            signal_units=qc["signal_units"], score_handling=qc["score_handling"],
            min_mapq=qc["min_mapq"], provenance_file="Dsim-remapping-qc.json"))
    shutil.copyfile(remap_qc, out / "Dsim-remapping-qc.json")
    print(f"Dsim: {len(remapped):,} remapped coverage intervals; every base independently verified", flush=True)

    links = []
    if not shutil.which("lastz"):
        raise RuntimeError("LASTZ is required to reproduce the nucleotide ribbons")
    version = subprocess.run(["lastz", "--version"], text=True, capture_output=True)
    lastz_version = (version.stdout + version.stderr).splitlines()[0]
    if "lastz" not in lastz_version.lower():
        raise RuntimeError("Unable to identify the LASTZ version")
    for a, b in [("Dmau", "Dsim"), ("Dsim", "Dsech")]:
        path = cache / f"{a}-{b}.lastz.tsv"
        command = ["lastz", f"{out / (a+'.fa')}[unmask]", f"{out / (b+'.fa')}[unmask]",
                   "--nogapped", "--chain", "--format=general:name1,start1,end1,name2,strand2,start2+,end2+,identity,nmatch", "--output="+str(path)]
        subprocess.run(command, check=True)
        wa, wb = next(l for l in LOCI if l["species"] == a), next(l for l in LOCI if l["species"] == b)
        for line in path.read_text().splitlines():
            if line.startswith("#"):
                continue
            # identity emits a fraction and a percent in two tab-separated fields.
            r = line.split("\t")
            start1, end1, start2, end2 = map(int, [r[1], r[2], r[5], r[6]])
            matches = int(r[-1])
            width = end1-start1+1
            identity = matches/width*100
            if width < 80 or identity < 85:
                continue
            if end2-start2+1 != width:
                raise ValueError("Expected ungapped equal-length blocks")
            links.append(dict(tspecies=a, tchr="X", tstart=start1+wa["start"]-1,
                              tend=end1+wa["start"]-1, qspecies=b, qchr="X",
                              qstart=start2+wb["start"]-1, qend=end2+wb["start"]-1,
                              strand=r[4], nmatch=matches, identity=identity, length=width,
                              group=len(links)+1))
        print(f"{a} / {b}: {sum(r['tspecies']==a for r in links)} nucleotide blocks", flush=True)
    write_tsv(out / "windows.tsv", LOCI)
    genes.sort(key=lambda r: ([l["species"] for l in LOCI].index(r["species"]), r["start"]))
    write_tsv(out / "genes.tsv", genes)
    write_tsv(out / "features.tsv", features)
    write_tsv(out / "coverage.tsv.gz", coverage)
    write_tsv(out / "links.tsv", links)
    outputs = [dict(file=p.name, md5=hashlib.md5(p.read_bytes()).hexdigest())
               for p in sorted(out.iterdir()) if p.suffix in [".tsv", ".fa", ".gz"] or p.name == "Dsim-remapping-qc.json"]
    manifest = dict(paper=PAPER, dox_cds_source=SUPPLEMENT,
                    coordinate_system="1-based inclusive; all seqnames aliased to X, original contigs in windows.tsv",
                    RNA_seq="Testis, biological replicate 1; GSE185361. Dmau/Dsech: deposited strand signals. Dsim: complete SRR23418969 library remapped directly to the plotted assembly.",
                    signal_units="Dmau/Dsech: author-deposited BigWig values. Dsim: unnormalized primary read depth, MAPQ>=20. Independent y scales; no between-species expression inference.",
                    RNA_seq_remapping=dict(run=qc["run"], sample=qc["sample"], read_pairs=qc["read_pairs"],
                        assembly=qc["assembly"], provenance_file="Dsim-remapping-qc.json"),
                    alignment=dict(tool=lastz_version, mode="--nogapped --chain", minimum_length=80,
                                   minimum_identity=85, selection="No gene- or insertion-dependent filtering"),
                    annotation="Two common D. melanogaster RefSeq transcript models mapped to the exact species assemblies; three Dox coding exons mapped from the published CDS with 100% identity",
                    sources=sources, outputs=outputs)
    manifest["dox_cds_mapping"] = dict(cigar=cigar, identity=100, length=len(cds),
        genomic_start=dox_gene["start"], genomic_end=dox_gene["end"], strand=dox_gene["strand"])
    (out / "manifest.json").write_text(json.dumps(manifest, indent=2)+"\n")
    destination.mkdir(parents=True, exist_ok=True)
    for path in out.iterdir():
        shutil.copyfile(path, destination / path.name)
    print(f"Wrote reproducible offline inputs to {destination}")


if __name__ == "__main__":
    main()
