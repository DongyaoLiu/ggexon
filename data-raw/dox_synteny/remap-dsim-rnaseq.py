#!/usr/bin/env python3
"""Remap the complete public Dsim testis RNA-seq library to the plotted genome.

Requires Python's standard library, minimap2 >=2.29 (splice:sr), and samtools.
Raw reads/reference and BAM files stay in the ignored downloads/remapping cache.
Only verified locus coverage and its QC/provenance are consumed by the builder.
"""
import argparse
import csv
import gzip
import hashlib
import io
import json
from pathlib import Path
import re
import shutil
import subprocess
import urllib.request

ROOT = Path(__file__).resolve().parents[2]
CACHE = Path(__file__).resolve().parent / "downloads/remapping"
CHROM, START, END = "CM015611.1", 9233164, 9276944
ASSEMBLY = "GCA_004382185.1"
RUN, SAMPLE, PAIRS = "SRR23418969", "GSM7039340", 40381684
EXCLUDE_FLAGS = 4 + 256 + 2048
REFERENCE_NAME = "GCA_004382185.1_ASM438218v1_genomic.fna"
REFERENCE_URL = ("https://ftp.ncbi.nlm.nih.gov/genomes/all/GCA/004/382/185/"
                 "GCA_004382185.1_ASM438218v1/" + REFERENCE_NAME + ".gz")
FASTQ_BASE = "https://ftp.sra.ebi.ac.uk/vol1/fastq/SRR234/069/SRR23418969/"
INPUTS = [
    (f"{RUN}_1.fastq.gz", FASTQ_BASE + f"{RUN}_1.fastq.gz", "093d1bbce10343a70c83fdb212d06c79"),
    (f"{RUN}_2.fastq.gz", FASTQ_BASE + f"{RUN}_2.fastq.gz", "c741f7a08fe4c49a899d10e1d1bf5c9a"),
    (REFERENCE_NAME + ".gz", REFERENCE_URL, "70b1a5aa5706a03139e6853138437f31"),
]


def cigar_operations(cigar):
    operations = re.findall(r"(\d+)([MIDNSHP=X])", cigar)
    if not operations or "".join(n + op for n, op in operations) != cigar:
        raise ValueError(f"Invalid CIGAR: {cigar}")
    return [(int(n), op) for n, op in operations]


def aligned_blocks(start1, cigar):
    """Return 1-based inclusive aligned-base blocks; introns/deletions are gaps."""
    pos, blocks = start1, []
    for n, op in cigar_operations(cigar):
        if op in "M=X":
            blocks.append((pos, pos + n - 1))
            pos += n
        elif op in "DN":
            pos += n
    return blocks


def transcript_strand(flag):
    """Reverse-stranded paired RNA: read 1 is antisense and read 2 is sense."""
    first, second, reverse = bool(flag & 64), bool(flag & 128), bool(flag & 16)
    if not flag & 1 or first == second:
        raise ValueError("Expected a paired alignment with exactly one mate flag")
    return "+" if first == reverse else "-"


def locus_depth(sam_lines, chromosome, start, end, min_mapq=20):
    size = end - start + 1
    differences = {"+": [0] * (size + 1), "-": [0] * (size + 1)}
    stats = dict(accepted_alignments=0, accepted_spliced_alignments=0,
                 plus_alignments=0, minus_alignments=0)
    for line in sam_lines:
        if line.startswith("@") or not line.strip():
            continue
        r = line.rstrip().split("\t")
        flag, mapq = int(r[1]), int(r[4])
        if r[2] != chromosome or flag & EXCLUDE_FLAGS or mapq < min_mapq:
            continue
        strand = transcript_strand(flag)
        has_overlap = False
        for a, b in aligned_blocks(int(r[3]), r[5]):
            a, b = max(a, start), min(b, end)
            if a <= b:
                differences[strand][a - start] += 1
                differences[strand][b - start + 1] -= 1
                has_overlap = True
        if has_overlap:
            stats["accepted_alignments"] += 1
            stats["accepted_spliced_alignments"] += int("N" in r[5])
            stats["plus_alignments" if strand == "+" else "minus_alignments"] += 1
    depths = {}
    for strand, diff in differences.items():
        current, values = 0, []
        for change in diff[:-1]:
            current += change
            if current < 0:
                raise ValueError("Negative read depth")
            values.append(current)
        depths[strand] = values
    return depths["+"], depths["-"], stats


def depth_rows(plus, minus, start):
    """Run-length encode nonzero, unchanged depth; store reverse signal negative."""
    rows = []
    for strand, values, sign in [("+", plus, 1), ("-", minus, -1)]:
        i = 0
        while i < len(values):
            j = i + 1
            while j < len(values) and values[j] == values[i]:
                j += 1
            if values[i] > 0:
                rows.append(dict(species="Dsim", seqnames="X", start=start + i,
                    end=start + j - 1, strand=strand, score=sign * values[i], sample=SAMPLE))
            i = j
    return rows


def digest(path, algorithm="md5"):
    h = hashlib.new(algorithm)
    with path.open("rb") as inp:
        for chunk in iter(lambda: inp.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def ensure_input(cache, item, download):
    name, url, expected = item
    path = cache / name
    if not path.exists():
        if not download:
            raise FileNotFoundError(f"Missing {path}. Supply --download to fetch the public inputs.")
        pending = path.with_suffix(path.suffix + ".pending")
        with urllib.request.urlopen(url, timeout=120) as inp, pending.open("wb") as out:
            shutil.copyfileobj(inp, out, length=1024 * 1024)
        pending.replace(path)
    if digest(path) != expected:
        raise ValueError(f"Source MD5 mismatch: {name}")
    return path


def reference_region(reference, chromosome, start, end):
    result = subprocess.check_output(["samtools", "faidx", str(reference),
        f"{chromosome}:{start}-{end}"], text=True)
    return "".join(result.splitlines()[1:]).upper()


def check_stranding(sam_lines, sequence):
    counts = dict(reverse_library_agreement=0, forward_library_agreement=0)
    for line in sam_lines:
        r = line.rstrip().split("\t")
        pos = int(r[3])
        for n, op in cigar_operations(r[5]):
            if op == "N" and START <= pos and pos + n - 1 <= END:
                motif = sequence[pos - START:pos - START + 2] + sequence[pos + n - START - 2:pos + n - START]
                if motif in ("GTAG", "GCAG", "ATAC", "CTAC", "CTGC", "GTAT"):
                    agrees = (transcript_strand(int(r[1])) == "+") == (motif in ("GTAG", "GCAG", "ATAC"))
                    counts["reverse_library_agreement" if agrees else "forward_library_agreement"] += 1
                    break
            if op in "MDN=X":
                pos += n
    total = sum(counts.values())
    agreement = counts["reverse_library_agreement"] / total if total else 0
    if total < 20 or agreement < .95:
        raise ValueError(f"Insufficient evidence for reverse-stranded RNA: {counts}")
    return dict(method="One canonical genomic splice motif per local MAPQ-filtered read",
                counts=counts, agreement=agreement)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache-dir", type=Path, default=CACHE)
    parser.add_argument("--threads", type=int, default=6)
    parser.add_argument("--min-mapq", type=int, default=20)
    parser.add_argument("--download", action="store_true")
    parser.add_argument("--reuse-bam", action="store_true",
                        help="Audit and reuse this run's existing BAM rather than aligning again")
    args = parser.parse_args()
    if args.threads < 1 or args.min_mapq < 0:
        parser.error("threads must be positive and min-mapq must be nonnegative")
    cache = args.cache_dir
    cache.mkdir(parents=True, exist_ok=True)
    paths = [ensure_input(cache, item, args.download) for item in INPUTS]
    reference = cache / REFERENCE_NAME
    if not reference.exists():
        with gzip.open(paths[2], "rb") as inp, reference.open("wb") as out:
            shutil.copyfileobj(inp, out)
    # Also reject a stale decompressed reference, independently of the .gz MD5.
    if digest(reference, "sha256") != "cc17c119200f78b71cbb691b97f5c09ff28647211544545667834035bb926c2f":
        raise ValueError("Decompressed reference differs from the pinned genome")
    subprocess.run(["samtools", "faidx", str(reference)], check=True)
    sequence = reference_region(reference, CHROM, START, END)
    expected = "".join((ROOT / "inst/extdata/dox_synteny/Dsim.fa").read_text().splitlines()[1:]).upper()
    if sequence != expected:
        raise ValueError("Remapping reference does not match the plotted locus")
    bam = cache / f"{RUN}.sorted.bam"
    if not args.reuse_bam:
        index = cache / "Dsim-splice-sr.mmi"
        subprocess.run(["minimap2", "-x", "splice:sr", "-t", str(args.threads),
                        "-d", str(index), str(reference)], check=True)
        mapping = ["minimap2", "-ax", "splice:sr", "--secondary=no", "-t", str(args.threads),
            "-K", "50M", "-R", f"@RG\\tID:{RUN}\\tSM:{SAMPLE}\\tLB:Dsim_testis_rep1\\tPL:ILLUMINA",
            str(index), str(paths[0]), str(paths[1])]
        pending_bam = cache / f"{RUN}.pending.bam"
        with (cache / "full-mapping.log").open("w") as log:
            mapper = subprocess.Popen(mapping, stdout=subprocess.PIPE, stderr=log)
            sorter = subprocess.Popen(["samtools", "sort", "-@", "2", "-m", "256M",
                "-T", str(cache / "sort-rnaseq"), "-o", str(pending_bam)], stdin=mapper.stdout)
            mapper.stdout.close()
            sort_status, map_status = sorter.wait(), mapper.wait()
        if sort_status or map_status:
            raise RuntimeError(f"Remapping failed: minimap2={map_status}, samtools={sort_status}")
        pending_bam.replace(bam)
    subprocess.run(["samtools", "quickcheck", "-v", str(bam)], check=True)
    subprocess.run(["samtools", "index", str(bam)], check=True)
    header = subprocess.check_output(["samtools", "view", "-H", str(bam)], text=True)
    if f"ID:{RUN}" not in header or "splice:sr" not in header:
        raise ValueError("BAM does not record the expected run and splice-aware mapper")
    bam_lengths = {}
    for line in header.splitlines():
        if line.startswith("@SQ\t"):
            fields = dict(field.split(":", 1) for field in line.split("\t")[1:])
            bam_lengths[fields["SN"]] = int(fields["LN"])
    reference_lengths = {}
    for line in Path(str(reference) + ".fai").read_text().splitlines():
        name, length, *_ = line.split("\t")
        reference_lengths[name] = int(length)
    if bam_lengths != reference_lengths:
        raise ValueError("BAM chromosome dictionary differs from the exact reference")
    flagstat = json.loads(subprocess.check_output(["samtools", "flagstat", "-O", "json", str(bam)], text=True))
    primary = sum(r["primary"] for r in flagstat.values())
    if primary != 2 * PAIRS:
        raise ValueError(f"Expected all {2 * PAIRS:,} reads; found {primary:,} primary records")
    region = f"{CHROM}:{START}-{END}"
    sam = subprocess.check_output(["samtools", "view", "-F", str(EXCLUDE_FLAGS),
        "-q", str(args.min_mapq), str(bam), region], text=True).splitlines()
    plus, minus, stats = locus_depth(sam, CHROM, START, END, args.min_mapq)
    orientation = check_stranding(sam, sequence)
    # An independent implementation must agree at every genomic base.
    native_depth = subprocess.check_output(["samtools", "depth", "-a", "-Q", str(args.min_mapq),
        "-G", str(EXCLUDE_FLAGS), "-r", region, str(bam)], text=True)
    observed = {}
    for line in native_depth.splitlines():
        chrom, pos, value = line.split("\t")
        if chrom != CHROM:
            raise ValueError("Unexpected depth chromosome")
        observed[int(pos)] = int(value)
    for i in range(END - START + 1):
        if observed.get(START + i, 0) != plus[i] + minus[i]:
            raise ValueError(f"Independent depth mismatch at {START + i}")
    rows = depth_rows(plus, minus, START)
    if not rows:
        raise ValueError("No verified RNA coverage in the locus")
    output = cache / "Dsim-remapped-coverage.tsv.gz"
    with output.open("wb") as raw, gzip.GzipFile(filename="", fileobj=raw, mode="wb", mtime=0) as zipped:
        with io.TextIOWrapper(zipped, newline="") as out:
            writer = csv.DictWriter(out, list(rows[0]), delimiter="\t", lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
    qc = dict(sample=SAMPLE, run=RUN, experiment="SRX19338860", read_pairs=PAIRS,
        primary_records=primary, assembly=ASSEMBLY, chrom=CHROM, chromosome_length=23123156,
        start=START, end=END, min_mapq=args.min_mapq, excluded_flags=EXCLUDE_FLAGS,
        library="reverse-stranded paired-end; read1 antisense, read2 sense",
        strand_evidence=orientation, signal_units="Per-base primary read depth; no library normalization",
        score_handling="Positive/negative signed depths encode transcript strand; introns/deletions excluded; overlapping mates each count",
        independent_depth_verified=True, verified_genomic_bases=END - START + 1,
        full_reference_dictionary_verified=True,
        reference_sequence_sha256=digest(reference, "sha256"),
        locus_sequence_sha256=hashlib.sha256(sequence.encode()).hexdigest(),
        coverage_file=output.name, coverage_md5=digest(output), locus_stats=stats,
        maxima={"+": max(plus), "-": max(minus)},
        inputs=[dict(file=n, url=u, md5=m) for n, u, m in INPUTS],
        versions=dict(minimap2=subprocess.check_output(["minimap2", "--version"], text=True).strip(),
                      samtools=subprocess.check_output(["samtools", "--version"], text=True).splitlines()[0]),
        mapping_program_headers=[l.replace(str(ROOT) + "/", "")
            for l in header.splitlines() if l.startswith("@PG")],
        mapping_statistics=flagstat)
    (cache / "Dsim-remapping-qc.json").write_text(json.dumps(qc, indent=2) + "\n")
    print(f"Verified complete-library remapping: {PAIRS:,} pairs, {stats['accepted_alignments']:,} local alignments")
    print(f"Strand agreement {orientation['agreement']:.3%}; independent depth agrees at every locus base")
    print(f"Wrote {len(rows):,} nonzero coverage intervals: {output}")


if __name__ == "__main__":
    main()
