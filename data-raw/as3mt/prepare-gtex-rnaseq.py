#!/usr/bin/env python3
"""Bundle unchanged GTEx AS3MT BigWig signal and sample-matched junctions.

The standard-library BigWig reader follows chromosome and interval indexes and
fetches only overlapping source blocks with HTTP Range. It never downloads the
full BigWig, bins coverage, fills missing bases, or normalizes source scores.
Junctions are separate exact, sample-filtered Snaptron gtexv2 queries.

From the repository root:
  python3 data-raw/as3mt/prepare-gtex-rnaseq.py \
    --out-dir inst/extdata/as3mt --cache-dir /tmp/as3mt-gtex-source \
    --verify-with-rtracklayer

Use --offline with the same cache to reproduce without network. The optional
R verification imports a sparse local copy of the original BigWig: only fetched
bytes are populated, and rtracklayer independently follows its original indexes.
"""

import argparse
import csv
import datetime
import hashlib
import io
import json
import math
from pathlib import Path
import shutil
import struct
import subprocess
import tempfile
import urllib.request
import zlib


SAMPLE = "GTEX-15G19-0011-R10b-SM-7KUG4.1"
RAIL_ID = "9717366"
RUN = None  # Snaptron sample metadata reports run_acc="NA"
SOURCE_URL = (
    "https://recount-opendata.s3.amazonaws.com/recount3/release/human/"
    "data_sources/gtex/base_sums/IN/BRAIN/G4/"
    "gtex.base_sums.BRAIN_GTEX-15G19-0011-R10b-SM-7KUG4.1.ALL.bw"
)
SOURCE_SIZE = 160835940
CHROMOSOME = "chr10"
REGION_START, REGION_END = 102869470, 102901899  # 1-based closed
JUNCTIONS = (
    (102869594, 102869804, "E1", "E2", 9, "inclusion"),
    (102869846, 102870083, "E2", "E3", 10, "inclusion"),
    (102870212, 102872447, "E3", "E4", 11, "inclusion"),
    (102869594, 102872447, "E1", "E4", 12, "skipping"),
)


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def fetch_cached(url, cache_path, offline=False, byte_range=None):
    if cache_path.exists():
        result = cache_path.read_bytes()
    else:
        if offline:
            raise RuntimeError(f"Required cached response is missing: {cache_path}")
        headers = {"Range": f"bytes={byte_range[0]}-{byte_range[1]}"} if byte_range else {}
        with urllib.request.urlopen(urllib.request.Request(url, headers=headers), timeout=30) as response:
            if byte_range:
                start, end = byte_range
                expected_range = f"bytes {start}-{end}/{SOURCE_SIZE}"
                if response.status != 206 or response.headers.get("Content-Range") != expected_range:
                    raise RuntimeError("Source server did not honor the bounded HTTP Range request")
                result = response.read(end - start + 2)
            else:
                if response.status != 200:
                    raise RuntimeError(f"Unexpected Snaptron status {response.status}")
                result = response.read(1000001)
                if len(result) > 1000000:
                    raise RuntimeError("Unexpectedly large sample-filtered Snaptron response")
        cache_path.parent.mkdir(parents=True, exist_ok=True)
        cache_path.write_bytes(result)
    if byte_range and len(result) != byte_range[1] - byte_range[0] + 1:
        raise RuntimeError("Incorrect cached/fetched source byte-range length")
    return result


class BigWigRanges:
    """Bounded source reader; cache each fetched byte range for offline replay."""

    def __init__(self, cache_dir, offline=False):
        self.cache_dir, self.offline = cache_dir, offline
        self.chunks = {}
        self.byte_requests = []

    def read(self, offset, size):
        if size < 0 or size > 1000000 or offset < 0 or offset + size > SOURCE_SIZE:
            raise ValueError("Invalid or oversized BigWig range")
        for start, content in self.chunks.items():
            if start <= offset and offset + size <= start + len(content):
                return content[offset - start:offset - start + size]
        cache_path = self.cache_dir / f"bigwig-{offset}-{offset + size - 1}.bin"
        retrieved_from = "cache" if cache_path.exists() else "http"
        content = fetch_cached(
            SOURCE_URL, cache_path,
            self.offline, (offset, offset + size - 1),
        )
        self.byte_requests.append({
            "start": offset, "end": offset + size - 1,
            "sha256": sha256(content), "retrieved_from": retrieved_from,
        })
        self.chunks[offset] = content
        if sum(item["end"] - item["start"] + 1 for item in self.byte_requests) > 2000000:
            raise RuntimeError("Extraction exceeded its 2 MB source-byte budget")
        return content

    def unique_byte_count(self):
        """Count the union of retrieved source positions, including overlaps once."""
        right, count = -1, 0
        for item in sorted(self.byte_requests, key=lambda value: value["start"]):
            left, end = item["start"], item["end"]
            count += max(0, end - max(left, right + 1) + 1)
            right = max(right, end)
        return count

    def sparse_copy(self, path):
        with path.open("wb") as handle:
            handle.truncate(SOURCE_SIZE)
            for start, content in sorted(self.chunks.items()):
                handle.seek(start)
                handle.write(content)


def extract_bigwig(reader):
    header = struct.unpack("<IHHQQQHHQQIQ", reader.read(0, 64))
    magic, version, _, chrom_offset, data_offset, index_offset, _, _, _, _, uncompress_size, _ = header
    if magic != 0x888FFC26 or version != 4:
        raise RuntimeError("Unsupported BigWig header")
    # Include header, zoom headers, total summary and complete chromosome tree.
    reader.read(0, data_offset)
    magic, _, key_size, value_size, _, _ = struct.unpack("<IIIIQQ", reader.read(chrom_offset, 32))
    if magic != 0x78CA8C91 or value_size != 8:
        raise RuntimeError("Invalid BigWig chromosome tree")

    def chromosome(node):
        is_leaf, _, count = struct.unpack("<BBH", reader.read(node, 4))
        body = reader.read(node + 4, count * (key_size + 8))
        for index in range(count):
            position = index * (key_size + 8)
            key = body[position:position + key_size].rstrip(b"\x00").decode()
            value = body[position + key_size:position + key_size + 8]
            if is_leaf and key == CHROMOSOME:
                return struct.unpack("<II", value)
            if not is_leaf:
                found = chromosome(struct.unpack("<Q", value)[0])
                if found:
                    return found
        return None

    chromosome_info = chromosome(chrom_offset + 32)
    if chromosome_info is None:
        raise RuntimeError(f"Missing chromosome {CHROMOSOME}")
    chrom_id, chrom_size = chromosome_info
    if chrom_size != 133797422:
        raise RuntimeError("chr10 length does not match GRCh38")
    index_header = struct.unpack("<IIQIIIIQII", reader.read(index_offset, 48))
    if index_header[0] != 0x2468ACE0:
        raise RuntimeError("Invalid BigWig interval tree")
    query_lo, query_hi = (chrom_id, REGION_START - 1), (chrom_id, REGION_END)
    blocks = set()

    def overlapping_blocks(node):
        is_leaf, _, count = struct.unpack("<BBH", reader.read(node, 4))
        entry_size = 32 if is_leaf else 24
        body = reader.read(node + 4, count * entry_size)
        for index in range(count):
            position = index * entry_size
            start_chrom, start, end_chrom, end, child = struct.unpack_from("<IIIIQ", body, position)
            if (start_chrom, start) < query_hi and (end_chrom, end) > query_lo:
                if is_leaf:
                    blocks.add((child, struct.unpack_from("<Q", body, position + 24)[0]))
                else:
                    overlapping_blocks(child)

    overlapping_blocks(index_offset + 48)
    rows = []
    for offset, size in sorted(blocks):
        content = reader.read(offset, size)
        content = zlib.decompress(content) if uncompress_size else content
        position = 0
        while position < len(content):
            cid, start, _, step, span, kind, _, count = struct.unpack_from("<IIIIIBBH", content, position)
            position += 24
            for index in range(count):
                if kind == 1:
                    left, right, score = struct.unpack_from("<IIf", content, position)
                    position += 12
                elif kind == 2:
                    left, score = struct.unpack_from("<If", content, position)
                    right = left + span
                    position += 8
                elif kind == 3:
                    score = struct.unpack_from("<f", content, position)[0]
                    left, right = start + index * step, start + index * step + span
                    position += 4
                else:
                    raise RuntimeError(f"Unsupported BigWig section type {kind}")
                if cid == chrom_id and left < REGION_END and right > REGION_START - 1:
                    if not math.isfinite(score) or right <= left:
                        raise RuntimeError("Invalid source signal interval")
                    rows.append((CHROMOSOME, max(left + 1, REGION_START), min(right, REGION_END), score))
        if position != len(content):
            raise RuntimeError("BigWig section length mismatch")
    rows.sort(key=lambda row: row[1:3])
    if not rows or any(a[2] >= b[1] for a, b in zip(rows, rows[1:])):
        raise RuntimeError("Missing or overlapping source coverage intervals")
    return rows, sorted(blocks)


def extract_junctions(cache_dir, offline=False):
    rows, queries = [], []
    for start, end, from_exon, to_exon, expected_reads, event in JUNCTIONS:
        url = ("https://snaptron.cs.jhu.edu/gtexv2/snaptron?regions="
               f"{CHROMOSOME}:{start}-{end}&exact=1&sids={RAIL_ID}")
        content = fetch_cached(url, cache_dir / f"junction-{start}-{end}.tsv", offline)
        records = [row for row in csv.DictReader(io.StringIO(content.decode()), delimiter="\t")
                   if row["chromosome"] == CHROMOSOME and int(row["start"]) == start
                   and int(row["end"]) == end and row["strand"] == "+"]
        if len(records) != 1:
            raise RuntimeError("Expected one exact plus-strand Snaptron junction")
        record = records[0]
        samples = dict(item.split(":") for item in record["samples"].strip(",").split(",") if item)
        if set(samples) != {RAIL_ID} or int(samples[RAIL_ID]) != expected_reads:
            raise RuntimeError("Sample identity or reported junction count changed")
        rows.append((CHROMOSOME, start, end, start - 1, end + 1, from_exon,
                     to_exon, int(samples[RAIL_ID]), event))
        queries.append({"url": url, "response_sha256": sha256(content),
                        "snaptron_id": record["snaptron_id"], "intron_start": start,
                        "intron_end": end, "reads": int(samples[RAIL_ID]), "event": event})
    return rows, queries


def verify_rtracklayer(reader, rows):
    if not shutil.which("Rscript"):
        raise RuntimeError("Rscript is required for independent rtracklayer verification")
    with tempfile.TemporaryDirectory(prefix="as3mt-gtex-verify-") as directory:
        source, result = Path(directory) / "source.bw", Path(directory) / "imported.tsv"
        reader.sparse_copy(source)
        code = '''args <- commandArgs(TRUE)
        suppressPackageStartupMessages(library(rtracklayer))
        locus <- GenomicRanges::GRanges("chr10", IRanges::IRanges(102869470, 102901899))
        x <- as.data.frame(import(args[1], which = locus, as = "GRanges"))
        x$start <- pmax(x$start, 102869470L); x$end <- pmin(x$end, 102901899L)
        options(digits = 17)
        write.table(x[,c("seqnames","start","end","score")], args[2],
                    sep="\\t", row.names=FALSE, quote=FALSE)
        '''
        subprocess.run(["Rscript", "-e", code, str(source), str(result)], check=True, timeout=60)
        with result.open() as handle:
            observed = [(row["seqnames"], int(row["start"]), int(row["end"]), float(row["score"]))
                        for row in csv.DictReader(handle, delimiter="\t")]
        if len(observed) != len(rows) or any(
                expected[:3] != actual[:3] or not math.isclose(expected[3], actual[3], rel_tol=1e-14, abs_tol=1e-14)
                for expected, actual in zip(rows, observed)):
            raise RuntimeError("Independent rtracklayer import differs from extracted source signal")
        return f"All {len(rows)} coordinates and source scores matched an independent rtracklayer import of original indexed source bytes."


def write_tsv(path, header, rows):
    with path.open("w", newline="") as handle:
        writer = csv.writer(handle, delimiter="\t", lineterminator="\n")
        writer.writerow(header)
        writer.writerows(rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--cache-dir", type=Path, required=True)
    parser.add_argument("--offline", action="store_true")
    parser.add_argument("--verify-with-rtracklayer", action="store_true")
    args = parser.parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)
    reader = BigWigRanges(args.cache_dir, args.offline)
    signal, blocks = extract_bigwig(reader)
    junctions, queries = extract_junctions(args.cache_dir, args.offline)
    verification = "Original BigWig chromosome and interval indexes plus compressed signal sections validated."
    if args.verify_with_rtracklayer:
        verification += " " + verify_rtracklayer(reader, signal)
    signal_path = args.out_dir / "human-rnaseq-GTEX-15G19.tsv"
    junction_path = args.out_dir / "human-junctions-GTEX-15G19.tsv"
    write_tsv(signal_path, ["seqnames", "start", "end", "score"], signal)
    write_tsv(junction_path, ["seqnames", "intron_start", "intron_end", "donor", "acceptor",
                              "from_exon", "to_exon", "reads", "event"], junctions)
    provenance = {
        "sample": SAMPLE, "rail_id": RAIL_ID, "run_accession": RUN,
        "assembly": "GRCh38", "gene": "AS3MT", "gene_strand": "+",
        "seqnames": CHROMOSOME, "start": REGION_START, "end": REGION_END,
        "coordinate_system": "1-based closed; donor/acceptor are flanking exonic bases, not intronic bases",
        "biosample": "Human frontal cortex (BA9), male, age 60-69",
        "sample_metadata_url": "https://snaptron.cs.jhu.edu/gtexv2/samples?all=1",
        "sample_metadata": {"rail_id": RAIL_ID, "external_id": SAMPLE, "run_acc": "NA",
                            "SMTSD": "Brain - Frontal Cortex (BA9)", "SEX": "1", "AGE": "60-69",
                            "SMRIN": "7.9", "SMAFRZE": "RNASEQ", "SMGEBTCHT": "TruSeq.v1",
                            "min_len": "76", "max_len": "76"},
        "source": "recount3 GTEx release human base_sums, ALL (combined-strand signal)",
        "source_url": SOURCE_URL, "source_full_file_size": SOURCE_SIZE,
        "full_file_checksum_verified": False,
        "score_units": "Raw per-base RNA-seq coverage counts; source values displayed without a library-size normalization or conversion to transcript abundance",
        "score_units_source_url": "https://pmc.ncbi.nlm.nih.gov/articles/PMC8628444/",
        "coverage_documentation_url": "https://research.libd.org/recount3/articles/recount3-quickstart.html",
        "score_handling": "Source float32 values unchanged. Only locus boundaries are clipped. No normalization, interpolation, binning, or missing-base zero filling.",
        "signal_tsv_sha256": sha256(signal_path.read_bytes()),
        "junction_tsv_sha256": sha256(junction_path.read_bytes()),
        "n_intervals": len(signal),
        "n_covered_bases": sum(end - start + 1 for _, start, end, _ in signal),
        "n_explicit_zero_intervals": sum(score == 0 for _, _, _, score in signal),
        "n_explicit_zero_bases": sum(end - start + 1 for _, start, end, score in signal if score == 0),
        "min_score": min(row[3] for row in signal), "max_score": max(row[3] for row in signal),
        "source_signal_blocks": [{"offset": offset, "size": size} for offset, size in blocks],
        "retained_byte_ranges": [{"start": offset, "end": offset + len(content) - 1,
                                 "sha256": sha256(content)} for offset, content in sorted(reader.chunks.items())],
        "n_retained_source_bytes": sum(map(len, reader.chunks.values())),
        "n_unique_source_bytes": reader.unique_byte_count(),
        "source_byte_requests": reader.byte_requests,
        "n_source_byte_requests": len(reader.byte_requests),
        "n_requested_source_bytes": sum(item["end"] - item["start"] + 1 for item in reader.byte_requests),
        "n_http_source_requests": sum(item["retrieved_from"] == "http" for item in reader.byte_requests),
        "n_http_source_bytes": sum(item["end"] - item["start"] + 1 for item in reader.byte_requests if item["retrieved_from"] == "http"),
        "n_cached_source_bytes": sum(item["end"] - item["start"] + 1 for item in reader.byte_requests if item["retrieved_from"] == "cache"),
        "source_byte_accounting": "Counters describe this preparation run only, excluding Snaptron queries. Requested bytes count each cache/HTTP retrieval, including the 64-byte header read again within the larger header/tree range. Unique bytes count overlapping source positions once. Retained bytes are the chunks used for the sparse BigWig; an expanded chunk replaces an earlier chunk with the same start. HTTP counters exclude cached and in-memory reads.",
        "offline_replay": args.offline,
        "junction_source": "Snaptron gtexv2, exact intron coordinates and sample-filtered rail_id",
        "junction_count_units": "Reported supporting-read counts for each splice junction in this sample",
        "junction_queries": queries, "verification": verification,
        "prepared_utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "caveats": [
            "Coverage is combined across transcripts and strands; shared-exon coverage cannot be assigned separately to either AS3MT isoform.",
            "Junction counts support inclusion or E2/E3 skipping events and do not measure complete transcript abundance or prove the full reconstructed transcript.",
            "This independent adult GTEx sample is separate from the organoid and minigene experiments in the cited AS3MT papers.",
            "Selected for the coexistence of inclusion and skipping junctions in one standard GTEx RNA-seq sample. This selection is for visualization and does not support an unbiased prevalence estimate or biological group comparison.",
        ],
    }
    (args.out_dir / "human-rnaseq-GTEX-15G19-provenance.json").write_text(json.dumps(provenance, indent=2) + "\n")
    print(json.dumps({key: provenance[key] for key in ("n_intervals", "n_covered_bases", "min_score", "max_score", "n_requested_source_bytes", "n_unique_source_bytes", "n_http_source_bytes", "verification")}, indent=2))


if __name__ == "__main__":
    main()
