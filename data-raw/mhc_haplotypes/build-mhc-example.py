#!/usr/bin/env python3
"""Build bounded, native-coordinate inputs for the three-haplotype MHC tutorial."""
import argparse
import csv
import gzip
import hashlib
import io
import json
from pathlib import Path
import re
import struct
import subprocess
from urllib.parse import quote
from urllib.request import Request, urlopen

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
SOURCE = HERE / "source"
OUT = ROOT / "inst/extdata/mhc_haplotypes"
TRACKS = ["SSYh1", "GGOh1", "HSAh1"]
LABELS = ["Siamang H1", "Gorilla H1", "Human CHM13"]
ASSEMBLIES = ["mSymSyn1_v2.0", "mGorGor1_v2.0", "T2T-CHM13v2.0"]
REVISION = "eabd8aa"


def read(path):
    text = gzip.decompress(path.read_bytes()).decode() if path.suffix == ".gz" else path.read_text()
    return list(csv.DictReader(io.StringIO(text), delimiter="\t"))


def write(path, rows):
    buffer = io.StringIO()
    writer = csv.DictWriter(buffer, fieldnames=list(rows[0]), delimiter="\t", lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    data = buffer.getvalue().encode()
    path.write_bytes(gzip.compress(data, mtime=0) if path.suffix == ".gz" else data)


def snapshot_sources():
    """Keep only the three selected tracks from the reviewed historical inputs."""
    SOURCE.mkdir(parents=True, exist_ok=True)
    if (SOURCE / "manifest.json").exists():
        return
    revision = subprocess.check_output(["git", "rev-parse", REVISION], cwd=ROOT, text=True).strip()
    base = "data-raw/ape_paper/"
    inputs = {
        "windows.tsv": "results/mhc_continuous/windows.tsv",
        "curated-mhc.tsv": "results/mhc_full/genes.tsv",
        "context-models.tsv.gz": "results/mhc_continuous/gene_models_with_recovered_context/models.tsv",
    }
    records = []
    for name, original in inputs.items():
        raw = subprocess.check_output(["git", "show", revision + ":" + base + original], cwd=ROOT)
        rows = list(csv.DictReader(io.StringIO(raw.decode()), delimiter="\t"))
        rows = [r for r in rows if r["track"] in TRACKS]
        write(SOURCE / name, rows)
        records.append(dict(file=name, original_path=base + original,
            original_sha256=hashlib.sha256(raw).hexdigest(), selected_rows=len(rows)))
    colours = subprocess.check_output(["git", "show", revision + ":" + base +
        "source_metadata/mhc_continuous/colors.txt"], cwd=ROOT)
    (SOURCE / "colours.tsv").write_bytes(colours)
    (SOURCE / "manifest.json").write_text(json.dumps(dict(repository_commit=revision,
        files=records, paper="https://doi.org/10.1038/s41586-025-08816-3",
        browser_revision="603f414e9278545a1c8dcf416f3226e7a3d07e86"), indent=2) + "\n")


def range_bytes(url, start, size):
    # Distinct URLs prevent range-oblivious HTTP caches from reusing another slice.
    request = Request(url + f"?ggexon_range={start}-{start + size - 1}",
        headers={"Range": f"bytes={start}-{start + size - 1}"})
    with urlopen(request, timeout=45) as response:
        expected = f"bytes {start}-{start + size - 1}/"
        if response.status != 206 or not response.headers.get("Content-Range", "").startswith(expected):
            raise ValueError(f"Expected an exact {size}-byte range: {url}")
        data = response.read(size + 1)
        if len(data) != size:
            raise ValueError(f"Expected {size} bytes, received {len(data)}: {url}")
        return data


def fetch_sequence(url, chrom, start0, end0):
    """Read only a chromosome index and the requested 2bit nucleotide slice."""
    header = range_bytes(url, 0, 16)
    endian = ">" if struct.unpack(">I", header[:4])[0] == 0x1A412743 else "<"
    signature, version, count, _ = struct.unpack(endian + "4I", header)
    assert signature == 0x1A412743 and version in (0, 1)
    index = range_bytes(url, 16, count * 300)
    position, offsets = 0, {}
    width, code = (8, "Q") if version == 1 else (4, "I")
    for _ in range(count):
        length = index[position]
        position += 1
        name = index[position:position + length].decode()
        position += length
        offsets[name] = struct.unpack(endian + code, index[position:position + width])[0]
        position += width
    offset = offsets[chrom]
    chrom_length, n_count = struct.unpack(endian + "2I", range_bytes(url, offset, 8))
    assert 0 <= start0 < end0 <= chrom_length
    blocks = range_bytes(url, offset + 8, n_count * 8 + 4)
    starts = struct.unpack(endian + f"{n_count}I", blocks[:n_count * 4]) if n_count else []
    sizes = struct.unpack(endian + f"{n_count}I", blocks[n_count * 4:n_count * 8]) if n_count else []
    mask_count = struct.unpack(endian + "I", blocks[-4:])[0]
    packed = offset + 8 + n_count * 8 + 4 + mask_count * 8 + 4
    raw = range_bytes(url, packed + start0 // 4, (end0 + 3) // 4 - start0 // 4)
    lookup = ["".join("TCAG"[(byte >> shift) & 3] for shift in (6, 4, 2, 0)) for byte in range(256)]
    sequence = "".join(lookup[b] for b in raw)[start0 % 4:start0 % 4 + end0 - start0]
    for start, length in zip(starts, sizes):
        a, b = max(start, start0) - start0, min(start + length, end0) - start0
        if a < b:
            sequence = sequence[:a] + "N" * (b - a) + sequence[b:]
    assert len(sequence) == end0 - start0
    return sequence, chrom_length


def key(name):
    return name.split("-", 1)[-1] if name.startswith(("HLA-", "Gogo-", "Sysy-")) else name


def prepare_genes():
    original = {r["track"]: r for r in read(SOURCE / "windows.tsv")}
    curated = read(SOURCE / "curated-mhc.tsv")
    functional = [r for r in curated if r["functionality"] == "functional"]
    # One common span, determined only by the selected functional MHC loci.
    span = max(max(int(r["end"]) for r in functional if r["track"] == track) -
        min(int(r["start"]) for r in functional if r["track"] == track) + 1 for track in TRACKS) + 200000
    windows = []
    for track, label, assembly in zip(TRACKS, LABELS, ASSEMBLIES):
        genes = [r for r in functional if r["track"] == track]
        start = (min(int(r["start"]) for r in genes) + max(int(r["end"]) for r in genes) - span + 1) // 2
        w = original[track]
        windows.append(dict(track=track, label=label, assembly=assembly, chr=w["chr"],
            start=start, end=start + span - 1, orientation=w["orientation"], sequence_url=w["twoBit_url"]))
    by_track = {w["track"]: w for w in windows}
    palette = {}
    for line in (SOURCE / "colours.tsv").read_text().splitlines():
        name, rgb = line.split("\t")
        palette[name] = "#" + "".join(f"{int(v):02X}" for v in rgb.split(","))
    genes = [dict(track=r["track"], gene_id=f"{r['track']}_MHC_{i + 1}", gene_name=r["gene_name"],
        start=int(r["start"]), end=int(r["end"]), strand=r["strand"], role="MHC",
        gene_type="protein_coding", source_status=r["functionality"], label=key(r["gene_name"]),
        fill=palette[key(r["gene_name"])], source_url=r["source"], evidence="curated MHC")
        for i, r in enumerate(functional)]
    models = read(SOURCE / "context-models.tsv.gz")
    # CAT omits biotype: also omit background symbols explicitly called pseudogene
    # in the CHM13 source. This is a display filter, not an ape functional-status call.
    pseudogene_symbols = {r["gene_symbol"] for r in models if "pseudogene" in r["gene_type"].lower()}
    candidates, exclusions = [], []
    for r in models:
        w = by_track[r["track"]]
        start, end = int(r["chromStart"]) + 1, int(r["chromEnd"])
        if end < w["start"] or start > w["end"]:
            continue
        duplicate = any(g["track"] == r["track"] and g["strand"] == r["strand"] and
            key(g["gene_name"]) == key(r["gene_symbol"]) and int(g["start"]) <= end and
            start <= int(g["end"]) for g in curated)
        reason = "curated MHC supersedes source transcript" if duplicate else (
            "source pseudogene biotype" if "pseudo" in r["gene_type"].lower() else (
            "background symbol annotated as pseudogene in CHM13" if r["gene_symbol"] in pseudogene_symbols else ""))
        if reason:
            exclusions.append(dict(track=r["track"], model_id=r["model_id"],
                gene_name=r["gene_symbol"], reason=reason))
            continue
        candidates.append(r)
    groups = {}
    for r in candidates:
        groups.setdefault((r["track"], r["gene_symbol"], r["strand"]), []).append(r)
    representatives = []
    for group in groups.values():
        cluster, end = [], -1
        for r in sorted(group, key=lambda r: int(r["chromStart"])):
            if cluster and int(r["chromStart"]) > end:
                representatives.append(max(cluster, key=lambda r: sum(map(int, r["blockSizes"].strip(",").split(",")))))
                cluster = []
            cluster.append(r)
            end = max(end if len(cluster) > 1 else -1, int(r["chromEnd"]))
        representatives.append(max(cluster, key=lambda r: sum(map(int, r["blockSizes"].strip(",").split(",")))))
    for r in representatives:
        genes.append(dict(track=r["track"], gene_id=r["model_id"], gene_name=r["gene_symbol"],
            start=int(r["chromStart"]) + 1, end=int(r["chromEnd"]), strand=r["strand"], role="context",
            gene_type=r["gene_type"] or "unspecified", source_status="not_assigned", label="",
            fill="#B0B0B0", source_url=r["source_url"], evidence=r["annotation_evidence"]))
    genes.sort(key=lambda r: (TRACKS.index(r["track"]), r["start"], r["gene_id"]))
    write(OUT / "genes.tsv", genes)
    write(HERE / "excluded-models.tsv", exclusions)
    for w in windows:
        lines = ["##gff-version 3"]
        for g in genes:
            if g["track"] != w["track"]:
                continue
            attributes = dict(ID=g["gene_id"], Name=g["gene_name"], gene_id=g["gene_id"],
                gene_name=g["gene_name"], gene_role=g["role"], gene_type=g["gene_type"],
                plot_label=g["label"], plot_fill=g["fill"], source_status=g["source_status"])
            attr = ";".join(k + "=" + quote(str(v), safe=":_-") for k, v in attributes.items())
            lines.append("\t".join(map(str, [w["chr"], "MHC_example", "gene", g["start"],
                g["end"], ".", g["strand"], ".", attr])))
        (OUT / f"{w['track']}.gff3").write_text("\n".join(lines) + "\n")
    return windows


def cigar_blocks(cigar, target, query):
    operations = re.findall(r"(\d+)([=XIDM])", cigar)
    assert "".join(n + op for n, op in operations) == cigar
    t0, q0, matches, length = target, query, 0, 0
    for size, op in operations:
        size = int(size)
        if op in "ID" and size >= 1000:
            if length:
                yield t0, target, q0, query, matches, length
            target += size if op == "D" else 0
            query += size if op == "I" else 0
            t0, q0, matches, length = target, query, 0, 0
        else:
            target += size if op in "M=XD" else 0
            query += size if op in "M=XI" else 0
            matches += size if op in "M=" else 0
            length += size
    if length:
        yield t0, target, q0, query, matches, length


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cache-dir", type=Path,
        default=Path.home() / "Library/Caches/ggexon/mhc_haplotypes")
    args = parser.parse_args()
    snapshot_sources()
    OUT.mkdir(parents=True, exist_ok=True)
    args.cache_dir.mkdir(parents=True, exist_ok=True)
    windows = prepare_genes()
    sequence_records, alignments, links = [], [], []
    for w in windows:
        fasta = args.cache_dir / f"{w['track']}.fa"
        metadata = fasta.with_suffix(".json")
        request = {k: w[k] for k in ("sequence_url", "chr", "start", "end")}
        if fasta.exists() and metadata.exists():
            cached = json.loads(metadata.read_text())
            assert cached["request"] == request, "Sequence cache has different bounds"
            w["chr_length"] = cached["chr_length"]
        else:
            sequence, w["chr_length"] = fetch_sequence(w["sequence_url"], w["chr"], w["start"] - 1, w["end"])
            fasta.write_text(">" + w["track"] + "\n" + "\n".join(sequence[i:i + 80] for i in range(0, len(sequence), 80)) + "\n")
            metadata.write_text(json.dumps(dict(request=request, chr_length=w["chr_length"]), indent=2) + "\n")
        sequence_records.append(dict(track=w["track"], sha256=hashlib.sha256(fasta.read_bytes()).hexdigest(),
            **request, chr_length=w["chr_length"]))
        print("Sequence ready:", w["track"], w["end"] - w["start"] + 1, "bp", flush=True)
    write(OUT / "windows.tsv", windows)
    for target, query in zip(windows, windows[1:]):
        name = target["track"] + "_" + query["track"]
        paf = args.cache_dir / (name + ".paf")
        command = ["minimap2", "-x", "asm20", "-c", "--eqx", "--secondary=no", "-t", "2",
            target["track"] + ".fa", query["track"] + ".fa"]
        if not paf.exists():
            with paf.open("w") as handle, (args.cache_dir / (name + ".log")).open("w") as log:
                subprocess.run(command, cwd=args.cache_dir, stdout=handle, stderr=log, check=True)
        rows = []
        for index, line in enumerate(paf.read_text().splitlines(), 1):
            f = line.split("\t")
            if int(f[9]) / int(f[10]) < 0.7:
                continue
            tags = {v[:2]: v[5:] for v in f[12:]}
            qlen, strand = int(f[1]), f[4]
            oriented = int(f[2]) if strand == "+" else qlen - int(f[3])
            for block, (a, b, c, d, matches, length) in enumerate(cigar_blocks(tags["cg"], int(f[7]), oriented), 1):
                if min(b - a, d - c) < 500 or matches / length < 0.7:
                    continue
                c, d = (c, d) if strand == "+" else (qlen - d, qlen - c)
                a, b = a + target["start"] - 1, b + target["start"] - 1
                c, d = c + query["start"] - 1, d + query["start"] - 1
                rows.append("\t".join(map(str, [query["chr"], query["chr_length"], c, d, strand,
                    target["chr"], target["chr_length"], a, b, matches, length, 255])))
                links.append(dict(alignment=name, tchr=target["chr"], tstart=a + 1, tend=b,
                    qchr=query["chr"], qstart=c + 1, qend=d, strand=strand,
                    nmatch=matches, alen=length, source_row=index, cigar_block=block))
        assert rows, "No retained direct alignment blocks"
        (OUT / (name + ".paf")).write_text("\n".join(rows) + "\n")
        alignments.append(dict(name=name, target=target["track"], query=query["track"], command=command,
            raw_paf_sha256=hashlib.sha256(paf.read_bytes()).hexdigest(), blocks=len(rows)))
        print("Direct alignment:", name, len(rows), "blocks", flush=True)
    write(OUT / "links.tsv", links)
    files = sorted(p for p in OUT.iterdir() if p.suffix in (".tsv", ".gff3", ".paf"))
    manifest = dict(source=json.loads((SOURCE / "manifest.json").read_text()),
        selection=TRACKS, window_rule="Equal native spans: longest functional MHC extent among the selected tracks plus 200 kb; centred per track.",
        coordinates="GFF3/TSV: one-based closed native assembly coordinates; PAF starts: zero-based; no pre-reversal.",
        filtering="Retain functional curated MHC. Context: remove explicit pseudogene types, background symbols annotated as pseudogene in CHM13, and overlapping duplicate curated loci; choose the longest exonic transcript per overlapping same-name/strand locus.",
        background_status="CAT biotypes are unspecified. The background symbol filter is a display choice, not an ape functional-status inference.",
        alignment_filter="Direct minimap2 asm20; no secondary records; split at indels >=1000 bp; each span >=500 bp and identity >=70%; short internal indels remain inside ribbon envelopes; PAF mapQ 255 denotes unknown.",
        minimap2_version=subprocess.check_output(["minimap2", "--version"], text=True).strip(),
        sequences=sequence_records, alignments=alignments,
        files=[dict(file=p.name, md5=hashlib.md5(p.read_bytes()).hexdigest()) for p in files])
    (OUT / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")


if __name__ == "__main__":
    main()
