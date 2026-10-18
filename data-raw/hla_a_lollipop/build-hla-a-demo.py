"""Build the dense, offline HLA-A example from pinned public sources.

Run from any directory with Python 3. Downloads are cached outside the package.
Original FASTA records are copied verbatim; comparisons are computed in R.
"""

import csv
import gzip
import hashlib
import json
from pathlib import Path
import shutil
import urllib.request


COMMIT = "d972ae287948dc2e5468a40ff3b3d27227541189"
REFERENCE = "A*02:01:01:01"
SIGNAL_LENGTH = 24
ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "inst/extdata/hla_a_lollipop"
CACHE = Path.home() / ".cache/ggexon/hla_a_lollipop" / COMMIT
BASE = f"https://raw.githubusercontent.com/ANHIG/IMGTHLA/{COMMIT}/"


def write_tsv(name, rows):
    with (OUT / name).open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]), delimiter="\t")
        writer.writeheader()
        writer.writerows(rows)


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    CACHE.mkdir(parents=True, exist_ok=True)
    sources = []
    for name in ["fasta/A_prot.fasta", "alignments/A_prot.txt",
                 "release_version.txt", "LICENCE.md"]:
        target = CACHE / Path(name).name
        if not target.exists():
            urllib.request.urlretrieve(BASE + name, target)
        sources.append({"source": name, "url": BASE + name,
                        "sha256": hashlib.sha256(target.read_bytes()).hexdigest()})

    # Preserve the original headers and line wrapping of selected source records.
    records = {}
    for line in (CACHE / "A_prot.fasta").read_text().splitlines(keepends=True):
        if line.startswith(">"):
            allele = line.split()[1]
            records[allele] = []
        records[allele].append(line)
    sequences = {name: "".join(line.strip() for line in record[1:])
                 for name, record in records.items()}
    selected = [name for name, seq in sequences.items()
                if len(seq) == 365 and set(seq) <= set("ACDEFGHIKLMNPQRSTVWY")
                and name[-1].isdigit()]
    selected.sort(key=lambda name: (name != REFERENCE, name))
    assert len(selected) == 5998 and selected[0] == REFERENCE
    # Equal raw lengths alone do not prove matching residue coordinates.
    # Verify occupancy and sequences against the official gapped alignment.
    aligned = {}
    for line in (CACHE / "A_prot.txt").read_text().splitlines():
        fields = line.split()
        if fields and fields[0].startswith("A*"):
            aligned[fields[0]] = aligned.get(fields[0], "") + "".join(fields[1:])
    base = aligned["A*01:01:01:01"]
    def resolve(name):
        return "".join(b if a == "-" else a for a, b in zip(aligned[name], base))
    ref_gaps = [aa == "." for aa in resolve(REFERENCE)]
    manifest = []
    original_records = []
    for allele in selected:
        record = records[allele]
        sequence = sequences[allele]
        resolved = resolve(allele)
        assert resolved.replace(".", "") == sequence
        assert [aa == "." for aa in resolved] == ref_gaps
        original_records.append("".join(record))
        manifest.append({"allele": allele,
                         "allele_group": ":".join(allele.split(":")[:2]),
                         "ipd_id": record[0].split()[0][1:],
                         "reference": "TRUE" if allele == REFERENCE else "FALSE",
                         "precursor_length": len(sequence),
                         "signal_length": SIGNAL_LENGTH,
                         "sequence_sha256": hashlib.sha256(sequence.encode()).hexdigest()})
    (OUT / "hla_a_selected_proteins.fasta.gz").write_bytes(
        gzip.compress("".join(original_records).encode(), mtime=0))
    (OUT / "hla_a_selected_proteins.fasta").unlink(missing_ok=True)
    write_tsv("hla_a_alleles.tsv", manifest)

    annotation_path = OUT / "uniprot-P04439.json"
    # Keep the annotation snapshot bundled: subsequent builds are deterministic.
    annotation_url = "https://rest.uniprot.org/uniprotkb/P04439.json"
    if not annotation_path.exists():
        urllib.request.urlretrieve(annotation_url, annotation_path)
    annotation = json.loads(annotation_path.read_text())
    sources.append({"source": "UniProt P04439 annotation snapshot", "url": annotation_url,
                    "sha256": hashlib.sha256(annotation_path.read_bytes()).hexdigest()})
    domains = []
    for feature in annotation["features"]:
        description = feature.get("description", "")
        if (feature["type"] == "Region" and description in
                ["Alpha-1", "Alpha-2", "Alpha-3", "Connecting peptide"]):
            name = description
        elif feature["type"] == "Transmembrane":
            name = "Transmembrane"
        elif feature["type"] == "Topological domain" and description == "Cytoplasmic":
            name = "Cytoplasmic"
        else:
            continue
        domains.append({"domain": name,
                        "start": feature["location"]["start"]["value"] - SIGNAL_LENGTH,
                        "end": feature["location"]["end"]["value"] - SIGNAL_LENGTH})
    domains.sort(key=lambda row: row["start"])
    assert domains[0]["start"] == 1 and domains[-1]["end"] == 341
    write_tsv("hla_a_domains.tsv", domains)
    reference_path = OUT / "ipd-HLA00005-reference.json"
    reference_url = "https://www.ebi.ac.uk/cgi-bin/ipd/api/allele/HLA00005"
    if not reference_path.exists():
        urllib.request.urlretrieve(reference_url, reference_path)
    reference = json.loads(reference_path.read_text())
    assert reference["release_version"] == "3.66.0"
    assert reference["name"] == REFERENCE
    assert reference["sequence"]["protein"] == sequences[REFERENCE]
    genomic = reference["sequence"]["genomic"]
    features = reference["feature"]["genomic"]
    exons = [feature for feature in features if feature["type"] == "exon"]
    cds = "".join(genomic[exon["start"] - 1:exon["start"] + exon["length"] - 1]
                  for exon in exons)
    assert cds == reference["sequence"]["coding"] and len(cds) == 1098
    sources.append({"source": "IPD HLA00005 reference snapshot, release 3.66.0",
                    "url": reference_url,
                    "sha256": hashlib.sha256(reference_path.read_bytes()).hexdigest()})
    (OUT / "hla_a_reference_genomic.fasta").write_text(
        ">HLA00005 A*02:01:01:01 allele-local genomic sequence\n" +
        "\n".join(genomic[i:i + 60] for i in range(0, len(genomic), 60)) + "\n")
    write_tsv("hla_a_gene_structure.tsv", [
        {"type": f["type"], "number": f["number"], "start": f["start"],
         "end": f["start"] + f["length"] - 1, "length": f["length"]}
        for f in features if f["type"] in ["exon", "intron"]])
    start = exons[0]["start"]
    end = exons[-1]["start"] + exons[-1]["length"] - 1
    attrs = "gene_id=HLA_A0201;gene_name=HLA-A"
    gff = ["##gff-version 3", f"##sequence-region HLA00005 1 {len(genomic)}",
           f"HLA00005\tIPD\tgene\t{start}\t{end}\t.\t+\t.\tID=HLA_A0201;{attrs}",
           f"HLA00005\tIPD\tmRNA\t{start}\t{end}\t.\t+\t.\t"
           f"ID=HLA_A0201_tx;Parent=HLA_A0201;transcript_id=HLA_A0201_tx;{attrs}"]
    offset = 0
    for exon in exons:
        i, start = exon["number"], exon["start"]
        end = start + exon["length"] - 1
        phase = (3 - offset % 3) % 3
        for kind in ["exon", "CDS"]:
            gff.append(f"HLA00005\tIPD\t{kind}\t{start}\t{end}\t.\t+\t"
                       f"{phase if kind == 'CDS' else '.'}\t"
                       f"ID=HLA_A0201_tx.{kind}.{i};Parent=HLA_A0201_tx;"
                       f"transcript_id=HLA_A0201_tx;exon_number={i};{attrs}")
        offset += exon["length"]
    (OUT / "hla_a_reference.gff3").write_text("\n".join(gff) + "\n")
    write_tsv("hla_a_sources.tsv", sources)
    shutil.copyfile(CACHE / "LICENCE.md", OUT / "IPD-LICENCE.md")
    shutil.copyfile(CACHE / "release_version.txt", OUT / "release_version.txt")
    print(f"Wrote {len(manifest)} original allele records and {len(domains)} regions to {OUT}")


if __name__ == "__main__":
    main()
