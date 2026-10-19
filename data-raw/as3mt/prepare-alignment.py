"""Fetch assembly-checked reference windows and align them with LASTZ.

Run from the repository root. The supplied handoff remains the annotation source.
"""
import concurrent.futures
import argparse
import hashlib
import json
import pathlib
import subprocess
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parent
WINDOWS = {
    "human": ("homo_sapiens", "GRCh38", 102869470, 102901899),
    "chimp": ("pan_troglodytes", "Pan_tro_3.0", 104324156, 104356197),
}


def fetch(item):
    name, (species, assembly, start, end) = item
    base = "https://rest.ensembl.org/"
    info = json.load(urllib.request.urlopen(
        base + "info/assembly/" + species + "?content-type=application/json"))
    # GRCh38.p14 has the same primary chromosome coordinates as GRCh38.
    assert info["assembly_name"].split(".p")[0] == assembly
    url = base + f"sequence/region/{species}/10:{start}..{end}:1?content-type=text/plain"
    seq = urllib.request.urlopen(url, timeout=90).read().decode().strip().upper()
    assert len(seq) == end - start + 1 and set(seq) <= set("ACGTN")
    (ROOT / (name + ".fa")).write_text(">" + name + "\n" + seq + "\n")
    return dict(species=name, assembly=info["assembly_name"], chr="chr10",
                start=start, end=end, url=url,
                sequence_sha256=hashlib.sha256(seq.encode()).hexdigest())


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--reuse-windows", action="store_true",
                        help="Reuse and hash-check the previously downloaded DNA windows")
    args = parser.parse_args()
    if args.reuse_windows:
        records = json.loads((ROOT / "sequence-provenance.json").read_text())["windows"]
        for record in records:
            seq = "".join((ROOT / (record["species"] + ".fa")).read_text().splitlines()[1:])
            assert hashlib.sha256(seq.encode()).hexdigest() == record["sequence_sha256"]
    else:
        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as ex:
            records = list(ex.map(fetch, WINDOWS.items()))
    # Disable gapped extension but retain the original chaining selection.
    # LASTZ still emits individual gap-free blocks; verify that below.
    # Keep the previous human_chimp.paf and its provenance as the gapped baseline.
    cmd = ["lastz", "human.fa", "chimp.fa", "--strand=both", "--nogapped", "--chain",
           "--format=paf:minimap2", "--output=human_chimp.nogap.paf"]
    subprocess.run(cmd, cwd=ROOT, check=True)
    # Check every block's spans, CIGAR, and exact match count against input DNA.
    sequences = {r["species"]: "".join((ROOT / (r["species"] + ".fa")).read_text().splitlines()[1:])
                 for r in records}
    count = 0
    for line in (ROOT / "human_chimp.nogap.paf").read_text().splitlines():
        f = line.split("\t")
        length = int(f[10])
        assert int(f[3]) - int(f[2]) == int(f[8]) - int(f[7]) == length
        assert "cg:Z:" + str(length) + "M" in f[12:]
        query = sequences[f[0]][int(f[2]):int(f[3])]
        target = sequences[f[5]][int(f[7]):int(f[8])]
        if f[4] == "-":
            query = query.translate(str.maketrans("ACGTN", "TGCAN"))[::-1]
        assert sum(a == b for a, b in zip(query, target)) == int(f[9])
        count += 1
    assert count > 0
    (ROOT / "sequence-provenance.nogap.json").write_text(json.dumps(dict(
        windows=records, command=cmd,
        alignment_mode="ungapped blocks with chaining selection; mismatches allowed, no insertions or deletions within blocks",
        verified_raw_blocks=count,
        # LASTZ prints its version but exits with status 1 on this build.
        lastz_version=subprocess.run(["lastz", "--version"], text=True,
                                     capture_output=True).stdout.strip(),
        paf_sha256=hashlib.sha256((ROOT / "human_chimp.nogap.paf").read_bytes()).hexdigest()
    ), indent=2) + "\n")
    print(f"Generated and sequence-verified {count} ungapped LASTZ blocks.")
