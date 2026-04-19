#!/usr/bin/env python3

import argparse
import csv
import os
import re
import shutil
import subprocess
import sys
from collections import Counter
from pathlib import Path


def run_command(cmd):
    try:
        result = subprocess.run(
            cmd,
            check=True,
            capture_output=True,
            text=True,
        )
    except subprocess.CalledProcessError as exc:
        sys.stderr.write(exc.stderr)
        raise SystemExit(f"Command failed: {' '.join(cmd)}") from exc
    return result.stdout


def parse_interval_path(path_name):
    if ":" in path_name:
        core, interval = path_name.rsplit(":", 1)
        if re.fullmatch(r"\d+-\d+", interval):
            start_str, end_str = interval.split("-", 1)
            start = int(start_str)
            end = int(end_str)
        else:
            core = path_name
            start = 0
            end = None
    else:
        core = path_name
        start = 0
        end = None

    parts = core.split("#")
    sample = parts[0]

    if len(parts) == 2:
        chromosome = parts[1]
    elif len(parts) >= 4 and parts[1] == "0" and parts[-1] == "0":
        chromosome = "#".join(parts[2:-1])
    elif len(parts) >= 2:
        chromosome = "#".join(parts[1:])
    else:
        chromosome = ""

    return {
        "path_name": path_name,
        "path_core": core,
        "sample": sample,
        "chromosome": chromosome,
        "start": start,
        "end": end,
    }


def safe_label(path_info, sample_counts):
    sample = path_info["sample"]
    if sample_counts[sample] == 1:
        return sample
    return re.sub(r"[^A-Za-z0-9]+", "_", path_info["path_core"]).strip("_")


def parse_segments(odgi_bin, og_path):
    gfa_text = run_command([odgi_bin, "view", "-i", str(og_path), "-g"])
    segments = {}
    for line in gfa_text.splitlines():
        if not line or not line.startswith("S\t"):
            continue
        _, node_id, sequence, *_ = line.split("\t")
        segments[int(node_id)] = sequence
    return segments


def parse_path_steps(odgi_bin, og_path):
    gfa_text = run_command([odgi_bin, "view", "-i", str(og_path), "-g"])
    path_steps = {}
    for line in gfa_text.splitlines():
        if not line:
            continue

        if line.startswith("P\t"):
            fields = line.rstrip().split("\t")
            if len(fields) < 3:
                continue
            path_name = fields[1]
            steps = fields[2]
            step_list = []
            for step in steps.split(","):
                step = step.strip()
                if not step:
                    continue
                orientation = step[-1]
                node_id = int(step[:-1])
                step_list.append((node_id, orientation))
            path_steps[path_name] = step_list
    return path_steps


def list_paths(odgi_bin, og_path):
    stdout = run_command([odgi_bin, "paths", "-i", str(og_path), "-L"])
    return [line.strip() for line in stdout.splitlines() if line.strip()]


def collect_path_positions(odgi_bin, og_path, path_name, start_offset):
    stdout = run_command(
        [odgi_bin, "position", "-i", str(og_path), "-r", path_name, "--all-positions"]
    )
    positions = {}
    lines = stdout.splitlines()
    if not lines:
        return positions

    for line in lines[1:]:
        if not line.strip():
            continue
        path, node_id, local_position = line.split("\t")
        del path
        abs_position = start_offset + int(local_position)
        positions.setdefault(int(node_id), []).append(abs_position)
    return positions


def collect_path_strands(path_steps, path_name):
    strands = {}
    for node_id, orientation in path_steps.get(path_name, []):
        strands.setdefault(node_id, []).append(orientation)
    return strands


def choose_output_path(og_path, output_path):
    if output_path:
        return Path(output_path)
    base = og_path.name
    if base.endswith(".og"):
        base = base[:-3]
    return og_path.with_name(f"{base}.node_table.tsv")


def resolve_odgi_binary(requested_odgi):
    candidates = []
    if requested_odgi:
        candidates.append(requested_odgi)
    env_odgi = os.environ.get("ODGI_BIN")
    if env_odgi:
        candidates.append(env_odgi)
    candidates.append("odgi")

    checked = []
    for candidate in candidates:
        if Path(candidate).is_absolute():
            checked.append(candidate)
            if Path(candidate).exists() and os.access(candidate, os.X_OK):
                return candidate
            continue

        resolved = shutil.which(candidate)
        checked.append(candidate)
        if resolved:
            return resolved

    checked_str = ", ".join(checked)
    raise SystemExit(
        "odgi executable not found. "
        f"Tried: {checked_str}. "
        "Pass --odgi /path/to/odgi, set ODGI_BIN, or add odgi to your PATH."
    )


def main():
    parser = argparse.ArgumentParser(
        description=(
            "Create a node-by-node TSV from an ODGI graph with sequence and "
            "per-path absolute start/end coordinates."
        )
    )
    parser.add_argument(
        "--og",
        required=True,
        help="Input ODGI graph (.og).",
    )
    parser.add_argument(
        "--output",
        help="Output TSV path. Defaults to <graph>.node_table.tsv next to the input graph.",
    )
    parser.add_argument(
        "--odgi",
        help=(
            "Path to the odgi executable. If omitted, the script checks ODGI_BIN "
            "and then falls back to odgi on PATH."
        ),
    )
    args = parser.parse_args()

    og_path = Path(args.og)
    odgi_bin = resolve_odgi_binary(args.odgi)
    output_path = choose_output_path(og_path, args.output)

    if not og_path.exists():
        raise SystemExit(f"Input graph not found: {og_path}")

    segments = parse_segments(odgi_bin, og_path)
    path_steps = parse_path_steps(odgi_bin, og_path)
    path_names = list_paths(odgi_bin, og_path)
    path_infos = [parse_interval_path(path_name) for path_name in path_names]
    sample_counts = Counter(info["sample"] for info in path_infos)

    for info in path_infos:
        info["label"] = safe_label(info, sample_counts)
        info["positions"] = collect_path_positions(
            odgi_bin, og_path, info["path_name"], info["start"]
        )
        info["strands"] = collect_path_strands(path_steps, info["path_name"])

    fieldnames = ["node_id", "sequence"]
    for info in path_infos:
        fieldnames.append(f"{info['label']}_chromosome")
        fieldnames.append(f"{info['label']}_strand")
        fieldnames.append(f"{info['label']}_absolute_start")
        fieldnames.append(f"{info['label']}_absolute_end")

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, delimiter="\t")
        writer.writeheader()

        for node_id in sorted(segments):
            row = {"node_id": node_id, "sequence": segments[node_id]}
            node_length = len(segments[node_id])
            for info in path_infos:
                row[f"{info['label']}_chromosome"] = info["chromosome"]
                positions = info["positions"].get(node_id)
                strands = info["strands"].get(node_id)
                row[f"{info['label']}_strand"] = (
                    ",".join(strand for strand in strands) if strands else "NA"
                )
                if positions:
                    starts = [str(pos) for pos in positions]
                    ends = [str(pos + node_length - 1) for pos in positions]
                    row[f"{info['label']}_absolute_start"] = ",".join(starts)
                    row[f"{info['label']}_absolute_end"] = ",".join(ends)
                else:
                    row[f"{info['label']}_absolute_start"] = "NA"
                    row[f"{info['label']}_absolute_end"] = "NA"
            writer.writerow(row)

    print(output_path)


if __name__ == "__main__":
    main()
