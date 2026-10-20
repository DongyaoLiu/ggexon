#!/usr/bin/env python3
"""Independently reconstruct every source PAF CIGAR against window FASTAs."""
from pathlib import Path
import re

total = reverse = 0
for folder in sorted((Path(__file__).resolve().parent / 'results').iterdir()):
    if not folder.is_dir():
        continue
    sequences = {f.stem: ''.join(f.read_text().splitlines()[1:]).upper()
                 for f in folder.glob('*.fa')}
    for path in sorted(folder.glob('*.paf')):
        count = negative = 0
        for line in path.read_text().splitlines():
            fields = line.split('\t')
            query = sequences[fields[0]][int(fields[2]):int(fields[3])]
            target = sequences[fields[5]][int(fields[7]):int(fields[8])]
            if fields[4] == '-':
                query = query.translate(str.maketrans('ACGTN', 'TGCAN'))[::-1]
                negative += 1
            cigar = next(f[5:] for f in fields[12:] if f.startswith('cg:Z:'))
            operations = re.findall(r'(\d+)([MID])', cigar)
            assert ''.join(n + op for n, op in operations) == cigar
            qi = ti = matches = 0
            for length, operation in operations:
                n = int(length)
                if operation == 'M':
                    matches += sum(a == b and a != 'N' for a, b in
                                   zip(query[qi:qi+n], target[ti:ti+n]))
                    qi += n
                    ti += n
                elif operation == 'I':
                    qi += n
                else:
                    ti += n
            assert qi == len(query) and ti == len(target), (path, fields[:12])
            assert matches == int(fields[9]), (path, fields[:12], matches)
            count += 1
        total += count
        reverse += negative
        print(f'{path.name}: PASS {count} alignments ({negative} reverse)')
assert total > 0
print(f'PASS: {total} alignments ({reverse} reverse), exact CIGAR extents and match counts')
