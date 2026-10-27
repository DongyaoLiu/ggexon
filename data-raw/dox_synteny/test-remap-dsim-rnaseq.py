"""Regression checks for converting genuine splice alignments to locus depth."""
import importlib.util
from pathlib import Path
import unittest

MODULE = Path(__file__).with_name("remap-dsim-rnaseq.py")
spec = importlib.util.spec_from_file_location("remap_dsim", MODULE)
remap = importlib.util.module_from_spec(spec)
spec.loader.exec_module(remap)


class CoverageTests(unittest.TestCase):
    def test_cigar_coordinates_skip_introns_deletions_and_insertions(self):
        self.assertEqual(remap.aligned_blocks(95, "5S10M5N4M2D3M1I2M"),
                         [(95, 104), (110, 113), (116, 118), (119, 120)])

    def test_reverse_library_uses_both_mates_to_infer_transcript_strand(self):
        for flag in [81, 161]:
            self.assertEqual(remap.transcript_strand(flag), "+")
        for flag in [97, 145]:
            self.assertEqual(remap.transcript_strand(flag), "-")
        for flag in [0, 1, 193]:
            with self.assertRaises(ValueError):
                remap.transcript_strand(flag)

    def test_clipped_locus_depth_retains_real_aligned_bases_only(self):
        def sam(name, flag, pos, cigar, mq=60, chrom="CM015611.1"):
            return f"{name}\t{flag}\t{chrom}\t{pos}\t{mq}\t{cigar}\t*\t0\t0\tA\tI\n"
        lines = [sam("plus", 81, 95, "5S10M5N4M2D3M1I2M"),
                 sam("minus", 145, 103, "3M"),
                 sam("low_quality", 81, 100, "21M", mq=19),
                 sam("secondary", 337, 100, "21M"),
                 sam("supplementary", 2129, 100, "21M"),
                 sam("other_chr", 81, 100, "21M", chrom="other")]
        plus, minus, stats = remap.locus_depth(lines, "CM015611.1", 100, 120)
        expected_plus = [1 if p in list(range(100, 105)) + list(range(110, 114)) +
                         list(range(116, 121)) else 0 for p in range(100, 121)]
        expected_minus = [1 if 103 <= p <= 105 else 0 for p in range(100, 121)]
        self.assertEqual(plus, expected_plus)
        self.assertEqual(minus, expected_minus)
        self.assertEqual(stats["accepted_alignments"], 2)
        self.assertEqual(stats["accepted_spliced_alignments"], 1)

    def test_nonzero_runs_preserve_per_base_depth_and_reverse_sign(self):
        rows = remap.depth_rows([0, 2, 2, 0, 1], [1, 1, 0, 0, 0], 100)
        self.assertEqual([(r["start"], r["end"], r["score"], r["strand"]) for r in rows],
                         [(101, 102, 2, "+"), (104, 104, 1, "+"), (100, 101, -1, "-")])
        self.assertTrue(all(r["species"] == "Dsim" and r["sample"] == "GSM7039340"
                            for r in rows))


if __name__ == "__main__":
    unittest.main()
