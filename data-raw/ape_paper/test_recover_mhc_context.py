"""Recovery must preserve strict sequence support and reject displaced paralogs."""
import unittest
from recover_mhc_context import choose_recoveries, qualifies_rna_locus

class RecoveryTest(unittest.TestCase):
    def candidate(self,**updates):
        r=dict(target='ALT',gene_symbol='GENE',source_model_id='source1',coverage=.99,identity=.99,global_identity=.99,native_start0=10100,native_end0=11000,exonic_bp=800,source_strand='+',strand='+',expected_start0=10000,expected_end0=12000)
        r.update(updates);return r

    def test_rna_requires_source_assignment_and_exonic_strand_agreement(self):
        good=dict(source_usable=True,primary=True,mapq=60,strand='+',gene_strand='+',identity=.99,query_coverage=.99,exonic_overlap=100)
        self.assertTrue(qualifies_rna_locus(good))
        for change in [dict(source_usable=False),dict(strand='-'),dict(exonic_overlap=0),dict(primary=False),dict(identity=.94),dict(query_coverage=.5)]:
            self.assertFalse(qualifies_rna_locus(dict(good,**change)))

    def test_accepts_only_one_supported_isoform_per_locus(self):
        a=self.candidate(source_model_id='best',identity=1.,global_identity=1.)
        b=self.candidate(source_model_id='longer',exonic_bp=1500)
        self.assertEqual([r['source_model_id'] for r in choose_recoveries([b,a])],['best'])

    def test_partial_weak_reverse_and_displaced_candidates_are_rejected(self):
        bad=[self.candidate(coverage=.94),self.candidate(identity=.94),self.candidate(global_identity=.94),self.candidate(strand='-'),self.candidate(native_start0=30000,native_end0=32000)]
        self.assertEqual(choose_recoveries(bad),[])

    def test_keeps_separate_haplotypes_and_genes(self):
        rows=[self.candidate(),self.candidate(target='ALT2'),self.candidate(gene_symbol='GENE2')]
        self.assertEqual(len(choose_recoveries(rows)),3)

if __name__=='__main__':unittest.main()
