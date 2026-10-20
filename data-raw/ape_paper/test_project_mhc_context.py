"""Hand-checked contracts: native coordinates, strand and projection filters."""
import unittest
from project_mhc_context import native_span, parse_liftoff, eligible_context

class ContextProjectionTest(unittest.TestCase):
    def test_mhc_symbols_are_not_transferred_even_if_source_matching_missed_them(self):
        self.assertFalse(eligible_context(dict(native_mhc_model='FALSE',gene_symbol='HLA-DRB1')))
        self.assertFalse(eligible_context(dict(native_mhc_model='TRUE',gene_symbol='TAP1')))
        self.assertTrue(eligible_context(dict(native_mhc_model='FALSE',gene_symbol='TAP1')))

    def test_one_based_regional_span_becomes_native_bed_without_scaling(self):
        self.assertEqual(native_span(101,150,'+',dict(start0='10000',end0='11000',orientation='+')),(10100,10150,'+'))
        self.assertEqual(native_span(101,150,'+',dict(start0='10000',end0='11000',orientation='-')),(10850,10900,'-'))
        self.assertEqual(native_span(1,1000,'-',dict(start0='10000',end0='11000',orientation='-')),(10000,11000,'+'))

    def test_projection_preserves_native_exons_and_marks_inferred_evidence(self):
        gff='''ALT\tLiftoff\tgene\t101\t150\t.\t+\t.\tID=ctx1;coverage=0.98;sequence_ID=0.99
ALT\tLiftoff\tmRNA\t101\t150\t.\t+\t.\tID=ctx1.t1;Parent=ctx1
ALT\tLiftoff\texon\t101\t110\t.\t+\t.\tID=e1;Parent=ctx1.t1
ALT\tLiftoff\texon\t141\t150\t.\t+\t.\tID=e2;Parent=ctx1.t1
'''
        window=dict(track='ALT',chr='chr23_hap2',species_id='SSY',haplotype='h2',start0='10000',end0='11000',orientation='-')
        source={'ctx1':dict(model_id='PRIMARY:NM_1',gene_symbol='TEST1',gene_type='protein_coding',source_url='https://source.example/models.bb',track='PRIMARY')}
        models,exons,audit=parse_liftoff(gff,window,source)
        self.assertEqual(len(models),1)
        self.assertEqual((models[0]['chromStart'],models[0]['chromEnd'],models[0]['strand']),(10850,10900,'-'))
        self.assertEqual([(e['start0'],e['end0']) for e in exons],[(10850,10860),(10890,10900)])
        self.assertEqual(models[0]['annotation_evidence'],'same-species Liftoff projection')
        self.assertEqual(models[0]['projection_source_model_id'],'PRIMARY:NM_1')
        self.assertEqual(audit[0]['status'],'accepted')
        self.assertEqual(models[0]['blockSizes'],'10,10,')
        self.assertEqual(models[0]['blockStarts'],'0,40,')

    def test_low_coverage_and_low_identity_do_not_become_context_genes(self):
        source={x:dict(model_id=x,gene_symbol=x,gene_type='',source_url='',track='PRIMARY') for x in ['a','b']}
        window=dict(track='ALT',chr='chr1',species_id='PTR',haplotype='h2',start0='100',end0='1100',orientation='+')
        gff='ALT\tLiftoff\tgene\t1\t20\t.\t+\t.\tID=a;coverage=0.9;sequence_ID=0.99\nALT\tLiftoff\tgene\t30\t50\t.\t+\t.\tID=b;coverage=1;sequence_ID=0.94\n'
        models,exons,audit=parse_liftoff(gff,window,source)
        self.assertEqual(models,[])
        self.assertEqual(exons,[])
        self.assertEqual({a['status'] for a in audit},{'below_threshold'})

    def test_wrong_regional_chromosome_or_out_of_bounds_is_rejected(self):
        with self.assertRaises(ValueError):native_span(0,20,'+',dict(start0='100',end0='1100',orientation='+'))
        with self.assertRaises(ValueError):native_span(1,1001,'+',dict(start0='100',end0='1100',orientation='+'))
        with self.assertRaises(ValueError):parse_liftoff('WRONG\tLiftoff\tgene\t1\t20\t.\t+\t.\tID=a;coverage=1;sequence_ID=1\n',dict(track='ALT',chr='chr1',start0='100',end0='1100',orientation='+'),{})

if __name__=='__main__':unittest.main()
