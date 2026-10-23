import unittest
from mhc_continuous_utils import clip_block,display_interval,split_cigar
class Coordinates(unittest.TestCase):
 def test_positive_block_clips_both_genomes(self):
  self.assertEqual(clip_block(100,200,1000,1100,'+',125,180),(125,180,1025,1080))
 def test_reverse_block_clips_from_other_end(self):
  self.assertEqual(clip_block(100,200,1000,1100,'-',125,180),(125,180,1020,1075))
 def test_display_preserves_length_in_either_orientation(self):
  self.assertEqual(display_interval(120,180,100,300,'+'),(20,80))
  self.assertEqual(display_interval(120,180,100,300,'-'),(120,180))
 def test_alignment_ribbons_split_large_native_gaps(self):
  self.assertEqual(split_cigar('100M2000I50M1500D25M',0,0,'+',split_gap=1000),[(0,100,0,100),(100,150,2100,2150),(1650,1675,2150,2175)])
if __name__=='__main__':unittest.main()
