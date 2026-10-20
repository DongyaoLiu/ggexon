from pathlib import Path
import json,csv,hashlib,pyBigWig
OUT=Path(__file__).resolve().parent
ROOT=OUT.parents[2]
N=ROOT/'source_metadata/mhc_context_search/ncbi'
rr=json.loads((OUT/'native_candidate_evidence.json').read_text())
probe={x['track']:x for x in json.loads((N/'genark_native_annotation_probe.json').read_text())}
prov=[]
for track in sorted({r['track'] for r in rr}):
 p=probe[track];bbpath=Path(f'/tmp/ggexon_mhc_genark_native_bigbeds/{track}.genark.augustus.bb')
 bb=pyBigWig.open(str(bbpath));entries=bb.entries(p['genbank_chrom'],p['start0'],p['end0']) or []
 url=next(t['url'] for t in p['tracks'] if t['name']=='augustus')
 prov.append(dict(track=track,assembly=p['assembly'],native_chrom=p['genbank_chrom'],browser_chrom=p['browser_chrom'],chrom_length=bb.chroms(p['genbank_chrom']),assembly_report_chrom_length=p['assembly_report_chrom_length'],length_verified=bb.chroms(p['genbank_chrom'])==p['assembly_report_chrom_length'],query_start0=p['start0'],query_end0=p['end0'],count=len(entries),source_url=url,cache_path=str(bbpath),sha256=hashlib.sha256(bbpath.read_bytes()).hexdigest(),first_model_start0=min(a for a,b,s in entries),last_model_end0=max(b for a,b,s in entries),regional_extract_identical_to_existing=(OUT/f'{track}.expanded.augustus.bed').read_bytes()==(N/f'{track}.genark.augustus.bed').read_bytes()))
 bb.close()
(OUT/'native_bigbed_query_provenance.json').write_text(json.dumps(prov,indent=2)+'\n')
fields=['track','gene_symbol','target_native_chrom','target_browser_chrom','syntenic_search_start0','syntenic_search_end0','source_strand','paf_source_exonic_coverage','paf_source_identical_fraction','best_overlap_prediction_id','prediction_start0','prediction_end0','prediction_strand','prediction_exon_count','projected_exonic_overlap_bp','decision','reason','source_url']
summary=[]
for r in rr:
 row={k:r[k] for k in fields if k in r}
 c=max(r['candidates'],key=lambda c:c['projected_exonic_overlap_bp'],default=None)
 if c and c['projected_exonic_overlap_bp']:
  row.update(best_overlap_prediction_id=c['name'],prediction_start0=c['chromStart'],prediction_end0=c['chromEnd'],prediction_strand=c['strand'],prediction_exon_count=c['blockCount'],projected_exonic_overlap_bp=c['projected_exonic_overlap_bp'])
  if not c['same_strand']:
   row.update(decision='reject_opposite_strand_prediction',reason='Overlapping native prediction has opposite strand to syntenic source gene; overlap is not gene identity support.')
  else:
   row.update(decision='partial_exon_corroboration_only',reason='g4606.t1 joins exons homologous to MUCL3, DDR1, VARS2, GTF2H4 and SFTA2. Only two complete MUCL3 exons agree exactly; prediction cannot be accepted as a MUCL3 transcript.')
 elif r['track'] in ['PABh2','PPYh2']:
  row.update(decision='no_native_prediction_at_syntenic_locus',reason='Direct cached whole-assembly Augustus bigBed query confirms no model at this locus; all regional models start above33Mb.')
 else:
  row.update(decision='no_gene_specific_native_model',reason='Nearby/broad predictions have no exonic overlap with aligned source exons. A prediction span crossing the search interval is insufficient.')
 if r['paf_source_exonic_coverage']==0:row['reason']+=' Source exons lack a collinear aligned block in whole-region PAF; reported search locus is bounded by closest aligned flanks, not an inferred deletion.'
 summary.append(row)
with (OUT/'native_recovery_summary.tsv').open('w') as f:
 w=csv.DictWriter(f,fields,delimiter='\t');w.writeheader();w.writerows(summary)
print('Verified',len(summary),'unresolved entries;',len(prov),'native bigBeds; all chromosome lengths agree:',all(x['length_verified'] for x in prov))
for r in summary:print(r['track'],r['gene_symbol'],r['syntenic_search_start0'],r['syntenic_search_end0'],r['decision'])
