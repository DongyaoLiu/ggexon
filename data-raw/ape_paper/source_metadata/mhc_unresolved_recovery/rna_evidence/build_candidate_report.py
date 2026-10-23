#!/usr/bin/env python3
"""Produce fourteen-row evidence report without modifying gene models or figures."""
import csv,json,pathlib,collections
HERE=pathlib.Path(__file__).resolve().parent;APE=HERE.parents[2]
def rows(p):return list(csv.DictReader(open(p),delimiter='\t'))
def write(name,rr):
 with open(HERE/name,'w') as f:
  w=csv.DictWriter(f,fieldnames=list(rr[0]),delimiter='\t');w.writeheader();w.writerows(rr)
def ints(v):return [int(x) for x in v.rstrip(',').split(',')]
def overlap(a,b):return sum(max(0,min(y,v)-max(x,u)) for x,y in a for u,v in b)
audit=[r for r in rows(APE/'results/mhc_continuous/gene_models_with_projected_context/projection-audit.tsv') if r['status']!='accepted']
reps={r['model_id']:r for r in rows(APE/'results/mhc_continuous/figures/before-alt-context/representative-models.tsv')}
meta=rows(APE/'source_metadata/mhc_context_search/ncbi/genark_xenoRefGene_mhc_accession_metadata.tsv')
probes={r['track']:r for r in json.load(open(APE/'source_metadata/mhc_context_search/ncbi/genark_native_annotation_probe.json'))}
diag={(r['track'],r['gene_symbol']):r for r in json.load(open(APE/'results/mhc_continuous/context_recovery/diagnostics/unresolved_liftoff_diagnostics.json'))}
windows={r['track']:r for r in rows(APE/'results/mhc_continuous/windows.tsv')}
experiment_metadata={}
for mf in HERE.glob('*.ENA.metadata.tsv'):
 for row in rows(mf):experiment_metadata[row['run_accession']]=row
evidence=[]
for f in sorted(HERE.glob('*.source_loci.evidence.json')):
 for e in json.load(open(f)):
  run=e['read_name'].split('.')[0];em=experiment_metadata.get(run,{})
  e['experimental_run_accession']=run;e['experimental_source_species']=em.get('scientific_name','unqueried');e['experimental_library_source']=em.get('library_source','unqueried');e['experimental_sample_title']=em.get('sample_title','unqueried');e['experimental_study_accession']=em.get('study_accession','unqueried')
  ar=next(a for a in audit if a['track']==e['track'] and a['gene_symbol']==e['gene_symbol']);s=reps[ar['source_model_id']]
  starts=ints(s['blockStarts']);sizes=ints(s['blockSizes']);exons=[(int(s['chromStart'])+b,int(s['chromStart'])+b+n) for b,n in zip(starts,sizes)]
  e['source_gene_strand']=s['strand'];e['same_strand']=e['strand']==s['strand'];e['source_exon_overlap_bp']=overlap(e['blocks_bed0'],exons)
  e['source_gene_exonic_bp']=sum(b-a for a,b in exons);e['source_exon_fraction']=round(e['source_exon_overlap_bp']/e['source_gene_exonic_bp'],5)
  sj=[(exons[i][1],exons[i+1][0]) for i in range(len(exons)-1)]
  e['junctions_matching_source_model_within5bp']=sum(any(abs(a-u)<=5 and abs(b-v)<=5 for u,v in sj) for a,b in e['junctions_bed0'])
  e['source_locus_usable']=e['primary'] and e['mapq']>=20 and e['same_strand'] and e['source_exon_overlap_bp']>=100
  e['source_envelope_spanned']=e['read_start0']<=int(s['chromStart']) and e['read_end0']>=int(s['chromEnd'])
  evidence.append(e)
(HERE/'source_rna_read_qc.json').write_text(json.dumps(evidence,indent=2))
if evidence:
 flat=[dict(e,blocks_bed0=json.dumps(e['blocks_bed0']),junctions_bed0=json.dumps(e['junctions_bed0'])) for e in evidence];write('source_rna_read_qc.tsv',flat)
# Realign only sense-strand, >=100bp annotated-exon overlap, primary MAPQ20 reads.
# Retrieve FASTQ from permissive regional extracts. A read may intersect multiple genes.
fastq={}
for f in HERE.glob('*.source_loci.fastq'):
 lines=f.read_text().splitlines()
 for i in range(0,len(lines),4):fastq[(f.name.split('.bam.')[0]+'.bam',lines[i][1:])]='\n'.join(lines[i:i+4])+'\n'
for a in audit:
 key=(a['track'],a['gene_symbol']);good=[e for e in evidence if (e['track'],e['gene_symbol'])==key and e['source_locus_usable']]
 seqs={e['read_name']:fastq[(e['bam_url'].split('/')[-1],e['read_name'])] for e in good}
 (HERE/f'{key[0]}.{key[1]}.source_sense_exonic.fastq').write_text(''.join(seqs.values()))
report=[];bed=[]
for a in audit:
 key=(a['track'],a['gene_symbol']);s=reps[a['source_model_id']];p=probes[a['track']];w=windows[a['track']]
 hits=[m for m in meta if m['track']==a['track'] and a['gene_symbol'] in m['source_gene_symbols'].split(';')]
 assert len(hits)<=1
 h=hits[0] if hits else None;d=diag[key]
 ds=de='';dst=''
 if d.get('regional_start1'):
  if w['orientation']=='+':ds=int(w['start0'])+int(d['regional_start1'])-1;de=int(w['start0'])+int(d['regional_end1']);dst=d['regional_strand']
  else:ds=int(w['end0'])-int(d['regional_end1']);de=int(w['end0'])-int(d['regional_start1'])+1;dst='-' if d['regional_strand']=='+' else '+'
 raw='';blocks0=[];xurl='';strandcheck='NA';spanoverlap=0
 if h:
  bf=APE/f'source_metadata/mhc_context_search/ncbi/{a["track"]}.genark.xenoRefGene.bed'
  raw=next(line for line in bf.read_text().splitlines() if line.split('\t')[3]==h['refseq_accession'] and line.split('\t')[1]==h['start0'])
  bed.append(raw);bs=ints(h['block_sizes']);bt=ints(h['block_starts']);blocks0=[[int(h['start0'])+b,int(h['start0'])+b+n] for b,n in zip(bt,bs)]
  xurl=next(t['url'] for t in p['tracks'] if t['name']=='xenoRefGene')
  strandcheck=str(h['strand']==dst) if dst else 'NA';spanoverlap=max(0,min(int(h['end0']),de)-max(int(h['start0']),ds)) if ds!='' else 0
 allreads=[e for e in evidence if (e['track'],e['gene_symbol'])==key]
 good=[e for e in allreads if e['source_locus_usable']]
 if h and h['source_organism']=='Homo sapiens':quality='human_cross_species_translated_alignment_only';rec='candidate_locus_support_only; not a native target exon model'
 elif h:quality='nonprimate_cross_species_translated_alignment_only';rec='weak LY6-family locus hint; insufficient to identify LY6G6E or recover exons'
 else:quality='no_symbol_matched_RefSeq_alignment';rec='no candidate from xenoRefGene; do not substitute olfactory paralogs'
 if good:
  if any(e['junctions_matching_source_model_within5bp'] for e in good):rec+='; partial same-species source RNA has matched source splice junctions; realign to target'
  else:rec+='; only source RNA fragments without source-junction concordance; weak candidates for target realignment'
 else:rec+='; no usable same-strand exonic RNA in sampled source intervals; nonexpression not gene absence'
 report.append({'track':a['track'],'gene_symbol':a['gene_symbol'],'target_browser_chrom':w['chr'],'target_genbank_chrom':p['genbank_chrom'],'refseq_accession':h['refseq_accession'] if h else 'NA','refseq_source_species':h['source_organism'] if h else 'NA','refseq_symbol':h['source_gene_symbols'] if h else 'NA','raw_native_start0':h['start0'] if h else 'NA','raw_native_end0':h['end0'] if h else 'NA','raw_native_strand':h['strand'] if h else 'NA','block_count':h['block_count'] if h else 'NA','raw_block_sizes':h['block_sizes'] if h else 'NA','raw_block_starts':h['block_starts'] if h else 'NA','native_blocks_bed0':json.dumps(blocks0),'xenoRefGene_url':xurl or 'NA','NCBI_accession_url':'https://www.ncbi.nlm.nih.gov/nuccore/'+h['refseq_accession'] if h else 'NA','diagnostic_liftoff_native_start0':ds,'diagnostic_liftoff_native_end0':de,'diagnostic_liftoff_native_strand':dst,'xeno_diagnostic_span_overlap_bp':spanoverlap,'xeno_diagnostic_strand_match':strandcheck,'evidence_quality':quality,'source_track':a['source_track'],'source_model_id':a['source_model_id'],'primary_rna_source_chrom':s['annotation_chr'],'primary_rna_query_start0':s['chromStart'],'primary_rna_query_end0':s['chromEnd'],'primary_rna_query_strand':s['strand'],'direct_target_aligned_rna_available':False,'primary_rna_interval_overlap_records':len(allreads),'primary_rna_usable_read_count':len(good),'primary_rna_usable_read_ids':';'.join(e['read_name'] for e in good),'primary_rna_max_source_exon_fraction':max([e['source_exon_fraction'] for e in good],default=0),'primary_rna_read_count_with_source_junction_match':sum(bool(e['junctions_matching_source_model_within5bp']) for e in good),'primary_rna_usable_spliced_count':sum(bool(e['junctions_bed0']) for e in good),'primary_rna_urls':';'.join(sorted(set(e['bam_url'] for e in good))),'recovery_assessment':rec})
write('unresolved14_rna_candidates.tsv',report)
(HERE/'symbol_matched_xenoRefGene_candidates.raw.bed').write_text('\n'.join(bed)+'\n')
assert len(report)==14
print('REPORT',len(report),'xeno',sum(r['refseq_accession']!='NA' for r in report),'usable RNA',[(r['track'],r['gene_symbol'],r['primary_rna_usable_read_count']) for r in report if r['primary_rna_usable_read_count']])
