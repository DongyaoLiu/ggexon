#!/usr/bin/env python3
"""Rescue context using alternate same-species models, keeping strict thresholds.

Native Augustus/RNA evidence is reported separately. Unsupported or partial RNA
loci never become complete context models. The original merged release/projection
files are not modified; a new output directory adds only accepted recoveries.
"""
from pathlib import Path
import collections,csv,json,hashlib
import parasail,pysam
from project_mhc_context import parse_liftoff,native_span
R=Path(__file__).resolve().parent;B=R/'results/mhc_continuous';P=B/'gene_models_with_projected_context';O=B/'gene_models_with_recovered_context';D=B/'context_recovery'

def rd(p):return list(csv.DictReader(p.open(),delimiter='\t'))
def wr(p,rows,fields):
    with p.open('w') as h:
        w=csv.DictWriter(h,fieldnames=fields,delimiter='\t',lineterminator='\n',extrasaction='ignore');w.writeheader();w.writerows(rows)
def rev(s):return s.translate(str.maketrans('ACGTN','TGCAN'))[::-1]
def qualifies_rna_locus(row):
    return bool(row['source_usable'] and row['primary'] and row['mapq']>=20 and row['strand']==row['gene_strand'] and row['identity']>=.95 and row['query_coverage']>=.9 and row['exonic_overlap']>=100)

def choose_recoveries(rows):
    groups=collections.defaultdict(list)
    for r in rows:
        if min(r['coverage'],r['identity'],r['global_identity'])<.95:continue
        if r['strand']!=r['source_strand']:continue
        if r['native_start0']<r['expected_start0']-5000 or r['native_end0']>r['expected_end0']+5000:continue
        groups[r['target'],r['gene_symbol']].append(r)
    return [max(v,key=lambda r:(r['identity'],r['global_identity'],r['coverage'],r['exonic_bp'])) for k,v in sorted(groups.items())]

def main():
    O.mkdir(exist_ok=True);windows={r['track']:r for r in rd(B/'windows.tsv')};models=rd(P/'models.tsv');exons=rd(P/'exons.tsv');byid={r['model_id']:r for r in models};exbyid=collections.defaultdict(list)
    for e in exons:exbyid[e['model_id']].append(e)
    seq={t:''.join((B/(t+'.fa')).read_text().splitlines()[1:]).upper() for t in windows}
    def spliced(m,es):
        w=windows[m['track']];blocks=[]
        for e in es:
            a,b=int(e['start0']),int(e['end0']);x,y=(a-int(w['start0']),b-int(w['start0'])) if w['orientation']=='+' else (int(w['end0'])-b,int(w['end0'])-a)
            assert 0<=x<y<=len(seq[m['track']]);blocks.append((x,y))
        s=''.join(seq[m['track']][a:b] for a,b in sorted(blocks));st=m['strand'] if w['orientation']=='+' else ('-' if m['strand']=='+' else '+')
        return s if st=='+' else rev(s)
    missing=[r for r in rd(P/'projection-audit.tsv') if r['status']!='accepted']
    native=json.loads((R/'source_metadata/mhc_unresolved_recovery/native_predictions/native_candidate_evidence.json').read_text());expected={(r['track'],r['gene_symbol']):r for r in native}
    candidates=[];parsed={}
    for r in json.loads((D/'alternative_isoforms/results.json').read_text()):
        folder=Path(r['folder']);src=byid[r['alternative_source_model_id']]
        mm,ee,aa=parse_liftoff((folder/'projection.gff3').read_text(),windows[r['target']],{'alternate':src})
        if not mm:continue
        m=mm[0];source_dna=spliced(src,exbyid[src['model_id']]);target_dna=spliced(m,ee)
        aln=parasail.nw_stats_striped_32(source_dna,target_dna,5,1,parasail.matrix_create('ACGTN',2,-3));assert not aln.saturated
        exp=expected[r['target'],r['gene_symbol']]
        c=dict(target=r['target'],gene_symbol=r['gene_symbol'],source_model_id=src['model_id'],original_source_model_id=r['original_source_model_id'],coverage=float(m['projection_coverage']),identity=float(m['projection_identity']),global_identity=aln.matches/aln.length,native_start0=m['chromStart'],native_end0=m['chromEnd'],strand=m['strand'],source_strand=src['strand'],expected_start0=exp['syntenic_search_start0'],expected_end0=exp['syntenic_search_end0'],exonic_bp=len(target_dna),source_exonic_bp=len(source_dna),raw_gff=str(folder/'projection.gff3'),raw_gff_sha256=hashlib.sha256((folder/'projection.gff3').read_bytes()).hexdigest())
        candidates.append(c);parsed[src['model_id']]=m,ee
    selected=choose_recoveries(candidates);new_models=[];new_exons=[];selected_keys={(r['target'],r['gene_symbol']) for r in selected}
    for c in selected:
        m,ee=parsed[c['source_model_id']];mid=f'{c["target"]}:recovered:{c["gene_symbol"]}'
        extra=dict(model_id=mid,recovery_method='alternative same-species transcript; strict95',recovery_original_source_model_id=c['original_source_model_id'],recovery_raw_gff=c['raw_gff'],recovery_global_identity=c['global_identity'])
        m.update(extra);new_models.append(m)
        for e in ee:e.update(extra);new_exons.append(e)
    extra_fields=['recovery_method','recovery_original_source_model_id','recovery_raw_gff','recovery_global_identity']
    for r in models+exons:
        for field in extra_fields:r[field]=''
    wr(O/'models.tsv',models+new_models,list(models[0]));wr(O/'exons.tsv',exons+new_exons,list(exons[0]))
    wr(O/'recovered-models.tsv',selected,list(selected[0]));wr(O/'candidate-assessments.tsv',candidates,list(candidates[0]))
    # Diagnostic target-RNA alignment support, including negative/antisense cases.
    # At least100bp exonic overlap, correct annotation strand, MAPQ20,
    # gap-inclusive identity95 and aligned query90 are needed for even partial
    # support. This does not validate complete transcript boundaries.
    rna_records=[];case_records=[]
    source_rna=rd(R/'source_metadata/mhc_unresolved_recovery/rna_evidence/source_rna_read_qc.tsv')
    source_usable={(r['track'],r['gene_symbol'],r['read_name']) for r in source_rna if r['source_locus_usable']=='True'}
    native_summary={(r['track'],r['gene_symbol']):r for r in rd(R/'source_metadata/mhc_unresolved_recovery/native_predictions/native_recovery_summary.tsv')}
    xeno=rd(R/'source_metadata/mhc_context_search/ncbi/genark_xenoRefGene_mhc_accession_metadata.tsv')
    for case in missing:
        target,gene=case['track'],case['gene_symbol'];exp=expected[target,gene];w=windows[target];chosen=next((x for x in selected if (x['target'],x['gene_symbol'])==(target,gene)),None)
        if chosen:
            model=next(x for x in new_models if x['model_id']==f'{target}:recovered:{gene}');target_ex=[(int(e['start0']),int(e['end0'])) for e in new_exons if e['model_id']==model['model_id']]
        else:
            # Read rejected diagnostic GFF only as an RNA comparison candidate.
            # Do not mistake split CIGAR alignment blocks for biological exons.
            source_map=json.loads((P/target/'source_models.json').read_text())
            gid=next(k for k,v in source_map.items() if v['model_id']==case['source_model_id'])
            transcripts=set();target_ex=[]
            for line in (D/'diagnostics'/target/'all_mappings.gff3').read_text().splitlines():
                if not line or line.startswith('#'):continue
                f=line.split('\t');attr=dict(x.split('=',1) for x in f[8].split(';') if '=' in x)
                if f[2] in ('mRNA','transcript') and attr.get('Parent')==gid:transcripts.add(attr['ID'])
                if f[2]=='exon' and attr.get('Parent') in transcripts:
                    a,b,_=native_span(int(f[3]),int(f[4]),f[6],w);target_ex.append((a,b))
            target_ex=sorted(set(target_ex))
        good=[]
        with pysam.AlignmentFile(str(D/'rna_realignments'/f'{target}.sam'),'r') as sam:
            for read in sam:
                if read.is_unmapped:continue
                st='-' if read.is_reverse else '+';blocks=[]
                for a,b in read.get_blocks():
                    x,y,ns=native_span(a+1,b,st,w);blocks.append((x,y))
                native_st=ns;ov=sum(max(0,min(b,y)-max(a,x)) for a,b in target_ex for x,y in blocks)
                if ov==0:continue
                count=collections.Counter()
                for op,n in read.cigartuples:count[op]+=n
                ident=count[7]/sum(count[o] for o in [0,1,2,7,8]);qcov=sum(count[o] for o in [0,1,7,8])/read.query_length
                total=sum(b-a for a,b in target_ex)
                source_ok=(target,gene,read.query_name) in source_usable
                support=qualifies_rna_locus(dict(source_usable=source_ok,primary=not(read.is_secondary or read.is_supplementary),mapq=read.mapping_quality,strand=native_st,gene_strand=exp['source_strand'],identity=ident,query_coverage=qcov,exonic_overlap=ov))
                # Only CIGAR N operations are introns; retain all ordinary indels.
                junctions=[];pos=read.reference_start
                for op,n in read.cigartuples:
                    if op==3:
                        x,y,_=native_span(pos+1,pos+n,st,w);junctions.append((x,y))
                    if op in [0,2,3,7,8]:pos+=n
                expected_junctions=[(a[1],b[0]) for a,b in zip(sorted(target_ex),sorted(target_ex)[1:])]
                junction_matches=sum(any(abs(a-x)<=5 and abs(b-y)<=5 for x,y in expected_junctions) for a,b in junctions)
                rec=dict(track=target,gene_symbol=gene,read_name=read.query_name,flag=read.flag,mapq=read.mapping_quality,identity=ident,query_coverage=qcov,native_strand=native_st,gene_strand=exp['source_strand'],exonic_overlap_bp=ov,model_exonic_fraction=ov/total if total else 0,cigar=read.cigarstring,source_gene_assignment_usable=source_ok,junctions_matching_target_model=junction_matches,supports_partial_locus=support,qualification='same-species source-primary-selected RNA realigned to alternate MHC; haplotype origin unresolved')
                rna_records.append(rec)
                if support:good.append(rec)
        case_records.append(dict(track=target,gene_symbol=gene,status='recovered_strict_alternative_model' if chosen else 'unresolved_complete_model',source_model_id=case['source_model_id'],native_prediction_assessment=native_summary[target,gene]['decision'],cross_species_refseq_accessions=';'.join(r['refseq_accession'] for r in xeno if r['track']==target and r['source_gene_symbols'].upper()==gene.upper()),recovered_source_model_id=chosen['source_model_id'] if chosen else '',native_start0=chosen['native_start0'] if chosen else '',native_end0=chosen['native_end0'] if chosen else '',coverage=chosen['coverage'] if chosen else '',sequence_identity=chosen['identity'] if chosen else '',global_spliced_identity=chosen['global_identity'] if chosen else '',same_species_rna_partial_locus_reads=len({r['read_name'] for r in good}),rna_reads_matching_target_junctions=len({r['read_name'] for r in good if r['junctions_matching_target_model']>0}),best_rna_model_exonic_fraction=max((r['model_exonic_fraction'] for r in good),default=0),rna_interpretation='partial locus support only; transcript boundaries not established' if good else 'no qualifying RNA in bounded queried source-selected reads; not evidence of absence'))
    wr(O/'recovery-audit.tsv',case_records,list(case_records[0]));wr(O/'rna-realignment-qc.tsv',rna_records,list(rna_records[0]))
    manifest=dict(method='Only alternate same-species context models meeting original95coverage/identity plus independent global-spliced95 and syntenic/strand checks are added',original_models_sha256=hashlib.sha256((P/'models.tsv').read_bytes()).hexdigest(),original_exons_sha256=hashlib.sha256((P/'exons.tsv').read_bytes()).hexdigest(),recovered=len(selected),remaining_unresolved=len(missing)-len(selected),all_14_cases_audited=len(case_records),models_added=selected,limitations=['Native Augustus predictions are not accepted when absent, antisense, or fused.','RNA is same-species experimental evidence selected on source-primary coordinates then realigned; it is not haplotype-origin-resolved.','Partial RNA or cross-species RefSeq evidence alone never establishes a complete model.','The original strict956 projections and all released records remain unchanged.','RNA sampling is bounded; PPA testis was not queried due coarse index range budget.'])
    inputs=[B/'windows.tsv',P/'models.tsv',P/'exons.tsv',D/'alternative_isoforms/results.json',R/'source_metadata/mhc_unresolved_recovery/rna_evidence/source_rna_read_qc.tsv',R/'source_metadata/mhc_unresolved_recovery/native_predictions/native_candidate_evidence.json']+list((D/'rna_realignments').glob('*.sam'))+list((D/'rna_realignments').glob('*.fastq'))
    manifest['input_hashes']={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs}
    (O/'recovery_manifest.json').write_text(json.dumps(manifest,indent=2)+'\n');print(json.dumps(case_records,indent=2))
if __name__=='__main__':main()
