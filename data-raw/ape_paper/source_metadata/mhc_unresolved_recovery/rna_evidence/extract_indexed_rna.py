#!/usr/bin/env python3
"""Fetch BAI and bounded BAM byte ranges, retain primary-source locus IsoSeq reads.
No full BAM download. Original source coordinates are BED0. Sequences are experimental
same-species RNA aligned to the source primary haplotype, NOT target annotation.
"""
import csv,json,pathlib,urllib.request,gzip,struct,concurrent.futures,collections,io
import pysam
HERE=pathlib.Path(__file__).resolve().parent
ROOT=HERE.parents[3]
# HERE = ape_paper/source_metadata/mhc_unresolved_recovery/rna_evidence
APE=HERE.parents[2]
def rows(p):return list(csv.DictReader(open(p),delimiter='\t'))
audit=rows(APE/'results/mhc_continuous/gene_models_with_projected_context/projection-audit.tsv')
reps={r['model_id']:r for r in rows(APE/'results/mhc_continuous/figures/before-alt-context/representative-models.tsv')}
missing=[r for r in audit if r['status']!='accepted']
source=[]
for r in missing:
 s=reps[r['source_model_id']]
 source.append(dict(r,source_chrom=s['annotation_chr'],source_start0=int(s['chromStart']),source_end0=int(s['chromEnd']),source_strand=s['strand']))
(HERE/'primary_rna_query_intervals.json').write_text(json.dumps(source,indent=2))
headers=json.load(open(HERE/'native_rna_bam_header_audit.json'))
TARGET_CHROMS={r['track']:r['genbank_chrom'] for r in json.load(open(APE/'source_metadata/mhc_context_search/ncbi/genark_native_annotation_probe.json'))}
SP={'mGorGor1':'GGO','mPanPan1':'PPA','mPonAbe1':'PAB','mPonPyg2':'PPY','mSymSyn1':'SSY'}
def fetch(url,start=None,end=None):
 req=urllib.request.Request(url,headers={'Range':f'bytes={start}-{end}'} if start is not None else {})
 with urllib.request.urlopen(req,timeout=35) as f:
  if start is not None and f.status!=206:raise ValueError('HTTP range unavailable')
  return f.read()
def blocks(data,base=0):
 p=0
 while p+18<=len(data):
  n=struct.unpack_from('<H',data,p+16)[0]+1
  if p+n>len(data):break
  yield base+p,gzip.decompress(data[p:p+n]);p+=n

def bam_header(raw):
 n=struct.unpack_from('<i',raw,4)[0];p=8+n;nref=struct.unpack_from('<i',raw,p)[0];p+=4;names=[]
 for _ in range(nref):
  n=struct.unpack_from('<i',raw,p)[0];p+=4;names.append(raw[p:p+n-1].decode());p+=n+4
 return raw[:p],names

def bai_ref(data,rid):
 p=8
 for k in range(struct.unpack_from('<i',data,4)[0]):
  nbin=struct.unpack_from('<i',data,p)[0];p+=4;bins={}
  for _ in range(nbin):
   b,n=struct.unpack_from('<Ii',data,p);p+=8
   vals=[struct.unpack_from('<QQ',data,p+16*i) for i in range(n)];p+=16*n
   if k==rid:bins[b]=vals
  ni=struct.unpack_from('<i',data,p)[0];p+=4
  idx=struct.unpack_from('<'+'Q'*ni,data,p);p+=8*ni
  if k==rid:return bins,idx
 raise ValueError('Reference missing from index')

def regbins(a,b):
 b-=1;out=[0]
 for off,shift in [(1,26),(9,23),(73,20),(585,17),(4681,14)]:out.extend(range(off+(a>>shift),off+(b>>shift)+1))
 return out

def extract(task):
 url=task['bam_url'];name=url.split('/')[-1];species=next(v for k,v in SP.items() if name.startswith(k))
 loc=[s for s in source if s['track'].startswith(species)]
 try:
  raw=b''.join(x[1] for x in blocks(fetch(url,0,65535)))
  head,names=bam_header(raw)
  chrom=loc[0]['source_chrom'];rid=names.index(chrom)
  idxpath=HERE/(name+'.bai')
  if not idxpath.exists():idxpath.write_bytes(fetch(url+'.bai'))
  index=idxpath.read_bytes();bins,linear=bai_ref(index,rid)
  chunks=[]
  for s in loc:
   a,b=s['source_start0'],s['source_end0'];minoff=linear[a>>14] if a>>14<len(linear) else 0
   chunks.extend((x,y) for bn in regbins(a,b) for x,y in bins.get(bn,[]) if y>minoff)
  chunks=sorted(set(chunks));merged=[]
  for a,b in chunks:
   if merged and (a>>16)<=(merged[-1][1]>>16):merged[-1]=(merged[-1][0],max(b,merged[-1][1]))
   else:merged.append((a,b))
  planned=sum((b>>16)-(a>>16)+65536 for a,b in merged)
  if planned>10_000_000 or len(merged)>100:
   return {'bam_url':url,'species':species,'source_chrom':chrom,'index_bytes':len(index),'planned_bam_bytes':planned,'planned_range_count':len(merged),'status':'skipped_indexed_range_budget','reason':'Source loci require more than 10 MB or 100 indexed byte ranges; no RNA absence conclusion'}
  downloaded=0;records=[];rangeinfo=[]
  for a,b in merged:
   start,end=a>>16,(b>>16)+65535
   if end-start>10_000_000:raise ValueError('Chunk above 10 MB safety bound')
   data=fetch(url,start,end);downloaded+=len(data)
   rec=b''
   for pos,dec in blocks(data,start):
    if pos>b>>16:break
    lo=a&65535 if pos==a>>16 else 0
    hi=b&65535 if pos==b>>16 else len(dec)
    rec+=dec[lo:hi]
   records.append(rec);rangeinfo.append({'byte_start':start,'byte_end':end,'virtual_start':a,'virtual_end':b,'fetched_bytes':len(data)})
  tmp=HERE/(name+'.regional.tmp.bam')
  with pysam.BGZFile(str(tmp),'wb') as f:
   f.write(head)
   for rec in records:f.write(rec)
  evidence=[];fastqs={};samout=HERE/(name+'.source_loci.sam')
  with pysam.AlignmentFile(str(tmp),'rb') as f, pysam.AlignmentFile(str(samout),'w',template=f) as sf:
   for read in f.fetch(until_eof=True):
    if read.reference_id!=rid or read.is_unmapped:continue
    hits=[s for s in loc if read.reference_start<s['source_end0'] and read.reference_end>s['source_start0']]
    if not hits:continue
    sf.write(read)
    blocks0=read.get_blocks();junctions=[];pos=read.reference_start
    for op,n in read.cigartuples or []:
     if op==3:junctions.append([pos,pos+n])
     if op in [0,2,3,7,8]:pos+=n
    for s in hits:
     exonoverlap=sum(max(0,min(e,s['source_end0'])-max(a,s['source_start0'])) for a,e in blocks0)
     evidence.append({'track':s['track'],'gene_symbol':s['gene_symbol'],'source_track':s['source_track'],'bam_url':url,'source_chrom':chrom,'source_start0':s['source_start0'],'source_end0':s['source_end0'],'read_name':read.query_name,'flag':read.flag,'primary':not(read.is_secondary or read.is_supplementary),'mapq':read.mapping_quality,'read_start0':read.reference_start,'read_end0':read.reference_end,'strand':'-' if read.is_reverse else '+','cigar':read.cigarstring,'query_length':read.query_length,'aligned_overlap_bp':exonoverlap,'blocks_bed0':blocks0,'junctions_bed0':junctions})
    # Keep source-read orientation as FASTQ sequencing orientation; do not imply target haplotype.
    if not(read.is_secondary or read.is_supplementary) and read.mapping_quality>=20 and read.query_sequence:
     seq=read.get_forward_sequence();qs=read.get_forward_qualities();qual=''.join(chr(x+33) for x in qs) if qs is not None else 'I'*len(seq)
     fastqs[read.query_name]=f'@{read.query_name}\n{seq}\n+\n{qual}\n'
  tmp.unlink()
  (HERE/(name+'.source_loci.fastq')).write_text(''.join(fastqs.values()))
  (HERE/(name+'.source_loci.evidence.json')).write_text(json.dumps(evidence,indent=2))
  return {'bam_url':url,'species':species,'source_chrom':chrom,'target_chrom_present':any(TARGET_CHROMS[s['track']] in names for s in loc),'index_bytes':len(index),'bam_bytes_fetched':downloaded+65536,'range_requests':rangeinfo,'read_gene_overlaps':len(evidence),'unique_primary_mapq20_reads':len(fastqs),'per_gene':{s['gene_symbol']:sum(1 for e in evidence if e['gene_symbol']==s['gene_symbol'] and e['primary'] and e['mapq']>=20) for s in loc}}
 except Exception as e:return {'bam_url':url,'species':species,'error':str(e)}
if __name__=='__main__':
 with concurrent.futures.ThreadPoolExecutor(5) as pool:
  result=[]
  for r in pool.map(extract,headers):result.append(r);print(json.dumps(r),flush=True);(HERE/'primary_rna_extraction_manifest.json').write_text(json.dumps(result,indent=2))
 (HERE/'primary_rna_extraction_manifest.json').write_text(json.dumps(result,indent=2))
