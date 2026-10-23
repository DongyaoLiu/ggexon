#!/usr/bin/env python3
"""Range-extract auditable locus data. Requires requests and pyBigWig.
BED/BigWig inputs are 0-based half-open; TSV/GFF outputs are 1-based closed.
"""
from pathlib import Path
import csv, json, hashlib, struct, time, subprocess, re
import requests
import pyBigWig

ROOT = Path(__file__).resolve().parent
DATA = ROOT / 'results'
META = ROOT / 'source_metadata'
DATA.mkdir(exist_ok=True)
SESSION = requests.Session()
MANIFEST = []

def write_tsv(path, rows, fields):
    path.parent.mkdir(parents=True,exist_ok=True)
    with path.open('w') as h:
        w=csv.DictWriter(h,fieldnames=fields,delimiter='\t',extrasaction='ignore');w.writeheader();w.writerows(rows)

def record(url, path, **kwargs):
    MANIFEST.append(dict(url=url,file=str(path.relative_to(ROOT)),sha256=hashlib.sha256(path.read_bytes()).hexdigest(),retrieved_utc=time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime()),**kwargs))

def get_range(url, start, size):
    for attempt in range(3):
        try:
            r=SESSION.get(url,headers={'Range':f'bytes={start}-{start+size-1}'},timeout=45)
            r.raise_for_status()
            if r.status_code != 206 or len(r.content) != size:
                raise ValueError(f'Expected exact HTTP range {size}, got {r.status_code}/{len(r.content)}')
            return r.content
        except Exception:
            if attempt==2: raise
            time.sleep(1)

class RemoteTwoBit:
    def __init__(self,url):
        self.url=url
        h=get_range(url,0,16)
        self.endian='>' if struct.unpack('>I',h[:4])[0]==0x1A412743 else '<'
        sig,ver,n,res=struct.unpack(self.endian+'4I',h)
        assert sig==0x1A412743 and ver in (0,1)
        offset_size=8 if ver==1 else 4
        offset_type="Q" if ver==1 else "I"
        # Read index exactly; the upper bound is small for these chromosome assemblies.
        data=get_range(url,16,n*300)
        pos=0;self.offsets={}
        for i in range(n):
            ln=data[pos];pos+=1;name=data[pos:pos+ln].decode();pos+=ln
            self.offsets[name]=struct.unpack(self.endian+offset_type,data[pos:pos+offset_size])[0];pos+=offset_size
    def sequence(self,chrom,start,end):
        off=self.offsets[chrom]
        u=lambda b:struct.unpack(self.endian+'I',b)[0]
        nsize,ncount=struct.unpack(self.endian+'2I',get_range(self.url,off,8))
        assert 0<=start<end<=nsize
        nsdata=get_range(self.url,off+8,ncount*8+4)
        ns=struct.unpack(self.endian+str(ncount)+'I',nsdata[:ncount*4]) if ncount else []
        sizes=struct.unpack(self.endian+str(ncount)+'I',nsdata[ncount*4:ncount*8]) if ncount else []
        mask_count=u(nsdata[-4:])
        packed=off+8+ncount*8+4+mask_count*8+4
        b0=start//4;b1=(end+3)//4
        raw=get_range(self.url,packed+b0,b1-b0)
        lut=[''.join('TCAG'[(b>>s)&3] for s in (6,4,2,0)) for b in range(256)]
        seq=''.join(lut[b] for b in raw)[start%4:start%4+end-start]
        for s,n in zip(ns,sizes):
            a=max(s,start)-start;z=min(s+n,end)-start
            if a<z:seq=seq[:a]+'N'*(z-a)+seq[z:]
        assert len(seq)==end-start
        return seq

def bb_entries(url,chrom,start,end,path):
    if path.exists():
        rows=[]
        for line in path.read_text().splitlines():
            f=line.split('\t',3);rows.append((int(f[1]),int(f[2]),f[3]))
        record(url,path,chrom=chrom,start0=start,end0=end,coordinate_system='0-based half-open')
        return rows
    with pyBigWig.open(url) as b:
        if chrom not in b.chroms(): raise ValueError(f'{chrom} not in {url}; examples {list(b.chroms())[:4]}')
        rows=b.entries(chrom,start,end) or []
        schema=b.SQL()
    path.parent.mkdir(parents=True,exist_ok=True)
    path.write_text(''.join(f'{chrom}\t{a}\t{z}\t{s}\n' for a,z,s in rows))
    path.with_suffix(path.suffix+'.as').write_bytes(schema or b'')
    record(url,path,chrom=chrom,start0=start,end0=end,coordinate_system='0-based half-open')
    return rows

def genes_from_bed(rows,chrom,track,preferred=None):
    models=[]
    for a,z,rest in rows:
        f=[chrom,str(a),str(z)]+rest.split('\t')
        if len(f)<12: continue
        symbol=f[12] if len(f)>12 else f[3]
        if preferred and not any(k.upper() in symbol.upper() or k.upper() in f[3].upper() for k in preferred):continue
        sizes=list(map(int,f[10].rstrip(',').split(','))); offsets=list(map(int,f[11].rstrip(',').split(',')))
        models.append(dict(start=a,end=z,transcript=f[3],symbol=symbol,strand=f[5],exons=[(a+s,a+s+n) for n,s in zip(sizes,offsets)],cds_start=int(f[6]),cds_end=int(f[7])))
    # Representative transcript per symbol and genomic locus: keep longest exonic model.
    kept=[]
    for m in sorted(models,key=lambda m:sum(z-a for a,z in m['exons']),reverse=True):
        if not any(m['symbol']==k['symbol'] and m['start']<k['end'] and k['start']<m['end'] for k in kept):kept.append(m)
    return sorted(kept,key=lambda m:m['start'])

def gff_write(path,models,chrom):
    lines=['##gff-version 3']
    for i,m in enumerate(models):
        tid=f'transcript_{i+1}';gid=f'gene_{i+1}'
        attrs=f'ID={tid};Parent={gid};gene_id={gid};transcript_id={tid};gene_name={m["symbol"]};transcript_name={m["symbol"]}'
        lines.append(f'{chrom}\tsource\tgene\t{m["start"]+1}\t{m["end"]}\t.\t{m["strand"]}\t.\tID={gid};Name={m["symbol"]};gene_id={gid};gene_name={m["symbol"]}')
        lines.append(f'{chrom}\tsource\tmRNA\t{m["start"]+1}\t{m["end"]}\t.\t{m["strand"]}\t.\t{attrs}')
        for j,(a,z) in enumerate(m['exons']):
            lines.append(f'{chrom}\tsource\texon\t{a+1}\t{z}\t.\t{m["strand"]}\t.\tID={tid}_exon_{j+1};Parent={tid};gene_id={gid};transcript_id={tid};gene_name={m["symbol"]};transcript_name={m["symbol"]}')
    path.write_text('\n'.join(lines)+'\n')

def signal_extract(url,chrom,start,end,out,bin_bp=250,scale=1):
    """Mean observed CpG calls per bin. Missing bins stay absent, never zero-filled."""
    with pyBigWig.open(url) as b:
        vals=b.intervals(chrom,start,end) or []
    write_tsv(out.with_suffix('.raw.tsv'),[dict(start0=a,end0=z,value=v) for a,z,v in vals],['start0','end0','value'])
    rows=[]
    for a in range(start,end,bin_bp):
        z=min(a+bin_bp,end);obs=[(min(y,z)-max(x,a),v) for x,y,v in vals if x<z and y>a]
        den=sum(n for n,v in obs)
        if den:rows.append(dict(xmin=a+1,xmax=z,coverage=scale*sum(n*v for n,v in obs)/den,observed_bases=den))
    write_tsv(out,rows,['xmin','xmax','coverage','observed_bases'])
    record(url,out,chrom=chrom,start0=start,end0=end,coordinate_system='1-based closed',bin_bp=bin_bp,operation='observed-base weighted mean; missing omitted',scale=scale)
    return rows

def run_alignment(folder,windows,minlen=150,identity=70):
    links=[]
    for t,q in zip(windows,windows[1:]):
        paf=folder/f'{t["id"]}__{q["id"]}.paf'
        cmd=['lastz',str(folder/(t['id']+'.fa')),str(folder/(q['id']+'.fa')),'--format=paf:minimap2','--chain','--identity='+str(identity)]
        with paf.open('w') as h:subprocess.run(cmd,stdout=h,check=True)
        (paf.with_suffix('.command.json')).write_text(json.dumps(cmd,indent=2))
        for line in paf.read_text().splitlines():
            f=line.split('\t'); qs,qe,ts,te=map(int,[f[2],f[3],f[7],f[8]])
            if int(f[10])<minlen:continue
            links.append(dict(tspecies=t['id'],tchr=t['chrom'],tstart=t['start0']+ts+1,tend=t['start0']+te,qspecies=q['id'],qchr=q['chrom'],qstart=q['start0']+qs+1,qend=q['start0']+qe,strand=f[4],identity=100*int(f[9])/int(f[10]),alen=int(f[10]),nmatch=int(f[9])))
    assert links,'No alignment links'
    write_tsv(folder/'links.tsv',links,['tspecies','tchr','tstart','tend','qspecies','qchr','qstart','qend','strand','identity','alen','nmatch'])
    return links

def finalize(name="sources"):
    manifest_path = ROOT / ("manifest-"+name+".json")
    old=json.loads(manifest_path.read_text()) if manifest_path.exists() else []
    merged={m['file']:m for m in old+MANIFEST}
    manifest_path.write_text(json.dumps(list(merged.values()),indent=2)+'\n')
