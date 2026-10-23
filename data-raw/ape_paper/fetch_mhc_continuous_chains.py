#!/usr/bin/env python3
"""Bounded CHM13chr6[28M,34M) chain and ungapped block source extracts."""
from pathlib import Path
import requests,pyBigWig,re,csv,json,concurrent.futures,hashlib
root=Path(__file__).resolve().parent/'source_metadata/mhc_continuous'
root.mkdir(parents=True,exist_ok=True)
trackdb=root/'human.com.trackDb.txt'
if not trackdb.exists():
 r=requests.get('https://research.nhgri.nih.gov/CustomTracks/T2T_hubs/T2Tgenomes/T2T-CHM13v2.0/com.trackDb.txt',timeout=45);r.raise_for_status();trackdb.write_text(r.text)
text=(root/'human.com.trackDb.txt').read_text()
config=[]
for block in re.split(r'\n\s*track ', '\n'+text):
 d={}
 for line in block.splitlines():
  f=line.strip().split(None,1)
  if len(f)==2:d[f[0]]=f[1]
 if 'linkDataUrl' in d and any(k in d.get('bigDataUrl','') for k in ['mPanTro3hap','mPanPan1','mGorGor1','mPonAbe1','mPonPyg2','mSymSyn1']):config.append(d)
print('tracks',len(config),flush=True)
(root/'chain_sources.json').write_text(json.dumps(config,indent=2))
def fetch(d):
 name=d['shortLabel'].replace('#','_');count=[]
 for field in ['bigDataUrl','linkDataUrl']:
  p=root/(name+('.chains.bed' if field=='bigDataUrl' else '.links.bed'))
  if not p.exists():
   with pyBigWig.open(d[field]) as b:
    rows=b.entries('chr6',28000000,34000000) or []
    p.write_text(''.join(f'chr6\t{s}\t{e}\t{v}\n' for s,e,v in rows))
    p.with_suffix(p.suffix+'.as').write_bytes(b.SQL() or b'')
  count.append(sum(1 for _ in p.open()))
 return name,count
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as ex:
 for r in ex.map(fetch,config):print(r,flush=True)
