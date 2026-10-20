#!/usr/bin/env python3
"""Render native-scale detail crops of the completed vector figure for eye QC.

These are crops of the full figure, not additional zoom panels or changed axes.
Requires Poppler's pdfinfo and pdftoppm; uses only Python's standard library.
"""
from pathlib import Path
import csv,json,math,re,subprocess

F=Path(__file__).resolve().parent/'results/mhc_continuous/figures'
PDF=F/'mhc_continuous.pdf'
rows=list(csv.DictReader((F/'mhc-lollipop-layout.tsv').open(),delimiter='\t'))
info=subprocess.check_output(['pdfinfo',str(PDF)],text=True)
match=re.search(r'Page size:\s+([\d.]+) x ([\d.]+) pts',info)
page_height_mm=float(match[2])*25.4/72
dpi=220
reports=[]
for tracks,name in [(['HSAh1'],'HSAh1_classII_detail'),
                    (['GGOh1'],'GGOh1_classII_detail'),
                    (['GGOh2','GGOh1'],'GGOh2_GGOh1_classII_comparison'),
                    (['GGOh2','GGOh1'],'GGOh2_GGOh1_full_MHC_comparison')]:
 track='+'.join(tracks)
 d=[r for r in rows if r['device']=='pdf' and r['track'] in tracks]
 selected=[r for r in d if r['label'].startswith(('DR','DQ','DM','DO','DP'))]
 left=float(d[0]['device_left_mm']);bottom=float(d[0]['device_bottom_mm'])
 x0=left+min(min(float(r['channel_x_mm']),float(r['anchor_x_mm'])) for r in selected)-1
 x1=left+max(max(float(r['right_mm']),float(r['anchor_x_mm'])) for r in selected)+1
 # Keep a 5 mm margin for complete native tick labels.
 y0=min(float(r['device_bottom_mm']) for r in d)-5
 y1=max(float(r['device_bottom_mm'])+float(r['panel_height_mm']) for r in d)
 # A shared crop boundary must not cut through a neighbouring head on either
 # haplotype when genomic expansions place columns at different positions.
 changed=True
 while changed:
  changed=False
  for r in d:
   l=left+float(r['left_mm']);right=left+float(r['right_mm'])
   if l<x0<right:
    x0=left+float(r['channel_x_mm'])-.5;changed=True
   if l<x1<right:
    x1=right+.5;changed=True
 if name.endswith('full_MHC_comparison'):x0=0;x1=float(match[1])*25.4/72
 px=math.floor(x0*dpi/25.4);py=math.floor((page_height_mm-y1)*dpi/25.4)
 width=math.ceil((x1-x0)*dpi/25.4);height=math.ceil((y1-y0)*dpi/25.4)
 output=F/f'mhc_{name}'
 subprocess.run(['pdftoppm','-f','1','-singlefile','-r',str(dpi),
   '-x',str(px),'-y',str(py),'-W',str(width),'-H',str(height),
   '-png',str(PDF),str(output)],check=True)
 reports.append(dict(track=track,source_pdf=PDF.name,dpi=dpi,
   crop_mm=dict(left=x0,right=x1,bottom=y0,top=y1),
   interpretation='Native-scale crop of the full figure; axes and genomic geometry unchanged.'))
(F/'classII-detail-crops.json').write_text(json.dumps(reports,indent=2)+'\n')
print('PASS: human, gorilla class II and full-width paired-haplotype MHC crops rendered.')
