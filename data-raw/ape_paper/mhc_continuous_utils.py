"""Half-open interval operations for source chain/PAF and native coordinates."""
import re

def clip_block(t0,t1,q0,q1,strand,lo,hi):
    assert t1-t0==q1-q0 and strand in ('+','-')
    a,b=max(t0,lo),min(t1,hi)
    if a>=b:return None
    if strand=='+':c,d=q0+a-t0,q0+b-t0
    else:c,d=q1-(b-t0),q1-(a-t0)
    assert b-a==d-c
    return a,b,c,d

def display_interval(a,b,lo,hi,orientation):
    assert lo<=a<=b<=hi
    return (a-lo,b-lo) if orientation=='+' else (hi-b,hi-a)

def split_cigar(cigar,t0,q0,strand,split_gap=1000):
    """Split PAF alignments at large indels. q0 is oriented query start."""
    assert strand in ('+','-')
    operations=re.findall(r'(\d+)([MID=X])',cigar)
    assert ''.join(n+op for n,op in operations)==cigar
    ti,qi=t0,q0;start_t,start_q=ti,qi;out=[]
    for n,op in operations:
        n=int(n)
        if op in 'ID' and n>=split_gap:
            if ti>start_t and qi>start_q:out.append((start_t,ti,start_q,qi))
            if op=='I':qi+=n
            else:ti+=n
            start_t,start_q=ti,qi
        else:
            if op in 'MD=X':ti+=n
            if op in 'MI=X':qi+=n
    if ti>start_t and qi>start_q:out.append((start_t,ti,start_q,qi))
    return out
