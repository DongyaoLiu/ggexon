# Mappability and transcript assembly of long RNA-seq – Ape Genome Study

This repository contains scripts used in the **gene annotation section** of:

> **Complete sequencing of ape genomes**  
> bioRxiv [Preprint]. 2024 Oct 5:2024.07.31.605654  
> Originally published 2024 Jul 31. [Version 2]  
> [doi:10.1101/2024.07.31.605654](https://doi.org/10.1101/2024.07.31.605654)

## Study Summary

We present haplotype-resolved reference genomes and comparative analyses of six ape species: chimpanzee, bonobo, gorilla, Bornean orangutan, Sumatran orangutan, and siamang. With chromosome-level contiguity, we fully sequence 215 gapless chromosomes and resolve highly repetitive immune and centromeric regions. This enables unbiased annotation and evolutionary analysis of gene families, segmental duplications, and other complex loci.

### This repository focuses on the **impact of T2T assemblies on mappability and transcript assembly**, leveraging long-read IsoSeq data.

### Mapping statistics

We used PacBio Iso-Seq long reads from testis RNA samples of four great apes (chimpanzee, Sumatran orangutan, gorilla, and bonobo) to quantify the potential impact of T2T genome assemblies on read mapping and observed improvements in mappability, soft-clipping, and error rates. Iso-Seq reads were mapped with minimap2 v2.24-r112227 with parameters recommended for PacBio Iso-Seq cDNA (-ax splice:hq -uf) allowing up to 15 alignments per read (-N 15) to both T2T assemblies and previous assemblies. 
Mismatch and indel rates were computed based on primary alignments only using Perbase v0.9.0 (Stadick 2023 https://github.com/sstadick/perbase).
Transcripts were assembled from aligned reads using StringTie2 v2.2.128 with default parameters. We did not provide a reference annotation file to be able to attribute differences in transcript assembly to the quality of the reference sequence alone. 

### StringTie bundles analysis

StringTie2 and most other existing algorithms infer transcripts locus by locus. If not relying on a gene annotation, loci are identified by sets of reads, i.e., read bundles, that together span a (almost) contiguous genomic region. We ran StringTie2 with option -v and parsed the output to collect read bundles that allow for gaps, i.e., genomic regions that are not covered by any reads, of length at most 50 bp. The similarity of bundles formed by reads mapped to T2T and to previous assemblies was measured by the Jaccard index.

## Scripts for mapping statistics

### `calculate_stats.py`

Usage

The script requires four positional arguments:

    old_bam_file: Path to the first (old) BAM file.

    new_bam_file: Path to the second (new) BAM file.

    equivalence_file: CSV file containing chromosome equivalence mappings. Each line must include at least three comma-separated values (old name, new name, common name).

    output_file: Path where the output CSV file will be written.

Command-Line Example

```{python}
python script.py old.bam new.bam chromosome_equivalences.csv output.csv
```

Input File Formats
Chromosome Equivalence File

    Format: CSV

    Columns: The file must contain at least three columns per line:

        Column 1: Old chromosome name.

        Column 2: New chromosome name.

        Column 3: Common name to map both names.

    Note: Lines with fewer than three fields will be skipped.

BAM Files

    Standard BAM files used for alignment data.

    Ensure that the BAM files are valid and indexed if necessary.

Output

The script creates an output CSV file with the following columns:

    Chromosome: The common chromosome name.

    Old Soft Clipped: Count of significant soft-clipped reads in the old BAM file.

    New Soft Clipped: Count of significant soft-clipped reads in the new BAM file.

    Old Mismatch Rate: Mismatch rate for the old BAM file (mismatches / total bases).

    New Mismatch Rate: Mismatch rate for the new BAM file.

    Old Multimap Rate: Proportion of secondary/supplementary reads in the old BAM file.

    New Multimap Rate: Proportion of secondary/supplementary reads in the new BAM file.

    Unmapped_old: Number of reads that transitioned from unmapped in the old file to mapped in the new file.

    Unmapped_new: Number of reads that transitioned from unmapped in the new file to mapped in the old file.



## Scripts for bundle analysis

# Bundle Analysis Scripts

This repository includes several scripts to analyze and compare read bundles from StringTie logs and BAM files. Note that some file names in the code (e.g., `compare_diff_bundles.py` and `compare_self_bundles.py`) have been renamed here for clarity to match their purpose. The following sections describe each script, its inputs/outputs, and usage.

---

## 1. `calculate_stats.py`

**Purpose:**  
Compares two BAM files (old and new) using a chromosome equivalence file. It computes per-chromosome metrics (soft clipping, mismatch rate, and multimap rate) and tracks transitions in read mapping between the datasets.

**Inputs:**  
- **old_bam_file:** Path to the BAM file for the old dataset.  
- **new_bam_file:** Path to the BAM file for the new dataset.  
- **equivalence_file:** CSV file containing chromosome equivalences (each line must have at least three comma-separated values: old name, new name, common name).  

**Output:**  
- A CSV file containing columns:  
  `Chromosome, Old Soft Clipped, New Soft Clipped, Old Mismatch Rate, New Mismatch Rate, Old Multimap Rate, New Multimap Rate, Unmapped_old, Unmapped_new`

**Usage:**  
```{python}
python calculate_stats.py <old_bam_file> <new_bam_file> <equivalence_file> <output_file>
```
## 2. `compare_diff_bundles.py`


Purpose:
Compares read bundles between old and new datasets based on StringTie logs. It uses set-based comparisons (with Jaccard similarity) to identify similar bundles between the datasets and outputs the results to a CSV file.

Inputs:

    --log_old LOG_OLD: Path to the old StringTie log file.

    --log_new LOG_NEW: Path to the new StringTie log file.

    --bam_old BAM_OLD: Path to the old BAM file.

    --bam_new BAM_NEW: Path to the new BAM file.

    --output_csv OUTPUT_CSV: Path for the output CSV file.

Output:

    A CSV file with columns:
    Bundle1, Bundle2, Size1, Size2, Jaccard
    Each line represents a comparison between a bundle from the new log and a matching bundle from the old log where the Jaccard similarity meets or exceeds the threshold.

**Usage**

```{python}
python compare_diff_bundles.py --log_old <LOG_OLD> --log_new <LOG_NEW> --bam_old <BAM_OLD> --bam_new <BAM_NEW> --output_csv <OUTPUT_CSV>
```
## 3. `compare_self_bundles.py`

Purpose:
Compares read bundles within the same assembly (using a new StringTie log and corresponding BAM file) to identify similar bundles. The script calculates the Jaccard similarity between each pair of bundles and outputs comparisons that meet a minimum similarity threshold.

Inputs:

    --log_new LOG_NEW: Path to the new StringTie log file.

    --bam_new BAM_NEW: Path to the new BAM file.

    --output_csv OUTPUT_CSV: Path for the output CSV file.

Output:

    A CSV file with columns:
    Bundle, Matched Bundle, Size of Original Bundle, Size of Matched Bundle, Overlapping Reads, Jaccard
    Each row represents a pairwise comparison between bundles that exceeds the Jaccard threshold.

Usage:

```{python}
python compare_self_bundles.py--log_new <LOG_NEW> --bam_new <BAM_NEW> --output_csv <OUTPUT_CSV>
```

## 4. `count_total_bundles.py`

Purpose:
Counts the number of reads per bundle from a StringTie log and a corresponding BAM file. This script aggregates read counts for each bundle defined in the log file.

Inputs:

    --log_new LOG_NEW: Path to the StringTie log file.

    --bam_new BAM_NEW: Path to the BAM file.

    --output_csv OUTPUT_CSV: Path for the output CSV file.

Output:

    A CSV file summarizing the total read count for each bundle. (Columns may include a bundle identifier and the corresponding read count.)

Usage:

```{python}
python count_total_bundles.py --log_new <LOG_NEW> --bam_new <BAM_NEW> --output_csv <OUTPUT_CSV>
```
