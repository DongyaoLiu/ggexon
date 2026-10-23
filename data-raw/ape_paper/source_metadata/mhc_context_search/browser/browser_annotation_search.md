# Additional browser annotation search

Checked 2026-10-20 against live official NHGRI stable/development hubs, GenomeArk BigBeds, UCSC CGL CAT release directories and marbl/Primates documentation.

## Finding

None of the 48 examined native-coordinate gene-model BigBeds contains a chromosome alias for any of the six missing MHC haplotypes. All 48 corresponding primary-haplotype checks are positive. This is absence of annotation coverage in these files, not evidence of biological gene absence and not a catRefGene query filter or chromosome-name mistake.

The complete chromosome dictionaries were read independently from HTTP Range responses (first 65,536 bytes) and parsed from the BigBed B+ tree. Every dictionary item count matched its header. This avoids relying on user-facing browser display or file-name assumptions. See all_browser_bigbed_dictionaries.json and browser_coverage.tsv.

## Missing native chromosomes

| Track | Browser chromosome | GenBank accession | BED0 window |
|---|---|---|---|
| PTRh2 | chr5_hap2_hsa6 | CM054416.2 | 33350131-38784666 |
| PPAh1 | chr5_mat_hsa6 | CM054489.2 | 39408542-44487238 |
| GGOh2 | chr5_pat_hsa6 | CM054563.2 | 38874988-44233933 |
| PABh2 | chr5_hap2_hsa6 | CM054661.2 | 28697494-34321984 |
| PPYh2 | chr5_hap2_hsa6 | CM054612.2 | 28710772-34509381 |
| SSYh2 | chr23_hap2 | CM054556.2 | 33263362-38051052 |

## Exact tested downloads

| Species | Track | URL | Native MHC hap coverage |
|---|---|---|---|
| PTR | catRefGene | https://genomeark.s3.amazonaws.com/species/Pan_troglodytes/mPanTro3/assembly_curated/gene/mPanTro3_v2.0_CatRefConsensus_v3.bb | PTRh1 only |
| PTR | cat_v0.9 | https://genomeark.s3.amazonaws.com/species/Pan_troglodytes/mPanTro3/assembly_curated/gene/mPanTro3_catAnnotation_v0.9.bb | PTRh1 only |
| PTR | TOGA_hg38 | https://genomeark.s3.amazonaws.com/species/Pan_troglodytes/mPanTro3/assembly_curated/gene/HLTOGAannotVsHg38v1.bb | PTRh1 only |
| PTR | TOGA_mm10 | https://genomeark.s3.amazonaws.com/species/Pan_troglodytes/mPanTro3/assembly_curated/gene/HLTOGAannotVsMm10v1.bb | PTRh1 only |
| PTR | CGL_CAT_v0.9 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v0.9/GCA_028858775.2.catAnnotation.bb | PTRh1 only |
| PTR | CGL_CAT_v1.0 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v1.0/GCA_028858775.2.cat.bb | PTRh1 only |
| PTR | refGene | https://genomeark.s3.amazonaws.com/species/Pan_troglodytes/mPanTro3/assembly_curated/gene/mPanTro3_refGene.bb | PTRh1 only |
| PTR | catGene | https://genomeark.s3.amazonaws.com/species/Pan_troglodytes/mPanTro3/assembly_curated/gene/mPanTro3_catAnnotation_v1.0.bb | PTRh1 only |
| PPA | catRefGene | https://genomeark.s3.amazonaws.com/species/Pan_paniscus/mPanPan1/assembly_curated/gene/mPanPan1_v2.0_CatRefConsensus_v3.bb | PPAh2 only |
| PPA | cat_v0.9 | https://genomeark.s3.amazonaws.com/species/Pan_paniscus/mPanPan1/assembly_curated/gene/mPanPan1_catAnnotation_v0.9.bb | PPAh2 only |
| PPA | TOGA_hg38 | https://genomeark.s3.amazonaws.com/species/Pan_paniscus/mPanPan1/assembly_curated/gene/HLTOGAannotVsHg38v1.bb | PPAh2 only |
| PPA | TOGA_mm10 | https://genomeark.s3.amazonaws.com/species/Pan_paniscus/mPanPan1/assembly_curated/gene/HLTOGAannotVsMm10v1.bb | PPAh2 only |
| PPA | CGL_CAT_v0.9 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v0.9/GCA_029289425.2.catAnnotation.bb | PPAh2 only |
| PPA | CGL_CAT_v1.0 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v1.0/GCA_029289425.2.cat.bb | PPAh2 only |
| PPA | refGene | https://genomeark.s3.amazonaws.com/species/Pan_paniscus/mPanPan1/assembly_curated/gene/mPanPan1_refGene.bb | PPAh2 only |
| PPA | catGene | https://genomeark.s3.amazonaws.com/species/Pan_paniscus/mPanPan1/assembly_curated/gene/mPanPan1_catAnnotation_v1.0.bb | PPAh2 only |
| GGO | catRefGene | https://genomeark.s3.amazonaws.com/species/Gorilla_gorilla/mGorGor1/assembly_curated/gene/mGorGor1_v2.0_CatRefConsensus_v3.bb | GGOh1 only |
| GGO | cat_v0.9 | https://genomeark.s3.amazonaws.com/species/Gorilla_gorilla/mGorGor1/assembly_curated/gene/mGorGor1_catAnnotation_v0.9.bb | GGOh1 only |
| GGO | TOGA_hg38 | https://genomeark.s3.amazonaws.com/species/Gorilla_gorilla/mGorGor1/assembly_curated/gene/HLTOGAannotVsHg38v1.bb | GGOh1 only |
| GGO | TOGA_mm10 | https://genomeark.s3.amazonaws.com/species/Gorilla_gorilla/mGorGor1/assembly_curated/gene/HLTOGAannotVsMm10v1.bb | GGOh1 only |
| GGO | CGL_CAT_v0.9 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v0.9/GCA_029281585.2.catAnnotation.bb | GGOh1 only |
| GGO | CGL_CAT_v1.0 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v1.0/GCA_029281585.2.cat.bb | GGOh1 only |
| GGO | refGene | https://genomeark.s3.amazonaws.com/species/Gorilla_gorilla/mGorGor1/assembly_curated/gene/mGorGor1_refGene.bb | GGOh1 only |
| GGO | catGene | https://genomeark.s3.amazonaws.com/species/Gorilla_gorilla/mGorGor1/assembly_curated/gene/mGorGor1_catAnnotation_v1.0.bb | GGOh1 only |
| PAB | catRefGene | https://genomeark.s3.amazonaws.com/species/Pongo_abelii/mPonAbe1/assembly_curated/gene/mPonAbe1_v2.0_CatRefConsensus_v3.bb | PABh1 only |
| PAB | cat_v0.9 | https://genomeark.s3.amazonaws.com/species/Pongo_abelii/mPonAbe1/assembly_curated/gene/mPonAbe1_catAnnotation_v0.9.bb | PABh1 only |
| PAB | TOGA_hg38 | https://genomeark.s3.amazonaws.com/species/Pongo_abelii/mPonAbe1/assembly_curated/gene/HLTOGAannotVsHg38v1.bb | PABh1 only |
| PAB | TOGA_mm10 | https://genomeark.s3.amazonaws.com/species/Pongo_abelii/mPonAbe1/assembly_curated/gene/HLTOGAannotVsMm10v1.bb | PABh1 only |
| PAB | CGL_CAT_v0.9 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v0.9/GCA_028885655.2.catAnnotation.bb | PABh1 only |
| PAB | CGL_CAT_v1.0 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v1.0/GCA_028885655.2.cat.bb | PABh1 only |
| PAB | refGene | https://genomeark.s3.amazonaws.com/species/Pongo_abelii/mPonAbe1/assembly_curated/gene/mPonAbe1_refGene.bb | PABh1 only |
| PAB | catGene | https://genomeark.s3.amazonaws.com/species/Pongo_abelii/mPonAbe1/assembly_curated/gene/mPonAbe1_catAnnotation_v1.0.bb | PABh1 only |
| PPY | catRefGene | https://genomeark.s3.amazonaws.com/species/Pongo_pygmaeus/mPonPyg2/assembly_curated/gene/mPonPyg2_v2.0_CatRefConsensus_v3.bb | PPYh1 only |
| PPY | cat_v0.9 | https://genomeark.s3.amazonaws.com/species/Pongo_pygmaeus/mPonPyg2/assembly_curated/gene/mPonPyg2_catAnnotation_v0.9.bb | PPYh1 only |
| PPY | TOGA_hg38 | https://genomeark.s3.amazonaws.com/species/Pongo_pygmaeus/mPonPyg2/assembly_curated/gene/HLTOGAannotVsHg38v1.bb | PPYh1 only |
| PPY | TOGA_mm10 | https://genomeark.s3.amazonaws.com/species/Pongo_pygmaeus/mPonPyg2/assembly_curated/gene/HLTOGAannotVsMm10v1.bb | PPYh1 only |
| PPY | CGL_CAT_v0.9 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v0.9/GCA_028885625.2.catAnnotation.bb | PPYh1 only |
| PPY | CGL_CAT_v1.0 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v1.0/GCA_028885625.2.cat.bb | PPYh1 only |
| PPY | refGene | https://genomeark.s3.amazonaws.com/species/Pongo_pygmaeus/mPonPyg2/assembly_curated/gene/mPonPyg2_refGene.bb | PPYh1 only |
| PPY | catGene | https://genomeark.s3.amazonaws.com/species/Pongo_pygmaeus/mPonPyg2/assembly_curated/gene/mPonPyg2_catAnnotation_v1.0.bb | PPYh1 only |
| SSY | catRefGene | https://genomeark.s3.amazonaws.com/species/Symphalangus_syndactylus/mSymSyn1/assembly_curated/gene/mSymSyn1_v2.0_CatRefConsensus_v3.bb | SSYh1 only |
| SSY | cat_v0.9 | https://genomeark.s3.amazonaws.com/species/Symphalangus_syndactylus/mSymSyn1/assembly_curated/gene/mSymSyn1_catAnnotation_v0.9.bb | SSYh1 only |
| SSY | TOGA_hg38 | https://genomeark.s3.amazonaws.com/species/Symphalangus_syndactylus/mSymSyn1/assembly_curated/gene/HLTOGAannotVsHg38v1.bb | SSYh1 only |
| SSY | TOGA_mm10 | https://genomeark.s3.amazonaws.com/species/Symphalangus_syndactylus/mSymSyn1/assembly_curated/gene/HLTOGAannotVsMm10v1.bb | SSYh1 only |
| SSY | CGL_CAT_v0.9 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v0.9/GCA_028878055.2.catAnnotation.bb | SSYh1 only |
| SSY | CGL_CAT_v1.0 | https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v1.0/GCA_028878055.2.cat.bb | SSYh1 only |
| SSY | refGene | https://genomeark.s3.amazonaws.com/species/Symphalangus_syndactylus/mSymSyn1/assembly_curated/gene/mSymSyn1_refGene.bb | SSYh1 only |
| SSY | catGene | https://genomeark.s3.amazonaws.com/species/Symphalangus_syndactylus/mSymSyn1/assembly_curated/gene/mSymSyn1_catAnnotation_v1.0.bb | SSYh1 only |

## Hub and bulk-release evidence

- NHGRI stable and developmental gen.trackDb.txt files are byte-for-byte identical for each of the six assemblies. They advertise CAT+RefSeq v3, CAT/LiftOff v1.0, RefSeq March 2024, human/mouse TOGA, and manually curated MHC. No Ensembl or secondary-haplotype-specific CAT/RefSeq track is listed. See stable_development_comparison.json and archived live trackDb files.
- The manually curated MHC track is BED9 locus annotation, not exon-resolved continuous context gene models; it already covers both MHC haplotypes in the figure workflow.
- marbl/Primates README explicitly says only selected primary assemblies will be annotated and serve as linear references. https://github.com/marbl/Primates#assembly-releases
- CGL CAT bulk v0.9 and v1.0 index files contain six primary-assembly accession outputs only. Their 12 BigBed dictionaries are checked above. https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/8-t2t-apes-2023v2/annotations-v1.0/
- The CGL 16-way diploid resource includes both primary/alternate or maternal/paternal genomes, but its public directory provides HAL, MAF, chains and configuration; no CAT annotation output is listed. The accompanying README documents an alignment workflow. https://cgl.gi.ucsc.edu/data/cactus/t2t-apes/16-t2t-apes-2023v2/
- CAT and TOGA annotations themselves use source-genome projection plus other evidence. They are released models in target coordinates, distinct from performing a new chain projection locally. No released target-coordinate model for the six missing chromosomes was found in these resources.
- CAT consensus description confirms protein-coding genes only and use of all six primary assemblies. Full CAT includes other biotypes; switching to it improves primary coverage but does not add missing-haplotype chromosomes. https://raw.githubusercontent.com/marbl/T2T-Browser/main/T2Tgenomes/html/consGene.html
- The canzarlab/apes_transcriptome_analysis repository contains StringTie transcript assemblies for five species (no siamang). These are expression-derived transcript outputs, not a complete six-species secondary-haplotype gene annotation. A sample is Git LFS (115,967,340 bytes for gorilla); no claim of missing-haplotype coverage is made here. https://github.com/canzarlab/apes_transcriptome_analysis

## Coordinate limitation

All queries use the existing browser chromosome aliases and unchanged native BED0 windows. Primary/alternate NCBI assembly accessions can differ from h1/h2 labels, especially trio-phased bonobo and gorilla. Do not substitute primary chromosome coordinates onto a secondary chromosome. The unrelated v1/chrXY hub was not used as an MHC annotation source.
