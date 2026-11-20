# A population VCF uses the reference assembly's coordinates.
# The correspondence table represents MUMmer "show-snps -ClrTHI pair.delta".
library(ggexon)
library(ggplot2)

vcf_file <- tempfile(fileext = ".vcf")
snp_file <- tempfile(fileext = ".snps")
ref_pos <- c(140, 180, 220, 260)
query_pos <- ref_pos + 800
writeLines(c(
  "##fileformat=VCFv4.2",
  '##INFO=<ID=AF,Number=A,Type=Float,Description="Population ALT frequency">',
  "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
  sprintf("chrR\t%d\t.\tA\tG\t.\tPASS\tAF=%s", ref_pos, c(0, .25, .5, 1))
), vcf_file)
writeLines(sprintf(
  "%d\tA\tG\t%d\t1\t1\t2000\t2000\t1\t1\tchrR\tchrQ", ref_pos, query_pos
), snp_file)

reference <- add_annotation(SynIndividual(id = "reference"),
                            SynVCFAnnotation("population", vcf_file))
strains <- SynSpecies("pair") |>
  add_individual(reference) |>
  add_individual(SynIndividual(id = "alternate"))
block <- data.frame(qchr = "chrQ", qstart = 900, qend = 1100,
                    tchr = "chrR", tstart = 100, tend = 300,
                    strand = "+", alen = 201)
strains <- add_pairwise_alignment(strains, SynPairAlignment(
  "assembly_pair", "alternate", "reference", "pair.paf", data = block
))
strains <- strains |>
  add_snp_annotation(SynSNPAnnotation("differences", snp_file, "reference",
                                      "alternate", "assembly_pair")) |>
  prepare_allele_frequency(vcf = "population", snps = "differences",
                           name = "population_AF")
print(query_allele_frequency(strains, component = "summary"))
print(query_allele_frequency(strains, component = "excluded"))

# Small exon tracks establish independent genomic x scales.
exons <- data.frame(track = rep(c("reference", "alternate"), each = 2),
                    xmin = c(1, 180, 890, 1110), xmax = c(165, 330, 1065, 1190))
frequency_plot <- ggexon(strains) +
  geom_exon2(aes(xmin = xmin, xmax = xmax, ymin = 1, track = track,
                 transcripts = track, strand = "+", type = "exon"),
             data = exons, annotation_type = "all", compress_introns = FALSE,
             fill = "#536975") +
  geom_nuclink(alignment = "assembly_pair", fill = "#91b7ca", colour = NA, alpha = .55) +
  geom_allele_frequency(preparation = "population_AF",
                       aes(colour = af), size = 3, show.legend = TRUE) +
  scale_colour_gradient(name = "ALT frequency", low = "#162458", high = "#bd3a16",
                        breaks = c(0, .25, .5, 1)) +
  facet_genomics(vars(track), ncol = 1, scales = "free_x", link_axis = "none",
                 link_strip = "blank") +
  labs(x = "Genomic position (bp)", y = NULL)
# With default panel clipping, the edge bisects the AF=0 and AF=1 glyphs.
print(frequency_plot)
