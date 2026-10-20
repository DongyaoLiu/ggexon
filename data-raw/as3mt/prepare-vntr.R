# Reproduce the literature-supported VNTR feature from the supplied specification
# and independently check each base against the offline assembly FASTAs.
source_dir <- "data-raw/as3mt"
bundle_dir <- "inst/extdata/as3mt"
spec <- readRDS(file.path(bundle_dir, "specification.rds"))
motif <- spec$genomic_VNTR$motif
human <- as.character(Biostrings::readDNAStringSet(file.path(source_dir, "human.fa"))[[1]])
chimp <- as.character(Biostrings::readDNAStringSet(file.path(source_dir, "chimp.fa"))[[1]])
human_start <- 102869470
chimp_start <- 104324156
human_units <- data.frame(start = c(102869498, 102869534, 102869570),
  end = c(102869533, 102869569, 102869605), unit = 1:3)
stopifnot(nchar(motif) == 36L,
  all(vapply(1:3, function(i) substr(human,
    human_units$start[i] - human_start + 1, human_units$end[i] - human_start + 1) == motif,
    logical(1))))
# Align the homologous unit by the independently annotated E1 donor, not by an
# arbitrary local match among identical human copies. The human extra copies
# are placed before the donor-containing copy; their individual histories are
# not identifiable from this alignment.
donor_after <- c(human = 102869593, chimp = 104324234)
chimp_unit_start <- donor_after[["chimp"]] - 23L
chimp_unit <- substr(chimp, chimp_unit_start - chimp_start + 1,
  chimp_unit_start - chimp_start + 36)
stopifnot(substr(motif, 25, 26) == "GT", substr(chimp_unit, 25, 26) == "GT",
  sum(strsplit(motif, "", fixed = TRUE)[[1]] == strsplit(chimp_unit, "", fixed = TRUE)[[1]]) == 34L)
anchors <- data.frame(sequence = c("human", "chimp"), start = c(102869498, chimp_unit_start),
  strand = "+", track = c("human", "chimp"), chr = "chr10", y = 1.9)
aligned <- c(human = strrep(motif, 3), chimp = paste0(strrep("-", 72), chimp_unit))
constructs <- data.frame(repeat_count = c(2L, 3L),
  sequence = c(strrep(motif, 2), strrep(motif, 3)),
  interpretation = "Motif-level reconstruction of repeat counts tested by Cai2021; not deposited full plasmid sequences")
vntr <- list(motif = motif, human_units = human_units,
  chimp_unit = chimp_unit, chimp_unit_start = chimp_unit_start,
  chimp_unit_end = chimp_unit_start + 35L, donor_after = donor_after,
  aligned = aligned, anchors = anchors, reference = "human",
  regions = c(102869498, 102869605), details = c(102869570, 102869605), flank = 0L,
  constructs = constructs,
  provenance = list(motif_source = "Li2016 Supplementary Fig.3a; supplied as3mt_plot_spec.json",
    functional_source = "Cai2021 Fig.1, doi:10.1093/schbul/sbaa098",
    effect = "3-repeat minigenes produced more d2d3 than 2-repeat minigenes in HEK293T, NIH/3T3 and Neuro-2a assays",
    comparison = "Selected GRCh38 three-exact-unit array and donor-anchored Pan_tro_3.0 imperfect homologous unit",
    alignment_method = "Explicit donor-anchored repeat-unit alignment; equivalent identical-copy gap placements are not distinguishable",
    caveat = "Functional human 2-versus-3-repeat evidence regulates splicing; it does not prove evolutionary sufficiency of the human-versus-chimp expansion",
    human_fasta_md5 = unname(tools::md5sum(file.path(source_dir, "human.fa"))),
    chimp_fasta_md5 = unname(tools::md5sum(file.path(source_dir, "chimp.fa")))))
saveRDS(vntr, file.path(bundle_dir, "vntr-dna.rds"), version = 2)
utils::write.table(human_units, file.path(source_dir, "vntr-repeat-units.tsv"),
  sep = "\t", quote = FALSE, row.names = FALSE)
writeLines(as.vector(rbind(paste0(">human_", constructs$repeat_count,
  "_repeat_motif_reconstruction_not_full_plasmid"), constructs$sequence)),
  file.path(source_dir, "vntr-motif-reconstructions.fasta"))
message("Verified human 3 x 36 bp array, chimp 34/36 homolog and conserved E1 donor.")
