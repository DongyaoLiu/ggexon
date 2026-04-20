library(devtools)
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[1L]))
} else {
  normalizePath("tests/ldy_test.R")
}
repo_root <- dirname(script_path)
repo_root <- normalizePath(file.path(repo_root, ".."))
setwd(repo_root)
devtools::load_all(".")

# Input files ---------------------------------------------------------------

xz_genome <- system.file("extdata", "XZ1516.fasta", package = "ggexon")
xz_annotation <- system.file(
  "extdata",
  "caenorhabditis_XZ1516.gff3",
  package = "ggexon"
)
xz_patch <- system.file("extdata", "XZ1516.TA.gff", package = "ggexon")
xz_interpro <- system.file("extdata", "InterProScan.tsv", package = "ggexon")

n2_genome <- system.file(
  "extdata",
  "c_elegans.PRJNA13758.WS285.genomic.fa",
  package = "ggexon"
)
n2_annotation <- system.file(
  "extdata",
  "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf",
  package = "ggexon"
)

paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")

xz_window_chr <- "RagTag_V"
xz_window <- c(21558028, 21620381)
n2_window_chr <- "V"
n2_window <- c(20454111, 20491853)

# XZ1516 setup --------------------------------------------------------------

xz <- SynIndividual(
  genome_file = xz_genome,
  annotation_file = xz_annotation,
  id = "XZ1516"
) |>
  load_annotation()

xz <- patch_annotation_from_gff(
  xz,
  patch_file = xz_patch,
  mode = "replace",
  name = "ta-correction"
)

xz <- add_interproscan_annotation(
  xz,
  domain_file = xz_interpro,
  name = "interpro"
)
xz <- rename_domain_annotation_ids(
  xz,
  annotation = "interpro",
  mapping = c(Sequence1 = "mRNAXZ1516_zina-1"),
  from = "protein_id",
  to = "transcript_id",
  drop_unmapped = TRUE
)

patched_ann <- get_annotation(xz)
cat("Applied patches:", paste(names(list_patches(xz)), collapse = ", "), "\n")
cat("Patched annotation rows:", length(annotation_data(patched_ann)), "\n")
cat("InterProScan file:", xz_interpro, "\n")
cat("Mapped InterProScan IDs: Sequence1 -> mRNAXZ1516_zina-1\n")

projected_interpro <- project_domains_to_genome(
  xz,
  annotation = "interpro",
  model = c("SMART", "PANTHER", "Gene3D", "ProSiteProfiles"),
  chr = xz_window_chr,
  start = xz_window[1],
  end = xz_window[2]
)
xz <- store_projected_domains(xz, projected_interpro, name = "interpro_window")

cat("Projected InterPro rows in XZ1516 window:", nrow(projected_interpro), "\n")
cat(
  "Projected tables stored on XZ1516:",
  paste(names(projected_domains(xz)), collapse = ", "),
  "\n"
)
if (nrow(projected_interpro) > 0L) {
  print(utils::head(projected_interpro, n = 10L))
} else {
  cat("No projected domain rows were found for this window.\n")
  cat("This usually means the InterProScan IDs do not match the XZ1516 gene/transcript IDs.\n")
}

# SynSpecies setup ----------------------------------------------------------

sp <- SynSpecies(name = "Caenorhabditis")
sp <- add_individual(sp, xz)
sp <- add_individual(
  sp,
  test_syn_individual(
    genome_file = n2_genome,
    annotation_file = n2_annotation,
    id = "N2",
    annotation_format = "gtf"
  )
)

sp <- add_pairwise_alignment(
  sp,
  SynPairAlignment(
    name = "XZ1516_vs_N2",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = paf_path
  )
)

panels <- data.frame(
  PANEL = c(1L, 2L, 3L),
  ROW = c(1L, 2L, 3L),
  COL = c(1L, 1L, 1L),
  track = c("XZ1516", "link_XZ1516_vs_N2", "N2"),
  panel_type = c("annotation", "link", "annotation"),
  species = c("XZ1516", NA, "N2"),
  alignment_name = c(NA, "XZ1516_vs_N2", NA),
  tspecies = c(NA, "N2", NA),
  qspecies = c(NA, "XZ1516", NA),
  stringsAsFactors = FALSE
)

species_layout(sp) <- SynLayout(
  panels = panels,
  layout_type = "chain",
  free = list(x = TRUE, y = TRUE),
  exon_height = 0.8,
  y_scale = 100,
  x_translation = 0
)

# Plot ----------------------------------------------------------------------

plot_obj <- ggexon(sp) +
  geom_exon(aes(fill = transcript_id),
    species = "XZ1516",
    chr = xz_window_chr,
    subset = xz_window
  ) +
  geom_exon(
    species = "N2",
    chr = n2_window_chr,
    subset = n2_window
  ) +
  geom_genelabel() +
  geom_nuclink() +
  geom_motif(
    annotation = "interpro",
    model = c("SMART", "PANTHER", "Gene3D", "ProSiteProfiles"),
    y_offset = -1.1,
    fill = "#C24E3F",
    alpha = 0.85
  ) +
  facet_genomics(ggplot2::vars(track), scales = "free")

if (!interactive()) {
  dir.create("codex_output", showWarnings = FALSE, recursive = TRUE)
  png(
    filename = "codex_output/ldy_test_plot.png",
    width = 1600,
    height = 1200,
    res = 180
  )
  on.exit(dev.off(), add = TRUE)
}

print(plot_obj)
