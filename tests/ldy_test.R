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
indexed_gff_dir <- file.path(repo_root, "inst", "extdata", "gff")

xz_window_chr <- "RagTag_V"
xz_window <- c(21558028, 21620381)
n2_window_chr <- "V"
n2_window <- c(20454111, 20491853)

# Indexed folder speed test -------------------------------------------------

indexed_gff_files <- sort(list.files(
  indexed_gff_dir,
  pattern = "\\.gff3\\.gz$",
  full.names = TRUE
))
stopifnot(length(indexed_gff_files) > 0L)

indexed_only_dir <- file.path(tempdir(), "ggexon-indexed-gff")
unlink(indexed_only_dir, recursive = TRUE, force = TRUE)
dir.create(indexed_only_dir, recursive = TRUE, showWarnings = FALSE)

for (annotation_path in indexed_gff_files) {
  file.symlink(
    annotation_path,
    file.path(indexed_only_dir, basename(annotation_path))
  )
  file.symlink(
    paste0(annotation_path, ".tbi"),
    file.path(indexed_only_dir, paste0(basename(annotation_path), ".tbi"))
  )
}

indexed_synspecies_time <- system.time({
  indexed_folder_sp <- SynSpecies(
    name = "CaenorhabditisIndexed",
    annotation_folder = indexed_only_dir
  )
})

indexed_query_time <- system.time({
  indexed_window_gr <- query_features(
    individuals(indexed_folder_sp)[["caenorhabditis_XZ1516"]],
    chr = xz_window_chr,
    start = xz_window[1],
    end = xz_window[2],
    feature_type = NULL
  )
})

indexed_full_load_time <- system.time({
  indexed_loaded_individuals <- lapply(individuals(indexed_folder_sp), load_annotation)
})

indexed_row_counts <- vapply(
  indexed_loaded_individuals,
  function(ind) length(annotation_data(ind)),
  integer(1)
)

cat("Indexed GFF folder:", indexed_gff_dir, "\n")
cat("Indexed GFF files discovered:", length(indexed_gff_files), "\n")
cat(
  "SynSpecies(annotation_folder = indexed_only_dir) elapsed:",
  indexed_synspecies_time[["elapsed"]],
  "seconds\n"
)
cat(
  "Indexed XZ1516 window query rows:",
  length(indexed_window_gr),
  "elapsed:",
  indexed_query_time[["elapsed"]],
  "seconds\n"
)
cat(
  "Full indexed annotation materialization elapsed:",
  indexed_full_load_time[["elapsed"]],
  "seconds\n"
)
cat("Loaded annotation row counts (first five):\n")
print(utils::head(indexed_row_counts, n = 5L))









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
  geom_exon(aes(fill = gene_id),
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


# ODGI multiple-alignment test ---------------------------------------------

odgi_graph <- system.file(
  "extdata",
  "XZ1516_chrV_21560000_21620000.og",
  package = "ggexon"
)
odgi_bin <- Sys.getenv("ODGI_BIN", unset = Sys.which("odgi"))

if (!nzchar(odgi_bin)) {
  cat(
    "ODGI binary not found; skipping graph-backed ODGI test. ",
    "Set ODGI_BIN or add odgi to PATH to run it.\n",
    sep = ""
  )
} else {
  odgi_table_time <- system.time({
    odgi_tsv <- odgi_node_table(
      og_file = odgi_graph,
      output = tempfile(pattern = "ggexon-odgi-", fileext = ".tsv"),
      odgi = odgi_bin,
      read = FALSE
    )
  })

  odgi_parse_time <- system.time({
    odgi_multi <- odgi_multi_alignment(
      odgi_tsv,
      name = "XZ1516_chrV_21560000_21620000"
    )
  })

  odgi_holder <- SynSpecies(name = "ODGIHolder")
  odgi_holder <- add_multiple_alignment(odgi_holder, odgi_multi)
  odgi_multi_data <- multiple_alignment_data(
    odgi_holder,
    alignment = "XZ1516_chrV_21560000_21620000"
  )

  cat("ODGI graph file:", odgi_graph, "\n")
  cat("ODGI node table file:", odgi_tsv, "\n")
  cat("ODGI node-table build elapsed:", odgi_table_time[["elapsed"]], "seconds\n")
  cat("ODGI parse elapsed:", odgi_parse_time[["elapsed"]], "seconds\n")
  cat(
    "Stored multiple alignments:",
    paste(names(multiple_alignments(odgi_holder)), collapse = ", "),
    "\n"
  )
  cat("ODGI alignment format:", alignment_format(odgi_multi), "\n")
  cat(
    "ODGI alignment individuals:",
    paste(alignment_individuals(odgi_multi), collapse = ", "),
    "\n"
  )
  cat("ODGI alignment rows:", nrow(odgi_multi_data), "\n")
  print(utils::head(odgi_multi_data, n = 10L))
}
