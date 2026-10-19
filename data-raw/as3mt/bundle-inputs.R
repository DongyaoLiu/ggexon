# Refresh the installed offline example after rebuilding the raw-data outputs.
# Run from the repository root; this does not fetch or align sequences.
source_dir <- "data-raw/as3mt"
bundle_dir <- "inst/extdata/as3mt"
stopifnot(dir.exists(bundle_dir))
inputs <- c("human.gff3", "chimp.gff3", "human.fa", "chimp.fa",
  "human_chimp.nogap.paf", "human-chimp-links.nogap.tsv",
  "human-chimp-links.nogap.top2.tsv", "sequence-provenance.nogap.json",
  "organoid-reported-values.tsv", "organoid-summary.tsv")
stopifnot(all(file.copy(file.path(source_dir, inputs), bundle_dir, overwrite = TRUE)))
snapshots <- list.files(file.path(source_dir, "handoff"), recursive = TRUE, full.names = TRUE)
for (path in snapshots) {
  relative <- substring(path, nchar(source_dir) + 2L)
  destination <- file.path(bundle_dir, relative)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(path, destination, overwrite = TRUE))
}
spec <- jsonlite::fromJSON(file.path(bundle_dir, "handoff/as3mt_plot_spec.json"), simplifyVector = FALSE)
provenance <- jsonlite::fromJSON(file.path(bundle_dir, "sequence-provenance.nogap.json"))
saveRDS(spec, file.path(bundle_dir, "specification.rds"), version = 2)
saveRDS(provenance, file.path(bundle_dir, "alignment-provenance.rds"), version = 2)
dir.create(file.path(bundle_dir, "figures"), showWarnings = FALSE)
for (extension in c("png", "pdf")) {
  filename <- paste0("AS3MT-exon-skipping-neural-phenotypes.", extension)
  stopifnot(file.copy(file.path(source_dir, "figures", filename),
    file.path(bundle_dir, "figures", filename), overwrite = TRUE))
}
message("Refreshed offline AS3MT bundle.")
