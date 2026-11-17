# Example helper for the grey native-span background view. Gene-lollipop
# data and shared slots are prepared by the package's prepare_gene_lollipop().
prepare_mhc_context <- function(comparison) {
  stopifnot(methods::is(comparison, "SynSpecies"))
  rows <- lapply(ggexon::individual_names(comparison), function(id) {
    gr <- ggexon::query_features(comparison@individuals[[id]],
      feature_type = "gene", all = TRUE)
    gr <- gr[as.character(S4Vectors::mcols(gr)$gene_role) == "context"]
    meta <- S4Vectors::mcols(gr)
    data.frame(track = id, chr = as.character(GenomeInfoDb::seqnames(gr)),
      gene_id = as.character(meta$gene_id),
      gene_name = as.character(meta$gene_name),
      xmin = IRanges::start(gr), xmax = IRanges::end(gr),
      x = (IRanges::start(gr) + IRanges::end(gr)) / 2,
      ymin = 0, strand = as.character(BiocGenerics::strand(gr)),
      transcripts = as.character(meta$gene_id),
      role = as.character(meta$gene_role),
      label = as.character(meta$plot_label),
      fill = as.character(meta$plot_fill), stringsAsFactors = FALSE)
  })
  genes <- do.call(rbind, rows)
  comparison@metadata$mhc_plot_data <- list(context = genes)
  comparison
}

mhc_plot_data <- function(comparison, view) {
  if (identical(view, "MHC"))
    return(ggexon::query_gene_lollipop(comparison, name = "mhc"))
  data <- comparison@metadata$mhc_plot_data[[view]]
  if (is.null(data)) stop("Unknown MHC plot view: ", view, call. = FALSE)
  data
}
