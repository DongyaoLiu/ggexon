## ---- cd44-object-construction ----
# Example-level loader, not an exported ggexon function.
load_cd44_example <- function(demo_dir) {
  # Keep all supporting tables in the comparative object's metadata.
  paths <- list.files(demo_dir, pattern = "\\.tsv$", full.names = TRUE)
  tables <- stats::setNames(lapply(paths, function(path) {
    utils::read.delim(path, check.names = FALSE)
  }), sub("\\.tsv$", "", basename(paths)))
  species <- tables$cd44_species
  selected_exons <- tables$cd44_selected_exons
  raw_paf <- utils::read.table(file.path(demo_dir, "cd44_lastz.paf"),
    sep = "\t", header = FALSE, quote = "", comment.char = "", fill = TRUE)
  names(raw_paf)[1:12] <- c("qname", "qlen", "qstart", "qend", "strand",
    "tname", "tlen", "tstart", "tend", "nmatch", "alen", "mapq")

  cd44 <- ggexon::SynSpecies(name = "CD44 human-mouse comparison", metadata = list(
    species = species,
    tables = tables,
    raw_paf = raw_paf,
    raw_paf_coordinates = "zero-based, half-open offsets within the extracted windows",
    alignment_coordinates = "one-based, closed chromosome coordinates"
  ))

  for (id in c("human", "mouse")) {
    info <- species[species$species == id, , drop = FALSE]
    window_sequence <- Biostrings::readDNAStringSet(file.path(demo_dir, info$fasta_file))
    # The FASTA is a window, not a full chromosome: do not use it as genome_file.
    individual <- ggexon::SynIndividual(
      id = id,
      annotation_file = file.path(demo_dir, "annotations", paste0(id, ".gff3")),
      metadata = list(
        species = info,
        selected_isoforms = tables$cd44_selected_isoforms[
          tables$cd44_selected_isoforms$species == id, , drop = FALSE],
        sequence_window = list(chr = info$chr, start = info$window_start,
          end = info$window_end, sequence = window_sequence)
      )
    )
    individual <- ggexon::load_annotation(individual)
    features <- ggexon::annotation_data(individual)
    attributes <- S4Vectors::mcols(features)
    exons <- selected_exons[selected_exons$species == id, , drop = FALSE]
    feature_key <- paste(attributes$transcript_id,
      as.character(GenomeInfoDb::seqnames(features)),
      BiocGenerics::start(features), BiocGenerics::end(features), sep = ":")
    exon_key <- paste(exons$transcript_id, exons$chr, exons$xmin, exons$xmax, sep = ":")
    index <- match(feature_key, exon_key)
    is_exon <- as.character(attributes$type) == "exon"
    stopifnot(!anyNA(index[is_exon]), sum(is_exon) == nrow(exons))
    attributes$exon_role <- exons$exon_role[index]
    attributes$selection_order <- exons$selection_order[index]
    attributes$isoform_y <- exons$ymin[index]
    S4Vectors::mcols(features) <- attributes
    ggexon::annotation_data(individual) <- features
    cd44 <- ggexon::add_individual(cd44, individual)
  }

  # The prepared link table already includes the genomic window offsets and
  # filtering. Cache that representation; do not reimport the raw local PAF.
  links <- tables$cd44_nuclinks_lastz
  links$identity_bin <- factor(links$identity_bin,
    levels = c("50-55%", "55-60%", "60-65%", "65-70%", ">=70%"))
  links$alen <- links$alignment_length
  links$nmatch <- links$score
  alignment <- ggexon::SynPairAlignment(
    name = "human_mouse", query_individual = "mouse", target_individual = "human",
    file = "<CD44 chromosome-coordinate LASTZ links>", format = "paf", data = links,
    metadata = list(
      source_file = file.path(demo_dir, "cd44_lastz.paf"),
      coordinate_system = "one-based, closed chromosome coordinates",
      source_coordinate_system = "zero-based, half-open window coordinates",
      minimum_alignment_length = 80L, minimum_identity = 50,
      provenance = tables$cd44_provenance
    )
  )
  alignment <- ggexon::load_alignment(alignment)
  cd44 <- ggexon::add_pairwise_alignment(cd44, alignment)
  # Keep presentation order and genomic windows with the comparison, too.
  cd44 <- ggexon::store_chain_layout(cd44, free = list(x = TRUE, y = FALSE))
  windows <- stats::setNames(lapply(seq_len(nrow(species)), function(i) {
    c(species$window_start[i], species$window_end[i])
  }), species$species)
  cd44 <- ggexon::set_panel_xlim(cd44, individual = species$species, xlim = windows,
    xlim_chr = stats::setNames(species$chr, species$species))
  methods::validObject(cd44)
  cd44
}
