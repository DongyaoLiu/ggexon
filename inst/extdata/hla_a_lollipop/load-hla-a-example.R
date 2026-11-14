# Example-level helper; not an exported package function.
load_hla_a_example <- function(data_dir = system.file(
  "extdata", "hla_a_lollipop", package = "ggexon"
)) {
  read_table <- function(name) {
    utils::read.delim(file.path(data_dir, name), stringsAsFactors = FALSE,
                      check.names = FALSE)
  }
  alleles <- read_table("hla_a_alleles.tsv")
  domains <- read_table("hla_a_domains.tsv")
  proteins <- Biostrings::readAAStringSet(file.path(
    data_dir, "hla_a_selected_proteins.fasta.gz"
  ))
  names(proteins) <- vapply(strsplit(names(proteins), " "), `[`, "", 2L)
  stopifnot(identical(names(proteins), alleles$allele),
            all(Biostrings::width(proteins) == 365L),
            sum(alleles$reference) == 1L, all(alleles$signal_length == 24L))
  sequences <- substring(as.character(proteins), 25L)
  residues <- do.call(rbind, strsplit(sequences, "", fixed = TRUE))
  reference <- alleles$allele[alleles$reference]
  ref <- residues[alleles$reference, ]

  # Retain all states (including reference matches) for transparent denominators.
  states <- do.call(rbind, lapply(seq_along(ref), function(position) {
    amino_acids <- sort(unique(residues[, position]))
    do.call(rbind, lapply(amino_acids, function(aa) {
      carriers <- alleles$allele[residues[, position] == aa]
      data.frame(position = position, precursor_position = position + 24L,
                 ref = ref[[position]], alt = aa, is_reference = aa == ref[[position]],
                 allele_count = length(carriers),
                 example_alleles = paste(utils::head(carriers, 3L), collapse = ";"))
    }))
  }))
  mutations <- states[!states$is_reference, , drop = FALSE]
  mutations$mutation <- paste0(mutations$ref, mutations$position, mutations$alt)
  n_alternatives <- table(mutations$position)
  mutations$n_alternatives <- as.integer(n_alternatives[as.character(mutations$position)])
  mutations$site_class <- ifelse(mutations$n_alternatives > 1L,
                                 "Multiple alternatives", "Single alternative")
  aa_groups <- c(
    A = "Aliphatic", V = "Aliphatic", I = "Aliphatic", L = "Aliphatic", M = "Aliphatic",
    F = "Aromatic", W = "Aromatic", Y = "Aromatic",
    S = "Polar", T = "Polar", N = "Polar", Q = "Polar", C = "Polar",
    K = "Basic", R = "Basic", H = "Basic", D = "Acidic", E = "Acidic",
    G = "Gly / Pro", P = "Gly / Pro"
  )
  mutations$amino_acid_group <- unname(aa_groups[mutations$alt])
  rownames(mutations) <- rownames(states) <- NULL
  # Prefer a well-known example residue if it is multiallelic in this panel.
  focus_position <- if (any(mutations$position == 156L & mutations$n_alternatives > 1L)) {
    156L
  } else {
    as.integer(names(n_alternatives)[which.max(n_alternatives)])
  }
  # Store one canonical, precursor-coordinate annotation for projection.
  # Mature coordinates remain explicit columns for the protein summary view.
  mutations$mature_position <- mutations$position
  mutations$position <- mutations$precursor_position
  mutations$gene_id <- "HLA_A0201"
  mutations$transcript_id <- "HLA_A0201_tx"
  mutations$sample_count <- mutations$allele_count
  mutations$protein_length <- 365L
  domains$mature_start <- domains$start
  domains$mature_end <- domains$end
  domains$start <- domains$start + 24L
  domains$end <- domains$end + 24L
  domains$gene_id <- "HLA_A0201"
  domains$transcript_id <- "HLA_A0201_tx"
  domains$protein_length <- 365L

  model <- ggexon::SynIndividual(
    id = "HLA_A0201",
    genome_file = file.path(data_dir, "hla_a_reference_genomic.fasta"),
    annotation_file = file.path(data_dir, "hla_a_reference.gff3"),
    metadata = list(
      reference_allele = reference,
      coordinate_system = "HLA00005 allele-local genomic bases, one-based closed",
      hla_a = list(
        alleles = alleles, proteins = proteins, states = states,
        reference = reference, signal_length = 24L,
        protein_length = ncol(residues), precursor_length = 365L,
        focus_position = focus_position,
        distinct_mature_proteins = length(unique(sequences)),
        sources = read_table("hla_a_sources.tsv")
      )
    )
  )
  model <- ggexon::load_annotation(model)
  model <- ggexon::translate_protein(model, genes = "HLA_A0201")
  stopifnot(identical(unname(as.character(ggexon::protein_seq(model))),
                     paste0(unname(as.character(proteins[alleles$reference])), "*")))
  model <- ggexon::add_annotation(model, ggexon::SynProteinMutationAnnotation(
    name = "hla_substitutions", mutation_file = "<computed HLA-A allele summary>",
    mutation_data = mutations, keytype = "transcript_id", lazy = FALSE,
    metadata = list(coordinates = "precursor amino-acid positions",
                    mutation_labels = "mature-protein positions")
  ), set_active = FALSE)
  regions <- ggexon::SynProteinDomainAnnotation(
    name = "hla_regions", domain_file = file.path(data_dir, "hla_a_domains.tsv"),
    keytype = "transcript_id", source_db = "UniProt P04439", lazy = FALSE,
    metadata = list(coordinates = "precursor amino-acid positions",
                    signal_length = 24L)
  )
  regions@domain_data <- S4Vectors::DataFrame(domains)
  regions@loaded <- TRUE
  ggexon::add_annotation(model, regions, set_active = FALSE)
}

# The display view of the stored structural regions, in mature coordinates.
hla_a_regions <- function(model) {
  regions <- as.data.frame(ggexon::query_domains(
    ggexon::get_annotation(model, "hla_regions"), ids = "HLA_A0201_tx"
  ))
  regions$start <- regions$mature_start
  regions$end <- regions$mature_end
  regions[, c("domain", "start", "end"), drop = FALSE]
}

# Source records and counting evidence remain inside the reference object.
hla_a_evidence <- function(model) {
  if (!methods::is(model, "SynIndividual")) {
    stop("Supply the SynIndividual returned by load_hla_a_example().", call. = FALSE)
  }
  model@metadata$hla_a
}
