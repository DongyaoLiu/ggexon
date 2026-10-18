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
  list(alleles = alleles, proteins = proteins, domains = domains,
       states = states, mutations = mutations, reference = reference,
       protein_length = ncol(residues), focus_position = focus_position,
       distinct_mature_proteins = length(unique(sequences)),
       sources = read_table("hla_a_sources.tsv"))
}
