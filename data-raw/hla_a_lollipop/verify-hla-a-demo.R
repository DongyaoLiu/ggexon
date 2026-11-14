# Independent checks against original sequence records, run from repo root.
pkgload::load_all(quiet = TRUE)
demo_dir <- file.path("inst", "extdata", "hla_a_lollipop")
stopifnot(file.exists(file.path(demo_dir, "hla_a_selected_proteins.fasta.gz")))
source(file.path(demo_dir, "load-hla-a-example.R"))
hla <- load_hla_a_example(demo_dir)
evidence <- hla_a_evidence(hla)
mutations <- ggexon::query_protein_mutations(hla, annotation = "hla_substitutions")

original <- as.character(Biostrings::readAAStringSet(file.path(
  demo_dir, "hla_a_selected_proteins.fasta.gz"
)))
ref <- original[evidence$alleles$reference]
stopifnot(length(original) == 5998L, evidence$protein_length == 341L,
          evidence$distinct_mature_proteins == 3188L, nrow(mutations) == 1212L,
          !anyDuplicated(evidence$alleles$allele),
          any(mutations$n_alternatives > 1L),
          all(tapply(evidence$states$allele_count, evidence$states$position, sum) == 5998L))
focus <- mutations[mutations$mature_position == 156L, ]
stopifnot(identical(focus$mutation, c("L156F", "L156M", "L156Q", "L156R", "L156W")),
          identical(focus$allele_count, c(2L, 1L, 1184L, 596L, 1043L)))
for (i in seq_len(nrow(mutations))) {
  row <- mutations[i, ]
  aa <- substring(original, row$mature_position + 24L, row$mature_position + 24L)
  stopifnot(row$allele_count == sum(aa == row$alt),
            row$ref == substring(ref, row$mature_position + 24L, row$mature_position + 24L),
            identical(strsplit(row$example_alleles, ";", fixed = TRUE)[[1L]],
                      utils::head(evidence$alleles$allele[aa == row$alt], 3L)))
}
source(file.path(demo_dir, "plot-hla-a-example.R"))
plots <- plot_hla_a_example(hla)
prepared <- plots$focus_data
stopifnot(length(unique(prepared$mutations$lollipop_position)) == 1L,
          !anyDuplicated(prepared$mutations$lollipop_x),
          isTRUE(all.equal(prepared$mutations$lollipop_y,
                           log10(prepared$mutations$allele_count + 1))))
first_stems <- prepared$stems[!duplicated(prepared$stems$lollipop_curve_id), ]
stopifnot(all(first_stems$lollipop_x == evidence$focus_position))
for (name in c("overview", "focus")) {
  layout <- plots[[paste0(name, "_data")]]
  heads <- layout$mutations
  starts <- layout$stems[!duplicated(layout$stems$lollipop_curve_id), ]
  ends <- layout$stems[!duplicated(layout$stems$lollipop_curve_id, fromLast = TRUE), ]
  stopifnot(identical(starts$lollipop_x, as.numeric(heads$mature_position)),
            isTRUE(all.equal(ends$lollipop_x, heads$lollipop_x)),
            max(abs(heads$lollipop_x - heads$mature_position)) <=
              if (name == "overview") hla_a_plot_parameters$head_half_span + 1e-10 else
                hla_a_plot_parameters$focus_half_span + 1e-10,
            all(vapply(split(heads$lollipop_x, heads$mature_position), function(x) {
              !anyDuplicated(x)
            }, logical(1))))
  built <- ggplot2::ggplot_build(plots[[name]])
  stopifnot(length(built$data) >= 4L)
  points <- which(vapply(built$plot$layers, function(layer) {
    inherits(layer$geom, "GeomPoint")
  }, logical(1)))
  expected <- if (name == "overview") mutations$allele_count else focus$allele_count
  stopifnot(length(points) == 1L,
            nrow(built$data[[points]]) == length(expected),
            isTRUE(all.equal(built$data[[points]]$y, log10(expected + 1))))
}
cat(sprintf("Verified %d substitutions at %d residues; focus residue %d has %d alternatives.\n",
            nrow(mutations), length(unique(mutations$mature_position)),
            evidence$focus_position, nrow(prepared$mutations)))
