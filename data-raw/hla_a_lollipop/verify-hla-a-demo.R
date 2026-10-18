# Independent checks against original sequence records, run from repo root.
pkgload::load_all(quiet = TRUE)
demo_dir <- file.path("inst", "extdata", "hla_a_lollipop")
stopifnot(file.exists(file.path(demo_dir, "hla_a_selected_proteins.fasta.gz")))
source(file.path(demo_dir, "load-hla-a-example.R"))
hla <- load_hla_a_example(demo_dir)

original <- as.character(Biostrings::readAAStringSet(file.path(
  demo_dir, "hla_a_selected_proteins.fasta.gz"
)))
ref <- original[hla$alleles$reference]
stopifnot(length(original) == 5998L, hla$protein_length == 341L,
          hla$distinct_mature_proteins == 3188L, nrow(hla$mutations) == 1212L,
          !anyDuplicated(hla$alleles$allele),
          any(hla$mutations$n_alternatives > 1L),
          all(tapply(hla$states$allele_count, hla$states$position, sum) == 5998L))
focus <- hla$mutations[hla$mutations$position == 156L, ]
stopifnot(identical(focus$mutation, c("L156F", "L156M", "L156Q", "L156R", "L156W")),
          identical(focus$allele_count, c(2L, 1L, 1184L, 596L, 1043L)))
for (i in seq_len(nrow(hla$mutations))) {
  row <- hla$mutations[i, ]
  aa <- substring(original, row$position + 24L, row$position + 24L)
  stopifnot(row$allele_count == sum(aa == row$alt),
            row$ref == substring(ref, row$position + 24L, row$position + 24L),
            identical(strsplit(row$example_alleles, ";", fixed = TRUE)[[1L]],
                      utils::head(hla$alleles$allele[aa == row$alt], 3L)))
}
source(file.path(demo_dir, "plot-hla-a-example.R"))
plots <- plot_hla_a_example(hla)
prepared <- plots$focus_data
stopifnot(length(unique(prepared$mutations$lollipop_position)) == 1L,
          !anyDuplicated(prepared$mutations$lollipop_x),
          isTRUE(all.equal(prepared$mutations$lollipop_y,
                           log10(prepared$mutations$allele_count + 1))))
first_stems <- prepared$stems[!duplicated(prepared$stems$lollipop_curve_id), ]
stopifnot(all(first_stems$lollipop_x == hla$focus_position))
for (name in c("overview", "focus")) {
  layout <- plots[[paste0(name, "_data")]]
  heads <- layout$mutations
  starts <- layout$stems[!duplicated(layout$stems$lollipop_curve_id), ]
  ends <- layout$stems[!duplicated(layout$stems$lollipop_curve_id, fromLast = TRUE), ]
  stopifnot(identical(starts$lollipop_x, as.numeric(heads$position)),
            isTRUE(all.equal(ends$lollipop_x, heads$lollipop_x)),
            max(abs(heads$lollipop_x - heads$position)) <=
              if (name == "overview") hla_a_plot_parameters$head_half_span + 1e-10 else
                hla_a_plot_parameters$focus_half_span + 1e-10,
            all(vapply(split(heads$lollipop_x, heads$position), function(x) {
              !anyDuplicated(x)
            }, logical(1))))
  built <- ggplot2::ggplot_build(plots[[name]])
  stopifnot(length(built$data) >= 4L)
  points <- which(vapply(built$plot$layers, function(layer) {
    inherits(layer$geom, "GeomPoint")
  }, logical(1)))
  expected <- if (name == "overview") hla$mutations$allele_count else focus$allele_count
  stopifnot(length(points) == 1L,
            nrow(built$data[[points]]) == length(expected),
            isTRUE(all.equal(built$data[[points]]$y, log10(expected + 1))))
}
cat(sprintf("Verified %d substitutions at %d residues; focus residue %d has %d alternatives.\n",
            nrow(hla$mutations), length(unique(hla$mutations$position)),
            hla$focus_position, nrow(prepared$mutations)))
