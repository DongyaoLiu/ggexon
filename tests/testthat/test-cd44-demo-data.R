test_that("bundled CD44 demo windows use strand-aware promoter and 3-prime flanks", {
  demo_dir <- system.file("extdata", "cd44_pairwise_ensembl116", package = "ggexon")
  expect_true(dir.exists(demo_dir))

  species <- read.delim(file.path(demo_dir, "cd44_species.tsv"), check.names = FALSE)
  links <- read.delim(file.path(demo_dir, "cd44_nuclinks_lastz.tsv"), check.names = FALSE)

  expect_setequal(species$species, c("human", "mouse"))
  expect_true(all(c(
    "gene_start", "gene_end", "gene_strand",
    "window_start", "window_end", "promoter_flank_bp", "three_prime_flank_bp",
    "promoter_start", "promoter_end", "three_prime_start", "three_prime_end"
  ) %in% names(species)))
  expect_equal(species$promoter_flank_bp, rep(20000L, nrow(species)))
  expect_equal(species$three_prime_flank_bp, rep(10000L, nrow(species)))

  human <- species[species$species == "human", , drop = FALSE]
  mouse <- species[species$species == "mouse", , drop = FALSE]

  expect_equal(human$gene_strand, 1L)
  expect_equal(human$window_start, human$gene_start - 20000L)
  expect_equal(human$window_end, human$gene_end + 10000L)
  expect_equal(human$promoter_start, human$window_start)
  expect_equal(human$promoter_end, human$gene_start - 1L)
  expect_equal(human$three_prime_start, human$gene_end + 1L)
  expect_equal(human$three_prime_end, human$window_end)

  expect_equal(mouse$gene_strand, -1L)
  expect_equal(mouse$window_start, mouse$gene_start - 10000L)
  expect_equal(mouse$window_end, mouse$gene_end + 20000L)
  expect_equal(mouse$promoter_start, mouse$gene_end + 1L)
  expect_equal(mouse$promoter_end, mouse$window_end)
  expect_equal(mouse$three_prime_start, mouse$window_start)
  expect_equal(mouse$three_prime_end, mouse$gene_start - 1L)

  expect_true(nrow(links) > 0L)
  expect_true(all(links$identity >= 50))
  expect_true(all(links$alignment_length >= 80))
})

test_that("bundled CD44 demo reports exon-split protein identity", {
  demo_dir <- system.file("extdata", "cd44_pairwise_ensembl116", package = "ggexon")
  expect_true(file.exists(file.path(demo_dir, "cd44_exon_peptides.tsv")))
  expect_true(file.exists(file.path(demo_dir, "cd44_unique_exon_peptides.tsv")))
  expect_true(file.exists(file.path(demo_dir, "cd44_exon_protein_identity.tsv")))

  selected <- read.delim(file.path(demo_dir, "cd44_selected_isoforms.tsv"), check.names = FALSE)
  peptides <- read.delim(file.path(demo_dir, "cd44_exon_peptides.tsv"), check.names = FALSE)
  protein_identity <- read.delim(file.path(demo_dir, "cd44_exon_protein_identity.tsv"), check.names = FALSE)

  expect_true(all(c(
    "translation_id", "protein_length", "peptide_length_aa", "peptide"
  ) %in% names(peptides)))
  expect_true(all(c(
    "human_peptide_length_aa", "mouse_peptide_length_aa",
    "protein_aligned_aa", "protein_identical_aa", "protein_gap_aa",
    "protein_identity"
  ) %in% names(protein_identity)))

  peptide_lengths <- aggregate(
    peptide_length_aa ~ species + transcript_id,
    peptides,
    sum
  )
  peptide_lengths <- merge(
    selected[, c("species", "transcript_id", "protein_length")],
    peptide_lengths,
    by = c("species", "transcript_id"),
    all.x = TRUE
  )
  expect_equal(peptide_lengths$peptide_length_aa, peptide_lengths$protein_length)

  reciprocal <- protein_identity[protein_identity$reciprocal_best, , drop = FALSE]
  expect_true(nrow(reciprocal) > 0L)
  expect_true(all(!is.na(reciprocal$protein_identity)))
  expect_true(all(reciprocal$protein_identity >= 0 & reciprocal$protein_identity <= 100))
  expect_true(all(reciprocal$human_peptide_length_aa > 0))
  expect_true(all(reciprocal$mouse_peptide_length_aa > 0))
})
