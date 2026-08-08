test_that("bundled yeast cox1 demo data are internally consistent", {
  demo_dir <- system.file("extdata", "cox1_yeast_pairwise", package = "ggexon")
  expect_true(dir.exists(demo_dir))

  species <- read.delim(file.path(demo_dir, "cox1_species.tsv"), check.names = FALSE)
  transcripts <- read.delim(file.path(demo_dir, "cox1_transcripts.tsv"), check.names = FALSE)
  exons <- read.delim(file.path(demo_dir, "cox1_plot_exons.tsv"), check.names = FALSE)
  introns <- read.delim(file.path(demo_dir, "cox1_introns.tsv"), check.names = FALSE)
  links <- read.delim(file.path(demo_dir, "cox1_nuclinks_lastz.tsv"), check.names = FALSE)
  homology <- read.delim(file.path(demo_dir, "cox1_exon_homology_ranked.tsv"), check.names = FALSE)

  expect_setequal(species$species, c("fission_yeast", "budding_yeast"))
  expect_setequal(species$gene_id, c("SPMIT.01", "Q0045"))
  expect_equal(transcripts$exon_count[transcripts$species == "fission_yeast"], 3)
  expect_equal(transcripts$exon_count[transcripts$species == "budding_yeast"], 8)
  expect_equal(transcripts$intron_count[transcripts$species == "fission_yeast"], 2)
  expect_equal(transcripts$intron_count[transcripts$species == "budding_yeast"], 7)

  expect_equal(nrow(exons[exons$species == "fission_yeast", , drop = FALSE]), 3)
  expect_equal(nrow(exons[exons$species == "budding_yeast", , drop = FALSE]), 8)
  expect_equal(nrow(introns[introns$species == "fission_yeast", , drop = FALSE]), 2)
  expect_equal(nrow(introns[introns$species == "budding_yeast", , drop = FALSE]), 7)
  expect_true(all(exons$xmin < exons$xmax))

  expect_equal(nrow(links), 4)
  expect_true(all(links$tspecies == "fission_yeast"))
  expect_true(all(links$qspecies == "budding_yeast"))
  expect_true(all(links$tstart < links$tend))
  expect_true(all(links$qstart < links$qend))
  expect_true(all(links$identity >= 50))
  expect_true(all(links$alignment_length >= 40))

  expect_equal(nrow(homology), 7)
  expect_equal(sum(homology$reciprocal_best), 3)
  expect_true(all(file.exists(file.path(demo_dir, "annotations", paste0(species$species, ".gff3")))))
})

test_that("yeast cox1 provenance is portable and pins downloaded inputs", {
  demo_dir <- system.file("extdata", "cox1_yeast_pairwise", package = "ggexon")
  provenance <- read.delim(
    file.path(demo_dir, "cox1_provenance.tsv"),
    check.names = FALSE,
    colClasses = "character"
  )

  checksum_keys <- paste0(
    c(
      "pombase_gff3",
      "pombase_sequence",
      "pombase_orthologs",
      "sgd_features",
      "sgd_mitochondrial_sequence"
    ),
    "_md5"
  )
  expect_setequal(intersect(provenance$key, checksum_keys), checksum_keys)
  checksums <- provenance$value[match(checksum_keys, provenance$key)]
  expect_true(all(grepl("^[0-9a-f]{32}$", checksums)))

  command_values <- provenance$value[
    provenance$key %in% c("lastz_binary", "lastz_command", "rscript")
  ]
  expect_false(any(grepl("(^|[[:space:]])(/Users/|/opt/|/Library/)", command_values)))
  expect_false(any(grepl("^[A-Za-z]:[/\\\\]", command_values)))
})

test_that("yeast cox1 demo data render with geom_nuclink", {
  demo_dir <- system.file("extdata", "cox1_yeast_pairwise", package = "ggexon")
  exons <- read.delim(file.path(demo_dir, "cox1_plot_exons.tsv"), check.names = FALSE)
  links <- read.delim(file.path(demo_dir, "cox1_nuclinks_lastz.tsv"), check.names = FALSE)

  track_levels <- c("fission_yeast", "link_fission_budding", "budding_yeast")
  exons$track <- factor(exons$track, levels = track_levels)
  links$track <- factor(links$track, levels = track_levels)

  built <- ggexon_build(
    ggexon() +
      geom_nuclink(
        data = links,
        mapping = ggplot2::aes(
          tspecies = tspecies,
          tchr = tchr,
          tstart = tstart,
          tend = tend,
          qspecies = qspecies,
          qchr = qchr,
          qstart = qstart,
          qend = qend,
          strand = strand,
          group = group,
          fill = identity_bin
        ),
        inherit.aes = FALSE
      ) +
      geom_exon(
        data = exons,
        mapping = ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          transcripts = transcripts,
          strand = strand,
          track = track,
          type = type
        )
      ) +
      facet_genomics(
        ggplot2::vars(track),
        scales = "free_x",
        ncol = 1,
        link_panel_height = 0.38,
        link_axis = "none",
        link_strip = "blank"
      )
  )

  expect_equal(nrow(built@layout$layout), length(track_levels))
  expect_true(all(c("t_panel", "q_panel") %in% names(built@layout$layout)))
  expect_equal(nrow(built@data[[1L]]), nrow(links) * 4L)
  expect_true(all(built@data[[1L]]$x_variable %in% c("tstart", "tend", "qstart", "qend")))
})
