test_that("bundled Ensembl 115 HOXA demo data are internally consistent", {
  demo_dir <- system.file("extdata", "hoxa_ensembl115", package = "ggexon")
  expect_true(dir.exists(demo_dir))

  genes <- read.delim(file.path(demo_dir, "hoxa_genes.tsv"), check.names = FALSE)
  links <- read.delim(file.path(demo_dir, "hoxa_links.tsv"), check.names = FALSE)
  species <- read.delim(file.path(demo_dir, "hoxa_species.tsv"), check.names = FALSE)

  expect_setequal(species$species, c("human", "macaque", "mouse", "chicken", "anole"))
  expect_true(all(c(
    "species", "source_url", "source_seqname", "hoxa_gene_count", "hoxa_groups"
  ) %in% names(species)))
  expect_true(all(c(
    "track", "xmin", "xmax", "strand", "hox_group", "genomic_start", "genomic_end"
  ) %in% names(genes)))
  expect_true(all(c(
    "track", "tspecies", "tstart", "tend", "qspecies", "qstart", "qend", "hox_group"
  ) %in% names(links)))

  expect_true(all(genes$xmin < genes$xmax))
  expect_true(all(links$tstart < links$tend))
  expect_true(all(links$qstart < links$qend))
  expect_true(all(links$tspecies %in% species$species))
  expect_true(all(links$qspecies %in% species$species))

  linked_groups <- paste(links$tspecies, links$qspecies, links$hox_group, sep = "::")
  expect_identical(anyDuplicated(linked_groups), 0L)
  expect_identical(species$source_seqname[species$species == "anole"], "GL343275.1")
  expect_match(
    species$source_note[species$species == "anole"],
    "full GTF",
    fixed = TRUE
  )
})

test_that("HOXA demo data render with geom_synteny_link fill mappings", {
  demo_dir <- system.file("extdata", "hoxa_ensembl115", package = "ggexon")
  genes <- read.delim(file.path(demo_dir, "hoxa_genes.tsv"), check.names = FALSE)
  links <- read.delim(file.path(demo_dir, "hoxa_links.tsv"), check.names = FALSE)
  species <- read.delim(file.path(demo_dir, "hoxa_species.tsv"), check.names = FALSE)

  track_levels <- as.vector(rbind(
    species$species[-nrow(species)],
    paste0("link_", species$species[-nrow(species)], "_", species$species[-1])
  ))
  track_levels <- c(track_levels, species$species[nrow(species)])
  genes$track <- factor(genes$track, levels = track_levels)
  links$track <- factor(links$track, levels = track_levels)

  built <- ggexon_build(
    ggexon() +
      geom_synteny_link(
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
          fill = hox_group
        ),
        inherit.aes = FALSE
      ) +
      geom_genetag(
        data = genes,
        mapping = ggplot2::aes(fill = hox_group),
        gene_layout = "nested",
        show_label = FALSE
      ) +
      facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1)
  )

  expect_equal(nrow(built@layout$layout), length(track_levels))
  expect_true(all(c("t_panel", "q_panel") %in% names(built@layout$layout)))
  expect_true(nrow(built@data[[1L]]) > 0L)
  expect_true(all(built@data[[1L]]$x_variable %in% c("tstart", "tend", "qstart", "qend")))
  expect_true("fill" %in% names(built@data[[1L]]))
  gene_data <- built@data[[2L]]
  chicken_lanes <- gene_data$gene_lane[as.character(gene_data$track) == "chicken"]
  expect_true(max(chicken_lanes, na.rm = TRUE) > 1L)
})
