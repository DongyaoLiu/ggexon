test_that("bundled Ensembl 115 HOXA demo data are internally consistent", {
  demo_dir <- system.file("extdata", "hoxa_ensembl115", package = "ggexon")
  expect_true(dir.exists(demo_dir))

  genes <- read.delim(file.path(demo_dir, "hoxa_genes.tsv"), check.names = FALSE)
  links <- read.delim(file.path(demo_dir, "hoxa_links.tsv"), check.names = FALSE)
  species <- read.delim(file.path(demo_dir, "hoxa_species.tsv"), check.names = FALSE)
  homology <- read.delim(file.path(demo_dir, "hoxa_homology.tsv"), check.names = FALSE)

  expect_setequal(species$species, c("human", "macaque", "mouse", "chicken", "anole"))
  expect_true(all(c(
    "species", "source_url", "source_seqname", "hoxa_gene_count", "hoxa_groups"
  ) %in% names(species)))
  expect_true(all(c(
    "track", "xmin", "xmax", "strand", "hox_group", "genomic_start", "genomic_end"
  ) %in% names(genes)))
  expect_true("reference_gene" %in% names(genes))
  expect_true(all(c(
    "track", "tspecies", "tstart", "tend", "qspecies", "qstart", "qend", "hox_group"
  ) %in% names(links)))
  expect_true(all(c(
    "reference_species", "query_species", "query_gene", "reference_gene", "hox_group"
  ) %in% names(homology)))

  expect_true(all(genes$xmin < genes$xmax))
  expect_identical(genes$reference_gene, genes$hox_group)
  expect_true(all(links$tstart < links$tend))
  expect_true(all(links$qstart < links$qend))
  expect_true(all(links$tspecies %in% species$species))
  expect_true(all(links$qspecies %in% species$species))
  expect_true(all(homology$reference_species == "human"))
  expect_setequal(homology$query_species, setdiff(species$species, "human"))
  expect_equal(nrow(homology), sum(genes$species != "human" & genes$hox_group %in% genes$hox_group[genes$species == "human"]))

  linked_groups <- paste(links$tspecies, links$qspecies, links$hox_group, sep = "::")
  expect_identical(anyDuplicated(linked_groups), 0L)
  homology_keys <- paste(homology$query_species, homology$query_gene, sep = "::")
  expect_identical(anyDuplicated(homology_keys), 0L)
  expect_true(all(file.exists(file.path(demo_dir, "annotations", paste0(species$species, ".gff3")))))
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

  species_tracks <- species$species
  link_tracks <- paste0("link_", head(species_tracks, -1), "_", tail(species_tracks, -1))
  track_levels <- as.vector(rbind(head(species_tracks, -1), link_tracks))
  track_levels <- c(track_levels, tail(species_tracks, 1))
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
      facet_genomics(
        ggplot2::vars(track),
        scales = "free_x",
        ncol = 1,
        link_panel_height = 0.32,
        link_axis = "none",
        link_strip = "blank"
      )
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

test_that("HOXA demo homology can populate SynSpecies gene tags", {
  demo_dir <- system.file("extdata", "hoxa_ensembl115", package = "ggexon")
  species <- read.delim(file.path(demo_dir, "hoxa_species.tsv"), check.names = FALSE)
  homology <- read.delim(file.path(demo_dir, "hoxa_homology.tsv"), check.names = FALSE)
  annotation_dir <- file.path(demo_dir, "annotations")

  sp <- SynSpecies(name = "HOXA")
  for (species_id in species$species) {
    sp <- add_individual(
      sp,
      SynIndividual(
        annotation_file = file.path(annotation_dir, paste0(species_id, ".gff3")),
        genome_file = genome_waiver(),
        id = species_id,
        annotation_format = "gff"
      )
    )
  }

  for (query_species in unique(homology$query_species)) {
    rows <- homology[homology$query_species == query_species, , drop = FALSE]
    sp <- add_homology_annotation(
      sp,
      HomologyAnnotation(
        name = paste0(query_species, "_to_human"),
        reference_species = "human",
        query_species = query_species,
        homology_table = rows[c("query_gene", "reference_gene", "hox_group")]
      )
    )
  }

  tags <- syn_to_genetag_df(sp, species = c("human", "mouse"), feature_type = "gene")
  human_tags <- tags[tags$track == "human", , drop = FALSE]
  mouse_tags <- tags[tags$track == "mouse", , drop = FALSE]

  expect_true(all(human_tags$is_homology_reference_track))
  expect_true(all(mouse_tags$homology_hit))
  expect_setequal(mouse_tags$reference_gene, homology$reference_gene[homology$query_species == "mouse"])

  built <- ggexon_build(
    ggexon() +
      geom_genetag(data = tags, mapping = ggplot2::aes(fill = reference_gene)) +
      strip_scale_x(reference_track = "human", gene_order = "reference", guide = "none") +
      facet_genomics(ggplot2::vars(track), scales = "free_x")
  )
  expect_true("homology_anchor" %in% names(built@data[[1L]]))
  expect_true(any(built@data[[1L]]$homology_anchor))
})
