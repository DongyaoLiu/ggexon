read_hox_demo_table <- function(demo_dir, file) {
  read.delim(
    file.path(demo_dir, file),
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

test_that("bundled HOX expansion data preserve sources and matrix states", {
  demo_dir <- system.file("extdata", "hox_cluster_expansion", package = "ggexon")
  expect_true(dir.exists(demo_dir))

  expected_files <- c(
    "hox_genes.tsv",
    "hox_cds.tsv",
    "hox_clusters.tsv",
    "hox_species.tsv",
    "hox_annotation_gaps.tsv",
    "hox_expected_complement.tsv",
    "hox_slot_states.tsv",
    "curated_transcript_exclusions.tsv",
    "hox_xref_conflicts.tsv",
    "manual_hox_mapping.tsv",
    "amphioxus_hox_mapping.tsv"
  )
  expect_true(all(file.exists(file.path(demo_dir, expected_files))))

  genes <- read_hox_demo_table(demo_dir, "hox_genes.tsv")
  panels <- read_hox_demo_table(demo_dir, "hox_clusters.tsv")
  species <- read_hox_demo_table(demo_dir, "hox_species.tsv")
  gaps <- read_hox_demo_table(demo_dir, "hox_annotation_gaps.tsv")
  expected <- read_hox_demo_table(demo_dir, "hox_expected_complement.tsv")
  slot_states <- read_hox_demo_table(demo_dir, "hox_slot_states.tsv")

  expect_setequal(
    species$species,
    c("human", "mouse", "chicken", "gar", "zebrafish", "amphioxus")
  )
  expect_equal(species$release[species$species != "amphioxus"], rep(116L, 5L))
  expect_equal(species$release[species$species == "amphioxus"], 63L)
  expect_equal(species$assembly[species$species == "human"], "GRCh38")
  expect_equal(species$assembly[species$species == "mouse"], "GRCm39")
  expect_equal(species$assembly[species$species == "amphioxus"], "BraLan2")
  expect_true(all(grepl("^https://", species$source_url)))
  expect_true(all(grepl("^[[:xdigit:]]{64}$", species$sha256)))
  expect_true(all(!is.na(as.Date(species$retrieved_on))))

  row_order <- c(
    "human", "mouse", "chicken", "gar",
    "zebrafish_a", "zebrafish_b", "amphioxus"
  )
  column_order <- c("A", "B", "C", "D")
  expect_equal(nrow(panels), 28L)
  expect_setequal(panels$matrix_row, row_order)
  expect_setequal(panels$matrix_column, column_order)
  expect_false(anyDuplicated(panels[c("matrix_row", "matrix_column")]) > 0L)
  expect_equal(sum(panels$cell_status == "retained"), 24L)
  expect_equal(sum(panels$cell_status == "structural_blank"), 3L)
  expect_equal(sum(panels$cell_status == "cluster_not_retained"), 1L)

  not_retained <- panels[panels$cell_status == "cluster_not_retained", , drop = FALSE]
  expect_equal(not_retained$matrix_row, "zebrafish_b")
  expect_equal(not_retained$matrix_column, "D")
  expect_equal(not_retained$cluster, "DB")

  structural <- panels[panels$cell_status == "structural_blank", , drop = FALSE]
  expect_true(all(
    structural$matrix_row == "amphioxus" &
      structural$matrix_column %in% c("B", "C", "D")
  ))

  expect_equal(
    as.integer(table(genes$species)[c(
      "human", "mouse", "chicken", "gar", "zebrafish", "amphioxus"
    )]),
    c(39L, 39L, 35L, 31L, 49L, 14L)
  )
  expect_equal(nrow(genes), 207L)
  expect_equal(nrow(gaps), 17L)
  expect_equal(
    as.integer(table(gaps$species)[c("chicken", "gar", "amphioxus")]),
    c(4L, 12L, 1L)
  )
  expect_equal(nrow(expected), nrow(genes) + nrow(gaps))
  expect_equal(nrow(expected), 224L)
  expect_equal(nrow(slot_states), 24L * 15L)
  expect_false(anyDuplicated(
    slot_states[c("matrix_row", "matrix_column", "slot")]
  ) > 0L)
  expect_equal(sum(slot_states$slot_state == "plotted_gene_model"), nrow(genes))
  expect_equal(
    sum(slot_states$slot_state == "annotation_gap_expected_functional_member"),
    nrow(gaps)
  )
  expect_false(anyDuplicated(genes[c("species", "cluster", "hox_number")]) > 0L)
  expect_true(all(genes$slot == paste0("Hox", genes$hox_number)))
  expect_true(all(genes$hox_number %in% 1:15))

  amphioxus_genes <- genes[genes$species == "amphioxus", , drop = FALSE]
  expect_true(all(amphioxus_genes$matrix_column == "A"))
  expect_true(all(amphioxus_genes$cluster_column == "A"))
  expect_true(all(amphioxus_genes$cluster_family == "ancestral"))
  expect_true(all(amphioxus_genes$cluster == "ancestral"))
  expect_true(all(amphioxus_genes$strand == "-"))
  expect_true(all(gaps$matrix_column[gaps$species == "amphioxus"] == "A"))
  expect_true(all(expected$matrix_column[expected$species == "amphioxus"] == "A"))
  expect_true(all(slot_states$matrix_column[slot_states$species == "amphioxus"] == "A"))

  amphioxus_panel <- panels[
    panels$matrix_row == "amphioxus" & panels$matrix_column == "A",
    ,
    drop = FALSE
  ]
  expect_equal(amphioxus_panel$cluster, "ancestral")
  expect_equal(amphioxus_panel$cell_status, "retained")
  expect_false(amphioxus_panel$display_reverse)

  zebrafish_clusters <- sort(unique(genes$cluster[genes$species == "zebrafish"]))
  expect_setequal(zebrafish_clusters, c("AA", "AB", "BA", "BB", "CA", "CB", "DA"))
  expect_false("DB" %in% zebrafish_clusters)

  expect_equal(
    sort(gaps$hox_number[gaps$species == "chicken"]),
    c(1L, 4L, 5L, 6L)
  )
  expect_equal(gaps$hox_number[gaps$species == "amphioxus"], 13L)
  expect_true(all(gaps$annotation_gap_class == "source_annotation_gap_no_safe_gene_model"))
  expect_true(any(
    grepl("^unsafe_", gaps$candidate_model_assessment),
    na.rm = TRUE
  ))

  gar_hox3 <- genes[
    genes$species == "gar" & genes$cluster %in% c("A", "B") &
      genes$hox_number == 3L,
    ,
    drop = FALSE
  ]
  gar_hox3 <- gar_hox3[match(c("A", "B"), gar_hox3$cluster), , drop = FALSE]
  expect_equal(
    gar_hox3$transcript_id,
    c("ENSLOCT00000014553", "ENSLOCT00000016597")
  )
  expect_true(all(gar_hox3$curated_transcript_exclusion_applied))
  expect_true(all(gar_hox3$unsafe_merged_transcript_exclusion_applied))
  expect_true(all(grepl("exclude_curated_unsafe_merged", gar_hox3$transcript_selection_rule)))
  expect_true(gar_hox3$xref_conflict_flag[gar_hox3$cluster == "A"])
  expect_match(
    gar_hox3$xref_conflict_note[gar_hox3$cluster == "A"],
    "UniProt/TrEMBL"
  )

  expect_setequal(
    gaps$hox_number[
      gaps$species == "gar" &
        !is.na(gaps$candidate_transcript_id) &
        gaps$candidate_transcript_id == "ENSLOCT00000014539"
    ],
    c(2L, 4L)
  )
  expect_setequal(
    gaps$hox_number[
      gaps$species == "gar" &
        !is.na(gaps$candidate_transcript_id) &
        gaps$candidate_transcript_id == "ENSLOCT00000016594"
    ],
    c(2L, 4L)
  )
  expect_setequal(
    gaps$hox_number[
      gaps$species == "gar" &
        !is.na(gaps$candidate_transcript_id) &
        gaps$candidate_transcript_id == "ENSLOCT00000007673"
    ],
    c(6L, 9L)
  )
  expect_false(any(
    genes$species == "gar" & genes$cluster == "C" & genes$hox_number == 6L
  ))
  gar_hox14 <- slot_states[
    slot_states$species == "gar" & slot_states$hox_number == 14L &
      slot_states$cluster %in% c("A", "D"),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(gar_hox14), 2L)
  expect_equal(
    gar_hox14$slot_state[match(c("A", "D"), gar_hox14$cluster)],
    c("lineage_absence", "recognizable_pseudogene_not_plotted")
  )
  expect_false(any(gar_hox14$expected_functional_member))
})

test_that("bundled HOX anchors and compact annotations round trip", {
  demo_dir <- system.file("extdata", "hox_cluster_expansion", package = "ggexon")
  genes <- read_hox_demo_table(demo_dir, "hox_genes.tsv")
  cds <- read_hox_demo_table(demo_dir, "hox_cds.tsv")

  required_gene_fields <- c(
    "gene_stable_id_version", "transcript_stable_id_version",
    "transcript_span_bp", "genomic_x_start", "genomic_x_middle",
    "genomic_x_end", "initiation_anchor_source", "stop_anchor_source",
    "initiation_anchor_fallback", "stop_anchor_fallback", "mapping_method",
    "curated_transcript_exclusion_applied",
    "unsafe_merged_transcript_exclusion_applied", "excluded_transcript_ids",
    "xref_conflict_flag", "xref_conflict_note"
  )
  expect_true(all(required_gene_fields %in% names(genes)))
  expect_true(all(c("transcript_id", "cds_rank", "start", "end") %in% names(cds)))
  expect_true(all(genes$strand %in% c("+", "-")))
  expect_true(all(genes$transcript_span_bp > 0L))
  expect_equal(
    genes$genomic_x_middle,
    (genes$genomic_x_start + genes$genomic_x_end) / 2
  )
  expect_equal(genes$x, genes$genomic_x_middle)
  expect_true(all(
    genes$genomic_x_start >= genes$transcript_start &
      genes$genomic_x_start <= genes$transcript_end
  ))
  expect_true(all(
    genes$genomic_x_end >= genes$transcript_start &
      genes$genomic_x_end <= genes$transcript_end
  ))
  expect_equal(
    genes$any_anchor_fallback,
    genes$initiation_anchor_fallback | genes$stop_anchor_fallback
  )
  expect_true(all(
    genes$initiation_anchor_fallback ==
      (genes$initiation_anchor_source != "explicit_start_codon")
  ))
  expect_true(all(
    genes$stop_anchor_fallback ==
      (genes$stop_anchor_source != "explicit_stop_codon")
  ))
  expect_true(all(grepl("greatest_genomic_span", genes$transcript_selection_rule)))

  selected_cds_counts <- table(cds$transcript_id)
  expect_equal(
    as.integer(selected_cds_counts[genes$transcript_id]),
    genes$cds_piece_count
  )

  annotation_files <- file.path(
    demo_dir,
    "annotations",
    paste0(c("human", "mouse", "chicken", "gar", "zebrafish", "amphioxus"), ".gff3")
  )
  expect_true(all(file.exists(annotation_files)))
  mrna_lines <- unlist(lapply(annotation_files, function(path) {
    lines <- readLines(path, warn = FALSE)
    lines[grepl("\\tmRNA\\t", lines, fixed = FALSE)]
  }), use.names = FALSE)
  expect_equal(length(mrna_lines), nrow(genes))
  expect_true(all(grepl("(?:^|;)slot=Hox[0-9]+(?:;|$)", mrna_lines, perl = TRUE)))
  expect_true(all(grepl("(?:^|;)mapping_method=", mrna_lines, perl = TRUE)))

  human <- SynIndividual(
    annotation_file = file.path(demo_dir, "annotations", "human.gff3"),
    genome_file = genome_waiver(),
    id = "human"
  )
  compiled <- syn_to_genebox_df(human, anchor = "middle", na.rm = TRUE)
  human_genes <- genes[genes$species == "human", , drop = FALSE]
  expect_equal(nrow(compiled), nrow(human_genes))
  expect_setequal(sub("^gene:", "", compiled$gene_key), human_genes$gene_id)
  expect_setequal(compiled$slot, human_genes$slot)
})

test_that("HOX tutorial matrix renders 28 equal exact-slot panels without links", {
  demo_dir <- system.file("extdata", "hox_cluster_expansion", package = "ggexon")
  genes <- read_hox_demo_table(demo_dir, "hox_genes.tsv")
  panels <- read_hox_demo_table(demo_dir, "hox_clusters.tsv")
  row_order <- c(
    "human", "mouse", "chicken", "gar",
    "zebrafish_a", "zebrafish_b", "amphioxus"
  )
  column_order <- c("A", "B", "C", "D")
  slot_order <- paste0("Hox", 15:1)

  genes$species_row <- factor(genes$species_row, levels = row_order)
  genes$cluster_column <- factor(genes$cluster_column, levels = column_order)
  genes$track <- paste(genes$species_row, genes$cluster_column, sep = "::")
  genes$y <- 1
  panels$species_row <- factor(panels$matrix_row, levels = row_order)
  panels$cluster_column <- factor(panels$matrix_column, levels = column_order)

  plot <- ggexon() +
    geom_genebox(data = genes) +
    ggplot2::geom_blank(
      data = panels,
      ggplot2::aes(x = 8, y = 1),
      inherit.aes = FALSE
    ) +
    strip_scale_x(slot_order = slot_order, guide = "none") +
    ggplot2::facet_grid(
      rows = ggplot2::vars(species_row),
      cols = ggplot2::vars(cluster_column),
      scales = "fixed",
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0))

  built <- ggexon_build(plot)
  expect_equal(nrow(built@layout$layout), 28L)
  expect_equal(built@layout$strip_scale_x_limits, c(0.5, 15.5))
  expect_true(all(vapply(
    built@layout$panel_params,
    function(panel) isTRUE(all.equal(panel$x.range, c(0.5, 15.5))),
    logical(1)
  )))
  expect_equal(built@data[[1L]]$x, match(built@data[[1L]]$slot, slot_order))
  expect_equal(built@data[[1L]]$genomic_x, genes$x)

  amphioxus_built <- built@data[[1L]][
    built@data[[1L]]$species == "amphioxus",
    ,
    drop = FALSE
  ]
  expect_equal(amphioxus_built$x, match(amphioxus_built$slot, slot_order))
  expect_true(all(amphioxus_built$strand == "-"))
  expect_true(all(amphioxus_built$strip_x_direction == 1))

  table <- ggplot2::ggplotGrob(plot)
  panel_layout <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
  panel_columns <- sort(unique(panel_layout$l))
  panel_rows <- sort(unique(panel_layout$t))
  expect_equal(length(panel_columns), 4L)
  expect_equal(length(panel_rows), 7L)
  expect_length(unique(as.character(table$widths[panel_columns])), 1L)

  vignette_candidates <- c(
    testthat::test_path("..", "..", "vignettes", "hox-cluster-expansion-demo.Rmd"),
    system.file("doc", "hox-cluster-expansion-demo.Rmd", package = "ggexon")
  )
  vignette_path <- vignette_candidates[file.exists(vignette_candidates)][[1L]]
  vignette_source <- paste(readLines(vignette_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("geom_nuclink\\s*\\(", vignette_source))
  expect_false(grepl("geom_synteny_link\\s*\\(", vignette_source))
  expect_match(vignette_source, "displayed under HOXA for layout only", fixed = TRUE)
  expect_match(
    vignette_source,
    "Box position therefore represents the curated",
    fixed = TRUE
  )
})
