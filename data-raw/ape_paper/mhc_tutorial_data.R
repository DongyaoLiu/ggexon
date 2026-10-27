# Repository example helpers, not exported package functions.
# Load with source() after library(ggexon), from the repository root.
read_mhc_data <- function(root = '.') {
  root <- normalizePath(root, mustWork = TRUE)
  base <- file.path(root, 'data-raw/ape_paper/results/mhc_continuous')
  compact <- file.path(root, 'data-raw/ape_paper/results/mhc_compact')
  read <- function(path) read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  # Add the two direct bridges to the complete alignment collection before
  # selecting individuals. The existing gorilla h2/h1 alignment is not duplicated.
  links <- read(file.path(base, 'links.tsv'))
  bridges <- read(file.path(compact, 'links.tsv'))
  original_pairs <- paste(links$tspecies, links$qspecies, sep = '__')
  bridge_pairs <- paste(bridges$tspecies, bridges$qspecies, sep = '__')
  links <- rbind(links, bridges[!bridge_pairs %in% original_pairs, , drop = FALSE])
  windows <- read(file.path(base, 'windows.tsv'))
  sp <- SynSpecies(name = 'complete_MHC', metadata = list(source = base))
  for (id in windows$track) {
    ind <- SynIndividual(id = id, annotation_format = 'gff',
                         annotation_file = file.path(base, 'figures', paste0(id, '.gff3')))
    sp <- add_individual(sp, load_annotation(ind))
  }
  for (nm in unique(paste(links$tspecies, links$qspecies, sep = '__'))) {
    z <- links[paste(links$tspecies, links$qspecies, sep = '__') == nm, , drop = FALSE]
    # Match the approved renderer's normalized display endpoints exactly.
    z$tstart <- z$tstart - 1; z$qstart <- z$qstart - 1
    paf <- file.path(base, z$source_paf[1])
    if (!file.exists(paf)) paf <- file.path(compact, z$source_paf[1])
    sp <- add_pairwise_alignment(sp, SynPairAlignment(
      nm, target_individual = z$tspecies[1], query_individual = z$qspecies[1],
      file = paf, data = z))
  }
  list(
    species = sp, windows = windows, links = links,
    models = read(file.path(base, 'gene_models_with_recovered_context/models.tsv')),
    exons = read(file.path(base, 'gene_models_with_recovered_context/exons.tsv')),
    mhc = read(file.path(root, 'data-raw/ape_paper/results/mhc_full/genes.tsv')),
    regions = read(file.path(base, 'discordant_regions.tsv')),
    max_bp = max(windows$length), all_tracks = windows$track, root = root
  )
}

subset_mhc_data <- function(x, tracks) {
  if (!is.character(tracks) || !length(tracks) || anyNA(tracks) ||
      anyDuplicated(tracks) || !all(tracks %in% individual_names(x$species))) {
    stop('Supply distinct track IDs present in the full MHC object.', call. = FALSE)
  }
  adjacent <- if (length(tracks) > 1L) paste(head(tracks, -1L), tail(tracks, -1L), sep = '__') else character()
  missing <- setdiff(adjacent, names(pairwise_alignments(x$species)))
  if (length(missing)) {
    stop('Add direct alignments to the full object before subsetting: ',
         paste(missing, collapse = ', '), call. = FALSE)
  }
  # Copy first: the complete object and its attached annotations stay intact.
  out <- x
  out$windows <- x$windows[match(tracks, x$windows$track), , drop = FALSE]
  for (nm in c('models', 'exons', 'mhc', 'regions')) {
    out[[nm]] <- x[[nm]][x[[nm]]$track %in% tracks, , drop = FALSE]
  }
  out$links <- x$links[paste(x$links$tspecies, x$links$qspecies, sep = '__') %in% adjacent, , drop = FALSE]
  out$species <- SynSpecies(name = 'selected_MHC', metadata = x$species@metadata)
  for (id in tracks) out$species <- add_individual(out$species, individuals(x$species)[[id]])
  for (nm in adjacent) out$species <- add_pairwise_alignment(out$species, pairwise_alignments(x$species)[[nm]])
  out$species <- store_chain_layout(out$species, free = list(x = TRUE, y = TRUE))
  for (id in tracks) {
    out$species <- set_panel_xlim(out$species, individual = id,
                                 xlim = c(0, out$max_bp), xlim_chr = 'MHC')
  }
  out
}

render_mhc_data <- function(x, output_dir = 'data-raw/ape_paper/results/mhc_compact/figures') {
  if (!identical(normalizePath('.', mustWork = TRUE), x$root)) {
    stop('Run the renderer from the repository root used by read_mhc_data().', call. = FALSE)
  }
  # Ensure only direct links for the chosen chain are plotted, even for all tracks.
  x <- subset_mhc_data(x, x$windows$track)
  env <- new.env(parent = globalenv())
  env$mhc_dataset <- x
  # Prepare grid text metrics on the same base PDF device used by Rscript,
  # independently of an already open RStudio, PNG or Cairo display device.
  metric_file <- tempfile(fileext = '.pdf')
  on.exit(unlink(metric_file), add = TRUE)
  withr::with_pdf(metric_file, width = 7, height = 7, code = {
    withr::with_envvar(c(GGEXON_MHC_OUTPUT = output_dir,
                         GGEXON_MHC_TRACKS = paste(x$windows$track, collapse = ',')),
      sys.source(file.path(x$root, 'data-raw/ape_paper/render_mhc_continuous.R'), envir = env))
  })
  invisible(list(plot = env$p, gtable = env$tab,
                 pdf = file.path(output_dir, 'mhc_continuous.pdf'),
                 png = file.path(output_dir, 'mhc_continuous.png')))
}
