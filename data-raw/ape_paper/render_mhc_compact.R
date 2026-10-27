#!/usr/bin/env Rscript
# Run from the repository root. The full object is loaded before any selection.
suppressPackageStartupMessages(pkgload::load_all('.', quiet = TRUE))
source('data-raw/ape_paper/mhc_tutorial_data.R')
mhc_all <- read_mhc_data()
mhc_small <- subset_mhc_data(
  mhc_all, tracks = c('SSYh1', 'GGOh2', 'GGOh1', 'HSAh1')
)
result <- render_mhc_data(mhc_small)
for (ext in c('pdf', 'png')) {
  stopifnot(file.copy(
    result[[ext]], file.path('data-raw/ape_paper/results/mhc_compact',
                            paste0('mhc_three_species.', ext)), overwrite = TRUE))
}
