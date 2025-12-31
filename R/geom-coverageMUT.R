#' designed for plot coverage data.
#' detect there is imcompbility with scale_x_reverse


GeomCoverageMUT <- ggproto("GeomCoverageMUT", GeomRect,
                    required_aes = c("ymin", "xmin", "xmax","transcripts","strand", "track", "type"),
                    non_missing_aes = c("ymax", "linewidth", "shape"),
                    extra_params = c("exon_height", "na.rm", "x_translation", "layout", "annotation_type", "subset", "y_threshold",
                                     "x_threshold", "region_based_calling", "bigwig", "ref_chr",
                                     "bigwig_track_height", "bigwig_height_normalize", "track_max", "vcf_table"),
                    default_aes = aes(linewidth = 0, linejoin = "mitre", fill="grey80",
                                      colour = NULL,
                                      size = 15,
                                      linetype = 1,
                                      shape = 19,
                                      alpha = NA,
                                      stroke = 1
                    ),

                    setup_data = function(data, params){
                      if(is.null(params$annotation_type)){
                        cli_abort(c("annotation_type is required"))
                      }else if(params$annotation_type == "mRNA") {
                        print("gff file format detected")
                      }else if(params$annotation_type == "transcript"){
                        print("gtf file format detected" )
                      }else{
                        cli::cli_abort(c("Please offer gff or gtf format"))
                      }

                      #' extract the subset coordinates.
                      if (!is.null(params$subset)) {
                        #' filter base on the subset region.
                        start1 = int(params$subset[1])
                        end1 = int(params$subset[2])
                        data = data %>% filter(xmin >= start1, xmax <= end1)
                      }else{
                        print("didn't constrain the visualization region")
                      }

                      #' select transcript data.
                      ymin = min(data$ymin) + params$exon_height
                      print(ymin)
                      data = data %>% select(track, PANEL, fill) %>% base::unique()
                      print(data)


                      Chrlist = params$ref_chr

                      print(paste("The Chrlist is", Chrlist, sep = " "))

                      if (length(Chrlist) == 1) {
                        print(paste("The bigwig list is ", params$bigwig, sep = ""))
                        if (length(params$bigwig) == 1) {
                          #' single track single bigwig
                          print("single track single bigwig")
                          CoverageTable = read_bigwig_region(params$bigwig[[1]], params$ref_chr, start = start1, end = end1, track_name = Chrlist,
                                                             x_threshold = params$x_threshold, y_threshold = params$y_threshold)


                          CoverageTable = CoverageTable %>% mutate(xmin = position - 0.5,
                                                                   xmax = position + 0.5,
                                                                   ymax = rescale(height, to = c(ymin, ymin + params$bigwig_track_height)),
                                                                   ymin = ymin,
                                                                   PANEL = data$PANEL[1],
                                                                   fill = data$fill[1])
                        } else {
                          #' single track multiple bigwig
                          CoverageTableList = list()

                          for (i in 1:length(params$bigwig)) {
                            range = seq(ymin, 20, 2)
                            TmpCoverageTable = read_bigwig_region(params$bigwig[[i]], params$ref_chr, start = start1, end = end1,
                                                                  track_name = names(params$bigwig)[i],
                                                                  x_threshold = params$x_threshold, y_threshold = params$y_threshold)
                            Tmpvcftable = params$vcf_table[[i]] %>% select(Pos, Alt_percent)
                            TmpCoverageTable2 = right_join(TmpCoverageTable, Tmpvcftable, join_by(position == Pos)) %>%
                              mutate(height = Alt_percent * height)

                            TmpCoverageTable2 = TmpCoverageTable2 %>% mutate(group = paste(names(params$bigwig)[i],
                                                                                         group, sep = "_"),
                                                                           xmin = position - 1.5,
                                                                           xmax = position + 1.5,
                                                                           ymax = rescale(height,
                                                                                          to = c(range[i], range[i] + params$bigwig_track_height),
                                                                                          from = range(TmpCoverageTable$height)),
                                                                           PANEL = data$PANEL[1],
                                                                           fill = data$fill[1],
                                                                           ymin = range[i])
                            print(head(TmpCoverageTable2))
                            CoverageTableList = append(CoverageTableList,  list(TmpCoverageTable2))

                          }
                          CoverageTable = do.call(base::rbind,CoverageTableList)
                        }
                      }
                      #' ./R/utilities.R:empty <- function(df) will exam the data at rendering
                      #' layout$panel_params[[data$PANEL[1]]] this will check the PANEL col


                      return(CoverageTable)
                    },

                    draw_key = draw_key_polygon
)

geom_coverageMUT <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, show.legend = NA, exon_height=1.5,
                      x_translation = NULL, layout = "up" ,subset = NULL, annotation_type = NULL, y_threshold =2,
                      x_threshold =1, region_based_calling = F, bigwig = NULL, ref_chr = NULL, bigwig_height_normalize = T, bigwig_track_height =1.5,
                      inherit.aes = TRUE, track_max = NULL, vcf_table = NULL) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomCoverageMUT,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  exon_height = exon_height,
                  x_translation = x_translation,
                  subset = subset,
                  layout = layout,
                  y_threshold = y_threshold,
                  x_threshold = x_threshold,
                  region_based_calling = region_based_calling,
                  bigwig = bigwig,
                  annotation_type = annotation_type,
                  ref_chr = ref_chr,
                  bigwig_height_normalize = bigwig_height_normalize,
                  bigwig_track_height = bigwig_track_height,
                  track_max = track_max,
                  vcf_table = vcf_table))
}


# Function to read a specific region from a BigWig file and add a group column
read_bigwig_region <- function(bigwig_file, chr, start, end, track_name = "track1",
                               x_threshold = 1, y_threshold = 0) {
  # Define the genomic region of interest
  region <- GRanges(seqnames = chr, ranges = IRanges(start = start, end = end))

  # Import the BigWig file for the specified region
  bw_data <- rtracklayer::import.bw(bigwig_file, which = region, as = "GRanges")

  # Covert the width = 1
  single_base_gr <- GRanges(
    seqnames = rep(seqnames(bw_data), width(bw_data)),
    ranges = IRanges(
      start = unlist(Map(seq, start(bw_data), end(bw_data))),
      width = 1
    ),
    score = rep(bw_data$score, width(bw_data))
  )

  # Convert to a data frame
  bigwig_df <- as.data.frame(single_base_gr)

  # Create the initial table
  result <- tibble(
    track = track_name,
    chr = bigwig_df$seqnames,
    position = bigwig_df$start,
    height = bigwig_df$score
  )

  # Apply y_threshold: Treat heights below y_threshold as 0
  result$height[result$height < y_threshold] <- 0

  # Add a group column based on consecutive rows with height = 0
  result$group <- 1 # Initialize the group column
  zero_count <- 0 # Counter for consecutive rows with height = 0

  if (nrow(result) > 1) {
    for (i in 2:nrow(result)) {
      if (result$height[i] == 0) {
        zero_count <- zero_count + 1
      } else {
        zero_count <- 0
      }

      # If the number of consecutive rows with height = 0 exceeds the x_threshold, start a new group
      if (zero_count > x_threshold) {
        result$group[i] <- result$group[i - 1] + 1
      } else {
        result$group[i] <- result$group[i - 1]
      }
    }
  }
  return(result)
}

add_transcript_name_fuzzy <- function(df1, df2) {
     # Perform the interval join using fuzzyjoin
  result <- fuzzyjoin::fuzzy_left_join(
    df1,
    df2,
    by = c("position" = "xmin", "position" = "xmax"),
    match_fun = list(`>=`, `<=`)
  )

  # Select and rename the relevant columns
  result <- result %>%
    select(track, position, height, transcripts, height2, ymin)

  return(result)
}
