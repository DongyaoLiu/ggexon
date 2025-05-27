#' designed for plot coverage data.


GeomCoverage <- ggproto("GeomCoverage", GeomRect,
                    required_aes = c("ymin", "xmin", "xmax","transcripts","strand", "track", "type"),
                    non_missing_aes = c("ymax", "linewidth", "shape"),
                    extra_params = c("exon_height", "na.rm", "x_translation", "layout", "annotation_type", "subset", "y_threshold",
                                     "x_threshold", "region_based_calling", "bigwig", "ref_chr",
                                     "bigwig_track_height", "bigwig_height_normalize"),
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
                        cli_abort(c("Please offer gff or gtf format"))
                      }

                      #' assess the validation of annotation_type
                      if (is.na(params$bigwig)) {
                        cli_abort(c("bigwig file path is required"))
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

                      #' get translation data.
                      data = data %>% group_by(track) %>%
                        mutate(xmin2 = min(xmin)) %>%
                        mutate(x_adjustment = 0) %>%
                        mutate(xmin = xmin - xmin2, xmax = xmax - xmin2)
                      translation_data = unique(data$xmin2)


                      #' select transcript data.
                      data = data  %>% filter(type == params$annotation_type)
                      rec_data = seq_add_y(data = data,
                                           track_proportion = params$transcripts_track_ratio,
                                           y_scale = params$y_scale,
                                           exon_proportion = 0.8, blank_proportion = 0.2,
                                           sandwich_ratio = params$sandwich_ratio,
                                           exon_height = params$exon_height)
                      track_data = rec_data %>% ungroup() %>% select(ymin, transcripts, xmin, xmax)


                      #' extract the bigwig data.
                      #' deal with more than one bigwig data.
                      #' one transcript, more coverage track.
                      #' one transcript, one coverage track.
                      #' specify the multi-track multi-coverage by named list.

                      Chrlist = unique(rec_data$track)

                      print(paste("The Chrlist is", Chrlist, sep = " "))

                      if (length(Chrlist) == 1) {
                        bigwig_list = params$bigwig[[Chrlist]]
                        print(paste("The bigwig list is ", params$bigwig,sep = ""))
                        if (length(bigwig_list) == 1) {
                          #' single track single bigwig
                          print("single track single bigwig")
                          CoverageTable = read_bigwig_region(bigwig_list, params$ref_chr, start = start1, end = end1, track_name = Chrlist,
                                                             x_threshold = params$x_threshold, y_threshold = params$y_threshold)
                        } else {
                          #' single track multiple bigwig
                          for (i in 1:length(params$bigwig)) {
                            TmpCoverageTable = read_bigwig_region(bigwig_list[i], params$ref_chr, start = start1, end = end1, track_name = Chrlist[i],
                                                                  x_threshold = params$x_threshold, y_threshold = params$y_threshold)
                            TmpCoverageTable = TmpCoverageTable %>% mutate(group = paste(track_name, group, sep = "_"))
                            CoverageTableList[i] = TmpCoverageTable
                          }
                        }
                      }else{
                        CoverageTableList = list()
                        for (i in 1:length(Chrlist)) {
                          track_name_tmp = bigwig_list[[Chrlist[i]]]
                          for (j in 1:length(track_name_tmp)) {
                            #' multiple track multiple bigwig files.
                            bigwig_file_name = paste(i,j,sep="\t")
                            TmpCoverageTable = read_bigwig_region(track_name_tmp[j], chr, start = start1, end = end1, track_name = Chrlist[i],
                                                                  x_threshold = params$x_threshold, y_threshold = params$y_threshold)
                            TmpCoverageTable = TmpCoverageTable %>% mutate(group = paste(track_name, group, sep = "_"))
                            CoverageTableList[bigwig_file_name] = TmpCoverageTable
                          }
                        }
                        CoverageTable = do.call(rbind, CoverageTableList)
                      }

                      #' do y_threshold.
                      CoverageTable = CoverageTable %>% filter(height >= params$y_threshold)

                      #' translation of coordinates of CoverageTable
                      CoverageTable = CoverageTable %>% mutate(position = position - translation_data)



                      #' do bigwig track normalization.
                      if (is.na(params$bigwig_height_normalize)) {
                        cli_about(c("bigwig_height_normalize is required"))
                      }else if(params$bigwig_height_normalize == T) {
                        CoverageTable = CoverageTable %>% ungroup() %>%
                          mutate(height_max = max(height), height2 = (height/height_max)*params$bigwig_track_height)
                        print("normalize bigwig_height tracks")
                      }else{
                        print("don't normalize the bigwig track.")
                      }


                      #' processing the coverage data and merging with track information
                      CoverageTable = add_transcript_name_fuzzy(CoverageTable, track_data) %>%
                        mutate(across(everything(), ~ ifelse(is.na(.x), min(.x, na.rm = TRUE), .x))) %>%
                        mutate(group2 = paste(track, transcripts, sep = "_"), ymax = ymin + height2)

                      if (params$region_based_calling == F) {
                        #' extract coverage based on subset genomic region.
                        print("region based coverage calling is turned off, here is used for develope region based\
                        scale for coverage data")
                      }else{
                        #' extract base on specific region.
                        print("region based coverage calling, extracting the coverage data base on ...")

                      }
                      #' CoverageTable$PANLE = 1
                      #' print(CoverageTable)
                      #' all the munipulation of
                      #' ./R/utilities.R:empty <- function(df) will exam the data at rendering
                      #' layout$panel_params[[data$PANEL[1]]] this will check the PANEL col
                      CoverageTable = CoverageTable %>% mutate(xmin = position - 0.5,
                                                               xmax = position + 0.5,
                                                               PANEL = 1,
                                                               ymax = height2 + ymin)
                      CoverageTable
                    },

                    draw_key = draw_key_polygon
)

geom_coverage <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, show.legend = NA, exon_height=1.5,
                      x_translation = NULL, layout = "up" ,subset = NULL, annotation_type = NULL, y_threshold =2,
                      x_threshold =1, region_based_calling = F, bigwig = NULL, ref_chr = NULL, bigwig_height_normalize = T, bigwig_track_height =1.5,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomCoverage,
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
                  bigwig_track_height = bigwig_track_height))
}


# Function to read a specific region from a BigWig file and add a group column
read_bigwig_region <- function(bigwig_file, chr, start, end, track_name = "track1", 
                               x_threshold = 1, y_threshold = 0) {
  # Load the rtracklayer package
  if (!requireNamespace("rtracklayer", quietly = TRUE)) {
    install.packages("rtracklayer")
  }
  library(rtracklayer)
  
  # Define the genomic region of interest
  region <- GRanges(seqnames = chr, ranges = IRanges(start = start, end = end))
  
  # Import the BigWig file for the specified region
  bigwig_data <- import(bigwig_file, which = region)
  
  # Convert to a data frame
  bigwig_df <- as.data.frame(bigwig_data)
  
  # Create the initial table
  result <- data.frame(
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
