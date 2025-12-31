#'calculate the exon block y location
seq_add_y = function(data, track_proportion, y_scale,
                     exon_proportion, blank_proportion, exon_height, sandwich_ratio){
  #' ymin and ymax is for draw the exon part
    data2 = data %>%
    mutate(y_range = exon_height) %>%
    mutate(ymax = ymin + y_range, y_middle = ymin + y_range/2)
  # data$y_range = rep((exon_proportion * y_min_list[1]), nrow(data))
  # data$ymax = data$ymin + data$y_range
  # data$y_middle = (data$ymin + data$ymax)/2
  # data = as.data.frame(data)
  return(data2)
}

protein_link_prepare = function(data, panel_middle = c(47, 53), annotation_start = 10000,
                                interval = 2000, midle_align_box_length = 10000) {
  transcript_group_number = length(unique(data$group))
  if (transcript_group_number == 1) {
    coord_x_align = rep(annotation_start,2)
  }else{
    coord_x_align = seq(from = annotation_start, by = (midle_align_box_length + interval), transcript_group_number)
    coord_x_align = rep(coord_x_align, each =2)
  }
  coord_x0_align = coord_x_align + midle_align_box_length
  data2 = data %>% rowwise() %>% mutate(y = if_else(ymin > 50, ymin, ymax)) %>%
    mutate(y0 = if_else(ymin > 50, panel_middle[2], panel_middle[1])) %>%
    group_by(group) %>%
    mutate(start_average = mean(start)) %>% arrange(start_average) %>%
    select(transcripts, start, end, y, y0) %>% ungroup()
  data2$x = coord_x_align
  data2$x0 = coord_x0_align
  data2$group = 1:nrow(data2)
  ##Reshape your data
  data_melt1 = melt(data2[, 1:5], id = c("group", "transcripts", "y"), value.name = "x")
  data_melt2 = melt(data2[, c(1:2,6:8)], id = c("group", "transcripts", "y0"), value.name = "x") %>%
    mutate(transcripts = paste(transcripts, "_algin_point", sep = "_"))
  colnames(data_melt2)[3] = "y"
  data_xy = rbind(data_melt1, data_melt2) %>% dplyr::rename(attr = transcripts) %>% select(!variable)
  data_xy_spline = Splines_link_generate(data = data_xy)
  return(data_xy_spline)
}

#'calculate the start and end of the sequencing track
seq_track = function(data, start=NULL, end=NULL){
  seq_track_data = data %>% group_by(track_name) %>%
    summarize(min_xmin = min(xmin), max_xmax = max(xmax), y_middle = first(y_middle)) %>%
    mutate(length = abs(min_xmin- max_xmax))
  if (is.null(start)){
    seq_track_data = seq_track_data %>% mutate(start = min_xmin - seq_track_data$length * 0.1)
  }else{
    seq_track_data$start = start
  }
  if (is.null(end)){
    seq_track_data = seq_track_data %>% mutate(end = max_xmax + seq_track_data$length * 0.1)
  }else{
    seq_track_data$end = end()
  }
  seq_track_data = melt(seq_track_data) %>%
    filter(variable == "start" | variable == "end") %>%
    rename(seq_track_x = value) %>%
    arrange(track_name)
  data = data %>% arrange(track_name)
  data = cbind(data, seq_track_data["seq_track_x"])
  return(data)
}

#'calculate the transcripts lines coordinates.
transcripts_track = function(data, transcript_data=NULL, between_margin=NULL,
                             Track_Position = "below", joint_x = 0.5,
                             joint_y = 0.2, transcripts_y = 0.1){
  if (is.null(between_margin)){
    between_margin = 0.2
  }
  if (is.null(transcript_data)) {
    transc3ript_data = data
  }else{
    pass
  }
  transcript_x_data = melt(data) %>% filter(variable == "xmin" | variable == "xmax")
  if (Track_Position == "below"){
    transcript_y_data = transcript_data %>% mutate(y_range = abs(ymax - ymin)) %>%
      select(transcripts, ymin, y_range) %>% distinct()
  }else if (Track_Position == "above"){
    transcript_y_data = transcript_data %>% mutate(y_range = abs(ymax - ymin)) %>%
      select(transcripts, ymax, y_range) %>% distinct()
  }
#  transcript_y_data = transcript_data %>% select(transcripts, y_middle) %>% distinct()
  transcript_x_data = transcript_data %>% rownames_to_column(var = "exon_ID") %>%
    melt() %>% filter(variable == "xmin" | variable == "xmax") %>% arrange(transcripts,value)
  joint_x_data = transcript_x_data %>% group_by(transcripts) %>% slice(-1,-n()) %>% ungroup()
  intron_ID = rep(1:(nrow(joint_x_data) / 2), each = 2)
  joint_x_data = joint_x_data %>% mutate(intron_ID = intron_ID) %>%
    group_by(intron_ID) %>% mutate(joint_x_position = sum(value) * joint_x) %>% ungroup()
  #' tmp_1 extract the joint top of the angle
  #'
  tmp_1 = joint_x_data[ ,c(1:4,7:8)] %>% mutate(attr = rep("point", nrow(joint_x_data)))
  tmp_2 = joint_x_data[ ,c(1:4,7,6)] %>% mutate(attr = rep("bottom", nrow(joint_x_data)))
  colnames(tmp_2) = colnames(tmp_1)
  joint_x_data = rbind(tmp_1, tmp_2)
  joint_xy_data = left_join(joint_x_data, transcript_y_data, by = "transcripts") %>% rowwise() %>%
    mutate(joint_y_position = ymin - if_else(attr == "point", y_range * joint_y, y_range * transcripts_y))
  return(joint_xy_data)
}



#' plot seq stick and tri-angle for indicating the direction of the gene.
#'

#' pass either proportion or length.
add_transcripts_seq_line = function(data, transcripts_stop_proportion = NULL, transcripts_stop_length = 400){
  data2 = data %>% group_by(transcripts) %>%
    dplyr::summarize(x = min(xmin), max_xmax = max(xmax), y_middle = dplyr::first(y_middle),
     PANEL = dplyr::first(PANEL), group = dplyr::first(group), colour = dplyr::first(fill), strand = dplyr::first(strand),
      linewidth = dplyr::first(linewidth), alpha = dplyr::first(alpha), y_range = dplyr::first(y_range)) %>%
    mutate(transcripts_length = abs(max_xmax - x))

  if (is.null(transcripts_stop_length)){
    data2 = data2 %>% mutate(transcripts_stop_length = transcripts_stop_proportion *transcripts_length)
  }else{
    {}
  }
  data2 = data2 %>% mutate(xend = if_else(strand == "+",
                                          max_xmax + transcripts_stop_length,
                                          x - transcripts_stop_length)) %>%
    mutate(x = if_else(strand == "+", x, max_xmax)) %>%
    mutate(y = y_middle, yend = y_middle)
  #' determination of transcription direction.
  return(data2)
}

add_transcripts_direction = function(data, ratio = 0.25, angle = pi/3, lengthABS = 160, lengthPRO =NULL){
  #' angle is the base angle of isosceles
  #' pre: the transcripts dataframe , is the output of add_transcripts, require at least the end point of each transcripts. xend, yend, y_range(the height of exon) and the ID of
  #' each transcripts
  #' ratio is a para to describe the ratio = the length of base of isosceles triangle / (y_range)
  #' prototyep of this function use the ratio to calculate the actual length of triangle.
  #' potencial para r1 and r2.

  data = data %>% arrange(transcripts)
  #data_xy = data %>% select(transcripts, xend, yend, y_range, strand, transcripts_length)
  data_triangle = data %>% mutate(base_length = y_range * ratio) %>%
    mutate(b1_x = xend , b1_y = yend + base_length/2) %>%
    mutate(b2_x = xend, b2_y = yend - base_length/2)

  if (!is.null(lengthPRO)) {
    data_triangle = data_triangle %>% mutate(a_x = if_else(strand == "+", xend + transcripts_length*lengthPRO, xend - transcripts_length*lengthPRO), a_y = yend) %>%
      select(!c(base_length, y_range, xend, yend))
  }else if (!is.null(lengthABS)) {
    data_triangle = data_triangle %>% mutate(a_x = if_else(strand == "+", xend + lengthABS, xend - lengthABS), a_y = yend) %>%
      select(!c(base_length, y_range, xend, yend))
  }else{
    {}
  }
  data_triangle = data_triangle %>% select(transcripts, b1_x, b1_y, b2_x, b2_y, a_x, a_y)
  data_melt = melt(data_triangle, id=c("transcripts")) %>%
    arrange(transcripts)
  #' be careful about the new columns with the name "x" and "y"
  data2= data.frame(
    x = as.numeric(data_melt$value[grep("x",data_melt$variable)]),
    y = as.numeric(data_melt$value[grep("y", data_melt$variable)]),
    group = rep(data$group, each=3),
    PANEL = rep(data$PANEL, each=3),
    linewidth = rep(data$linewidth, each=3),
    colour = rep(data$colour %||% data$fill , each=3),
    fill = rep(data$fill %||% data$colour, each=3),
    alpha = rep(1, (length(unique(data$transcripts))*3))
  )
  return(data2)
}

#' prepare the gene_tag ploygons

add_genetag = function(data, tag_height=NULL, tag_width=NULL, tag_angle=NULL, tag_rotate_angle=NULL, point_number = 4){

  tag_data = data %>% select(trackname, x, track_y) %>% mutate(track_y_m = track_y/2) %>%
    mutate(s = tag_height/tan(tag_angle),
                             x1 = x - s - (tag_width - 2*s)/2,
                             y1 = track_y - track_y_m,
                             x2 = x + (tag_width - 2*s)/2,
                             y2 = y1,
                             x3 = x + s + (tag_width - 2*s)/2,
                             y3 = track_y + track_y_m,
                             x4 = x - (tag_width - 2*s)/2,
                             y4 = y3) %>%
    select(!c(track_y, s, track_y_m)) %>% rename(x_center = x)

  tag_data_melt = melt(tag_data, id=c("trackname", "x_center")) %>%
    arrange(trackname)
  data2= data.frame(
    x = as.numeric(data_melt$value[grep("x",data_melt$variable)]),
    y = as.numeric(data_melt$value[grep("y", data_melt$variable)]),
    group = rep(data$group, each=point_number),
    PANEL = rep(data$PANEL, each=point_number),
    linewidth = rep(data$linewidth, each=point_number),
    colour = rep(data$colour, each=point_number),
    fill = rep("black", (length(unique(data$transcripts))*point_number)),
    alpha = rep(1, (length(unique(data$transcripts))*point_number))
  )
  return(data2)
}

##### protein_linker
##### This is for protein test data set
protein_link_data = data.frame(x = c(0, 100, 30, 40),
                               y = c(0, 0, 30, 30),
                               attr = c(rep("transcripts1",2),
                                        rep("transcripts2", 2)),
                               group = c(1,1,1,1))

#' from ggforce
#getSplines <- function(x, y, id, detail, type = "clamped") {
#  .Call('_ggforce_getSplines', PACKAGE = 'ggforce', x, y, id, detail, type)
#}



Splines_link_generate = function(data, detail = 100){
  withr::with_package("dplyr", {
  link_mins =  data %>% group_by(attr) %>% filter(x == min(x)) %>%
    arrange(attr) %>% ungroup()
  link_mins = link_mins %>% dplyr::mutate(attr = if_else(row_number() %% 2 == 1,1,4))

  link_maxs = data %>% group_by(attr) %>% filter(x == max(x)) %>%
    arrange(attr) %>% ungroup()
  link_maxs = link_maxs %>% dplyr::mutate(attr = if_else(row_number() %% 2 == 1,4,1))
  link_mins_control_point = link_mins %>%
    group_by(group) %>% dplyr::mutate(x = rev(x), attr = if_else(row_number() %% 2 == 1,3,2))
  link_maxs_control_point = link_maxs %>%
    group_by(group) %>% dplyr::mutate(x = rev(x), attr = if_else(row_number() %% 2 == 1,2,3))
  min_spline = rbind(link_mins, link_mins_control_point) %>%
    arrange(group, attr)
  max_spline = rbind(link_maxs, link_maxs_control_point) %>%
    arrange(group, attr)


  spline_1 = ggforce:::getSplines(x = min_spline$x, y = min_spline$y,
                        id = min_spline$group, detail = detail, type = "clamped")
  spline_1_data = spline_1$paths %>% as.data.frame() %>% mutate(group = spline_1$pathID)

  spline_2 = ggforce:::getSplines(x = max_spline$x, y = max_spline$y,
                        id = max_spline$group, detail = detail, type = "clamped")
  spline_2_data = spline_2$paths %>% as.data.frame() %>% mutate(group = spline_2$pathID)

  spline_shape = rbind(spline_1_data, spline_2_data)
  colnames(spline_shape)[1:2] = c("x", "y")
  return(spline_shape)
    }
  )
}





