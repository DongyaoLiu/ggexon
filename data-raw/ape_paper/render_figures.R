#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(ggplot2); library(pkgload)})
# Parameters: all figure styling and export controls live here.
P <- list(text_size_pt=8, family="Times", width_mm=180,
          height_mm=c(mhc=210), dpi=180,
          data_linewidth=0.25, axis_linewidth=0.2, marker_size=0.5,
          link_min_length=1000, link_min_identity=85,
          exon_height=0.4, feature_height=0.3, link_alpha=0.35,
          feature_gap=0.8, signal_height=1.65, signal_gap=0.75,
          wong=c("#E69F00","#56B4E9","#009E73","#CC79A7","#D55E00","#F0E442","#0072B2","#000000"),
          continuous_map="viridis", auto_quantile=TRUE)
# Continuous colour, if introduced, must use finite-data equal-count quantiles.
ROOT <- normalizePath(".")
pkgload::load_all(ROOT, quiet=TRUE)
source(file.path(ROOT,"data-raw/ape_paper/annotation-lanes.R"))
BASE <- file.path(ROOT,"data-raw/ape_paper/results")
OUT <- file.path(BASE,"figures"); dir.create(OUT, recursive=TRUE, showWarnings=FALSE)
read_tsv <- function(f) read.delim(f, check.names=FALSE, stringsAsFactors=FALSE)
merge_features <- function(d) {
  if (!nrow(d)) return(d)
  groups <- split(d, interaction(d$track,d$chr,d$kind,drop=TRUE))
  do.call(rbind,lapply(groups,function(z) {
    z <- z[order(z$xmin,z$xmax),]; out <- z[1,,drop=FALSE]
    if (nrow(z)>1) for (i in 2:nrow(z)) {
      j <- nrow(out)
      if (z$xmin[i]<=out$xmax[j]+1) out$xmax[j] <- max(out$xmax[j],z$xmax[i])
      else out <- rbind(out,z[i,,drop=FALSE])
    }
    out
  }))
}
render_case <- function(case) {
  path <- file.path(BASE,case); wf <- file.path(path,"windows.tsv")
  if (!file.exists(wf)) return(paste(case,"SKIPPED: windows.tsv missing"))
  w <- read_tsv(wf)
  if (!all(file.exists(file.path(path,paste0(w$id,".gff3"))))) return(paste(case,"SKIPPED: annotation missing"))
  sp <- SynSpecies(name=case)
  for (id in w$id) sp <- add_individual(sp, load_annotation(SynIndividual(
    annotation_file=file.path(path,paste0(id,".gff3")),annotation_format="gff",id=id)))
  links <- read_tsv(file.path(path,"links.tsv"))
  candidate_count <- nrow(links)
  # Display filtering uses computed identity in links.tsv, including the PAF gap denominator.
  links <- links[is.finite(links$alen) & links$alen>=P$link_min_length &
                   is.finite(links$identity) & links$identity>=P$link_min_identity,]
  pairs <- split(links,interaction(links$tspecies,links$qspecies,drop=TRUE))
  pair_names <- character()
  for (d in pairs) {
    nm <- paste(d$tspecies[1],d$qspecies[1],sep="__")
    sp <- add_pairwise_alignment(sp,SynPairAlignment(nm,query_individual=d$qspecies[1],
      target_individual=d$tspecies[1],file=file.path(path,"links.tsv"),data=d))
    pair_names <- c(pair_names,nm)
  }
  sp <- store_chain_layout(sp,free=list(x=TRUE,y=TRUE))
  for (i in seq_len(nrow(w))) sp <- set_panel_xlim(sp,individual=w$id[i],xlim=c(w$start0[i]+1,w$end0[i]),xlim_chr=w$chrom[i])
  flat <- TRUE
  p <- ggexon(sp) + geom_exon(species=if(case=="mhc")setdiff(w$id,"chimp_h2") else w$id,
    force_flat=flat,exon_height=P$exon_height,fill=P$wong[7],colour=NA)
  if(case=="mhc" && "chimp_h2" %in% w$id) p <- p + geom_genetag(species="chimp_h2",feature_type="gene",fill=P$wong[7],label_family=P$family,label_size=P$text_size_pt/ggplot2::.pt)
  label_data <- ggexon:::syn_to_exon_df(sp,species=if(case=="mhc")setdiff(w$id,"chimp_h2") else w$id)
  label_data <- label_data[!duplicated(paste(label_data$track,label_data$gene_name)),]
  label_data$y <- if(flat) 1.65 else label_data$ymin+0.7
  label_data$label <- label_data$gene_name
  p <- p + geom_text(data=label_data,aes(x=xmin,y=y,label=label),
    hjust=0,vjust=0,family=P$family,size=P$text_size_pt/ggplot2::.pt,check_overlap=FALSE,inherit.aes=FALSE)
  for (nm in pair_names) p <- p + geom_nuclink(alignment=nm,fill="grey60",alpha=P$link_alpha,colour=NA)
  feature_kinds <- character()
  lane_cursor <- setNames(rep(-0.8,nrow(w)),w$id)
  interval_layer <- NULL
  interval_source <- NULL
  features_file <- file.path(path,"features.tsv")
  if (file.exists(features_file)) {
    f <- merge_features(read_tsv(features_file))
    if (nrow(f)) {
      feature_kinds <- sort(unique(f$kind))
      f$species <- f$individual <- f$track
      for (id in unique(f$track)) {
        kinds <- sort(unique(f$kind[f$track==id]))
        for (kind in kinds) {
          take <- f$track==id & f$kind==kind
          f$ymin[take] <- lane_cursor[[id]]-P$feature_height
          f$ymax[take] <- lane_cursor[[id]]
          lane_cursor[[id]] <- lane_cursor[[id]]-P$feature_gap
        }
      }
      interval_layer <- length(p@layers)+1L
      interval_source <- f
      p <- p + geom_rect(data=f,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=kind),inherit.aes=FALSE)
      # Feature identities are labelled once in the legend to avoid lane text overlaps.
    }
  }
  cov <- list()
  for (id in w$id) {
    file <- file.path(path,paste0(id,".methylation.tsv"))
    if (!file.exists(file)) next
    d <- read_tsv(file); if (!nrow(d)) next
    d$chr <- w$chrom[match(id,w$id)]
    d$track <- id; d$assay <- "mCG"; d$species <- d$individual <- id
    cov[[length(cov)+1]] <- d
  }
  lims <- setNames(lapply(seq_len(nrow(w)),function(i)c(w$start0[i]+1,w$end0[i])),w$id)
  chrs <- setNames(as.list(w$chrom),w$id)
  signal_qc_inputs <- list()
  # Shared raw assay ranges across genomes; no per-genome rescaling.
  atac_max <- max(c(0,unlist(lapply(cov,function(d) if(d$assay[1]=="ATAC_RPKM")d$coverage else numeric()))))
  atac_limit <- if(atac_max>0) max(pretty(c(0,atac_max),n=4)) else 1
  for (d in cov) {
    id <- d$track[1]; assay <- d$assay[1]
    limits <- c(0,if(assay=="ATAC_RPKM")atac_limit else 1)
    lane_top <- lane_cursor[[id]]-P$signal_gap
    baseline <- lane_top-P$signal_height
    lane <- prepare_signal_lane(d,baseline,P$signal_height,limits)
    index <- length(p@layers)+1L
    p <- p + annotation_signal_layer(lane,fill=P$wong[if(assay=="ATAC_RPKM")1 else 3])
    signal_qc_inputs[[length(signal_qc_inputs)+1L]] <- list(
      layer=index,source=d,prepared=lane,assay=assay,baseline=baseline,
      lane_top=lane_top,limits=limits)
    window <- w[w$id==id,]
    lane_label <- if(assay=="ATAC_RPKM")sprintf("L5 ET ATAC (RPKM; 0–%g)",limits[2]) else
      "CpG methylation (fraction; 0–1)"
    guide <- data.frame(track=id,xmin=window$start0+1,xmax=window$end0,
      baseline=baseline,top=lane_top,label=lane_label)
    p <- p + geom_segment(data=guide,aes(x=xmin,xend=xmax,y=baseline,yend=baseline),
      inherit.aes=FALSE,colour="grey75",linewidth=P$axis_linewidth) +
      geom_segment(data=guide,aes(x=xmin,xend=xmax,y=top,yend=top),
        inherit.aes=FALSE,colour="grey80",linetype="dotted",linewidth=P$axis_linewidth) +
      geom_text(data=guide,aes(x=xmin,y=top+0.12,label=label),hjust=0,vjust=0,
        family=P$family,size=P$text_size_pt/ggplot2::.pt,inherit.aes=FALSE)
    lane_cursor[[id]] <- baseline-P$signal_gap
  }
  annotation_limits <- setNames(lapply(w$id,function(id) {
    gene_top <- if(flat)2.4 else max(label_data$y[label_data$track==id])+0.8
    c(lane_cursor[[id]]-0.2,gene_top)
  }),w$id)
  strip_labels <- setNames(w$label,w$id)
  if(case=="mhc") strip_labels[["chimp_h2"]] <- paste(strip_labels[["chimp_h2"]],"(curated gene spans)")
  p <- p + scale_x_continuous(oob=scales::oob_keep) + facet_genomics(vars(track),labeller=labeller(track=function(x)ifelse(x %in% names(strip_labels),strip_labels[x],x)),scales="free",ncol=1,xlim=lims,xlim_chr=chrs,
    link_panel_height=0.5,link_axis="none",link_strip="blank") +
    scale_panel_annotation("free_y",limits=annotation_limits) +
    scale_fill_manual(values=setNames(P$wong, c(feature_kinds,
      paste0("unused",seq_len(8)))[seq_len(8)])) +
    labs(x="Genomic coordinate (bp)",y=NULL,fill=NULL) + theme_bw(base_size=P$text_size_pt,base_family=P$family) +
    theme(text=element_text(size=P$text_size_pt,family=P$family),
      axis.text=element_text(size=P$text_size_pt),strip.text=element_text(size=P$text_size_pt),
      legend.text=element_text(size=P$text_size_pt),legend.title=element_text(size=P$text_size_pt),
      plot.title=element_blank(),plot.subtitle=element_blank(),panel.grid.minor=element_blank(),
      panel.grid.major=element_line(colour="grey93",linewidth=P$axis_linewidth),
      strip.background=element_rect(fill="white"),legend.position="bottom")
  built <- ggexon_build(p)
  if (!is.null(interval_layer)) {
    interval_shown <- built@data[[interval_layer]]
    interval_owner <- built@layout$layout[match(interval_shown$PANEL,built@layout$layout$PANEL),]
    stopifnot(nrow(interval_shown)==nrow(interval_source),
      all(interval_shown$ymax<0),all(interval_owner$panel_type=="annotation"),
      identical(as.character(interval_owner$track),as.character(interval_source$track)))
    interval_qc <- data.frame(track=interval_source$track,kind=interval_source$kind,
      panel_type=interval_owner$panel_type,ymin=interval_shown$ymin,ymax=interval_shown$ymax)
    write.table(interval_qc,file.path(OUT,paste0(case,".feature-lane-qc.tsv")),sep="\t",quote=FALSE,row.names=FALSE)
  }
  signal_qc <- lapply(signal_qc_inputs,function(info) {
    source <- info$source; shown <- built@data[[info$layer]]
    owner <- built@layout$layout[match(unique(shown$PANEL),built@layout$layout$PANEL),]
    score_match <- identical(source$coverage,shown$raw_score)
    coordinate_match <- identical(as.numeric(source$xmin),as.numeric(shown$interval_start)) &&
      identical(as.numeric(source$xmax),as.numeric(shown$interval_end))
    stopifnot(nrow(source)==nrow(shown),score_match,coordinate_match,
      all(is.finite(shown$xmin)),all(is.finite(shown$xmax)),
      identical(as.character(owner$track),source$track[1]),
      all(owner$panel_type=="annotation"),all(shown$ymax<0))
    data.frame(track=source$track[1],assay=info$assay,panel_type=owner$panel_type,
      input_intervals=nrow(source),built_intervals=nrow(shown),
      input_score_sum=sum(source$coverage),built_score_sum=sum(shown$raw_score),
      exact_score_match=score_match,raw_coordinate_match=coordinate_match,
      lane_top=info$lane_top,baseline=info$baseline,
      scale_min=info$limits[1],scale_max=info$limits[2])
  })
  write.table(do.call(rbind,signal_qc),file.path(OUT,paste0(case,".signal-lane-qc.tsv")),sep="\t",quote=FALSE,row.names=FALSE)
  layout_roles <- built@layout$layout$panel_type
  stopifnot(sum(layout_roles=="annotation")==nrow(w),sum(layout_roles=="coverage")==0L,
    sum(layout_roles=="link")==nrow(w)-1L)
  write.table(as.data.frame(built@layout$layout),file.path(OUT,paste0(case,".layout-qc.tsv")),sep="\t",quote=FALSE,row.names=FALSE)
  # ggexon currently reuses reference-coordinate tick labels across panels.
  # Replace axis grobs only, using actual trained display ranges and native windows.
  saveRDS(built@layout$panel_params,file.path(OUT,paste0(case,".panel-params.rds")))
  table <- ggexon_gtable(built)
  layout <- as.data.frame(built@layout$layout)
  axis_qc <- list()
  for (i in seq_len(nrow(layout))) {
    row <- layout[i,]; if(row$panel_type=="link") next
    if(row$panel_type=="annotation") {
      yi <- which(table$layout$name==paste0("axis-l-",row$ROW,"-",row$COL))
      if(length(yi)) table$grobs[[yi]] <- grid::nullGrob()
    }
    idx <- which(table$layout$name==paste0("axis-b-",row$COL,"-",row$ROW))
    if(!length(idx)) next
    lo <- row$xlim_min; hi <- row$xlim_max
    bp <- pretty(c(lo,hi),n=4); bp <- bp[bp>=lo & bp<=hi]
    # The display-coordinate limits are trained separately, while ggexon maps
    # native window endpoints onto those displayed endpoints with affine alignment.
    pp <- built@layout$panel_params[[as.integer(row$PANEL)]]
    display <- pp$x.range
    # panel_scales_x stores aligned coordinate limits before visual expansion.
    trained <- built@layout$panel_scales_x[[row$SCALE_X]]$get_limits()
    pos <- (trained[1]+(bp-lo)/(hi-lo)*diff(trained)-display[1])/diff(display)
    table$grobs[[idx]] <- grid::grobTree(
      grid::segmentsGrob(x0=pos,x1=pos,y0=grid::unit(1,"npc"),y1=grid::unit(1,"npc")-grid::unit(1,"mm"),gp=grid::gpar(lwd=P$axis_linewidth)),
      grid::textGrob(format(bp,big.mark=",",scientific=FALSE,trim=TRUE),x=pos,y=grid::unit(1,"npc")-grid::unit(1.5,"mm"),vjust=1,gp=grid::gpar(fontfamily=P$family,fontsize=P$text_size_pt)))
    axis_qc[[length(axis_qc)+1]] <- data.frame(track=row$track,native_tick=bp,npc=pos,display_min=display[1],display_max=display[2],trained_min=trained[1],trained_max=trained[2])
  }
  write.table(do.call(rbind,axis_qc),file.path(OUT,paste0(case,".axis-qc.tsv")),sep="\t",quote=FALSE,row.names=FALSE)
  saveRDS(table,file.path(OUT,paste0(case,".gtable.rds")))
  ggsave(file.path(OUT,paste0(case,".pdf")),table,width=P$width_mm,height=P$height_mm[[case]],units="mm",device=cairo_pdf)
  ggsave(file.path(OUT,paste0(case,".png")),table,width=P$width_mm,height=P$height_mm[[case]],units="mm",dpi=P$dpi)
  paste(case,"OK:",nrow(w),"genomes;",candidate_count,"candidate links;",nrow(links),"displayed links;",length(cov),"in-panel signal lanes")
}
args <- commandArgs(trailingOnly=TRUE); cases <- if(length(args)) args else c("mhc")
report <- vapply(cases,function(case)tryCatch(render_case(case),error=function(e)paste(case,"ERROR:",conditionMessage(e))),character(1))
writeLines(c("# Integrated annotation render report", "",paste("Run:",format(Sys.time())),
  "", report, "",
  "All feature intervals and signal lanes share their owning genome annotation panel, below its gene models. Only nucleotide links have separate panels.",
  "Signal lane headings give original units and raw-value ranges. Matching assays share the same range across genomes; this affine display placement does not change source scores.",
  "Signal-lane QC asserts every interval/score survives unchanged and is routed to its owning annotation panel below zero. Missing chimp_h2 methylation is omitted.",
  "Native genomic axis labels retain the documented gtable workaround. scale_x_continuous(oob=scales::oob_keep) preserves boundary intervals for clipping.",
  "All changes are example-local; package source files and the primary checkout remain unchanged."),
  file.path(ROOT,"data-raw/ape_paper/render-report.md"))
cat(paste(report,collapse="\n"),"\n")
if (any(grepl(" ERROR:| SKIPPED:",report))) quit(status=1)
