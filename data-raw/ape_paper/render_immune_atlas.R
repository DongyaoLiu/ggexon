#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(ggplot2);library(pkgload);library(grid)})
P <- list(text_size_pt=8,family="Times",width_mm=180,height_mm=230,
          atlas_height_mm=720,dpi=130,data_linewidth=.3,axis_linewidth=.2,
          marker_size=.5,gene_height=.45,link_alpha=.28,
          wong=c("#E69F00","#56B4E9","#009E73","#CC79A7","#D55E00","#F0E442","#0072B2","#000000"),
          continuous_map="viridis",auto_quantile=TRUE)
# Continuous colour, if used, follows equal-count finite-data quantiles.
pkgload::load_all('.',quiet=TRUE)
B <- 'data-raw/ape_paper/results'; O <- file.path(B,'immune_atlas');dir.create(O,recursive=TRUE,showWarnings=FALSE)
rd <- function(f) read.delim(f,stringsAsFactors=FALSE,check.names=FALSE)
wr <- function(d,f) write.table(d,file.path(O,f),sep='\t',quote=FALSE,row.names=FALSE)
g <- rd(file.path(B,'igtr/paper_genes.tsv')); w <- rd(file.path(B,'igtr/loci.tsv'))
a <- rd(file.path(B,'igtr/alignments.tsv')); sv <- rd(file.path(B,'igtr/sv_blocks.tsv'))
order_ids <- c('PPA1','PPA2','HSA1','GGO1','GGO2','PAB1','PAB2','PPY1','PPY2')
labels <- setNames(vapply(order_ids,function(id) {
  z<-w[w$track_id==id,][1,];if(id=='HSA1')'Human CHM13' else paste(z$species,paste0('h',z$haplotype))
},character(1)),order_ids)
# Focused input gates: each published pair belongs to one locus and a real adjacent source track.
stopifnot(all(g$within_locus=="True"),!anyDuplicated(paste(w$locus,w$track_id)),all(g$start<=g$end))
stopifnot(all(abs(match(a$track1,order_ids)-match(a$track2,order_ids))==1))
mg <- rd(file.path(B,'mhc_full/genes_ed3_scope.tsv'))
mg$track_id <- sub('h','',mg$track,fixed=TRUE);mg_all<-mg;mg <- mg[mg$track_id %in% order_ids,]
stopifnot(nrow(mg)==191)
levels_fill <- c('V','D','J','Unclassified','MHC functional','MHC pseudogene')
colors <- setNames(P$wong[seq_along(levels_fill)],levels_fill)
fix_axes <- function(built) {
  tab <- ggexon_gtable(built); lay <- as.data.frame(built@layout$layout)
  for(i in seq_len(nrow(lay))) {
    r <- lay[i,];if(r$panel_type!='annotation') next
    k <- which(tab$layout$name==paste0('axis-l-',r$ROW,'-',r$COL));if(length(k))tab$grobs[[k]]<-nullGrob()
    k <- which(tab$layout$name==paste0('axis-b-',r$COL,'-',r$ROW));if(!length(k))next
    lim <- c(r$xlim_min,r$xlim_max);bp<-pretty(lim,n=3);bp<-bp[bp>=lim[1]&bp<=lim[2]]
    pp<-built@layout$panel_params[[as.integer(r$PANEL)]]; tr<-built@layout$panel_scales_x[[r$SCALE_X]]$get_limits()
    pos<-(tr[1]+(bp-lim[1])/diff(lim)*diff(tr)-pp$x.range[1])/diff(pp$x.range)
    tab$grobs[[k]]<-grobTree(textGrob(format(bp/1000,trim=TRUE,scientific=FALSE),x=pos,y=unit(1,'npc')-unit(1,'mm'),vjust=1,gp=gpar(fontfamily=P$family,fontsize=P$text_size_pt)))
  };tab
}
make_plot <- function(locus,full_mhc=FALSE) {
  output_name<-paste0(locus,if(full_mhc)"_all_species" else "")
  if(full_mhc) {
    order_ids<-c(order_ids,"PTR1","PTR2","SSY1","SSY2");labels<-c(labels,PTR1="Chimpanzee h1",PTR2="Chimpanzee h2",SSY1="Siamang h1",SSY2="Siamang h2");mg<-mg_all
  }
  is_mhc<-grepl('MHC',locus);links<-NULL;s<-NULL
  if(!is_mhc) {
    d<-g[g$locus==locus,];ww<-w[w$locus==locus,];d$xmin<-d$display_start;d$xmax<-d$display_end
    reverse_gene<-ww$display_orientation[match(d$track_id,ww$track_id)]==-1
    d$strand[reverse_gene]<-ifelse(d$strand[reverse_gene]=='+','-','+')
    d$kind<-ifelse(d$gene_type %in% c('V','D','J'),d$gene_type,'Unclassified')
    aa<-a[a$locus==locus,];s<-sv[sv$locus==locus,]
    s$xmin<-s$display_start;s$xmax<-s$display_end
    len<-setNames(ww$length,ww$track_id)
    links<-data.frame(tspecies=aa$track1,qspecies=aa$track2,tchr=locus,qchr=locus,
      tstart=pmin(aa$display_start1,aa$display_end1),tend=pmax(aa$display_start1,aa$display_end1),
      qstart=pmin(aa$display_start2,aa$display_end2),qend=pmax(aa$display_start2,aa$display_end2),
      strand=ifelse(sign(aa$display_end1-aa$display_start1)==sign(aa$display_end2-aa$display_start2),'+','-'),identity=aa$identity)
  } else {
    d<-mg[mg$locus==locus,];starts<-tapply(d$start,d$track_id,min)
    d$xmin<-d$start-starts[d$track_id]+1;d$xmax<-d$end-starts[d$track_id]+1
    d$kind<-ifelse(d$functionality=='functional','MHC functional','MHC pseudogene')
    len<-tapply(d$xmax,d$track_id,max);d$functional<-d$functionality
    d$is_species_specific<-'unknown';d$gene_type<-'MHC'
  }
  d$track<-d$track_id;d$species<-d$individual<-d$track;d$y<-P$gene_height/2;d$chr<-locus
  d$kind<-factor(d$kind,levels=levels_fill)
  d$strand<-ifelse(d$strand %in% c('+','-'),d$strand,'+')
  # Each track uses its true locus window, so length differences remain visible on kb axes.
  sp<-SynSpecies(name=locus)
  for(id in order_ids) {
    z<-d[d$track==id,];f<-file.path(O,paste0(locus,'_',id,'.gff3'))
    gf<-data.frame(locus,'source','gene',as.integer(z$xmin),as.integer(z$xmax),'.',z$strand,'.',paste0('ID=g',seq_len(nrow(z))))
    writeLines('##gff-version 3',f);write.table(gf,f,append=TRUE,sep='\t',quote=FALSE,row.names=FALSE,col.names=FALSE)
    sp<-add_individual(sp,load_annotation(SynIndividual(id=id,annotation_file=f,annotation_format='gff')))
  }
  pair_names<-character()
  if(!is.null(links))for(z in split(links,interaction(links$tspecies,links$qspecies,drop=TRUE))) {
    nm<-paste(z$tspecies[1],z$qspecies[1],sep='__');pair_names<-c(pair_names,nm)
    sp<-add_pairwise_alignment(sp,SynPairAlignment(nm,query_individual=z$qspecies[1],target_individual=z$tspecies[1],file=file.path(B,'igtr/alignments.tsv'),data=z))
  }
  sp<-store_chain_layout(sp,free=list(x=TRUE,y=TRUE))
  for(id in order_ids)sp<-set_panel_xlim(sp,individual=id,xlim=c(1,len[[id]]),xlim_chr=locus)
  p<-ggexon(sp)+geom_genetag(data=d,aes(xmin=xmin,xmax=xmax,y=y,strand=strand,track=track,fill=kind),show_label=FALSE,exon_height=P$gene_height,colour=NA)+geom_point(data=d,aes(x=(xmin+xmax)/2,y=y,colour=kind),size=P$marker_size,inherit.aes=FALSE,show.legend=FALSE)
  for(nm in pair_names)p<-p+geom_nuclink(alignment=nm,fill='grey55',alpha=P$link_alpha,colour=NA)
  annotations<-list()
  if(!is_mhc) {
    # Functional/ORF and species specificity are distinct source fields, below gene spans.
    orf<-d[d$functional=='ORF',];orf$ymin<-rep(-.35,nrow(orf));orf$ymax<-rep(-.05,nrow(orf))
    ss<-d[d$is_species_specific=='True',];ss$ymin<-rep(-.95,nrow(ss));ss$ymax<-rep(-.65,nrow(ss))
    if(nrow(orf))annotations$orf<-orf
    if(nrow(ss))annotations$species_specific<-ss
    if(nrow(s)){s$track<-s$track_id;s$species<-s$individual<-s$track;s$ymin<--1.55;s$ymax<--1.25;annotations$sv<-s}
  }
  qc<-list();for(nm in names(annotations)) {
    z<-annotations[[nm]];index<-length(p@layers)+1
    p<-p+geom_rect(data=z,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=if(nm=='species_specific')P$wong[5] else 'grey45',colour=NA,inherit.aes=FALSE)
    if(nm!='sv')p<-p+geom_point(data=z,aes(x=(xmin+xmax)/2,y=(ymin+ymax)/2),shape=if(nm=='orf')4 else 17,size=P$marker_size,colour=if(nm=='species_specific')P$wong[5] else 'grey45',inherit.aes=FALSE)
    qc[[nm]]<-list(layer=index,source=z)
  }
  summaries<-do.call(rbind,lapply(order_ids,function(id) {
    z<-d[d$track==id,];txt<-if(is_mhc)sprintf('F %d; pseudo %d',sum(z$functional=='functional'),sum(z$functional!='functional')) else {
      v<-z[z$gene_type=='V',];sprintf('V: F %s / ORF %s; SS %d (+? %d)',
      if(all(v$functional=='unknown'))'?' else sum(v$functional=='Functional'),
      if(all(v$functional=='unknown'))'?' else sum(v$functional=='ORF'),sum(z$is_species_specific=='True'),sum(z$is_species_specific=='unknown'))
    };data.frame(track=id,x=1,y=if(is_mhc)-.5 else -2.1,label=txt)
  }))
  p<-p+geom_text(data=summaries,aes(x=x,y=y,label=label),hjust=0,size=P$text_size_pt/ggplot2::.pt,family=P$family,inherit.aes=FALSE)
  labs_track<-setNames(paste(if(locus=='TRA')'TRA / TRD' else gsub('_',' ',locus),labels,sep=' | '),order_ids)
  limits<-setNames(lapply(order_ids,function(id)c(1,len[[id]])),order_ids)
  p<-p+scale_x_continuous(oob=scales::oob_keep)+scale_fill_manual(values=colors,drop=TRUE)+scale_colour_manual(values=colors,guide='none')+
    facet_genomics(vars(track),scales='free',ncol=1,xlim=limits,xlim_chr=setNames(as.list(rep(locus,length(order_ids))),order_ids),link_panel_height=.35,link_axis='none',link_strip='blank',labeller=labeller(track=function(x)ifelse(x %in% names(labs_track),labs_track[x],x)))+
    scale_panel_annotation('free_y',limits=setNames(rep(list(c(if(is_mhc)-1.2 else -3,1.65)),length(order_ids)),order_ids))+
    labs(x=if(is_mhc)'Position from first curated gene (kb)' else 'Position in extracted locus (kb)',y=NULL,fill=NULL)+
    theme_bw(base_family=P$family,base_size=P$text_size_pt)+theme(text=element_text(size=P$text_size_pt,family=P$family),axis.text=element_text(size=P$text_size_pt),strip.text=element_text(size=P$text_size_pt),legend.text=element_text(size=P$text_size_pt),legend.position='bottom',panel.grid=element_blank(),strip.background=element_rect(fill='white'),plot.title=element_blank(),plot.subtitle=element_blank(),plot.margin=margin(2,2,2,2))
  built<-ggexon_build(p);lay<-as.data.frame(built@layout$layout)
  stopifnot(sum(lay$panel_type=='annotation')==length(order_ids),!any(lay$panel_type=='coverage'),nrow(built@data[[1]])==nrow(d))
  link_qc<-list()
  for(j in seq_along(pair_names)) {
    nm<-pair_names[j];shown<-built@data[[j+2L]];owner<-lay[match(shown$PANEL,lay$PANEL),]
    pair<-pairwise_alignments(sp)[[nm]];expected<-links[links$tspecies==target_individual(pair) & links$qspecies==query_individual(pair),]
    stopifnot(length(unique(shown$group))==nrow(expected),all(table(shown$group)==4),all(is.finite(shown$x)),all(owner$panel_type=='link'),all(owner$tspecies==expected$tspecies[1]),all(owner$qspecies==expected$qspecies[1]))
    link_qc[[j]]<-data.frame(locus,pair=nm,input_blocks=nrow(expected),rendered_blocks=length(unique(shown$group)),owner_match=TRUE)
  }
  if(length(link_qc))wr(do.call(rbind,link_qc),paste0(output_name,'.link-qc.tsv'))
  checked<-list();for(nm in names(qc)) {
    info<-qc[[nm]];shown<-built@data[[info$layer]];owner<-lay[match(shown$PANEL,lay$PANEL),]
    stopifnot(nrow(shown)==nrow(info$source),all(shown$ymax<0),all(owner$panel_type=='annotation'),all(as.character(owner$track)==info$source$track))
    checked[[nm]]<-data.frame(locus,annotation=nm,rows=nrow(shown),owner_match=TRUE,below_gene=TRUE)
  }
  wr(lay,paste0(output_name,'.layout-qc.tsv'));if(length(checked))wr(do.call(rbind,checked),paste0(output_name,'.lane-qc.tsv'))
  wr(summaries,paste0(output_name,'.summaries.tsv'));wr(d,paste0(output_name,'.plotted-genes.tsv'))
  tab<-fix_axes(built);saveRDS(tab,file.path(O,paste0(output_name,'.gtable.rds')))
  ggsave(file.path(O,paste0(output_name,'.pdf')),tab,width=P$width_mm,height=if(full_mhc)310 else P$height_mm,units='mm',device=cairo_pdf)
  ggsave(file.path(O,paste0(output_name,'.png')),tab,width=P$width_mm,height=if(full_mhc)310 else P$height_mm,units='mm',dpi=P$dpi)
  message(locus,': ',nrow(d),' genes; ',if(is.null(links))0 else nrow(links),' real alignment blocks')
  tab
}
loci<-c('IGH','IGK','IGL','TRA','TRB','TRG','MHC_I','MHC_II')
args<-commandArgs(trailingOnly=TRUE);if(length(args))loci<-args
plots<-lapply(loci,make_plot)
if(length(plots)==8) {
  # Composite of eight independent ggexon facets preserves locus-specific coordinates.
  atlas<-gridExtra::arrangeGrob(grobs=plots,ncol=2)
  ggsave(file.path(O,'immune_combined.pdf'),atlas,width=P$width_mm,height=P$atlas_height_mm,units='mm',device=cairo_pdf,limitsize=FALSE)
  ggsave(file.path(O,'immune_combined.png'),atlas,width=P$width_mm,height=P$atlas_height_mm,units='mm',dpi=P$dpi,limitsize=FALSE)
}

if(length(plots)==8)invisible(lapply(c('MHC_I','MHC_II'),make_plot,full_mhc=TRUE))
