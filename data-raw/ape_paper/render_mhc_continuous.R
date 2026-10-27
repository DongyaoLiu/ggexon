#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(ggplot2);library(pkgload);library(grid)})
P<-list(text_size_pt=8,family='Times',width_mm=180,height_mm=560,dpi=150,export_background='white',
 gene_height=.5,data_linewidth=.25,axis_linewidth=.15,
 link_alpha=.3,lollipop_headroom=8,lollipop_head_width_mm=NULL,
 lollipop_head_layout='columns',anchor_tick_mm=.9,fan_height_mm=2.5,
 lane_spacing_mm=.65,lane_clearance_mm=.5,stem_linewidth=.22,stem_halo_mm=.12,
 head_top_margin_mm=1,link_height_mm=8,
 lollipop_head_height_mm=4,lollipop_padding_mm=.6,lollipop_tip_mm=1.8,
 lollipop_gap_mm=.8,lollipop_row_gap_mm=.8,lollipop_stem_height_mm=1.5,
 lollipop_max_shift_mm=25,lollipop_max_rows=Inf,
 vr_top=-1.0,vr_height=.32,class_y=-2.1,class_label_offset=.3,bottom_padding=.8,
 source_ortholog_colors='data-raw/ape_paper/source_metadata/mhc_continuous/colors.txt',
 context_grey='grey65',pseudo_grey='grey70',discordant_grey='grey85',
 wong=c('#E69F00','#56B4E9','#009E73','#CC79A7','#D55E00','#F0E442','#0072B2','#000000'),
 continuous_map='viridis',auto_quantile=TRUE)
# Original ED3 ortholog colors are an explicit user requirement and override Wong.
# Any continuous color introduced later must use equal-count finite-data quantiles.
if(!exists('mhc_dataset',inherits=FALSE))pkgload::load_all('.',quiet=TRUE)
B<-'data-raw/ape_paper/results/mhc_continuous'
# Optional subset/export inputs share the approved full-figure rendering path.
O<-Sys.getenv('GGEXON_MHC_OUTPUT',file.path(B,'figures'));dir.create(O,recursive=TRUE,showWarnings=FALSE)
selected_tracks<-strsplit(Sys.getenv('GGEXON_MHC_TRACKS',''),',',fixed=TRUE)[[1]]
subset_requested<-length(selected_tracks)>0L && nzchar(selected_tracks[1])
if(subset_requested) stopifnot(normalizePath(O,mustWork=FALSE)!=normalizePath(file.path(B,'figures'),mustWork=FALSE))
rd<-function(p)read.delim(p,check.names=FALSE,stringsAsFactors=FALSE)
wr<-function(d,f)write.table(d,file.path(O,f),sep='\t',quote=FALSE,row.names=FALSE)
# Tutorial mode consumes a complete, already subsetted data object. File mode
# remains available for reproducing the full figure and older entry points.
object_mode<-exists('mhc_dataset',inherits=FALSE)
if(object_mode) {
 w<-mhc_dataset$windows;m<-mhc_dataset$models;e<-mhc_dataset$exons
 cg<-mhc_dataset$mhc;links<-mhc_dataset$links;vr<-mhc_dataset$regions
 maxbp<-mhc_dataset$max_bp
} else {
w<-rd(file.path(B,'windows.tsv'));model_dir<-file.path(B,'gene_models_with_recovered_context')
stopifnot(file.exists(file.path(model_dir,'recovery_manifest.json')))
m<-rd(file.path(model_dir,'models.tsv'));e<-rd(file.path(model_dir,'exons.tsv'))
cg<-rd('data-raw/ape_paper/results/mhc_full/genes.tsv');links<-rd(Sys.getenv('GGEXON_MHC_LINKS',file.path(B,'links.tsv')));vr<-rd(file.path(B,'discordant_regions.tsv'))
# Preserve the original bp-to-mm scale even when the longest track is omitted.
maxbp<-max(w$length)
}
full_track_count<-if(object_mode)length(mhc_dataset$all_tracks) else nrow(w)
if(subset_requested) {
 stopifnot(!anyDuplicated(selected_tracks),all(selected_tracks %in% w$track))
 w<-w[match(selected_tracks,w$track),]
 m<-m[m$track %in% selected_tracks,];e<-e[e$track %in% selected_tracks,]
 cg<-cg[cg$track %in% selected_tracks,];vr<-vr[vr$track %in% selected_tracks,]
}
compact<-nrow(w)<full_track_count
expected_mhc_genes<-nrow(cg)
color_source<-read.delim(P$source_ortholog_colors,header=FALSE,stringsAsFactors=FALSE)
cc<-setNames(vapply(strsplit(color_source[[2]],','),function(x)rgb(as.numeric(x[1]),as.numeric(x[2]),as.numeric(x[3]),maxColorValue=255),character(1)),color_source[[1]])
palette<-c(cc[names(cc)!='p'],Pseudogene=unname(cc['p']),'Other gene'=P$context_grey)
key<-function(x)sub('^[^-]+-','',x)
# Choose the longest exonic source transcript at each overlapping named gene locus.
exonic_bp<-tapply(e$end0-e$start0,e$model_id,sum);m$exonic_bp<-unname(exonic_bp[m$model_id]);selected<-integer()
for(z in split(seq_len(nrow(m)),interaction(m$track,m$gene_symbol,m$strand,drop=TRUE))) {
 z<-z[order(m$chromStart[z])];cluster<-0;ends<--Inf;groups<-list()
 for(j in z){if(m$chromStart[j]>ends){cluster<-cluster+1;ends<-m$chromEnd[j]}else ends<-max(ends,m$chromEnd[j]);groups[[as.character(cluster)]]<-c(groups[[as.character(cluster)]],j)}
 for(v in groups)selected<-c(selected,v[which.max(m$exonic_bp[v])])
}
m<-m[selected,];e<-e[e$model_id %in% m$model_id,]
# Identify source exon models supported by curated MHC position+strand/name.
m$mhc_row<-NA_integer_
for(i in seq_len(nrow(cg))) {
 hit<-which(m$annotation_evidence=='released annotation' & m$track==cg$track[i] & m$strand==cg$strand[i] & m$chromStart<cg$end[i] & m$chromEnd>cg$start[i]-1)
 if(!length(hit))next
 overlap<-pmax(0,pmin(m$chromEnd[hit],cg$end[i])-pmax(m$chromStart[hit],cg$start[i]-1))
 exact<-key(m$gene_symbol[hit])==key(cg$gene_name[i]);score<-overlap/(cg$end[i]-cg$start[i]+1)+10*exact
 best<-hit[which.max(score)];if(max(score)>=.5)m$mhc_row[best]<-i
}
m$kind<-'Other gene';matched<-!is.na(m$mhc_row)
m$kind[matched]<-ifelse(cg$functionality[m$mhc_row[matched]]=='pseudogene','Pseudogene',key(cg$gene_name[m$mhc_row[matched]]))
stopifnot(all(m$kind %in% names(palette)))
cg$kind<-ifelse(cg$functionality=='pseudogene','Pseudogene',key(cg$gene_name));stopifnot(all(cg$kind %in% names(palette)))
cg$label<-paste0(key(cg$gene_name),ifelse(cg$functionality=='pseudogene','*',''))
# Native->display is translation, or reflection for reverse-oriented siamang;
# no per-track length rescaling or separate classI/classII origin is permitted.
transform_native<-function(d,a,b,trackcol='track') {
 z<-w[match(d[[trackcol]],w$track),];lo<-pmax(d[[a]],z$start0);hi<-pmin(d[[b]],z$end0)
 stopifnot(all(lo<hi));d$xmin<-ifelse(z$orientation=='+',lo-z$start0,z$end0-hi)
 d$xmax<-ifelse(z$orientation=='+',hi-z$start0,z$end0-lo)
 d$species<-d$individual<-d$track<-d[[trackcol]];d
}
m<-transform_native(m,'chromStart','chromEnd');ew<-w[match(e$track,w$track),];e<-e[e$end0>ew$start0 & e$start0<ew$end0,];e<-transform_native(e,'start0','end0')
e$kind<-m$kind[match(e$model_id,m$model_id)];cg$start0<-cg$start-1;cg$end0<-cg$end;cg<-transform_native(cg,'start0','end0')
lims<-setNames(rep(list(c(0,maxbp)),nrow(w)),w$track);chr<-setNames(as.list(rep('MHC',nrow(w))),w$track)
pair_names<-if(nrow(w)>1L)paste(head(w$track,-1L),tail(w$track,-1L),sep='__') else character()
if(object_mode) {
 sp<-mhc_dataset$species
 stopifnot(identical(individual_names(sp),w$track),identical(names(pairwise_alignments(sp)),pair_names))
} else {
sp<-SynSpecies(name='continuous_MHC')
for(id in w$track) {
 z<-cg[cg$track==id,];f<-file.path(O,paste0(id,'.gff3'))
 gf<-data.frame('MHC','curated','gene',z$xmin+1,z$xmax,'.',ifelse(w$orientation[match(id,w$track)]=='-',ifelse(z$strand=='+','-','+'),z$strand),'.',paste0('ID=mhc',seq_len(nrow(z))))
 writeLines('##gff-version 3',f);write.table(gf,f,append=TRUE,sep='\t',quote=FALSE,row.names=FALSE,col.names=FALSE)
 sp<-add_individual(sp,load_annotation(SynIndividual(id=id,annotation_file=f,annotation_format='gff')))
}
pair_names<-character()
for(i in seq_len(nrow(w)-1)) {
 t<-w$track[i];q<-w$track[i+1];z<-links[links$tspecies==t & links$qspecies==q,];stopifnot(nrow(z)>0)
 nm<-paste(t,q,sep='__');pair_names<-c(pair_names,nm)
 # PAF coordinates are display-oriented sequence coordinates. Subtract the
 # converted closed-interval start so plotting uses native BED0 endpoint lengths.
 z$tstart<-z$tstart-1;z$qstart<-z$qstart-1
 sp<-add_pairwise_alignment(sp,SynPairAlignment(nm,target_individual=t,query_individual=q,file=file.path(B,'links.tsv'),data=z))
}
}
sp<-store_chain_layout(sp,free=list(x=TRUE,y=TRUE))
for(id in w$track)sp<-set_panel_xlim(sp,individual=id,xlim=c(0,maxbp),xlim_chr='MHC')
# Use one curated native arrow and one enlarged head for each MHC gene.
# Source transcript models matching curated MHC records remain excluded,
# so transcript extents cannot duplicate or replace the curated gene spans.
m$duplicate_curated_rows<-vapply(seq_len(nrow(m)),function(i)paste(which(cg$track==m$track[i] & cg$strand==m$strand[i] & key(cg$gene_name)==key(m$gene_symbol[i]) & cg$start0<m$chromEnd[i] & cg$end0>m$chromStart[i]),collapse=','),character(1))
m$native_mhc_model<-m$kind!='Other gene' | nzchar(m$duplicate_curated_rows)
m$native_mhc_reason<-ifelse(m$kind!='Other gene','supported curated MHC model',ifelse(nzchar(m$duplicate_curated_rows),'same ortholog, track, strand and overlapping curated interval','retained source context'))
context_models<-m[!m$native_mhc_model,]
gene_arrows<-data.frame(track=context_models$track,species=context_models$species,individual=context_models$individual,xmin=context_models$xmin,xmax=context_models$xmax,ymin=-P$gene_height,
 transcripts=context_models$model_id,strand=context_models$strand,kind=context_models$kind,native_start0=context_models$chromStart,native_end0=context_models$chromEnd,source='source representative context model',annotation_evidence=context_models$annotation_evidence,projection_source_model_id=context_models$projection_source_model_id,projection_coverage=context_models$projection_coverage,projection_identity=context_models$projection_identity,recovery_method=context_models$recovery_method)
reverse_genes<-w$orientation[match(gene_arrows$track,w$track)]=='-'
gene_arrows$strand[reverse_genes]<-ifelse(gene_arrows$strand[reverse_genes]=='+','-','+')
stopifnot(!anyDuplicated(gene_arrows$transcripts),all(gene_arrows$kind=='Other gene'))
cg$true_midpoint<-(cg$xmin+cg$xmax)/2;cg$anchor_y<-0
cg$display_oriented_strand<-cg$strand
reverse_mhc<-w$orientation[match(cg$track,w$track)]=='-'
cg$display_oriented_strand[reverse_mhc]<-ifelse(cg$display_oriented_strand[reverse_mhc]=='+','-','+')
cg$gene_label<-cg$label
# Native arrow widths are true BED0 spans. Identity colors are independent of
# pseudogene status, which remains marked by grey enlarged heads and asterisks.
native_mhc_arrows<-data.frame(track=cg$track,species=cg$track,individual=cg$track,
 xmin=cg$xmin,xmax=cg$xmax,ymin=-P$gene_height,ymax=0,
 transcripts=paste0('curated_native_mhc_',seq_len(nrow(cg))),
 strand=cg$display_oriented_strand,gene_name=cg$gene_name,functionality=cg$functionality,
 native_start0=cg$start0,native_end0=cg$end0,ortholog_fill=unname(cc[key(cg$gene_name)]),
 span_source='curated MHC gene span; no transcript structure inferred')
missing_ortholog_color<-is.na(native_mhc_arrows$ortholog_fill)
stopifnot(all(native_mhc_arrows$functionality[missing_ortholog_color]=='pseudogene'))
native_mhc_arrows$ortholog_fill[missing_ortholog_color]<-unname(cc['p'])
native_mhc_arrows$color_basis<-ifelse(missing_ortholog_color,
 'source pseudogene grey; no separate ortholog color defined','source ortholog color')
stopifnot(nrow(native_mhc_arrows)==expected_mhc_genes,!anyNA(native_mhc_arrows$ortholog_fill))

# Explicit layout metadata: preserve source identities and status independently.
# Six physical rows are shared across all tracks; blank slots are not loss calls.
shared_rows<-c(F=6,V=5,P=4,G=3,H=2,T=1,K=6,U=5,A=4,W=3,J=2,L=1,
 N=6,E=5,C=4,B=3,S=2,X=1,DRA=6,DRB9=5,DRB5=4,DRB6=3,DRB1=2,DQA1=1,
 DQB1=6,DQA2=5,DQB2=4,DOB=3,Z=2,DMB=1,DMA=6,DOA=5,DPA1=4,DPB1=3,DPA2=2,DPB2=1,DPA3=6,
 Ap=2,OKO=1,DRB3=2,DRB=3)
cg$source_gene_key<-key(cg$gene_name)
cg$head_row<-unname(shared_rows[cg$source_gene_key]);stopifnot(!anyNA(cg$head_row))
wr(data.frame(source_gene_key=names(shared_rows),head_row=unname(shared_rows),
 assignment_basis=ifelse(names(shared_rows) %in% c('Ap','OKO','DRB3','DRB'),
 'explicit ape-only layout slot','CHM13 six-row layout slot')),'mhc-head-row-template.tsv')
layout_records<-list();render_device<-'build'
record_layout<-function(dataframe) {
 origin<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE)
 dataframe$device_left_mm<-origin$x*25.4;dataframe$device_bottom_mm<-origin$y*25.4
 dataframe$source_gene_key<-sub('\\*$','',dataframe$label)
 dataframe$device<-render_device
 layout_records[[length(layout_records)+1L]]<<-dataframe
}
p<-ggexon(sp)+geom_gene(data=gene_arrows,aes(xmin=xmin,xmax=xmax,ymin=ymin,transcripts=transcripts,strand=strand,track=track,fill=kind),exon_height=P$gene_height,colour=NA,inherit.aes=FALSE)
gene_index<-1L
native_mhc_index<-length(p@layers)+1L
# AsIs colors bypass scale training, preserving the existing head legend and
# its physical dimensions while coloring native pseudogene arrows by ortholog.
p<-p+geom_gene(data=native_mhc_arrows,
 aes(xmin=xmin,xmax=xmax,ymin=ymin,transcripts=transcripts,strand=strand,track=track,fill=I(ortholog_fill)),
 exon_height=P$gene_height,colour=NA,show.legend=FALSE,inherit.aes=FALSE)
lollipop_index<-length(p@layers)+1L
p<-p+geom_gene_lollipop(data=cg,aes(x=true_midpoint,y=anchor_y,strand=display_oriented_strand,label=gene_label,fill=kind,track=track,head_row=head_row),
 headroom=P$lollipop_headroom,head_width_mm=P$lollipop_head_width_mm,
 head_height_mm=P$lollipop_head_height_mm,padding_mm=P$lollipop_padding_mm,
 tip_mm=P$lollipop_tip_mm,gap_mm=P$lollipop_gap_mm,row_gap_mm=P$lollipop_row_gap_mm,
 stem_height_mm=P$lollipop_stem_height_mm,max_shift_mm=P$lollipop_max_shift_mm,
 max_rows=P$lollipop_max_rows,head_layout=P$lollipop_head_layout,
 anchor_tick_mm=P$anchor_tick_mm,fan_height_mm=P$fan_height_mm,
 lane_spacing_mm=P$lane_spacing_mm,lane_clearance_mm=P$lane_clearance_mm,
 stem_linewidth=P$stem_linewidth,stem_halo_mm=P$stem_halo_mm,layout_callback=record_layout,
 size=P$text_size_pt/ggplot2::.pt,family=P$family,linewidth=P$data_linewidth,inherit.aes=FALSE)
link_index<-integer()
for(nm in pair_names){link_index<-c(link_index,length(p@layers)+1L);p<-p+geom_nuclink(alignment=nm,fill='grey60',alpha=P$link_alpha,colour=NA)}
# The three variable regions share one lane below the genomic anchors.
vr_top<-P$vr_top
vr$species<-vr$individual<-vr$track;vr$xmin<-vr$display_start0;vr$xmax<-vr$display_end0
vr$ymax<-vr_top;vr$ymin<-vr$ymax-P$vr_height
vr_index<-length(p@layers)+1L
p<-p+geom_rect(data=vr,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=P$discordant_grey,colour=NA,inherit.aes=FALSE)
vrlabels<-do.call(rbind,lapply(split(vr,interaction(vr$track,vr$region,drop=TRUE)),function(z)data.frame(track=z$track[1],x=(min(z$xmin)+max(z$xmax))/2,y=z$ymax[1]-.15,label=z$region[1])))
p<-p+geom_text(data=vrlabels,aes(x=x,y=y,label=label),size=P$text_size_pt/.pt,family=P$family,inherit.aes=FALSE)
# MHC-class spans are annotations underneath models; they retain the actual inter-class gap.
class_spans<-do.call(rbind,lapply(split(cg,interaction(cg$track,cg$locus,drop=TRUE)),function(z)data.frame(track=z$track[1],xmin=min(z$xmin),xmax=max(z$xmax),label=gsub('MHC_','',z$locus[1]))))
class_spans$y<-P$class_y
p<-p+geom_segment(data=class_spans,aes(x=xmin,xend=xmax,y=y,yend=y),colour='grey55',linewidth=P$data_linewidth,inherit.aes=FALSE)+
 geom_text(data=class_spans,aes(x=(xmin+xmax)/2,y=y-P$class_label_offset,label=label),size=P$text_size_pt/.pt,family=P$family,inherit.aes=FALSE)
miny<-min(class_spans$y)-P$bottom_padding
coverage<-table(factor(m$track,levels=w$track));evidence_by_track<-setNames(vapply(w$track,function(id){z<-unique(m$annotation_evidence[m$track==id]);stopifnot(length(z)==1);z},character(1)),w$track)
context_labels<-ifelse(evidence_by_track[w$track]=='same-species Liftoff projection','context projected','context annotated')
striplabels<-setNames(paste0(w$label,'\n',w$chr,ifelse(w$orientation=='-',' (reverse)',''),paste0('\n',context_labels)),w$track)
p<-p+scale_x_continuous(limits=c(0,maxbp),breaks=seq(0,5e6,1e6),labels=function(x)x/1e6,oob=scales::oob_keep,expand=expansion(mult=c(.01,.01)))+
 scale_fill_manual(values=palette,breaks=names(palette),drop=TRUE)+scale_colour_manual(values=palette,guide='none')+
 facet_genomics(vars(track),scales='free',ncol=1,strip.position='left',xlim=lims,xlim_chr=chr,link_panel_height=.35,link_axis='none',link_strip='blank',labeller=labeller(track=function(x)ifelse(x %in% names(striplabels),striplabels[x],x)))+
 scale_panel_annotation('free_y',limits=setNames(rep(list(c(miny,P$lollipop_headroom)),nrow(w)),w$track))+
 labs(x='Genomic coordinate (bp; native assembly, common bp scale)',y=NULL,fill='MHC ortholog / annotation',caption=if(compact) 'Human CHM13; gorilla h1/h2 differ in MHC heads; siamang h1 represents matching h1/h2 head profiles.\nSmall arrows: true genomic spans; enlarged heads: shared rows; * pseudogene. Grey: context genes.\nVR1-3: published CHM13 intervals or chain-supported projections; ribbons: direct nucleotide alignments.\nContext includes labelled same-species projections; profiles describe these sampled assemblies.' else 'Small arrows retain true spans and ortholog colors; enlarged heads retain shared rows.\nEmpty head slots do not establish gene absence.\nVR1–VR3: published CHM13 regions; ape bars show chain-supported projections.\n* pseudogene. Projected context: same-species Liftoff; exon coverage and identity >=95%.\nFive alternatives recovered at >=95%; nine transfers remain unresolved.\nRecoveries use shorter source isoforms; RNA support is partial and not haplotype-resolved.\nRegion boundaries and adjacent alignments are reconstructed; see the source notes.')+
 theme_ggexon_track(base_family=P$family,base_size=P$text_size_pt,show_x_grid=FALSE,show_legend=TRUE)+theme_ggexon_side_strips('left',base_size=P$text_size_pt)+theme(text=element_text(size=P$text_size_pt,family=P$family),axis.text=element_text(size=P$text_size_pt),strip.text=element_text(size=P$text_size_pt),legend.text=element_text(size=P$text_size_pt),legend.title=element_text(size=P$text_size_pt),plot.caption=element_text(size=P$text_size_pt,hjust=0),legend.position='bottom',legend.location='plot',legend.justification='left',legend.box.just='left',plot.caption.position='plot',legend.key.size=unit(2.5,'mm'),panel.grid=element_blank(),panel.border=element_blank(),strip.background=element_blank(),plot.title=element_blank(),plot.subtitle=element_blank(),plot.margin=margin(2,2,2,2))+guides(fill=guide_legend(nrow=4,byrow=TRUE))
stopifnot(inherits(p@theme$panel.border,'element_blank'),inherits(p@theme$strip.background,'element_blank'),p@facet$params$strip.position=='left')
wr(data.frame(theme='theme_ggexon_track + theme_ggexon_side_strips',strip_side='left',panel_border=FALSE,strip_background=FALSE,gene_geom='geom_gene',mhc_geom='geom_gene_lollipop',native_mhc_geom='geom_gene',native_mhc_colors='ortholog identity; status in enlarged heads',vr_shared_y=TRUE,headroom=P$lollipop_headroom,max_shift_mm=P$lollipop_max_shift_mm,head_gap_mm=P$lollipop_gap_mm,head_row_gap_mm=P$lollipop_row_gap_mm,head_linewidth_mm=P$data_linewidth,padding_mm=P$lollipop_padding_mm,tip_mm=P$lollipop_tip_mm,head_layout=P$lollipop_head_layout,head_rows='fixed by source gene key',anchor_tick_mm=P$anchor_tick_mm,fan_height_mm=P$fan_height_mm,stem_linewidth_mm=P$stem_linewidth,lane_spacing_mm=P$lane_spacing_mm,lane_clearance_mm=P$lane_clearance_mm,context_bottom_y=-P$gene_height,context_top_y=0),'style-qc.tsv')
built<-ggexon_build(p);lay<-as.data.frame(built@layout$layout);stopifnot(sum(lay$panel_type=='annotation')==nrow(w),sum(lay$panel_type=='link')==nrow(w)-1L,!any(lay$panel_type=='coverage'))
# Export assertions establish equal geometry, not merely equal printed ticks.
scales_qc<-do.call(rbind,lapply(which(lay$panel_type=='annotation'),function(i){z<-lay[i,];lim<-built@layout$panel_scales_x[[z$SCALE_X]]$get_limits();data.frame(track=z$track,min=lim[1],max=lim[2],bp_per_axis_unit=diff(c(0,maxbp))/diff(lim))}))
stopifnot(length(unique(round(scales_qc$bp_per_axis_unit,8)))==1)
anchor_data<-built@data[[lollipop_index]];anchor_owner<-lay[match(anchor_data$PANEL,lay$PANEL),]
stopifnot(nrow(anchor_data)==nrow(cg),all(as.character(anchor_owner$track)==cg$track),all(anchor_data$x==cg$true_midpoint),all(anchor_data$y==cg$anchor_y),all(anchor_data$strand==cg$display_oriented_strand),all(anchor_data$label==cg$gene_label),all(anchor_data$head_row==cg$head_row))
geometry_qc<-do.call(rbind,lapply(w$track,function(id){ii<-which(cg$track==id);native<-(cg$xmin[ii]+cg$xmax[ii])/2;shown<-anchor_data$x[ii];slope<-cov(native,shown)/var(native);data.frame(track=id,built_units_per_native_bp=slope,intercept=mean(shown)-slope*mean(native),max_affine_error=max(abs(shown-(slope*native+mean(shown)-slope*mean(native)))))}))
stopifnot(length(unique(round(geometry_qc$built_units_per_native_bp,10)))==1,all(geometry_qc$max_affine_error<1e-5))
wr(geometry_qc,'native-geometry-qc.tsv')
shown<-built@data[[vr_index]];owner<-lay[match(shown$PANEL,lay$PANEL),]
stopifnot(nrow(shown)==nrow(vr),all(shown$ymax<0),all(owner$panel_type=='annotation'),all(as.character(owner$track)==vr$track),length(unique(shown$ymin))==1,length(unique(shown$ymax))==1)
wr(data.frame(track=vr$track,region=vr$region,ymin=shown$ymin,ymax=shown$ymax),'vr-lane-qc.tsv')
gene_shown<-built@data[[gene_index]];gi<-match(gene_shown$transcripts,gene_arrows$transcripts)
stopifnot(nrow(gene_shown)==nrow(gene_arrows),!anyNA(gi),all(gene_shown$Xmin==gene_arrows$xmin[gi]),all(gene_shown$Xmax==gene_arrows$xmax[gi]),all(gene_shown$strand==gene_arrows$strand[gi]),all(gene_shown$ymax==0),all(gene_shown$ymin==-P$gene_height))
native_shown<-built@data[[native_mhc_index]];ni<-match(native_mhc_arrows$transcripts,native_shown$transcripts)
stopifnot(nrow(native_shown)==expected_mhc_genes,!anyNA(ni),
 all(native_shown$Xmin[ni]==native_mhc_arrows$xmin),all(native_shown$Xmax[ni]==native_mhc_arrows$xmax),
 all(native_shown$strand[ni]==native_mhc_arrows$strand),all(native_shown$ymin[ni]==-P$gene_height),
 all(native_shown$ymax[ni]==0),all(ggexon:::to_lower_ascii(native_shown$fill[ni])==ggexon:::to_lower_ascii(native_mhc_arrows$ortholog_fill)))
native_mhc_arrows$rendered_fill<-as.character(native_shown$fill[ni])
wr(native_mhc_arrows,'plotted-native-mhc-arrows.tsv')
wr(gene_arrows,'plotted-gene-arrows.tsv');wr(m[m$native_mhc_model,],'excluded-native-mhc-models.tsv');wr(class_spans,'class-spans.tsv')
link_qc<-list()
for(j in seq_along(link_index)){
 z<-built@data[[link_index[j]]];owner<-lay[match(z$PANEL,lay$PANEL),];t<-w$track[j];q<-w$track[j+1];n<-sum(links$tspecies==t & links$qspecies==q)
 stopifnot(length(unique(z$group))==n,all(table(z$group)==4),all(is.finite(z$x)),all(owner$tspecies==t),all(owner$qspecies==q),all(owner$panel_type=='link'))
 link_qc[[j]]<-data.frame(target=t,query=q,input_blocks=n,rendered_blocks=length(unique(z$group)))
}
wr(lay,'layout-qc.tsv');wr(scales_qc,'common-bp-scale-qc.tsv');wr(do.call(rbind,link_qc),'link-qc.tsv');wr(m,'representative-models.tsv');wr(e,'source-representative-exons.tsv');wr(cg,'plotted-mhc-genes.tsv');wr(data.frame(track=cg$track,label=cg$gene_label,x=anchor_data$x,y=anchor_data$y,strand=anchor_data$strand,fill=anchor_data$fill,PANEL=anchor_data$PANEL),'mhc-anchors.tsv');wr(data.frame(track=w$track,source_gene_models=as.numeric(coverage),annotation_evidence=unname(evidence_by_track[w$track]),context_gene_arrows=as.numeric(table(factor(gene_arrows$track,levels=w$track))),curated_genes=as.numeric(table(factor(cg$track,levels=w$track)))),'annotation-coverage.tsv')
tab<-ggexon_gtable(built)
# Axis grobs retain the shared display-bp geometry, but label the actual
# one-based native assembly coordinates. Reverse-oriented tracks count down.
axis_qc<-list()
for(i in seq_len(nrow(lay))) {
 r<-lay[i,];if(r$panel_type!='annotation')next
 k<-which(tab$layout$name==paste0('axis-l-',r$ROW,'-',r$COL));if(length(k))tab$grobs[[k]]<-nullGrob()
 k<-which(tab$layout$name==paste0('axis-b-',r$COL,'-',r$ROW));if(!length(k))next
 native<-w[match(r$track,w$track),]
 ticks<-seq(ceiling((native$start0+1)/1e6)*1e6,floor(native$end0/1e6)*1e6,by=1e6)
 display_bp<-if(native$orientation=='+')ticks-1-native$start0 else native$end0-ticks
 stopifnot(all(display_bp>=0 & display_bp<=native$length))
 roundtrip<-if(native$orientation=='+')display_bp+native$start0+1 else native$end0-display_bp
 stopifnot(identical(as.numeric(roundtrip),as.numeric(ticks)))
 pp<-built@layout$panel_params[[as.integer(r$PANEL)]];tr<-built@layout$panel_scales_x[[r$SCALE_X]]$get_limits()
 pos<-(tr[1]+display_bp/maxbp*diff(tr)-pp$x.range[1])/diff(pp$x.range)
 # Only the axis baseline remains: the ggexon theme removes panel/strip boxes.
 end_pos<-(tr[1]+native$length/maxbp*diff(tr)-pp$x.range[1])/diff(pp$x.range)
 start_pos<-(tr[1]-pp$x.range[1])/diff(pp$x.range)
 tab$grobs[[k]]<-grobTree(
  segmentsGrob(x0=start_pos,x1=end_pos,y0=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(col='grey55',lwd=P$axis_linewidth)),
  segmentsGrob(x0=pos,x1=pos,y0=unit(1,'npc'),y1=unit(1,'npc')-unit(.7,'mm'),gp=gpar(col='grey35',lwd=P$axis_linewidth)),
  textGrob(format(ticks,big.mark=',',scientific=FALSE,trim=TRUE),x=pos,y=unit(1,'npc')-unit(1,'mm'),vjust=1,gp=gpar(fontfamily=P$family,fontsize=P$text_size_pt)))
 axis_qc[[length(axis_qc)+1L]]<-data.frame(track=r$track,chr=native$chr,orientation=native$orientation,native_tick=ticks,display_bp=display_bp,npc=pos,coordinate_system='1-based native assembly')
}
wr(do.call(rbind,axis_qc),'native-axis-qc.tsv')
if(file.exists(file.path(O,'plotted-exons.tsv')))unlink(file.path(O,'plotted-exons.tsv'))
# Measure real PDF label metrics and column counts once on a roomy device.
# Then give each annotation track only the physical height its heads need.
measure_file<-tempfile(fileext='.pdf');render_device<-'measure'
ggsave(measure_file,tab,width=P$width_mm,height=1000,units='mm',device=cairo_pdf,bg=P$export_background)
measured<-do.call(rbind,layout_records);unlink(measure_file)
height_qc<-list()
for(i in seq_len(nrow(lay))) {
 r<-lay[i,];k<-which(tab$layout$name==paste0('panel-',r$COL,'-',r$ROW));stopifnot(length(k)==1L)
 stopifnot(tab$layout$t[k]==tab$layout$b[k]);height_row<-tab$layout$t[k]
 if(r$panel_type=='annotation') {
  z<-measured[measured$track==r$track,]
  fraction<-(z$headroom_limit_mm[1]-z$anchor_y_mm[1])/z$panel_height_mm[1]
  needed<-max(z$top_mm-z$anchor_y_mm)+P$data_linewidth/2+P$head_top_margin_mm
  mm<-ceiling(needed/fraction*10)/10
  tab$heights[height_row]<-unit(mm,'mm')
  height_qc[[length(height_qc)+1L]]<-data.frame(track=r$track,rows=max(z$row),columns=max(z$column),height_mm=mm)
 } else tab$heights[height_row]<-unit(P$link_height_mm,'mm')
}
# Resolve fixed text/axis/legend heights on the same publication device.
measure_file<-tempfile(fileext='.pdf');cairo_pdf(measure_file,width=P$width_mm/25.4,height=1000/25.4)
P$height_mm<-convertHeight(sum(tab$heights),'mm',valueOnly=TRUE)
dev.off();unlink(measure_file)
wr(do.call(rbind,height_qc),'annotation-height-qc.tsv')
wr(data.frame(width_mm=P$width_mm,height_mm=P$height_mm),'figure-dimensions.tsv')
layout_records<-list()
saveRDS(tab,file.path(O,'mhc_continuous.gtable.rds'))
render_device<-'pdf'
ggsave(file.path(O,'mhc_continuous.pdf'),tab,width=P$width_mm,height=P$height_mm,units='mm',device=cairo_pdf,bg=P$export_background)
render_device<-'png'
ggsave(file.path(O,'mhc_continuous.png'),tab,width=P$width_mm,height=P$height_mm,units='mm',dpi=P$dpi,bg=P$export_background)
stopifnot(length(layout_records)>0L)
wr(do.call(rbind,layout_records),'mhc-lollipop-layout.tsv')
if(file.exists(file.path(O,'mhc-labels.tsv')))unlink(file.path(O,'mhc-labels.tsv'))
message('PASS: ',nrow(w),' native tracks; ',nrow(w)-1L,' link panels; ',nrow(links),' ribbons; ',nrow(m),' representative gene models; ',nrow(cg),' curated MHC genes; ',nrow(vr),' exact source/projection segments; enlarged MHC heads=',nrow(cg),'; native MHC arrows=',nrow(native_mhc_arrows))
