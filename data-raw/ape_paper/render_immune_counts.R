#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(ggplot2))
P<-list(text_size_pt=8,family='Times',width_mm=180,height_mm=115,dpi=180,marker_size=.5,data_linewidth=.3,axis_linewidth=.2,
 wong=c('#E69F00','#56B4E9','#009E73','#CC79A7','#D55E00','#F0E442','#0072B2','#000000'),continuous_map='viridis',auto_quantile=TRUE)
# Finite counts are coloured by equal-count quantile bands.
B<-'data-raw/ape_paper/results';O<-file.path(B,'immune_atlas')
g<-read.delim(file.path(B,'igtr/paper_genes.tsv'));m<-read.delim(file.path(B,'mhc_full/genes_ed3_scope.tsv'))
ids<-c('PPA1','PPA2','HSA1','GGO1','GGO2','PAB1','PAB2','PPY1','PPY2')
group<-c('IGHV','IGKV','IGLV','TRAV + TRDV','TRBV','TRGV','MHC I','MHC II')
d<-expand.grid(track=ids,group=group,stringsAsFactors=FALSE);d$count<-NA_real_
for(i in seq_len(nrow(d))) {
 id<-d$track[i];k<-d$group[i]
 if(grepl('MHC',k)) {z<-m[sub('h','',m$track,fixed=TRUE)==id & m$locus==gsub(' ','_',k),];d$count[i]<-sum(z$functionality=='functional')}
 else {fam<-if(k=='TRAV + TRDV')c('TRAV','TRAV/DV','TRDV') else k;z<-g[g$track_id==id & g$gene_family %in% fam,];if(nrow(z)&&!all(z$functional=='unknown'))d$count[i]<-sum(z$functional=='Functional')}
}
d$label<-ifelse(is.na(d$count),'NA',d$count)
x<-d$count[is.finite(d$count)];cuts<-unique(quantile(x,seq(0,1,length.out=6),names=FALSE))
d$band<-cut(d$count,cuts,include.lowest=TRUE,ordered_result=TRUE)
d$text_colour<-ifelse(!is.na(d$count) & d$count>cuts[length(cuts)-2],'white','black')
d$track<-factor(d$track,levels=rev(ids));d$group<-factor(d$group,levels=group)
p<-ggplot(d,aes(x=group,y=track))+geom_tile(aes(fill=band),colour='white',linewidth=P$axis_linewidth)+geom_text(aes(label=label,colour=text_colour),family=P$family,size=P$text_size_pt/.pt)+
 scale_colour_identity()+scale_fill_viridis_d(option=P$continuous_map,na.value='grey93',direction=-1,name='Count bands')+
 labs(x=NULL,y=NULL)+theme_bw(base_family=P$family,base_size=P$text_size_pt)+theme(text=element_text(size=P$text_size_pt),axis.text=element_text(size=P$text_size_pt,colour='black'),panel.grid=element_blank(),legend.position='bottom',legend.text=element_text(size=P$text_size_pt),legend.title=element_text(size=P$text_size_pt),plot.title=element_blank(),plot.subtitle=element_blank())
write.table(d,file.path(O,'functional_gene_counts.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
means<-aggregate(count~group,data=d,FUN=mean);write.table(means,file.path(O,'functional_gene_means.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
ggsave(file.path(O,'functional_gene_counts.pdf'),p,width=P$width_mm,height=P$height_mm,units='mm',device=cairo_pdf)
ggsave(file.path(O,'functional_gene_counts.png'),p,width=P$width_mm,height=P$height_mm,units='mm',dpi=P$dpi)
