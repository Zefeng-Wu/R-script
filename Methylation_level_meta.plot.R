require(GenomicFeatures)
require(GenomicRanges)
require(rtracklayer)



Average_signal_over_intervals<-function(methy_bed,gene_window_bed){
    gene_window_bed$average_score<-0.0
    
    overlaps <- findOverlaps(methy_bed, gene_window_bed)
    if (length(overlaps)>0){
    sites <- methy_bed[queryHits(overlaps)]
    bin_signal <- aggregate(score(sites), list(subjectHits(overlaps)), mean)
    gene_window_bed$average_score[bin_signal[,1]]<-bin_signal[,2]
    out_vector<-ifelse(strand(gene_window_bed)=="+",gene_window_bed$average_score,rev(gene_window_bed$average_score))
    }
    else{
      out_vector<-gene_window_bed$average_score
    }
    return(abs(out_vector))
    }
  

meta_plot<-function(gene_bed,object_bed,bin_number=20,flank_length=1000){
  #body
  require(GenomicFeatures)
  require(GenomicRanges)
  require(rtracklayer)
  message('Performing gene body analysis!')
  tg<-tile(gene_bed,n=bin_number)
  body_df<-mclapply(1:length(tg),function(x) Average_signal_over_intervals(methy_bed = methy_data,gene_window_bed = tg[[x]]),mc.cores = 8)
  
  body_df<-t(as.data.frame(body_df))
  rownames(body_df)<-NULL
  body_df<-body_df*50/(width(genes)/bin_number)  # 20 shoud be bin width of up streams
  
  ##upstream
  message('Performing upstream analysis!')
  up_1kb <- flank(gene_bed,width = flank_length,both = FALSE)
  tg<-tile(up_1kb,n = bin_number)
  all_genes_bin_count_up_1kb<-mclapply(1:length(tg),function(x) Average_signal_over_intervals(methy_bed = methy_data,gene_window_bed = tg[[x]]),mc.cores = 8)
  up_df<-t(as.data.frame(all_genes_bin_count_up_1kb))
  rownames(up_df)<-NULL
  
  ##downstream
  message('Performing downstream analysis!')
  down_1kb <- flank(genes,width = flank_length,both = FALSE,start = FALSE)
  tg<-tile(down_1kb,n = bin_number)
  #names(tg)<-names(down_1kb)
  all_genes_bin_count_down_1kb<-mclapply(1:length(tg),function(x) Average_signal_over_intervals(methy_bed = methy_data,gene_window_bed = tg[[x]]),mc.cores = 8)
  down_df<-t(as.data.frame(all_genes_bin_count_down_1kb))
  rownames(down_df)<-NULL
  
  
  ###merge three parts
  dd<-as.data.frame(do.call("cbind", list(up_df, body_df, down_df)))
  return(colMeans(dd))
}

tr<-makeTxDbFromGFF("~/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
genes<-genes(tr)
coding_genes<-read.table("~/MyResearch/genome.db/TAIR/gtf/protein_gene_ids",stringsAsFactors = FALSE)$V1
genes<-genes[coding_genes,]

### methylation
methy_data<-import.gff("/home/wuzefeng/MyResearch/methylation_database/tair/ara.meth.gff3")
seqlevels(methy_data)<-c("1","2","3","4","5")
strand(methy_data)<-ifelse(methy_data$score>0,"+","-")

###### CpG  mrthylation
cpg_meth<-methy_data[methy_data$source=="cg"]

dd<-meta_plot(gene_bed = genes,object_bed = methy_data,bin_number = 20,flank_length = 1000)



