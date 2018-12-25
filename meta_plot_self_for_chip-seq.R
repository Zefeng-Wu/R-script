meta_plot<-function(gene_bed,object_bed,bin_number){
  #body
  require(GenomicFeatures)
  require(GenomicRanges)
  require(rtracklayer)
  tg<-tile(gene_bed,n=bin_number)
  body_df<-mclapply(1:length(tg),function(x) ifelse(strand(tg[[x]])=="+",countOverlaps(query = tg[[x]],subject = object_bed),rev(countOverlaps(query = tg[[x]],subject = object_bed))),mc.cores = 8)
  body_df<-t(as.data.frame(body_df))
  rownames(body_df)<-NULL
  body_df<-body_df*50/(width(genes)/20)  # 20 shoud be bin width of up streams
  
  ##upstream
  up_1kb <- flank(genes,width = 1000,both = FALSE)
  tg<-tile(up_1kb,n = bin_number)
  all_genes_bin_count_up_1kb<-mclapply(1:length(tg),function(x) ifelse(strand(tg[[x]])=="+",countOverlaps(query = tg[[x]],subject = object_bed),rev(countOverlaps(query = tg[[x]],subject = object_bed))),mc.cores = 8)
  up_df<-t(as.data.frame(all_genes_bin_count_up_1kb))
  rownames(up_df)<-NULL
  
  ##downstream
  down_1kb <- flank(genes,width = 1000,both = FALSE,start = FALSE)
  tg<-tile(down_1kb,n = bin_number)
  names(tg)<-names(down_1kb)
  all_genes_bin_count_down_1kb<-mclapply(1:length(tg),function(x) ifelse(strand(tg[[x]])=="+",countOverlaps(query = tg[[x]],subject = object_bed),rev(countOverlaps(query = tg[[x]],subject = object_bed))),mc.cores = 8)
  down_df<-t(as.data.frame(all_genes_bin_count_down_1kb))
  rownames(down_df)<-NULL
  
  
  ###merge three parts
  dd<-as.data.frame(do.call("cbind", list(up_df, body_df, down_df)))
  return(colMeans(dd))
}


## genes 
tr<-makeTxDbFromGFF("~/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
genes<-genes(tr)
coding_genes<-read.table("~/MyResearch/genome.db/TAIR/gtf/protein_gene_ids",stringsAsFactors = FALSE)$V1
genes<-genes[coding_genes,]

## cpg island
data<-import.gff("/home/wuzefeng/MyResearch/cpg_island/tair.cpg_island.gff")
data@elementMetadata$LENGTH<-as.numeric(data@elementMetadata$LENGTH)
data@elementMetadata$GCC<-as.numeric(data@elementMetadata$GCC)
data@elementMetadata$`ObsCpG/ExpCpG`<-as.numeric(data@elementMetadata$`ObsCpG/ExpCpG`)
data@elementMetadata$CpGNum<-as.numeric(data@elementMetadata$CpGNum)


##human h3k4me3
tr<-makeTxDbFromGFF("~/Desktop/tangjing_test/Homo_sapiens.GRCh37.gtf")
genes<-genes(tr)
coding_genes<-read.table("~//Desktop/tangjing_test/whgene_extss2000.bed",stringsAsFactors = FALSE)$V5
genes<-genes[coding_genes,]

object<-read.table("~/Desktop/tangjing_test/E006-H3K4me3.broadPeak")
object_bed<-GRanges(seqnames = object$V1,ranges = IRanges(start = object$V2,end = object$V3))

## body
tg<-tile(genes,n=bin_number)
body_df<-mclapply(1:1000,function(x) ifelse(strand(tg[[x]])=="+",countOverlaps(query = tg[[x]],subject = object_bed),rev(countOverlaps(query = tg[[x]],subject = object_bed))),mc.cores = 8)
body_df<-t(as.data.frame(body_df))

##
up_1kb <- flank(genes,width = 1000,both = FALSE)
tg<-tile(up_1kb,n = bin_number)
all_genes_bin_count_up_1kb<-mclapply(1:1000,function(x) ifelse(strand(tg[[x]])=="+",countOverlaps(query = tg[[x]],subject = object_bed),rev(countOverlaps(query = tg[[x]],subject = object_bed))),mc.cores = 8)
up_df<-t(as.data.frame(all_genes_bin_count_up_1kb))
rownames(up_df)<-NULL
## downstream
down_1kb <- flank(genes,width = 1000,both = FALSE,start = FALSE)
tg<-tile(down_1kb,n = bin_number)
names(tg)<-names(down_1kb)
all_genes_bin_count_down_1kb<-mclapply(1:1000,function(x) ifelse(strand(tg[[x]])=="+",countOverlaps(query = tg[[x]],subject = object_bed),rev(countOverlaps(query = tg[[x]],subject = object_bed))),mc.cores = 8)
down_df<-t(as.data.frame(all_genes_bin_count_down_1kb))
rownames(down_df)<-NULL
