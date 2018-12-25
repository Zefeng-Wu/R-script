arg <- commandArgs(T) 
if(length(arg) < 3){ 
  cat("Argument: fasta_file gtf_file gene_name_list_file\n") 
  quit('no') 
} 

library(GenomicFeatures)
library(Biostrings)
library(BSgenome)#provide getSeq(), howerver rsamtools also provides getSeq(),but can not exract sequences  

fa<-readDNAStringSet(arg[1],"fasta")
names(fa)<-unlist(lapply(names(fa),FUN=function(x)strsplit(x,split=" ")[[1]][1]))
tx<-makeTxDbFromGFF(arg[2])
genes<-genes(tx)
fa<-fa[names(fa)%in%names(seqlengths(genes))] ### get the chromosome contains genes
seqlengths(genes)[names(fa)]<-width(fa)       ### set gene boundrary

promt<-promoters(genes,upstream = 2500,downstream = 500)
promt<-trim(promt)
promt<-promt[!seqnames(promt)%in%c("Mt","Pt")]
#promoter_seq<- getSeq(fa, promt) # too large to extract all promoter
########################## out write
object_gene<-arg[3]
gene_id_list = as.character(read.table(object_gene)$V1)
object_pro<-promt[promt$gene_id%in%gene_id_list]
promoter_seq<- getSeq(fa, object_pro)
names(promoter_seq)<-gene_id_list
writeXStringSet(promoter_seq,"promoter.fa",format = "fasta",append = TRUE)
message("OK")
