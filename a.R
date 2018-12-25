library(GenomicFeatures)
library(Biostrings)
library(BSgenome)#provide getSeq(), howerver rsamtolls also provides getSeq(),but can not exract sequences  

fa<-readDNAStringSet("/home/wuzefeng/MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa","fasta")
tx<-makeTxDbFromGFF("/home/wuzefeng/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
genes<-genes(tx)
seqlengths(genes)[names(fa)]<-width(fa)

promt<-promoters(genes,upstream = 2500,downstream = 500)
promt<-trim(promt)
promt<-promt[!seqnames(promt)%in%c("Mt","Pt")]
#promoter_seq<- getSeq(fa, promt) # too large to extract all promoter
########################## out write
object_gene<-c("AT3G20740","AT1G01010")
object_pro<-promt[promt$gene_id%in%object_gene]
promoter_seq<- getSeq(fa, object_pro)
names(promoter_seq)<-object_gene
writeXStringSet(promoter_seq,"Desktop/cds_seq.fa",format = "fasta",append = TRUE)