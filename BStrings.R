library(Biostrings)
fa<-readBStringSet("/home/wuzefeng/MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa") #读取fasta文件
names(fa) #染色体名字
width(fa) #每条染色体长度
as.character(subseq(fa$`1`,1,5)) #取第一号染色体1-5个碱基



#### combine ggbio, Biostrings, GenomicFeatures
library(GenomicFeatures)
library(ggbio)
library(BioStrings)

tr<-makeTxDbFromGFF("/home/wuzefeng/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
genes<-genes(tr)
fa<-readBStringSet("/home/wuzefeng/MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa")
seqlengths(genes)[names(fa)]<-width(fa)
autoplot(seqinfo(genes))+layout_karyogram(sample(genes,2000), aes(x = start, y = meth), ylim = c(10, 30), geom = "line", color = "red")


##################
library(Biostrings)
a <- readDNAStringSet("your_file.fa")
b <- subseq(a, start=start, stop=stop)
writeXStringSet(b, file="new.fa")
