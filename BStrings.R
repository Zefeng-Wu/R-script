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


##### 计算grange对象属性,如gc含量
repeat_gr<-makeGRangesFromDataFrame(data,keep.extra.columns = TRUE,ignore.strand = TRUE,seqnames.field = "V1",start.field = "V2",end.field = "V3")
repeat_gr$V15<-DNAStringSet(repeat_gr$V15) # take seed sequence as biostring
repeat_gr$V16<-DNAStringSet(repeat_gr$V16) #take seed sequence as biostring
repeat_gr$repeat_gc<-rowSums(letterFrequency(repeat_gr$V15,"CG",OR=0,as.prob = TRUE))



#### k-mer计算
library(Biostrings)
a <- readDNAStringSet("your_file.fa")
k_matrix<-oligonucleotideFrequency(fa, width=8)
rownames(k_matrix)<-names(a)
