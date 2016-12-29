library(GenomicFeatures)
tr<-makeTxDbFromGFF("/home/wuzefeng/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf") #将gtf/gff文件转换为Genomic Feature(TDX)对象
cds(tr)

genes(tr)  #获取基因Grange对象
exonsBy(tr,by="gene") #返回GRangesList object ,每个基因包含的exons作为一个Grange对象

exons(tr)  #获取外显子Grange对象
exonsBy(tr,by="gene")

transcripts(tr)#获取所有转录本Grange对象
transcriptLengths(tr))#每个转录本的长度(一个基因可能还有多个转录本,包含了对应的基因名字)

coverageByTranscript()

introns<-intronsByTranscript(tr)
introns<-unlist(introns)
max(width(introns)) #最长内含子长度



###########提取每个基因cds序列
tr<-makeTxDbFromGFF("MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
cds<-cdsBy(tr,"gene")
library(Rsamtools)
fa<-FaFile("MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa",index = "MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa.fai")
cds_seq<-extractTranscriptSeqs(fa,cds)
library(Biostrings)
writeXStringSet(cds_seq,"Desktop/cds_seq.fa",format = "fasta")
