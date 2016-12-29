library(GenomicFeatures)
tr<-makeTxDbFromGFF("MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
gene<-genes(tr)

library(Biotrings)
fa<-readBStringSet("/home/wuzefeng/MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa")
seqlengths(gene)[names(fa)]<-width(fa)
tile_geonme<-tileGenome(seqlengths(gene),tilewidth = 100000,cut.last.tile.in.chrom = TRUE) #step size
tile_geonme<-trim(resize(tile_geonme, width=500000))  #windows size

