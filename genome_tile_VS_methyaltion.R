library(Biotrings)
library(rtracklayer)

fa<-readBStringSet("/home/wuzefeng/MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa")
seqlengths(gene)[names(fa)]<-width(fa)

############ make windows for genome (drop cirlar chromosomes )
tile_geonme<-tileGenome(seqlengths(gene),tilewidth = 100000,cut.last.tile.in.chrom = TRUE) #sliding windows size
tile_geonme<-trim(resize(tile_geonme, width=500000)) #windows size 
tile_geonme<-tile_geonme[!seqnames(tile_geonme)%in%c("Mt","Pt")]
seqlevels(tile_geonme)<-paste("Chr",seqlevels(tile_geonme),sep="") #make seqlevel same with 

############# import methlation data (gff3) downloaded from plant methylation database 
meth<-import.gff3("MyResearch/methylation_database/tair/ara.meth.gff3")
strand(meth)<-ifelse(meth$score>=0,"+","-")
meth$score<-abs(meth$score)

############# intersect methylation data with tiled genome  and get the average score for reach of windows
overlaps <- findOverlaps(meth, tile_geonme)
signal <- meth[queryHits(overlaps)]
averagedSignal <- aggregate(score(signal), list(subjectHits(overlaps)), mean)
###################
tile_geonme$average_score<-averagedSignal$x
df<-data.frame(seqname=seqnames(tile_geonme),start = start(tile_geonme),end=end(tile_geonme),score=tile_geonme$average_score)
df$seqname<-gsub("Chr","zm",df$seqname)
