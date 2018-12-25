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





### slidingWindows
slidingWindows(gr, width=3L, step=2L)
### tile
tile(genes[2],n =20 )


##看cpg在基因body中的分布
1.将每个基因划分区间
tg<-tile(genes,n = 20) #grange list
names(tg)<-names(genes)

2.计算每个基因每个区间与cpg的交集个数

tgg<-lapply(tg,function(x)countQueryHits(findOverlaps(query = x,subject = cpg_gr)))


3
#First, I don't actually know if I'm using all these packages, I just include them in case.
   library(GenomicRanges)
   library(rtracklayer)
   library(Rsamtools)

   #read in a bam file
   temp<-readGappedAlignments(bamfile)
   #summarize into a coverage Rle
   cvg <- coverage(temp)
   #may want to scale coverage here based on number of reads

   #then I loop through a bed file of genes pulling out the coverage values for each and sticking them in a data frame
   for each gene i
   {
      #use approx to create the bins, in this case 20... and save it into some data frame...
      df[i,]<-approx(window(cvg[[chrom]],start,end),n=20)$y
   }
