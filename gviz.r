#cummeRbund implements the makeGeneRegionTrack() method to quickly create a GeneRegionTrack from the gene feature
library(rtracklayer)
library(GenomicFeatures)
library(Gviz)

options(ucscChromosomeNames=FALSE)
tx<-makeTxDbFromGFF("~/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
txTr <- GeneRegionTrack(tr,fill="blue",lwd=2,lex=1,col.line="red",col="transparent",lty=1)
gtrack <- GenomeAxisTrack()

plotTracks(list(gtrack,txTr),from = 1.254e+07, to = 1.255e+07,chromosome = '2',showId=TRUE)
