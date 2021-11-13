library(methylKit)

## get lmethylation calls from bismark alignment file 
obj <- processBismarkAln(
  location = "MyResearch/WJP/methylation/3bismark_dedup_out/D1806120_L3_J9093.deduplicated.sorted.bam", # bam file
  sample.id = "D1806120_L3_J9093",
  assembly = "B.np",
  save.folder = "MyResearch/WJP/methylation/5methylkit/", # save folder
  save.context = c("CpG","CHG","CHH"),   # save three separate files
  read.context = "CpG",   # read into memory
  nolap = FALSE,
  mincov = 10, # minimum coverage need to report 
  minqual = 20,
  phred64 = FALSE,
  treatment = NULL,
  save.db = FALSE,
  verbose = T
)

## load cpg methylation calls
myobj = methRead(location = "MyResearch/WJP/methylation/5methylkit/D1806120_L3_J9093_CpG.txt",
                 assembly="B.np",
                 sample.id = "D1806120_L3_J9093",
                 context="CpG",
                 mincov = 10
)
## plot methlation percentage
getMethylationStats(myobj,plot=TRUE,both.strands = F,labels = T)

## plot the coverage
getMethylationStats(myobj,plot=TRUE,both.strands = F,labels = T)


## convert to GRange object
myobj_gr <- as(myobj,"GRanges")
chromsome_methy <- c(myobj_gr[grep(pattern = "scaffoldA",x = seqnames(myobj_gr))],
                     myobj_gr[grep(pattern = "scaffoldC",x = seqnames(myobj_gr))])

## tiles
tiles = tileMethylCounts(myobj,win.size=10000,step.size=5000,cov.bases = 10)
myobj_gr <- as(tiles,"GRanges")
chromsome_methy <- c(myobj_gr[grep(pattern = "scaffoldA",x = seqnames(myobj_gr))],
                     myobj_gr[grep(pattern = "scaffoldC",x = seqnames(myobj_gr))])
