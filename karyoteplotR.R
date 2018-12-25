ara_pos<-read.table("/home/wuzefeng/MyResearch/genome.db/TAIR/dna/chrom_size")
ara_pos$start=1
colnames(ara_pos)<-c("chr","end","start")
ara_pos<-ara_pos[,c(1,3,2)]
ara_pos<-ara_pos[order(ara_pos$chr),]

tr<-makeTxDbFromGFF("~/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
genes<-genes(tr)


## define karyotype
kp <- plotKaryotype(genome = toGRanges(ara_pos),plot.type = 1)

##density
kpPlotDensity(kp, data=genes,window.size = 1000)

# link to two position
start.regs <- toGRanges(data.frame("1", 90e5, 90e5))
end.regs <- toGRanges(data.frame("1", 2.3e5, 2.3e5))
kpPlotLinks(kp, data=start.regs, data2=end.regs)
