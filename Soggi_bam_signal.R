library(soGGi)
library(GenomicFeatures)



tr<-makeTxDbFromGFF("~/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.gtf")
genes<- genes(tr)

### function of 
plot_data<-function(histone_file){
df<-data.frame(xIndex=NA,Sample=NA,Score=NA)
chipExample1 <- regionPlot(histone_file,samplename ="test",testRanges = genes,format = "bam",distanceUp = 1000,distanceDown = 1000,style = "percentOfRegion",method = "spline")
df<-rbind(df,plotRegion(chipExample1)$data)
return(df)
}

######### run functuon for each histone 
dd1<-plot_data("~/Desktop/SRR2297459.sorted.rmdup.bam")
dd1<-dd1[-1,]


p<-ggplot(df1,aes(x=xIndex,y=Score))+geom_line()+scale_x_discrete(limits=seq(1,400,100),labels=c("-1kb","TSS","TTS","1kb"))+theme(text = element_text(size=18),plot.margin = unit(c(1,1,1,1), "cm"))+ylab("Mean RPM")+xlab("Region")
