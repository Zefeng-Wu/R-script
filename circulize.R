## get chromsome info
library(GenomicFeatures)
genes<-genes(tr)
library(Biostrings)
fa<-readDNAStringSet("~/MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa",format = "fasta")
seqlengths(genes)<-width(fa)[match(names(seqlengths(genes)),names(fa))]

chrom_info<-as.data.frame(seqlengths(genes))
chrom_info$star=1
chrom_info$chr<-rownames(chrom_info)
rownames(chrom_info)<-NULL
chrom_info<-chrom_info[c(3,2,1)]

#### set background 
library(circlize)
circos.initializeWithIdeogram(chrom_info,plotType = NULL)
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  chr = CELL_META$sector.index
  xlim = CELL_META$xlim
  ylim = CELL_META$ylim
  circos.rect(xlim[1], 0, xlim[2], 1, col = rand_color(1))
  circos.text(mean(xlim), mean(ylim), chr, cex = 0.7, col = "white",
              facing = "inside", niceFacing = TRUE)
}, track.height = 0.15, bg.border = NA)

### cross_link (two bed file are needed)

gene_bed<-as.data.frame(genes)[1:4]
rownames(gene_bed)<-NULL
bed1<-gene_bed[sample(1:nrow(gene_bed),10),]
bed1$end<-bed1$end+1000000
bed2<-gene_bed[sample(1:nrow(gene_bed),10),]
bed2$end<-bed2$end+1000000
colnames(bed1)<-colnames(bed2)<-c("chr","start","end","value")
circos.genomicLink(bed1, bed2, col = rand_color(nrow(bed1), transparency = 0.5), border = NA)

## genomicPoints

circos.genomicTrackPlotRegion(bed1, panel.fun = function(region, value, ...) {
  cex = (value[[1]] - min(value[[1]]))/(max(value[[1]]) - min(value[[1]]))
  i = getI(...)
  circos.genomicPoints(region, value, cex = cex, pch = 16, col = i, ...)
})

