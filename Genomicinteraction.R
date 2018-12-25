library(GenomicInteractions)
library(GenomicRanges)
library(InteractionSet)
library(Gviz)

hic_data <- makeGenomicInteractionsFromFile("/home/wuzefeng/MyResearch/HI-C/cell_GEO/HIC_homer/CHr_significantInteractions_100k.txt",  type="homer", experiment_name = "HiC 100kb",  description = "HiC 100kb resolution")
interaction_track <- InteractionTrack(hic_data[seqinfo(hic_data)@seqnames=="1"], name = "HiC", chromosome = "1")
plotTracks(list(interaction_track),sizes=c(0.6),from = 1,to = 10000000)
