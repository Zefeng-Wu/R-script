
library(ggplot2)
library(GenomicRanges)
gr<-GRanges(seqname=rep(c("chr1","chr2","chr3","chr4"),10),range=IRanges(start=sample(seq(1:100),40),width=1),strand=sample(c("+","-"),40,replace=TRUE))
seqlengths(gr)<-c(100,120,150,110)
gr$cov = sample(seq(1:100),length(gr))
df<-as.data.frame(gr)

p <- ggplot(df, aes(x=start, y=cov))
p <- p + geom_rect(aes(xmin=start, ymin=0, xmax=end, ymax=cov), fill='black', color='black')
p <- p + facet_grid(seqnames ~., scales="free")
#p <- p + theme_classic()
p <- p + scale_y_continuous(expand=c(0,0))
p <- p + theme(strip.text.y=element_text(angle=360))

## method 2
library(ggplot2)
library(GenomicRanges)
gr<-GRanges(seqname=rep(c("chr1","chr2","chr3","chr4"),10),range=IRanges(start=sample(seq(1:100),40),width=1),strand=sample(c("+","-"),40,replace=TRUE))
seqlengths(gr)<-c(100,120,150,110)
gr$cov = sample(seq(1:100),length(gr))
df<-as.data.frame(gr)

p <- ggplot(df, aes(x=start, y=cov))
p <- p + geom_bar(aes(x=start, y=cov,fill=strand),stat = "identity")
p <- p + facet_grid(seqnames ~.,space = "free_x")
#p <- p + theme_classic()
p <- p + scale_y_continuous(expand=c(0,0))
p <- p + theme(strip.text.y=element_text(angle=360)) 


## methods 3 heatmap
dt <- data.table(
chromosome = c(rep(1, 100), 
     rep(2, 100), 
     rep(3, 80)),
mb_from = c(seq(1, 1000, by=10),
      seq(1, 1000, by=10),
      seq(1, 800, by=10)),
mb_to = c(seq(10, 1000, by=10),
    seq(10, 1000, by=10),
    seq(10, 800, by=10)),
score = c(sample(1:10, 100, replace = T),
       sample(1:10, 100, replace = T),
       sample(1:10, 80, replace = T))
)


library(dplyr)
library(plotly)
library(RColorBrewer)

dat <- apply(dt,
      1,
      function(x) data.table(chromosome = x["chromosome"], mb = x["mb_from"]:x["mb_to"], score = x["score"])
) %>%
  rbindlist()

plot_ly(dat, x = ~chromosome, y = ~mb, z = ~score, type = "heatmap",
        colors = "RdYlBu", reversescale = T) %>%
  layout(yaxis = list(range = c(1000, 0)))

## method 4
library("ggplot2") # for the plot
library("ggrepel") # for spreading text labels on the plot
library("scales") # for axis labels notation

# insert your steps to load data from tabular files or other sources here; 
# dummy datasets taken directly from files shown in this example

# data with the copy number alterations for the sample
sample_cns <- structure(list(gene = c("AC116366.7", "ANKRD24", "APC", "SNAPC3", 
                                      "ARID1A", "ATM", "BOD1L1", "BRCA1", "C11orf65", "CHD5", "COL1A1", 
                                      "CTC-554D6.1", "CTD-2047H16.4", "DNAH9", "DOT1L", "DPYD", "DPYD-AS1", 
                                      "EP300", "EP300-AS1", "ERBB2", "FGFR2", "KMT2A", "KMT2B", "LRP1B", 
                                      "MAP3K1"), chromosome = c("chr5", "chr19", "chr5", "chr9", "chr1", 
                                                                "chr11", "chr4", "chr17", "chr11", "chr1", "chr17", "chr5", "chr17", 
                                                                "chr17", "chr19", "chr1", "chr1", "chr22", "chr22", "chr17", 
                                                                "chr10", "chr11", "chr19", "chr2", "chr5"), start = c(131893016L, 
                                                                                                                      4183350L, 112043414L, 15465517L, 27022894L, 108098351L, 13571634L, 
                                                                                                                      41197694L, 108180886L, 6166339L, 48262862L, 112162804L, 78326739L, 
                                                                                                                      11501815L, 2164183L, 97544531L, 97564044L, 41489008L, 41572250L, 
                                                                                                                      37844086L, 123239094L, 118307227L, 36208920L, 140990754L, 56111400L
                                                                ), end = c(131978056L, 4224502L, 112179823L, 15465578L, 27107247L, 
                                                                           108236235L, 13629211L, 41276113L, 108236235L, 6240083L, 48278874L, 
                                                                           112179823L, 78367298L, 11872844L, 2229791L, 98386478L, 97771853L, 
                                                                           41574960L, 41574960L, 37884297L, 123353331L, 118392887L, 36229458L, 
                                                                           142888298L, 56189507L), log2 = c(-0.850333, -0.802459, -0.850333, 
                                                                                                            1.68765, -0.828046, -0.883559, 0.495105, 0.51503, -0.883559, 
                                                                                                            -0.828046, 0.51503, -0.850333, 0.51503, -0.801607, -0.802459, 
                                                                                                            -0.828046, -0.828046, 0.517179, 0.517179, 0.51503, -0.865372, 
                                                                                                            -0.883559, 0.970523, -0.809056, -0.850333), depth = c(823.473, 
                                                                                                                                                                  240.685, 723.721, 4325.57, 596.063, 560.472, 1563.44, 1609.96, 
                                                                                                                                                                  703.526, 411.25, 1586.75, 986.95, 2779.47, 643.981, 219.58, 654.69, 
                                                                                                                                                                  648.597, 1488.49, 2631.62, 2144.13, 893.806, 222.718, 985.121, 
                                                                                                                                                                  1112.21, 571.51), weight = c(16.7856, 17.0764, 31.7769, 0.557449, 
                                                                                                                                                                                               23.296, 39.5052, 34.3571, 23.5551, 15.7455, 25.5399, 28.9927, 
                                                                                                                                                                                               22.9053, 23.2428, 52.522, 26.4509, 18.9309, 3.71943, 27.8139, 
                                                                                                                                                                                               7.18582, 18.225, 21.8383, 43.5557, 31.4704, 58.351, 19.5343), 
                             cn = c(1L, 1L, 1L, 7L, 1L, 1L, 3L, 3L, 1L, 1L, 3L, 1L, 3L, 
                                    1L, 1L, 1L, 1L, 3L, 3L, 3L, 1L, 1L, 4L, 1L, 1L), probes = c(897L, 
                                                                                                508L, 897L, 51L, 1052L, 434L, 370L, 847L, 434L, 1052L, 847L, 
                                                                                                897L, 847L, 284L, 508L, 1052L, 1052L, 125L, 125L, 847L, 157L, 
                                                                                                434L, 66L, 226L, 897L)), .Names = c("gene", "chromosome", 
                                                                                                                                    "start", "end", "log2", "depth", "weight", "cn", "probes"), row.names = c(NA, 
                                                                                                                                                                                                              25L), class = "data.frame")
# hg19 chromosome sizes
chrom_sizes <- structure(list(V1 = c("chrM", "chr1", "chr2", "chr3", "chr4", 
                                     "chr5", "chr6", "chr7", "chr8", "chr9", "chr10", "chr11", "chr12", 
                                     "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", "chr19", 
                                     "chr20", "chr21", "chr22", "chrX", "chrY"), V2 = c(16571L, 249250621L, 
                                                                                        243199373L, 198022430L, 191154276L, 180915260L, 171115067L, 159138663L, 
                                                                                        146364022L, 141213431L, 135534747L, 135006516L, 133851895L, 115169878L, 
                                                                                        107349540L, 102531392L, 90354753L, 81195210L, 78077248L, 59128983L, 
                                                                                        63025520L, 48129895L, 51304566L, 155270560L, 59373566L)), .Names = c("V1", 
                                                                                                                                                             "V2"), class = "data.frame", row.names = c(NA, -25L))
centromeres <- structure(list(X.bin = c(23L, 20L, 2L, 1L, 14L, 16L, 1L, 14L, 
                                        1L, 1L, 10L, 1L, 15L, 13L, 1L, 1L, 11L, 13L, 1L, 1L, 1L, 12L, 
                                        10L, 10L), chrom = c("chr1", "chr2", "chr3", "chr4", "chr5", 
                                                             "chr6", "chr7", "chr8", "chr9", "chrX", "chrY", "chr10", "chr11", 
                                                             "chr12", "chr13", "chr14", "chr15", "chr16", "chr17", "chr18", 
                                                             "chr19", "chr20", "chr21", "chr22"), chromStart = c(121535434L, 
                                                                                                                 92326171L, 90504854L, 49660117L, 46405641L, 58830166L, 58054331L, 
                                                                                                                 43838887L, 47367679L, 58632012L, 10104553L, 39254935L, 51644205L, 
                                                                                                                 34856694L, 16000000L, 16000000L, 17000000L, 35335801L, 22263006L, 
                                                                                                                 15460898L, 24681782L, 26369569L, 11288129L, 13000000L), chromEnd = c(124535434L, 
                                                                                                                                                                                      95326171L, 93504854L, 52660117L, 49405641L, 61830166L, 61054331L, 
                                                                                                                                                                                      46838887L, 50367679L, 61632012L, 13104553L, 42254935L, 54644205L, 
                                                                                                                                                                                      37856694L, 19000000L, 19000000L, 20000000L, 38335801L, 25263006L, 
                                                                                                                                                                                      18460898L, 27681782L, 29369569L, 14288129L, 16000000L), ix = c(1270L, 
                                                                                                                                                                                                                                                     770L, 784L, 447L, 452L, 628L, 564L, 376L, 411L, 583L, 105L, 341L, 
                                                                                                                                                                                                                                                     447L, 304L, 3L, 3L, 3L, 354L, 192L, 125L, 410L, 275L, 22L, 3L
                                                                                                                                                                                      ), n = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", 
                                                                                                                                                                                               "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N"
                                                                                                                                                                                      ), size = c(3000000L, 3000000L, 3000000L, 3000000L, 3000000L, 
                                                                                                                                                                                                  3000000L, 3000000L, 3000000L, 3000000L, 3000000L, 3000000L, 3000000L, 
                                                                                                                                                                                                  3000000L, 3000000L, 3000000L, 3000000L, 3000000L, 3000000L, 3000000L, 
                                                                                                                                                                                                  3000000L, 3000000L, 3000000L, 3000000L, 3000000L), type = c("centromere", 
                                                                                                                                                                                                                                                              "centromere", "centromere", "centromere", "centromere", "centromere", 
                                                                                                                                                                                                                                                              "centromere", "centromere", "centromere", "centromere", "centromere", 
                                                                                                                                                                                                                                                              "centromere", "centromere", "centromere", "centromere", "centromere", 
                                                                                                                                                                                                                                                              "centromere", "centromere", "centromere", "centromere", "centromere", 
                                                                                                                                                                                                                                                              "centromere", "centromere", "centromere"), bridge = c("no", "no", 
                                                                                                                                                                                                                                                                                                                    "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", 
                                                                                                                                                                                                                                                                                                                    "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no"
                                                                                                                                                                                                                                                              )), .Names = c("X.bin", "chrom", "chromStart", "chromEnd", "ix", 
                                                                                                                                                                                                                                                                             "n", "size", "type", "bridge"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                 -24L))

colnames(chrom_sizes) <- c("chromosome", "size")
colnames(centromeres) <- c('bin', "chromosome", 'start', 'end',
                           'ix', 'n', 'size', 'type', 'bridge')

# create an ordered factor level to use for the chromosomes in all the datasets
chrom_order <- c("chr1", "chr2", "chr3", "chr4", "chr5", "chr6", "chr7", 
                 "chr8", "chr9", "chr10", "chr11", "chr12", "chr13", "chr14", 
                 "chr15", "chr16", "chr17", "chr18", "chr19", "chr20", "chr21", 
                 "chr22", "chrX", "chrY", "chrM")
chrom_key <- setNames(object = as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
                                              12, 13, 14, 15, 16, 17, 18, 19, 20, 
                                              21, 22, 23, 24, 25)), 
                      nm = chrom_order)
chrom_order <- factor(x = chrom_order, levels = rev(chrom_order))

### plot
ggplot(data = chrom_sizes) +
  geom_segment(aes(x = as.numeric(chromosome),
                  xend = as.numeric(chromosome), 
                  y=size,
                  yend = 0),
                  size = 4,
                  lineend="round",
                  col = "gray",radim=0.1)
  # base rectangles for the chroms, with numeric value for each chrom on the x-axis
  geom_rect(aes(xmin = as.numeric(chromosome) - 0.2, 
                xmax = as.numeric(chromosome) + 0.2, 
                ymax = size, ymin = 0), 
            colour="black", 
            fill = "white",linejoin = "round") + 
  # rotate the plot 90 degrees
  coord_flip() +
  # black & white color theme 
  theme(axis.text.x = element_text(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank()) + 
  # give the appearance of a discrete axis with chrom labels
  scale_x_discrete(name = "chromosome", limits = names(chrom_key)) +
  # add bands for centromeres
  geom_rect(data = centromeres, aes(xmin = as.numeric(chromosome) - 0.2, 
                                    xmax = as.numeric(chromosome) + 0.2, 
                                    ymax = end, ymin = start)) +
  # add bands for CNA value
  geom_rect(data = sample_cns, aes(xmin = as.numeric(chromosome) - 0.2, 
                                   xmax = as.numeric(chromosome) + 0.2, 
                                   ymax = end, ymin = start, fill = CNA)) + 
  scale_fill_manual(values = group.colors) +
  # add 'gain' gene markers
  geom_text_repel(data = subset(sample_cns, sample_cns$CNA == "gain"), 
                  aes(x = chromosome, y = start, label = gene), 
                  color = "red", show.legend = FALSE) +
  # add 'loss' gene markers
  geom_text_repel(data = subset(sample_cns, sample_cns$CNA == "loss"), 
                  aes(x = chromosome, y = start, label = gene ), 
                  color = "blue", show.legend = FALSE) +
  ggtitle("Copy Number Alterations") +
  # supress scientific notation on the y-axis
  scale_y_continuous(labels = comma) +
  ylab("region (bp)")


##average gene plot (https://rpubs.com/achitsaz/124552)


library(GenomicFeatures)
library(GenomicRanges)
library(tidyr)
library(dplyr)
library(ggplot2)



## Load in Files

rpm.files <- list.files(path="R", pattern="IP.*rpm.RData$", full.names=T)
for (i in 1:length(rpm.files)) {
    load(rpm.files[i])
}
rpm.names <- ls(pattern="rpm$")
rpm.names
dre.NS.list <- sapply(grep("dre4_NS", rpm.names, value=T), get)
dre.dre.list <- sapply(grep("dre4_dre", rpm.names, value=T), get)
ssrp.NS.list <- sapply(grep("SSRP_NS", rpm.names, value=T), get)
ssrp.ssrp.list <- sapply(grep("SSRP_SSRP", rpm.names, value=T), get)
igg.list <- sapply(grep("IgG", rpm.names, value=T), get)


collapse.cov <- function(cov, genes.gr) {
    cov.new <- Reduce('+', cov)
    cov.new <- cov.new/length(cov)
    names(cov.new) <- gsub("chr", "", names(cov.new))
    keep <- match(seqlevels(genes.gr), names(cov.new))
    cov.new <- cov.new[keep]
    return(cov.new)
}


dre.NS.cov <- collapse.cov(cov=dre.NS.list, genes.gr = ssrp)
dre.dre.cov <- collapse.cov(cov=dre.dre.list, genes.gr = ssrp)
ssrp.NS.cov <- collapse.cov(cov=ssrp.NS.list, genes.gr = ssrp)
ssrp.ssrp.cov <- collapse.cov(cov=ssrp.ssrp.list, genes.gr = ssrp)
igg.cov <- collapse.cov(cov=igg.list, genes.gr =ssrp)


####Average Gene
get.cov.from.gr <- function(gr, gene, covs, bins) {
    ch <- as.character(unique(seqnames(gr[gene])))
    view <- Views(covs[ch][[1]], ranges(gr[gene]))
    cov <- as.numeric(view[1][[1]])
    if (as.logical(strand(gr[gene]) == "-")) {
        cov <- rev(cov)
    }
    b   <- length(cov)
    st  <- trunc(seq(1,b,by=(b/bins)) + .001) 
    end <- trunc(seq(trunc(b/bins), b, by=(b/bins)) + .001)
    bin.mtx <- mapply(function(x,y) {cov[x:y]}, st, end)
    cov.mtx <- colMeans(bin.mtx)
    cov.mtx <- t(matrix(cov.mtx))
    row.names(cov.mtx) <-  names(gr[gene])
    cov.mtx
}


get.cov.mtx <- function(gr.var, covs.var, n.cores, bins.var) {
    cov.list <- mclapply(1:length(gr.var),
                         FUN=get.cov.from.gr,
                         gr = gr.var,
                         covs = covs.var,
                         bins = bins.var,
                         mc.cores = n.cores)
    cov.mtx <- do.call(rbind, cov.list)
    cov.mtx
}

colstd <- function(x) {
    sapply(1:ncol(x), function(y) { sd(x[,y])})
}

ggplot_avg.gene.4 <- function(w, x, y, z, title) {
    df <- data.frame(cbind(colMeans(w), colMeans(x), colMeans(y), colMeans(z)))
    colnames(df) <- c("dre4.NS", "dre4.dre4", "ssrp.NS", "ssrp.ssrp")
    df.gg <- df %>% gather(Expression, RPM, dre4.NS:ssrp.ssrp)
    l <- ncol(x)
    df.gg$Index <- rep(1:l, 4)
    df.gg$se <- c(colstd(w)/sqrt(nrow(w)), colstd(x)/sqrt(nrow(x)), colstd(y)/sqrt(nrow(y)), colstd(z)/sqrt(nrow(z)))
    ggplot(df.gg, aes(x=Index, y=RPM, Group=factor(Expression))) +
#        geom_ribbon(aes(ymin=RPM-se, ymax=RPM+se), alpha=0.2) +
        geom_line(aes(colour=factor(Expression)), size=1) +
        #scale_colour_manual(labels = c("NS", "dre4", "ssrp"),
        #                    values = c("#C77CFF","#F8766D", "#7CAE00", "#00BFC4")) +
        scale_x_continuous(breaks = c(0, 150, 650, 800),
                           labels = c("-1500", "TSS", "pA", "+1500")) +
        geom_vline(xintercept=150,
                   colour="red",
                   linetype="longdash") +
        geom_vline(xintercept=650,
                   colour="red",
                   linetype="longdash") +
        labs(colour = "Expiriment (ChIP.Knockdown)") +
        xlab("Position") +
        ggtitle(paste0("Average Gene Plot of ", title)) + 
        theme_bw()     
}

ggplot_avg.gene <- function(w, x, y) {
    df <- data.frame(cbind(colMeans(w), colMeans(x), colMeans(y)))
   colnames(df) <- c("dre4", "NS", "ssrp")
    df.gg <- df %>% gather(Expression, RPM, dre4:ssrp)
    l <- ncol(x)
    df.gg$Index <- rep(1:l, 3)
    df.gg$se <- c(colstd(w)/sqrt(nrow(w)), colstd(x)/sqrt(nrow(x)), colstd(y)/sqrt(nrow(y)))
    ggplot(df.gg, aes(x=Index, y=RPM, Group=factor(Expression))) +
#        geom_ribbon(aes(ymin=RPM-se, ymax=RPM+se), alpha=0.2) +
        geom_line(aes(colour=factor(Expression)), size=1) +
        #scale_colour_manual(labels = c("NS", "dre4", "ssrp"),
        #                    values = c("#C77CFF","#F8766D", "#7CAE00", "#00BFC4")) +
        scale_x_continuous(breaks = c(0, 150, 650, 800),
                           labels = c("-1500", "TSS", "TES", "+1500")) +
        geom_vline(xintercept=150,
                   colour="red",
                   linetype="longdash") +
        geom_vline(xintercept=650,
                   colour="red",
                   linetype="longdash") +
        labs(colour = "Expression") +
        xlab("Position") +
        ggtitle("Average Gene Plot") +
        theme_bw()     
}

###Load in granges
genes.all <- readRDS(file="/n/core/Genomics/Analysis/Conaway/theo_tettey/tht1/goi.RDS")
txdb <- loadDb("/home/adc/txdb/Dmel.txdb")
tx <- transcriptsBy(txdb, 'gene')



### Run Function
getplots <- function(geneset) {
    unlist.tx <- unlist(tx)
    gen <- unlist.tx[geneset[[1]]]
    genes.gr <- gen[width(gen) > 1000]
    genes.gr <- keepSeqlevels(genes.gr, names(dre.NS.cov))
    chr.len <- seqlengths(seqinfo(genes.gr))
    is.close.end <- end(genes.gr) + 1500 > chr.len[as.character(seqnames(genes.gr))]
    genes.gr <- genes.gr[start(genes.gr) > 1500 & !(is.close.end)]
    label <- paste0("img/average.gene/", names(geneset), "_cov.averagegene.png")
   
    ## Average Gene
    dre.NS.mtx.gen <- get.cov.mtx(genes.gr, dre.NS.cov, 22, 500)
    dre.dre.mtx.gen <- get.cov.mtx(genes.gr, dre.dre.cov, 22, 500)
    ssrp.NS.mtx.gen <- get.cov.mtx(genes.gr, ssrp.NS.cov, 22, 500)
    ssrp.ssrp.mtx.gen <- get.cov.mtx(genes.gr, ssrp.ssrp.cov, 22, 500)
    
    
    
    ##1500 upstream of Gene
    pre <- flank(genes.gr, 1500, start=TRUE)
    dre.NS.mtx.pre <- get.cov.mtx(pre, dre.NS.cov, 22, 150)
    dre.dre.mtx.pre <- get.cov.mtx(pre, dre.dre.cov, 22, 150)
    ssrp.NS.mtx.pre <- get.cov.mtx(pre, ssrp.NS.cov, 22, 150)
    ssrp.ssrp.mtx.pre <- get.cov.mtx(pre, ssrp.ssrp.cov, 22, 150)
    
    ##1500 downstream of gene
    post <- flank(genes.gr, 1500, start=FALSE)
    dre.NS.mtx.post <- get.cov.mtx(post, dre.NS.cov, 22, 150)
    dre.dre.mtx.post <- get.cov.mtx(post, dre.dre.cov, 22, 150)
    ssrp.NS.mtx.post <- get.cov.mtx(post, ssrp.NS.cov, 22, 150)
    ssrp.ssrp.mtx.post <- get.cov.mtx(post, ssrp.ssrp.cov, 22, 150)

    ##Combine them together
    dre.NS.avg.gene <- cbind(dre.NS.mtx.pre, dre.NS.mtx.gen, dre.NS.mtx.post) 
    dre.dre.avg.gene <- cbind(dre.dre.mtx.pre, dre.dre.mtx.gen, dre.dre.mtx.post) 
    ssrp.ssrp.avg.gene <- cbind(ssrp.ssrp.mtx.pre, ssrp.ssrp.mtx.gen, ssrp.ssrp.mtx.post) 
    ssrp.NS.avg.gene <- cbind(ssrp.NS.mtx.pre, ssrp.NS.mtx.gen, ssrp.NS.mtx.post)

    png(file=label, width=1080, height=720)
    p <- ggplot_avg.gene.4(dre.NS.avg.gene, dre.dre.avg.gene, ssrp.NS.avg.gene, ssrp.ssrp.avg.gene, title = paste0(names(geneset), " (", length(geneset[[1]]), " Genes)"))
    print(p)
    dev.off()
}

for (i in 1:length(genes.all)) {
    getplots(genes.all[i])
}
