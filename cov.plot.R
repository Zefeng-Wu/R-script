
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
