################### affy microarry 
#RMA/GCRMA generate log2 values and MAS5 produces absolute values

#1older chip : 3' IVT 

library(affy)
data <- ReadAffy(celfile.path = "/home/wuzefeng/R/test_data/microarrary/GSE11787_RAW")  # read all *.CEL  file dowloaded form GEO database raw data

### 质量控制
#library("affyQCReport")
library("simpleaffy")
scQC <- qc(data)
plot(scQC)

avbg.data <- as.data.frame(sort(avbg(scQC))) # 平均背景值，如果太大则表示可能有问题
sort(avbg(scQC))

sfs.data <- sort(sfs(scQC))                 # Scale factor
max(sfs.data)/min(sfs.data)                 # #平均背景值，如果太大则表示可能有问题

as.data.frame(percent.present(scQC))        #表达基因所占的比例，太小则表示有问题
percent.present(scQC)

ratios(scQC)                               #内参基因的表达比例

RNAdeg <- AffyRNAdeg(data)                 # RNA降解分析
summaryAffyRNAdeg(RNAdeg)
plotAffyRNAdeg(RNAdeg)

library(affyPLM)                           ### tongji 
Pset <- fitPLM(data) 
x11()
par(mfrow=c(2,2))
image(data[,2])
image(Pset,type="weights",which=2,main="Weights")
image(Pset, type="resids", which=2, main="Residuals")
image(Pset, type="sign.resids", which=2, main="Residuals sign")

library(RColorBrewer)  
colors <- brewer.pal(12,"Set3") 
x11()
Mbox(Pset, ylim = c(-1,1), col=colors,main="RLE") #信号强度箱线图
boxplot(Pset, ylim= c(0.95, 1.20), col=colors, main="NUSE")

NUSE(Pset)
RLE(Pset)

eset <- rma(data) #normalize 
write.exprs(eset,file="data.txt")



#2. st (whole transcripts)
library(oligo)
celFiles <- list.celfiles(path="/home/wuzefeng/Desktop/未命名文件夹/GSE63774_RAW",listGzipped = TRUE) # Read in the CEL files in the directory
affyRaw <- read.celfiles(celFiles) # You might need to install and load a package for the specific array you are using (this example is mouse gene 2.0 ST)
eset <- rma(affyRaw)
write.exprs(eset,file="data.txt")# Finally, save the data to an output file to be used by other programs, etc (Data will be log2 transformed and normalized)

#3 Differitial expressed gene analysis
pData(eset)
design <- model.matrix(~ -1+factor(c(1,1,2,2,3,3)))
colnames(design) <- c("group1", "group2", "group3")
fit <- lmFit(eset, design)
contrast.matrix <- makeContrasts(group2-group1, group3-group2, group3-group1, levels=design) 
fit2 <- contrasts.fit(fit, contrast.matrix) # Computes estimated coefficients and standard errors for a given set of contrasts.
fit2 <- eBayes(fit2) 
topTable(fit2, coef=1, adjust="fdr", sort.by="B", number=10) 
write.table(topTable(fit2, coef=1, adjust="fdr", sort.by="B", number=50000), file="limma_complete.xls", row.names=F, sep="\t") 
results <- decideTests(fit2, p.value=0.05); vennDiagram(results) 
x <- topTable(fit2, coef=1, adjust="fdr", sort.by="P", number=50000); y <- x[x$adj.P.Val < 0.05,]; y; print("Number of genes in this list:"); length(y$ID) 
x <- topTable(fit2, coef=1, adjust="fdr", sort.by="P", number=50000); y <- x[x$adj.P.Val < 0.01 & (x$logFC > 1 | x$logFC < -1) & x$AveExpr > 10,]; y; print("Number of genes in this list:"); length(y$ID) 
results <- decideTests(fit2, p.value=0.000005); heatDiagram(results, fit2$coef, primary=1) 




####流程
GEO(每个样20sample)
platform accession number GPL2025. 
550 CEL files were obtained from GEO
RMA normalization
arrayQualityMetrics(outliear)
