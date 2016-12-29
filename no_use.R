############################## RUV_seq remove unwanted 
library(RUVSeq)
library(edgeR)

set.seed(123)
seq <- newSeqExpressionSet(as.matrix(rc_new))
design <- model.matrix(~x)
y <- DGEList(counts=counts(seq), group=class)
y <- calcNormFactors(y, method="upperquartile")
y <- estimateGLMCommonDisp(y, design) #slow
y <- estimateGLMTagwiseDisp(y, design) #slow
fit <- glmFit(y, design)
res <- residuals(fit, type="deviance")
seqRUVr <-RUVr(seq, rownames(rc_new), k=1, res)
nm_counts_1 <- normCounts(seqRUVr)


###################### edgeR nomaliztion
dge<-DGEList(counts=nm_counts_ruvr, group=class)
d = calcNormFactors(dge)
d = estimateCommonDisp(d)
nc = cpm(d, normalized.lib.sizes = TRUE)

####################### quantile 




quantile_genes<-apply(rc_new,2,function(x) rownames(rc_new)[x>=quantile(x,0.25)&x<=quantile(x,0.75)])
genes <-Reduce(intersect, quantile_genes)
medium <-apply(rc_new,2,function(x) median(x[rownames(rc_new)==genes]))




############################# RUV_seq remove unwanted 
library(RUVSeq)
library(edgeR)

set.seed(123)
seq <- newSeqExpressionSet(as.matrix(rc_new))
x<-class
design <- model.matrix(~x)
y <- DGEList(counts=counts(seq), group=class)
y <- calcNormFactors(y, method="upperquartile")
y <- estimateGLMCommonDisp(y, design) #slow
y <- estimateGLMTagwiseDisp(y, design) #slow
fit <- glmFit(y, design)
res <- residuals(fit, type="deviance")
seqRUVr <-RUVr(seq, rownames(rc_new), k=1, res)
nm_counts <- normCounts(seqRUVr)

