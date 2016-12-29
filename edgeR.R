#http://blog.qiubio.com:8080/archives/3777

library(edgeR) 
load(url("http://qiubio.com:8080/bioconductor/RNA-seq/ds1.Rdata")) # get the count matrix for all samples(columns) with genes(rows)

########filter low expressed genes
cpms <- cpm(counts)
keep <- rowSums(cpms > 1) >= 3 #过滤的reads count 对应最小的libsize*1
counts <- counts[keep,]

####### group information
grp <- as.factor(substr(colnames(counts), 1, 2)) 

######### Make a DGE list 
d = DGEList(counts = counts, group = grp)

######## Estimate normalization factors ? # reads count 受每个sample的测序深度影响最大
d = calcNormFactors(d,method = "TMM")

#######(MDS) plot ?
cols <- as.numeric(d$samples$group)
plotMDS(d,col=cols)
#plotMDS(d, col=cols, top=2000, method="bcv", main="2000 / BCV")

############# 构建模型,如果是control,case一组对照(简单设计)就不需要模型了
mm <- model.matrix(~0+grp)

d <- estimateGLMCommonDisp(d,mm)
d <- estimateGLMTrendedDisp(d,mm)

############# 统计分析(似然比检验)
f <- glmFit(d,mm)
con <- makeContrasts("DE-ES"=grpDE-grpES,levels=mm) #make contrast pair 
lrt <- glmLRT(f,contrast=con) #差异表达基因 or #lrt <- glmLRT(f,contrast=c(1,-1,0,0,0,0,0,0)) or glmLRT(f,coef=2) #需要截距
topTags(lrt,20)

############ 挑几个差异基因看表达
cps <- cpm(d)
o <- order(colnames(counts))
barplot( cps["ENSG00000095596",o], col=cols[o], las=2)

#################输出结果至文件
tt <- topTags(lrt, n=Inf)$table
write.table(tt, file="LRT1.xls", row.names=FALSE, sep="\t", quote=FALSE)

####
nc = cpm(d, normalized.lib.sizes = TRUE)
