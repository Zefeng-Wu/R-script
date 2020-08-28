library(ggplot2)
library(ggpubr)

setwd("/mnt/local_disk1/wzf/4apply/")
rna_seq_samples <- read.table("RNA_seq.sample.ID.txt",stringsAsFactors = FALSE)    # 11768 samples (download from web)
ATAC_seq_samples <- read.table("ATAC_seq.sample.ID.txt",stringsAsFactors = FALSE) # 404 samples
length(intersect(rna_seq_samples$V1,ATAC_seq_samples$V1)) # matched 375 samples


## plot data summary
sample_summary_data <- read.table("ATAC_sample_summary.txt",header = TRUE,stringsAsFactors = FALSE,sep="\t")
ggplot(sample_summary_data,aes(x=cohort,fill=Has.RNA.seq))+
  geom_bar()+
  theme_bw()+
  theme(legend.position = "top",
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 45,vjust = 0.5))+
  scale_fill_manual(values = c("#EEC900", "#1874CD"))+
  labs(fill="With RNA-Seq data")+
  ylab("Sample number")


ggplot(sample_summary_data,aes(x=cohort,fill=Has.WXS))+
  geom_bar()+
  theme_bw()+
  theme(legend.position = "top",
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 45,vjust = 0.5))+
  scale_fill_manual(values = c("#EEC900", "#1874CD"))+
  labs(fill="With exon sequencing data")+
  ylab("Sample number")

ggplot(sample_summary_data,aes(x=cohort,fill=Has.WGS))+
  geom_bar()+
  theme_bw()+
  theme(legend.position = "top",
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 45,vjust = 0.5))+
  scale_fill_manual(values = c("#EEC900", "#1874CD"))+
  labs(fill="With whole genome sequencing data")+
  ylab("Sample number")


## RNA-seq analysis
library(TCGAbiolinks)
x <- getGDCprojects()$project_id  

query<-GDCquery(project = "TCGA-COAD",
                legacy=F,
                experimental.strategy ="RNA-Seq",
                data.category = "Transcriptome Profiling",
                data.type="Gene Expression Quantification",
                workflow.type="HTSeq - Counts")
GDCdownload(query)

## load downloded data into expression data
dataPrep1<-GDCprepare(query=query)
library(SummarizedExperiment)
expdat<-assay(dataPrep1)
dim(expdat) # 56457 (genes) * 521 (samples)

## DEG analysis using two methods
## DEG analysis 1
library(stringr)
library(DESeq2)

group_list<-factor(ifelse(str_sub(colnames(expdat),14,15) == "01","tumor","normal")) # make group list
table(group_list) # 43 normal and 478 tumor
coldata<-data.frame(row.names = colnames(expdat),condition=group_list)  # make coldata

expdat<-round(expdat,0) # make round data
dds<-DESeqDataSetFromMatrix(countData = expdat,colData = coldata,design = ~ condition) # make DEseq dataset
keep<-rowSums(counts(dds))>=10 # filter
dds<-dds[keep,] # 51671
dds<-DESeq(dds) # normalization

res<-results(dds,contrast = c("condition","tumor","normal"))   ## tumor/normal
resOrdered <- res[order(res$pvalue),]
DEG <- as.data.frame(resOrdered)
DEG <- na.omit(DEG)

logFC_cutoff<-with(DEG,mean(abs(log2FoldChange))+2*sd(abs(log2FoldChange)))       ##  mean + 2sd 
DEG$change<-as.factor(ifelse(DEG$pvalue<0.05&abs(DEG$log2FoldChange)>logFC_cutoff,ifelse(DEG$log2FoldChange>logFC_cutoff,"UP","DOWN"),"NOT")) # label the change
## plot vacano plot
library(ggplot2)
require("ggrepel")

this_title <- paste0('Cutoff for logFC is ',round(logFC_cutoff,3),
                     '\nThe number of up-regulated genes is ',nrow(DEG[DEG$change =='UP',]) ,
                     '\nThe number of down-regulated genes is ',nrow(DEG[DEG$change =='DOWN',]))
ggplot(data=DEG,aes(x=log2FoldChange,y=-log10(pvalue),color=change))+
  geom_point(alpha=0.4,size=1.75)+
  labs(x="log2 fold change")+ 
  ylab("-log10 pvalue")+
  ggtitle(this_title)+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(size=15,hjust=0.5))+
  scale_color_manual(values=c('steelblue','gray','orange'))+
  
  geom_label_repel(
    data = DEG[DEG$change!="NOT",][1:5,],
    aes(label = rownames(DEG[DEG$change!="NOT",][1:5,]),
        fill=as.factor(change)),
    color="white",
    show.legend = FALSE ,
    size = 5,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.3, "lines"),
    segment.color = 'grey50'
  )+
  scale_fill_manual(values=c('steelblue','orange'))

######  Edger analysis
###### Edger analysis
library(edgeR)
expdat<-assay(dataPrep1)
expdat = expdat[rowSums(cpm(expdat)>=1) >= 10,]  # filter
exprSet <- DGEList(counts = expdat, group = group_list) # make dgelist
exprSet <- calcNormFactors(exprSet)  # normlaized factor

exprSet <- estimateCommonDisp(exprSet) # 
exprSet <- estimateTagwiseDisp(exprSet)

et <- exactTest(exprSet,pair = c("normal","tumor"))
tTag <- topTags(et, n=nrow(exprSet))
tTag <- as.data.frame(tTag)

logFC_cutoff<-with(tTag,mean(abs(logFC))+2*sd(abs(logFC)))       ##  mean + 2sd 
tTag$change<-as.factor(ifelse(tTag$PValue<0.05&abs(tTag$logFC)>logFC_cutoff,
                             ifelse(tTag$logFC>logFC_cutoff,"UP","DOWN"),"NOT"))

##plot vacano
this_title <- paste0('Cutoff for logFC is ',round(logFC_cutoff,3),
                     '\nThe number of up regulated gene is ',nrow(tTag[tTag$change =='UP',]) ,
                     '\nThe number of down regulated gene is ',nrow(tTag[tTag$change =='DOWN',]))
ggplot(data=tTag,aes(x=logFC,y=-log10(PValue),color=change))+
  geom_point(alpha=0.4,size=1.75)+
  labs(x="log2 fold change")+ 
  ylab("-log10 pvalue")+
  ggtitle(this_title)+
  theme_bw(base_size = 15)+
  theme(plot.title = element_text(size=15,hjust=0.5))+
  scale_color_manual(values=c('blue','black','red'))

## veen plot