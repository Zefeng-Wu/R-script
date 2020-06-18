library(WGCNA)
library(tidyverse)
mapping4<-read.csv("../1co-expression/V4_probe_mapping4.csv",stringsAsFactors =FALSE ) #49151
mapping3<-read.csv("../1co-expression/V3_probes_mapping.csv",stringsAsFactors = FALSE) #46072
colnames(mapping3)<-c("gene_id","probe_set")
mapping3<-mapping3[,c(2,1)]
mapping<-rbind(mapping4,mapping3) # 95223
uniq_gene_mapping<-mapping[!duplicated(mapping[,"gene_id"]),] #42379 #drop multiple probes for one gene ; gene  probes

## read 275 samples
express_data<-read.table("../1co-expression/export_2017-01-15-21-40-06_8843.txt",header = TRUE,sep="\t",stringsAsFactors = FALSE,row.names = 1) #61278
colnames(express_data)<-str_split_fixed(colnames(express_data),pattern ="\\..?", 2)[,1]
express_data<-subset(express_data,rownames(express_data)%in%uniq_gene_mapping$probe_set) #27777 ()
express_data$probe_set<-rownames(express_data)

############### convert gene2sample matrix
express_data<-merge(uniq_gene_mapping,express_data,by = "probe_set") #exsit one pro 2 many transcription
express_data<-express_data[!duplicated(express_data[,"probe_set"]),]
express_data<-express_data[-1]
express_data$gene_id<-str_split_fixed(express_data$gene_id,pattern = "\\.",n = 2)[,1]
express_data<-express_data%>%group_by(gene_id)%>%summarise_all(mean)%>%column_to_rownames("gene_id") # 26961, 274 
express_data<-log(express_data+1,2)
############################
### WGCNA network build
############################
dataTraits <- as.data.frame(str_split_fixed(colnames(express_data),pattern = "_",n = 2)[,1])
rownames(dataTraits)<-colnames(express_data)
colnames(dataTraits)<-"Class"

## filter low varation 
dataExp <-express_data
mad <- apply(dataExp, 1, mad)
WGCNA_matrix<-t(dataExp[mad>quantile(mad)[2],])

### inspect outliner
gsg<- goodSamplesGenes(WGCNA_matrix,verbose = 3)
gsg$allOK
sampleTree <-hclust(dist(WGCNA_matrix),method = "average")
plot(sampleTree,main="Sample clustering",sub="",xlab = "")

## remove outliear 
cluster <- cutreeStatic(sampleTree,cutHeight=1000,minSize=10)
table(cluster)
keepsample <- (cluster==1)
dataExp_keep <-WGCNA_matrix[keepsample,]
############################
### choose soft threshold
##############################
powers <-c(seq(1,10,1),seq(12,20,2))
sft <-pickSoftThreshold(WGCNA_matrix,powerVector = powers,verbose = 5)
str(sft)
par(mfrow=c(2,1))
cex1 <- 0.9
plot(sft$fitIndices[,1],-sign(sft$fitIndices[,3]) * sft$fitIndices[,2],
     xlab = "Soft Threshold (power)",
     ylab = "Scale Free Topological Model Fit, signed R-squre",
     type="n",
     main = "Scale independence")
text(sft$fitIndices[,1],-sign(sft$fitIndices[,3]) * sft$fitIndices[,2],
     labels = powers,
     cex = cex1,
     col="red")
abline(h=0.8,col="red")

plot(sft$fitIndices[,1],sft$fitIndices[,5],
     xlab = "Soft Threshold (power)",
     ylab = "Mean Connectivity",
     type="n",
     main = "Mean connectivity")
text(sft$fitIndices[,1],sft$fitIndices[,5],
     labels = powers,
     cex=cex1,
     col="red")
######################
### Network build
#####################
enableWGCNAThreads()

net <-blockwiseModules(WGCNA_matrix,
                       power = 10,
                       maxBlockSize = 30000, # depend on your memory  (can get one block, rather than several blocks)
                       TOMType = "unsigned",
                       minModuleSize = 30,
                       reassignThreshold = 0,
                       mergeCutHeight = 0.25,
                       numericLabels = TRUE,
                       pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "Nobel_chip_TOM",
                       verbose = 3,
                       nThreads=20)

table(net$colors)
mergedColors = labels2colors(net$colors)
plotDendroAndColors(net$dendrograms[[1]],
                    mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE,
                    hang=0.03,
                    addGuide = TRUE,
                    guideHang = 0.05)

#######################################
## heatmap plot for module similarity
######################################
geneTree <- net$dendrograms[[1]]
moduleColors <- labels2colors(net$colors)
dissTOM = 1-TOMsimilarityFromExpr(datExpr = WGCNA_matrix,
                                power = 10)
plotTOM <- dissTOM ^ 7
diag(plotTOM)<-NA
TOMplot(plotTOM,geneTree,moduleColors,main="Network heatmap plot")

library(gplots)
myheatcol <-colorpanel(250,'red','orange','lemonchiffon')
TOMplot(plotTOM,geneTree,moduleColors,main="Network heatmap plot",col=myheatcol)

### save result
moduleLabels <-net$colors
moduleColors <-labels2colors(net$colors)
MEs <-net$MEs
geneTree <-net$dendrograms[[1]]
save(MEs,moduleLabels,moduleColors,geneTree,file="../1co-expression/WGCNA/274-CHIP_networkCOnstruction.RData")

### insepct gene name in each module
gene_list<-colnames(WGCNA_matrix)[moduleColors=="black"]

## select the module genes and show the expression patterns
which.module <-"black"
plotMat(t(scale(WGCNA_matrix[,moduleColors==which.module])),
        nrgcols =30,
        rlabels=F,
        rcols = which.module,
        main=which.module,
        cex.main=2)

## correlation between module eigengene
MEs <-net$MEs
MEs_col <-MEs
colnames(MEs_col) <-paste("ME",labels2colors(as.numeric(str_replace_all(colnames(MEs),"ME",""))))
MEs_col <-orderMEs(MEs_col)
plotEigengeneNetworks(MEs_col,"Eigengen adjacency heatmap",
                      marDendro = c(8,8,1,2),
                      marHeatmap = c(8,8,1,2),
                      plotDendrograms = TRUE,
                      xLabelsAngle=45,cex.preservation = 0.2)

###############################
## module and trait relationship
################################
design = model.matrix(~0+ dataTraits$Class)
colnames(design) = levels(dataTraits$Class)
moduleColors = labels2colors(net$colors)
nGenes = ncol(WGCNA_matrix)
nSamples = nrow(WGCNA_matrix)

MEs0 = moduleEigengenes(WGCNA_matrix, moduleColors)$eigengenes
MEs = orderMEs(MEs0)                                          #274sample ,21module
moduleTraitCor = cor(MEs, design, use = "p")
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples)

textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
                   signif(moduleTraitPvalue, 1), ")", sep = "")
dim(textMatrix) = dim(moduleTraitCor)
par(mar = c(6, 8.5, 3, 3))

labeledHeatmap(Matrix = moduleTraitCor,
               xLabels = colnames(design),
               yLabels = names(MEs),
               ySymbols = names(MEs),
               colorLabels = FALSE,
               colors = blueWhiteRed(50),
               textMatrix = textMatrix,
               setStdMargins = FALSE,
               cex.text = 0.6,
               zlim = c(-1,1),
               main = paste("Module-trait relationships"))

##################################
### select speicific module
##################################
connet=abs(cor(WGCNA_matrix,use="p"))^6
Alldegrees1=intramodularConnectivity(connet, moduleColors)

nodule= as.data.frame(design[,19]) # need just based on trait data
names(nodule) = "nodule"
GS1 = as.numeric(cor(WGCNA_matrix, nodule, use = "p"))
GeneSignificance=abs(GS1)

#Generalizing intramodular connectivity for all genes on the array
datKME=signedKME(WGCNA_matrix, MEs, outputColumnName="MM.")


#Finding genes with high gene significance and high intramodular connectivity in specific modules


FilterGenes<- abs(GS1)>=0.5 & abs(datKME$MM.black)>=0.5 #abs(GS1)>.8 #adjust parameter based on actual situations
table(FilterGenes)
hubgenes <- rownames(datKME)[FilterGenes]
hubgenes

####################################
## export the network
#####################################
TOM = TOMsimilarityFromExpr(WGCNA_matrix, power = 10) 

module = "black" # Select module probes
probes = colnames(WGCNA_matrix)
inModule = (moduleColors==module)
modProbes = probes[inModule] 

modTOM = TOM[inModule, inModule] ## Select the corresponding Topological Overlap
dimnames(modTOM) = list(modProbes, modProbes)

cyt = exportNetworkToCytoscape(modTOM,
                               edgeFile = paste("CytoscapeInput-edges-", paste(module, collapse="-"), ".txt", sep=""),
                               nodeFile = paste("CytoscapeInput-nodes-", paste(module, collapse="-"), ".txt", sep=""),
                               weighted = TRUE,
                               threshold = 0.02, #
                               nodeNames = modProbes, 
                               nodeAttr = moduleColors[inModule])
#######################
#Screen the top genes
#######################
nTop = 10
IMConn = softConnectivity(datExpr[, modProbes])
top = (rank(-IMConn) <= nTop)
filter <- modTOM[top, top]

cyt = exportNetworkToCytoscape(filter,
                               edgeFile = paste("CytoscapeInput-edges-filter-", paste(module, collapse="-"), ".txt", sep=""),
                               nodeFile = paste("CytoscapeInput-nodes-filter-", paste(module, collapse="-"), ".txt", sep=""),
                               weighted = TRUE,
                               threshold = 0.02,
                               nodeNames = rownames(filter), 
                               nodeAttr = moduleColors[inModule][1:nTop])

#########################
#get node color
########################
for (color in unique(labels2colors(net$colors))){
  message(color)
  inModule <- (moduleColors==color)
  probes <- colnames(WGCNA_matrix)
  modProbes <- probes[inModule]
  write.table(data.frame(gene_id=modProbes,col=color),"../1co-expression/WGCNA/gene_color.txt",append = TRUE,quote = FALSE,sep="\t",col.names = FALSE,row.names = FALSE)
}
