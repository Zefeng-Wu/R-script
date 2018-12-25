#1 quantile normalization

reads_count<-data.frame(Control=c(2,2,2,2,2),Treat=c(6,6,6,6,76))


#1
library(limma)
normalizeMedianValues(reads_count)

#Control     Treat
#[1,] 3.464102  3.464102
#[2,] 3.464102  3.464102
#[3,] 3.464102  3.464102
#[4,] 3.464102  3.464102
#[5,] 3.464102 43.878620

##1.Median normalization(myself)
Med <- apply(reads_count, 2, median)
Med <- Med/mean(Med)
normalized_reads_counts<-scale(x = reads_count,center = FALSE,scale = Med)


#2. quantile normalization
normalizeBetweenArrays(reads_count)
#Control Treat
#[1,]       4     4
#[2,]       4     4
#[3,]       4     4
#[4,]       4     4
#[5,]       4    39

#3
normalizeQuantiles(reads_count)
#Control Treat
#[1,]       4     4
#[2,]       4     4
#[3,]       4     4
#[4,]       4     4
#[5,]       4    39

##4 TMM normalize
library(edgeR)
N <- colSums(reads_count)
f<-calcNormFactors(as.matrix(reads_count),method="TMM")
f.scale<-N*f / mean(N*f)  # 0.5,1.5
normalize_count<-scale(reads_count,center = FALSE,scale = f.scale)

#Control Treat
#[1,]       4     4
#[2,]       4     4
#[3,]       4     4
#[4,]       4     4
#[5,]       4    50.6

##5. DESeq normalization
library(DESeq)
cds <- newCountDataSet(reads_count, c(1,3))
cds <- estimateSizeFactors(cds)
deseq <- sizeFactors(cds)
normalize_count<-scale(reads_count,center = FALSE,scale =deseq)

#Control     Treat
#[1,] 3.464102  3.464102
#[2,] 3.464102  3.464102
#[3,] 3.464102  3.464102
#[4,] 3.464102  3.464102
#[5,] 3.464102 43.878620
