fpkm <- c()
# create directory names

cufflink_dir <- dir("/home/wuzefeng/opt/53maize_rna-seq/3cufflinks/",full.names = TRUE)
# loop through all directories and grab fpkm columns
for( i in 1:length(cufflink_dir) ){
  fname <- paste(cufflink_dir[i], "/genes.fpkm_tracking",sep="")
  x <- read.table(file=fname, sep="\t", header=T, as.is=T)
  fpkm <- cbind(fpkm, x[,"FPKM"])
}
# name the columns
colnames(fpkm) <- basename(cufflink_dir)
# name the rows, they're all in the same order
rownames(fpkm) <- x[,1]

write.table(fpkm, file="fpkm.txt", sep="\t", col.names=NA)

### sample information



