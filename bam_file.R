#https://rpubs.com/achitsaz/98857

library(GenomicAlignments)

bam2RPM <- function(PathToBam, LabelName, OutputFile) {
  ### Read in BamFile 
  H3K4_X1 <- BamFile(PathToBam)
  aln <- readGAlignments(H3K4_X1)
  
  ### Convert to Granges and resize to fragment length
  aln <- as(aln, "GRanges")
  aln <- resize(aln, 150)
  
  ### Exract coverages per nt
  cov <- coverage(aln)
  nreads <- length(aln)
  
  ### Function that coverts raw cov to rpm
  rpmfun <- function(x) {
    signif(x/nreads * 10^6, 3)
  }
  
  ### Run function and convert to RleList
  rpmfile <- lapply(cov, rpmfun)
  rpmfile <- as(rpmfile, "SimpleRleList")
  assign(LabelName, rpmfile)
  
  ### Save output
  save(list=LabelName, file=OutputFile)
}