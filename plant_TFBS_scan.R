library(JASPAR2016)
library(TFBSTools)
opts<-list()  #set filter conditions
opts[["tax_group"]]<-"plants"
PFMatrixList <- getMatrixSet(JASPAR2016, opts)  #get pf Matrix List

pwmList <- PWMatrixList(A=toPWM(PFMatrixList[["MA1097.1"]]), B=toPWM(PFMatrixList[["MA1098.1"]]), use.names=TRUE)# get pwmtrix list
library(Biostrings)
subject = DNAString("GAATTCTCTCTTGTTGTAGTCTCTTGACAAAATG")
sitesetList = searchSeq(pwmList, subject, seqname="seq1",min.score="60%", strand="*")
head(writeGFF3(sitesetList))


