GO_enrichemnts<-function(gene2go_file,interested_genes){
  require(topGO)
  geneID2GO <- readMappings(file = gene2go_file) 
  geneUniverse <- names(geneID2GO)
  
  ################ read interested  genes  
  genesOfInterest <- interested_genes
  geneList <- factor(as.integer(geneUniverse %in% genesOfInterest)) #98 in go term
  names(geneList) <- geneUniverse
  
  ################## build the GOdata object in topGO
  myGOdata <- new("topGOdata", description="My project", ontology="BP", allGenes=geneList,  annot = annFUN.gene2GO, gene2GO = geneID2GO)
  
  #### run the Fisher's exact tests
  resultClassic <- runTest(myGOdata, algorithm="classic", statistic="fisher")
  resultElim <- runTest(myGOdata, algorithm="elim", statistic="fisher")
  resultTopgo <- runTest(myGOdata, algorithm="weight01", statistic="fisher")
  resultParentchild <- runTest(myGOdata, algorithm="parentchild", statistic="fisher")
  
  #### see how many results we get where weight01 gives a P-value <= 0.001:
  mysummary <- summary(attributes(resultTopgo)$score <= 0.01)
  numsignif <- as.integer(mysummary[[3]]) # how many terms is it true that P <= 0.01
  
  #### print out the top 'numsignif' results:
  allRes <- GenTable(myGOdata, classicFisher = resultClassic, elimFisher = resultElim, topgoFisher = resultTopgo, parentchildFisher = resultParentchild, orderBy = "topgoFisher", ranksOf = "classicFisher", topNodes = numsignif)
  return(allRes)
}
aa<-GO_enrichemnts(gene2go_file = "/home/wuzefeng/MyResearch/Imprinting_prediction/imprint_gene_list/TAIR10/imp_go_enrich/1gene2go.out",interested_genes = partners)
aa<-aa[apply(aa,1,function(x) x[7]<0.001),]
write.csv(aa,file = "partners.go.csv",quote = FALSE,col.names = TRUE,row.names = FALSE)



