library(Biostrings)
a <- readDNAStringSet("TSS500.fa")
g<-oligonucleotideFrequency(a,width = 8)
g<-as.data.frame(g)
rownames(g)<-names(a)

imprinted_genes<-read.table("~/MyResearch/Imprinting_prediction/imprint_gene_list/TAIR10/ara_imp_by_paper_num/imp2+.list",stringsAsFactors = TRUE)
g$class<-ifelse(rownames(g)%in%imprinted_genes$V1,1,0)

library(e1071)
Naive_Bayes_Model <- naiveBayes(class ~., data=g) # train model
NB_Predictions <- predict(Naive_Bayes_Model,train_data,type="raw")  # prediction with post probabality
table(ifelse(NB_Predictions[,1]>=NB_Predictions[,2],"neg","pos"),train_data$class) # confuse matrix

### plot
require(ggplot2)
require(precrec)
eval1 <- evalmod(scores = NB_Predictions[,2],labels = train_data$class)
autoplot(eval1)
aucs <- auc(eval1) #

