data<-matrix(rnorm(16,3,1),ncol=4)
colnames(data)<-paste("sample",c(1:ncol(data)),sep="")
rownames(data)<-paste("gene",letters[1:4],sep = "_")


Mutual_Rank<-function(exp_matrix){  #row genes and colname is samples
  exp<-cor(t(data)) # pcc  cor between genes
  mutual_rank<- t(apply(exp_cor,1,function(x)rank(-x)))
  mutual_rank_value<-sqrt((mutual_rank)**2+(t(mutual_rank))**2)
  mutual_rank_df<-data.frame(row=rownames(mutual_rank_value)[row(mutual_rank_value)[upper.tri(mutual_rank_value)]],col=colnames(mutual_rank_value)[col(mutual_rank_value)[upper.tri(mutual_rank_value)]], corr=mutual_rank_value[upper.tri(mutual_rank_value)])
  return(mutual_rank_df)
}

Threshold<-function(focus_genes){  # mutual rank threshold set
  focus_genes<-c("gene_a","gene_b")
  threshold<-max(mutual_rank_value[focus_genes,])
  return (threshold)
}

