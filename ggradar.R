data<-read.csv("/home/wuzefeng/MyResearch/Imprinting_prediction/imprint_gene_list/TAIR10/imp_go_enrich/GO_term_enrichment.csv")
data_temp<-data[,2:ncol(data)]
data_temp<--(log(data_temp,base = 10))
data<-data.frame(data$Term,data_temp)