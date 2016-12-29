listMarts(host="plants.ensembl.org")
#ensemble = useMart("plants_mart",host="plants.ensembl.org")
#listDatasets(ensemble)
ensembl = useMart("plants_mart",dataset="athaliana_eg_gene",host="plants.ensembl.org")


#filters = listFilters(ensembl) #可用的过滤条件
#value  #必须与过滤条件对应
#attributes = listAttributes(ensembl) #可输出的属性(列名)
chrom=1
go="GO:0008150"
getBM(attributes=c("ensembl_gene_id","chromosome_name","start_position","end_position"), filters=c("go","chromosome_name"),values=list(go,chrom),mart = ensembl)

