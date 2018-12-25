library(cgdsr)
library(DT)
mycgds<-CGDS("http://www.cbioportal.org/public-portal/")
all_TCGA_studies<- getCancerStudies(mycgds)

stad2014<-"stad_tcga_pub"
all_tables <- getCaseLists(mycgds,stad2014)
all_dataset <- getGeneticProfiles(mycgds,stad2014) 

DT::datatable(all_dataset,extensions = "FixedColumns",options = list(scrollx=TRUE,fixedColumn=TRUE))

my_dataset<-'stad_tcga_pub_rna_seq_v2_mrna'
my_table <-'stad_tcga_pub_rna_seq_v2_mrna'
BRCA1<-getProfileData(mycgds,"BRCA1",my_dataset,my_table)

clinicaldata <- getClinicalData(mycgds,my_table)
DT::datatable(clinicaldata,extensions = "FixedColumns",options = list(scrollx=TRUE,fixedColumn=TRUE))


## find gene expression in specific sample
mycancerastudy = "brca_tcga"
#getCaseLists(mycgds,mycancerastudy)
mycaselist<-'brca_tcga_mrna'
mygeneticprofile <-'brca_tcga_mrna'
xpr <- getProfileData(mycgds,c('BRCA1','BRCA2'),mygeneticprofile,mycaselist)

myclinicaldata <-getClinicalData(mycgds,mycaselist)
DT::datatable(clinicaldata,extensions = "FixedColumns",options = list(scrollx=TRUE,fixedColumn=TRUE))

### mutation data
mutaGene <- c("EGFR","PTEN","TP53","ATRX")
mut_df <-getProfileData(mycgds,caseList = "gbm_tcga_sequenced",geneticProfiles = "gbm_tcga_mutations",genes=mutaGene)
mut_df<-apply(mut_df,2,as.factor)
mut_df[mut_df=="NaN"] = ""
mut_df[is.na(mut_df)] = ""
mut_df[mut_df!=""] = "MUT"

DT::datatable(mut_df)
library(ComplexHeatmap)
