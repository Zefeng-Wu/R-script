library(MatrixEQTL)

## Location of the package with the data files.
base.dir = find.package('MatrixEQTL')

# Linear model to use, modelANOVA, modelLINEAR, or modelLINEAR_CROSS
useModel = modelLINEAR # modelANOVA, modelLINEAR, or modelLINEAR_CROSS

# 1. Genotype file name
SNP_file_name = paste(base.dir, "/data/SNP.txt", sep="");

# 2. Gene expression file name
expression_file_name = paste(base.dir, "/data/GE.txt", sep="")

# 3. Covariates file name; Set to character() for no covariates
covariates_file_name = paste(base.dir, "/data/Covariates.txt", sep="");

# Output file name
output_file_name = "~/Desktop/out.txt"

# Only associations significant at this level will be saved
pvOutputThreshold = 1e-2

# Error covariance matrix; Set to numeric() for identity.
errorCovariance = numeric() # errorCovariance = read.table("Sample_Data/errorCovariance.txt");

## Load genotype data

snps = SlicedData$new()
snps$fileDelimiter = "\t"     # the TAB character
snps$fileOmitCharacters = "NA" # denote missing values;
snps$fileSkipRows = 1         # one row of column labels
snps$fileSkipColumns = 1       # one column of row labels
snps$fileSliceSize = 2000      # read file in slices of 2,000 rows
snps$LoadFile(SNP_file_name)

## Load gene expression data

gene = SlicedData$new();
gene$fileDelimiter = "\t";      # the TAB character
gene$fileOmitCharacters = "NA"; # denote missing values;
gene$fileSkipRows = 1;          # one row of column labels
gene$fileSkipColumns = 1;       # one column of row labels
gene$fileSliceSize = 2000;      # read file in slices of 2,000 rows
gene$LoadFile(expression_file_name);

## Load covariates

cvrt = SlicedData$new();
cvrt$fileDelimiter = "\t";      # the TAB character
cvrt$fileOmitCharacters = "NA"; # denote missing values;
cvrt$fileSkipRows = 1;          # one row of column labels
cvrt$fileSkipColumns = 1;       # one column of row labels
if(length(covariates_file_name)>0) {
  cvrt$LoadFile(covariates_file_name);
}


## Run the analysis

me = Matrix_eQTL_engine(
  snps = snps,
  gene = gene,
  cvrt = cvrt,
  output_file_name = output_file_name,
  pvOutputThreshold = pvOutputThreshold,
  useModel = useModel, 
  errorCovariance = errorCovariance, 
  verbose = TRUE,
  pvalue.hist = TRUE,
  min.pv.by.genesnp = FALSE,
  noFDRsaveMemory = FALSE)

## plot data
res <- read.table("~/Desktop/out.txt",stringsAsFactors = F,header = T)
snp_focus = "Snp_05"
gene_foucs = "Gene_03"

gene_exp_file <- read.table(expression_file_name,header = T,row.names = 1)
snp_file <- read.table(SNP_file_name,header = T,row.names = 1)

snp_type = as.numeric(snp_file[snp_focus,])
gene_exps <- as.numeric(gene_exp_file[gene_foucs,])
dd<-data.frame(x=snp_type,y=gene_exps)
ggplot(dd,aes(x=as.factor(x),y=y))+geom_boxplot()

#################################################
## cis and trans eqtl
##################################################

## Location of the package with the data files.
base.dir = find.package('MatrixEQTL')

# Linear model to use, modelANOVA, modelLINEAR, or modelLINEAR_CROSS
useModel = modelLINEAR; # modelANOVA, modelLINEAR, or modelLINEAR_CROSS

# Genotype file name
SNP_file_name = paste(base.dir, "/data/SNP.txt", sep="")
snps_location_file_name = paste(base.dir, "/data/snpsloc.txt", sep="")

# Gene expression file name
expression_file_name = paste(base.dir, "/data/GE.txt", sep="")
gene_location_file_name = paste(base.dir, "/data/geneloc.txt", sep="")

# Covariates file name， Set to character() for no covariates
covariates_file_name = paste(base.dir, "/data/Covariates.txt", sep="")

# Output file name
output_file_name_cis = "~/Desktop/cis.out"
output_file_name_tra = "~/Desktop/trans.out"

# Only associations significant at this level will be saved
pvOutputThreshold_cis = 2e-2
pvOutputThreshold_tra = 1e-2

# Error covariance matrix; Set to numeric() for identity.
errorCovariance = numeric();
# errorCovariance = read.table("Sample_Data/errorCovariance.txt");

# Distance for local gene-SNP pairs
cisDist = 1e6

## Load genotype data
snps = SlicedData$new();
snps$fileDelimiter = "\t";      # the TAB character
snps$fileOmitCharacters = "NA"; # denote missing values;
snps$fileSkipRows = 1;          # one row of column labels
snps$fileSkipColumns = 1;       # one column of row labels
snps$fileSliceSize = 2000;      # read file in slices of 2,000 rows
snps$LoadFile(SNP_file_name);

## Load gene expression data
gene = SlicedData$new();
gene$fileDelimiter = "\t";      # the TAB character
gene$fileOmitCharacters = "NA"; # denote missing values;
gene$fileSkipRows = 1;          # one row of column labels
gene$fileSkipColumns = 1;       # one column of row labels
gene$fileSliceSize = 2000;      # read file in slices of 2,000 rows
gene$LoadFile(expression_file_name);

## Load covariates
cvrt = SlicedData$new();
cvrt$fileDelimiter = "\t";      # the TAB character
cvrt$fileOmitCharacters = "NA"; # denote missing values;
cvrt$fileSkipRows = 1;          # one row of column labels
cvrt$fileSkipColumns = 1;       # one column of row labels
if(length(covariates_file_name)>0) {
  cvrt$LoadFile(covariates_file_name);
}

## Run the analysis
snpspos = read.table(snps_location_file_name, header = TRUE, stringsAsFactors = FALSE);
genepos = read.table(gene_location_file_name, header = TRUE, stringsAsFactors = FALSE);

me = Matrix_eQTL_main(
  snps = snps, 
  gene = gene, 
  cvrt = cvrt,
  output_file_name     = output_file_name_tra,
  pvOutputThreshold     = pvOutputThreshold_tra,
  useModel = useModel, 
  errorCovariance = errorCovariance, 
  verbose = TRUE, 
  output_file_name.cis = output_file_name_cis,
  pvOutputThreshold.cis = pvOutputThreshold_cis,
  snpspos = snpspos, 
  genepos = genepos,
  cisDist = cisDist,
  pvalue.hist = "qqplot",
  min.pv.by.genesnp = FALSE,
  noFDRsaveMemory = FALSE);


## Results:
cat('Analysis done in: ', me$time.in.sec, ' seconds', '\n');
cat('Detected local eQTLs:', '\n');
show(me$cis$eqtls)
cat('Detected distant eQTLs:', '\n');
show(me$trans$eqtls)

## Plot the Q-Q plot of local and distant p-values
plot(me)
