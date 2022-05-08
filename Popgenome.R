library(PopGenome)

# read data with vcf or fasta formatted snp files in a folder 
vcf_dir <- "/home/wuzefeng/R/x86_64-pc-linux-gnu-library/3.6/PopGenome/data/vcf/"
snp<-readData(vcf_dir,format = "VCF")

# make a summary
get.sum.data(snp)

# making groups based on group names and return two lists
pops<-get.individuals(snp)[[1]]
pop1<-pops[1:18]
pop2<-pops[19:36]

snp<-set.populations(snp,list(pop1,pop2))
snp@populations 

# calculate Fst
snp <- F_ST.stats(snp)
get.F_ST(snp)  # haplotype and nucelotide level fst

# calculate diversity
get.diversity(snp)[[1]]

# sliding windows calcuate
win_snp <- sliding.window.transform(snp, width = 10000, jump = 2000,type = 2)
win_snp <- F_ST.stats(win_snp)

win_snp@nucleotide.F_ST      # One column
win_snp@nuc.diversity.within # A dataframe with two columns for pop1 and pop2

# visulization for fst
library(ggplot2)
win_fst <- data.frame(x=1:dim(win_snp@nucleotide.F_ST)[1],y=win_snp@nucleotide.F_ST[,1])
head(win_fst)
p1<-ggplot(win_fst,aes(x=x,y=y))+
  geom_point()+
  geom_line()+
  theme_classic()+
  scale_x_continuous(breaks = win_fst$x,labels = win_fst$x)+
  labs(x=NULL,y="Fst")


# visulization for diversity
pop1_div  <- win_snp@nuc.diversity.within[,1] # diversity among pop1
pop2_div  <- win_snp@nuc.diversity.within[,2] # diversity among pop2

df1<-data.frame(x=1:length(pop1_div),y=pop1_div)
df2<-data.frame(x=1:length(pop2_div),y=pop2_div)
p2<-ggplot()+
  geom_line(data=df1,aes(x=x,y=y),color="red")+
  geom_point(data=df1,aes(x=x,y=y),size=2,color="red")+
  geom_line(data=df2,aes(x=x,y=y),color="blue")+
  geom_point(data=df2,aes(x=x,y=y),size=2,color="blue")+
  theme_bw()+labs(x=NULL,y="Diversity")

# Neutrality statistics
snp <- neutrality.stats(snp)
get.neutrality(snp)
get.neutrality(snp)[[1]] # first pop1
  snp@Tajima.D
