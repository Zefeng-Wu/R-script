library(GenomicFeatures)
library(Biostrings)
library(BSgenome)
library(stringr)

### parse  genome and gtf files for 7 species

fa_list <-c("/home/wuzefeng/MyResearch/genome.db/TAIR/dna/Arabidopsis_thaliana.TAIR10.31.dna.toplevel.fa",
            "/home/wuzefeng/MyResearch/genome.db/Arabidopsis_lyrata/dna/Arabidopsis_lyrata.v.1.0.31.dna.toplevel.fa",
            "/home/wuzefeng/MyResearch/genome.db/rice/gtf/Oryza_sativa.IRGSP-1.0.31.chr.fa",
            "/home/wuzefeng/MyResearch/genome.db/Maize/gtf/Zea_mays.AGPv3.31.chr.fa",
            "/home/wuzefeng/MyResearch/genome.db/Sbicolor/dna/Sorghum_bicolor.Sorbi1.31.dna.toplevel.fa",
            "/home/wuzefeng/MyResearch/genome.db/Solanum_lycopersicum/dna/Solanum_lycopersicum.SL2.50.31.dna.toplevel.fa")
            #"/home/wuzefeng/MyResearch/genome.db/caster_bean/dna/TIGR_castorWGS_release_0.1.assembly.fsa") # castor gff format problems

gtf_list <-c("/home/wuzefeng/MyResearch/genome.db/TAIR/gtf/Arabidopsis_thaliana.TAIR10.31.protein_coding.gtf",
             "/home/wuzefeng/MyResearch/genome.db/Arabidopsis_lyrata/gtf/Arabidopsis_lyrata.v.1.0.31.gtf",
             "/home/wuzefeng/MyResearch/genome.db/rice/gtf/Oryza_sativa.IRGSP-1.0.31.chr.gtf",
             "/home/wuzefeng/MyResearch/genome.db/Maize/gtf/Zea_mays.AGPv3.31.chr.gtf",
             "/home/wuzefeng/MyResearch/genome.db/Sbicolor/gtf/Sorghum_bicolor.Sorbi1.31.gtf",
             "/home/wuzefeng/MyResearch/genome.db/Solanum_lycopersicum/gtf/Solanum_lycopersicum.SL2.50.31.gtf")
             #"/home/wuzefeng/MyResearch/genome.db/caster_bean/gff/TIGR_castorWGS_release_0.1.model.gff")


for(m in 1:length(fa_list)){
  message(fa_list[m],gtf_list[m])
  tx0<-makeTxDbFromGFF(gtf_list[m])
  fa0<-readDNAStringSet(fa_list[m])
  names(fa0)<-unlist(lapply(strsplit(names(fa0)," "),function(x)x[1])) # scaffold id too long
  if(m==3){ #rice
    genes<-genes(tx0)
    genes<-genes[startsWith(genes$gene_id,"OS")]
  }
  else{
    genes<-genes(tx0)
  }
  seqlengths(genes)<-width(fa0)[match(names(seqlengths(genes)),names(fa0))]  # genes's seqlength
  message(length(genes))
  ### get all promoters Granges by object genes ids
  promt<-promoters(genes,upstream = 500,downstream = 500)
  promt<-trim(promt) # trim non_circular chromosome 
  promt<-restrict(promt,end=seqlengths(promt))
  
  ### Get promoter sequences
  promoter_seq<- getSeq(fa0, promt)
  names(promoter_seq)<-promt$gene_id
  writeXStringSet(promoter_seq,paste("/home/wuzefeng/Desktop/pp/promoter","1000.fa",m,sep="."),format = "fasta",append = TRUE)
  message("OK")
}

####
#### castor bean

fa<-readDNAStringSet("/home/wuzefeng/MyResearch/genome.db/caster_bean/dna/TIGR_castorWGS_release_0.1.assembly.fsa")
castor_bean<-read.table("/home/wuzefeng/MyResearch/genome.db/caster_bean/gff/TIGR_castorWGS_release_0.1.model.gff",stringsAsFactors = FALSE,sep="\t")
genes_items<-subset(castor_bean,castor_bean$V3=="mRNA")
genes_gr<-GRanges(seqnames = genes_items$V1,ranges = IRanges(start = genes_items$V4,end = genes_items$V5),strand = genes_items$V7,gene_id= genes_items$V9)
genes_gr$gene_id<-unlist(lapply(strsplit(genes_gr$gene_id,";"),function(x)str_replace_all(x[1],"mRNA","")))
seqlengths(genes_gr)<-width(fa)[match(names(seqlengths(genes_gr)),names(fa))]
### get all promoters Granges by object genes ids
promt<-promoters(genes_gr,upstream = 1000,downstream = 0)
promt<-trim(promt) # trim non_circular chromosome 
promt<-restrict(promt,end=seqlengths(promt))

### Get promoter sequences
promoter_seq<- getSeq(fa, promt)
names(promoter_seq)<-promt$gene_id
writeXStringSet(promoter_seq,"/home/wuzefeng/MyResearch/genome.db/caster_bean/promoter/promoter.1000.fa",format = "fasta",append = TRUE)
message("OK")

