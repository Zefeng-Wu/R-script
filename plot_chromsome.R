library(ggplot2)
my_data<-data.frame(chr_len=c(10,20,30,40,50),y_pos = seq(0,by = 20,length.out = length(chr_len)),chrom_id=c(1,2,3,4,5)) # chromosoems length and chromosome (y) position 

focused_df<-data.frame(xpos=c(5,10,11,13,15,25),chr_id = c(1,1,2,2,3,3),strand = c("+","+","-")) # x-pos chom_id ,stand

ggplot(my_data, aes(chr_len, y_pos))  + geom_vline(xintercept=0)  # chrom_length ~ chromo_position
                                      + geom_segment(aes(x=chr_len, xend=0, y=y_pos, yend=y_pos), colour="blue") # each chromosomes
                                      + geom_segment(data= focused_df,aes(x = xpos,y = y_pos2[chr_id],xend = xpos,yend=ifelse(strand=="-",y_pos2[chr_id]-5,y_pos2[chr_id]+5),color="blue"))
                                      + ylab("Chromosome")
                                      + scale_x_continuous()
                                      + scale_y_continuous(labels=c("Chr1","Chr2","Chr3","Chr4","Chr5")) 
