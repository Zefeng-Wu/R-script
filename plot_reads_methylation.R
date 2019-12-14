#pdf(paste("aa","density.pdf",sep="."),width=5+5*0.3,height=0.5*(5+2))

## load Grange data with score column
library(rtracklayer)
meth<-import.gff3("/home/wuzefeng/MyResearch/methylation_database/tair/ara.meth.gff3")
meth<-meth[meth$source=="chh"]
strand(meth)<-ifelse(meth$score>=0,"+","-")
meth$score<-abs(meth$score)
split_meth<-split(meth,seqnames(meth))

### define some variables
max_chr_length <- max(end(meth))
chrom_number <- length(split_meth)
window_size <- 1000

### set the layout for ploting 
par(mfrow=c(chrom_number+2,1),oma=c(1,1,1,5),mar=c(1,1,1,5))
plot(1:10,1:10,col="white",bty="n",yaxt="n",xaxt="n",xlab="",ylab="")
mtext(paste("Methylation level in Chromosomes"),side=3,line=-1,cex=1.5)


### for loop chromosomes
i=1
for (chr in names(split_meth)){
  par(las=0,plt=c(0.1,0.9,0.1,0.9))
  temp_chr_gr<-split_meth[[chr]]
  
  
  temp_chr_gr_pos<-split(temp_chr_gr,strand(temp_chr_gr))$`+`
  temp_chr_gr_neg<-split(temp_chr_gr,strand(temp_chr_gr))$`-`
  
  ## make bin window (1000bp)
  xmax<-max(end(temp_chr_gr))
  names(xmax)<-chr
  ## make windows for each chromsomes
  tiles <- unlist(tileGenome(xmax, tilewidth = window_size))
  ## window average score
  hits_pos <- findOverlaps(tiles, temp_chr_gr_pos)
  hits_neg <- findOverlaps(tiles, temp_chr_gr_neg)
  agg_pos <- aggregate(temp_chr_gr_pos, hits_pos, score=mean(score))
  agg_neg <- aggregate(temp_chr_gr_neg, hits_neg, score=mean(score))
  
  tiles$score_pos[countQueryHits(hits_pos) > 0L] <- agg_pos$score
  tiles$score_pos[is.na(tiles$score_pos)]<-0
  tiles$score_neg[countQueryHits(hits_neg) > 0L] <- agg_neg$score
  tiles$score_neg[is.na(tiles$score_neg)]<-0
  
  ## plot scores in both pos and neg strand 
  barplot(tiles$score_pos,space=0,xlim=c(0,max_chr_length/window_size),ylim=c(-1,1),col="#66C2A5",border="#66C2A5");
  barplot(-as.numeric(tiles$score_neg),space=0,xlim=c(0,max_chr_length/window_size),add=TRUE,col="#FC8D62",border="#FC8D62");
  par(las=2)
  mtext(chr,side=4,line=1,adj=0.0)
  i=i+1
  if(i==ceiling(chrom_number/1.5)){
   par(las=0)
   mtext("Median of read density (log2)",side=2,line=3,cex=1)
}
}

###legend plot ###
par(plt=c(0.1,0.9,0.7,0.9),las=1)
plot(seq(0,max_chr_length/window_size,length=10),rep(1,10),col="white",bty="n",yaxt="n",xaxt="n",ylab="",xlab="")
axis(side=1,at=seq(0,ceiling(max_chr_length/window_size),length=8),labels=round(seq(0,ceiling(max_chr_length/window_size),length=8)/1000,1))
mtext("chromosome position (Mb)",side=1,line=2.5)
#dev.off()


## oma: 以行数为单位设置的外边界尺度, c(bottom, left, top, right)
## mar: 以行数来表示图像边距, c(bottom, left, top, right), 默认是 c(5, 4, 4, 2) + 0.1.
## bty: 设定 box 所绘制的方框的类型, plot 等函数也可以设置, 默认为 "o", 设置的型式类似其大写字母性状. "o" 代表四周都有线; "l" 代表左侧和下侧有线; "7" 代表右侧和上侧有线; "c" 代表左侧和上下侧均有线; "u" 代表左右两侧和下侧有线; "]" 代表右侧和上下侧均有线; "n" 不输出方框.
## xaxt, yaxt: 坐标轴的型式, 值为字符. "n" 表示不绘制坐标轴, 其他字符均表示绘制坐标轴
## mtext() 属于子标题，注释坐标轴。
## Line主要指的相对于坐标轴线的位置，负数线内，正数线外
## as: 设置坐标轴标签的风格, 在 0, 1, 2, 3 中取值. 0, 默认, 和坐标轴平行; 1, 水平; 2, 和坐标轴垂直; 3, 垂直. crt 和 srt 不会对其产生影响.
## plt: 当前绘图区域的范围, c(x1, x2, y1, y2), 其中每个取值是相对于当前图像输出设备的比例, 和 omd 类似.



