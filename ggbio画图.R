l#ggbio常用命令
#1.arrangeGrobByParsingLegend函数
library(ggbio)
p1 <- qplot(x = mpg, y= cyl, data = mtcars, color = carb)
p2 <- qplot(x = mpg, y= cyl, data = mtcars, color = wt)
p3 <- qplot(x = mpg, y= cyl, data = mtcars, color = qsec)
p4 <- qplot(x = mpg, y= cyl, data = mtcars, color = gear)
arrangeGrobByParsingLegend(p1, p2, p3, p4,ncol=2)#将四个图放
arrangeGrobByParsingLegend(p1, p2, p3, p4,legend.idx = 2)#显示第二个标签

#导入bed文件,画染色体位置图#有问题,染色体长度未知,会缩小染色体长度
library(ggbio)
library(rtracklayer)
er_ranges <-  rtracklayer::import("Desktop//test.bed", format = "bed")
autoplot(er_ranges,layout="karyogram",aes(color=er_ranges$name,fill=er_ranges$name))
#autoplot(er_ranges,layout="karyogram",aes(fill = er_ranges$name))#按指定列不同颜色填充


#可以画覆盖度
ggplot(gr) + stat_aggregate(aes(y = value), geom = "boxplot", window = 60)






##
library(GenomicRanges)
library(ggbio)

## make chromosome gr
gr <- GRanges(seqnames = c("chr1", "chr2", "chr3"),IRanges(start = c(0,0,0), end=c(400,500,700)))
seqlengths(gr) <- c(400, 500, 700)

## make plot gr
set.seed(1)
N <- 100
plot_gr <- GRanges(seqnames = sample(c("chr1", "chr2", "chr3"), size = N, replace = TRUE),
              IRanges(start = sample(1:300, size = N, replace = TRUE), width = sample(70:75,size = N, replace = TRUE)),
              strand = sample(c("+", "-", "*"),size = N,replace = TRUE),
              value = rnorm(N, 10, 3), 
              score = rnorm(N, 100, 30),
              sample = sample(c("Normal", "Tumor"),
              size = N, replace = TRUE), pair = sample(letters,size = N, replace = TRUE))
seqlengths(plot_gr) <- c(400, 500, 700)
values(plot_gr)$to.gr <- plot_gr[sample(1:length(plot_gr), size = length(plot_gr))]
idx <- sample(1:length(plot_gr), size = 50)
plot_gr <- plot_gr[idx]

###### plot 
ggplot()+ layout_circle(gr, geom = "ideo", fill = aes(fill=seqnames), radius = 7, trackWidth = 3)+
  layout_circle(gr, geom = "text", aes(label = seqnames), vjust = 0, size = 5,radius = 6)+
  layout_circle(plot_gr, geom = "bar", radius = 10, trackWidth = 4,fill="steelblue",aes(y = score))+
  layout_circle(plot_gr, geom = "point", color = "red", radius = 14,trackWidth = 3, grid = TRUE, aes(y = score))+
  layout_circle(plot_gr, geom = "link", linked.to = "to.gr", radius = 6, trackWidth= 1)
## or
ggbio()+ circle(gr, geom = "ideo", fill = aes(fill=seqnames), radius = 7, trackWidth = 3)+
  circle(gr, geom = "text", aes(label = seqnames), vjust = 0, size = 5,radius = 6)+
  circle(plot_gr, geom = "bar", radius = 10, trackWidth = 4,fill="steelblue",aes(y = score))+
  circle(plot_gr, geom = "point", color = "red", radius = 14,trackWidth = 3, grid = TRUE, aes(y = score,size=score))+
  circle(plot_gr, geom = "link", linked.to = "to.gr", radius = 6, trackWidth= 1)
