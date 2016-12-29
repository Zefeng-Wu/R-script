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
