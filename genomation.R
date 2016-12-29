library(genomation)
data(cage) #Grange对象,提供数据
data(promoters) #启动子Grange,每个区间间距必须相等,作为作图横轴的区间.
sm <- ScoreMatrix(target = cage, windows = promoters) 
plotMeta(mat=sm,overlay=TRUE,main="my plotowski",xcoords = c(-1000, 1000))
#------------------------------------------------------------------------------

data<-readGeneric(file = "MyResearch/retro_gene/mouse/mus_musculus.bed",chr = 4,start = 5,end = 6,strand = 7)#读取table
#文件,并转换为GRange对象.  #refseq.bed文件可以在UCSC数据库table中下载