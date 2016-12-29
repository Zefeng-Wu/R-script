##example1

library(Gviz)                                         #导入模块
#######################################################################
data(cpgIslands)                                      #导入CpG岛数据
atr <- AnnotationTrack(genome="mm9", chromosome="chr1",cpgIslands, name="CpG")        #添加注释track
#######################################################################
gtr <- GenomeAxisTrack(genome="mm9", chromosome="chr1")                              #添加基因组(线条)track
#######################################################################
itr <- IdeogramTrack(genome="mm9", chromosome="chr1") #添加染色体图形track
#######################################################################
data(geneModels)                                      #导入基因模型数据
grtr <- GeneRegionTrack(geneModels, name="Gene Model", showId=TRUE,genome="mm9", chromosome="chr1") #添加基因模型数据track
#######################################################################
plotTracks(list(itr, gtr, atr, grtr), from=26654641, to=26694641)                               #画图

#*******example2****************************************************************
#画染色体线条axisTrack
axisTrack <- GenomeAxisTrack()
plotTracks(axisTrack, from=1e06, to=9e6)
#########高亮显示一部分
axisTrack <- GenomeAxisTrack(range=IRanges(start=c(2e6, 4e6), end=c(3e6, 7e6), names=rep("N-stretch", 2)))#高亮起始和结束
plotTracks(axisTrack, from=1e6, to=9e6) #画图起始和结束
#plotTracks(axisTrack, from=1e6, to=9e6, showId=TRUE)#显示名字
#plotTracks(axisTrack, from=1e6, to=9e6, add53=TRUE, add35=TRUE)#添加链方向信息
#plotTracks(axisTrack, from=1e6, to=9e6, littleTicks=TRUE) #添加小刻度
#plotTracks(axisTrack, from=1e6, to=9e6, labelPos="below") #标签显示位置(下)

#**example3*****************************************************************************
##花染色体条带
ideoTrack <- IdeogramTrack(genome="hg19", chromosome="chrX")
plotTracks(ideoTrack, from=8e7, to=12e7)
#plotTracks(ideoTrack, from=8e7, to=12e7, showId=FALSE) #不显示染色体名字
#plotTracks(ideoTrack, from=8e7, to=12e7, showId=FALSE, showBandId=TRUE) #显示染色体体带
  
#**example4*****************************************************************************
###序列track
library(BSgenome.Hsapiens.UCSC.hg19)
sTrack <- SequenceTrack(Hsapiens)
plotTracks(sTrack, chromosome="chr1", from=20000, to=20050)


#**example5*****************************************************************************
##注释track


#**example6*****************************************************************************
##GeneRegionTrack track



#**example7*****************************************************************************
##数据track

  