#1基本操作
alns<-IRanges(start = sample(seq_len(50),10),width=50) #
reduced_gr<-reduce(alns) 　　　　#计算两个granges覆盖度时，需要reduce去掉相互重叠的range,因为不需要关注每个grange
gaps<-gaps(start=,end=) 　　　　　#可以产生外显子之间的内含子range,以及获得基因间隔区域range;默认不会返回序列的第一位到range的起始位置(start,end可选)
intersect(range_1,range2)　　　　#相当与元素取交集
setdiff(range1,rnage2)　　　　　　　#元素取差集

#2重叠操作
hits<-findOverlaps( query_range, subject_range,type="within",select="first") #用query_range每一个range找与subject中重叠的部分,返回两列(一列是query index,对应第二列是subject index);type可选;select可选:选query对应的subject首次重叠还是其他次数重叠
countQueryHits(hits)         #返回一个向量(每个query重叠了几个subject)
countSubjectHits(hits)	     #返回
range(hits,query_range,subject_range) #返回每个query与subject的重叠部分(可以把一个query产生多个range列出)
subsetByOverlaps(query_range,subject_range)           #返回与subject有重叠的query_range
countByOverlaps(query_range,,subject_range)	      #返回每个query有几次重叠

#3临近操作
nearest(query_range,subject_range)    #找与query_range最临近的subject range,返回subject index
precede(query_range,subject_range) #上游
follow(query_range,subject_range)  #下游
distanceToNearest(query_range,subject_range)  #每个query与最近的subject的index以及它们之间的距离
distance() #分别计算对应每个range之间的距离


###Granges对象操作
gr<-GRanges(seqname=c("chr1","chr1","chr2","chr3"),range=IRanges(start=5:8,width=10),strand=c("+","-","-","+"))
start(gr)
end(gr)
width(gr)
gr[start(gr)>7] #选取
mcol(gr) #获取metadata

####GrangesList对象
gr1<-GRanges(seqname=c("chr1","chr1","chr2","chr3"),range=IRanges(start=5:8,width=10),strand=c("+","-","-","+"))
gr2<-GRanges(seqname=c("chr3","chr6","chr2","chr3"),range=IRanges(start=6:9,width=20),strand=c("+","-","-","+"))
grl<-GRangesList(gr1,gr2)
unlist(grl) #解除list
split(gr1,seqnames(gr1)) #按某例如分割granges对象
lapply(grl,function(x) order(width(x)))  #对GrangesList对象每个元素(GRanges对象)操作函数
###reduce();flank();coverage()和findOverlaps直接可以操作GrangesList对象
reduce(grl)

######GenomicFeatures
transcripsByOverlaps(tr,genes(tr)) #提取和基因想重叠的转录本 
flank(grange,width=3000) #默认往上游取3000bp
gaps(Granges) #默认考虑了正负链,产生许多冗余信息,一般情况下不考虑正负链了
subsetByOverlaps(query_range,subject_range,ignore.strand=TRUE)
 



