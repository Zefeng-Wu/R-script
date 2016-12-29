library(ggplot2)
#qplot:quick plot,与经典的plot（）函数类似
#qplot(x, y = NULL, ..., data, facets = NULL, margins = FALSE, geom = "auto", stat = list(NULL), position = list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), asp = NA)

##-----------qplot散点图-------------------
datax<-diamonds[sample(53940,100),seq(1,7)] #从钻石数据随机抽取100个样，取前7个指标
attach(datax)
qplot(x=carat,y=price,colour=cut,shape=cut,main="qplot")
theme_set(theme_gray())#背景设置

##------------qplot曲线图（接上图数据）-------------------
qplot(price, data = diamonds, fill=cut, geom = "histogram", main = "histogram")#直方图和箱线图都可以添加填充颜色参数
qplot(price, data = diamonds, color=cut, geom = "density", main = "density")


qplot(x=carat,y=price,colour=cut,geom=c("point","line"),main="qplot")#geom=list（）可设置多种曲线类型
qplot(carat, price, data = diamonds, color=cut, geom = "smooth", main = "smooth")
qplot(cut, price, data = diamonds, fill=cut, geom = "boxplot", main = "boxplot")
##-------------------柱状图(多因子)
a<-read.csv('F://CD-HIT聚类进化/wheat-family-plot/family_ggplot.csv',head=TRUE)
gg_normal <-  ggplot(data = my_data, aes(x = factor(family), fill = subgenome))
gg_normal + geom_bar(position = "fill")+ ggtitle("Distribution")+ylab("Ratio") + 
  xlab("Helitron Family") + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  
##-------------------ggplot2 热谱图----------------------
library(ggplot2)
a<-data.frame(c(5,10,10,20,25,30,35,40,50),c(0,5,10,10,20,25,30,35,40),c(200,180,140,120,100,98,0,0,0))
colnames(a)<-c('head','tail','num') #三列数据，第三列就是想要展现的数据，相当于两个坐标轴
p<-ggplot(data = a,aes(a$head,a$tail,fill=a$num))#如果x和y是离散变量，则需a$head<-as.factor(a$head)
p+geom_tile()+xlab('head')+ylab('tail')+scale_fill_gradient(low = "yellow",high = "red")#low:最低颜色值，high：最高颜色值

##------------------箱线图
p<-ggplot(a,aes(x=Class,y=Tail)) #a:数据；x:分类变量，y是一组数据
p+geom_boxplot(col='blue',pch=16,cex=1)+theme(text = element_text(size=12))

ggplot(data,aes(class, data[,m]))+geom_boxplot()  #根据第m列的数据按照class列的因子进行boxplot作图.

##Case-control 误差线图(两组对照)
library(ggplot2)
Normal <- c(0.83, 0.79, 0. 99, 0.69)
Cancer <- c(0.56, 0.56, 0.64, 0.52)
m <- c(mean(Normal), mean(Cancer))
s <- c(sd(Normal), sd(Cancer))
d <- data.frame(V=c("Normal", "Cancer"), mean=m, sd=s)
d$V <- factor(d$V, levels=c("Normal", "Cancer"))
p <- ggplot(d, aes(V, mean, fill=V, width=.5))
p <- p+geom_errorbar(aes(ymin=mean, ymax=mean+sd, width=.2),position=position_dodge(width=.8))
p <- p + geom_bar(stat="identity", position=position_dodge(width=.8), colour="black")
p <- p + scale_fill_manual(values=c("grey80", "white"))
p <- p + theme_bw() +theme(legend.position="none") + xlab("") + ylab("")
p <- p + theme(axis.text.x = element_text(face="bold", size=14), axis.text.y = element_text(face="bold", size=14))
p <- p+scale_y_continuous(expand=c(0,0), limits=c(0, 1.2), breaks=seq(0, 1.2, by=.2))
p <- p+geom_segment(aes(x=1, y=.98, xend=1, yend=1.1))
p <- p+geom_segment(aes(x=2, y=.65, xend=2, yend=1.1))
p <- p+geom_segment(aes(x=1, y=1.1, xend=2, yend=1.1))
p <- p + annotate("text", x=1.5, y=1.06, label="*")

#多个变量做箱线图
a <- data.frame(group = "a", value = rnorm(10))
b <- data.frame(group = "b", value = rnorm(100))
c <- data.frame(group = "c", value = rnorm(1000))

plot.data <- rbind(a, b, c)
library(ggplot2)
ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot()

#多组变量做密度图：
ggplot(df,aes(x=value))+geom_density(aes(colour=group,fill=group),alpha=I(0.2)) #参数I控制透明度，不然回叠加到一块的

##################柱状图
#############################
# > df                      #
# num            class      #
# 1  210  intra_chromsome   #
# 2 4252 inter_chromosome   #
#############################

ggplot(df,aes(x=class,y=num,fill=class,group=factor(1)))+geom_bar(stat = "identity")

#一维离散变量做柱状图
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity"，fill="steelblue")

#一维连续变量做直方图(density)
ggplot(as.data.frame(data),aes(data))+geom_histogram(binwidth = 20)+xlab("a")+ylab("b")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
ggplot(as.data.frame(data),aes(data))+geom_histogram(binwidth = 20,aes(fill = ..count..))+scale_fill_gradient("Count", low = "green", high = "red")

######变count为密度曲线
ggplot(dat, aes(x=rating)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

########## 添加垂直线,显示平均值
ggplot(dat, aes(x=rating)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(rating, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)



##饼图
library(ggplot2)
rna<-read.table("/home/wuzefeng/文档/other/syl/plot/Alfalfa.Unigene.fa.nr.lib.stat",sep="\t")
values<-rna$V2 #value
labels<-rna$V1  #name
colours=c("#8dd3c7", "#ffffb3", "#bebada", "#80b1d3", "#fb8072", "#fdb462", "#b3de69", "#d9d9d9", "#fccde5","#FF1493","#A52A2A")
percent_str <- paste(round(values/sum(values) * 100,1), "%", sep="")
values <- data.frame(Percentage = round(values/sum(values) * 100,1), Type = labels,percent=percent_str )
pie <- ggplot(values, aes(x = "" ,y = Percentage, fill = Type)) +  geom_bar(width = 3,stat = "identity") 
pie = pie + coord_polar("y")
pie = pie + xlab('') + ylab('') + labs(fill="Types")
pie<-pie+theme(axis.ticks = element_blank()) #	去掉左上角小胡子
#加比例
#pie+geom_text(aes(y = Percentage/2+c(0, cumsum(Percentage)[-length(Percentage)]), label = percent), size=3)
#自动颜色填充
#pie + scale_fill_manual(values = colours,labels = labels)

##重设标签，假入百分比
myLabel = as.vector(rna$V1)   ## 转成向量，否则图例的标签可能与实际顺序不一致
myLabel = paste(myLabel, "(", round(rna$V2 / sum(rna$V2) * 100, 2), "%)", sep = "")   ## 用 round() 对结果保留两位小数

pie<-pie+scale_fill_discrete(breaks = labels, labels = myLabel) 

#stacked barplot
"""
product	year	export	percentage	sum
copper	2006	4176	79	5255
copper	2007	8560	81	10505
copper	2008	6473	76	8519
copper	2009	10465	80	13027
copper	2010	14977	86	17325
copper	2011	15421	83	18629
copper	2012	14805	82	18079
copper	2013	15183	80	19088
copper	2014	14012	76	18437
others	2006	1079	21	5255
others	2007	1945	19	10505
others	2008	2046	24	8519
others	2009	2562	20	13027
others	2010	2348	14	17325
others	2011	3208	17	18629
others	2012	3274	18	18079
others	2013	3905	20	19088
others	2014	4425	24	18437
"""
#for gene expression

family	up_down	number	percentage	sum
TCA	up	20	20		100
TCA	down	80	80		100
p53	up	100	10		1000
p53	down	900	90		1000

charts.data <- read.csv("/home/wuzefeng/R/test.table",sep="\t")
charts.data <- ddply(charts.data, .(year), transform, pos = cumsum(percentage) - (0.5 * percentage))
p10 <- ggplot() + geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, stat="identity")
p10+coord_flip()+geom_text(data=charts.data,aes(x = year,y=105,label=sum))+geom_text(data=charts.data, aes(x = year, y = pos, label = paste0(percentage,"%")),size=4)

data <- read.csv("/home/wuzefeng/R/test.table",sep="\t")
p10<-ggplot() + geom_bar(aes(y = percentage, x = family, fill = up_down), data = data, stat="identity")
data <- ddply(data, .(family), transform, pos = cumsum(percentage) - (0.5 * percentage))
p10+coord_flip()+geom_text(data=data, aes(x = family, y = pos, label = paste0(percentage,"%")),size=4)+geom_text(data=data,aes(x = family,y=105,label=sum))


##########coverage plot 
set.seed(45)
chr <- rep(paste0("chr", 1:3), each=100)
pos <- rep(1:100, 3)
cov <- sample(0:500, 300)
df  <- data.frame(chr, pos, cov)

require(ggplot2)
p <- ggplot(data = df, aes(x=pos, y=cov)) + geom_area(aes(fill=chr))
p + facet_wrap(~ chr, ncol=1)
