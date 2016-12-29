#---------------韦恩图1(I II III IV四类）[已知各个集合】-------------------------------------------------------------------
library(gplots)
library(VennDiagram)
venn.diagram(
  x = list(
    I = c(1:60,61:105),
    IV = c(2:61,70:120),
    II = c(3:65,80:110),
    III = c(5:100,40:80)
  ),
  filename = "3D-quadruple_Venn.tiff",#-outfile-name--#
  col = "black",                      #----border_colour#
  lty='dotted',
  lwd = 4,
  fill = c("cornflowerblue", "green", "yellow", "darkorchid1"),#filling colour,length=length(x)#
  alpha = 0.50,
  label.col = c("orange", "white", "darkorchid4", "white", "white", "white", "white", "white", "darkblue", "white", "white", "white", "white", "darkgreen", "white"),#digit colour#
  cex = 2.5,
  fontfamily = "serif",
  fontface = "bold",
  cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"),
  cat.cex = 2.5,
  cat.fontfamily = "serif"
)
#---------------韦恩图1(I II III IV四类）[0,1 分布]-------------------------------------------------------------------
venn.plot <- draw.quad.venn(
  area1 = 72,
  area2 = 86,
  area3 = 50,
  area4 = 52,
  n12 = 44,
  n13 = 27,
  n14 = 32,
  n23 = 38,
  n24 = 32,
  n34 = 20,
  n123 = 18,
  n124 = 17,
  n134 = 11,
  n234 = 13,
  n1234 = 6,
  category = c("First", "Second", "Third", "Fourth"),
  fill = c("orange", "red", "green", "blue"),
  lty = "dashed",
  cex = 2,
  cat.cex = 2,
  cat.col = c("orange", "red", "green", "blue")
)




#-------------------------------------------------------------------------------------------------------
line_num=0
for(i in a$V1:a$V3){
if (i >=5)
  
print(index(i))
}

#---------------------------------------------ks_test_gammma--------------------------------------------
for(i in 1:50){
  for(m in 1:50){
    fit<-ks.test(data$V1,"pgamma",i,m);
    print(fit$p.value)}
}

#---------------------------------------------3维饼图-----------------------

library("plotrix")
rna<-read.table("C://Users/Administrator.ZGC-20130202MIX/Desktop/test_small.txt")#导入数据
ratio<-sprintf("%.2f",100*rna[,2]/sum(rna[,2]))
ratio<-paste(ratio,"%",sep="")
label<-paste(rna[,1],ratio,sep="\n")
pie3D(rna[,2],col=1:6,main="test",border="purple",labels=label,font=2,labelcex=0.8,explode=0.1,radius=0.95)

#--------------------------------------------热谱图1heatmap--------------------------------
a<-read.table("C://Users/Administrator.ZGC-20130202MIX/Desktop/heatmap_chr1_chg_100bp.txt")
colnames(a)<-as.factor(colnames(a))
rownames(a)<-as.factor(rownames(a))
b<-as.matrix(a)
library(gplots)
heatmap.2(b,distfun=function(x) dist(x,method='euclidean'),hclustfun=function(x) hclust(x,method='centroid'),dendrogram="row",labRow=NA,col=colorRampPalette(c("white","brown")),trace="none",density.info="none",srtCol=45) #对行聚类
heatmap.2(b,distfun=function(x) dist(x,method='euclidean'),hclustfun=function(x) hclust(x,method='centroid'),labRow=NA,col=colorRampPalette(c("white","brown")),trace="none",density.info="none") #对行列都聚类


#-------------------------------------------误差图(t-检验）-----------------------------
library(gplots)
a<-read.csv("C://Users/Administrator.ZGC-20130202MIX/Desktop/HELIX.csv")
b<-edit(a)
heights<-tapply(distance,class,mean)
lower<-tapply(distance,class,function(v) t.test(v)$conf.int[1])
upper<-tapply(distance,class,function(v)t.test(v)$conf.int[2])
barplot2(heights,plot.ci=TRUE,ci.l=lower,ci.u=upper,xpd=FALSE,main="test",names.arg=c("XPD","CHL1","RTEL","FANCJ"),cex.axis=0.8,cex=0.8,width=c(20,20,20,20))


#---------------------------------
sqrt(sd(rtel))=3.21
c[class==1,]$distance#提取1、2、3、4

#-------------------------------------带误差棒（自设）的条形图
library(gplots)
a<-read.csv("C://Users/Administrator.ZGC-20130202MIX/Desktop/HELIX.csv")
b<-edit(a)
heights<-tapply(distance,class,mean)
lower<-0.90*heights
upper<-1.10*heights
barplot2(heights,plot.ci=TRUE,ci.l=lower,ci.u=upper,xpd=FALSE,main="test",names.arg=c("XPD","CHL1","RTEL","FANCJ"),cex.axis=0.8,cex=0.8,width=c(10,10,10,10))
abline(h=0)
#-------------------------------------直方图（连续性随机变量）------------
x<-c(1,2,3,4,5,6,7,8,6,4,3,2,2,1)
hist(x,breaks=12,col="green",xlab="testx",ylab="testy") #break:指定组数

#-------------------------------------热谱平面图

heli<-read.csv('F://hel-scannner在不同参数的下的统计/wheat.csv',head=TRUE)
library(RColorBrewer)
rownames(heli)<-heli[,1]
colnames(heli)<-c("head",5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50)
data_matrix<-data.matrix(heli[,-1])
pal=brewer.pal(9,"YlOrRd")
breaks<-seq(0,40000,40000/8)
layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(8,1), heights=c(1,1))
par(mar = c(5,10,4,2),oma=c(0.2,0.2,0.2,0.2),mex=0.5)#Set margins for the heatmap
image(x=1:nrow(data_matrix),y=1:ncol(data_matrix),   
      z=data_matrix,
      axes=FALSE,
      xlab="head scores",
      ylab="tail scores",
      col=pal[1:(length(breaks)-1)], 
      breaks=breaks,
      main=NULL)
axis(1,at=1:nrow(data_matrix),labels=rownames(data_matrix), col="white",las=1,font=0.1)
axis(2,at=1:ncol(data_matrix),labels=colnames(data_matrix), col="white",las=1)
abline(h=c(1:ncol(data_matrix))+0.5, 
       v=c(1:nrow(data_matrix))+0.5, col="white",lwd=2,xpd=FALSE)
breaks2<-breaks[-length(breaks)]

#margin too small:
par(mar = c(5,1,4,7))
image(x=1, y=0:length(breaks2),z=t(matrix(breaks2))*1.001,
      col=pal[1:length(breaks)-1],
      axes=FALSE,
      breaks=breaks,
      xlab="", ylab="",
      xaxt="n")
axis(4,at=0:(length(breaks2)-1), labels=breaks2, col="white", las=1)
abline(h=c(1:length(breaks2)),col="white",lwd=2,xpd=F)

#jitter plot
set.seed(1)
t1 = rnorm(10)
t2 = rnorm(10, 2)
t1_g2 = rnorm(10, 4)
t2_g2 = rnorm(10)

##Don't print the axes labels
par(ann=FALSE)

##Plot first set of data.
##Need to check for sensible ranges
##Use the jitter function to spread data out.
plot(jitter(rep(0,10),amount=0.2), t1, xlim=range(-0.5,3.5), ylim=range(-3,8),axes=FALSE,frame.plot=TRUE)
points(jitter(rep(1,10), amount=0.2), t1_g2, col=2)
points(jitter(rep(2,10), amount=0.2), t2)
points(jitter(rep(3,10), amount=0.2), t2_g2, col=2)

##Add in the y-axis
axis(2, seq(-4,8,by=2))

##Add in the x-axis labels
mtext("Treatment 1", side = 1, at=0.5)
mtext("Treatment 2", side = 1, at=2.5)

##Add in the means
segments(-0.25, mean(t1), 0.25, mean(t1))
segments(0.75, mean(t1_g2), 1.25, mean(t1_g2))
segments(1.75, mean(t2), 2.25, mean(t2))
segments(2.75, mean(t2_g2), 3.25, mean(t2_g2))

##Add in the legend
legend(0, 8, c("Group 1", "Group 2"), col=1:2, pch=1)




#-------------------------ggplot颜色渐变图----
library(ggplot2)

#Some sample data
x <- sort(runif(100))
dat <- data.frame(x = x,y = x^2 + 1)
# Some external vector for the color scale
col <- sort(rnorm(100))

qplot(x, y, data=dat, colour=col) + scale_colour_gradient(low="red", high="blue")


###########热图
library(pheatmap)
> pheatmap(data,fontsize=9, fontsize_row=6) #最简单地直接出图
> pheatmap(data, scale = "row", clustering_distance_row = "correlation", fontsize=9, fontsize_row=6) #改变排序算法
> pheatmap(data, color = colorRampPalette(c("navy", "white", "firebrick3"))(50), fontsize=9, fontsize_row=6) #自定义颜色
> pheatmap(data, cluster_row=FALSE, fontsize=9, fontsize_row=6) #关闭按行排序
> pheatmap(data, legend = FALSE, fontsize=9, fontsize_row=6) #关闭图例
> pheatmap(data, cellwidth = 6, cellheight = 5, fontsize=9, fontsize_row=6) #设定格子的尺寸
> color.map <- function(mol.biol) { if (mol.biol=="ALL1/AF4") 1 else 2 }
> patientcolors <- unlist(lapply(esetSel$mol.bio, color.map))
> hc<-hclust(dist(t(data)))
> dd.col<-as.dendrogram(hc)
> groups <- cutree(hc,k=7)
> annotation<-data.frame(Var1=factor(patientcolors,labels=c("class1","class2")),Var2=groups)
> pheatmap(data, annotation=annotation, fontsize=9, fontsize_row=6) #为样品分组
> Var1 = c("navy", "skyblue")
> Var2 = c("snow", "steelblue")
> names(Var1) = c("class1", "class2")
> ann_colors = list(Var1 = Var1, Var2 = Var2)
> pheatmap(data, annotation=annotation, annotation_colors = ann_colors, fontsize=9, fontsize_row=6) #为分组的样品设定颜色



################### pie
library(ggplot2)
dt<-read.table("/home/wuzefeng/文档/other/syl/plot/Alfalfa.Unigene.fa.nr.lib.stat",sep="\t")
dt<-dt[order(dt$V2,decreasing = TRUE),]

myLabel = as.vector(dt$V1)   
myLabel = paste(myLabel, "(", round(dt$V2 / sum(dt$V2) * 100, 2), "%)", sep = "")
p = ggplot(dt, aes(x = "", y = V2, fill = V1)) + 
  geom_bar(stat = "identity", width = 1) +    
  coord_polar(theta = "y") + 
  labs(x = "", y = "", title = "") + 
  theme(axis.ticks = element_blank()) + 
  theme(legend.title = element_blank(), legend.position = "right") + 
  scale_fill_discrete(breaks = dt$V1, labels = myLabel)      

