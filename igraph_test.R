1.#####################数据框转换为igraph
library(igraph)
df<-data.frame(a=c(1,2,3,4,5,3,6),b=c(2,3,4,5,6,4,5))
g<-graph.data.frame(df,directed = FALSE)
g<-simplify(g,remove.multiple = TRUE,remove.loops = TRUE)
simp_df <- as.data.frame(get.edgelist(g)) #as_data_frame(g)

#2.简单统计
#e边数
ecount(g)

#节点数目
vcount(g)

#获取特定节点
V(g)[name == 1]

## 出度
E(g)[from(3)]

## 入读
E(g)[to(3)]

## 网络直径
farthest.nodes(g)


#给graph添加属性（包括节点属性和边属性）

g$layout<-layout_in_circle   # plot layout
V(g)$color<-"white"          # vertex color 
V(g)$size <- 40              # vertex size
V(g)$label.cex <- 3          # vertex label size
E(g)$color <- "black"        # edge color 
E(t1)$width <- 3             # edge width

#添加一条边
g<-add_edges(g,c(1,5),color="red")


#对网络进行划分
dg <- decompose.graph(g) # return list; for example, dg[[1]] is the first subgraph
clusters<-clusters(g)


3.#produce randow grpah
rg<-sample_gnm(50,150)
length(E(g)[5 %--% 10])


###### 计算网络属性
#1边属性

#2节点属性
#聚类系数 cluster correlation#反应个体对相邻节点之间的作用的影响
transitivity(g) # 整个网络
transitivity(g,vids = which(V(bigest.comp.network)$color=="red"),"local") #个别节点

###
betweesness


#3网络属性

g<-erdos.renyi.game(100,0.4,directed=T)#生成ER随机图表
m<-gsize(g)#获取边数
n<-vcount(g)#获取顶点数
l<-mean_distance(g)##计算联络平均路径长度
c<-transitivity(g)#计算聚类系数
degree<-degree(g,mode="all",normalized=T)#mode=in点入度;out=点出度;total点度中心度，三者统称绝对点中心度,相对点中心度=绝对点中心度/最大度数
table(degree)#度统计
plot(table(degree),type="h")#绘制直方图
degree.distribution(g)#查看度分布
closeness(g,mode="in")##计算接近中心度，点与其他点距离之和的倒数
order(closeness(g,mode="in"))#排序
betweenness(g,normalized=T)#查看点的中间中心度,代表最短距离是否经过该点
edge.betweenness(g)#查看线的中间中心度
evcent(g,scale = F)$vector#计算点的特征向量中心度
page.rank(g)$vector#计算邻接矩阵,计算点的特征向量中心度


###(一)点-点和点-网络
#1.点度中心度：入度+出度
		1.出度
		2.入度
		3.相对点中心度=绝对点中心度/最大度数： degree(g,normalized = T)
		4.点度频率

#2.接近中心度：该点与网络中其他点距离之和的倒数,越大说明越在中心,越能够很快到达其他点
closeness(g,vids=which(V(g)$label=="c"))
	相对接近中心度normalized = T

#3中间中心度：
	#1.点的中心度,代表最短距离是否都经过该点,如果都经过说明这个点很重要。也是强调
	#与网络的价值,而且更能说明转发、中介的情况
	betweenness(g,normalized = T)  
	#2. 线的中心度
	edge.betweenness

#4 特征向量中心度evcent：如果某人四周都是大牛人,那么这个人也很牛逼,这个方法是根据相邻点的重要性来衡
#量该点的价值。首先计算邻接矩阵,然后计算邻接矩阵的特征向量
 evcent(g,scale = F)$vector 

##(二)网络属性
#1中心势：中心势的原理就是比较一个网络的边缘点以及中心点的中心度的情况,
#如果一个网络很集中,那么势必是中心点,中心度高;而边缘点中心度低。
#如果一个网络很稀疏,那么中心点、边缘点的中心度没有多少差异

#1.网络聚类系数——transitivity#可以衡量网络中关联性如何,值越大代表交互关系越大。说明网络越复杂,越能放在一
块儿,聚类。

 transitivity(g)
#2.网络密度——graph.density
#跟网路聚类系数差不多,也是用来形容网络的结构复杂程度。越大,说明网络越复杂,
#说明网络越能够放在一块。
graph.density(g.zn)  

##layout
layout_with_dh #太慢,不建议用
layout.davidson.harel #太慢,不建议用

#保持度分布不变,随机网络
rewire(g,with=keeping_degseq())

