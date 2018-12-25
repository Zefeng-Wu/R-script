library(baidumap)
library(ggmap)

#获取位置纬度经度
geo_positione = get_geo_position('北京')  # 返回精度、纬度、城市名称:三列数据框


#起点-目的图
remap(mapdata, title = "", subtitle = "",theme =get_theme("Dark"))

#mapdata一个数据框对象,第一列为出发地点,第二列为到达地点
#title标题
#subtitle副标题
#theme:控制生成地图的颜色,具体将会在get_theme部分说明

####################################################
origin = rep("北京",10)
destination = c('上海','广州','大连','南宁','南昌',
'拉萨','长春','包头','重庆','常州')
dat = data.frame(origin,destination)
out = remap(dat,title = "REmap实例数据",subtitle = "theme:Dark")
plot(out)
######################################

#REmapH函数:热力图
remapH(data,maptype = 'china',theme = get_theme("Dark"),blurSize = 30, color = c('blue', 'cyan', 'lime', 'yellow', 'red'),minAlpha = 0.05,opacity = 1,...)
#data为要传入的数据，数据为三列，第一列为lon(经度)，第二列为lat(维度)，第三列为prob(密度/概率)；
#maptype为要绘制的地图类型，可选有："china"，"world"或中国各省份名字；
#theme为绘制的地图主题类型，可由get_theme函数传入；
#blurSize为热力效果的泛化范围，可调整热力点中心的扩散程度；
#color为热力的渐变颜色；
#minAlpha为热力点的展示阈值，对应data中的prob列，作图时各点密度会对比minAlpha，以凸显不同密度所展示的不同热力分布；
#opacity为透明度，调整热力图的透明度。

city_ln<-mapNames("neimenggu") #城市名字
city_list<-get_geo_position(city_ln) #得到个城市位置信息
point<-round(runif(length(city_ln),min=0.3,max=0.95),2) #产生一列热力数据
newdata<-data.frame(city_list[,1:2],point) #三列数据
map_out1<-remapH(newdata,
maptype = "内蒙古",
theme =get_theme(theme = "Dark"),
blurSize = 70,
color = "red",
minAlpha = 10,
opacity = 1,
)


###目标分布图:remapB



#制作填充图:REmapC函数

#虚构一份数据:城市名单及对应值
city <- mapNames("guangdong")
value <- runif(21,min = 1,max = 100)
 
#构建数据框
data_DF <- data.frame(city,value)
 
 
#绘制填充地图:两列数据
#example1

city <- mapNames("guangdong")
value <- runif(21,min = 1,max = 100)
result <- remapC(data_DF,
title ="广东省热力图",
maptype = "guangdong",
color = "green",
theme = get_theme("Dark"),
maxdata = 100,
mindata = 1)

#example2
province <- mapNames("china") #全国省份
value <- rnorm(34,100,30) #随机生成分省值
mydata <- data.frame(province,value) #合成数据框作图数据
remapC(mydata,color=c("yellow","red"),title="全国分省热地图",subtitle="我是副标题")
