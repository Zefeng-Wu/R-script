##R学习---------------------
#批处理
R CMD BATCH options infile.R outfile.Rout
source infile.R
#R批处理输出数据需借助print()函数

#矩阵赋值：
r_names<-c('t1','t2','t3','t4')
c_names<-c('f1','f2','f3','f4','f5')
a<-matrix(1:20,nrow=4,dimnames=list(r_names,c_names))


#which()函数
a<-c(1:10)
a[which(a>=5)] #得到(5,6,7,8,9，10)

#apply()函数：按行或列执行函数,针对全数字数据框或矩阵，否则用改用lapply或sapply
a<-data.frame(c(1:10),c(2:11))
apply(a,2,mean)#按列（2）取平均值，得到列数的个数的列表
apply(a,1,mean)#按行取平均数，得到行数个数字的列表

#sapply函数，针对数据框（含有混合数据等）
a<-data.frame(c(1:10),c(2:11))


#连乘
1-cumprod(seq(365,302))[64]/(365**64)
[1] 0.9971905
 cumprod(seq(365,356))[10]/(365**10)
[1] 0.8830518

# R语言命令行参数；执行时：R xx.R 参数1
args<-commandArgs(TRUE)
header<-eval(parse(text=args[1]))

#R 统计列表各个元素的数目
table()
a<-c(1,2,3,4,5,3,5)
table(a)

#R 语言读取大文件

con <- file("C://Users/Administrator.ZGC-20130202MIX/Desktop/test.fasta", "r")
line=readLines(con,n=1)
while( length(line) != 0 ) {
  #print(line)
  line=readLines(con,n=1)
  b= unlist(strsplit(line,"\t"))
  print b
}
close(con)


#strsplit()与paste()函数相反


#数据框操作,赋值，过滤
data<-data.frame(x=rnorm(100),y=rnorm(100),z=c(rep("imprinted",30),rep("unimprinted",70)))
subset(data$x,data$z=="imprinted")
data[apply(data,1,mean)>3,]#提取每行均值大于3的数据
#for 循环
for (m in seq(1,100)){aa=data$x[m];if (aa>1){print (aa)}}
#定义函数
a<-function(x,y){
  print (x*y)
}
#a(3,5)
#产生多列随机数
data<-as.data.frame(matrix(rnorm(1000),nrow = 100))


#读取不同列数据
fc <- file("mylist.txt")
mylist <- strsplit(readLines(fc), " ") #可以指定分割
close(fc)

##数据框过滤
test[which(test[,1]>1),] #选出第一列大于1的数据
test[which(test$a>1),]
subset(test,test$a>1)

##数据框按条件替换某列
test[test>1]<-3

##数据框按行table
xx=apply(data.gt1,1,function(x) paste(x,collapse=""))
table(as.numeric(table(xx)))

#数据框按连续3列取平均值，添加到新的dataframe中
empty_data.frame<-data.frame(V1=rnorm(100)) #产生长度为100行的空dataframe
for(m in 1:2){new[m]<-(rowMeans(data[,c(2*m-1,2*m)]))}

#相关系数矩阵变为两列数据框 
data.frame(row=rownames(m)[row(m)[upper.tri(m)]],col=colnames(m)[col(m)[upper.tri(m)]], corr=m[upper.tri(m)])

#矩阵计算
apply(data,1,sum)
rowSums(data)

##选择特定行数
partial_dataframe<-dataframe[match(object_lines_items,rownames(dataframe)),]

#对数据框某列进行ifelse修改
dataframe$col.name<-ifelse(dataframe$col.name>0,1,0)

#两个数据框按某个共同列合并
merge(df1,df2,by="shared_colname")

#按分隔符分割数据框中的某列,变成两列的数据框
library(reshape)
new.df<-colsplit(vector,":",c("new_column1","new_colunm2")) #遇到"."时用"\\."

##将数据框的某列作为key,对应另外某列的值作为值,组成一个list
#df,colunm a作为key,colunmn b作为values
colunma_list<-vector(mode="list",length=length(unique(df$a)))
names(colunma_list)<-unique(df$a)
for (m in names(colunma_list)){colunma_list[[1]]<-as.vector(df$b[df$a==m])}

###获取某个文件夹下的某些特定文件,组成文件列表
dir(path = "",pattern = "",all.files = TRUE)

##### split string
gsub("a:","","a:123") #get 123  #可加fixed =TRUE 分割特殊字符

####删除(merge)数据框某列(某几列)数据
rc[,n]<-rowSums(rc[,c(col.name.to.merge)])
rc<-rc[,!(colnames(rc) %in% col.name.to.drop)]

###### get the number  of the column
which(colnames(df)=="col_name")

#scale 
x <- matrix(1:10, ncol = 2)
scale(x,center = a,scale = b)
#center参数:    1.向量:每个元素对应一列,然后用每列的元素减去该值
                #2.TRUE : 每个元素减去每列"均值"	
                #3.FALSE: 不做减法

#scale参数:     1.向量:每个列元素除以对应的向量元素
                #2.TRUE: 1.center=TRUE
                         #2.center向量:
               #3. FALSE:不做除法

##向量a中离均值最近的n个数
a[order(abs(a-mean(a)))][1:n]


###向量按分位数分组
library(dplyr)
ntile(x, 4)　#返回分类因子
##或
library(Hmisc) 
cut2(x, g=4) 



#####提取频数最多的的名字
names(table(a)[max(order(table(a)))])

###################矩阵保存
write.table(mat, file="mymatrix.txt", row.names=FALSE, col.names=FALSE)

######################字符串排序
strSort <-function(x) sapply(lapply(strsplit(x,NULL),sort),paste,collapse="")

##################数据框根据某列去冗余
df_deduplicate<-df[!duplicated(df[,"col_id"]),]

#################两个数据框根据某列索引时,不能用%in%,会导致位置错误,需要用merge数据框
