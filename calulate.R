#计算data.frame任意两列,并做比较

for (m in 1:(length(data)-1)){
  for (n in (m+1):(length(data)-1)){
    p_value = wilcox.test(data[,m],data[,n],mu=0,paired = FALSE)[3]$p.value;
    if(!(is.na(p_value))&p_value<0.05)
      {print(c(p_value,m,n))}
    }
  }
#计算data.frame任意两行,并做比较
data<-read.csv("/home/wuzefeng/MyResearch/mouse/1prediction/MachineLearning/balanced.data",sep="\t",header=FALSE)
for (m in 2:37){
  for (n in 38:60){
    x = data[m,][1:(length(data)-1)];
    print (class(x))
    y = data[n,][1:(length(data)-1)];
    p_value = wilcox.test(x,y,mu=0,paired = FALSE)[3]$p.value
    if(!(is.na(p_value))&p_value<0.05){print(c(p_value,m,n))}
  }
}

#不同factor对应的数据集的均值检验
for (m in 2:4319){if(ks.test(subset(data[,m],data$class=="Imprinted"),subset(data[,m],data$class=="Non_imprinted"))[2]$p.value<=0.05){print(m)}}