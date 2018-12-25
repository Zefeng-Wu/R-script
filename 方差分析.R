x<-c(0.124285714,0.148571429, 0.128571429, 0.137142857, 0.1075, 0.08625,0.11,0.1, 0.14,0.1075,0.1225,0.16,0.196666667,0.1916,0.193333333,0.21,0.224285714,0.19,0.185714286,0.191428571)
b<-data.frame(x,a=factor(c(rep("sp11648",4),rep("1391",4),rep("104",4),rep("s32620",4),rep("s20",4))))
library(multcomp)
m1<-aov(x~a,data=b)
summary(m1)

#方差分析只告诉我们这五组之间是不同的，但没有告诉我们哪两组之间有明显差别，此时需要使用TukeyHSD函数进行均值的多重比较分析，从结果中观察到有三个两两比较是不显著的。
TukeyHSD(m1)
par(lass=2)
par(mar=c(5,8,4,2))
plot(TukeyHSD(m1),yaxt="n", ylab="")

##condition test

library(car)
qqPlot(lm(x~a,data=b),simulate=TRUE)

### fangchaqi
bartlett.test(x~a,data=b)