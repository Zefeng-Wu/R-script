library(ggplot2)
library(e1071)
library(reshape)
data<-read.csv("test.txt",sep="\t",header = TRUE)
attach(data)
ggplot(data,aes(x=X,y=Y,colour=factor(Class)))+geom_point()

#################################-SVM_classifier
svm.fit<-svm(Class~X+Y,data=data)
svm.predictions<-ifelse(predict(svm.fit)>0,1,0)
mean(with(data,svm.predictions==Class))

linear.svm.fit<-svm(Class~X+Y,data=data,kernel = 'linear')
linear.svm.predictions<-ifelse(predict(linear.svm.fit)>0,1,0)
mean(with(data,linear.svm.predictions==Class))

radial.svm.fit<-svm(Class~X+Y,data=data,kernel = 'radial',gamma = 0.6) #good
radial.svm.predictions<-ifelse(predict(radial.svm.fit)>0,1,0)
mean(with(data,radial.svm.predictions==Class))

polynomial.svm.fit<-svm(Class~X+Y,data=data,kernel = 'polynomial')#hyper_paremeter:degree=10
polynomial.svm.predictions<-ifelse(predict(polynomial.svm.fit)>0,1,0)
mean(with(data,polynomial.svm.predictions==Class))

sigmoid.svm.fit<-svm(Class~X+Y,data=data,kernel = 'sigmoid',gamma = 5,coef0 =-10 ,cost = 100)
sigmoid.svm.predictions<-ifelse(predict(sigmoid.svm.fit)>0,1,0)
mean(with(data,sigmoid.svm.predictions==Class))


df<-cbind(data,data.frame(SVM=ifelse(predict(sigmoid.svm.fit)>0,1,0)))
predictions<-melt(df,id.vars=c("X","Y"))
ggplot(predictions,aes(x=X,y=Y,color=factor(value)))+geom_point()
#cost and gamma hyper-parameters can used any kernals in SVM. 

#################################-logistic
logit.fit<-glm(Class~X+Y,data=data,family=binomial(link = 'logit'))
logit.predictions<-ifelse(predict(logit.fit)>0,1,0)
mean(with(data,logit.predictions==Class))
#######################???########navie bayes
NABayes.fit <- naiveBayes(Class ~ ., data = data)
NABayes.predictions<-ifelse(predict(NABayes.fit,as.data.frame(data))>0,1,0)
mean(data,NABayes.predictions==Class)



###nnet
library(nnet)
data("iris")
set.seed(2)
ind = sample(2,nrow(iris),replace = TRUE,prob = c(0.7,0.3))
trainset = iris[ind == 1,]
testset = iris[ind == 2,]
iris.nn = nnet(Species ~ .,data = trainset,size = 2,rang = 0.1,decay = 5e-4,maxit = 200)
summary(iris.nn)
library(caret)
iris.predict = predict(iris.nn,testset,type = "class")
nn.table = table(testset$Species,iris.predict)
confusionMatrix(nn.table)



##auc
### too slow
require(pROC)
modelroc <- roc(train_data[,target_variable],NB_Predictions)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
grid.col=c("green", "red"), max.auc.polygon=TRUE,
auc.polygon.col="skyblue", print.thres=TRUE)


