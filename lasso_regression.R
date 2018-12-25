library(glmnet)

grid =10^seq(10, -2, length=100)
ridge.mod = glmnet (x,y,alpha=0, lambda = grid,standardize=FALSE )  ## alpha=0 领回归; alpha = 1, 套锁回归
coef(ridge.mod)
predict(ride.mod, s=50, type="coefficients") #获得领回归系数:领回归不能选择特征


## training and test
set.seed (1)
train = sample(1:nrow(x),nrow(x)/2) #抽一半训练集
test = (-train) #一半测试集
y.test =y[test]

ridge.mod = glmnet(x[train,], y[train],alpha =0,lambda = grid, thresh =1e-12)
ridge.pred = predict(ridge.mod, s=4, newx = x[test,])
mean((ridge.pred-y.test)^2)

### cross validaton to choose tunining paramaters lamda λ

cv.out = cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

out = glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

####
#### 套锁回归 (可能比领回归更好)
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda = grid)
plot(lasso.mod)

##交叉验证
set.seed (1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot (cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict (lasso.mod, s = bestlam, newx=x [test,])
mean((lasso.pred-y.test)^2)

##减少了变量


###弹性回归,就是考虑alpha的最佳取值,0就是领回归,趋近1就是套锁回归
for (m in seq(0,1,0.01))
{
	cv.out = cv.glmnet(x[train,],y[train],alpha=m)
	bestlam = cv.out$lambda.min
	lasso.pred = predict (lasso.mod, s = bestlam, newx=x [test,])
	mean((lasso.pred-y.test)^2)
}







, 


