pls.regression(Xtrain, Ytrain, Xtest=NULL, ncomp=NULL,  unit.weights=TRUE)

# Xtrain: gene*TFs (Ecoli数据:基因-转录因子矩阵):Each row corresponds to an observation(基因) and each column to a predictor variable(转录因子).:连接矩阵

# Ytrain: gene *sample 数据 (Ecoli数据:基因-样本数据):Each row corresponds to observation(基因) and each column to a response variable(样本表达量)



#返回值
是个array: 
# B:转录因子* 样本(矩阵):预测变量*响应变量
#第三个量对应PLS成分数目

# Ypred:根据Xtest预测值:


pls.regression.cv(Xtrain, Ytrain, ncomp, nruncv=20, alpha=2/3) #交叉验证 
