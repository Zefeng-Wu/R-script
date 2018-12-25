fun <- function(x){
  return (x+1)
}

########### parallel (prefer)#########
library(parallel)
cl<-makeCluster(detectCores()-1)
system.time(results<-parLapply(cl,1:5000000,fun))
res<-do.call('rbind',results)
stopCluster(cl)


###对数据框操作

library(parallel)
cl<-makeCluster(detectCores()-1)

clusterExport(cl,"custom.function")
aa <- parRapply(cl,dataframe, function(x) custom.function(x[1],x[2],x[3]))


######### foreach #############(semms slow)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores()-1,type = "FORK")
registerDoParallel(cl)
system(res <- foreach(x=1:5000000,.combine='rbind') %dopar% fun(x))
stopCluster(cl)


q<-mclapply(1:100000,function(x) data[x,1]+data[x,2],mc.cores = 40)





