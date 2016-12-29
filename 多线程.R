fun <- function(x){
  return (x+1)
}

########### parallel (prefer)#########
library(parallel)
cl<-makeCluster(detectCores()-1)
system.time(results<-parLapply(cl,1:5000000,fun))
res<-do.call('rbind',results)
stopCluster(cl)


######### foreach #############(semms slow)
library(foreach)
library(doParallel)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
system(res <- foreach(x=1:5000000,.combine='rbind') %dopar% fun(x))
stopCluster(cl)