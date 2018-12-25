# predict imprinted gene by using glm
train_set<-read.table("/home/wuzefeng/MyResearch/motif_dbs/5jaspar2018/1fimo_out/Tair/2trining_set/imp_vs_unimp_motif.txt",stringsAsFactors = FALSE)
train_set$class<-ifelse(train_set$class=="Nonimp",0,1)
train_set$class<-factor(train_set$class,levels = c(0,1),labels=c("No","Yes"))

table(train_set$class)

fit.full <- glm(class ~ ., data = train_set ,family = binomial())