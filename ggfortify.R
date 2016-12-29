#PCA plot for iris data
library(ggfortify)
df<-iris[c(1, 2, 3, 4)]
autoplot(prcomp(df), data = iris, colour = 'Species', label = FALSE,label.size = 3,frame.type = 'norm')
