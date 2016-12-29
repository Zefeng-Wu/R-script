d_dist <- dist(cor(log_norm))
hc<-hclust(d_dist)
plot(hc)
rect.hclust(hc,h=0.6,border=2)
length(rect.hclust(hc,h=0.6,border=2))#223