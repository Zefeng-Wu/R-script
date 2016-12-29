library(raster)
library(jpeg)
raster.photo <-raster("Rlogo.jpg")
photo.flip <-flip(raster.photo, direction = "y")## 将数据转换为矩阵形式
photo.raster <-t(as.matrix(photo.flip))
dim(photo.raster)
image(photo.raster, col = grey(seq(0, 1, length = 256))) ## 灰化处理

photo.svd <-svd(photo.raster)
d <-diag(photo.svd$d)
v <-as.matrix(photo.svd$v)
u <-photo.svd$u
取第一个奇异值进行估计,如下左图
u1 <-as.matrix(u[, 1])
d1 <-as.matrix(d[1, 1])
v1 <-as.matrix(v1[, 1])
photo1 <-u1 %*% d1 %*% t(v1)
image(photo1, col = grey(seq(0, 1, length = 256)))



u2 <-as.matrix(u[, 1:50])
d2 <-as.matrix(d[1:50, 1:50])
v2 <-as.matrix(v[, 1:50])
photo2 <-u2 %*% d2 %*% t(v2)
image(photo2, col = grey(seq(0, 1, length = 256)))
