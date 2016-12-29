#超几何检验
phyper(x = '自己关注的某类基因中最终富集到某类go term的基因数目-1', m = '具有该类go term的原基因总数',n="不具有某类go term的原基因数目" ,k="关注的一类基因的数目(比如差异基因数目等样本数)",lower.tail = FALSE)
phyper(x = 20, m = 50,n = 50,k =25) #m+n=总体

#fisher.test检验
fisher.test(data.frame(c(m-x,n-k+x),c(x,k-x)))

