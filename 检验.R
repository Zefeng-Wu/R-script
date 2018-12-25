#超几何检验
phyper(x = '自己关注的某类基因中最终富集到某类go term的基因数目-1', m = '具有该类go term的原基因总数',n="不具有某类go term的原基因数目" ,k="关注的一类基因的数目(比如差异基因数目等样本数)",lower.tail = FALSE)
phyper(x = 20, m = 50,n = 50,k =25) #m+n=总体


#检验维恩图的交集部分
假设有两组ChIP-seq的数据A和B，我想知道它们之间是否相关。A组一共有50个数据，B组一共有60个数据，它们之间重叠的数据有30个，总计的binding位点如果是200个，那么如果它们相关的p-value如何计算呢？这个问题其实就是转变成抽到重叠数比30或者比30大的可能性是多大。所以我们需要计算抽到重叠数为29的可能性，然后用1减去它就可以了。
phyper(29, 60, 200-60, 50, lower.tail=FALSE)

#fisher.test检验
fisher.test(data.frame(c(m-x,n-k+x),c(x,k-x)),alternative = 'less')

