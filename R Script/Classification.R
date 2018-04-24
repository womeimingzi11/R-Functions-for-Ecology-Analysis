#par(mfrow = c(2,3))

library(vegan)
data(dune)

dis <- vegdist(dune)
# 最近邻体法（nearest neighbor sorting，单一连接法 Single Linkage Agglomerative Clustering）
near <- hclust(dis,method = "single")

# Further neighbor sorting
fur <- hclust(dis,method = "complete")
md <- hclust(dis,method = "median")
cent <- hclust(dis,method = "centroid")
avg<- hclust(dis,method = "average")

plot(near)
plot(fur)
plot(md)
plot(cent)
plot(avg,hang = 1)

rect.hclust(avg, k = 3, border = "red")