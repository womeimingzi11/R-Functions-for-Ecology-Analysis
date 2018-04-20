library(vegan)

# The comDF is the data of communities.
decrn<-decorana(comDF)
#In theory, if the max Axis lenghth is larger than 4, choose CA or CCA
# if is fewer than 3, PCA or RDA. Else, you can choose CA|CCA or PCA|RDA
ca<-cca(comDF,scale = T)
pca <- rda(comDF,scale = T)