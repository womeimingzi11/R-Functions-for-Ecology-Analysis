library(vegan)
library(ggplot2)

colnames(rawDF)[1]<-"sites"
chrtRowName <- chrtDF$sites
chrtDF <- chrtDF[,-1]
rownames(chrtDF) <- chrtRowName

chrtDF <- as.data.frame(chrtDF)

metaDF <- as.data.frame(metaDF)

sites <- as.vector(metaDF[,1])
treatMent <- as.vector(metaDF[,2])
nmdsRaw <- metaMDS(chrtDF)

MyMeta <- data.frame(sites,treatMent,row.names = "sites")
NMDS <- data.frame(MDS1 = nmdsRaw$points[,1], MDS2 = nmdsRaw$points[,2])

P <- ggplot(data = NMDS, aes(MDS1,MDS2))+theme_bw()
P+theme(panel.grid = element_blank(),legend.title=element_blank())
P+geom_point(aes(data = MyMeta,shpe = MyMeta$treatMent, color = MyMeta$treatMent))
  +labs(shape="Habitat",colour = "Habitat")
  +annotate("text",x=min(NMDS$MDS1),y=min(NMDS$MDS2),hjust=0,vjust=0,label=paste("Stress:",nmdsRaw$stress))
