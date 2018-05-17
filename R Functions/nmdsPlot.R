#Load vegan to finish NMDS by metaMDS function.
#Load ggplot2 to Plot figure.
library(vegan)
library(ggplot2)

#Before to use this script, the first data.frame (named as rawDF), should be N rows and M colums.
#The site or sample name in the first colum. And the character information, such as genus name in the smaple, should in the other colum.
colnames(rawDF)[1]<-"sites"
chrtRowName <- chrtDF$sites
chrtDF <- chrtDF[,-1]
rownames(chrtDF) <- chrtRowName
chrtDF <- as.data.frame(chrtDF)

# The second data.frame (named as metaDF), should be N rows, as many as rawDF, and 2 colums.
# The first colum is also as the first colum of rawDF, and the second one, should be filled with treatment or classication name, such as Control, Treatmen1, Treatment2
metaDF <- as.data.frame(metaDF)

# Generate new Meta Data.frame, to make sure it satisfied the next step. 
sites <- as.vector(metaDF[,1])
treatMent <- as.vector(metaDF[,2])
MyMeta <- data.frame(sites,treatMent,row.names = "sites")

# Start NMDS calculate
nmdsRaw <- metaMDS(chrtDF)

# Extract the first 2 axises as data.frame.
NMDS <- data.frame(MDS1 = nmdsRaw$points[,1], MDS2 = nmdsRaw$points[,2])

# Plot the first tow NMDS axises as the Axises of coordination.
P <- ggplot(data = NMDS, aes(MDS1,MDS2))+theme_bw()
# Prepare the theme of plot.
P+theme(panel.grid = element_blank(),legend.title=element_blank())
# Plot the points, draw the legend, and draw the stress.
P+geom_point(aes(data = MyMeta,shpe = MyMeta$treatMent, color = MyMeta$treatMent))+labs(shape="Habitat",colour = "Habitat")+annotate("text",x=min(NMDS$MDS1),y=min(NMDS$MDS2),hjust=0,vjust=0,label=paste("Stress:",nmdsRaw$stress))