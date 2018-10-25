#Load vegan to finish NMDS by metaMDS function.
#Load ggplot2 to Plot figure.
library(vegan)
library(ggplot2)

#Before to use this script, the first data.frame (named as chrtDF), should be N rows and M colums.
#The site or sample name in the first colum. And the character information, such as genus name in the smaple, should in the other colum.
chrtDF<-as.data.frame(chrtDF)
row.names(chrtDF)<-chrtDF[,1]
chrtDF<-chrtDF[,-1]

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
NMDS <- data.frame(MDS1 = nmdsRaw$points[,1], MDS2 = nmdsRaw$points[,2],group = MyMeta$treatMent)

# Get the ordiellipse like ordiellipse in vegan.
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

ord <- ordiellipse(nmdsRaw, MyMeta$treatMent,display = "sites",
                   kind = 'se', conf = 0.95, label = T)

df_ell <- data.frame()

for(g in levels(NMDS$group)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                                   veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
                                ,group=g))
}

# Plot the first tow NMDS axises as the Axises of coordination.
P <- ggplot(data = NMDS, aes(MDS1,MDS2))+theme_bw()
# Prepare the theme of plot.
P+theme(panel.grid = element_blank(),legend.title=element_blank())
# Plot the points, draw the legend, and draw the stress.
# If you have more than 6 shapes, the scale_color_manual and scale_shape_manual is the workaround.

P+geom_point(aes(data=MyMeta,shape = MyMeta$treatMent, color = MyMeta$treatMent)+
               scale_color_manual(values = 1:nlevels(MyMeta$treatMent))+
               scale_shape_manual(values = 1:nlevels(MyMeta$treatMent))+
               labs(shape="Habitat",color = "Habitat")+
               annotate("text",x=min(NMDS$MDS1),y=min(NMDS$MDS2),hjust=0,vjust=0,label=paste("Stress:",nmdsRaw$stress))+
               geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2,colour=group), size=1, linetype=2, show.legend = FALSE)
