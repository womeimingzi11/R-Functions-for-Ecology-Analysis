library(ggplot2)


PlotIncMSE<-function(data,group,MSE,pV,xlab){
  rf.imp <- as.data.frame(MSE)
  
  pV <- as.numeric(pV)
  if (pV < 0.05) {
    pV<-round(pV,digits = 2)
  }
  P <- ggplot(data,aes(x= group,y = MSE))
  P + geom_bar(stat = "identity", fill = "lightblue", colour = "black") + 
    geom_text(aes(label = round(MSE,digits = 2)),vjust = 1.5, colour = "black")+
    annotate("text",x=1,y=max(MSE), label=paste("R² =",pV," P < 0.01"))+
    labs(x = xlab,y = "Increase in MSE(%)")
}

abd.rfP.imp<-as.data.frame(abd.rf.noHBT$importance[,-2])*100
sp.rfP.imp<-as.data.frame(sp.rf.noHBT$importance[,-2])*100

abd.rfP.group <- as.factor(rownames(abd.rfP.imp))
sp.rfP.group <- as.factor(rownames(sp.rfP.imp))

abd.rfP.mse <- as.vector(abd.rfP.imp[,1])
sp.rfP.mse <- as.vector(sp.rfP.imp[,1])

abd.rfP.pV <- round(0.3199,digits = 2)
sp.rfP.pV <- round(0.3648,digits = 2)

library(ggplot2)

PlotIncMSE(abd.rfP.imp,abd.rfP.group,abd.rfP.mse,abd.rfP.pV,"Abundance")
PlotIncMSE(sp.rfP.imp,sp.rfP.group,sp.rfP.mse,sp.rfP.pV,"Species Number ")
