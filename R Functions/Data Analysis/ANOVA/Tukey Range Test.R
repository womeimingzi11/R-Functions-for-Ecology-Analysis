# The Y is Response Var, X is Indp Var, Y is the Name of Y letter, X is so, 
#the rev is to control what's the order of the mark. Which in the function:
#if (rev) {
#Tukey.labels<-data.frame(multcompLetters(Tukey.levels,rev = TRUE)['Letters'])
#} else {Tukey.labels<-data.frame(multcompLetters(Tukey.levels)['Letters'])}

library(multcompView)
library(plotrix)

TukeyTestFigure<- function(Y,X,Yletter,Xletter,rev = FALSE){
# Function to calculate the p value of F-test  (with Library(multcompView))
  lmp<-function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  
# Function to get the a-b-c letter by TUKEY test
  generate_label_df<-function(TUKEY,variable){
    # Extract labels and factor levels from Tukey post-hoc
    Tukey.levels<-TUKEY[[variable]][,4]
    
    if (rev) {
      Tukey.labels<-data.frame(multcompLetters(Tukey.levels,rev = TRUE)['Letters'])
    } else {Tukey.labels<-data.frame(multcompLetters(Tukey.levels)['Letters'])}
    #I need to put the labels in the same order as in the boxplot :
    Tukey.labels$treatment=rownames(Tukey.labels)
    Tukey.labels=Tukey.labels[order(Tukey.labels$treatment),]
    return(Tukey.labels)
  }
  X<-as.factor(X)
  
  # Dive the X factor into group
  #X <- ordered(X,levels = Group)
  #X<-Group
  
  # Make the ANOVA
  model<-lm(Y~X)
  ANOVA<- aov(model)
  
  # Make the Tukey test  
  TUKEY<-TukeyHSD(x=ANOVA,'X',conf.level = 0.95)
  
  # get the Letter Label
  
  LABELS=generate_label_df(TUKEY,'X')
  my_colors=c( rgb(143,199,74,maxColorValue = 255),rgb(242,104,34,maxColorValue = 255), rgb(111,145,202,maxColorValue = 255),rgb(254,188,18,maxColorValue = 255) , rgb(74,132,54,maxColorValue = 255))
  
  # plot the Boxplot
  
  a=boxplot(Y~X,ylim=c(min(Y),1.3*max(Y)),col=my_colors[as.numeric(LABELS[,1])],ylab=Yletter,xlab=Xletter)
  
  
  over=0.1*max(a$stats[nrow(a$stats),])
  text(c(1:nlevels(X)),a$stats[nrow(a$stats),]+over,LABELS[,1] ,col=my_colors[as.numeric(LABELS[,1])])
  
  # Draw the F and P value  (with Library(plotrix))
  
  pV<-round(lmp(model),5)
  
  if (pV < 0.01) {
    corner.label(label =paste("F =",round(summary(model)$fstatistic[1],3),"P < 0.01"),x=1,y=1)
    #pvalue<-c("P < 0.01")
  } else {
    corner.label(label =paste("F =",round(summary(model)$fstatistic[1],3),"P =",round(pV,3)),x=1,y=1)
  }
#  pv<-if(pv < 0.01
 # pV<-ifelse(pV < 0.01,c("P < 0.01"),c("P =",pV))
  
  #b<-c("P < 0.01")
  #c<-c("P =",pV)
  #d<-ifelse(pV < 0.01,b,c)
  #corner.label(label =paste("F =",round(summary(model)$fstatistic[1],3),pva),x=1,y=1)
}
