data<-abVeg[,1]
BC.Trans<-function(diver,data){
  lambda<-apply(diver, 2, BoxCox.lambda)
  for (i in 1:length(lambda)) {
    t<-BoxCox(diver[i],lambda[i])
    data<-cbind(data,t)
  }
  return(data)
}
out<-rownames(summary(influence.measures(lm.ab.abv)))
out<-as.numeric(out)
