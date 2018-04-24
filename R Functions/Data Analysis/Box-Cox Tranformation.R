library(forecast)

# dataDF is the data.frame that you want to tranformation
boxcox_trans<-function(dataDF){
  # Initialize the output data.frame, the row-number same as dataDF.
  outputDF <- dataDF[,0]
  # Caculate lambda for Box-Cox transformation
  lambda<-apply(dataDF, 2, BoxCox.lambda)
  # Out-put the result of Box-Cox transofrmation
  
  #apply(dataDF, 2, )
  
  for (i in 1:length(lambda)) {
    t<-BoxCox(dataDF[i],lambda[i])
    outputDF<-cbind(outputDF,t)
  }
  return(outputDF)
}