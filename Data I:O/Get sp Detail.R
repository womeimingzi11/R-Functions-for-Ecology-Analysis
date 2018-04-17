loop <- length(allSheets)
for (i in 1:loop) {
  a<-allSheets[i]
  fname<-names(a)
  a<-as.data.frame(a)
  a<-a[-c(1:12),]
  tof <- is.na(a[,1])
  for (j in 1:length(tof)) {
    if(!tof[j]){
      break
    }
  }
  if (j > 1) {
    a<-a[-c(1:j),]
  }
  a<-a[,1:3]
  cname<-c("1","2",fname)
  colnames(a)<-cname
  filename <- paste(fname, ".csv", sep="")
  write.csv(t(a)[-1,],file = filename,col.names = FALSE)
  print(paste(i,"has done"))
}
