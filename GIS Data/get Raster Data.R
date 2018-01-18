# Which wd is the workdirectory that the Rster Files saved.
# rsData is the 

extrack_raster_method<-function(wd,coord){
  rsData<-data.frame(c(1:length(coord[,1])))
  setwd(wd)
  fList<-t(dir())
  rasterStack<-apply(fList, 2, raster)
  for (i in 1:length(rasterStack)) {
    #if (inter == 'simple'){
      #data<-extract(rasterStack[[i]],coord,methods= 'bilinear' )
    #} else{
      data<-extract(rasterStack[[i]],coord,methods= 'bilinear')
    #  }
    rsData<-cbind(rsData,data)
  }
  colnames(rsData)[1]<-'Serial'
  colnames(rsData)[2:length(rsData)]<-dir()
  return(rsData)
}






