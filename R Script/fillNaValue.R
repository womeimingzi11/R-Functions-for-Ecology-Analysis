library(googlesheets)
library(mice)
gs_ls()
df<-gs_read(gs_title("15/16 Soil Property"))
nameSerial<-df[,c(1,2)]
soilPro<-df[,-c(1,2)]

fillData <- function(rawdf) {
  miceMod <- mice(rawdf, method="rf")
  miceOutput <- complete(miceMod)
  if (anyNA(miceOutput)) {
    outputDF<-cbind(nameSerial,miceOutput)
  } else {
    print("There are some NA value in outputDF, maybe the mice function didn't work well")
  }
}

outputDF<-cbind(nameSerial,miceOutput)

fileName = "15&16 SoilProoerty filled by mice(rf_method).csv"

write.csv(outputDF,fileName)
gs_upload(fileName)
file.remove(fileName)
