#Load Libraries READXL/XLSX for READ/WRITE XLSX FILES
library(readxl)
library(xlsx)

#Read Multi-Sheets Workbook Function
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-  lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

#Exctract GeoInfo and Comunities Info
creatTable<-function(mysheets){
  
  #Create Empty Data.Frame for fill geo-info
  data <- data.frame(
    ele=numeric(),
    lati= numeric(),
    long = numeric(),
    coverage = numeric(),
    stringsAsFactors=TRUE
  )
  #Create Empty Vector for fill spRichness and Abundance
  spVec<-vector()
  abdVec<-vector()
  #Extract geo-info
  for (i in 1:length(mysheets)) {
    t<-mysheets[i]
    t<- as.data.frame(t)
    r<-t[c(4,5,6,7),2]
    data[i,]<-r
  }
  #Extract spRichness
  for (j in 1:length(mysheets)){
    t<-mysheets[j]
    t<- as.data.frame(t)
    spMx<-t[,2]
    spMx<-spMx[-c(1:12)]
    spMx<-na.omit(spMx)
    spVec[j]<-length(spMx)-1
  }
  data$spRich<-spVec
  
  #Extract Abundance
  for (k in 1:length(mysheets)){
    t<-mysheets[k]
    t<- as.data.frame(t)
    abdMX<-t[,3]
    abdMX<-abdMX[-c(1:12)]
    abdMX<-as.numeric(na.omit(abdMX)[-1])
    abdVec[k]<-sum(abdMX)
  }
  data$abd<-abdVec
  #Use the site as ROWNAMES
  rownames(data)<-names(mysheets)
  return(data)
}

#Load XLSX File in Current Directory
fList<-dir()

#Remove other kind of files
fList<-fList[-c(1:2)]

#Create Empty Result Data.Frame
result <- data.frame(
  ele=numeric(),
  lati= numeric(),
  long = numeric(),
  coverage = numeric(),
  spRich = numeric(),
  abd = numeric(),
  stringsAsFactors=TRUE
)

# For Loop to generate result file
for (a in 1:length(fList)) {
  inputMX<-read_excel_allsheets(fList[a])
  output<-creatTable(inputMX)
  result<-rbind(result,output)
}

# Order result data.frame by Sites name
result$site<-row.names(result)
test<-result[order(result$site),c(1:length(colnames(result)))]

#Output ordered RESULT data.frame (by xlsx library write.xlsx function)
write.xlsx(test,'/Users/chenhan/Desktop/abvVeg.xlsx')