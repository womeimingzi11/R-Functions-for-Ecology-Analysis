library(googlesheets)
gSheetsList<-as.data.frame(gs_ls())
envRawDF<-gs_read(gs_key((gSheetsList$sheet_key[1])))

nameSerial <- envRawDF[,c(1,2)]

envDf<- envRawDF[,-c(1,2)]

library(vegan)
rda<-rda(comDF,envDf,scale = T)
cca<-cca(comDF,envDf,scale = T)


rda.16<-rda(comDF[(123:500),],envDf[(123:500),],scale = T)
cca.16<-cca(comDF[(123:500),],envDf[(123:500),],scale = T)

rda.15<-rda(comDF[(1:122),],envDf[(1:122),],scale = T)
cca.15<-cca(comDF[(1:122),],envDf[(1:122),],scale = T)


env.cca <- envfit(cca,envDf,permutations = 999)
env.rda <- envfit(rda,envDf,permutations = 999)

env.cca.15 <- envfit(cca.15,envDf[(1:122),],permutations = 999)
env.rda.15 <- envfit(rda.15,envDf[(1:122),],permutations = 999)

env.cca.16 <- envfit(cca.16,envDf[(123:500),],permutations = 999)
env.rda.16 <- envfit(rda.16,envDf[(123:500),],permutations = 999)

#save.image("~/R-Workspace/cca.RData")