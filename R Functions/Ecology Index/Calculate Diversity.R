# Load Vegan Package, the pacakge designed for ecologist
library(vegan)

# spdata is the data.frame that you want to calculate the Diversity Index
# you can set the row.name or any other information before hte diversity index as r_name
# But the row.number of the r_name must be as many as spdata.
# The defualt of r_name is nothing.
# We can also output the diversity index as csv file. But the default is False.
spDiversity_calc<-function(spdata,r_name  = spdata[,0], file_name = "Diversity Index.csv", write = FALSE) {
  # get the abundance
  
  abd<-apply(spdata, 1, sum)
  # get the species number
  species_number<-specnumber(spdata)
  # get shannon-weaver index
  shannon <- diversity(spdata)
  # get simpson index
  simpson <- diversity(spdata, index = "simpson")
  # get Pielou's evenness
  pielou_s_evenness <- shannon/log(species_number) #Pielou's evenness
  
  # Merge above index as on data.frame
  div <- data.frame(abd,species_number,shannon,simpson,pielou_s_evenness)
  
  #Add div data.frame into r_name, usually give them name back.
  div <- cbind(r_name,div)
  
  if (write == TRUE) {
    write.csv(div,file_name,row.names = FALSE)
  }
  
  return(div)
}