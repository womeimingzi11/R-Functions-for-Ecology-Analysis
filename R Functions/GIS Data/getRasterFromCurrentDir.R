getRasterData<-function(workdir,coord,methd = "bilinear"){
  
  require("raster")
  
  # workdir is the Dir of raster files that you want to extract value.
  # file.type can be any kind of raster file extensions which the raster package support.
  # coord is the Longtitude and Latitude, which can be array and data.frame.
  # methd is the Methods Parameter of Extract Function, it support "simple" and "bilinear", for more information, just type ?extract
  
  setwd(workdir)
  # Combine raster files as RASTER LAYER STACK in CURRENT DIR.
  # datafiles <- Sys.glob(file.type)
  datafiles <- Sys.glob("*.tif")#Or whatever identifies your files
  resultingStack <- stack()
  for(i in 1:NROW(datafiles)){
    tempraster <- raster(datafiles[i])
    resultingStack <- stack(resultingStack,tempraster)
  }
  
  # Extract values from RASTER LAYER STACK by COORD.
  resultDF<-data.frame(extract(resultingStack,coord,methods = methd))
  colnames(resultDF) <- c("mat","MeanDiurnalRange","Isothermality","Temperature.Seasonality",
                          "MaxTemperatureofWarmestMonth","MinTemperatureofColdestMonth","TemperatureAnnualRange",
                          "MeanTemperatureOfWettestQuarter","MeanTemperatureOfDriestQuarter",
                          "MeanTemperatureOfWarmestQuarter","MeanTemperatureOfColdestQuarter",
                          "map","PrecipitationOfWettestMonth","PrecipitationOfDriestMonth",
                          "PrecipitationSeasonality","PrecipitationOfWettestQuarter","PrecipitationOfDriestQuarter",
                          "PrecipitationOfWarmestQuarter","PrecipitationOfColdestQuarter"
                          )
  resultDF
}