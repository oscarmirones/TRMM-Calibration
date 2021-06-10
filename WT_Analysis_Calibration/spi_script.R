library(loadeR)
library(transformeR)
library(visualizeR)

load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/slp.RData")

computeSPI <- function(slp, time.scale = "daily", season = "complete", export.csv = FALSE){
  if (time.scale %in% c("daily", "monthly", "annual")){
    if(season %in% c("complete", "wet")){
      if(season == "wet"){
        
        s <- c(11,12,1,2,3,4)
        slp <- subsetGrid(slp, season = s)
        
      }
      if(time.scale == "daily"){
        
        apia.slp <- subsetGrid(slp, lonLim = slp$xyCoords$x[which.min(abs(slp$xyCoords$x-(-171.7666636)))], latLim = slp$xyCoords$y[which.min(abs(slp$xyCoords$y-(-13.83333)))])
        
        suva.slp <- subsetGrid(slp, lonLim = slp$xyCoords$x[which.min(abs(slp$xyCoords$x-(178.44149)))], latLim = slp$xyCoords$y[which.min(abs(slp$xyCoords$y-(-18.1416)))])
        
        spi <- scale(suva.slp$Data - apia.slp$Data)
        
        df <- data.frame(apia.slp$Dates$start, spi)
        name <- "daily_spi.csv"
        
      }else if(time.scale == "monthly"){
        
        slp <- aggregateGrid(slp , aggr.m = list(FUN ="mean"))
        
        apia.slp <- subsetGrid(slp, lonLim = slp$xyCoords$x[which.min(abs(slp$xyCoords$x-(-171.7666636)))], latLim = slp$xyCoords$y[which.min(abs(slp$xyCoords$y-(-13.83333)))])
        
        suva.slp <- subsetGrid(slp, lonLim = slp$xyCoords$x[which.min(abs(slp$xyCoords$x-(178.44149)))], latLim = slp$xyCoords$y[which.min(abs(slp$xyCoords$y-(-18.1416)))])
        
        spi <- scale(suva.slp$Data - apia.slp$Data)
        
        df <- data.frame(apia.slp$Dates$start, spi)
        
        name <- "monthly_spi.csv"
        
      }else{
        
        slp <- aggregateGrid(slp , aggr.m = list(FUN ="mean"), aggr.y = list(FUN = "mean"))
        
        apia.slp <- subsetGrid(slp, lonLim = slp$xyCoords$x[which.min(abs(slp$xyCoords$x-(-171.7666636)))], latLim = slp$xyCoords$y[which.min(abs(slp$xyCoords$y-(-13.83333)))])
        
        suva.slp <- subsetGrid(slp, lonLim = slp$xyCoords$x[which.min(abs(slp$xyCoords$x-(178.44149)))], latLim = slp$xyCoords$y[which.min(abs(slp$xyCoords$y-(-18.1416)))])
        
        spi <- scale(suva.slp$Data - apia.slp$Data)
        
        df <- data.frame(apia.slp$Dates$start, spi)
        name <- "annual_spi.csv"
        
      }
      
      if(export.csv == TRUE){
        write.csv(df, name)
      }
      
      return(df)
    }else{
      stop("Season selected is not correct.")
    }
    
  }else{
      stop("The time scale is not valid!")
  }
  
  
}


data <- computeSPI(slp, export.csv = F)

plot(data$spi, type = 'l', main = "Daily SPI")
abline(h = 0)

data <- computeSPI(slp, time.scale = "monthly", export.csv = F)

plot(data$spi, type = 'l', main = "Monthly SPI")
abline(h = 0)

data <- computeSPI(slp,time.scale = "annual" ,export.csv = F)

plot(data$spi, type = 'l', main = "Annual SPI")
abline(h = 0)

#1979-97

load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/slp1979.1997.RData")

data <- computeSPI(slp1979.1997, export.csv = F)

plot(data$spi, type = 'l', main = "Daily SPI")
abline(h = 0)

data <- computeSPI(slp1979.1997, time.scale = "monthly", export.csv = F)

plot(data$spi, type = 'l', main = "Monthly SPI")
abline(h = 0)

data <- computeSPI(slp1979.1997,time.scale = "annual" ,export.csv = F)

plot(data$spi, type = 'l', main = "Annual SPI")
abline(h = 0)


#Using 1st EOF anomalies

pca1 <- prinComp(slp, n.eofs = 1)
plotEOF(pca1, var = "slp", n.eofs = 1, backdrop.theme = "coastline",
        main = "Leading EOF of MSLP anomaly")



data <- read.csv("C:/Users/usuario/Desktop/TRMM-Calibration/Data/spczi1911_2012.csv")

#########################################


load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/pp_reanalysis_complete.RData")

pp_reanalysis_1979_1997 <- subsetGrid(pp.complete, years = c(1979:1997))

negative.spi <- subsetGrid(pp_reanalysis_1979_1997, years = c(1979:1997)[which(data$SPCZI[69:87] < 0)])

positive.spi <- subsetGrid(pp_reanalysis_1979_1997, years = c(1979:1997)[which(data$SPCZI[69:87] > 0)])

#################################################

load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/kmModel5.RData")
load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/pp_reanalysis.RData")


cl4.slp <- subsetDimension(slp, dimension = "time", indices = kmModel5$cluster == 4)

cl4.slp.apia <- subsetGrid(cl4.slp, lonLim = cl4.slp$xyCoords$x[which.min(abs(cl4.slp$xyCoords$x-(-171.7666636)))], latLim = cl4.slp$xyCoords$y[which.min(abs(cl4.slp$xyCoords$y-(-13.83333)))])
cl4.slp.suva <- subsetGrid(cl4.slp, lonLim = cl4.slp$xyCoords$x[which.min(abs(cl4.slp$xyCoords$x-(178.44149)))], latLim = cl4.slp$xyCoords$y[which.min(abs(cl4.slp$xyCoords$y-(-18.1416)))])
cl4.pp <- subsetDimension(pp_reanalysis, dimension = "time", indices = kmModel5$cluster == 4)

cl4.spi <- scale(cl4.slp.suva$Data - cl4.slp.apia$Data)

plot(cl4.spi, type = 'l')