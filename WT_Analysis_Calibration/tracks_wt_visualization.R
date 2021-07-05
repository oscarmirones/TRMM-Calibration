library(visualizeR)
library(scales)

data <- read.csv("C:/Users/usuario/Desktop/TRMM-Calibration/Data/tracks_1998_2020.csv")
load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/pp_reanalysis.RData")

data$Date <- as.Date(data$Date)

wt <- read.csv("C:/Users/usuario/Desktop/TRMM-Calibration/Data/trends_data.csv")
wt <- wt[wt$Year %in% c(1998:2020),]

data$longitude[data$longitude < 0] <- data$longitude[data$longitude < 0] + 360#cambio en las coords de longitud para poderlas representar en el spatialPlot
data <- data[!(data$longitude > 210),]
#si la longitud es negativa hay que sumarle 360 para que lo pinte en el spatialPlot
#hay que diferenciar tambien cuando en un día ocurren dos(o mas) ciclones diferentes
#esto lo podemos hacer con las diferencias del idx o con la variable storm_name


# custom.coords <- data[idx[9:14],4:3] # longitud +360
# storm <- map.lines(coords = custom.coords,
#                    lwd = 3,
#                    col = "black")
# storm2 <- map.lines(coords= data[1:8,4:3], lwd = 3, col = "black")
# spatialPlot(climatology(pp_reanalysis), backdrop.theme = "countries",
#             sp.layout = list(storm, storm2), # Add storm track
#             scales = list(draw = TRUE)) # Add coordinate axes
# 

#for wt1

dates.wt1 <- wt[wt$WT == 1,]$Date
# storms <- list()
# for (i in c(1:length(dates.wt1))) {
#   idx <- which(data$Date == dates.wt1[i])
#   if(length(idx) < 1) next 
#   change <- which(diff(idx)> 1)
#   sep <- list()
#   if (length(change) > 1) {
#     for (k in c(1:length(change))) {
#       if (k == 1) {
#         sep[[length(sep)+1]] <- idx[1:change[k]]
#       }else if (k == length(change)) {
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):length(idx)]
#       }else{
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):change[k]]
#       }
#       
#     }
#   }else if (length(change) == 0) {
#     sep <- list(idx)
#     
#   }else{
#     sep <- list(idx[1:change], idx[(change+1):length(idx)])
#   }
#   for (j in c(1:(length(sep)))) {
#     coordinates <- data[sep[[j]],4:3]
#     if(dim(coordinates)[1] <= 1) next #si solo hay un centro de generación da error en map.lines
#     storms[[length(storms)+1]] <- map.lines(coords = coordinates, lwd = 2, col = "black")
#   }
# }
# 
# spatialPlot(climatology(pp_reanalysis), backdrop.theme = "countries",
#             sp.layout = storms, scales = list(draw = TRUE))

#for wt2

dates.wt2 <- wt[wt$WT == 2,]$Date
# storms <- list()
# for (i in c(1:length(dates.wt2))) {
#   idx <- which(data$Date == dates.wt2[i])
#   if(length(idx) < 1) next 
#   change <- which(diff(idx)> 1)
#   sep <- list()
#   if (length(change) > 1) {
#     for (k in c(1:length(change))) {
#       if (k == 1) {
#         sep[[length(sep)+1]] <- idx[1:change[k]]
#       }else if (k == length(change)) {
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):length(idx)]
#       }else{
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):change[k]]
#       }
#       
#     }
#   }else if (length(change) == 0) {
#     sep <- list(idx)
#     
#   }else{
#     sep <- list(idx[1:change], idx[(change+1):length(idx)])
#   }
#   for (j in c(1:(length(sep)))) {
#     coordinates <- data[sep[[j]],4:3]
#     if(dim(coordinates)[1] <= 1) next #si solo hay un centro de generación da error en map.lines
#     storms[[length(storms)+1]] <- map.lines(coords = coordinates, lwd = 2, col = "black")
#   }
# }
# 
# spatialPlot(climatology(pp_reanalysis), backdrop.theme = "countries",
#             sp.layout = storms, scales = list(draw = TRUE))

#for wt3

dates.wt3 <- wt[wt$WT == 3,]$Date
# storms <- list()
# for (i in c(1:length(dates.wt3))) {
#   idx <- which(data$Date == dates.wt3[i])
#   if(length(idx) < 1) next 
#   change <- which(diff(idx)> 1)
#   sep <- list()
#   if (length(change) > 1) {
#     for (k in c(1:length(change))) {
#       if (k == 1) {
#         sep[[length(sep)+1]] <- idx[1:change[k]]
#       }else if (k == length(change)) {
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):length(idx)]
#       }else{
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):change[k]]
#       }
#       
#     }
#   }else if (length(change) == 0) {
#     sep <- list(idx)
#     
#   }else{
#     sep <- list(idx[1:change], idx[(change+1):length(idx)])
#   }
#   for (j in c(1:(length(sep)))) {
#     coordinates <- data[sep[[j]],4:3]
#     if(dim(coordinates)[1] <= 1) next #si solo hay un centro de generación da error en map.lines
#     storms[[length(storms)+1]] <- map.lines(coords = coordinates, lwd = 2, col = "black")
#   }
# }
# 
# spatialPlot(climatology(pp_reanalysis), backdrop.theme = "countries",
#             sp.layout = storms, scales = list(draw = TRUE))

#for wt4

dates.wt4 <- wt[wt$WT == 4,]$Date
# storms <- list()
# for (i in c(1:length(dates.wt4))) {
#   idx <- which(data$Date == dates.wt4[i])
#   if(length(idx) < 1) next 
#   change <- which(diff(idx)> 1)
#   sep <- list()
#   if (length(change) > 1) {
#     for (k in c(1:length(change))) {
#       if (k == 1) {
#         sep[[length(sep)+1]] <- idx[1:change[k]]
#       }else if (k == length(change)) {
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):length(idx)]
#       }else{
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):change[k]]
#       }
#       
#     }
#   }else if (length(change) == 0) {
#     sep <- list(idx)
#     
#   }else{
#     sep <- list(idx[1:change], idx[(change+1):length(idx)])
#   }
#   for (j in c(1:(length(sep)))) {
#     coordinates <- data[sep[[j]],4:3]
#     if(dim(coordinates)[1] <= 1) next #si solo hay un centro de generación da error en map.lines
#     storms[[length(storms)+1]] <- map.lines(coords = coordinates, lwd = 2, col = "black")
#   }
# }
# 
# spatialPlot(climatology(pp_reanalysis), backdrop.theme = "countries",
#             sp.layout = storms, scales = list(draw = TRUE))


#for wt5

dates.wt5 <- wt[wt$WT == 5,]$Date
# storms <- list()
# for (i in c(1:length(dates.wt5))) {
#   idx <- which(data$Date == dates.wt5[i])
#   if(length(idx) < 1) next 
#   change <- which(diff(idx)> 1)
#   sep <- list()
#   if (length(change) > 1) {
#     for (k in c(1:length(change))) {
#       if (k == 1) {
#         sep[[length(sep)+1]] <- idx[1:change[k]]
#       }else if (k == length(change)) {
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):length(idx)]
#       }else{
#         sep[[length(sep)+1]] <- idx[(change[k-1]+1):change[k]]
#       }
#       
#     }
#   }else if (length(change) == 0) {
#     sep <- list(idx)
#     
#   }else{
#     sep <- list(idx[1:change], idx[(change+1):length(idx)])
#   }
#   for (j in c(1:(length(sep)))) {
#     coordinates <- data[sep[[j]],4:3]
#     if(dim(coordinates)[1] <= 1) next #si solo hay un centro de generación da error en map.lines
#     storms[[length(storms)+1]] <- map.lines(coords = coordinates, lwd = 2, col = "black")
#   }
# }
# 
# spatialPlot(climatology(pp_reanalysis), backdrop.theme = "countries",
#             sp.layout = storms, scales = list(draw = TRUE))


#also conditioned to terciles
df <- read.csv("C:/Users/usuario/Desktop/TRMM-Calibration/Data/trends_data.csv")

df <- df[df$Year >= 1998,]

dates.t1 <- df[df$Tercile == "T1",]$Date
dates.t2 <- df[df$Tercile == "T2",]$Date
dates.t3 <- df[df$Tercile == "T3",]$Date

dates.wt1.t1 <- intersect(dates.t1, dates.wt1)
dates.wt1.t2 <- intersect(dates.t2, dates.wt1)
dates.wt1.t3 <- intersect(dates.t3, dates.wt1)

dates.wt2.t1 <- intersect(dates.t1, dates.wt2)
dates.wt2.t2 <- intersect(dates.t2, dates.wt2)
dates.wt2.t3 <- intersect(dates.t3, dates.wt2)

dates.wt3.t1 <- intersect(dates.t1, dates.wt3)
dates.wt3.t2 <- intersect(dates.t2, dates.wt3)
dates.wt3.t3 <- intersect(dates.t3, dates.wt3)

dates.wt4.t1 <- intersect(dates.t1, dates.wt4)
dates.wt4.t2 <- intersect(dates.t2, dates.wt4)
dates.wt4.t3 <- intersect(dates.t3, dates.wt4)

dates.wt5.t1 <- intersect(dates.t1, dates.wt5)
dates.wt5.t2 <- intersect(dates.t2, dates.wt5)
dates.wt5.t3 <- intersect(dates.t3, dates.wt5)


storm.gen <- function(dates, which.plot = NULL, alpha.value = .3){
  storms <- list()
  for (i in c(1:length(dates))) {
    idx <- which(data$Date == dates[i])
    if(length(idx) < 1) next 
    change <- which(diff(idx)> 1)
    sep <- list()
    if (length(change) > 1) {
      for (k in c(1:length(change))) {
        if (k == 1) {
          sep[[length(sep)+1]] <- idx[1:change[k]]
        }else if (k == length(change)) {
          sep[[length(sep)+1]] <- idx[(change[k-1]+1):length(idx)]
        }else{
          sep[[length(sep)+1]] <- idx[(change[k-1]+1):change[k]]
        }
        
      }
    }else if (length(change) == 0) {
      sep <- list(idx)
      
    }else{
      sep <- list(idx[1:change], idx[(change+1):length(idx)])
    }
    for (j in c(1:(length(sep)))) {
      coordinates <- data[sep[[j]],4:3]
      if(dim(coordinates)[1] <= 1) next #si solo hay un centro de generación da error en map.lines
     
      storms[[length(storms)+1]] <- map.lines(coords = coordinates, lwd = 2, col = alpha('grey',alpha.value),
                                              which = which.plot)
    }
  }
  return(storms)
}



storms.wt1.t1 <- storm.gen(dates.wt1.t1, which.plot = 1)
storms.wt1.t2 <- storm.gen(dates.wt1.t2, which.plot = 2)
storms.wt1.t3 <- storm.gen(dates.wt1.t3, which.plot = 3)


storms.wt1.t1[[84]] <- NULL
storms.wt1.t3[[4]] <- NULL


#wt2
storms.wt2.t1 <- storm.gen(dates.wt2.t1, which.plot = 4)
storms.wt2.t2 <- storm.gen(dates.wt2.t2, which.plot = 5)
storms.wt2.t3 <- storm.gen(dates.wt2.t3, which.plot = 6)



#wt3
storms.wt3.t1 <- storm.gen(dates.wt3.t1, which.plot = 7)
storms.wt3.t2 <- storm.gen(dates.wt3.t2, which.plot = 8)
storms.wt3.t3 <- storm.gen(dates.wt3.t3, which.plot = 9)

storms.wt3.t2[[13]] <- NULL


#wt4
storms.wt4.t1 <- storm.gen(dates.wt4.t1, which.plot = 10)
storms.wt4.t2 <- storm.gen(dates.wt4.t2, which.plot = 11)
storms.wt4.t3 <- storm.gen(dates.wt4.t3, which.plot = 12)

storms.wt4.t2[[6]] <- NULL
storms.wt4.t3[[6]] <- NULL


#wt5
storms.wt5.t1 <- storm.gen(dates.wt5.t1, which.plot = 13)
storms.wt5.t2 <- storm.gen(dates.wt5.t2, which.plot = 14)
storms.wt5.t3 <- storm.gen(dates.wt5.t3, which.plot = 15)



##WT1
wt1.t1 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                          indices = intersect(which(df$Tercile == "T1"), which(df$WT == 1))))
wt1.t2 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T2"), which(df$WT == 1))))
wt1.t3 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T3"), which(df$WT == 1))))




##WT2
wt2.t1 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T1"), which(df$WT == 2))))
wt2.t2 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                     indices = intersect(which(df$Tercile == "T2"), which(df$WT == 2))))
wt2.t3 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T3"), which(df$WT == 2))))




##WT3
wt3.t1 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T1"), which(df$WT == 3))))
wt3.t2 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T2"), which(df$WT == 3))))
wt3.t3 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T3"), which(df$WT == 3))))


##WT4
wt4.t1 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T1"), which(df$WT == 4))))
wt4.t2 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T2"), which(df$WT == 4))))
wt4.t3 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T3"), which(df$WT == 4))))




##WT5
wt5.t1 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T1"), which(df$WT == 5))))
wt5.t2 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T2"), which(df$WT == 5))))
wt5.t3 <- climatology(subsetDimension(pp_reanalysis, dimension = "time",
                                      indices = intersect(which(df$Tercile == "T3"), which(df$WT == 5))))







# mg1 <- makeMultiGrid(wt1.t1,wt1.t2, wt1.t3, skip.temporal.check = T)
# mg2 <- makeMultiGrid(wt2.t1,wt2.t2, wt2.t3, skip.temporal.check = T)
# mg3 <- makeMultiGrid(wt3.t1,wt3.t2, wt3.t3, skip.temporal.check = T)
# mg4 <- makeMultiGrid(wt4.t1,wt4.t2, wt4.t3, skip.temporal.check = T)
# mg5 <- makeMultiGrid(wt5.t1,wt5.t2, wt5.t3, skip.temporal.check = T)
# 
# spatialPlot(mg1, sp.layout = list(storms.wt1.t1, storms.wt1.t2, storms.wt1.t3), backdrop.theme = "countries",
#             rev.colors = F, color.theme = "BrBG",as.table = T, names.attr = c("T1","T2","T3"), main = "WT1", colorkey = list(space ="bottom",height=1, width=1), set.max = 20)
# 
# spatialPlot(mg2, sp.layout = list(storms.wt2.t1, storms.wt2.t2, storms.wt2.t3), backdrop.theme = "countries",
#             rev.colors = F, color.theme = "BrBG",as.table = T, names.attr = c("T1","T2","T3"), main = "WT2", colorkey = list(space ="bottom",height=1, width=1), set.max = 20)
# 
# spatialPlot(mg3, sp.layout = list(storms.wt3.t1, storms.wt3.t2, storms.wt3.t3), backdrop.theme = "countries",
#             rev.colors = F,color.theme = "BrBG", as.table = T, names.attr = c("T1","T2","T3"), main = "WT3", colorkey = list(space ="bottom",height=1, width=1), set.max = 20)
# 
# spatialPlot(mg4, sp.layout = list(storms.wt4.t1, storms.wt4.t2, storms.wt4.t3), backdrop.theme = "countries",
#             rev.colors = F,color.theme = "BrBG", as.table = T, names.attr = c("T1","T2","T3"), main = "WT4", colorkey = list(space ="bottom",height=1, width=1), set.max = 20)
# 
# spatialPlot(mg5, sp.layout = list(storms.wt5.t1, storms.wt5.t2, storms.wt5.t3), backdrop.theme = "countries",
#             rev.colors = F, color.theme = "BrBG",as.table = T, names.attr = c("T1","T2","T3"), main = "WT5", colorkey = list(space ="bottom",height=1, width=1), set.max = 20)

mg <- makeMultiGrid(wt1.t1,wt1.t2, wt1.t3,wt2.t1,wt2.t2, wt2.t3,wt3.t1,wt3.t2, wt3.t3,wt4.t1,wt4.t2, wt4.t3,wt5.t1,wt5.t2, wt5.t3,
                    skip.temporal.check = T)

storm.list <- list(storms.wt1.t1,storms.wt1.t2,storms.wt1.t3,storms.wt2.t1,storms.wt2.t2,storms.wt2.t3,
                   storms.wt3.t1,storms.wt3.t2,storms.wt3.t3,storms.wt4.t1,storms.wt4.t2,storms.wt4.t3,
                   storms.wt5.t1,storms.wt5.t2,storms.wt5.t3)

spatialPlot(mg, backdrop.theme = "countries",rev.colors = F, color.theme = "BrBG",as.table = T, colorkey = list(space = "bottom", height = 1),
            names.attr = c("WT1 T1","WT1 T2","WT1 T3","WT2 T1","WT2 T2","WT2 T3","WT3 T1","WT3 T2","WT3 T3","WT4 T1","WT4 T2","WT4 T3","WT5 T1","WT5 T2","WT5 T3"),
            set.max = 20, sp.layout = storm.list)