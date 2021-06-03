library(transformeR)
library(visualizeR)


load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/pp_reanalysis.RData")
load("C:/Users/usuario/Desktop/TRMM-Calibration/Data/kmModel5.RData")

cl1 <- subsetDimension(pp_reanalysis, dimension = "time", indices = which(kmModel5$cluster == 1))
cl2 <- subsetDimension(pp_reanalysis, dimension = "time", indices = which(kmModel5$cluster == 2))
cl3 <- subsetDimension(pp_reanalysis, dimension = "time", indices = which(kmModel5$cluster == 3))
cl4 <- subsetDimension(pp_reanalysis, dimension = "time", indices = which(kmModel5$cluster == 4))
cl5 <- subsetDimension(pp_reanalysis, dimension = "time", indices = which(kmModel5$cluster == 5))

#WT1
lista <- list()
for (i in c(1:12)) {
  s <- subsetSeason(grid = cl1, season = i)
  season.sum <- c()
  for (j in c(1998:2020)) {
    if (j %in% unique(format(as.Date(s$Dates$start), format = "%Y"))) {
      s.year <- subsetYears(grid = s, years = j)
      season.sum <- c(season.sum, sum(apply(s.year$Data,MARGIN = 1, FUN = "mean"))) #pp de season i para el año j
    }else{
      season.sum <- c(season.sum, 0)
    }
  }
  lista[[length(lista)+1]] <- season.sum
}

wt1.season.acc <- c()
for (i in c(1:12)) {
  wt1.season.acc <- c(wt1.season.acc, mean(lista[[i]]))
}

names(wt1.season.acc) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#WT2
lista <- list()
for (i in c(1:12)) {
  s <- subsetSeason(grid = cl2, season = i)
  season.sum <- c()
  for (j in c(1998:2020)) {
    if (j %in% unique(format(as.Date(s$Dates$start), format = "%Y"))) {
      s.year <- subsetYears(grid = s, years = j)
      season.sum <- c(season.sum, sum(apply(s.year$Data,MARGIN = 1, FUN = "mean"))) #pp de season i para el año j
    }else{
      season.sum <- c(season.sum, 0)
    }
  }
  lista[[length(lista)+1]] <- season.sum
}

wt2.season.acc <- c()
for (i in c(1:12)) {
  wt2.season.acc <- c(wt2.season.acc, mean(lista[[i]]))
}

names(wt2.season.acc) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#WT3
lista <- list()
for (i in c(1:12)) {
  s <- subsetSeason(grid = cl3, season = i)
  season.sum <- c()
  for (j in c(1998:2020)) {
    if (j %in% unique(format(as.Date(s$Dates$start), format = "%Y"))) {
      s.year <- subsetYears(grid = s, years = j)
      season.sum <- c(season.sum, sum(apply(s.year$Data,MARGIN = 1, FUN = "mean"))) #pp de season i para el año j
    }else{
      season.sum <- c(season.sum, 0)
    }
  }
  lista[[length(lista)+1]] <- season.sum
}

wt3.season.acc <- c()
for (i in c(1:12)) {
  wt3.season.acc <- c(wt3.season.acc, mean(lista[[i]]))
}

names(wt3.season.acc) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#WT4
lista <- list()
for (i in c(1:12)) {
  s <- subsetSeason(grid = cl4, season = i)
  season.sum <- c()
  for (j in c(1998:2020)) {
    if (j %in% unique(format(as.Date(s$Dates$start), format = "%Y"))) {
      s.year <- subsetYears(grid = s, years = j)
      season.sum <- c(season.sum, sum(apply(s.year$Data,MARGIN = 1, FUN = "mean"))) #pp de season i para el año j
    }else{
      season.sum <- c(season.sum, 0)
    }
  }
  lista[[length(lista)+1]] <- season.sum
}

wt4.season.acc <- c()
for (i in c(1:12)) {
  wt4.season.acc <- c(wt4.season.acc, mean(lista[[i]]))
}

names(wt4.season.acc) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#WT5
lista <- list()
for (i in c(1:12)) {
  s <- subsetSeason(grid = cl5, season = i)
  season.sum <- c()
  for (j in c(1998:2020)) {
    if (j %in% unique(format(as.Date(s$Dates$start), format = "%Y"))) {
      s.year <- subsetYears(grid = s, years = j)
      season.sum <- c(season.sum, sum(apply(s.year$Data,MARGIN = 1, FUN = "mean"))) #pp de season i para el año j
    }else{
      season.sum <- c(season.sum, 0)
    }
  }
  lista[[length(lista)+1]] <- season.sum
}


wt5.season.acc <- c()
for (i in c(1:12)) {
  wt5.season.acc <- c(wt5.season.acc, mean(lista[[i]]))
}

names(wt5.season.acc) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

#plots

barplot(wt1.season.acc, names.arg = names(wt1.season.acc), col = 'blue', 
        main = " WT 1 Mean pp accumulated for each season", xlab = "Month", ylab = "Mean pp acc (mm)",
        ylim = c(0,wt4.season.acc[1]))

barplot(wt2.season.acc, names.arg = names(wt2.season.acc), col = 'blue', 
        main = " WT 2 Mean pp accumulated for each season", xlab = "Month", ylab = "Mean pp acc (mm)",ylim = c(0,wt4.season.acc[1]))

barplot(wt3.season.acc, names.arg = names(wt3.season.acc), col = 'blue', 
        main = " WT 3 Mean pp accumulated for each season", xlab = "Month", ylab = "Mean pp acc (mm)",ylim = c(0,wt4.season.acc[1]))

barplot(wt4.season.acc, names.arg = names(wt4.season.acc), col = 'blue', 
        main = " WT 4 Mean pp accumulated for each season", xlab = "Month", ylab = "Mean pp acc (mm)",ylim = c(0,wt4.season.acc[1]))

barplot(wt5.season.acc, names.arg = names(wt5.season.acc), col = 'blue', 
        main = " WT 5 Mean pp accumulated for each season", xlab = "Month", ylab = "Mean pp acc (mm)",ylim = c(0,wt4.season.acc[1]))
