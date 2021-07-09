cyclones <- read.csv('C:/Users/usuario/Desktop/Master/tfm/Notebooks_TFM/SP_cyclones_time_series_1979_2020.csv')
cyclones$Date <- as.Date(cyclones$Date)
cyclones <- cyclones[,2:3]

wt <- read.csv("C:/Users/usuario/Desktop/TRMM-Calibration/Data/trends_data.csv")

wt1 <- wt[which(wt$WT == 1),]

cyc.wt1 <- cyclones[which(wt$WT == 1),]

counts.wt1 <- c()
for (i in unique(wt1$Year)) {
  counts.wt1 <- c(counts.wt1, sum(cyc.wt1[which(wt1$Year == i),]$cyclones_generation_centers))
}

barplot(counts.wt1/sum(counts.wt1), names.arg = as.character(unique(wt1$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT1")

wt2 <- wt[which(wt$WT == 2),]

cyc.wt2 <- cyclones[which(wt$WT == 2),]

counts.wt2 <- c()
for (i in unique(wt2$Year)) {
  counts.wt2 <- c(counts.wt2, sum(cyc.wt2[which(wt2$Year == i),]$cyclones_generation_centers))
}

barplot(counts.wt2/sum(counts.wt2), names.arg = as.character(unique(wt2$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT2")

wt3 <- wt[which(wt$WT == 3),]

cyc.wt3 <- cyclones[which(wt$WT == 3),]

counts.wt3 <- c()
for (i in unique(wt3$Year)) {
  counts.wt3 <- c(counts.wt3, sum(cyc.wt3[which(wt3$Year == i),]$cyclones_generation_centers))
}

barplot(counts.wt3/sum(counts.wt3), names.arg = as.character(unique(wt3$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT3")

wt4 <- wt[which(wt$WT == 4),]

cyc.wt4 <- cyclones[which(wt$WT == 4),]

counts.wt4 <- c()
for (i in unique(wt4$Year)) {
  counts.wt4 <- c(counts.wt4, sum(cyc.wt4[which(wt4$Year == i),]$cyclones_generation_centers))
}

barplot(counts.wt4/sum(counts.wt4), names.arg = as.character(unique(wt4$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT4")

wt5 <- wt[which(wt$WT == 5),]

cyc.wt5 <- cyclones[which(wt$WT == 5),]

counts.wt5 <- c()
for (i in unique(wt5$Year)) {
  counts.wt5 <- c(counts.wt5, sum(cyc.wt5[which(wt5$Year == i),]$cyclones_generation_centers))
}

barplot(counts.wt5/sum(counts.wt5), names.arg = as.character(unique(wt5$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT5")

par(mfrow=c(2,3))
barplot(counts.wt1/sum(counts.wt1), names.arg = as.character(unique(wt1$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT1", ylim = c(0,0.25))
barplot(counts.wt2/sum(counts.wt2), names.arg = as.character(unique(wt2$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT2", ylim = c(0,0.25))
barplot(counts.wt3/sum(counts.wt3), names.arg = as.character(unique(wt3$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT3", ylim = c(0,0.25))
barplot(counts.wt4/sum(counts.wt4), names.arg = as.character(unique(wt4$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT4", ylim = c(0,0.25))
barplot(counts.wt5/sum(counts.wt5), names.arg = as.character(unique(wt5$Year)), col = 'blue',
        ylab = "Cyclones gen centers percentage", main = "WT5", ylim = c(0,0.25))

