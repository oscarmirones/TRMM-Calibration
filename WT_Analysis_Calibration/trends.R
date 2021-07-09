data <- read.csv("C:/Users/usuario/Desktop/TRMM-Calibration/Data/trends_data.csv")

wt1 <- data[data$WT == 1,]
wt1 <- wt1[-1,]

counts.wt1 <- c()
for (i in unique(wt1$Year)) {
  counts.wt1 <- c(counts.wt1,length(which(wt1$Year == i)))
}

wt2 <- data[data$WT == 2,]
wt2 <- wt2[-1,]

counts.wt2 <- c()
for (i in unique(wt2$Year)) {
  counts.wt2 <- c(counts.wt2,length(which(wt2$Year == i)))
}

wt3 <- data[data$WT == 3,]
wt3 <- wt3[-1,]

counts.wt3 <- c()
for (i in unique(wt3$Year)) {
  counts.wt3 <- c(counts.wt3,length(which(wt3$Year == i)))
}

wt4 <- data[data$WT == 4,]
wt4 <- wt4[-1,]

counts.wt4 <- c()
for (i in unique(wt4$Year)) {
  counts.wt4 <- c(counts.wt4,length(which(wt4$Year == i)))
}

wt5 <- data[data$WT == 5,]
wt5 <- wt5[-1,]

counts.wt5 <- c()
for (i in unique(wt5$Year)) {
  counts.wt5 <- c(counts.wt5,length(which(wt5$Year == i)))
}

df <- data.frame(unique(data$Year), counts.wt1, counts.wt2, counts.wt3, counts.wt4, counts.wt5)
colnames(df) <- c("Year", "WT1", "WT2", "WT3","WT4", "WT5")


library(ggplot2)

colors <- c("green","brown","red","blue","yellow")
names(colors) <- colnames(df[2:6])

ggplot(data = df, aes(x = Year)) + 
  geom_line(aes(x = Year, y = WT1, colour = "WT1")) + 
  geom_line(aes(y = WT2, colour = "WT2")) +
  geom_line(aes(y = WT3, colour = "WT3")) +
  geom_line(aes(y = WT4, colour = "WT4")) +
  geom_line(aes(y = WT5, colour = "WT5")) +
  ylab("WT days")+
  scale_color_manual(name = "WT",values =c(colors[1],colors[2],colors[3],colors[4],colors[5]))


cor.mat <- cor(df[,2:6])

library(lattice)
library(ts)

levelplot(cor.mat)

test <- matrix(NA, 5, 5)
for (i in c(2:6)) {
  for (j in c(2:6)) {
    test[i-1,j-1] <- cor.test(df[,i],df[,j])$p.value
  }
}

cor.mat[test > .05] <- NA
cor.mat

levelplot(cor.mat, col.regions = rev(hcl.colors(100,palette = 'GnBu')))


ggplot(data = df, aes(x = Year)) + 
 
  geom_line(aes(y = WT2, colour = "WT2")) +
 
  geom_line(aes(y = WT5, colour = "WT5")) +
  ylab("WT days")+
  scale_color_manual(name = "WT",values =c(colors[2],colors[5]))

####cyclones
cyclones <- read.csv('C:/Users/usuario/Desktop/Master/tfm/Notebooks_TFM/SP_cyclones_time_series_1979_2020.csv')

cyc.wt1 <- cyclones[data$WT == 1,]
cyc.wt1 <- cyc.wt1[-1,]

counts.cyc.wt1 <- c()
for (i in unique(wt1$Year)) {
  counts.cyc.wt1 <- c(counts.cyc.wt1,sum(cyc.wt1[which(wt1$Year == i),]$cyclones_generation_centers))
}


cyc.wt2 <- cyclones[data$WT == 2,]
cyc.wt2 <- cyc.wt2[-1,]

counts.cyc.wt2 <- c()
for (i in unique(wt2$Year)) {
  counts.cyc.wt2 <- c(counts.cyc.wt2,sum(cyc.wt2[which(wt2$Year == i),]$cyclones_generation_centers))
}


cyc.wt3 <- cyclones[data$WT == 3,]
cyc.wt3 <- cyc.wt3[-1,]

counts.cyc.wt3 <- c()
for (i in unique(wt3$Year)) {
  counts.cyc.wt3 <- c(counts.cyc.wt3,sum(cyc.wt3[which(wt3$Year == i),]$cyclones_generation_centers))
}


cyc.wt4 <- cyclones[data$WT == 4,]
cyc.wt4 <- cyc.wt4[-1,]

counts.cyc.wt4 <- c()
for (i in unique(wt4$Year)) {
  counts.cyc.wt4 <- c(counts.cyc.wt4,sum(cyc.wt4[which(wt4$Year == i),]$cyclones_generation_centers))
}


cyc.wt5 <- cyclones[data$WT == 5,]
cyc.wt5 <- cyc.wt5[-1,]

counts.cyc.wt5 <- c()
for (i in unique(wt5$Year)) {
  counts.cyc.wt5 <- c(counts.cyc.wt5,sum(cyc.wt5[which(wt5$Year == i),]$cyclones_generation_centers))
}


df.cyc <- data.frame(unique(data$Year), counts.cyc.wt1, counts.cyc.wt2, counts.cyc.wt3, counts.cyc.wt4, counts.cyc.wt5)
colnames(df.cyc) <- c("Year", "WT1", "WT2", "WT3","WT4", "WT5")

df.cyc 

library(ggplot2)

colors <- c("green","brown","red","blue","yellow")
names(colors) <- colnames(df.cyc[2:6])

ggplot(data = df.cyc, aes(x = Year)) + 
  geom_line(aes(x = Year, y = WT1, colour = "WT1")) + 
  geom_line(aes(y = WT2, colour = "WT2")) +
  geom_line(aes(y = WT3, colour = "WT3")) +
  geom_line(aes(y = WT4, colour = "WT4")) +
  geom_line(aes(y = WT5, colour = "WT5")) +
  ylab("Cyclone generation centers number")+
  scale_color_manual(name = "WT",values =c(colors[1],colors[2],colors[3],colors[4],colors[5]))


cor.mat.cyc <- cor(df.cyc[,2:6])

library(lattice)

levelplot(cor.mat)

counts.cyc <- c()
for (i in unique(data$Year)) {
  counts.cyc <- c(counts.cyc,sum(cyclones[which(data$Year == i),]$cyclones_generation_centers))
}

plot(unique(data$Year), counts.cyc, type = 'l', xlab = 'Year', ylab = 'Cyclone centers total nº')

test <- matrix(NA, 1, 5)
for (i in c(2:6)) {
  test[1,i-1] <- cor(df.cyc[,i],counts.cyc)
}

colnames(test) <- colnames(df.cyc)[2:6]

test

m1 <- lm(WT1 ~ Year + 0, data = df)
m2 <- lm(WT2 ~ Year + 0, data = df)
m3 <- lm(WT3 ~ Year + 0, data = df)
m4 <- lm(WT4 ~ Year + 0, data = df)
m5 <- lm(WT5 ~ Year + 0, data = df)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)


library(strucchange)

# efp.m1 <- efp(WT1 ~ Year, data = df, type = "OLS-CUSUM")
# efp.m2 <- efp(WT2 ~ Year, data = df, type = "OLS-CUSUM")
# efp.m3 <- efp(WT3 ~ Year, data = df, type = "OLS-CUSUM")
# efp.m4 <- efp(WT4 ~ Year, data = df, type = "OLS-CUSUM")
# efp.m5 <- efp(WT5 ~ Year, data = df, type = "OLS-CUSUM")


ts.wt1 <- ts(df$WT1, start = 1979, end = 2020)
ts.wt2 <- ts(df$WT2, start = 1979, end = 2020)
ts.wt3 <- ts(df$WT3, start = 1979, end = 2020)
ts.wt4 <- ts(df$WT4, start = 1979, end = 2020)
ts.wt5 <- ts(df$WT5, start = 1979, end = 2020)

efp.m1 <- efp(ts.wt1 ~ 1, data = df, type = "OLS-CUSUM")
efp.m2 <- efp(ts.wt2 ~ 1, data = df, type = "OLS-CUSUM")
efp.m3 <- efp(ts.wt3 ~ 1, data = df, type = "OLS-CUSUM")
efp.m4 <- efp(ts.wt4 ~ 1, data = df, type = "OLS-CUSUM")
efp.m5 <- efp(ts.wt5 ~ 1, data = df, type = "OLS-CUSUM")

# bound.m1 <- boundary(efp.m1, alpha = .05)
# bound.m2 <- boundary(efp.m2, alpha = .05)
# bound.m3 <- boundary(efp.m3, alpha = .05)
# bound.m4 <- boundary(efp.m4, alpha = .05)
# bound.m5 <- boundary(efp.m5, alpha = .05)

par(mfrow = c(2,3))
plot(efp.m1, main = "WT1")
# lines(bound.m1, col = 4)
# lines(-bound.m1,col = 4)
plot(efp.m2, main = "WT2")
# lines(bound.m2, col = 4)
# lines(-bound.m2,col = 4)
plot(efp.m3, main = "WT3")
# lines(bound.m3, col = 4)
# lines(-bound.m3,col = 4)
plot(efp.m4, main = "WT4")
# lines(bound.m4, col = 4)
# lines(-bound.m4,col = 4)
plot(efp.m5, main = "WT5")
# lines(bound.m5, col = 4)
# lines(-bound.m5,col = 4)

sctest(efp.m1)
sctest(efp.m2)
sctest(efp.m3)
sctest(efp.m4)
sctest(efp.m5)
