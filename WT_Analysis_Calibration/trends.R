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

wt1.break <- breakpoints(ts.wt1 ~ 1)
wt2.break <- breakpoints(ts.wt2 ~ 1)
wt3.break <- breakpoints(ts.wt3 ~ 1)
wt4.break <- breakpoints(ts.wt4 ~ 1)
wt5.break <- breakpoints(ts.wt5 ~ 1)

##wt1 two intervals
summary(wt1.break)

ts.wt1.b1 <- window(ts.wt1, start = 1979, end = 2012)
ts.wt1.b2 <- window(ts.wt1, start = 2013, end = 2020)

lm1.b1 <- lm(df[1:34,]$WT1 ~ df[1:34,]$Year + 0)
lm1.b2 <- lm(df[35:42,]$WT1 ~ df[35:42,]$Year + 0)

barplot(cbind(m1$coefficients, lm1.b1$coefficients, lm1.b2$coefficients), beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT1", col = c("orange","red","blue"))
##wt2 two intervals
summary(wt2.break)

ts.wt2.b1 <- window(ts.wt2, start = 1979, end = 1998)
ts.wt2.b2 <- window(ts.wt2, start = 1999, end = 2020)

lm2.b1 <- lm(df[1:20,]$WT2 ~ df[1:20,]$Year + 0)
lm2.b2 <- lm(df[21:42,]$WT2 ~ df[21:42,]$Year + 0)

barplot(cbind(m2$coefficients, lm2.b1$coefficients, lm2.b2$coefficients), beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT2", col = c("orange","red","blue"))
##wt3 two intervals
summary(wt3.break)

ts.wt3.b1 <- window(ts.wt3, start = 1979, end = 1998)
ts.wt3.b2 <- window(ts.wt3, start = 1999, end = 2020)

lm3.b1 <- lm(df[1:20,]$WT3 ~ df[1:20,]$Year + 0)
lm3.b2 <- lm(df[21:42,]$WT3 ~ df[21:42,]$Year + 0)

barplot(cbind(m3$coefficients, lm3.b1$coefficients, lm3.b2$coefficients),beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT3", col = c("orange","red","blue"))
##wt4 two intervals
summary(wt4.break)

ts.wt4.b1 <- window(ts.wt4, start = 1979, end = 1991)
ts.wt4.b2 <- window(ts.wt4, start = 1992, end = 2020)

lm4.b1 <- lm(df[1:13,]$WT4 ~ df[1:13,]$Year + 0)
lm4.b2 <- lm(df[14:42,]$WT4 ~ df[14:42,]$Year + 0)

barplot(cbind(m4$coefficients, lm4.b1$coefficients, lm4.b2$coefficients),beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT4", col = c("orange","red","blue"))
##wt5 two intervals
summary(wt5.break)

ts.wt5.b1 <- window(ts.wt5, start = 1979, end = 1998)
ts.wt5.b2 <- window(ts.wt5, start = 1999, end = 2020)

lm5.b1 <- lm(df[1:20,]$WT5 ~ df[1:20,]$Year + 0)
lm5.b2 <- lm(df[21:42,]$WT5 ~ df[21:42,]$Year + 0)

barplot(cbind(m5$coefficients, lm5.b1$coefficients, lm5.b2$coefficients),beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT5", col = c("orange","red","blue"))
#Nile data example

# data("Nile")
# plot(Nile)
# 
# bp.nile <- breakpoints(Nile ~ 1)
# summary(bp.nile)

library(lubridate)

############################monthly
y <- unique(wt1$Year)
m <- unique(wt1$Month)

counts.wt1 <- c()
for (i in y) {
  y.idx <- which(wt1$Year == i)
  for (j in m) {
    m.idx <- which(wt1$Month == j)
    idx <- intersect(y.idx, m.idx)
    counts.wt1 <- c(counts.wt1,length(idx))
  }  
}

y <- unique(wt2$Year)
m <- unique(wt2$Month)

counts.wt2 <- c()
for (i in y) {
  y.idx <- which(wt2$Year == i)
  for (j in m) {
    m.idx <- which(wt2$Month == j)
    idx <- intersect(y.idx, m.idx)
    counts.wt2 <- c(counts.wt2,length(idx))
  }  
}

y <- unique(wt3$Year)
m <- unique(wt3$Month)

counts.wt3 <- c()
for (i in y) {
  y.idx <- which(wt3$Year == i)
  for (j in m) {
    m.idx <- which(wt3$Month == j)
    idx <- intersect(y.idx, m.idx)
    counts.wt3 <- c(counts.wt3,length(idx))
  }  
}

y <- unique(wt4$Year)
m <- unique(wt4$Month)

counts.wt4 <- c()
for (i in y) {
  y.idx <- which(wt4$Year == i)
  for (j in m) {
    m.idx <- which(wt4$Month == j)
    idx <- intersect(y.idx, m.idx)
    counts.wt4 <- c(counts.wt4,length(idx))
  }  
}

y <- unique(wt5$Year)
m <- unique(wt5$Month)

counts.wt5 <- c()
for (i in y) {
  y.idx <- which(wt5$Year == i)
  for (j in m) {
    m.idx <- which(wt5$Month == j)
    idx <- intersect(y.idx, m.idx)
    counts.wt5 <- c(counts.wt5,length(idx))
  }  
}

yy <- c()
for (i in unique(y)) {
  yy <- c(yy, rep(i,12))
}

df <- data.frame(seq(1,504),yy, rep(seq(1:12),42), counts.wt1, counts.wt2, counts.wt3, counts.wt4, counts.wt5)
colnames(df) <- c("Id","Year","Month", "WT1", "WT2", "WT3","WT4", "WT5")


colors <- c("green","brown","red","blue","yellow")
names(colors) <- colnames(df[3:7])

ggplot(data = df, aes(x = Id)) + 
  geom_line(aes(x = Id, y = WT1, colour = "WT1")) + 
  geom_line(aes(y = WT2, colour = "WT2")) +
  geom_line(aes(y = WT3, colour = "WT3")) +
  geom_line(aes(y = WT4, colour = "WT4")) +
  geom_line(aes(y = WT5, colour = "WT5")) +
  ylab("WT days")+
  scale_color_manual(name = "WT",values =c(colors[1],colors[2],colors[3],colors[4],colors[5]))


ggplot(data = df, aes(x = Id)) + 
  geom_line(aes(x = Id, y = WT1, colour = "WT1")) + 
  ylab("WT days")+
  scale_color_manual(name = "WT",values =c(colors[1],colors[2],colors[3],colors[4],colors[5]))


ggplot(data = df, aes(x = Id)) + 
  geom_line(aes(x = Id, y = WT2, colour = "WT2")) + 
  ylab("WT days")+
  scale_color_manual(name = "WT",values =c(colors[1],colors[2],colors[3],colors[4],colors[5]))

ggplot(data = df, aes(x = Id)) + 
  geom_line(aes(x = Id, y = WT3, colour = "WT3")) + 
  ylab("WT days")+
  scale_color_manual(name = "WT",values =c(colors[1],colors[2],colors[3],colors[4],colors[5]))

ggplot(data = df, aes(x = Id)) + 
  geom_line(aes(x = Id, y = WT4, colour = "WT4")) + 
  ylab("WT days")+
  scale_color_manual(name = "WT",values =c(colors[1],colors[2],colors[3],colors[4],colors[5]))

ggplot(data = df, aes(x = Id)) + 
  geom_line(aes(x = Id, y = WT5, colour = "WT5")) + 
  ylab("WT days")+
  scale_color_manual(name = "WT",values =c(colors[1],colors[2],colors[3],colors[4],colors[5]))


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

ts.wt1 <- ts(df$WT1)
ts.wt2 <- ts(df$WT2)
ts.wt3 <- ts(df$WT3)
ts.wt4 <- ts(df$WT4)
ts.wt5 <- ts(df$WT5)

wt1.bp <- breakpoints(ts.wt1 ~ 1)
wt2.bp <- breakpoints(ts.wt2 ~ 1)
wt3.bp <- breakpoints(ts.wt3 ~ 1)
wt4.bp <- breakpoints(ts.wt4 ~ 1)
wt5.bp <- breakpoints(ts.wt5 ~ 1)

summary(wt1.bp)
ts.wt1.b1 <- window(ts.wt1, start = 1, end = 423)
ts.wt1.b2 <- window(ts.wt1, start = 424, end = 504)

lm1.b1 <- lm(df[1:423,]$WT1 ~ df[1:423,]$Id + 0)
lm1.b2 <- lm(df[424:504,]$WT1 ~ df[424:504,]$Id + 0)

summary(wt2.bp)
ts.wt2.b1 <- window(ts.wt2, start = 1, end = 249)
ts.wt2.b2 <- window(ts.wt2, start = 250, end = 504)

lm2.b1 <- lm(df[1:249,]$WT2 ~ df[1:249,]$Id + 0)
lm2.b2 <- lm(df[250:504,]$WT2 ~ df[250:504,]$Id + 0)


summary(wt3.bp)
ts.wt3.b1 <- window(ts.wt3, start = 1, end = 258)
ts.wt3.b2 <- window(ts.wt3, start = 259, end = 504)

lm3.b1 <- lm(df[1:258,]$WT3 ~ df[1:258,]$Id + 0)
lm3.b2 <- lm(df[259:504,]$WT3 ~ df[259:504,]$Id + 0)


summary(wt4.bp)
ts.wt4.b1 <- window(ts.wt4, start = 1, end = 111)
ts.wt4.b2 <- window(ts.wt4, start = 112, end = 504)

lm4.b1 <- lm(df[1:111,]$WT4 ~ df[1:111,]$Id + 0)
lm4.b2 <- lm(df[112:504,]$WT4 ~ df[112:504,]$Id + 0)


summary(wt5.bp)
ts.wt5.b1 <- window(ts.wt5, start = 1, end = 248)
ts.wt5.b2 <- window(ts.wt5, start = 249, end = 504)

lm5.b1 <- lm(df[1:248,]$WT5 ~ df[1:248,]$Id + 0)
lm5.b2 <- lm(df[249:504,]$WT5 ~ df[249:504,]$Id + 0)


par(mfrow = c(2,3))

barplot(cbind(m1$coefficients, lm1.b1$coefficients, lm1.b2$coefficients), beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT1", col = c("orange","red","blue"))

barplot(cbind(m2$coefficients, lm2.b1$coefficients, lm2.b2$coefficients), beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT2", col = c("orange","red","blue"))

barplot(cbind(m3$coefficients, lm3.b1$coefficients, lm3.b2$coefficients), beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT3", col = c("orange","red","blue"))

barplot(cbind(m4$coefficients, lm4.b1$coefficients, lm4.b2$coefficients), beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT4", col = c("orange","red","blue"))

barplot(cbind(m5$coefficients, lm5.b1$coefficients, lm5.b2$coefficients), beside = T,
        names.arg = c("complete","< bp","> bp"), main = "WT5", col = c("orange","red","blue"))

#####wt1+wt4
#annual
counts.wt1 <- c()
for (i in unique(wt1$Year)) {
  counts.wt1 <- c(counts.wt1,length(which(wt1$Year == i)))
}
counts.wt2 <- c()
for (i in unique(wt2$Year)) {
  counts.wt2 <- c(counts.wt2,length(which(wt2$Year == i)))
}

counts.wt4 <- c()
for (i in unique(wt4$Year)) {
  counts.wt4 <- c(counts.wt4,length(which(wt4$Year == i)))
}

counts.wts14 <- counts.wt1 + counts.wt4
df <- data.frame(unique(wt4$Year),counts.wts14)
colnames(df) <- c("Year","Counts")

ggplot(data = df, aes(x = Year)) + 
  geom_line(aes(x = Year, y = Counts, colour = "WT1 + WT4")) + 
  ylab("Cyclone generation centers number")+
  scale_color_manual(name = "WT",values = "blue")

model <- lm(Counts ~ Year + 0, data = df) #coef 0.1 (1,2,4) y 0.06(1,4)
summary(model)

model.intercept <- lm(Counts ~ Year, data = df) 
summary(model.intercept)


ts.wt14 <- ts(df$Counts, start = 1979, end = 2020)

wt14.break <- breakpoints(ts.wt14 ~ 1)
summary(wt14.break)

ts.wt14.b1 <- window(ts.wt14, start = unique(data$Year)[1], end = unique(data$Year)[13])
ts.wt14.b2 <- window(ts.wt14, start = unique(data$Year)[14], end = unique(data$Year)[42])

lm1.b1 <- lm(df[1:13,]$Counts ~ df[1:13,]$Year + 0)
lm1.b2 <- lm(df[14:42,]$Counts ~ df[14:42,]$Year + 0)

summary(lm1.b1)
summary(lm1.b2)

lm1.b1.int <- lm(df[1:13,]$Counts ~ df[1:13,]$Year)
lm1.b2.int <- lm(df[14:42,]$Counts ~ df[14:42,]$Year)

summary(lm1.b1.int)
summary(lm1.b2.int)

annual.results <- c(model$coefficients, model.intercept$coefficients[2],
                    lm1.b1$coefficients, lm1.b2$coefficients,
                    lm1.b1.int$coefficients[2], lm1.b2.int$coefficients[2])

names(annual.results) <- c("model complete","model complete int", "bp1","bp2",
                           "bp1 int", "bp2 int")
#monthly

counts.wt1 <- c()
for (i in y) {
  y.idx <- which(wt1$Year == i)
  for (j in m) {
    m.idx <- which(wt1$Month == j)
    idx <- intersect(y.idx, m.idx)
    counts.wt1 <- c(counts.wt1,length(idx))
  }  
}

counts.wt4 <- c()
for (i in y) {
  y.idx <- which(wt4$Year == i)
  for (j in m) {
    m.idx <- which(wt4$Month == j)
    idx <- intersect(y.idx, m.idx)
    counts.wt4 <- c(counts.wt4,length(idx))
  }  
}

dates <- c()
for (i in seq(1979,2020)) {
  dates <- c(dates, rep(i,12))
}
counts.wts14 <- counts.wt1 + counts.wt4
df <- data.frame(seq(1,504,1),counts.wts14)
colnames(df) <- c("Year","Counts")

ggplot(data = df, aes(x = Year)) + 
  geom_line(aes(x = Year, y = Counts, colour = "WT1 + WT4")) + 
  ylab("Cyclone generation centers number")+
  scale_color_manual(name = "WT",values = "blue")


model <- lm(Counts ~ Year + 0, data = df) 
summary(model)

model.intercept <- lm(Counts ~ Year, data = df) 
summary(model.intercept)

ts.wt14 <- ts(df$Counts, start = 1, end = 504)

wt14.break <- breakpoints(ts.wt14 ~ 1)
summary(wt14.break)

lm1.b1 <- lm(df[1:170,]$Counts ~ df[1:170,]$Year+0)
lm1.b2 <- lm(df[171:504,]$Counts ~ df[171:504,]$Year+0)

lm1.b1.int <- lm(df[1:170,]$Counts ~ df[1:170,]$Year)
lm1.b2.int <- lm(df[171:504,]$Counts ~ df[171:504,]$Year)

monthly.results <- c(model$coefficients, model.intercept$coefficients[2],
                    lm1.b1$coefficients, lm1.b2$coefficients,
                    lm1.b1.int$coefficients[2], lm1.b2.int$coefficients[2])

names(monthly.results) <- c("model complete","model complete int", "bp1","bp2",
                           "bp1 int", "bp2 int")

annual.results

monthly.results