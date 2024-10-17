###
load("Rain.RData")
load("Temp.RData")
load("Wind.RData")
load("CO2.RData")
library(ismev)
library(mev)
library(ggplot2)
library(maps)

station <- 1
### june-july-august = 92 days. 51 years of data
years <- format(RainDates, "%Y")
uyears <- unique(years)
maxrain <- indmaxrain <- vector(length = 51)
for(j in 1:51){
  maxrain[j] <- max(Rain[years == uyears[j],station])  
  indmaxrain[j] <- which.max(Rain[years == uyears[j],1]) + 92*(j-1)  
}
data <- data.frame('Date' = RainDates, 'Rain' = Rain[,station])
datamax <- data.frame('Date' = RainDates[indmaxrain], 'Rain' = maxrain)


pdf("Fig/rain.pdf", width = 10, height = 5)
ggplot(data, aes(x = Date, y = Rain)) + 
  geom_point() +
  theme_bw() + xlab("Time (years)") + ylab("Daily precipitation (mm)") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()


pdf("Fig/rainmax.pdf", width = 10, height = 5)
g <- ggplot(data, aes(x = Date, y = Rain)) + 
  geom_point() +
  geom_point(data = datamax,  mapping = aes(x = Date, y = Rain), col="red") + 
  theme_bw() + xlab("Time (years)") + ylab("Daily precipitation in mm") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
print(g)
dev.off()


datasub <- subset(data, Date >= as.Date("1996-05-01") & Date <= as.Date("2007-10-01"))
datamaxsub <- subset(datamax, Date >= as.Date("1996-05-01") &  Date <= as.Date("2007-10-01"))

pdf("Fig/rainmaxzoom.pdf", width = 10, height = 5)
g <- ggplot(datasub, aes(x = Date, y = Rain)) + 
  geom_point() +
  geom_point(data = datamaxsub,  mapping = aes(x = Date, y = Rain), col="red") + 
  theme_bw() + xlab("Time (years)") + ylab("Daily precipitation in mm") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
#for(i in 1:12){
#  date <- as.Date(paste(i+ 1995, "-01-15", sep = ""))
#  g <- g + geom_vline(xintercept = date, color = "grey")
#}
print(g)
dev.off()

pdf("Fig/rainmaxsample.pdf", width = 10, height = 5)
g <- ggplot(datamax, aes(x = Date, y = Rain)) + 
  geom_point() +
  theme_bw() + xlab("Time (years)") + ylab("Yearly maxima of daily precipitation") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
print(g)
dev.off()


gevrain <- gev.fit(datamax$Rain)
x <- seq(0, 150, by = 0.01)
dens <- dgev(x, loc = gevrain$mle[1], shape = gevrain$mle[3], scale = gevrain$mle[2])
raindens <- data.frame(x, dens)
pdf("Fig/raingevdens.pdf", width = 10, height = 5)
ggplot(raindens, aes(x = x, y = dens)) + 
  geom_line() +
  theme_bw() + xlab("x") + ylab("G(x)") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()

# lower bound
low <- gevrain$mle[1] - gevrain$mle[2]/gevrain$mle[3]
sum(Rain[,station]>=low)/length(Rain[,station])

# proba
1 - pgev(80, loc = gevrain$mle[1], shape = gevrain$mle[3], scale = gevrain$mle[2])

gev.Nyr(par = gevrain$mle, nobs = 51, N = 10)
gev.Nyr(par = gevrain$mle, nobs = 51, N = 100)
gev.Nyr(par = gevrain$mle, nobs = 51, N = 1000)


gev.rl(gevrain$mle,gevrain$cov,gevrain$data)

a <- gevrain$mle
mat <- gevrain$cov
dat <- gevrain$data
eps <- 1e-06
a1 <- a ; a2 <- a ; a3 <- a
a1[1] <- a[1] + eps
a2[2] <- a[2] + eps
a3[3] <- a[3] + eps
f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
       0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
q <- gevq(a, 1 - f)
d <- t(gev.rl.gradient(a = a, p = 1 - f))
v <- apply(d, 1, q.form, m = mat)


rldata <- data.frame('x' = -1/log(f), 'y' = q)
rldata2 <- data.frame('x' = -1/log((1:length(dat))/(length(dat) + 1)), 'y' = sort(dat))
pdf("Fig/rainrlplot.pdf")
ggplot(rldata, aes(x = x, y = y)) + 
  geom_point(data = rldata2, aes(x, y), col = "blue") +
  scale_x_continuous(trans='log10') + 
  geom_line(linewidth = 0.75)  +  coord_cartesian(ylim = c(0,300)) +
  theme_bw() + xlab("Return period") + ylab("Return level") + 
  geom_line(aes(x = x, y = q + 1.96 * sqrt(v)), linetype = 2) +
  geom_line(aes(x = x, y = q - 1.96 * sqrt(v)), linetype = 2) +
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()

qqdata <- data.frame('x' = gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), 'y' = sort(dat))
pdf("Fig/rainqqplot.pdf")
ggplot(qqdata, aes(x = x, y = y)) + 
  geom_point() +   theme_bw() + xlab("Model") + ylab("Empirical") + 
  geom_abline(col = "darkgrey") +
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()

u <- quantile(data$Rain,0.98)
datau <- subset(data, Rain >= u)
pdf("Fig/rainu.pdf", width = 10, height = 5)
ggplot(data, aes(x = Date, y = Rain)) + 
  geom_point() +
  geom_hline(yintercept = u, col = "red") +
  geom_point(datau, mapping = aes(x = Date, y = Rain), col = "red") + 
  theme_bw() + xlab("Time (years)") + ylab("Daily precipitation in mm") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()

gpd.fitrange(data$Rain, umin = quantile(data$Rain,0.9), umax = quantile(data$Rain,0.995), nint = 50)

gpdrain <- gpd.fit(data$Rain, threshold = quantile(data$Rain,0.95), npy = 92)
x <- seq(0, 75, by = 0.01)
dens <- dgpd(x, shape = gpdrain$mle[2], scale = gpdrain$mle[1])
raindens <- data.frame(x, dens)
pdf("Fig/raingpddens.pdf", width = 10, height = 5)
ggplot(raindens, aes(x = x, y = dens)) + 
  geom_line() +
  theme_bw() + xlab("x") + ylab("H(x)") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()


(prob <- length(which(data$Rain>u))*(1 - pgpd(80, shape = gpdrain$mle[2], scale = gpdrain$mle[1]))/length(data$Rain))
1 - dbinom(0, size = 92, prob =  prob)

gpd.rl(gpdrain$mle,gpdrain$threshold,gpdrain$rate, gpdrain$n, gpdrain$npy, gpdrain$cov, gpdrain$data, gpdrain$xdata)

a <- gpdrain$mle
u <- gpdrain$threshold
la <- gpdrain$rate
n <- gpdrain$n
npy <- gpdrain$npy
mat <- gpdrain$cov
dat <- gpdrain$data
xdat <- gpdrain$xdata
a <- c(la, a)
eps <- 1e-06
a1 <- a ; a2 <- a ; a3 <- a
a1[1] <- a[1] + eps
a2[2] <- a[2] + eps
a3[3] <- a[3] + eps
jj <- seq(-1, 3.75 + log10(npy), by = 0.1)
m <- c(1/la, 10^jj)
q <- gpdq2(a[2:3], u, la, m)
d <- t(gpd.rl.gradient(a = a, m = m))
mat <- matrix(c((la * (1 - la))/n, 0, 0, 0, mat[1, 1], mat[1, 
                                                           2], 0, mat[2, 1], mat[2, 2]), ncol = 3)
v <- apply(d, 1, q.form, m = mat)
nl <- n - length(dat) + 1
sdat <- sort(xdat)


#rldata <- data.frame('x' = m/npy, 'y' = q)
rldata <- data.frame('x' = m[q > u - 1]/npy, 'y' = q[q > u - 1])
rldata2 <- data.frame('x' = (1/(1 - (1:n)/(n + 1))/npy)[sdat > u], 'y' = sdat[sdat > u])
pdf("Fig/rainrlgpdplot.pdf")
ggplot(rldata, aes(x = x, y = y)) + 
  geom_point(data = rldata2, aes(x, y), col = "blue") +
  scale_x_continuous(trans='log10') + 
  geom_line(linewidth = 0.75)  +  coord_cartesian(ylim = c(0,300)) + 
  theme_bw() + xlab("Return period") + ylab("Return level") + 
  geom_line(aes(x = x, y = y + 1.96 * sqrt(v)[q > u - 1]), linetype = 2) +
  geom_line(aes(x = x, y = y - 1.96 * sqrt(v)[q > u - 1]), linetype = 2) +
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()

qqdata <- data.frame('x' = gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), 'y' = sort(dat))
pdf("Fig/rainqqplot.pdf")
ggplot(qqdata, aes(x = x, y = y)) + 
  geom_point() +   theme_bw() + xlab("Model") + ylab("Empirical") + 
  geom_abline(col = "darkgrey") +
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()



  
  
  
### 69 years of data
maxtemp <- Temp[,39] #Near Namur
datamax <- data.frame('Year' = TempDates, 'Temp' = maxtemp)

pdf("Fig/temp.pdf")
ggplot(datamax, aes(x = Year, y = Temp)) + 
  geom_point() +
  coord_cartesian(ylim = c(22,38)) + 
  theme_bw() + xlab("Time (years)") + ylab("Yearly maxima of daily max temperature") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()


gevtemp <- gev.fit(datamax$Temp)
x <- seq(17.5, 42.5, by = 0.01)
dens <- dgev(x, loc = gevtemp$mle[1], shape = gevtemp$mle[3], scale = gevtemp$mle[2])
tempdens <- data.frame(x, dens)
pdf("Fig/tempgevdens.pdf", width = 10, height = 5)
ggplot(tempdens, aes(x = x, y = dens)) + 
  geom_line() +
  theme_bw() + xlab("x") + ylab("G(x)") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()

# upper bound
up <- gevtemp$mle[1] - gevtemp$mle[2]/gevtemp$mle[3]

# proba
1 - pgev(38, loc = gevtemp$mle[1], shape = gevtemp$mle[3], scale = gevtemp$mle[2])



mat <- gevtemp$cov
dat <- gevtemp$data
eps <- 1e-06
a1 <- a ; a2 <- a ; a3 <- a
a1[1] <- a[1] + eps
a2[2] <- a[2] + eps
a3[3] <- a[3] + eps
f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
       0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
q <- gevq(a, 1 - f)
d <- t(gev.rl.gradient(a = a, p = 1 - f))
v <- apply(d, 1, q.form, m = mat)

rldata <- data.frame('x' = -1/log(f), 'y' = q)
rldata2 <- data.frame('x' = -1/log((1:length(dat))/(length(dat) + 1)), 'y' = sort(dat))
pdf("Fig/temprlplot.pdf")
ggplot(rldata, aes(x = x, y = y)) + 
  geom_point(data = rldata2, aes(x, y), col = "blue") +
  scale_x_continuous(trans='log10') + 
  geom_line(linewidth = 0.75)  + 
  theme_bw() + xlab("Return period") + ylab("Return level") + 
  geom_line(aes(x = x, y = q + 1.96 * sqrt(v)), linetype = 2) +
  geom_line(aes(x = x, y = q - 1.96 * sqrt(v)), linetype = 2) +
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()

qqdata <- data.frame('x' = gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), 'y' = sort(dat))
pdf("Fig/tempqqplot.pdf")
ggplot(qqdata, aes(x = x, y = y)) + 
  geom_point() +   theme_bw() + xlab("Model") + ylab("Empirical") + 
  geom_abline(col = "darkgrey") +
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
dev.off()











library(maps)
  xlon <- sort(unique(TempCoord[,1]))
  ylat <- sort(unique(TempCoord[,2]))
  ylatlim <- c(floor(min(ylat)), ceiling(max(ylat)))
  xlonlim <- c(floor(min(xlon)), ceiling(max(xlon)))
  map("world",xlim=xlonlim + c(-1.5,1.5),ylim=ylatlim + c(-1,1.5))
  cities <- get('world.cities')
  cities <- cities[cities$country.etc == 'Belgium', ]
  citiessel <- cities[which(cities$pop > 100000),]
  points(citiessel[,c(5,4)], col = "blue", pch = "15")
 
  points(TempCoord, col = "red")

