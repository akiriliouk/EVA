###
load("Rain.RData")
load("Temp.RData")
load("Wind.RData")
load("CO2.RData")
library(ismev)
library(ggplot2)

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
for(i in 1:12){
  date <- as.Date(paste(i+ 1995, "-01-15", sep = ""))
  g <- g + geom_vline(xintercept = date, color = "grey")
}
print(g)
dev.off()

pdf("Fig/rainmaxsample.pdf", width = 10, height = 5)
g <- ggplot(datamax, aes(x = Date, y = Rain)) + 
  geom_point() +
  theme_bw() + xlab("Time (years)") + ylab("Yearly maxima of daily precipitation in mm") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
print(g)
dev.off()

### 69 years of data
maxtemp <- Temp[,39] #Near Namur
ntemp <- length(maxtemp)
datamax <- data.frame('Year' = TempDates, 'Temp' = maxtemp)

g <- ggplot(datamax, aes(x = Year, y = Temp)) + 
  geom_point() +
  coord_cartesian(ylim = c(22,38)) + 
  theme_bw() + xlab("Time (years)") + ylab("Yearly maxima of daily maximum temperatures") + 
  theme(axis.title.x = element_text(size=19), 
        axis.title.y = element_text(size=19), 
        axis.text.x = element_text(size=16),  
        axis.text.y = element_text(size=16)) 
print(g)

gev.fit(maxtemp)

#plot.pars <- function(values, LonLat, nlevels, labels, limvalues, mars = c(3,2,4,0)+1,
#                      title = "",cexpoints = 1.5, CI = TRUE, cst = 2, rectm = c(41,43)){ 
  xlon <- sort(unique(TempCoord[,1]))
  ylat <- sort(unique(TempCoord[,2]))
  ylatlim <- c(floor(min(ylat)), ceiling(max(ylat)))
  xlonlim <- c(floor(min(xlon)), ceiling(max(xlon)))
  map("world",xlim=xlonlim + c(-1.5,1.5),ylim=ylatlim + c(-1,1.5))
  cities <- get('world.cities')
  cities <- cities[cities$country.etc == 'Belgium', ]
  citiessel <- cities[which(cities$pop > 100000),]
  points(citiessel[,c(5,4)], col = "blue", pch = "15")
  
  #axis(1, at=c(-10,0,10,20,30,40), labels=c(-10,0,10,20,30,40), cex.axis = 1.25)
  #axis(2, at=c(35,40,45,50,55,60), labels=c(35,40,45,50,55,60),  cex.axis = 1.25)
  #title("title", xlab="Longitudes", ylab="Latitudes", adj = 0.425, cex.main = 1.5, cex.lab=1.25)
  
  points(TempCoord, col = "red")
  
#  levels <- seq(limvalues[1],limvalues[2],length.out = nlevels)
#  col <- colorRampPalette(c("dark green","yellow","red"))(nlevels)  
#  colz <- col[cut(values[,1],levels, include.lowest = TRUE)]
#  rect(xlonlim[2]+0.5,ylatlim[1]-1,xlonlim[2]+5,ylatlim[2]+1,col = "white", border = NA)
#  points(LonLat,col = colz, xaxt='n',yaxt='n',xlab="",ylab="", bty="n", pch = 19, cex = cexpoints)
  
#  ylocs <- seq(ylatlim[1], ylatlim[2], length = nlevels)
#  slope <- (1/(levels[2]-levels[1]))*(ylocs[2]-ylocs[1])
#  intercept <- ylocs[1] + (-(levels[1])/(levels[2]-levels[1]))*(ylocs[2]-ylocs[1])
#  ylabels <- labels*slope + intercept
#  axis(4, at = ylabels, labels = labels, las = 1, cex.axis = 1.25)
#  rect(rectm[1], ylocs[-nlevels], rectm[2], ylocs[-1],col=col,border=col) 
  

  
