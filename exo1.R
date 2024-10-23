###
#load("Rain.RData")
#load("Temp.RData")
#load("Wind.RData")
#load("CO2.RData")
#library(ismev)
#library(mev)
#library(ggplot2)
#library(maps)
#library(evd)
#library(evir)

library(ismev)
load("Wind.RData")
data <- Wind[,32] #Eindhoven
times <- as.POSIXlt(WindDates)
#save(data, times, file = "Ex1uni.RData")

monthly <- as.vector(tapply(data, paste(times$year,times$mon), max))
yearly <- as.vector(tapply(data, times$year, max))


gmonthly <- gev.fit(monthly)
gyearly <- gev.fit(yearly)
gummonthly <- gum.fit(monthly)
gumyearly <- gum.fit(yearly)
gev.diag(gmonthly)
gev.diag(gyearly)
gum.diag(gummonthly)
gum.diag(gumyearly)

n <- length(yearly)
ydat <- cbind(1:n)
fit1 = gev.fit(yearly, ydat = ydat, mul = 1, show = F)
fit2 = gum.fit(yearly, ydat = ydat, mul = 1, show = F)

qchisq(0.95, df = 1) # reject if D = 2(L(M1) - L(M0)) bigger than this


## Code the return level with CIs. Is it surprising that CIs are so small compared to the rainfall example?

gmonthly <- gev(monthly)
gyearly <- gev(yearly)
rlevel.gev(gmonthly, k.blocks = 10)
rlevel.gev(gyearly, k.blocks = 10)
rlevel.gev(gmonthly, k.blocks = 10000)
rlevel.gev(gyearly, k.blocks = 10000)


