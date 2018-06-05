#==============================Chapter 1 ===================================
#  1.4.1 A flying start: Air passenger bookings
data(AirPassengers)
AP <- AirPassengers
AP

class(AP)


plot(AP, ylab = "Passengers (1000's)")


layout(1:2)
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

# 1.4.2 Unemployment: Maine

# Make sure you have set your working directory to where your files are
www <- "Maine.dat"
Maine.month <- read.table(www, header = TRUE)
attach(Maine.month)
class(Maine.month)

Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)
Maine.annual.ts <- aggregate(Maine.month.ts)/12
# or aggregate(Maine.month.ts, FUN = mean)
layout(1:2)
plot(Maine.month.ts, ylab = "unemployed (%)")
plot(Maine.annual.ts, ylab = "unemployed (%)")

Maine.Feb <- window(Maine.month.ts, start = c(1996,2), freq = TRUE)
Maine.Aug <- window(Maine.month.ts, start = c(1996,8), freq = TRUE)
Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts)
Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts)

Feb.ratio

Aug.ratio

www <- "USunemp.dat"
US.month <- read.table(www, header = T)
attach(US.month)
US.month.ts <- ts(USun, start=c(1996,1), end=c(2006,10), freq = 12)
layout(1:1)
plot(US.month.ts, ylab = "unemployed (%)")


#1.4.3 Multiple time series: Electricity, beer and chocolate data
www <- "cbe.dat"
CBE <- read.table(www,header = TRUE)
CBE[1:4,]

class(CBE)
Elec.ts <- ts(CBE[,3], start = 1958, freq = 12)
Beer.ts <- ts(CBE[,2], start = 1958, freq = 12)
Choc.ts <- ts(CBE[,1], start = 1958, freq = 12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

AP.elec <- ts.intersect(AP, Elec.ts)

start(AP.elec)
end(AP.elec)

AP.elec[1:3,]

AP <- AP.elec[,1]; Elec <- AP.elec[,2]
layout(1:2)
plot(AP, main = "Air Passengers", ylab = "Air Passengers /1000's")
plot(Elec, main = "Electricity", ylab = "Electricity production / MkWh")

plot(as.vector(AP), as.vector(Elec), 
     xlab = "Air Passengers/1000's",
     ylab = "Electricity production /Mwh")
abline(reg = lm(Elec~AP))
cor(AP,Elec)
# 1.4.4 Quarterly exchange rate: GBP to NZ dollar
www <- "pounds_nz.dat"
Z <- read.table(www,header = T)
Z[1:4,]
Z.ts <- ts(Z, st =1991, freq=4)
plot(Z.ts, xlab = "time/ years",
            ylab= "Quarterly exchange rate in $NZ/ pounds")

Z.92.96 <- window(Z.ts, start = c(1992,1), end =c(1996,1))
Z.96.98 <- window(Z.ts, start =c(1996,1), end = c(1998,1))

layout(1:2)
plot(Z.92.96, ylab = "Exchange rate in $NZ/pounds",
              xlab = "Time (years)")
plot(Z.96.98, ylab = "Exchange rate in $NZ/pounds",
     xlab = "Time (years)")

#1.4.5 Global temperature series        
www <-"global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st= c(1856,1), end = c(2005,12), frequency = 12)
Global.annual <-aggregate(Global.ts,FUN = mean)
plot(Global.ts)
plot(Global.annual)

New.series <- window(Global.ts, start= c(1970,1), end = c(2005,12))
New.time <- time(New.series)
plot(New.series); abline(reg = lm(New.series~New.time))

#1.5 Decomposition of series

plot(decompose(Elec.ts))

Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend, Trend*Seasonal), lty= 1:2)

# 1.7 Exercises
#1 
layout(1:1)
plot(aggregate(Choc.ts), main = "Australia Chocolate \n Production during 1958 - 1990 ")
plot(aggregate(Beer.ts), main = "Australia Beer \n Production during 1958 - 1990 ")

boxplot(Choc.ts~cycle(Choc.ts))
boxplot(Beer.ts~ cycle(Beer.ts))
choc.ts.decom <- decompose(Choc.ts)
plot(choc.ts.decom)
ts.plot(cbind(choc.ts.decom$trend, choc.ts.decom$trend+choc.ts.decom$seasonal), lty = 1:2)

#1 
mot <- matrix(c(0.33, 18000,0.5,20000,2000,0.80,1500,1.6,40,40,20,60,3,80,2,120,2,200,1,360), byrow = TRUE, nrow = 5,ncol = 4,dimnames = list(c("car","petrol(litre)","servicing (h)","tyre","clutch"),c("q00","u.p00","q04","u.p04")))
#LIt - Laspeyre Price Index 
LIt<- sum(mot[,1]*mot[,4])/sum(mot[,1]*mot[,2])
LIt
#3 
#a Paasche Price Index 
PIt <- sum(mot[,3]*mot[,4])/sum(mot[,3]*mot[,2])
#c 
IFP<- sqrt(LIt*PIt)
IFP

#==============================Chapter 2 ===================================

#2.2.1 Expected Value

www <- "Herald.dat"
Herald.dat <- read.table(www, header = T)
attach(Herald.dat)

x <- CO; y <- Benzoa; n <- length(x)
#-----------------
sum((x -mean(x))*(y-mean(y)))/(n-1)

mean((x-mean(x))*(y -mean(y)))

cov(x, y)

#------------------

cov(x,y)/(sd(x)*sd(y))

cor(x,y) 

#
www <- "wave.dat"
wave.dat <- read.table(www,header = T) ; attach(wave.dat)
layout(1:2)
plot(ts(waveht)); plot(ts(waveht[1:60]))
# Formula of Ck (proof of concept)
n <-length(waveht)

ck <- 1/n* sum((waveht[1:(n-1)] - mean(waveht))*(waveht[2:n]- mean(waveht)))
c0 <- 1/n * sum((waveht[1:n] - mean(waveht))* (waveht[1:n] - mean(waveht)))
rk <- ck/c0

ck/sd(waveht)
acf(waveht)$acf[2] # This should equal to rk 
plot(waveht[1:396], waveht[2:397])
acf(waveht, type = c("covariance"))$acf[2] # This should equal to Ck
abline(h = 0, v = 0, col= "pink")
acf.waveht<- acf(waveht)
acf.waveht$type
# 2.3.1  The correlogram General Discussion 
acf(AirPassengers)
layout(1:2)
data("AirPassengers")
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")
plot(ts(AP.decom$random[7:138]))
acf(AP.decom$random[7:138])

sd(AP[7:138])
sd(AP[7:138]-AP.decom$trend[7:138])
sd(AP.decom$random[7:138])

# 2.3.3 Example based on the Font Reservior series 
www <- "Fontdsdt.dat"
Fontdsdt.dat <- read.table(www, header = T)
attach(Fontdsdt.dat)
plot(ts(adflow), ylab = 'adflow')
acf(adflow, xlab = "lag (months)", main = "")
