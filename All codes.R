#==============================Chapter 1 ===================================
#----1.4.1 A flying start: Air passenger bookings-----
data(AirPassengers)
AP <- AirPassengers
AP

class(AP)


plot(AP, ylab = "Passengers (1000's)")


layout(1:2)
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

#----1.4.2 Unemployment: Maine----

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


#-----1.4.3 Multiple time series: Electricity, beer and chocolate data-----
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
#--- 1.4.4 Quarterly exchange rate: GBP to NZ dollar----
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

#----1.4.5 Global temperature series  ----      
www <-"global.dat"
Global <- scan(www)
Global.ts <- ts(Global, st= c(1856,1), end = c(2005,12), frequency = 12)
Global.annual <-aggregate(Global.ts,FUN = mean)
plot(Global.ts)
plot(Global.annual)

New.series <- window(Global.ts, start= c(1970,1), end = c(2005,12))
New.time <- time(New.series)
plot(New.series); abline(reg = lm(New.series~New.time))

#----1.5 Decomposition of series-----

plot(decompose(Elec.ts))

Elec.decom <- decompose(Elec.ts, type = "mult")
plot(Elec.decom)
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
ts.plot(cbind(Trend, Trend*Seasonal), lty= 1:2)

#--1.7 Exercises
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

#----2.2.1 Expected Value-----

www <- "Herald.dat"
Herald.dat <- read.table(www, header = T)
attach(Herald.dat)

x <- CO; y <- Benzoa; n <- length(x)
#===---
sum((x -mean(x))*(y-mean(y)))/(n-1)

mean((x-mean(x))*(y -mean(y)))

cov(x, y)

#===---

cov(x,y)/(sd(x)*sd(y))

cor(x,y) 

#
www <- "wave.dat"
wave.dat <- read.table(www,header = T) ; attach(wave.dat)
layout(1:2)
plot(ts(waveht)); plot(ts(waveht[1:60]))
#----Formula of Ck (proof of concept)-----
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
#----2.3.1  The correlogram General Discussion -----
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

#-----2.3.3 Example based on the Font Reservior series -----
www <- "Fontdsdt.dat"
Fontdsdt.dat <- read.table(www, header = T)
attach(Fontdsdt.dat)
plot(ts(adflow), ylab = 'adflow')
acf(adflow, xlab = "lag (months)", main = "")


#==============================Chapter 3 ===================================

# -----3.2.2 Building approvals publication-----

www <- "ApprovActiv.dat"
Build.dat <- read.table(www, header = TRUE); attach(Build.dat)
App.ts <- ts(Approvals, start = c(1996, 1), freq = 4)
Act.ts <- ts(Activity, start = c(1996, 1), freq = 4)
ts.plot(App.ts, Act.ts, lty = c(1,3))

acf(ts.union(App.ts, Act.ts))

app.ran <- decompose(App.ts)$random
app.ran.ts <- window (app.ran, start = c(1996, 3) )
act.ran <- decompose (Act.ts)$random
act.ran.ts <- window (act.ran, start = c(1996, 3) )
acf(ts.union(app.ran.ts, act.ran.ts),na.action = na.pass)
ccf(app.ran.ts, act.ran.ts, na.action = na.pass)

print(acf(ts.union(app.ran.ts, act.ran.ts),na.action = na.pass))
print(ccf(app.ran.ts, act.ran.ts, na.action = na.pass))


# ----3.3 Bass model----
#Example 

T79 <- 1:10
Tdelt <- (1:100)/10 
Sales <- c(840,1470,2110,4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cusales <- cumsum(Sales)
Bass.nls <-nls(Sales ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * T79) ) /
                 (1+(Q/P)*exp(-(P+Q)*T79))^2, start = list(M=60630, P=0.03, Q=0.38))
summary(Bass.nls)

Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]

ngete <- exp(-(p+q) * Tdelt)
Bpdf <- m * ( (p+q)^2 / p ) * ngete / (1 + (q/p) * ngete)^2
plot(Tdelt, Bpdf, xlab = "Year from 1979", ylab = "Sales per year", type='l')
points(T79, Sales)

Bcdf <- m * (1 - ngete)/(1 + (q/p)*ngete)
plot(Tdelt, Bcdf, xlab = "Year from 1979",
     ylab = "Cumulative sales", type='l')
points(T79, Cusales)

#----3.4 Exponential smoothing & the Holt-Winters method----
#----3.4.1 Exponential smoothing----

www <- "motororg.dat"
Motor.dat <- read.table(www, header = T); attach(Motor.dat)
Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)
plot(Comp.ts, xlab = "Time / months", ylab = "Complaints")

Comp.hw1 <- HoltWinters(Comp.ts, beta = FALSE, gamma = FALSE); Comp.hw1
plot(Comp.hw1)

Comp.hw2 <- HoltWinters(Comp.ts, alpha = 0.2, beta = FALSE, gamma = FALSE)
Comp.hw2
plot(Comp.hw2)

#----3.4.2 Holt-Winters method
wine <- "wine.dat"
wine.dat <- read.table(wine, header = T) ; attach (wine.dat)
sweetw.ts <- ts(sweetw, start = c(1980,1), freq = 12)
plot(sweetw.ts, xlab= "Time (months)", ylab = "sales (1000 litres)")
sweetw.hw <- HoltWinters(sweetw.ts, seasonal = "mult")
sweetw.hw ; sweetw.hw$coef ; sweetw.hw$SSE

#----3.4.3 Four-year-ahead forecasts for the air passenger data----
AP.hw <- HoltWinters(AP, seasonal = "mult")
plot(AP.hw)

AP.predict <- predict(AP.hw, n.ahead = 4*12)
ts.plot(AP, AP.predict, lty = 1:2)

#--------3.6 Exercises-----------
#1.a 
w <- 1:100
k <- 100
x <- w + k * rnorm(100)
y <- w + k * rnorm(100)
ccf(x,y)

# for k = 1 & 10, there is a high crosscorrelation
#However, as K reach 100 the crosscorrelation fades out 

#1.b
Time <- 1:370
x <- sin(2*pi*Time/37)
y <- sin(2*pi*(Time+4)/37)
ccf(x,y)
plot(x,y)

#==============================Chapter 4 ===================================
#-----4.2.3 Simulation in R------

set.seed(1)
w <- rnorm(100)
plot(w,type = "l")

x <- seq(-3,3,length = 1000)
hist(rnorm(100), prob = TRUE); points(x,dnorm(x), type = "l")


set.seed(2)
acf(rnorm(100))



