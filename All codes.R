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
?aggregate()
