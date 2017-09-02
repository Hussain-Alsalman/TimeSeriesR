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
