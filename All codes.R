#  1.4.1 A flying start: Air passenger bookings
data(AirPassengers)
AP <- AirPassengers
AP

class(AP)


plot(AP, ylab = "Passengers (1000's)")


layout(1:2)
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))

