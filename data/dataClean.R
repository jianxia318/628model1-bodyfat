rm(list = ls())

data = read.csv("BodyFat.csv", row.names = 1)
summary(data)
#detect outlier 39,42,182 by 
which(data$WEIGHT==max(data$WEIGHT))#39
which(data$HEIGHT==min(data$HEIGHT))#42
which(data$BODYFAT==0)#182
#bmi=weight/(height)^2*703
data[39,]
weight39=48.9/703*72.25^2 #same as the record,keep 39
data[42,]
height42=sqrt(703*205/29.9) #different from the record, impute new height
data[182,]
bodyfat182=495/1.1089-450 #negative value, delete 182


#detect 48,76,96
plot(y = data$BODYFAT, x = 1 / data$DENSITY, ylab = "bodyfat percentage", xlab = "body density", 
     main = "BODYFAT vs. 1 / DENSITY")
text(0.91,20,"96",col = "blue")
text(0.94,9,"48",col = "blue")
text(0.94,20.5,"76",col = "blue")
#treat bodyfat calculator as reference https://www.active.com/fitness/calculators/bodyfat
data[48,]
bodyfat48=495/1.0665-450 #bodyfay wrong
data[96,]
bodyfat96=495/1.0991-450 #density wrong
data[76,]
bodyfat76=495/1.0666-450 #density wrong

#outlier process
data$HEIGHT[42]==height42
data$BODYFAT[48]==bodyfat48
data=data[-182,]


#change unit for weight,height to kg,cm
data$WEIGHT=0.45*data$WEIGHT
data$HEIGHT=2.54*data$HEIGHT