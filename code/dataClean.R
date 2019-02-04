rm(list = ls())
dat = read.csv("BodyFat.csv", row.names = 1)
summary(dat)
#detect outlier 39,42,182 by 
which(dat$WEIGHT==max(dat$WEIGHT))#39
which(dat$HEIGHT==min(dat$HEIGHT))#42
which(dat$BODYFAT==0)#182
#bmi=weight/(height)^2*703
dat[39,]
weight39=48.9/703*72.25^2 #same as the record,keep 39
dat[42,]
height42=sqrt(703*205/29.9) #different from the record, impute new height
dat[182,]
bodyfat182=495/1.1089-450 #negative value, delete 182


#detect 48,76,96
plot(y = dat$BODYFAT, x = 1 / dat$DENSITY, ylab = "bodyfat percentage", xlab = "body density", 
     main = "BODYFAT vs. 1 / DENSITY")
text(0.91,20,"96",col = "blue")
text(0.94,9,"48",col = "blue")
text(0.94,20.5,"76",col = "blue")
#treat bodyfat calculator as reference https://www.active.com/fitness/calculators/bodyfat
dat[48,]
bodyfat48=495/1.0665-450 #bodyfay wrong
dat[96,]
bodyfat96=495/1.0991-450 #density wrong
dat[76,]
bodyfat76=495/1.0666-450 #density wrong

#outlier process
dat$HEIGHT[42]=height42
dat$BODYFAT[48]=bodyfat48
dat=dat[-182,]


#change unit for weight,height to kg,cm
dat$WEIGHT=0.45*dat$WEIGHT
dat$HEIGHT=2.54*dat$HEIGHT
