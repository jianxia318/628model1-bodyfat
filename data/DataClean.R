setwd("/Users/tyt/Documents/2019spring/stat628/module1")
mydat<-read.csv("BodyFat.csv",row.names=1)
dim(mydat)

### data cleaning ###
mydat$HEIGHT=mydat$HEIGHT*2.54 #inch to cm
mydat$WEIGHT=round(mydat$WEIGHT*0.453592,2) #lbs to kg
summary(mydat)#bodyfat=0, weight=164.72,height=74.93,age=81
mydat[mydat$AGE==81,]#a very thin old man
mydat[mydat$WEIGHT==164.72,]#reasonable with adiposity
mydat[mydat$HEIGHT==74.93,]
mydat$HEIGHT[42]=round(sqrt(92.99/29.9)*100,2)#bmi=weight/(height/100)^2
mydat=mydat[-182,]#can't be imputed by density,495/1.1089 - 450 less than zero

plot(mydat$BODYFAT~I(mydat$DENSITY^(-1)),xlim=c(0.90,1.01),ylab="Bodyfat",xlab="1/Density",col="skyblue",pch=19,cex=0.7,main="Bodyfat vs 1/Density")
x=seq(0.9,1.01,0.01)
lines(x,y=x*495-450,col="navyblue",lwd=1.2)
text(0.99,43,"y=495/x-450",col="navyblue",cex=1)
text(1/1.0991,18.3,"96",col="navyblue",cex=1)
text(1/1.0665,5.4,"48",col="navyblue",cex=1)
text(1/1.0666,19.3,"76",col="navyblue",cex=1)

res=mydat$BODYFAT-(495/mydat$DENSITY - 450)
sort(abs(res),decreasing=T)[1:10]
order(abs(res),decreasing=T)[1:3]
mydat[c(96,48,76),]
mydat$BODYFAT[48]=round(495/1.0665-450,1)

m_full<-lm(BODYFAT~.-DENSITY,data=mydat)
summary(m_full)
par(mfrow=c(2,2))
plot(m_full)
par(mfrow=c(1,1))
plot(m_full, which=4)#check cook's distance
abline( h = 4/(251-14),lty=2,col="red")
