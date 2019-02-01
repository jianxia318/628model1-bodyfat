rm(list = ls())
data = read.csv("BodyFat.csv", row.names = 1)
data = data[, -2]
#detect outlier 39,42 by 
pre1=lm(BODYFAT ~ ., data = data)
plot(pre1,which=4)
par(mfrow=c(2,2))
plot(pre1)
par(mfrow=c(1,1))
data = data[c(-39, -42, -48, -96, -76, -182), ]


library(leaps)
steps <- regsubsets(BODYFAT~., data = data, nvmax = 4,
                     method = "seqrep")
summary(steps)
coef(models,1:4)

#create accuracy matric
model=c()
accuracy=c()
#one predictor
step1<-lm(BODYFAT~ABDOMEN,data=data)
model <- c(model, "step1")


#two predictor
step2<-lm(BODYFAT~ABDOMEN+WEIGHT,data=data)
model <- c(model, "step2")

#three predictor
step3<-lm(BODYFAT~ABDOMEN+WEIGHT+WRIST,data=data)
model <- c(model, "step3")

#four predictor
step4<-lm(BODYFAT~ABDOMEN+AGE+HEIGHT+WRIST,data=data)
model <- c(model, "step4")

#test colinearity
library(car)
vif(step4)


