data=data[,-2]#remove density

#use stepwise method
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
summary(step1)
model <- c(model, "step1")


#two predictor
step2<-lm(BODYFAT~ABDOMEN+WEIGHT,data=data)
summary(step2)
model <- c(model, "step2")

#three predictor
step3<-lm(BODYFAT~ABDOMEN+WEIGHT+WRIST,data=data)
summary(step3)
model <- c(model, "step3")

#four predictor
step4<-lm(BODYFAT~ABDOMEN+WEIGHT+FOREARM+WRIST,data=data)
summary(step4)
model <- c(model, "step4")

#test colinearity
library(car)
vif(step4)

#ridge regression
library(MASS)
ridge2=lm.ridge(BODYFAT~ABDOMEN+WEIGHT,data=data,lambda = seq(0,10,0.1))
select(ridge2)
ridge2=lm.ridge(BODYFAT~ABDOMEN+WEIGHT,data=data,lambda = 0.2)
summary(ridge2)
pred.ridge2 <- coef(ridge2)[1] + coef(ridge2)[2]*data$ABDOMEN + coef(ridge2)[3]*data$WEIGHT
# Sum of Squares Total and Error
y=data$BODYFAT
y_predicted=pred.ridge2

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
# R squared
rsq <- 1 - sse / sst
rsq

