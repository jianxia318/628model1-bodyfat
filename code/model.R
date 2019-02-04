dat=dat[,-2]#remove density

#use stepwise method
library(leaps)
steps <- regsubsets(BODYFAT~., data = dat, nvmax = 4,
                     method = "seqrep")
summary(steps)


#create accuracy matric
model=c()
R2=c()
#one predictor
step1<-lm(BODYFAT~ABDOMEN,data=dat)
summary(step1)
model <- c(model, "step1")


#two predictor
step2<-lm(BODYFAT~ABDOMEN+WEIGHT,data=dat)
summary(step2)
model <- c(model, "step2")

#three predictor
step3<-lm(BODYFAT~ABDOMEN+WEIGHT+WRIST,data=dat)
summary(step3)
model <- c(model, "step3")

#four predictor
step4<-lm(BODYFAT~ABDOMEN+WEIGHT+FOREARM+WRIST,data=dat)
summary(step4)
model <- c(model, "step4")

#test collinearity
library(carData)
library(car)
vif(step2)

#ridge regression
library(MASS)
ridge2=lm.ridge(BODYFAT~ABDOMEN+WEIGHT,data=dat,lambda = seq(0,10,0.1))
select(ridge2)
ridge2=lm.ridge(BODYFAT~ABDOMEN+WEIGHT,data=dat,lambda = 0.2)
pred.ridge2 <- coef(ridge2)[1] + coef(ridge2)[2]*dat$ABDOMEN + coef(ridge2)[3]*dat$WEIGHT
# calculate R squared
y=dat$BODYFAT
y_predicted=pred.ridge2
sst <- sum((y - mean(y))^2) #Sum of Squares Total
sse <- sum((y_predicted - y)^2) #SSE
rsq <- 1 - sse / sst # R squared
MSE=mean((y_predicted - y)^2)
#Diagnostic for ridge Model
res=y_predicted - y
shapiro.test(res)




#final model
fit=lm(BODYFAT~ABDOMEN+WEIGHT:WRIST,data=dat)
coefficients(fit)
WW=dat$WEIGHT*dat$WRIST
plot(BODYFAT~WEIGHT,data=dat)
plot(BODYFAT~WRIST,data=dat)
plot(BODYFAT~WW,data=dat)
