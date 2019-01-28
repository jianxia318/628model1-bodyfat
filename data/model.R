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

#train-test split
smp_size <- floor(0.9 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]


library(leaps)
steps <- regsubsets(BODYFAT~., data = train, nvmax = 4,
                     method = "seqrep")
summary(steps)
coef(models,1:4)

#create accuracy matric
model=c()
accuracy=c()
#one predictor
step1<-lm(BODYFAT~ABDOMEN,data=train)
prediction=predict.lm(step1,test)
MSE=sum((test$BODYFAT-prediction)^2)#calculate prediction accuracy
model <- c(model, "step1")
accuracy<- c(accuracy,MSE)

#two predictor
step2<-lm(BODYFAT~ABDOMEN+WEIGHT,data=train)
prediction=predict.lm(step2,test)
MSE=sum((test$BODYFAT-prediction)^2)#calculate prediction accuracy
model <- c(model, "step2")
accuracy<- c(accuracy,MSE)

#three predictor
step3<-lm(BODYFAT~ABDOMEN+WEIGHT+WRIST,data=train)
prediction=predict.lm(step3,test)
MSE=sum((test$BODYFAT-prediction)^2)#calculate prediction accuracy
model <- c(model, "step3")
accuracy<- c(accuracy,MSE)

#four predictor
step4<-lm(BODYFAT~ABDOMEN+AGE+HEIGHT+WRIST,data=train)
prediction=predict.lm(step4,test)
MSE=sum((test$BODYFAT-prediction)^2)#calculate prediction accuracy
model <- c(model, "step4")
accuracy<- c(accuracy,MSE)

#test colinearity
library(car)
vif(step4)


