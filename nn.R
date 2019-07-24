#install.packages("brnn")
setwd("/Users/kangzhao/Downloads/")
#setwd("~/Desktop/NN")
library('plyr')
library('Metrics')
library('brnn')
library("caret")

#train and test
raw_data <- read.csv("without heating.csv", header=T)
#raw 1-15: temperature data of sensor 1-15; raw 16-22: outdoor weather data
data <- raw_data[,c(1,16,17,18,19,20,21,22)]
data <-data[complete.cases(data[,1]),]
split <-createDataPartition(y=data$SENSOR_1, p=0.8, list=FALSE)
train <- data[split, ]
test <- data[-split, ]
nn1.7 <- brnn(SENSOR_1 ~ TEMP+WINS+DEWP+PREA+AIRP+CLOC+HUM,neurons=7,train)
p<-predict(nn1.7,test)
t<-test$SENSOR_25
mse(t,p)
rmse(t,p)
relation<-lm(p~t)
print(summary(relation))
nn


#k-cross validatation
cv.error <- NULL
k <- 10
for(i in 1:k){
  index <- sample(1:nrow(data),round(0.8*nrow(data)))
  train.cv <- data[index,]
  test.cv <- data[-index,]
  nn<-brnn(SENSOR_1 ~ TEMP+WINS+DEWP+PREA+AIRP+CLOC+HUM,neurons=7,data=train.cv)
  pr.nn<-predict(nn,test.cv)
  test.cv.r <- test.cv$SENSOR_1
  cv.error[i] <- mse(pr.nn,test.cv.r)
}
mean(cv.error)
cv.error
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',horizontal=TRUE)

