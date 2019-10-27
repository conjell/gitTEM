#install.packages("brnn")
#document location
setwd("/Users/kangzhao/Downloads/")
#setwd("~/Desktop/NN")
library('plyr')
library('Metrics')
library('brnn')
library("caret")

#train and test
setwd("/Users/kangzhao/Downloads/")
raw_data <- read.csv("with.csv", header=T)
data <- raw_data[,c(2,12,15,16,17,18,19,20,21,22,23)]
data <-data[complete.cases(data[,2]),]
data<-data[data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ]
data<-data[data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ]
data<-data[data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ]
data<-data[data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ]


#raw 1-15: temperature data of sensor 1-15; raw 16-22: outdoor weather data
n=17
split <-createDataPartition(y=data$SEN21, p=0.8, list=FALSE)
train <- data[split, ]
test <- data[-split, ]
#neurons=number of hidden layers
nw21.17.5 <- brnn(SEN21 ~ TEM+WIN+AIR+CLO+HUM+RAI,neurons=n,train)
p<-predict(nw21.17.5,test)
t<-test$SEN21
rmse(t,p) 

#k-cross validatation
cv.error <- NULL
k <- 10
for(i in 1:k){
  index <- sample(1:nrow(data),round(0.8*nrow(data)))
  train.cv <- data[index,]
  test.cv <- data[-index,]
  nn<-brnn(SEN21 ~ TEM+WIN+AIR+CLO+HUM+RAI,neurons=n,data=train.cv)
  pr.nn<-predict(nn,test.cv)
  test.cv.r <- test.cv$SEN21
  cv.error[i] <- rmse(pr.nn,test.cv.r)
}
mean(cv.error)
cv.error
boxplot(cv.error,xlab='RMSE CV',col='cyan',
        border='blue',names='CV error (RMSE)',horizontal=TRUE)

