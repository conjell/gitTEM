library('ISLR')
library('leaps')
library('plyr')
library('Metrics')
library('MASS')
library('Iso')
library('car')

setwd("/Users/kangzhao/Downloads/")
raw_data <- read.csv("without.csv", header=T)
data <- raw_data[,c(2,3,15,16,17,18,19,20,21,22,23)]
data <-data[complete.cases(data[,2]),]
data<-data[data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ]
data<-data[data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ]
data<-data[data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ]
data<-data[data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ]

set.seed(1)
n=7
train<-sample(c(T,F),nrow(data),rep=T)
test<-!train
#full parameter: TEM+WIN+PRE+AIR+CLO+HUM+RAI+THU
regfit.best<-regsubsets(SEN23 ~ TEM+WIN+PRE+AIR+CLO+HUM+RAI+THU,data=data[train,],nvmax=n)
test.mat<-model.matrix(SEN23 ~ TEM+WIN+PRE+AIR+CLO+HUM+RAI+THU,data=data[test,])
val.errors<-rep(NA,n)
for(i in 1:n){
  coefi<-coef(regfit.best,i)
  pred<-test.mat[,names(coefi)]%*%coefi
  val.errors[i]<-mean((data$SEN23[test]-pred)^2)
}
which.min(val.errors)
coef(regfit.best,1)


k = 10
set.seed(1)
folds = sample(1:k,nrow(data),replace = T)
table(folds)
cv.errors = matrix(NA,k,n,dimnames = list(NULL,paste(1:n)))
for(j in 1:k){
  best.fit<-regsubsets(SEN23 ~ TEM+WIN+PRE+AIR+CLO+HUM+RAI+THU,data=data[folds!=j,],nvmax=n)
  test.mat<-model.matrix(SEN23 ~ TEM+WIN+PRE+AIR+CLO+HUM+RAI+THU,data=data[folds==j,])
  for(i in 1:n){
    coefi<-coef(regfit.best,i)
    pred<-test.mat[,names(coefi)]%*%coefi
    cv.errors[j,i] = mean((data$SEN23[folds==j]-pred)^2)
  }
} 
cv.errors
mean.cv.errors<-apply(cv.errors,2,mean)
which.min(mean.cv.errors)
reg.best<-regsubsets(SEN23 ~ TEM+WIN+PRE+AIR+CLO+HUM+RAI+THU,data=data,nvmax=n)
coef(reg.best,4)
plot(mean.cv.errors,type = "b")

#full parameter: TEM+WIN+PRE+AIR+CLO+HUM+RAI+THU
a<-lm(SEN1 ~  TEM+WIN+AIR+CLO+HUM+RAI+THU, data=data);
summary(a)

