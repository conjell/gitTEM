setwd("~/Desktop/")
#install.packages('brnn')
library('brnn')
raw_data <- read.csv("samplew.csv", header=T)
raw_data <- read.csv("samplec.csv", header=T)
raw_data <- read.csv("samplewx.csv", header=T)
raw_data <- read.csv("samplecx.csv", header=T)
###temperature prediction for all sensors in future 72 hours
##without heating peridod (May to August)
{#sensor1  
SEN1.2 <- predict(nn1.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
Y2 <- c(1:6,25:30,49:54)
aa <- cbind(Y2,SEN1.2)
SEN1.3 <- predict(nn1.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
Y3 <- c(7:12,31:36,55:60)
bb <- cbind(Y3,SEN1.3)
SEN1.4 <- predict(nn1.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
Y4 <- c(13:18,37:42,61:66)
cc <- cbind(Y4,SEN1.4)
SEN1.5 <- predict(nn1.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Y5 <- c(19:24,43:48,67:72)
dd <- cbind(Y5,SEN1.5)
Total1 <- rbind(aa,bb,cc,dd)

#sensor2
SEN2.2 <- predict(nn2.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN2.3 <- predict(nn2.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN2.4 <- predict(nn2.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN2.5 <- predict(nn2.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total2 <- t(cbind(t(SEN2.2),t(SEN2.3),t(SEN2.4),t(SEN2.5)))

#sensor3
SEN3.2 <- predict(nn3.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN3.3 <- predict(nn3.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN3.4 <- predict(nn3.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN3.5 <- predict(nn3.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total3 <- t(cbind(t(SEN3.2),t(SEN3.3),t(SEN3.4),t(SEN3.5)))

#sensor5
SEN5.2 <- predict(nn5.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN5.3 <- predict(nn5.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN5.4 <- predict(nn5.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN5.5 <- predict(nn5.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total5 <- t(cbind(t(SEN5.2),t(SEN5.3),t(SEN5.4),t(SEN5.5)))

#sensor15
SEN15.2 <- predict(nn15.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN15.3 <- predict(nn15.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN15.4 <- predict(nn15.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN15.5 <- predict(nn15.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total15 <- t(cbind(t(SEN15.2),t(SEN15.3),t(SEN15.4),t(SEN15.5)))

#sensor16
SEN16.2 <- predict(nn16.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN16.3 <- predict(nn16.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN16.4 <- predict(nn16.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN16.5 <- predict(nn16.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total16 <- t(cbind(t(SEN16.2),t(SEN16.3),t(SEN16.4),t(SEN16.5)))

#sensor18
SEN18.2 <- predict(nn18.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN18.3 <- predict(nn18.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN18.4 <- predict(nn18.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN18.5 <- predict(nn18.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total18 <- t(cbind(t(SEN18.2),t(SEN18.3),t(SEN18.4),t(SEN18.5)))

#sensor19
SEN19.2 <- predict(nn19.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN19.3 <- predict(nn19.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN19.4 <- predict(nn19.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN19.5 <- predict(nn19.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total19 <- t(cbind(t(SEN19.2),t(SEN19.3),t(SEN19.4),t(SEN19.5)))

#sensor20
SEN20.2 <- predict(nn20.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN20.3 <- predict(nn20.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN20.4 <- predict(nn20.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN20.5 <- predict(nn20.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total20 <- t(cbind(t(SEN20.2),t(SEN20.3),t(SEN20.4),t(SEN20.5)))

#sensor21
SEN21.2 <- predict(nn21.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN21.3 <- predict(nn21.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN21.4 <- predict(nn21.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN21.5 <- predict(nn21.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total21 <- t(cbind(t(SEN21.2),t(SEN21.3),t(SEN21.4),t(SEN21.5)))

#sensor22
SEN22.2 <- predict(nn22.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN22.3 <- predict(nn22.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN22.4 <- predict(nn22.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN22.5 <- predict(nn22.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total22 <- t(cbind(t(SEN22.2),t(SEN22.3),t(SEN22.4),t(SEN22.5)))

#sensor23
SEN23.2 <- predict(nn23.4.2,raw_data[raw_data$X.1%in% c('1:00:00','2:00:00','3:00:00','4:00:00','5:00:00','6:00:00'), ])
SEN23.3 <- predict(nn23.4.3,raw_data[raw_data$X.1%in% c('7:00:00','8:00:00','9:00:00','10:00:00','11:00:00','12:00:00'), ])
SEN23.4 <- predict(nn23.4.4,raw_data[raw_data$X.1%in% c('13:00:00','14:00:00','15:00:00','16:00:00','17:00:00','18:00:00'), ])
SEN23.5 <- predict(nn23.4.5,raw_data[raw_data$X.1%in% c('19:00:00','20:00:00','21:00:00','22:00:00','23:00:00','0:00:00'), ])
Total23 <- t(cbind(t(SEN23.2),t(SEN23.3),t(SEN23.4),t(SEN23.5)))
}

FinalT <- cbind(Total1,Total2,Total3,Total5,Total15,Total16,Total18,Total19,Total20,Total21,Total22,Total23)
FinalT <- FinalT[order(FinalT[,1]),] 
colnames(FinalT) <- c("TIME","SEN1","SEN2","SEN3","SEN5","SEN15","SEN16","SEN18","SEN19","SEN20","SEN21","SEN22","SEN23") 

#with heating period (September to April)
{
  average=cbind(FinalT[,1],rowMeans(FinalT[,2:13]))
#th: the critical temperature at which the heater is turned on
th <- 16
for (i in Y2){
  if (average[i,2]<th){
    FinalT[i,2]=predict(nw1.8.2,raw_data[i,3:10])
    FinalT[i,3]=predict(nw2.8.2,raw_data[i,3:10])
    FinalT[i,4]=predict(nw3.8.2,raw_data[i,3:10])
    FinalT[i,5]=predict(nw5.8.2,raw_data[i,3:10])
    FinalT[i,6]=predict(nw15.8.2,raw_data[i,3:10])
    FinalT[i,7]=predict(nw16.8.2,raw_data[i,3:10])
    FinalT[i,8]=predict(nw18.8.2,raw_data[i,3:10])
    FinalT[i,9]=predict(nw19.8.2,raw_data[i,3:10])
    FinalT[i,10]=predict(nw20.8.2,raw_data[i,3:10])
    FinalT[i,11]=predict(nw21.8.2,raw_data[i,3:10])
    FinalT[i,12]=predict(nw22.8.2,raw_data[i,3:10])
    FinalT[i,13]=predict(nw23.8.2,raw_data[i,3:10])
    }
}

for (i in Y3){
  if (average[i,2]<th){
    FinalT[i,2]=predict(nw1.8.3,raw_data[i,3:10])
    FinalT[i,3]=predict(nw2.8.3,raw_data[i,3:10])
    FinalT[i,4]=predict(nw3.8.3,raw_data[i,3:10])
    FinalT[i,5]=predict(nw5.8.3,raw_data[i,3:10])
    FinalT[i,6]=predict(nw15.8.3,raw_data[i,3:10])
    FinalT[i,7]=predict(nw16.8.3,raw_data[i,3:10])
    FinalT[i,8]=predict(nw18.8.3,raw_data[i,3:10])
    FinalT[i,9]=predict(nw19.8.3,raw_data[i,3:10])
    FinalT[i,10]=predict(nw20.8.3,raw_data[i,3:10])
    FinalT[i,11]=predict(nw21.8.3,raw_data[i,3:10])
    FinalT[i,12]=predict(nw22.8.2,raw_data[i,3:10])
    FinalT[i,13]=predict(nw23.8.2,raw_data[i,3:10])
  }
}

for (i in Y4){
  if (average[i,2]<th){
    FinalT[i,2]=predict(nw1.8.4,raw_data[i,3:10])
    FinalT[i,3]=predict(nw2.8.4,raw_data[i,3:10])
    FinalT[i,4]=predict(nw3.8.4,raw_data[i,3:10])
    FinalT[i,5]=predict(nw5.8.4,raw_data[i,3:10])
    FinalT[i,6]=predict(nw15.8.4,raw_data[i,3:10])
    FinalT[i,7]=predict(nw16.8.4,raw_data[i,3:10])
    FinalT[i,8]=predict(nw18.8.4,raw_data[i,3:10])
    FinalT[i,9]=predict(nw19.8.4,raw_data[i,3:10])
    FinalT[i,10]=predict(nw20.8.4,raw_data[i,3:10])
    FinalT[i,11]=predict(nw21.8.4,raw_data[i,3:10])
    FinalT[i,12]=predict(nw22.8.4,raw_data[i,3:10])
    FinalT[i,13]=predict(nw23.8.4,raw_data[i,3:10])
  }
}

for (i in Y5){
  if (average[i,2]<th){
    FinalT[i,2]=predict(nw1.8.5,raw_data[i,3:10])
    FinalT[i,3]=predict(nw2.8.5,raw_data[i,3:10])
    FinalT[i,4]=predict(nw3.8.5,raw_data[i,3:10])
    FinalT[i,5]=predict(nw5.8.5,raw_data[i,3:10])
    FinalT[i,6]=predict(nw15.8.5,raw_data[i,3:10])
    FinalT[i,7]=predict(nw16.8.5,raw_data[i,3:10])
    FinalT[i,8]=predict(nw18.8.5,raw_data[i,3:10])
    FinalT[i,9]=predict(nw19.8.5,raw_data[i,3:10])
    FinalT[i,10]=predict(nw20.8.5,raw_data[i,3:10])
    FinalT[i,11]=predict(nw21.8.5,raw_data[i,3:10])
    FinalT[i,12]=predict(nw22.8.5,raw_data[i,3:10])
    FinalT[i,13]=predict(nw23.8.5,raw_data[i,3:10])
  }
}
}

###Product information
##Pharma
pharma <- function(t1,t2,maxtem,value){
  temd<-FinalT[,2:13]-maxtem
  for (i in 1: nrow(temd)){
    for (j in 2:ncol(temd)){
      if (temd[i,j] < 0){
        temd[i,j]=0}
    }
  }
  #Q10=2:optimistic evaluation; Q10=3:stable evaluation; Q10=4:pessimistic evaluation.
  Q10=4
  shelflife=4320
  shelflife1=shelflife/(Q10^(temd/10))
  shelflifeloss=shelflife-shelflife1
  houlysloss=shelflifeloss/shelflife
  totalloss=colSums(houlysloss[t1:t2,])
  costfloss=(value/(4320^2))*(totalloss^2)
  return(costfloss)
}

##Floral
floral <- function(t1,t2,maxtem,value){
  temd<-FinalT[,2:13]-maxtem
  for (i in 1: nrow(temd)){
    for (j in 2:ncol(temd)){
      if (temd[i,j] < 0){
        temd[i,j]=0
      }
      if(temd[i,j] > 15){
        temd[i,j]=15 }
    }
  }
  vaselife=370.13-65.91*exp(0.1092*temd)
  vaselifeloss=304.22-vaselife
  hourlyfloss=vaselifeloss/vaselife
  totalloss=colSums(hourlyfloss[t1:t2,])
  costfloss=(value/(304.22^2))*(totalloss^2)
  return(costfloss)
}


##location choice based on storage duration
shipment<-read.csv("shipment.csv", header=T)
finallosst<-c()
rrab<-matrix(nrow=100,ncol=10000)
for (k in 1:10000){
  set.seed(k)
  range=1:70
  pro <- c("Flora","Pharma")
  opp <- c("COL","CRT","PIL")
  opf<-c("COL","PIL")
  for (p in 1:100){
    shipment[p,1]=sample(pro,1,replace = TRUE,prob = c(0.576,0.424))
    shipment[p,2]=sample(range,1,replace = TRUE)
    to<-as.numeric(shipment[p,2])+1
    too<-as.numeric(shipment[p,2])+9
    if (shipment[p,1] == "Pharma"){
      shipment[p,7]=sample(opp,1,replace = TRUE)
      if (shipment[p,7] == "COL"){
        shipment[p,4]=8
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "CRT"){
        shipment[p,4]=25
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "PIL"){
        shipment[p,4]=25
        shipment[p,3]=sample(to:72,1,replace = TRUE)}
    }
    if (shipment[p,1] == "Flora"){
      shipment[p,7]=sample(opf,1,replace = TRUE)
      if (shipment[p,7] == "COL"){
        shipment[p,4]=8
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "PIL"){
        shipment[p,4]=25
        shipment[p,3]=sample(to:72,1,replace = TRUE)}
    }
    shipment[p,6]=sample(1:5,1,replace = TRUE)
    shipment[p,8]=shipment[p,3]-shipment[p,2]
    if(shipment[p,1] == 'Pharma'){
      shipment[p,5]=sample(40624.8759:136355.5350,1,replace = TRUE)
    }else{
      shipment[p,5]=sample(3118.9354:7188.9507,1,replace = TRUE)
    }
  }
  colnames(shipment) <- c("Type","Time.in","Time.out","Optimal","Value","Amount","Category","V8") 
  
  shipment <- shipment[order(-shipment$V8,shipment$Time.in),]
  
  a<-function(i){
    pharma(as.numeric(shipment[i,2]),as.numeric(shipment[i,3]),
           as.numeric(shipment[i,4]),as.numeric(shipment[i,5]))
  }
  b<-function(i){
    floral(as.numeric(shipment[i,2]),as.numeric(shipment[i,3]),
           as.numeric(shipment[i,4]),as.numeric(shipment[i,5]))
  }
  
  y<-ifelse(shipment$Type != 'Pharma',0,1)
  y1<-ifelse(shipment$Type != 'Flora',0,1)
  y1
  
  c<-c()
  for (i in 1:100){
    x<-a(i)*y[i]
    c<-rbind(c,x)
  }
  
  m<-c()
  for (i in 1:100){
    x<-b(i)*y1[i]
    m<-rbind(m,x)
  }
  
  capacity<-c(0,120,120,120,0,0,0,0,0,0,0,0) 
  capacity
  amount<-as.numeric(shipment$Amount)
  
  n<-m+c
  n<-t(n)
  colnames <- c()
  for (i in 1:100){
    name<- paste('product',i)
    colnames<-cbind(colnames,name)
  }
  colnames(n)<-colnames
  
  n<-cbind(n,capacity)
  
  ab<-c()
  for(j in 1:100){
    n<-n[order(n[,j]),]
    n1<-n[order(n[,j]),]
    for (i in 1:12){
      if(n[i,ncol(n)] >= amount[j]){
        n[i,ncol(n)]<-n[i,ncol(n)]-amount[j]
        break;}
    }
    nnn<-n1-n
    x<-which(nnn==max(nnn),arr.ind=T)
    ab<-rbind(ab,x)
  }
  rab<-rownames(ab)
  rab<-matrix(rab)
  rrab[1:100,k]<-rab
  
  locationchoice <- cbind(shipment,rab)
  names(locationchoice)[7]<-c("location")
  locationchoice
  finallosst[k]<-0
  for(i in 1:100){
    loss<-n[which(rownames(n)==rab[i]),i]
    finallosst[k]<-finallosst[k]+loss
  }
} 
hist(finallosst)
finallosst<-matrix(finallosst)

choice<-t(rrab)
percentage<-matrix(nrow=12,ncol=1)
rr<-1:12
for (r in rr){
  name<-colnames(FinalT)
  name<-name[r+1]
  percentage[r,]=sum(choice==name)/(10000*100)
}
rrr<-colnames(FinalT)
rrr<-matrix(rrr)
rrr<-rrr[-1,]
percentage<-cbind(rrr,percentage)

write.table (finallosst, file ="finalloss-w1.csv",sep =",",row.names =FALSE)
write.table (percentage, file ="choice-w1.csv",sep =",",row.names =FALSE)


##location choice based on time in
shipment<-read.csv("shipment.csv", header=T)
finallosst<-c()
rrab<-matrix(nrow=100,ncol=10000)
for (k in 1:10000){
  set.seed(k)
  range=1:70
  pro <- c("Flora","Pharma")
  opp <- c("COL","CRT","PIL")
  opf<-c("COL","PIL")
  for (p in 1:100){
    shipment[p,1]=sample(pro,1,replace = TRUE,prob = c(0.576,0.424))
    shipment[p,2]=sample(range,1,replace = TRUE)
    to<-as.numeric(shipment[p,2])+1
    too<-as.numeric(shipment[p,2])+9
    if (shipment[p,1] == "Pharma"){
      shipment[p,7]=sample(opp,1,replace = TRUE)
      if (shipment[p,7] == "COL"){
        shipment[p,4]=8
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "CRT"){
        shipment[p,4]=25
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "PIL"){
        shipment[p,4]=25
        shipment[p,3]=sample(to:72,1,replace = TRUE)}
    }
    if (shipment[p,1] == "Flora"){
      shipment[p,7]=sample(opf,1,replace = TRUE)
      if (shipment[p,7] == "COL"){
        shipment[p,4]=8
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "PIL"){
        shipment[p,4]=25
        shipment[p,3]=sample(to:72,1,replace = TRUE)}
    }
    shipment[p,6]=sample(1:5,1,replace = TRUE)
    shipment[p,8]=shipment[p,3]-shipment[p,2]
    if(shipment[p,1] == 'Pharma'){
    shipment[p,5]=sample(40624.8759:136355.5350,1,replace = TRUE)
    }else{
      shipment[p,5]=sample(3118.9354:7188.9507,1,replace = TRUE)
    }
      }
colnames(shipment) <- c("Type","Time.in","Time.out","Optimal","Value","Amount","Category") 

shipment <- shipment[order(shipment$Time.in,-shipment$Time.out),]

a<-function(i){
  pharma(as.numeric(shipment[i,2]),as.numeric(shipment[i,3]),
         as.numeric(shipment[i,4]),as.numeric(shipment[i,5]))
}
b<-function(i){
  floral(as.numeric(shipment[i,2]),as.numeric(shipment[i,3]),
          as.numeric(shipment[i,4]),as.numeric(shipment[i,5]))
}

y<-ifelse(shipment$Type != 'Pharma',0,1)
y1<-ifelse(shipment$Type != 'Flora',0,1)
y1

c<-c()
for (i in 1:100){
  x<-a(i)*y[i]
  c<-rbind(c,x)
}

m<-c()
for (i in 1:100){
  x<-b(i)*y1[i]
  m<-rbind(m,x)
}

capacity<-c(0,120,120,120,0,0,0,0,0,0,0,0) 
capacity
amount<-as.numeric(shipment$Amount)

n<-m+c
n<-t(n)
colnames <- c()
for (i in 1:100){
  name<- paste('product',i)
  colnames<-cbind(colnames,name)
}
colnames(n)<-colnames

n<-cbind(n,capacity)

ab<-c()
for(j in 1:100){
  n<-n[order(n[,j]),]
  n1<-n[order(n[,j]),]
  for (i in 1:12){
    if(n[i,ncol(n)] >= amount[j]){
      n[i,ncol(n)]<-n[i,ncol(n)]-amount[j]
      break;}
  }
  nnn<-n1-n
  x<-which(nnn==max(nnn),arr.ind=T)
  ab<-rbind(ab,x)
}
rab<-rownames(ab)
rab<-matrix(rab)
rrab[1:100,k]<-rab

locationchoice <- cbind(shipment,rab)
names(locationchoice)[7]<-c("location")
locationchoice
finallosst[k]<-0
for(i in 1:100){
  loss<-n[which(rownames(n)==rab[i]),i]
  finallosst[k]<-finallosst[k]+loss
  }
} 
hist(finallosst)
finallosst<-matrix(finallosst)

choice<-t(rrab)
percentage<-matrix(nrow=12,ncol=1)
rr<-1:12
for (r in rr){
  name<-colnames(FinalT)
  name<-name[r+1]
  percentage[r,]=sum(choice==name)/(10000*100)
}
rrr<-colnames(FinalT)
rrr<-matrix(rrr)
rrr<-rrr[-1,]
percentage<-cbind(rrr,percentage)

write.table (finallosst, file ="finalloss-w1.csv",sep =",",row.names =FALSE)
write.table (percentage, file ="choice-w1.csv",sep =",",row.names =FALSE)


##location choice based on average loss
finallossa<-c()
rrab2<-matrix(nrow=100,ncol=10000)
for (k in 1:10000){
  set.seed(k)
  range=1:70
  pro <- c("Flora","Pharma")
  opp <- c("COL","CRT","PIL")
  opf<-c("COL","PIL")
  for (p in 1:100){
    shipment[p,1]=sample(pro,1,replace = TRUE,prob = c(0.576,0.424))
    shipment[p,2]=sample(range,1,replace = TRUE)
    to<-as.numeric(shipment[p,2])+1
    too<-as.numeric(shipment[p,2])+9
    if (shipment[p,1] == "Pharma"){
      shipment[p,7]=sample(opp,1,replace = TRUE)
      if (shipment[p,7] == "COL"){
        shipment[p,4]=8
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "CRT"){
        shipment[p,4]=25
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "PIL"){
        shipment[p,4]=25
        shipment[p,3]=sample(to:72,1,replace = TRUE)}
    }
    if (shipment[p,1] == "Flora"){
      shipment[p,7]=sample(opf,1,replace = TRUE)
      if (shipment[p,7] == "COL"){
        shipment[p,4]=8
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "PIL"){
        shipment[p,4]=25
        shipment[p,3]=sample(to:72,1,replace = TRUE)}
    }
    shipment[p,6]=sample(1:5,1,replace = TRUE)
    if(shipment[p,1] == 'Pharma'){
      shipment[p,5]=sample(40624.8759:136355.5350,1,replace = TRUE)
    }else{
      shipment[p,5]=sample(3118.9354:7188.9507,1,replace = TRUE)
    }
  }
  colnames(shipment) <- c("Type","Time.in","Time.out","Optimal","Value","Amount","Category") 
  
  a<-function(i){
    pharma(as.numeric(shipment[i,2]),as.numeric(shipment[i,3]),
           as.numeric(shipment[i,4]),as.numeric(shipment[i,5]))
  }
  b<-function(i){
    floral(as.numeric(shipment[i,2]),as.numeric(shipment[i,3]),
           as.numeric(shipment[i,4]),as.numeric(shipment[i,5]))
  }
  
  y<-ifelse(shipment$Type != 'Pharma',0,1)
  y1<-ifelse(shipment$Type != 'Flora',0,1)
  y1
  
  c<-c()
  for (i in 1:100){
    x<-a(i)*y[i]
    c<-rbind(c,x)
  }
  
  m<-c()
  for (i in 1:100){
    x<-b(i)*y1[i]
    m<-rbind(m,x)
  }
  
  capacity<-c(0,120,120,120,0,0,0,0,0,0,0,0) 
  capacity
  amount<-as.numeric(shipment$Amount)
  
  n<-m+c
  n<-t(n)
  colnames <- c()
  for (i in 1:100){
    name<- paste('product',i)
    colnames<-cbind(colnames,name)
  }
  colnames(n)<-colnames

  mean<-colMeans(n)
  meann<-rev(order(mean))
  meannn<-rev(sort(mean))
  n<-n[,meann]
  n<-cbind(n,capacity)
  
  ab<-c()
  for(j in 1:100){
    n<-n[order(n[,j]),]
    n1<-n[order(n[,j]),]
    for (i in 1:12){
      if(n[i,ncol(n)] >= amount[j]){
        n[i,ncol(n)]<-n[i,ncol(n)]-amount[j]
        break;}
    }
    nnn<-n1-n
    x<-which(nnn==max(nnn),arr.ind=T)
    ab<-rbind(ab,x)
  }
  rab<-rownames(ab)
  rab<-matrix(rab)
  rrab2[1:100,k]=rab
  locationchoice <- cbind(meannn,rownames(ab))
  locationchoice <- locationchoice[,-1]
  finallossa[k]<-0
  for(i in 1:100){
    loss<-n[which(rownames(n)==rab[i]),i]
    finallossa[k]<-finallossa[k]+loss
  } 
}
hist(finallossa)
finallossa<-matrix(finallossa)

choice2<-t(rrab2)
percentage2<-matrix(nrow=12,ncol=1)
rr<-1:12
for (r in rr){
  name<-colnames(FinalT)
  name<-name[r+1]
  percentage2[r,]=sum(choice2==name)/(10000*100)
}

write.table (finallossa, file ="212.csv",sep =",",row.names =FALSE)
write.table (percentage2, file ="schoice-w2.csv",sep =",",row.names =FALSE)


##location choice based on std
finallosss<-c()
rrab3<-matrix(nrow=100,ncol=10000)
for (k in 1:10000){
  set.seed(k)
  range=1:70
  pro <- c("Flora","Pharma")
  opp <- c("COL","CRT","PIL")
  opf<-c("COL","PIL")
  for (p in 1:100){
    shipment[p,1]=sample(pro,1,replace = TRUE,prob = c(0.576,0.424))
    shipment[p,2]=sample(range,1,replace = TRUE)
    to<-as.numeric(shipment[p,2])+1
    too<-as.numeric(shipment[p,2])+9
    if (shipment[p,1] == "Pharma"){
      shipment[p,7]=sample(opp,1,replace = TRUE)
      if (shipment[p,7] == "COL"){
        shipment[p,4]=8
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "CRT"){
        shipment[p,4]=25
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "PIL"){
        shipment[p,4]=25
        shipment[p,3]=sample(to:72,1,replace = TRUE)}
    }
    if (shipment[p,1] == "Flora"){
      shipment[p,7]=sample(opf,1,replace = TRUE)
      if (shipment[p,7] == "COL"){
        shipment[p,4]=8
        if(shipment[p,2]<63){
          shipment[p,3]=sample(to:too,1,replace = TRUE)}
        else{shipment[p,3]=sample(to:72,1,replace = TRUE)}
      }
      if(shipment[p,7] == "PIL"){
        shipment[p,4]=25
        shipment[p,3]=sample(to:72,1,replace = TRUE)}
    }
    shipment[p,6]=sample(1:5,1,replace = TRUE)
    if(shipment[p,1] == 'Pharma'){
      shipment[p,5]=sample(40624.8759:136355.5350,1,replace = TRUE)
    }else{
      shipment[p,5]=sample(3118.9354:7188.9507,1,replace = TRUE)
    }
  }
  colnames(shipment) <- c("Type","Time.in","Time.out","Optimal","Value","Amount","Category") 
  
  a<-function(i){
    pharma(as.numeric(shipment[i,2]),as.numeric(shipment[i,3]),
           as.numeric(shipment[i,4]),as.numeric(shipment[i,5]))
  }
  b<-function(i){
    floral(as.numeric(shipment[i,2]),as.numeric(shipment[i,3]),
           as.numeric(shipment[i,4]),as.numeric(shipment[i,5]))
  }
  
  y<-ifelse(shipment$Type != 'Pharma',0,1)
  y1<-ifelse(shipment$Type != 'Flora',0,1)
  y1
  
  c<-c()
  for (i in 1:100){
    x<-a(i)*y[i]
    c<-rbind(c,x)
  }
  
  m<-c()
  for (i in 1:100){
    x<-b(i)*y1[i]
    m<-rbind(m,x)
  }
  
  capacity<-c(0,120,120,120,0,0,0,0,0,0,0,0) 
  capacity
  amount<-as.numeric(shipment$Amount)
  
  n<-m+c
  n<-t(n)
  colnames <- c()
  for (i in 1:100){
    name<- paste('product',i)
    colnames<-cbind(colnames,name)
  }
  colnames(n)<-colnames 
  
  std<-c()
  for (i in 1:100){
    namee<- sd(n[1:12,i])
    std<-cbind(std,namee)
  }
  
  stdd<-rev(order(std))
  stddd<-rev(sort(std))
  n<-n[,stdd]
  n<-cbind(n,capacity)
  
  ab<-c()
  for(j in 1:100){
    n<-n[order(n[,j]),]
    n1<-n[order(n[,j]),]
    for (i in 1:12){
      if(n[i,ncol(n)] >= amount[j]){
        n[i,ncol(n)]<-n[i,ncol(n)]-amount[j]
        break;}
    }
    nnn<-n1-n
    x<-which(nnn==max(nnn),arr.ind=T)
    ab<-rbind(ab,x)
  }
  rab<-rownames(ab)
  rab<-matrix(rab)
  rrab3[1:100,k]=rab
  locationchoice <- cbind(stddd,rownames(ab))
  locationchoice <- locationchoice[,-1]
  finallosss[k]<-0
  for(i in 1:100){
    loss<-n[which(rownames(n)==rab[i]),i]
    finallosss[k]<-finallosss[k]+loss
  }
}
hist(finallosss)
finallosss<-matrix(finallosss)

choice3<-t(rrab3)
percentage3<-matrix(nrow=12,ncol=1)
rr<-1:12
for (r in rr){
  name<-colnames(FinalT)
  name<-name[r+1]
  percentage3[r,]=sum(choice3==name)/(10000*100)
}

write.table (finallosss, file ="finalloss-wx3.csv",sep =",",row.names =FALSE)
write.table (percentage3, file ="choice-wx3.csv",sep =",",row.names =FALSE)

finalloss<-cbind(finallosst,finallossa,finallosss)

percentagef<-cbind(percentage,percentage2,percentage3)
mean(finallosst)
mean(finallossa)
mean(finallosss)

write.table (finallossa, file ="finallossabc.csv",sep =",",row.names =FALSE)
write.table (percentage3, file ="choice-wx3.csv",sep =",",row.names =FALSE)

