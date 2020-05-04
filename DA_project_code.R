rm(list=ls())
library(ggplot2)
setwd("C:\\Users\\Lenovo\\Desktop\\homework&slide\\data analytics\\project")
data<-read.csv("fire-incidents.csv",sep=";")

##EDA part
#check the missing values in each variable
sapply(data,function(x)sum(is.na(x)))
#drop missing values
data<-na.omit(data)
#seperate month and day from time
datetime<-as.Date(data$alarmtime)
year<-lubridate::year(datetime)
month<-lubridate::month(datetime)
day<-lubridate::day(datetime)
#add month and day to the dataset
data$month<-month
data$day<-day
##do some EDA analysis
#plot histogram for day and month
NumYear<-as.data.frame(table(year))
NumMonth<-as.data.frame(table(month))
NumDay<-as.data.frame(table(day))
ggplot(NumYear,aes(year,Freq))+geom_bar(stat="identity",fill="steelblue")
ggplot(NumMonth,aes(month,Freq))+geom_bar(stat="identity",fill="steelblue")
ggplot(NumDay,aes(day,Freq))+geom_bar(stat="identity",fill="steelblue")
#plot histogram for cause types
NumType<-as.data.frame(table(data$incidentcode))
names(NumType)<-c('classification','number')
ggplot(NumType,aes(x=classification,y=number))+geom_bar(stat="identity",fill="steelblue")
#plot the number of types is more than 500
NumType_Update<-NumType[NumType$number>1500,]
ggplot(NumType_Update,aes(x=classification,y=number))+geom_bar(stat="identity",fill="steelblue")

#plot the number of station to represent different locations
NumStation<-as.data.frame(table(data$station))
names(NumStation)<-c('code','number')
ggplot(NumStation,aes(x=code,y=number))+geom_bar(stat="identity",fill="steelblue")

#Scatter plot for specific locations 
ggplot(data,aes(x=latitude,y=longitude))+geom_point()



##modle part
#build the model
#first model : logistic regression model
#give values between 0 and 1 to different groups
data$type1<-0
data$type2<-0
data$type3<-0
data$type4<-0
data$type5<-1
data$type1[which(data$incidentcode==321)]<-1
data$type2[which(data$incidentcode==311)]<-1
data$type3[which(data$incidentcode==611)]<-1
data$type4[which(data$incidentcode==743)]<-1
data$type5[which(data$incidentcode==321)]<-0
data$type5[which(data$incidentcode==311)]<-0
data$type5[which(data$incidentcode==611)]<-0
data$type5[which(data$incidentcode==743)]<-0
p<-as.data.frame(table(data$incidentcode))
P1<-p[which(p$Var1==321),2]/sum(p$Freq)
P2<-p[which(p$Var1==311),2]/sum(p$Freq)
P3<-p[which(p$Var1==611),2]/sum(p$Freq)
P4<-p[which(p$Var1==743),2]/sum(p$Freq)
P5<-1-P1-P2-P3-P4
#change the typr of each variable
data$type1<-as.factor(data$type1)
data$type2<-as.factor(data$type2)
data$type3<-as.factor(data$type3)
data$type4<-as.factor(data$type4)
data$type5<-as.factor(data$type5)
data$month<-as.factor(data$month)
data$day<-as.factor(data$day)
data$incidentcode<-as.factor(data$incidentcode)
data$station<-as.factor(data$station)
data$shift<-as.factor(data$shift)
data$mutl_aid<-as.factor(data$mutl_aid)
data$current_district<-as.factor(data$current_district)
data$current_fmz<-as.factor(data$current_fmz)
data$majorcategory<-as.factor(data$majorcategory)
data$indicentdesc<-as.factor(data$indicentdesc)
#generate train matrix
train1<-data[c('type1','month','day','station','shift','mutl_aid')]
train2<-data[c('type2','month','day','station','shift','mutl_aid')]
train3<-data[c('type3','month','day','station','shift','mutl_aid')]
train4<-data[c('type4','month','day','station','shift','mutl_aid')]
train5<-data[c('type5','month','day','station','shift','mutl_aid')]
#generate test set
test11<-train1[seq(10,47000,10),c(2:6)]
test12<-train1[seq(10,47000,10),1]
test21<-train2[seq(10,47000,10),c(2:6)]
test22<-train2[seq(10,47000,10),1]
test31<-train3[seq(10,47000,10),c(2:6)]
test32<-train3[seq(10,47000,10),1]
test41<-train4[seq(10,47000,10),c(2:6)]
test42<-train4[seq(10,47000,10),1]
test51<-train5[seq(10,47000,10),c(2:6)]
test52<-train5[seq(10,47000,10),1]
#build model to check whether type1 is the cause these fire incidents
model1<-glm(type1~.,data=train1,family = "binomial")
summary(model1)
#make a prediction
prediction1<-predict(model1,newdata=test11,type="response")
print(prediction1)
prediction1[prediction1>=1.05*P1]<-1
prediction1[prediction1<1.05*P1]<-0
#test the accuracy
a<-0
for(i in 1:length(prediction1)){
  if(prediction1[i]==test12[i]){
    a<-a+1
  }
}
result1<-data.frame(test12,prediction1)
accuracy1<-a/dim(test11)[1]
print(accuracy1)

#build model to check whether type2 is the cause these fire incidents
model2<-glm(type2~.,data=train2,family = "binomial")
summary(model2)
#make a prediction
prediction2<-predict(model2,newdata=test21,type="response")
print(prediction2)
prediction2[prediction2>=1.05*P2]<-1
prediction2[prediction2<1.05*P2]<-0
#test the accuracy
a<-0
for(i in 1:length(prediction2)){
  if(prediction2[i]==test22[i]){
    a<-a+1
  }
}
result2<-data.frame(test22,prediction2)
accuracy2<-a/dim(test21)[1]
print(accuracy2)

#build model to check whether type3 is the cause these fire incidents
model3<-glm(type3~.,data=train3,family = "binomial")
summary(model3)
#make a prediction
prediction3<-predict(model3,newdata=test31,type="response")
print(prediction3)
prediction3[prediction3>=1.05*P3]<-1
prediction3[prediction3<1.05*P3]<-0
#test the accuracy
a<-0
for(i in 1:length(prediction3)){
  if(prediction3[i]==test32[i]){
    a<-a+1
  }
}
result3<-data.frame(test32,prediction3)
accuracy3<-a/dim(test31)[1]
print(accuracy3)

#build model to check whether type4 is the cause these fire incidents
model4<-glm(type4~.,data=train4,family = "binomial")
summary(model4)
#make a prediction
prediction4<-predict(model4,newdata=test41,type="response")
print(prediction4)
prediction4[prediction4>=1.05*P4]<-1
prediction4[prediction4<1.05*P4]<-0
#test the accuracy
a<-0
for(i in 1:length(prediction4)){
  if(prediction4[i]==test42[i]){
    a<-a+1
  }
}
result4<-data.frame(test42,prediction4)
accuracy4<-a/dim(test41)[1]
print(accuracy4)

#build model to check whether type5 is the cause these fire incidents
model5<-glm(type5~.,data=train5,family = "binomial")
summary(model5)
#make a prediction
prediction5<-predict(model5,newdata=test51,type="response")
print(prediction5)
prediction5[prediction5>=1.05*P5]<-1
prediction5[prediction5<1.05*P5]<-0
#test the accuracy
a<-0
for(i in 1:length(prediction5)){
  if(prediction5[i]==test52[i]){
    a<-a+1
  }
}
result5<-data.frame(test52,prediction5)
accuracy5<-a/dim(test51)[1]
print(accuracy5)
#collect the accuracy for each cause variable
result<-data.frame(c("321","311","611","743","other"),c(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5))
names(result)<-c("cause code","accuracy")
print(result)

#model 2 : decision tree
#+majorcategory+indicentdesc
library(rpart)
library(rpart.plot)
data$type<-1
data[which(data$incidentcode==321),25]<-2
data[which(data$incidentcode==311),25]<-3
data[which(data$incidentcode==611),25]<-4
data[which(data$incidentcode==743),25]<-5
data$type<-as.factor(data$type)
train<-data[c('type','month','day','station','shift',"majorcategory",'mutl_aid',"current_district","current_fmz")]
TreeModel<-rpart(type~month+day+station+shift+majorcategory+mutl_aid+current_district+current_fmz,data=train,method='anova')
print(TreeModel)
windows()
rpart.plot(TreeModel)
train_DT<-train[seq(10,47000,10),c(2:9)]
test_DT<-train[seq(10,47000,10),1]
prediction_DT<-predict(TreeModel,train_DT)
print(prediction_DT)
prediction_DT[prediction_DT==1]<-1
prediction_DT[prediction_DT<2.1&prediction_DT>1.9]<-2
prediction_DT[prediction_DT<2.27&prediction_DT>2.26]<-3
prediction_DT[prediction_DT>2.27&prediction_DT<2.3]<-4
prediction_DT[prediction_DT>3&prediction_DT<4]<-5
a<-0
for( i in 1:length(prediction_DT)){
  if(prediction_DT[i]==test_DT[i]){
    a<-a+1
  }
}
accuracy<-a/length(prediction_DT)
print(accuracy)

