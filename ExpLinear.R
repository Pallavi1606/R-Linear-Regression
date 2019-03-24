#Using linear regression technique to predict the expenses based on factors like:
#age,sex,bmi,no. of children,smoking preference and the region

#Reading the file expenses.csv into R
setwd("E:\\Jigsaw\\BC1\\DataSets\\Class-Datasets\\Class Datasets")
exp<-read.csv("expenses.csv",header=T)
View(exp)

#Data Exploration
dim(exp)
str(exp)
summary(exp)

#Checking for outliers in age and replacing it with the median age value
x<-boxplot(exp$age)
x
index<-which(exp$age %in% x$out)
index
summary(exp$age)
exp$age[index]<-39
View(exp)

summary(exp)

#Checking for outliers in BMI and replacing it with the median bmi value
x<-boxplot(exp$bmi)
x
x$out
index1<-which(exp$bmi %in% x$out)
index1
hist(exp$bmi)
summary(exp$bmi)
exp$bmi[index1]<-30.67
View(exp)
summary(exp)

index2<-which(is.na(exp$bmi))
index2              
exp$bmi[index2]<-30.67
View(exp)
summary(exp)


#Building the model using Linear regression
Mulreg<-lm(charges~.,data=exp)
summary(Mulreg)
step(Mulreg,direction="backward")

MulReg<-lm(charges ~ age + bmi + children + smoker, data = exp)
summary(Mulreg)
step(MulReg,direction = "backward")

#Creating dummies for the variable smoker
exp$SMP<-ifelse(exp$smoker=="yes",1,0)
View(exp)

MulReg<-lm(charges~age+bmi+children+SMP,data=exp)
summary(MulReg)

#Predicted values for charges
predcharges<-predict(MulReg,data=exp)
predcharges
exp$Predcharges<-predcharges
View(exp)
class(predcharges)
MulReg$fitted.values

#Residual values
resi<-resid(MulReg)
resi
MulReg$residuals
exp$residuals<-MulReg$residuals
View(exp)

#Check for homoscadasity
plot(exp$residuals)
library(ggplot2)
library(car)
qplot(exp$Predcharges,exp$residuals)

#normality of residuals
hist(exp$residuals)
qplot(exp$residuals)

#check for multicollinearity
vif(MulReg)

library(corrgram)
cornmix<-corrgram(exp)
cornmix
write.csv(cornmix,"l1cor.csv")

#fitchart
p<-ggplot(exp,aes(x=as.numeric(rownames(exp))))
p+geom_line(aes(y=exp$charges,color="green"))+geom_line(aes(y=exp$Predcharges,color="blue"))        

#MAPE
exp$pe<-(abs(exp$residuals)/exp$charges)*100
View(exp)
MAPE<-mean(exp$pe)
MAPE
