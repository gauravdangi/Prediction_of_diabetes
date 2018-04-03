install.packages("mlbench")
library(mlbench)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
install.packages("caret")
library(caret)
install.packages("data.tab")
library(data.table)
install.packages("readr")
library(readr)
install.packages("data.table")
library(data.table)
#install.packages("")
#library(Amelia)
#library(plotly)
data("PimaIndiansDiabetes2")
head(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)
write.csv(PimaIndiansDiabetes2,"PimaIndiansDiabetes2.csv")
data<-PimaIndiansDiabetes2

#-------------Visualization data----------------------------
library(ggplot2)
attach(data)
data$diabetes<-as.factor(data$diabetes)

ggplot(data,aes(x=diabetes))+geom_bar(aes(fill=factor(diabetes)))
diabetes<-as.numeric(diabetes)
cor(as.numeric(diabetes),glucose)   #it's 0.5
ggplot(data,aes(x=glucose,y=diabetes))+geom_point()+geom_smooth()
cor(diabetes,pressure)  #it's 0.1926733
ggplot(data,aes(x=pressure,y=diabetes))+geom_point()+geom_smooth()
cor(insulin,diabetes)   #it's 0.3 but too many missing values
ggplot(data,aes(x=insulin,y=diabetes))+geom_point()+geom_smooth()
cor(diabetes,age)       #it's 0.35
ggplot(data,aes(x=age,y=diabetes))+geom_point()+geom_smooth()
cor(diabetes,pedigree)  #it's 0.21
ggplot(data,aes(x=pedigree,y=diabetes))+geom_point()+geom_smooth()

ggplot(data=data,aes(x=age,y=glucose,col=diabetes))+geom_point()

cor(mass,diabetes)

diabetes<-as.factor(diabetes)
#AmeliaView()
# too many missing values in insulin, triceps and pressure so i will not include these features
#As missing variables don't have high correlation with diabetes variable and too many missing values

PimaIndiansDiabetes2$insulin<-NULL;data$insulin<-NULL
PimaIndiansDiabetes2$triceps<-NULL;data$triceps<-NULL
PimaIndiansDiabetes2$pressure<-NULL;data$pressure<-NULL

#Now we can apply imputation to rest of the few missing values
write.csv(PimaIndiansDiabetes2,"PimaIndiansDiabetes2.csv")
#AmeliaView()
PimaIndiansDiabetes2 <- read.csv("PimaIndiansDiabetes2-imp1.csv")

PimaIndiansDiabetes2 <- read_csv("D:/Projects/R Projects/Diabetes_prediction/PimaIndiansDiabetes2.csv")
sapply(PimaIndiansDiabetes2,class)


PimaIndiansDiabetes2$X1<-NULL
PimaIndiansDiabetes2$pregnant<-as.numeric(PimaIndiansDiabetes2$pregnant)
PimaIndiansDiabetes2$glucose<-as.numeric(PimaIndiansDiabetes2$glucose)
PimaIndiansDiabetes2$age<-as.numeric(PimaIndiansDiabetes2$age)
#PimaIndiansDiabetes2[is.na(PimaIndiansDiabetes2)]<--9999
PimaIndiansDiabetes2$diabetes<-as.numeric(as.factor(PimaIndiansDiabetes2$diabetes))-1

NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
replace(PimaIndiansDiabetes2, TRUE, lapply(PimaIndiansDiabetes2, NA2mean))
PimaIndiansDiabetes2[] <- lapply(PimaIndiansDiabetes2, NA2mean)
#pos -> 1
#neg -> 0
PimaIndiansDiabetes2$diabetes<-as.factor(PimaIndiansDiabetes2$diabetes)


dd<-createDataPartition(PimaIndiansDiabetes2$diabetes,p=0.7,list = F)
training<-PimaIndiansDiabetes2[dd,]
testing<-PimaIndiansDiabetes2[-dd,]
sapply(training,class)
sapply(testing,class)
training<-data.table(training)
testing<-data.table(testing)

attach(training)

#Now we need to change diabetes into numeric for mathematical calculation

#1->positive
#0->negative

#-------------------------------------------------------
#Logistic regression

reg=glm(diabetes~.,data=training,family=binomial(logit))
summary(reg)

q<-predict(reg,testing)
q<-round(q)
levels(training$diabetes)
levels(testing$diabetes)

accuracy<-(1-mean(1!=testing$diabetes))*100
accuracy  # 85.52%

#--------------------------------------------------------
#Using Support vector machine algorithm(SVM)
install.packages("e1071")
library(e1071)
set.seed(1234)
t<-tune(svm,diabetes~.,data=training,ranges = list(cost=c(0.001,0.01,0.1,1,10,100)))
summary(t)  # best cost = 1
reg2=svm(diabetes~.,data=training,cost = 1,scale=F,gamma = 1)
reg2

q<-predict(reg2,testing)
q
#confusionMatrix(q,testing$diabetes
comp<-(1-mean(q!=testing$diabetes))*100
#comp<-ftable(q,testing$diabetes)
accuracy<-(sum(diag(comp))/sum(comp))*100
accuracy   # ----82.2% -------

set.seed(1234)
reg3=svm(diabetes~.,data=training,kernel="linear",cost = 1,scale=F)
reg3

q<-predict(reg3,testing)
q
confusionMatrix(q,testing$diabetes)
comp<-ftable(q,testing$diabetes)
accuracy<-(sum(diag(comp))/sum(comp))*100
accuracy

#----------  78.69565 %  ----------------------


#------------------------------------------------------------
#Desision tree
# Classification Tree with rpart
install.packages("rpart")
library(rpart)
# grow tree 
set.seed(1234)
fit=rpart(diabetes~.,data=training,method = "class")
fit
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

tree.predict <- predict(fit,testing,type = "class")
accuracy_decisionTree<-(1-mean(tree.predict!=testing$diabetes))*100
accuracy_decisionTree  ####### 73.91304

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for Diabetes")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

