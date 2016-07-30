<<<<<<< HEAD
# @Gaurav Dangi
library(mlbench)
library(ggplot2)
library(caret)
library(data.table)
library(Amelia)
library(plotly)
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

cor(diabetes,glucose)   #it's 0.5
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

diabetes<-as.numeric(diabetes)
cor(mass,diabetes)

diabetes<-as.factor(diabetes)
AmeliaView()
# too many missing values in insulin, triceps and pressure so i will not include these features
#As missing variables don't have high correlation with diabetes variable and too many missing values

PimaIndiansDiabetes2$insulin<-NULL
PimaIndiansDiabetes2$triceps<-NULL
PimaIndiansDiabetes2$pressure<-NULL

#Now we can apply imputation to rest of the few missing values
write.csv(PimaIndiansDiabetes2,"PimaIndiansDiabetes2.csv")
AmeliaView()
PimaIndiansDiabetes2 <- read.csv("~/PimaIndiansDiabetes2-imp1.csv")
sapply(PimaIndiansDiabetes2,class)
PimaIndiansDiabetes2$X<-NULL
PimaIndiansDiabetes2$pregnant<-as.numeric(PimaIndiansDiabetes2$pregnant)
PimaIndiansDiabetes2$glucose<-as.numeric(PimaIndiansDiabetes2$glucose)
PimaIndiansDiabetes2$age<-as.numeric(PimaIndiansDiabetes2$age)


dd<-createDataPartition(PimaIndiansDiabetes2$diabetes,p=0.7,list = F)
PimaIndiansDiabetes2$diabetes<-as.numeric(PimaIndiansDiabetes2$diabetes)-1
#pos -> 1
#neg -> 0
PimaIndiansDiabetes2$diabetes<-as.factor(PimaIndiansDiabetes2$diabetes)

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

reg=glm(diabetes~.,data=training,family=binomial)
summary(reg)

q<-predict(reg,testing,type="response")
q<-round(q)
q
levels(training$diabetes)
levels(testing$diabetes)
confusionMatrix(q,testing$diabetes)

accuracy<-(1-mean(q!=testing$diabetes))*100
accuracy  # 75.52%

#--------------------------------------------------------
#Using Support vector machine algorithm(SVM)

library(e1071)
set.seed(1234)
t<-tune(svm,diabetes~.,data=training,ranges = list(cost=c(0.001,0.01,0.1,1,10,100)))
summary(t)  # best cost = 1
reg2=svm(diabetes~.,data=training,cost = 1,scale=F,gamma = 1)
reg2

q<-predict(reg2,testing)
q
confusionMatrix(q,testing$diabetes)
comp<-ftable(q,testing$diabetes)
accuracy<-(sum(diag(comp))/sum(comp))*100
accuracy   # ----65.2% -------
#--- when gamma =1 -----
#             Reference
#    Prediction   0   1
#             0 150  80       
#             1   0   0
#------------------------
# So above svm model is predicting 0 for every value for gamma= 1


set.seed(1234)
reg3=svm(diabetes~.,data=training,kernel="linear",cost = 1,scale=F)
reg3

q<-predict(reg3,testing)
q
confusionMatrix(q,testing$diabetes)
comp<-ftable(q,testing$diabetes)
accuracy<-(sum(diag(comp))/sum(comp))*100
accuracy

#----------  74.34 %  ----------------------


#--------------------------------------------------------
#using neural networking
#hidden layer=1
library(neuralnet)
nn=neuralnet(diabetes~pregnant+mass+pedigree+glucose+age,data=training,hidden=1,linear.output = FALSE,err.fct="ce")
nn
plot(nn)

nn$weights
nn$net.result
pred_v3=ifelse(nn$net.result[[1]]>0.5,1,0)
comparison3=table(pred_v3,data$diabetes)
comparison3
accuracy3=sum(diag(comparison3))/sum(comparison3)*100
accuracy3
#hidden layer=2
nn2=neuralnet(diabetes~pregnant+glucose+triceps+insulin+mass+pedigree+age,data=data,hidden=2,linear.output = FALSE,err.fct="ce")
nn2
plot(nn2)
nn2$weights
nn2$net.result
pred_v3=ifelse(nn2$net.result[[1]]>0.5,1,0)
comparison4=table(pred_v3,data$diabetes)
comparison4
accuracy4=sum(diag(comparison3))/sum(comparison3)*100
accuracy4

#--------------k-means clustering---------------------------

k<-kmeans(training,centers = 2)
table(diabetes,k$cluster)
par(mfrow=c(1,2))
plot(x=training$age,y=training$glucose,col=diabetes,main = "Age vs Glucose")
plot(x=training$age,y=training$glucose,col=k$cluster,main = "Cluster analysis of Age vs Glucose")

#-----------------------------------------------------------
#K-Nearest Neighbour







#------------------------------------------------------------
#Desision tree
# Classification Tree with rpart
library(rpart)
# grow tree 
set.seed(1234)
fit=rpart(diabetes~.,data=training,method = "class")
fit
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for Diabetes")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", title = "Classification Tree for diabetes")
=======

# @Gaurav Dangi
