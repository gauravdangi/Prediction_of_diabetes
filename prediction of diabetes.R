library(mlbench)
data("PimaIndiansDiabetes2")
head(PimaIndiansDiabetes2)
data=na.omit(PimaIndiansDiabetes2)
head(data)
attach(data)
data$diabetes=as.numeric(data$diabetes)-1
head(data)
cor(data)
#-------------------------------------------------------
#Logistic regression

reg=glm(diabetes~.,data=data,family=binomial)
summary(reg)
head(reg$fitted.values)
pred_diabetes=round(reg$fitted.values)
comparison = ftable(pred_diabetes,data$diabetes)
comparison
accuracy = sum(diag(comparison))/sum(comparison)*100
accuracy  #78.31633% accuracy
#--------------------------------------------------------
#Using Support vector machine algorithm(SVM)

library(e1071)
reg2=svm(diabetes~.,data=data,cost = 100,gamma = 1)
reg2
pred_v2=round(fitted(reg2))
pred_v2  
comparision2=ftable(pred_v2,data$diabetes) 
comparision2
accuracy2=sum(diag(comparision2))/sum(comparision2)*100
accuracy2   #100% accuracy
accu=(1-mean(data$diabetes!=pred_v2))*100  #we may also check accuracy by this way
accu
#--------------------------------------------------------
#using neural networking
#hidden layer=1
library(neuralnet)
nn=neuralnet(diabetes~pregnant+glucose+triceps+insulin+mass+pedigree+age,data=data,hidden=1,linear.output = FALSE,err.fct="ce")
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
#-----------------------------------------------------------
#Desision tree
# Classification Tree with rpart
library(rpart)
# grow tree 
fit=rpart(diabetes~.,data=data,method = "class")
fit
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for Diabetes")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", title = "Classification Tree for diabetes")
