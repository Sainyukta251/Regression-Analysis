#import data
data=read.csv("D:\\Sainyukta\\wood-fibres.csv")
#descriptive statistics
#summary of dataset
summary(data)
#structure of dataset
str(data)
#correlation between dependent and independent variables
cor(data$TotalKinkAngle,data$Length)
cor(data$TotalKinkAngle,data$Curl)
cor(data$TotalKinkAngle,data$KinkIndex)
cor(data$TotalKinkAngle,data$NumberKink)
cor(data$TotalKinkAngle,data$Width)
#correlation test between dependent and independet variables
cor.test(data$TotalKinkAngle,data$Length)
cor.test(data$TotalKinkAngle,data$Curl)
cor.test(data$TotalKinkAngle,data$KinkIndex)
cor.test(data$TotalKinkAngle,data$NumberKink)
cor.test(data$TotalKinkAngle,data$Width)
#fitting of model
model=lm(data$TotalKinkAngle~data$NumberKink+data$Width+data$Length+data$Curl+data$KinkIndex)
summary(model)
#since value of adjusted R square is >0.5 the model is good fit for data
#the equation for fitting thus becomes TotalKinkAngle=-0.77+23.28*NimberKink+0.06*Width-2.87*Length+6.55*Curl+8.28*KinkIndex
#plotting the model
plot(model)
#ridge regression
library(glmnet)
y=data$TotalKinkAngle
x=data.matrix(data[,c("Length","NumberKink","Width","Curl","KinkIndex")])
rmodel=glmnet(x,y,alpha=0)
summary(rmodel)
#to find best value of lambda for ridge regression
cv_model=cv.glmnet(x,y,alpha=0)
best_lambda=cv_model$lambda.min
best_lambda
plot(cv_model) 
best_model=glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)
plot(rmodel, xvar = "lambda")
y_predicted=predict(rmodel, s = best_lambda, newx = x)
sst=sum((y - mean(y))^2)
sse=sum((y_predicted - y)^2)
rsq=1 - sse/sst
rsq
