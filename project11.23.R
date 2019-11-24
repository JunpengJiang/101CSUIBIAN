rm(list = ls())
train <- read.csv("train.csv")
test <- read.csv("test.csv")
View(copy)
copy <- train
train = subset(train, select = -c(id,gameID,VT,HT,VTleague,HTleague,date) )
train$HTWins <- as.factor(train$HTWins)

index <- sample(1:9520,7000,replace = T)
usetrain <- train[index,]
usetest <- train[-index,]
#########################################
x1 <- usetrain[,-1]
y1 <- usetrain$HTWins
x2 <- usetest[,-1]
y2 <- usetest$HTWins
library(class)
knnmodel <- knn(x1,x2,y1,k=20)
table(knnmodel,y2)
##########################################
library(glmnet)
x <- model.matrix(HTWins~.,data = usetrain)
y <- ifelse(usetrain$HTWins=="Yes", 1, 0)

lambda <- 10^seq(10,-2,length=100)
lasso <- glmnet(x,y,alpha = 1,lambda = lambda)
cvlasso <- cv.glmnet(x,y,alpha = 1,lambda = lambda)
plot(cvlasso)
best <- cvlasso$lambda.min
bestlasso <- glmnet(x,y,alpha = 1,lambda = best)
predict<-predict(bestlasso,newx=model.matrix(HTWins~.,data = usetest),s=best,type="response")
p<-ifelse(predict>0.5,"Yes","No")
table(usetest$HTWins,p)
mean(usetest$HTWins == p)
###########################################
library(glmnet)
x <- model.matrix(HTWins~.,data = usetrain)
y <- ifelse(usetrain$HTWins=="Yes", 1, 0)

lambda <- 10^seq(10,-2,length=100)
ridge <- glmnet(x,y,alpha = 0,lambda = lambda)
cvridge <- cv.glmnet(x,y,alpha = 0,lambda = lambda)
plot(cvridge)
best <- cvridge$lambda.min
bestridge <- glmnet(x,y,alpha = 0,lambda = best)
predict<-predict(bestridge,newx=model.matrix(HTWins~.,data = usetest),s=best,type="response")
p<-ifelse(predict>0.5,"Yes","No")
table(usetest$HTWins,p)
mean(usetest$HTWins == p)

pred.ridge=predict(bestridge,newx=model.matrix(HTWins~.,data = usetest),type = "response")