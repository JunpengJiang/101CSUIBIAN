rm(list = ls())
train <- read.csv("train117.csv")
test <- read.csv("test116.csv")

#train <- train[,-166]

usetrain <- train
usetest <- test
###### Lasso #######
library(glmnet)
set.seed(123)
x <- model.matrix(HTWins~.,data = usetrain)
y <- usetrain$HTWins

lambda <- 10^seq(10,-2,length=100)
lasso <- glmnet(x,y,alpha = 1,lambda = lambda)
cvlasso <- cv.glmnet(x,y,alpha = 1,lambda = lambda)
plot(cvlasso)
best <- cvlasso$lambda.min
bestlasso <- glmnet(x,y,alpha = 1,lambda = best)
pred_lasso<-predict(bestlasso,newx=model.matrix(HTWins~.,data = usetest),s=best,type="response")
pred_lasso<-ifelse(pred_lasso>0.5,1,0)
table(usetest$HTWins,pred_lasso)
mean(usetest$HTWins == pred_lasso) # 70.55

####### Ridge #######
set.seed(731)
x <- model.matrix(HTWins~.,data = usetrain)
y <- usetrain$HTWins

lambda <- 10^seq(10,-2,length=100)
ridge <- glmnet(x,y,alpha = 0,lambda = lambda)
cvridge <- cv.glmnet(x,y,alpha = 0,lambda = lambda)
plot(cvridge)
best <- cvridge$lambda.min
bestridge <- glmnet(x,y,alpha = 0,lambda = best)
pred_ridge<-predict(bestridge,newx=model.matrix(~.,data = usetest),s=best,type="response")
pred_ridge<-ifelse(pred_ridge>0.5,1,0)
#table(usetest$HTWins,pred_ridge)
#mean(usetest$HTWins == pred_ridge) # 70.6746

###### GBM ######
library(gbm)
set.seed(731)
gb <- gbm(HTWins~.,data = usetrain,interaction.depth = 2,n.trees=500,shrinkage = 0.01,distribution = "bernoulli")
pred_gbm <- predict.gbm(gb,newdata = usetest,type = "response",n.trees = 500)
pred_gbm <- ifelse(pred_gbm > 0.5,1,0)
table(pred_gbm,usetest$HTWins)
mean(usetest$HTWins == pred_gbm) # 0.6968

####### LDA ######
library(MASS)
set.seed(123)
ldamodel <-lda(HTWins~.,data=usetrain) 
pred_lda <-predict(ldamodel,usetest)
#pred_lda <- ifelse(pred_lda > 0.5,1,0)
table(pred_lda$class,usetest$HTWins)
mean(pred_lda$class == usetest$HTWins) # 0.709127

####### Logistic #######
set.seed(123)
lmodel <- glm(HTWins~.,data=usetrain,family = binomial)
pred_log <- predict(lmodel,usetest,type="response")
pred_log <- ifelse(pred_log>0.5,1,0)
table(pred_log,usetest$HTWins)
mean(pred_log == usetest$HTWins) # 70.95

###### pls ######
library(pls)
set.seed(731)
usetrain$HTWins<-as.numeric(usetrain$HTWins)
pls.fit<-plsr(HTWins ~.,data=usetrain[,-c(2,3,4,5)], scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP",xlim=c(0,20))
#summary(pls.fit)
predict_pls<-predict(pls.fit,usetest,ncomp=7,type="response")
p_pls<-ifelse(predict_pls>0.5,1,0) 
table(usetest$HTWins,p_pls)
pls=mean(usetest$HTWins == p_pls)
pls

######## RANDOM FOREST ##############
library(randomForest)
set.seed(1)
usetrain$HTWins<- as.factor(usetrain$HTWins)
rf<-randomForest(HTWins~.,data=usetrain ,ntree=501, importance=TRUE)
varImpPlot(rf)
predict_rf<-predict(rf,usetest)
table(usetest$HTWins,predict_rf)
random_forest=mean(usetest$HTWins==predict_rf)
random_forest

####### xgboost ###########
library(xgboost)
set.seed (1)
dtrain <- xgb.DMatrix(data = as.matrix(usetrain[,-c(1:5)]), label = as.matrix(usetrain$HTWins))
dtest <- xgb.DMatrix(data = as.matrix(usetest[,-c(1:5)]), label = as.matrix(usetest$HTWins))
bstSparse <- xgboost(data = dtrain,nround=50,verbose=0)
predict_xgboost<-predict(bstSparse,dtest)
p_xgboost<-ifelse(predict_xgboost>0.5,1,0)
table(usetest$HTWins,p_xgboost)
xgboost<-mean(usetest$HTWins == p_xgboost)
xgboost


####### ensemble ###########
col1 <- as.data.frame(pred_ridge)
col2 <- as.data.frame(pred_gbm)
col3 <- as.data.frame(p_pls)
#col4 <- as.data.frame(pred_log)
#col5 <- as.data.frame(pred_lda)

col1 <- col1$`1`
col2 <- col2$pred_gbm
col3 <- col3$`HTWins.7 comps`
#col4 <- col4$pred_log
#col5 <- as.numeric(col5$class) -1 
result <- cbind(col1,col2,col3)

colnames(result) <- c("ridge","gbm","pls")
View(result)
sub_126 <- ifelse(rowMeans(result) > 0.5,"Yes","No")
#table(test,usetest$HTWins)
#mean(test==usetest$HTWins)

sub_126 <- as.data.frame(sub_126)
write.csv(sub_126,"sub126.csv")


