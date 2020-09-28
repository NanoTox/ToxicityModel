set.seed(105)

#### Reading required libraries
library(plyr)
library(caret)
library(corrplot)
library(DMwR)
library(e1071)
library(RSNNS)
library(data.table)

#### Reading dataset of 483 instances
setwd("~/Desktop/working_dir/")
data_1 <- read.table("dataset.txt", header = TRUE)

#### Outcome class of interest: Toxic or not 
data_1$class_s <- revalue(data_1$class, c("Toxic"="1","nonToxic"="0"))

#### Train-test split (stratified random split) 
train.index <- createDataPartition(data_1$class_s, p = .7, list = FALSE)
train <- data_1[train.index,]
test <- data_1[-train.index,]

#### Feature elimination with corrplot
tox_1 <- subset(train,select = -c(NPs,viability,class,class_s,Cellline,Celltype))
ColumnNames <- c("CoreSize","HydroSize","SurfCharge","SurfArea","Hsf","Ec","Ev","MeO","Time","Dosage","enthalpy","ratio","eNeg","esum","esumbyo","MW","NMetal","NOxygen","ox")
colnames(tox_1) <- ColumnNames
tox_1.rearrange <-tox_1[,order(apply(tox_1,2,var),decreasing = T)]
#newtox_1 <- tox_1.rearrange[,1:19]
cor(tox_1.rearrange)
corrplot(cor(tox_1.rearrange))
newtox_1 <- tox_1.rearrange[,c("CoreSize","HydroSize","SurfCharge","SurfArea","Ec","Time","Dosage","eNeg","NOxygen")]
corrplot(cor(newtox_1))

#### VIF analysis
source("vif.R")

vif("CoreSize",tox_1)
vif("HydroSize",tox_1)
vif("SurfCharge",tox_1)
vif("SurfArea",tox_1)
vif("Hsf",tox_1)
vif("Ec",tox_1)
vif("Ev",tox_1)
vif("MeO",tox_1)
vif("Time",tox_1)
vif("Dosage",tox_1)
vif("enthalpy",tox_1)
vif("ratio",tox_1)
vif("eNeg",tox_1)
vif("esum",tox_1)
vif("esumbyo",tox_1)
vif("MW",tox_1)
vif("NMetal",tox_1)
vif("NOxygen",tox_1)
vif("ox",tox_1)

vif("CoreSize",newtox_1)
vif("HydroSize",newtox_1)
vif("SurfCharge",newtox_1)
vif("SurfArea",newtox_1)
vif("Ec",newtox_1)
vif("Time",newtox_1)
vif("Dosage",newtox_1)
vif("eNeg",newtox_1)
vif("NOxygen",newtox_1)

#### Feature-selected train and test data
train_data <- train[,c("coresize","hydrosize","surfcharge","surfarea","Ec","Expotime","dosage","e","NOxygen","class_s")]
test_data <- test[,c("coresize","hydrosize","surfcharge","surfarea","Ec","Expotime","dosage","e","NOxygen","class_s")]

ColumnNames <- c("CoreSize","HydroSize","SurfCharge","SurfArea","Ec","Time","Dosage","eNeg","NOxygen","class_s")
colnames(train_data) <- ColumnNames
colnames(test_data) <- ColumnNames

#### Data normalization: train and test instances
source("normalise.R")
train_data <- normalise(train_data)
test_data <- normalise(test_data)
save(train_data, file = "normalised_train_data.Rdata")

#### Train data Balancing using SMOTE
train_data_bal <- SMOTE(class_s~.,train_data)
table(train_data_bal$class)

#### MODELS
#### 1. Logistic regression
train_for_log <- train_data_bal
train_control <- trainControl(method="cv",number = 10)
model_1 <- caret::train(class_s~., data = train_for_log, trControl=train_control, method = "glm", family = binomial())
summary(model_1)
train_for_log$pred <- predict(model_1,train_for_log)
caret::confusionMatrix(data= train_for_log$pred, reference = train_for_log$class_s, positive = "1")

test_for_log <- test_data
test_for_log$pred <- predict(model_1,test_for_log)
caret::confusionMatrix(data= test_for_log$pred, reference = test_for_log$class_s, positive = "1")

saveRDS(model_1,file = "model_log.rds")
#### 2. Random forest
train_for_rf <- train_data_bal
control <- trainControl(method = "repeatedcv",number = 10,repeats= 3,search="grid")
mtry <- sqrt(ncol(train_for_rf))
tunegrid <- expand.grid(.mtry=mtry)
model_2 <- caret::train(class_s~.,data=train_for_rf,method="rf",tuneGrid=tunegrid,trControl=control)
summary(model_2)
plot(varImp(model_2), main="Variable importance: model_2")
train_for_rf$pred <- predict(model_2,train_for_rf)
caret::confusionMatrix(data= train_for_rf$pred, reference = train_for_rf$class_s, positive = "1")

test_for_rf <- test_data
yhat_rf <- predict(model_2,test_for_rf)
caret::confusionMatrix(data= yhat_rf, reference = test_for_rf$class_s, positive = "1")

saveRDS(model_2,file = "model_rf.rds")
#### 3a. SVM linear
train_for_svm_a <- train_data_bal
model_3a <- tune(svm,class_s~.,data = train_for_svm_a,kernel="linear",ranges = list(cost=c(0.01,0.1,0.5,1,5,10)))
summary(model_3a)
yhat_a <- predict(model_3a$best.model,train_for_svm_a)
caret::confusionMatrix(yhat_a,train_for_svm_a$class_s, positive = "1")

test_for_svm_a <- test_data
test_yhat_a <- predict(model_3a$best.model,test_for_svm_a)
caret::confusionMatrix(test_yhat_a,test_for_svm_a$class_s, positive = "1")

saveRDS(model_3a,file = "model_SVM_linear.rds")
#### 3b. SVM radial
train_for_svm_b <- train_data_bal
model_3b <- tune(svm,class_s~.,data=train_for_svm_b,kernel="radial",ranges = list(cost=c(0.01,0.1,0.5,1,5,10),gamma=c(0.1,0.5,1,5)))
summary(model_3b)
yhat_b <- predict(model_3b$best.model,train_for_svm_b)
caret::confusionMatrix(yhat_b,train_for_svm_b$class_s, positive = "1")

test_for_svm_b <- test_data
test_yhat_b <- predict(model_3b$best.model,test_for_svm_b)
caret::confusionMatrix(test_yhat_b,test_for_svm_b$class_s, positive = "1")

saveRDS(model_3b,file = "model_SVM_radial.rds")
#### 3c. SVM polynomial
train_for_svm_c <- train_data_bal
model_3c <- tune(svm,class_s~.,data=train_for_svm_c,kernel="polynomial",ranges = list(cost=c(0.01,0.1,0.5,1,5,10),gamma=c(0.1,0.5,1,5),degree=c(3,4,5)))
summary(model_3c)
yhat_c <- predict(model_3c$best.model,train_for_svm_c)
caret::confusionMatrix(yhat_c,train_for_svm_c$class_s, positive = "1")

test_for_svm_c <- test_data
test_yhat_c <- predict(model_3c$best.model,test_for_svm_c)
caret::confusionMatrix(test_yhat_c,test_for_svm_c$class_s, positive = "1")

saveRDS(model_3c,file = "model_SVM_poly.rds")
#### 4. Neural networks -- 1 hidden layer
train_for_nn_a <- train_data_bal
tc <- trainControl(method="cv",number=10,search = "grid")
my.grid <- expand.grid(.decay = c(0.1,0.5,1,5), .size = c(1:10))
model_4 <- caret::train(class_s ~., data = train_for_nn_a,method = "nnet",trControl=tc, maxit = 1000, tuneGrid = my.grid, trace = F) 
model_4$results
model_4$finalModel
model_4$bestTune
plot(model_4)
train_for_nn_a$pred <- predict(model_4,train_for_nn_a)
caret::confusionMatrix(train_for_nn_a$pred,train_for_nn_a$class_s, positive = "1")

test_for_nn_a <- test_data
test_for_nn_a$pred <- predict(model_4,test_for_nn_a)
caret::confusionMatrix(test_for_nn_a$pred,test_for_nn_a$class_s, positive = "1")

saveRDS(model_4,file = "model_neuralNet_1layer.rds")
#### 5. Neural networks -- 2 hidden layers
train_for_nn_b <- train_data_bal
train_x <- train_for_nn_b[,1:9]
train_y <- train_for_nn_b[,10]
mlp_grid = expand.grid(layer1 = c(1:9),
                       layer2 = c(1:9),
                       layer3 = 0)
mlp_fit = caret::train(x = train_x, 
                       y = train_y, 
                       method = "mlpML", 
                       preProc =  c('center', 'scale'),
                       trControl = trainControl(method = "cv", returnData = FALSE),
                       tuneGrid = mlp_grid)
mlp_fit$results
mlp_fit$bestTune
mlp_fit$finalModel
plot(mlp_fit)
train_for_nn_b$pred <- predict(mlp_fit,train_for_nn_b)
caret::confusionMatrix(train_for_nn_b$pred,train_for_nn_b$class_s, positive = "1")

test_for_nn_b <- test_data
test_for_nn_b$pred <- predict(mlp_fit,test_for_nn_b)
caret::confusionMatrix(test_for_nn_b$pred,test_for_nn_b$class_s, positive = "1")

saveRDS(mlp_fit,file = "model_neuralNet_2layer.rds")

