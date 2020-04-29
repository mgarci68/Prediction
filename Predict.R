library(caret); library(rattle); library(corrplot)

##Reading Data 
d_train <- read.csv("pml-training.csv", header=TRUE)
d_test <- read.csv("pml-testing.csv", header=TRUE)


## cleaning and sorting Data 

##ID removal

d_train <- d_train[, -(1:5)]
d_test  <- d_test[, -(1:5)]

##Variable selection 

var_remove <- nearZeroVar(d_train); d_train <- d_train[, -var_remove]; d_test  <- d_test[, -var_remove]

d_train <- d_train[, colSums(is.na(d_train)) == 0]
d_test <- d_test[, colSums(is.na(d_test)) == 0]

sum(complete.cases(d_train))

##Cross validation
inTrain <- createDataPartition(d_train$classe, p=0.75, list=FALSE)
train_d <- d_train[inTrain, ]
test_d <- d_train[-inTrain, ]


##Correlation Analysis

cor_train_mx <-  cor(d_train[,-54])
corrplot(cor_train_mx, method = "color", type= "upper", tl.cex = 0.4)



##Prediction Model (Random Forest)

mdl_trainRF <- randomForest(classe ~., data=train_d)
CV <- trainControl(method = "cv",number=4)
m_trainRF <- train(classe ~ ., data=train_d, method="rf", trControl= CV )
m_trainRF
getTree(m_trainRF$finalModel, k = 2)
pred <- predict(m_trainRF,test_d); test_d$predRigth <- pred == test_d$classe

cMatRF <- confusionMatrix(pred,test_d$classe)
cMatRF

d_test<- d_test[,-54]
pred_f <- predict(m_trainRF,d_test)
pred_f
