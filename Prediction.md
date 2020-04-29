---
title: "Prediction"
author: "Maria"
date: "29 de abril de 2020"
output: html_document
---

##OverView


##Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>

###Submition

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases

###Data

Data

The training data for this project are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source: <http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>. 

If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

##Geting and reading Data
```{r, warning=FALSE, message=FALSE}
library(caret); library(rattle); library(corrplot)

d_train <- read.csv("pml-training.csv", header=TRUE)
d_test <- read.csv("pml-testing.csv", header=TRUE)
```

## Cleaning and sorting Data 

###ID removal

The first Step for build our prediction model, is cleaning all data that wont be necessary for our calculation, in that matter we remove al ID variables contain in the dataset.

```{r}

d_train <- d_train[, -(1:5)]
d_test  <- d_test[, -(1:5)]
```

###Variable selection 

In order to performe an accuarate prediction, our parameter to select the fit variables is:
        
        *Cleaning the data or variables with NearZeroCovariance,
        *Clean all the variables with missing values.

```{r}
var_remove <- nearZeroVar(d_train); d_train <- d_train[, -var_remove]; d_test  <- d_test[, -var_remove]

d_train <- d_train[, colSums(is.na(d_train)) == 0]
d_test <- d_test[, colSums(is.na(d_test)) == 0]

sum(complete.cases(d_train))
```

##CrossValidation

By making a random subsample with the 75% of the original data, we stablish our traning set for the prediction model and a 25% of the original data, we set our subsample for the testing part of the prediction.

```{r}
inTrain <- createDataPartition(d_train$classe, p=0.75, list=FALSE)
train_d <- d_train[inTrain, ]
test_d <- d_train[-inTrain, ]

```

##Correlation Analysis

By performing a correlation analysis with the resultant variables we can see the level of correlation between each variables as shows the next figure.

```{r}
cor_train_mx <-  cor(d_train[,-54])
corrplot(cor_train_mx, method = "color", type= "upper", tl.cex = 0.4)

```
##Bulding Our Prediction Model 

###Random Forest

The random forest is a prediction  model with the estimated of decision trees, that is the main reason for chosing this method instead of performing jus one decision tree. in this model we use two main components, 1) random sampling and 2) random subsets of spliting nodes.

```{r}
mdl_trainRF <- randomForest(classe ~., data=train_d)

CV <- trainControl(method = "cv",number=4)

m_trainRF <- train(classe ~ ., data=train_d, method="rf", trControl= CV )
m_trainRF

getTree(m_trainRF$finalModel, k = 2)

pred <- predict(m_trainRF,test_d); test_d$predRigth <- pred == test_d$classe

cMatRF <- confusionMatrix(pred,test_d$classe)
cMatRF

```

##Modelation Applied to pml-TEST data 

With our final model, now we can apply it to our final goal, predic class from the pml-testing data. 

```{r}

d_test<- d_test[,-54] ##substrac problem_id
pred_f <- predict(m_trainRF,d_test)
pred_f
```



