library(readr)
library(caTools)
library(dplyr)
require(tree)
library(caTools)
library(mlbench)
library(caret)
library(adabag)
library(rpart)
library("rpart.plot")
library(randomForest)
library(pROC)
library(ROSE)
library(adabag)


dataset = read.csv("customer_churn.csv")
dataset[dataset == "Yes"] <- 1
dataset[dataset == "No"] <- 0
dataset$Churn <- as.numeric(as.character(dataset$Churn))  
head(dataset)
dataset = subset(dataset, select =  - customerID  )
dataset  = na.omit(dataset)

head(dataset)
train.index <- sample(c(1:dim(dataset)[1]), dim(dataset)[1]*0.67)
train.df <- dataset[train.index, ]
valid.df <-dataset[-train.index, ]
label <- length(dataset$Churn)
train_length <- length(train.df$Churn)
test_length <- length(valid.df$Churn)
H <- c(label, train_length, test_length)
M <- c("Dataset","Train","Test")
barplot(H, names.arg = M)
dim(train.df)
sapply(train.df, class)
table(train.df$Churn)
prop.table(table(train.df$Churn))
dim(train.df)
library(ROSE)
train_set <- ovun.sample(Churn~., data=train.df,
                                N=nrow(train.df), p=0.3, 
                                seed=1, method="both")$data


table(data.balanced.ou$Churn)

prop.table(table(train_set$Churn))
c####################
#Feature Importance
###################
set.seed(300)
model <- rpart(Churn~., data =  train_set, method = "class")
importance <- varImp(model, scale = FALSE)
print(importance)
train_set = subset(train_set, select = -c(gender,Partner,Dependents,StreamingMovies))
test_set = subset(valid.df, select = -c(gender,Partner,Dependents,StreamingMovies))
library(rpart)
library(rpart.plot)
dtree_fit_gini <- train(as.factor(Churn)~., data = train_set, method = "rpart", parms = list(split = "gini"),  tuneLength = 10)
print(dtree_fit_gini)
pred_tree <- predict(dtree_fit_gini, test_set)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = test_set$Churn)
caret::confusionMatrix(as.factor(pred_tree), as.factor(test_set$Churn))
plot(pred_tree)    

# boosting
set.seed(300)
train_set$Churn <- as.factor(train_set$Churn)
boost <- boosting(Churn ~ ., data = train_set)
pred_boost <- predict(boost, test_set, type = "class")
confusionMatrix(as.factor(pred$class), as.factor(test_set$Churn))


boost_2 <- boosting(Churn ~ ., data = train_set,coeflearn = "Freund")
pred_2 <- predict(boost_2, test_set, type = "class")

confusionMatrix(as.factor(pred_2$class), as.factor(test_set$Churn))

boost_3 <- boosting(Churn ~ ., data = train_set,mfinal = 150)
pred_3 <- predict(boost_3, test_set, type = "class")

confusionMatrix(as.factor(pred_3$class), as.factor(test_set$Churn))

pred_Tree_pro <- predict(dtree_fit_gini, test_set , type = "prob")
print( pred_boost_pro)
pred_boost2_pro <- predict(boost_2, test_set , type = "prob")
pred_Tree_pro
pred_boost1_pro <- predict(boost, test_set , type = "prob")
pred_boost3_pro <- predict(boost_3, test_set , type = "prob")

Label = as.numeric(test_set$Churn)

Decsion.tree.roc <- roc(Label , pred_Tree_pro[,2])
Radnom.boost1.roc <- roc(Label ,pred_boost1_pro$prob[,2])
Radnom.boost2.roc <- roc(Label ,pred_boost2_pro$prob[,2])
Radnom.boost3.roc <- roc(Label ,pred_boost3_pro$prob[,2])

plot(Decsion.tree.roc, col="red" , legecy.axis = TRUE)
plot(Radnom.boost1.roc, col="blue" ,add = TRUE)
plot(Radnom.boost2.roc, col="green" ,add = TRUE)
plot(Radnom.boost3.roc, col="yellow" ,add = TRUE)

auc(Decsion.tree.roc)
auc(Radnom.boost1.roc)
auc(Radnom.boost2.roc)
auc(Radnom.boost3.roc)

