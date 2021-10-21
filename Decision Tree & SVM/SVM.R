data <- read.csv("diabetes.csv")
#Data Preprocessing
data[data == "?"] <- NA

list_na <- colnames(data)[ apply(data, 2, anyNA) ]
list_na
sapply(data, class)
names = colnames(data)
data[names]<- apply(data[names], 2, function(x) as.numeric(as.character(x)))
sapply(data, class)
library(zoo)
data <- na.aggregate(data)
head(data)

#splitting data
dt = sort(sample(nrow(data), nrow(data)*.75))
train<-data[dt,]
test<-data[-dt,]

install.packages("neuralnet ")

# load library
library(neuralnet)
library(caret)
library(dplyr)
#using normal data as feed into NN
set.seed(2)
NN = neuralnet(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , train, hidden =2,linear.output = T  )
plot(NN)

#Evaluating

predict_testNN1 = neuralnet::compute(NN, test[,c(1:8)])
predict_testNN1 = (predict_testNN1$net.result * (max(data$Outcome) - min(data$Outcome))) + min(data$Outcome)
plot(test$Outcome, predict_testNN1, col='red', pch=16, ylab = "predicted rating NN", xlab = "Outcome")
abline(0,1)
RMSE.NN = (sum((test$Outcome - predict_testNN1)^2) / nrow(data)) ^ 0.5
RMSE.NN
predict_testNN1 <- ifelse(predict_testNN1>0.5, 1, 0)

results_first_trail <- data.frame(actual= test$Outcome, prediction = predict_testNN1)
print(results_first_trail)
sapply(results_first_trail, class)
results_first_trail <- as.data.frame(results_first_trail)
confusionMatrix(as.factor(results_first_trail$prediction), as.factor(results_first_trail$actual))
#Scaling data 
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

# creating training and test set
trainNN = scaled[dt , ]
testNN = scaled[-dt , ]

# fit neural network
set.seed(2)
NNscaled = neuralnet(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , trainNN, hidden = 2 , linear.output = T )
plot(NNscaled)

#Evaluating

predict_test_scaled = neuralnet::compute(NNscaled, testNN[,c(1:8)])
predict_test_scaled = (predict_test_scaled$net.result * (max(data$Outcome) - min(data$Outcome))) + min(data$Outcome)
plot(testNN$Outcome, predict_test_scaled, col='red', pch=16, ylab = "predicted rating NN", xlab = "Outcome")
abline(0,1)
RMSE.NNscaled = (sum((test$Outcome - predict_test_scaled)^2) / nrow(data)) ^ 0.5
RMSE.NNscaled
NNscaled$result.matrix
pred_sclaed <- ifelse(predict_test_scaled>0.5, 1, 0)
class(pred)
results_scaled <- data.frame(actual= testNN$Outcome, prediction = pred_sclaed)
print(results_scaled)
sapply(results_scaled, class)
results_scaled <- as.data.frame(results_scaled)
confusionMatrix(factor(results_scaled$prediction), factor(results_scaled$actual))



# NN With 2 hidden Layers Five nodes

set.seed(2)
NN_five_nodes = neuralnet(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , trainNN, hidden = c(5,5) ,stepmax = 10000000, linear.output = T )
plot(NN_five_nodes)

#Evaluating

predict_test_five = neuralnet::compute(NN_five_nodes, testNN[,c(1:8)])
predict_test_five = (predict_test_five$net.result * (max(data$Outcome) - min(data$Outcome))) + min(data$Outcome)
plot(testNN$Outcome, predict_test_five, col='red', pch=16, ylab = "predicted rating NN", xlab = "Outcome")
abline(0,1)
RMSE.NN_five = (sum((test$Outcome - predict_test_five)^2) / nrow(data)) ^ 0.5
RMSE.NN_five
pred_five_nodes <- ifelse(predict_test_five>0.5, 1, 0)

results_five_nodes <- data.frame(actual= testNN$Outcome, prediction = pred_five_nodes)
results_five_nodes <- as.data.frame(results_five_nodes)
confusionMatrix(as.factor(results_five_nodes$prediction), as.factor(results_five_nodes$actual))



# modified 2 node_tanh
set.seed(2)
NN_2_mode_act = neuralnet(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , train,act.fct = "tanh", hidden =2,linear.output = T  )
plot(NN_2_mode_act)

#Evaluating

predict_testNN_2_mod_act = neuralnet::compute(NN_2_mode_act, test[,c(1:8)])
predict_testNN_2_mod_act = (predict_testNN_2_mod_act$net.result * (max(data$Outcome) - min(data$Outcome))) + min(data$Outcome)
plot(test$Outcome, predict_testNN_2_mod_act, col='red', pch=16, ylab = "predicted rating NN", xlab = "Outcome")
abline(0,1)
RMSE.NN_2_mode_act = (sum((test$Outcome - predict_testNN1)^2) / nrow(data)) ^ 0.5
RMSE.NN_2_mode_act
predict_testNN_2_mod_act <- ifelse(predict_testNN_2_mod_act>0.5, 1, 0)

results_moded_trail <- data.frame(actual= test$Outcome, prediction = predict_testNN_2_mod_act)
print(results_moded_trail)
results_moded_trail <- as.data.frame(results_moded_trail)
confusionMatrix(as.factor(results_moded_trail$prediction), as.factor(results_moded_trail$actual))

#NN with higher learning rate
set.seed(2)
NN_2_mode_learning = neuralnet(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , train,learningrate = 0.5, hidden =2,linear.output = T )
plot(NN_2_mode_learning)

#Evaluating

predict_testNN_2_mod_learning = neuralnet::compute(NN_2_mode_learning, test[,c(1:8)])
predict_testNN_2_mod_learning = (predict_testNN_2_mod_learning$net.result * (max(data$Outcome) - min(data$Outcome))) + min(data$Outcome)
plot(test$Outcome, predict_testNN_2_mod_learning, col='red', pch=16, ylab = "predicted rating NN", xlab = "Outcome")
abline(0,1)
RMSE.NN_2_mode_learning = (sum((test$Outcome - predict_testNN1)^2) / nrow(data)) ^ 0.5
RMSE.NN_2_mode_act
predict_testNN_2_mod_learning <- ifelse(predict_testNN_2_mod_learning>0.5, 1, 0)

results_learning_trail <- data.frame(actual= test$Outcome, prediction = predict_testNN_2_mod_learning)
print(results_learning_trail)
results_learning_trail <- as.data.frame(results_learning_trail)
confusionMatrix(as.factor(results_learning_trail$prediction), as.factor(results_learning_trail$actual))
