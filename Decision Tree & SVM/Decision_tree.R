df <- read.csv("hypothyroid.csv")
head(df)
dim(df)
#df$Class <- ifelse(df$Class == "negative",0,1)
df = subset(df, select = -c(TBG))
df[df == "?"] <- NA
df[df == "f"] <- 0
df[df == "t"] <- 1
df[df == "n"] <- 0
df[df == "y"] <- 1
df[df == "M"] <- 0
df[df == "F"] <- 1
list_na <- colnames(df)[ apply(df, 2, anyNA) ]
list_na
#Data preprocessing 

df = subset(df, select = -c(referral_source ))
names = colnames(df)
names[1:27]
df[names[1:27]]<- apply(df[names[1:27]], 2, function(x) as.numeric(as.character(x)))
sapply(df, class)

hist(df$age)
install.packages(Hmsic)
library("Hmisc")
describe(df)
library(zoo)
df[names[1:27]]<- na.aggregate(df[names[1:27]])

require(caret)
df$age_z <- scale(df$age, center= TRUE, scale = TRUE )
head(df)


#removing outliers and normalizing age column
install.packages("tidyverse")
install.packages("outliers")
require(outliers)
outliers_scores <- scores(df$age)
head(outliers_scores)
head(df$age_z)
ggplot(df, aes(x = "age_z", y = age_z)) +geom_boxplot()
boxplot(df$age_z)
abline(h = min(df$age_z), col = "Blue")
abline(h = max(df$age_z), col = "Yellow")
abline(h = median(dfd$age_z), col = "Green")
abline(h = quantile(df$age_z, c(0.25, 0.75)), col = "Red")
is_outlier <- outliers_scores > 3 | outliers_scores < -3
df$outlier <- is_outlier
head(df)
df_cleaned_outlier <- df[is_outlier, ]
head(df_cleaned_outlier)
dim(df_cleaned_outlier)
head(df)
library(dplyr)
df_cleaned_outlier = anti_join(df, df_cleaned_outlier)
dim(df)
dim(df_cleaned_outlier)
df_cleaned_outlier
sapply(df, function(x) sum(is.na(x)))
sapply(df, class)
head(df_cleaned_outlier)
df_moded = subset(df_cleaned_outlier, select = -c(age, outlier))

#feature selection using importance
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

set.seed(42)
sample_split <- sample.split(Y = df_cleaned_outlier$Class, SplitRatio = 0.75)
train_set <- subset(x = df_cleaned_outlier, sample_split == TRUE)
test_set <- subset(x = df_cleaned_outlier, sample_split == FALSE)
head(train_set)

model <- rpart(Class ~ ., data = train_set, method = "class") #specify method as class since we are dealing with classification
model

#plot the model
rpart.plot(model)

#Select features by checking feature importance
importances <- varImp(model) #use the varImp() function to determine how much predictive power lies in each feature
importances %>% arrange(desc(Overall))


# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#Make predictions
preds <- predict(model, newdata = test_set, type = "class") #use the predict() function and pass in the testing subset
preds

keeps <- c("age_z","FTI","on_thyroxine","T3","T4U","thyroid_surgery","TSH", "TT4","Class")
reduced_Data2 <- df_cleaned_outlier[keeps]
head(reduced_Data2)

#set.seed(42)
#sample_split1 <- sample.split(Y = reduced_Data2$Class, SplitRatio = 0.75)
#train_set1 <- subset(x = reduced_Data2, sample_split == TRUE)
#test_set1 <- subset(x = reduced_Data2, sample_split == FALSE)
 
#Building model with reduced features 


set.seed(123) #generates a reproducible random sampling

#specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)

#fit a decision tree model and use k-fold CV to evaluate performance
reduced_Data2$Class = as.factor(reduced_Data2$Class)


dtree_fit_gini <- train(Class~., data = reduced_Data2, method = "rpart", parms = list(split = "gini"), trControl = ctrl, tuneLength = 10)

#Step 5: Evaluate - view summary of k-fold CV               
print(dtree_fit_gini) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
dtree_fit_gini$finalModel
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
dtree_fit_gini$resample
geni_predict = predict(dtree_fit_gini, newdata = df_cleaned_outlier)
confusionMatrix(as.factor(geni_predict), as.factor(df_cleaned_outlier$Class))

#Another type of decision Tree
dtree_fit_information <- train(Class~., data = reduced_Data2, method = "rpart", parms = list(split = "information"), trControl = ctrl, tuneLength = 10)

#Step 5: Evaluate - view summary of k-fold CV               
print(dtree_fit_information) #metrics give us an idea of how well the model performed on previously unseen data

#view final model
dtree_fit_information$finalModel
prp(dtree_fit_information$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
dtree_fit_information$resample

pruned_gini <- prune(dtree_fit_gini$finalModel, cp =0.1)
rpart.plot(pruned_gini)
pruned_information <- prune(dtree_fit_information$finalModel, cp =0.1)
rpart.plot(pruned_information)
