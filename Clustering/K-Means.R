library(tidyverse)  # data manipulation
library(cluster)  # clustering algorithms
install.packages("factoextra")
library(factoextra)
library("Hmisc")
df <- read.csv("framingham.csv")
keeps <- c("age","male")
df_cleaned <- df[keeps]
head(df_cleaned)
names(df_cleaned)[names(df_cleaned) == "male"] <- "sex"
max = apply(df_cleaned["age"] , 2 , max)
min = apply(df_cleaned["age"], 2 , min)
scaled = as.data.frame(scale(df_cleaned["age"], center = min, scale = max - min))
head(scaled)
df_cleaned["age"] <- scale(df_cleaned["age"], center = min, scale = max - min)
head(df_cleaned)
df_cleaned <- na.omit(df_cleaned)
head(df_cleaned)
sapply(df_cleaned, class)
df_cleaned$age <- as.numeric(df_cleaned$age)
df_cleaned$sex <- as.numeric(df_cleaned$sex)
head(df_cleaned)
describe(df_cleaned)
head(df_cleaned)

set.seed(917)

install.packages("animation")	
library("animation")
set.seed(2345)
kmeans.ani(df_cleaned, 4)

fviz_nbclust(df_cleaned, kmeans, method = "wss")


set.seed(2345)
kmeans.ani(df_cleaned, 5)
fviz_nbclust(df_cleaned, kmeans, method = "silhouette")
set.seed(2345)
kmeans.ani(df_cleaned, 4)




gap_stat <- clusGap(df_cleaned, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

