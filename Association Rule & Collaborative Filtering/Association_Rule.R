#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)
library(arules)
library(arulesViz)
transaction_data <- read.transactions("transactions.csv", sep = ",")
transaction_data
dim(transaction_data)
summary(transaction_data)
inspect(transaction_data[1:5])
itemFrequency(transaction_data[, 1:3])
itemFrequencyPlot(transaction_data, topN = 10)
image(transaction_data[1:5])
image(sample(transaction_data, 100))

apriori(transaction_data)

transaction_rules <- apriori(groceries, parameter = list(support = 0.002, confidence = 0.20, maxlen = 3))
transaction_rules

summary(transaction_rules)
inspect(transaction_rules[1:3])
inspect(sort(transaction_rules, by = "lift")[1:5])
transactions.sorted <- sort(transaction_rules, by="lift")
subset.matrix <- is.subset(transactions.sorted,transactions.sorted)
subset.matrix
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
subset.matrix
redundant <- apply(subset.matrix, 2, any)
redundant
rules.pruned <- transactions.sorted[!redundant]
inspect(rules.pruned)
plot(rules.pruned, jitter = 0)
plot(rules.pruned, method="graph")


transaction_rules_2 <- apriori(groceries, parameter = list(support = 0.002, confidence = 0.20, maxlen = 2))
transaction_rules_2
summary(transaction_rules_2)
inspect(transaction_rules_2[1:3])
inspect(sort(transaction_rules_2, by = "lift")[1:5])

