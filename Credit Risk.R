install.packages("tidyverse")
install.packages("tidymodels")
install.packages("tidyr")
install.packages("corrplot")
install.packages("superml")
install.packages("DataExplorer")
install.packages("caret")
install.packages("rpart.plot")
install.packages("tree")
install.packages("rsample")

library(dplyr)
library(tidyverse)
library(tidymodels)
library(tidyr)
library(corrplot)
library(superml)
library(DataExplorer)
library(caret)
library(rpart.plot)
library(tree)
library(rsample)

#Load data into variable called "dat"
dat = read.csv("~/Documents/Data Science Projects/Credit Risk/credit_customers.csv")

#Convert to dataframe
df = as.data.frame(dat)
dfcopy = df

#View data
head(df, n = 25)

#Observing the structure of the dataframe. 
str(df)

#List of all variable names
names(df)

#EXPLORATORY DATA ANALYSIS

plot_str(df)
plot_missing(df)

#For numerical variables
plot_histogram(df)
plot_correlation(df, type = "continuous")

#For catagorical variables
plot_bar(df)

#Bar plots of catagorical explanatory variables 

ggplot(data = df, aes(x = savings_status)) +
  geom_bar()

ggplot(data = df, aes(x = credit_history, fill = job)) +
  geom_bar(position = "fill")

ggplot(data = df, aes(x = checking_status)) +
  geom_bar()

ggplot(data = df, aes(x = purpose)) +
  geom_bar()

ggplot(data = df, aes(x = employment)) +
  geom_bar()

ggplot(data = df, aes(x = personal_status)) +
  geom_bar()

ggplot(data = df, aes(x = other_parties)) +
  geom_bar()

ggplot(data = df, aes(x = property_magnitude)) +
  geom_bar()

ggplot(data = df, aes(x = other_payment_plans)) +
  geom_bar()

ggplot(data = df, aes(x = housing)) +
  geom_bar()

ggplot(data = df, aes(x = job)) +
  geom_bar()

ggplot(data = df, aes(x = own_telephone)) +
  geom_bar()

ggplot(data = df, aes(x = foreign_worker)) +
  geom_bar()


#DATA SPLITTING 
set.seed(2023)
split = initial_split(df, prop = 0.8)
train = training(split)
test = testing(split)

#TRAINING AND FITTING MODEL

#Creating decision trees
model1 = rpart(df$class ~ ., data = df, method = "class")
model2 = rpart(df$class ~ df$credit_amount + df$duration, data = df, method = "class")

#plotting decision trees
rpart.plot(model1)
rpart.plot(model2)

#Evaluating decision trees
printcp(model1)
printcp(model2)

#Use model 1 for test dataset, and number of splits should be 8

bestcp = model1$cptable[which.min(model1$cptable[,"xerror"]), "CP"]
pruned_model1 = prune(model1, cp = bestcp)
rpart.plot(pruned_model1)

pred = predict(pruned_model1, test, type = "class")
table(pred, test$class)

x = (33+129)/(33+129+12+26)
sprintf("Percentage of correct good credit risk predictions: %f", x)

