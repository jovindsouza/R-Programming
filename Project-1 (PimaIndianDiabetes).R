#Install all the necessary Packages

install.packages("neuralnet")
install.packages("caret")
install.packages("ranger")
install.packages("tidyverse")
install.packages("mlbench")
install.packages("e1071")


#Load the Packages

library(neuralnet)
library(caret)
library(ranger)
library(tidyverse)
library(mlbench)
library(e1071)

#Loading the dataset

data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes
str(df)
?PimaIndiansDiabetes

#Plotting the diabetes entries
ggplot(df, aes(diabetes, fill = factor(diabetes)))+ geom_bar()


#Variable creation and the string values into numerical values
df$binary <- ifelse(df$diabetes == "neg",0, 1)
str(df)

#Data Partition
rows <- createDataPartition(df$binary, times =1, p= 0.7, list= F)

#Subsetting the data
train <- df[rows,]
test <- df[-rows,]

#Dimensions
dim(train)
dim(test)

#Removing the unwanted variable
train <- train[,-9]
test <- test[, -9]


#Model Creation
model <- train(as.factor(binary)~. , 
               data =train, 
               method="ranger", 
               trControl = trainControl(method ="repeatedcv", 
                                        number = 2, repeats = 2))
model


#Pedicting the results

pred_train <- predict(model, train)
pred_test <- predict(model, test)

pred_train

#Creating Confusion Matrix
confusionMatrix(pred_train, as.factor(train$binary))
confusionMatrix(pred_test, as.factor(test$binary))
