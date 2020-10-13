

#Loading the library
library(tidyverse)
library(neuralnet)
library(caret)
library(MASS)


#Data Exploration
data("Boston")
df <- Boston
str(df)
?Boston

#Histogram
hist(df$age, col="green", main="Age Histogram", xlab="age")


#Data Splitting
row <- createDataPartition(df$age, times=1, p=0.8, list=F)
train <- df[row,]
test <- df[-rows,]
dim(train)
dim(test)


#Model Creation

model_lm <- train(age ~.,
                  data=train,
                  method ="lm",
                  trControl = trainControl(method ="repeatedcv",
                                        number = 2, repeats =2))
model_rf <- train(age~.,
                  data= train,
                  method= "ranger",
                  trControl=trainControl(method="repeatedcv", 
                                         number = 2, repeats = 2))

model_gbm <- train(age~.,
                  data= train,
                  method= "gbm",
                  trControl=trainControl(method="repeatedcv", 
                                         number = 2, repeats = 2))

sample <- resamples(list(Linear = model_lm,
                         Forest= model_rf, 
                         GBM = model_gbm))

#Tabulation
bwplot(sample)
dotplot(sample)
 
#Summary
summary(sample)


# Center/Scale/PCA

modranger <- train(age ~ ., train,
                   method = "ranger",
                   trControl = trainControl(method = "repeatedcv",
                                            number = 10, repeats = 2),
                   preProcess = c("center", "scale", "pca"))
modlm <- train(age ~ ., train,
               method = "lm",
               trControl = trainControl(method = "repeatedcv",
                                        number = 10, repeats = 2),
               preProcess = c("center", "scale", "pca"))

modgbm <- train(age ~ ., train,
                method = "gbm",
                trControl = trainControl(method = "repeatedcv",
                                         number = 10, repeats = 2),
                preProcess = c("center", "scale", "pca"))


smpl2 <- resamples(list(Forest = modranger, LM = modlm, GBM = modgbm))

bwplot(smpl2)
summary(smpl2)


# Model on the Test set

#For Random Forest

#use the predict function using the model on the test set (data the model hasn't seen)
predrf <- predict(modranger, test)

#RMSE

error <- predrf - test$age

rmse <- sqrt(mean(error^2))

rmse

#For Linear Model
predlm <- predict(modlm, test)

#RMSE

errorlm <- predlm - test$age

rmselm <- sqrt(mean(errorlm^2))

rmselm

#For GBM
predgbm <- predict(modgbm, test)

#RMSE

errorgbm <- predgbm - test$age

rmsegbm <- sqrt(mean(errorgbm^2))

rmsegbm