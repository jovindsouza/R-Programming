#Building Simple Linear Regression Model

#Importing the dataset
dataset <- read.csv('D:/Predicting_Salaries.csv')

#install the package
install.packages("caTools")
library(caTools)
set.seed(123)

#splitting the data
split <- sample.split(dataset$AnnualSalary, SplitRatio = 3/4)
train <- subset(dataset, split == TRUE)
test <- subset(dataset, split == FALSE)


#Fitting simple linear regression into training and testing sets

linearregressor <- lm(formula = AnnualSalary ~ YearsOfExperience, data = train)
summary(linearregressor)

#Predicting results on test set
y_pred <- predict(linearregressor, newdata = test )
summary(y_pred)



#install.packages('scales')
library(scales)

# Visualising the Training set results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x=train$YearsOfExperience, y=train$AnnualSalary),
             colour = 'red') +
  geom_line (aes( x= train$YearsOfExperience, y=predict(linearregressor, newdata= train)),
             colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Training Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') +
  scale_x_continuous(limits = c(0, 12)) + 
  scale_y_continuous(limits = c(0, 150000)) 

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x=test$YearsOfExperience, y=test$AnnualSalary),
             colour = 'red') +
  geom_line (aes( x= train$YearsOfExperience, y=predict(linearregressor, newdata= train)),
             colour = 'navy') +
  ggtitle ('Annual Salaries of Data Scientists vs Experience in Years (Test Set)') +
  xlab ('Years of Experience') +
  ylab ('Annual Salary') +
  scale_x_continuous(limits = c(0, 12)) + 
  scale_y_continuous(limits = c(0, 150000))