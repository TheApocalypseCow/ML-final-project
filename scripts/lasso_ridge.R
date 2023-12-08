library(tidyverse)
library(ISLR)
library(glmnet)
library(gbm)
library(ROCR)
library(glmnet)
library(dplyr)
library(tidyr)
library(caret)

data.raw <- read.csv("data/final_dataset_cleaned.csv")

# adjust DATE formatting
data.raw$DATE <- as.POSIXct(data$DATE, format = "%m/%d/%Y")

# convert USREC columns to "Yes" or "No" factors
data.raw <- data.raw %>%
  mutate_at(vars(USREC1, USREC3, USREC6, USREC12), ~factor(ifelse(. == 1, "Yes", "No")))

set.seed(333)

test = sample(1:nrow(data.raw), 102)
test.recc = data.raw[test,]
train.recc = data.raw[-test,]

# Standardizing Data
# Lasso and Ridge will be very scale dependent so for some columns I will standardize the values

standard.data <- data.raw %>% mutate_at(c(4,5,6,7,10), ~(scale(.) %>% as.vector))

# glmnet uses matrices to process

matrix.data.1 <- model.matrix(USREC1~.-DATE-USREC-USREC3-USREC6-USREC12, standard.data)
matrix.data.3 <- model.matrix(USREC3~.-DATE-USREC-USREC1-USREC6-USREC12, standard.data)
matrix.data.6 <- model.matrix(USREC6~.-DATE-USREC-USREC3-USREC1-USREC12, standard.data)
matrix.data.12 <- model.matrix(USREC12~.-DATE-USREC-USREC3-USREC6-USREC1, standard.data)

test.recc.standard.1 = matrix.data.1[test,]
train.recc.standard.1 = matrix.data.1[-test,]

test.recc.standard.3 = matrix.data.3[test,]
train.recc.standard.3 = matrix.data.3[-test,]

test.recc.standard.6 = matrix.data.6[test,]
train.recc.standard.6 = matrix.data.6[-test,]

test.recc.standard.12 = matrix.data.12[test,]
train.recc.standard.12 = matrix.data.12[-test,]

y.train.1m = train.recc$USREC1
y.train.3m = train.recc$USREC3
y.train.6m = train.recc$USREC6
y.train.12m = train.recc$USREC12

x.test.standard = test.recc.standard[2:11]

y.test.1m = test.recc$USREC1
y.test.3m = test.recc$USREC3
y.test.6m = test.recc$USREC6
y.test.12m = test.recc$USREC12

#Ridge Regression

#Creating lamda list
lambda.list.ridge = 1000 * exp(seq(0, log(1e-5), length = 100))

#Creating models
ridge_model_1 <- glmnet(train.recc.standard.1, y.train.1m, alpha=0,lamda=lambda.list.ridge, family = "binomial")


ridge_model_3 <- glmnet(train.recc.standard.3, y.train.3m, alpha=0,lamda=lambda.list.ridge, family = "binomial")


ridge_model_6 <- glmnet(train.recc.standard.6, y.train.6m, alpha=0,lamda=lambda.list.ridge, family = "binomial")


ridge_model_12 <- glmnet(train.recc.standard.12, y.train.12m, alpha=0,lamda=lambda.list.ridge, family = "binomial")

#Cross validating models
cv.out.ridge.1 <- cv.glmnet(train.recc.standard.1, y.train.1m, alpha=0,folds=20, family="binomial")
cv.out.ridge.3 <- cv.glmnet(train.recc.standard.3, y.train.3m, alpha=0,folds=20, family="binomial")
cv.out.ridge.6 <- cv.glmnet(train.recc.standard.6, y.train.6m, alpha=0,folds=20, family="binomial")
cv.out.ridge.12 <- cv.glmnet(train.recc.standard.12, y.train.12m, alpha=0,folds=20, family="binomial")

#Ploting lamda values 1 month
plot.new()
plot(cv.out.ridge.1)
title("Lamda Values of Ridge model 1-month")
abline(v = log(cv.out.ridge.1$lambda.min), col="red", lwd=3, lty=2)
bestlam.1 = cv.out.ridge.1$lambda.min

#Ploting lamda values 3 month
plot.new()
plot(cv.out.ridge.3)
title("Lamda Values of Ridge model 3-month")
abline(v = log(cv.out.ridge.3$lambda.min), col="red", lwd=3, lty=2)
bestlam.3 = cv.out.ridge.3$lambda.min

#Ploting lamda values 6 month
plot.new()
plot(cv.out.ridge.6)
title("Lamda Values of Ridge model 6-month")
abline(v = log(cv.out.ridge.6$lambda.min), col="red", lwd=3, lty=2)
bestlam.6 = cv.out.ridge.6$lambda.min

#Ploting lamda values 12 month
plot.new()
plot(cv.out.ridge.12)
title("Lamda Values of Ridge model 12-month")
abline(v = log(cv.out.ridge.12$lambda.min), col="red", lwd=3, lty=2)
bestlam.12 = cv.out.ridge.12$lambda.min

#Confusion Matrix 1 month 
print("1 month Training")
ridge.prediction.train.1 <- factor(predict(ridge_model_1, s=bestlam.1, newx = train.recc.standard.1,type="class"))
confusionMatrix(ridge.prediction.train.1, y.train.1m, positive = "Yes")

print("1 month Test")
ridge.prediction.test.1 <- factor(predict(ridge_model_1, s=bestlam.1, newx = test.recc.standard.1, type="class"))
confusionMatrix(ridge.prediction.test.1, y.test.1m, positive = "Yes")

#Confusion Matrix 3 month 
print("3 month Training")
ridge.prediction.train.3 <- factor(predict(ridge_model_3, s=bestlam.3, newx = train.recc.standard.3,type="class"))
confusionMatrix(ridge.prediction.train.3, y.train.3m, positive = "Yes")

print("3 month Test")
ridge.prediction.test.3 <- factor(predict(ridge_model_3, s=bestlam.3, newx = test.recc.standard.3, type="class"))
confusionMatrix(ridge.prediction.test.3, y.test.3m, positive = "Yes")

#Confusion Matrix 6 month 
print("6 month Training")
ridge.prediction.train.6 <- factor(predict(ridge_model_6, s=bestlam.6, newx = train.recc.standard.6,type="class"))
confusionMatrix(ridge.prediction.train.6, y.train.6m, positive = "Yes")

print("6 month Test")
ridge.prediction.test.6 <- factor(predict(ridge_model_6, s=bestlam.6, newx = test.recc.standard.6, type="class"))
confusionMatrix(ridge.prediction.test.6, y.test.6m, positive = "Yes")

#Confusion Matrix 12 month 
print("12 month Training")
ridge.prediction.train.12 <- factor(predict(ridge_model_12, s=bestlam.12, newx = train.recc.standard.12,type="class"))
confusionMatrix(ridge.prediction.train.12, y.train.12m, positive = "Yes")

print("12 month Test")
ridge.prediction.test.12 <- factor(predict(ridge_model_12, s=bestlam.12, newx = test.recc.standard.12, type="class"))
confusionMatrix(ridge.prediction.test.12, y.test.12m, positive = "Yes")


###############################           LASSO MODEL               ##############################################

# Training each model

lasso_model_1 <- glmnet(train.recc.standard.1, y.train.1m, alpha=1,lamda=lambda.list.ridge, family = "binomial")
lasso_model_3 <- glmnet(train.recc.standard.3, y.train.3m, alpha=1,lamda=lambda.list.ridge, family = "binomial")
lasso_model_6 <- glmnet(train.recc.standard.6, y.train.6m, alpha=1,lamda=lambda.list.ridge, family = "binomial")
lasso_model_12 <- glmnet(train.recc.standard.12, y.train.12m, alpha=1,lamda=lambda.list.ridge, family = "binomial")

# CV tests to find optimal value of lamda


cv.out.lasso.6 = cv.glmnet(train.recc.standard.6, y.train.6m, alpha = 1, family = "binomial")
cv.out.lasso.12 = cv.glmnet(train.recc.standard.12, y.train.12m, alpha = 1, family = "binomial")

# 1 month CV Tests
cv.out.lasso.1 = cv.glmnet(train.recc.standard.1, y.train.1m, alpha = 1, family = "binomial")
plot.new()
title("Lamda Values of Lasso model 1-month")
plot(cv.out.lasso.1)
abline(v = log(cv.out.lasso.1$lambda.min), col="red", lwd=3, lty=2)
bestlam.1 = cv.out.lasso.1$lambda.min

# 3 month CV Tests
cv.out.lasso.3 = cv.glmnet(train.recc.standard.3, y.train.3m, alpha = 1, family = "binomial")
plot.new
title("Lamda Values of Lasso model 3-month")
plot(cv.out.lasso.3)
abline(v = log(cv.out.lasso.3$lambda.min), col="red", lwd=3, lty=2)
bestlam.3 = cv.out.lasso.3$lambda.min


# 6 month CV Tests
cv.out.lasso.6 = cv.glmnet(train.recc.standard.6, y.train.6m, alpha = 1, family = "binomial")
plot.new()
title("Lamda Values of Lasso model 6-month")
plot(cv.out.lasso.6)
abline(v = log(cv.out.lasso.6$lambda.min), col="red", lwd=3, lty=2)
bestlam.6 = cv.out.lasso.6$lambda.min

# 12 month CV Tests
cv.out.lasso.12 = cv.glmnet(train.recc.standard.12, y.train.12m, alpha = 1, family = "binomial")
plot.new()
title("Lamda Values of Lasso model 12-month")
plot(cv.out.lasso.12)
abline(v = log(cv.out.lasso.12$lambda.min), col="red", lwd=3, lty=2)
bestlam.12 = cv.out.lasso.12$lambda.min

#Lasso coefficient estimates 1 month
(lasso.model.coef.1 <- predict(cv.out.lasso.1, type="coefficients",s=bestlam.1))

#Lasso coefficient estimates 3 month
(lasso.model.coef.3 <- predict(cv.out.lasso.3, type="coefficients",s=bestlam.3))

#Lasso coefficient estimates 6 month
(lasso.model.coef.6 <- predict(cv.out.lasso.6, type="coefficients",s=bestlam.6))

#Lasso coefficient estimates 12 month
(lasso.model.coef.12 <- predict(cv.out.lasso.12, type="coefficients",s=bestlam.12))

#Confusion Matrix 1 month 
print("1 month Training")
lasso.prediction.train.1 <- factor(predict(lasso_model_1, s=bestlam.1, newx = train.recc.standard.1,type="class"))
confusionMatrix(lasso.prediction.train.1, y.train.1m, positive = "Yes")

print("1 month Test")
lasso.prediction.test.1 <- factor(predict(lasso_model_1, s=bestlam.1, newx = test.recc.standard.1, type="class"))
confusionMatrix(lasso.prediction.test.1, y.test.1m, positive = "Yes")

#Confusion Matrix 3 month 
print("3 month Training")
lasso.prediction.train.3 <- factor(predict(lasso_model_3, s=bestlam.3, newx = train.recc.standard.3,type="class"))
confusionMatrix(lasso.prediction.train.3, y.train.3m, positive = "Yes")

print("3 month Test")
lasso.prediction.test.3 <- factor(predict(lasso_model_3, s=bestlam.3, newx = test.recc.standard.3, type="class"))
confusionMatrix(lasso.prediction.test.3, y.test.3m, positive = "Yes")

#Confusion Matrix 6 month 
print("6 month Training")
lasso.prediction.train.6 <- factor(predict(lasso_model_6, s=bestlam.6, newx = train.recc.standard.6,type="class"))
confusionMatrix(lasso.prediction.train.6, y.train.6m, positive = "Yes")

print("6 month Test")
lasso.prediction.test.6 <- factor(predict(lasso_model_6, s=bestlam.6, newx = test.recc.standard.6, type="class"))
confusionMatrix(lasso.prediction.test.6, y.test.6m, positive = "Yes")

#Confusion Matrix 12 month 
print("12 month Training")
lasso.prediction.train.12 <- factor(predict(lasso_model_12, s=bestlam.12, newx = train.recc.standard.12,type="class"))
confusionMatrix(lasso.prediction.train.12, y.train.12m, positive = "Yes")

print("12 month Test")
lasso.prediction.test.12 <- factor(predict(lasso_model_12, s=bestlam.12, newx = test.recc.standard.12, type="class"))
confusionMatrix(lasso.prediction.test.12, y.test.12m, positive = "Yes")
