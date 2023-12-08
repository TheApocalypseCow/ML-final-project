# read in cleaned dataset
#hello
library(tidyverse)
library(ISLR)
library(glmnet)
library(tree)
library(maptree)
library(randomForest)
library(gbm)
library(ROCR)
library(dplyr)

data <- read.csv("data/final_dataset_cleaned.csv")

# adjust DATE formatting
data$DATE <- as.POSIXct(data$DATE, format = "%m/%d/%Y")

# convert USREC columns to "Yes" or "No" factors
data <- data %>%
  mutate_at(vars(USREC1, USREC3, USREC6, USREC12), ~factor(ifelse(. == 1, "Yes", "No")))

#implement a logistic regression as a benchmark method 
logistic_data_12 <- glm(USREC12~ .-DATE -USREC -USREC1 -USREC3 -USREC6,data=data,family = "binomial")
summary(logistic_data_12)

logistic_data_6 <- glm(USREC6~ .-DATE -USREC -USREC1 -USREC3 -USREC12,data=data,family = "binomial")
summary(logistic_data_6)

logistic_data_3 <- glm(USREC3~ .-DATE -USREC -USREC1 -USREC6 -USREC12,data=data,family = "binomial")
summary(logistic_data_3)

logistic_data_1 <- glm(USREC1~ .-DATE -USREC -USREC3 -USREC6 -USREC12,data=data,family = "binomial")
summary(logistic_data_1)

# fit a decision tree on the data 
set.seed(123)

tree.data_12 = tree(USREC12 ~.-DATE -USREC -USREC1 -USREC3 -USREC6, data = data)
cv.data_12 <- cv.tree(tree.data_12, FUN=prune.misclass, K=5)
best_size <- min(cv.data_12$size[cv.data_12$dev == min(cv.data_12$dev)])
best_tree12.cv = prune.misclass (tree.data_12, best=best_size)

plot(best_tree12.cv)
text(best_tree12.cv, pretty=0, col = "blue", cex = .5)
title("Best Tree of Size 11 for USREC12")

#---------------------------
#USREC12 Random Forest Model
# Create training and test datasets
index <- sample(1:nrow(data), 0.7 * nrow(data))
data_12.train <- data[index, ]
data_12.test <- data[-index, ]

#Boosted tree
boosted.tree.data12 <- gbm(USREC12~.-DATE -USREC -USREC1 -USREC3 -USREC6,
                         distribution = "gaussian", data = data_12.train,
                         n.trees = 1000, shrinkage = 0.01)
summary(boosted.tree.data12)

#Importance Score method 
is.tree.data12 <- randomForest(USREC12~.-DATE -USREC -USREC1 -USREC3 -USREC6,
                               data=data_12.train, importance = TRUE)
print(is.tree.data12)

importance_scores <- importance(is.tree.data12)
ordered_variables <- importance_scores[order(importance_scores[, 1], decreasing = TRUE), , drop = FALSE]
print(ordered_variables)

#---------------------------
#USREC1 Random Forest Model 
index <- sample(1:nrow(data), 0.7 * nrow(data))
data_1.train <- data[index, ]
data_1.test <- data[-index, ]

boosted.tree.data1 <- gbm(USREC1~.-DATE -USREC -USREC12 -USREC3 -USREC6,
                           distribution = "gaussian", data = data_1.train,
                           n.trees = 1000, shrinkage = 0.01)
summary(boosted.tree.data1)

#Importance Score method for USREC1
is.tree.data1 <- randomForest(USREC1~.-DATE -USREC -USREC12 -USREC3 -USREC6,
                               data=data_1.train, importance = TRUE)
print(is.tree.data1)

importance_scores <- importance(is.tree.data1)
ordered_variables <- importance_scores[order(importance_scores[, 1], decreasing = TRUE), , drop = FALSE]
print(ordered_variables)

#---------------------------
#USREC3 Random Forest Model 
index <- sample(1:nrow(data), 0.7 * nrow(data))
data_3.train <- data[index, ]
data_3.test <- data[-index, ]

boosted.tree.data3 <- gbm(USREC3~.-DATE -USREC -USREC12 -USREC1 -USREC6,
                          distribution = "gaussian", data = data_3.train,
                          n.trees = 1000, shrinkage = 0.01)
summary(boosted.tree.data3)

#Importance Score method for USREC3
is.tree.data3 <- randomForest(USREC3~.-DATE -USREC -USREC12 -USREC1 -USREC6,
                              data=data_3.train, importance = TRUE)
print(is.tree.data3)

importance_scores <- importance(is.tree.data3)
ordered_variables <- importance_scores[order(importance_scores[, 1], decreasing = TRUE), , drop = FALSE]
print(ordered_variables)

#---------------------------
#USREC6 Random Forest Model 
index <- sample(1:nrow(data), 0.7 * nrow(data))
data_6.train <- data[index, ]
data_6.test <- data[-index, ]

boosted.tree.data6 <- gbm(USREC6~.-DATE -USREC -USREC12 -USREC1 -USREC3,
                          distribution = "gaussian", data = data_6.train,
                          n.trees = 1000, shrinkage = 0.01)
summary(boosted.tree.data6)

#Importance Score method for USREC6
is.tree.data6 <- randomForest(USREC6~.-DATE -USREC -USREC12 -USREC1 -USREC3,
                              data=data_6.train, importance = TRUE)
print(is.tree.data6)

importance_scores <- importance(is.tree.data6)
ordered_variables <- importance_scores[order(importance_scores[, 1], decreasing = TRUE), , drop = FALSE]
print(ordered_variables)

head(data)
