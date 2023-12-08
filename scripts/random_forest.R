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

