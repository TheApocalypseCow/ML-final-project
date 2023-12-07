# This script contains the exploratory data anlaysis visuals
library(ggplot2)
library(corrplot)
library(reshape2)
library(ggcorrplot)

# read in cleaned dataset
#hello
data <- read.csv("data/final_dataset_cleaned.csv")

# adjust DATE formatting
data$DATE <- as.POSIXct(data$DATE, format = "%m/%d/%Y")

# Time vs T10Y3M
ggplot(data, aes(x = DATE, y = T10Y3M, color = T10Y3M)) +
  geom_line() +
  labs(title = "Time vs T10Y3M",
       x = "Time",
       y = "T10Y3M") +
  theme_minimal() 

# Time vs UNRATE
ggplot(data, aes(x = DATE, y = UNRATE, color = UNRATE)) +
  geom_line() +
  labs(title = "Time vs UNRATE",
       x = "Time",
       y = "UNRATE") +
  theme_minimal() 

# Time vs RPIT
ggplot(data, aes(x = DATE, y = RPIT, color = RPIT)) +
  geom_line() +
  labs(title = "Time vs RPIT",
       x = "Time",
       y = "RPIT") +
  theme_minimal() 

# Time vs RPIT
ggplot(data, aes(x = DATE, y = RPIT, color = RPIT)) +
  geom_line() +
  labs(title = "Time vs RPIT",
       x = "Time",
       y = "RPIT") +
  theme_minimal() 

# Time vs PAYEMS
ggplot(data, aes(x = DATE, y = PAYEMS, color = PAYEMS)) +
  geom_line() +
  labs(title = "Time vs PAYEMS",
       x = "Time",
       y = "PAYEMS") +
  theme_minimal() 

# Time vs INDPRO
ggplot(data, aes(x = DATE, y = INDPRO, color = INDPRO)) +
  geom_line() +
  labs(title = "Time vs INDPRO",
       x = "Time",
       y = "INDPRO") +
  theme_minimal() 

# Time vs CORP
ggplot(data, aes(x = DATE, y = CORP, color = CORP)) +
  geom_line() +
  labs(title = "Time vs CORP",
       x = "Time",
       y = "CORP") +
  theme_minimal() 

# Mention NA values starting 2013 (2013-2023 data not available)


# Time vs HTS
ggplot(data, aes(x = DATE, y = HTS, color = HTS)) +
  geom_line() +
  labs(title = "Time vs HTS",
       x = "Time",
       y = "HTS") +
  theme_minimal() 

# Time vs CPIAI
ggplot(data, aes(x = DATE, y = CPIAI, color = CPIAI)) +
  geom_line() +
  labs(title = "Time vs CPIAI",
       x = "Time",
       y = "CPIAI") +
  theme_minimal() 

# Correlation graph of all factors except time 

dateless_data <- data[, !names(data) %in% c("DATE",)]
correlation_matrix <- cor(dateless_data)
ggcorrplot(correlation_matrix, type = "lower", outline.col = "white")

# summary of dataset
summary(data)

