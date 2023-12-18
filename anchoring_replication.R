
library(haven)
library(readxl)
library(tidyr)
library(flexmix)

rm(list=ls())

data <- read_excel("CV data set.xlsx") 

data <- subset(data, BID > -999) 

#data[data == -999] <- NA 

#data <- data[complete.cases(data), ]

# Define the model for each component
model <- FLXMRglm(BID ~ PRICE + PRICE2 + PRICE3)

# Fit the finite mixture model
result <- flexmix(BID ~ PRICE + PRICE2 + PRICE3, data = data, k = 2)

# View the results
summary(result) 

plot(result)

result

(parameters(result))


# Get cluster memberships
cluster_assignments <- clusters(result)

# Add cluster assignments to the original data
data$cluster <- cluster_assignments 

data[data == -999] <- NA

summary_result <- data %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarize(
    mean_BID = mean(BID, na.rm = TRUE),
    sd_BID = sd(BID, na.rm = TRUE),
    mean_PRICE = mean(PRICE, na.rm = TRUE),
    sd_PRICE = sd(PRICE, na.rm = TRUE), 
    mean_TIME = mean(TIME, na.rm = TRUE),
    sd_TIME = sd(TIME, na.rm = TRUE) 
  )

summary_result


