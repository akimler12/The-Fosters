## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: Final Project- The Fosters
# Date:      2024_1_17
# Who:       Alli Kimler



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
# generate summary statistics for conversations
mean(data$conversations)
sd(data$conversations)
hist(data$conversations)
summary(data$conversations)
# generate summary statistics for how many people watch the episodes
mean(data$viewers)
sd(data$viewers)
hist(data$viewers)
summary(data$viewers)
# generate summary statistics for what gender is shown more in an episode
table(data$gender)
# generate summary statistics for how emotional an epsiode made me feel
table(data$emotional)
##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################

ggplot(data, aes(x = EMOTIONAL, y = ROMANTIC)) +
  geom_boxplot() +
  labs(title = "The Fosters",
       x = "EMOTIONAL",
       y = "ROMANTIC") +
  theme_minimal()
anova <- aov(ROMANTIC ~ EMOTIONAL, data = data)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$CONVERSATIONS, data$VIEWERS)
print(linear_plot)
# add x line and y line for means
meany <- mean(data$VIEWERS)
meanx <- mean(data$CONVERSATIONS)

abline(h = meany, col = "black")
abline(v = meanx, col = "black")

linear_relationship <- lm(data$VIEWERS ~ data$CONVERSATIONS, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
plot(data$CONVERSATIONS,residuals (linear_relationship))
abline(h = 0, col = "black")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$GENDER, data$EMOTIONAL)
chisq.test(data$GENDER, data$EMOTIONAL)
