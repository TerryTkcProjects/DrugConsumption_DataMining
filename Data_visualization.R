setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Programming App/R/Statistical Method I/Project")

library(reshape2)
library(dplyr)
library(cowplot)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

psycho_data <- read.csv(file = "Psycho.csv")
attach(psycho_data)

psycho_data$percentage <- Drug/Total
psycho_data$transformed <- log((psycho_data$percentage/(1-psycho_data$percentage)))

psycho_data$MeanAge <- as.numeric(psycho_data$MeanAge)
psycho_data$Sex <- as.factor(psycho_data$Sex)
psycho_data$GHQ <- as.factor(psycho_data$GHQ)
psycho_data$Age <- as.factor(psycho_data$Age)
psycho_data$Age <- factor(psycho_data$Age, levels = c("16-29", "30-44", "45-64","65-74",">74"))

pairs(psycho_data)




#Refer to https://www.r-bloggers.com/how-to-summarize-a-data-frame-by-groups-in-r/
Sex_group <- psycho_data %>%
  select(Sex,Drug) %>%
  group_by(Sex) %>%
  summarise(Drug = sum(Drug))

GHQ_group <- psycho_data %>%
  select(GHQ,Drug) %>%
  group_by(GHQ) %>%
  summarise(Drug = sum(Drug))

Age_group <- psycho_data %>%
  select(Age, Drug) %>%
  group_by(Age) %>%
  summarise(Drug = sum(Drug))

plot_1 <- ggplot(Sex_group, aes(x = Sex_group$Sex, y = Sex_group$Drug)) +
  geom_bar(fill = "blue", stat = "identity") +
  geom_text(aes(label = Sex_group$Drug), vjust = -0.3) + 
  labs(y = "Frequency") +
  scale_x_discrete(name="Gender", breaks=0:1,
                   labels=c("Male","Female")) +
  theme_pubclean()

plot_2 <- ggplot(GHQ_group, aes(x = GHQ_group$GHQ, y = GHQ_group$Drug)) +
  geom_bar(fill = "orange", stat = "identity") +
  geom_text(aes(label = GHQ_group$Drug), vjust = -0.3) + 
  labs(y = "Frequency") +
  scale_x_discrete(name="GHQ_score", breaks=0:1,
                   labels=c("Low","High")) +
  theme_pubclean()

plot_3 <- ggplot(Age_group, aes(x = Age_group$Age, y = Age_group$Drug)) +
  geom_bar(fill = "green", stat = "identity") +
  geom_text(aes(label = Age_group$Drug), vjust = -0.3) + 
  labs(y = "Frequency") +
  scale_x_discrete(name="Age_group") +
  theme_pubclean()


plot_grid(plot_1, plot_2, plot_3)

#########################################

Sex_Total <- psycho_data %>%
  select(Sex,Total) %>%
  group_by(Sex) %>%
  summarise(Total = (sum(Total)))

GHQ_Total <- psycho_data %>%
  select(GHQ,Total) %>%
  group_by(GHQ) %>%
  summarise(Total = (sum(Total)))


Age_Total <- psycho_data %>%
  select(Age,Total) %>%
  group_by(Age) %>%
  summarise(Total = (sum(Total)))

Sex_group <- psycho_data %>%
  select(Sex,Drug,Total) %>%
  group_by(Sex) %>%
  summarise(percentage = (sum(Drug)/sum(Total)))

GHQ_group <- psycho_data %>%
  select(GHQ,Drug,Total) %>%
  group_by(GHQ) %>%
  summarise(percentage = (sum(Drug)/sum(Total)))

Age_group <- psycho_data %>%
  select(Age,Drug,Total) %>%
  group_by(Age) %>%
  summarise(percentage = (sum(Drug)/sum(Total)))

plot_4 <- ggplot(Sex_group, aes(x = Sex_group$Sex, y = Sex_group$percentage)) +
  geom_bar(fill = "blue", stat = "identity") +
  geom_text(aes(label = paste0(round(Sex_group$percentage*100,2),"%"), vjust = -0.3)) + 
  labs(y = "Percent") +
  scale_x_discrete(name="Gender", breaks=0:1,
                   labels=c("Male","Female")) +
  theme_pubclean()

plot_5 <- ggplot(GHQ_group, aes(x = GHQ_group$GHQ, y = GHQ_group$percentage)) +
  geom_bar(fill = "orange", stat = "identity") +
  geom_text(aes(label = paste0(round(GHQ_group$percentage*100,2),"%"), vjust = -0.3)) + 
  labs(y = "Percent") +
  scale_x_discrete(name="GHQ_score", breaks=0:1,
                   labels=c("Low","High")) +
  theme_pubclean()

plot_6 <- ggplot(Age_group, aes(x = Age_group$Age, y = Age_group$percentage)) +
  geom_bar(fill = "green", stat = "identity") +
  geom_text(aes(label = paste0(round(Age_group$percentage*100,2),"%"), vjust = -0.3)) + 
  labs(y = "Percent") +
  scale_x_discrete(name="Age_group") +
  theme_pubclean()


plot_grid(plot_4,plot_5,plot_6)

Sex_Age_group <- psycho_data %>%
  select(Sex,Age,Drug) %>%
  group_by(Sex,Age) %>%
  summarise(Drug = sum(Drug))

plot_7 <- ggplot(Sex_Age_group, aes(x = Sex_Age_group$Age, y = Sex_Age_group$Drug, fill = Sex_Age_group$Sex)) +
  geom_bar(stat = 'identity', position = position_dodge(preserve = 'single')) +
  geom_text(aes(label = Sex_Age_group$Drug)) + 
  labs(y = "Frequency", fill="Gender") +
  scale_x_discrete(name="Age_group") +
  theme_pubclean()

Sex_Age <- psycho_data %>%
  select(Sex,Age,Drug,Total) %>%
  group_by(Sex,Age) %>%
  summarise(percentage = (sum(Drug)/sum(Total)))

plot_8 <- ggplot(Sex_Age, aes(x = Sex_Age$Age, y = Sex_Age$percentage, fill = Sex_Age$Sex)) +
  geom_bar(stat = 'identity', position = position_dodge(preserve = 'single')) +
  geom_text(aes(label = paste0(round(Sex_Age$percentage*100,2),"%"))) + 
  labs(y = "Percentage", fill="Gender") +
  scale_x_discrete(name="Age_group") +
  theme_pubclean()

plot_grid(plot_7,plot_8)
