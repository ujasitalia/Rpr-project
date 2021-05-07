# https://www.kaggle.com/abisheksudarshan/customer-segmentation?select=train.csv
library(dplyr)
library(ggplot2)
library(reshape)
library(skimr)
library(GGally)


data <- read.csv("train.csv")
summary(data)

# ----------- cleaning the data -------------- #
sum(is.na(data))
# family_size and work experience has null values
colSums(is.na(data))


# imputing missing values.
data <- data %>% mutate(Family_Size = ifelse(is.na(Family_Size), 1, Family_Size), Work_Experience = ifelse(is.na(Work_Experience), 2, Work_Experience))

# converting data types. 
data <- data %>% mutate(Gender = as.factor(Gender))

table(data$Work_Experience)
table(data$Spending_Score)

# detailed structure after all the cleaning and preprocessing.
skim(data)

# using xtabs to see the count of different combination of variables. 
xtabs(~ Gender+Spending_Score, data)
xtabs(~ Ever_Married+Spending_Score, data)

# data visualization
# age density distribution
ggplot(data, aes(Age)) + geom_histogram(aes(y=..density..), alpha = 0.8, fill = "#fa900c", bins=20) +
    geom_density(fill = "#592e02", alpha = 0.5) +
    geom_vline(aes(xintercept=mean(Age)), linetype="dashed", size=1) +
    ggtitle("Age Density")

# plotting familySize against spending score
ggplot(data, aes( Family_Size, Spending_Score)) + geom_bar(stat = "identity", width = 0.5, aes(fill=Gender)) +
    theme_light() + labs(x = "Family Size", y = "Spending Score", title = "Spending Score against family size gender wise")


# relation between profession and spending categorised by segmentation
ggplot(data, aes(Profession, Spending_Score)) + geom_jitter(aes(color=Gender)) + theme_linedraw() +
    labs(x="Profession", y="Spending Score", title="relation between profession and spending categorised by segmentation")





