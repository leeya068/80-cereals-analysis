View(cereal)

library(tidyverse)
library(readxl)
library(corrplot)

head(cereal)
str(cereal)
summary(cereal)

colSums(is.na(cereal)) #check for missing values

#Exploratory Data Analysis

##Distribution of ratings
ggplot(cereal, aes(rating)) + geom_histogram(bins = 15, fill="steelblue", color="black") + labs(title="Distribution of Cereal Ratings")

##Sugar vs Rating scatter plot
ggplot(cereal, aes(x = sugars, y = rating)) + geom_point(color="darkgreen") + geom_smooth(method="lm", color="red") + labs(title="Sugar vs Rating")

#Correlation Matrix
numeric_data <- cereal %>% select_if(is.numeric)
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method="color", tl.cex=0.7)

#Create Health Score
cereal <- cereal %>%
  mutate(health_score = fiber + protein - sugars - fat - sodium/100)


## Top 10 healthiest cereals
cereal %>%
arrange(desc(health_score)) %>%
select(name, health_score) %>%
head(10)

#Manufacturer Analysis
## Average rating by manufacturer
cereal %>%
group_by(mfr) %>%
summarise(avg_rating = mean(rating)) %>%
arrange(desc(avg_rating))


## Boxplot of ratings by manufacturer
ggplot(cereal, aes(mfr, rating)) + geom_boxplot(fill="lightblue") + labs(title="Ratings by Manufacturer")

#Simple Linear Regression
model <- lm(rating ~ sugars + fiber + protein + fat + calories, data=cereal)
summary(model)

