install.packages("tidyverse")
install.packages("skimr")
install.packages("psych")

library(tidyverse)
library(skimr)
library(psych) 

#loading data
train <- read.csv("C:/Users/Zaur/Downloads/train.csv")
test <- read.csv("C:/Users/Zaur/Downloads/test.csv")

#Explore data
str(train)
summary(train)
head(train)
skim(train)

#data cleaning
colSums(is.na(train))
train$Age[is.na(train$Age)] <- median(train$Age, na.rm = TRUE)
train <- na.omit(train)

#Exploratory  Data Analysis(EDA)
describe(train)

#Visualize Distributions

ggplot(train, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = 'blue', color = 'white') +
  theme_minimal()


ggplot(train, aes(x = HomePlanet, fill = Transported)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(y = "Proportion", title = "Transported by HomePlanet")


ggplot(train, aes(x = CryoSleep, fill = Transported)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(y = "Proportion", title = "CryoSleep vs Transported")

#Create new features
#new column for total spend
train <- train %>%
  mutate(TotalSpend = RoomService + FoodCourt + ShoppingMall + Spa + VRDeck)

# binary column for high spenders
train <- train %>%
  mutate(HighSpender = ifelse(TotalSpend > median(TotalSpend, na.rm = TRUE), 1, 0))

