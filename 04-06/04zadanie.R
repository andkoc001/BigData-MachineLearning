# Andrzej Kocielski, 11.05.2024

# Big Data i uczenie maszynowe - Zadanie 04: 
# Wzorując się na Zadaniu 1 opracuj i zaimplementuj system predykcji zakupów
# (zmienna Purchase) w oparciu o zbiór danych Tayko Software Cataloger 
# (dostępny również w katalogu z zadaniami jako Tayko.csv) przy użyciu segmentacji.


library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)

# Load data
#setwd("/home/ak/Desktop/_moje/_coding/WSB/06-BDiML/04/") # uwaga: ukośniki odpowiednie dla systemu - w moim przypadku Linux
tayko <- read_csv("Tayko.csv")

# Data prep - remove redundant columns
# tayko <- tayko %>% select(-Spending) # Spending causes data leakage!!!


# Modify dataset to include Retency and Monetary data in dataset

ggplot(tayko, aes(x = last_update_days_ago)) + geom_histogram(binwidth = 20) + labs(y = "Observations")
# Convert last_update_days_ago into Retency (categoriesed 1-8)
tayko <- tayko %>%
  mutate(Retency = 
    ifelse(last_update_days_ago < 500, 1, 
    ifelse(last_update_days_ago < 1000, 2, 
    ifelse(last_update_days_ago < 1500, 3, 
    ifelse(last_update_days_ago < 2000, 4, 
    ifelse(last_update_days_ago < 2500, 5, 
    ifelse(last_update_days_ago < 3000, 6, 
    ifelse(last_update_days_ago < 3500, 7, 8
           )))))))) 

ggplot(tayko, aes(x = Spending)) + geom_histogram(binwidth = 10) + labs(y = "Observations")
# Convert Purchase into Monetary (categoriesed 0-9)
tayko <- tayko %>%
  mutate(Monetary = 
    ifelse(Spending < 1, 0, 
    ifelse(Spending < 5, 1, 
    ifelse(Spending < 20, 2, 
    ifelse(Spending < 50, 4,
    ifelse(Spending < 100, 5,
    ifelse(Spending < 250, 6,
    ifelse(Spending < 500, 6,
    ifelse(Spending < 750, 7,
    ifelse(Spending < 1000, 8, 9
           ))))))))))


# Split the train/test in ratio
sample_size <- floor(0.7 * nrow(tayko)) 
set.seed(11052024)
train_index <- sample(seq_len(nrow(tayko)), size = sample_size) 
tayko_train <- tayko[train_index, ]
tayko_test <- tayko[-train_index, ]

# Calculate the purchase rate in the training set
m <- sum(as.integer(tayko_train$Purchase)) / nrow(tayko_train)

# Create function to calculate purchase rate based on Retency, Freq, Monetary (note correlation between Monetary and Purchase)
respondrate <- function(r, f, m, data, x=0, y=0) {
  x <- sum(data$Retency==r & data$Freq==f) # & data$Monetary==m)
  y <- sum(data$Retency==r & data$Freq==f & data$Monetary==m) # & data$Purchase==1)
  if (x > 0)
    y/x
  else
    0
  }

# Example for values of Retency, Frequency, Monetary passed on as shown
respondrate(r=1, f=1, m=4, data=tayko_train)

# Code to find RFM combination in segment 1 (code taken from the exercise description)
l <- tayko_train %>% 
  group_by(Retency, Freq, Monetary, Purchase) %>%
  summarize(rr=respondrate(Retency, Freq, Monetary, tayko_train))
l1 <- filter(l, rr > 2*m) #????
l2 <- filter(l, rr > m & rr <= 2*m) #????

# Print out the RFM combinations in segment 1
segment1 <- numeric()
for(i in 1:nrow(l1)) {
  print(l1[i,]$Retency * 100+l1[i,]$Freq * 10+l1[i,]$Monetary)
  # Attach all of these combinations in a vector
  wynik <- l1[i,]$Retency * 100+l1[i,]$Freq * 10+l1[i,]$Monetary
  segment1 <- c(segment1, wynik)
}

# Print out the RFM combinations in segment 2
segment2 <- numeric()
for(i in 1:nrow(l2)) {
  print(l2[i,]$Retency * 100+l2[i,]$Freq * 10+l2[i,]$Monetary)
  # Attach all of these combinations in a vector
  wynik <- l2[i,]$Retency * 100+l2[i,]$Freq * 10+l2[i,]$Monetary
  segment2 <- c(segment2, wynik)
  }

# And RFM combinations in segment 3 are the rest
all_combinations <- l$Retency * 100+l$Freq * 10+l$Monetary
segment3 <- setdiff(all_combinations, c(segment1, segment2))
# Check if number of elements is correct
nrow(l) == length(segment1) + length(segment2) + length(segment3)


# Calculate the response rate in the test set
m2 <- sum(as.integer(tayko_test$Purchase)) / nrow(tayko_test)

# Create one column called combination
tayko_test <- tayko_test %>% mutate(combination = Retency*100 + Freq*10 + Monetary) 
tayko_test$combination <- as.factor(tayko_test$combination)

# Calculate the response rate and size of segment 1 on the test set
v1 <- tayko_test %>% 
  filter((combination %in% segment1)) %>% 
  select(Purchase)
rate1 <- length(which(v1==1)) / nrow(v1) # Group 1
size1 <- nrow(v1)

# Calculate the response rate and size of segment 2 on the test set
v2 <- tayko_test %>% 
  filter((combination %in% segment2)) %>% 
  select(Purchase)
rate2 <- length(which(v2==1)) / nrow(v2) # Group 2
size2 <- nrow(v2)

# Calculate the response rate and size of segment 3 on the test set
'%notin%' <- Negate('%in%')
v3 <- tayko_test %>%
  filter((combination %notin% segment1) & (combination %notin% segment2)) %>%
  select(Purchase)
rate3 <- length(which(v3==1)) / nrow(v3) # Group 3
size3 <- nrow(v3)

# Calculate the lift ratio in each segment and plot it
lift1 <- rate1 / (size1/nrow(tayko_test))
lift2 <- rate2 / (size2/nrow(tayko_test))
lift3 <- rate3 / (size3/nrow(tayko_test))

# Plot the lift ratio
plot(
  c(lift1, lift2, lift3) ~ c(size1/nrow(tayko_test), size2/nrow(tayko_test), size3/nrow(tayko_test)),
  type = "b", xlab="% of Customers Contacted", ylab = "Lift ratio", 
  main="Lift Chart of Segmentation Model") + abline(h=1, col="red")


