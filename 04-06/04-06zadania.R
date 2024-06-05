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
setwd("/home/ak/Desktop/_moje/_coding/WSB/06-BDiML/04/") # uwaga: ukośniki odpowiednie dla systemu - w moim przypadku Linux
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





######### Big Data i uczenie maszynowe - Zadanie 05: 
# Wzorując się na Zadaniu 2 opracuj i zaimplementuj system predykcji zakupów
# (zmienna Purchase) w oparciu o zbiór danych Tayko Software Cataloger
# (Tayko.csv) przy użyciu regresji logistycznej.

library(readr)
library(dplyr)
library(ggplot2)
library(caret)

# Load data
setwd("/home/ak/Desktop/_moje/_coding/WSB/06-BDiML/04/") 
tayko <- read_csv("Tayko.csv")

# Split train/test
sample_size <- floor(0.7 * nrow(tayko)) 
set.seed(11052024)
train_index <- sample(seq_len(nrow(tayko)), size = sample_size) 
tayko_train <- tayko[train_index, ]
tayko_test <- tayko[-train_index, ]

# Model 1: Logistic model with all predictors
# Choose all the predictors
colnames(tayko)
log_all <- glm(formula = Purchase ~ US + 
                 source_a + source_b + source_c + source_d + source_e + 
                 source_m + source_o + source_h + source_r + source_s + 
                 source_t + source_u + source_p + source_x + source_w +
                 Freq + last_update_days_ago + `1st_update_days_ago` +
                 `Web order` + `Gender=male` + Address_is_res, 
               family = "binomial", data = tayko_train) # do zmiennej ze spacją w nazwie stosujemy odwrotne apostrofy: `

tayko_prob <- predict(log_all, tayko_test, type="response")
tayko_prob
tayko_pred <- rep(0, nrow(tayko_test))
tayko_pred
tayko_pred[tayko_prob>0.5] = 1
tayko_pred

# Create confusion matrix
tayko_mat <- confusionMatrix(as.factor(tayko_pred), as.factor(tayko_test$Purchase)) 
tayko_mat

# Sort predicted probability in decreasing order
tayko_test_new <- tayko_test %>% mutate(tayko_prob)
tayko_test_new <- tayko_test %>% arrange(desc(tayko_prob))

# Create function to calculate the cumulative response rate in each decile
cum_respond <- function(data, k) {
  length(which(data[1:k,]$Purchase==1)) / length(which(data$Purchase==1)) 
  }

# Calculate the cumulative response rate in each decile
percent_data <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
deciles <- length(percent_data)
cumrate <- rep(0, deciles)
for (i in 1:deciles) {
  cumrate[i] = cum_respond(tayko_test_new, 0.1*(i-1)*nrow(tayko_test_new))
}
# Create decile
dat <- data.frame(cumrate, percent_data)

# Plot the chart
ggplot(dat, aes(x=percent_data, y=cumrate)) + geom_point() + geom_line() +
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1) + 
  xlab("% of Customers") + ylab("% of purchases") +
  ggtitle("Cumlative gains chart in model 1")

# Model 2: Logistic model with only few selected predictors
#Fit the model with logistic regression
log_few <- glm(Purchase ~ US + 
                 #source_a + source_b + source_c + source_d + source_e + 
                 #source_m + source_o + source_h + source_r + source_s + 
                 #source_t + source_u + source_p + source_x + source_w +
                 Freq + last_update_days_ago + `1st_update_days_ago` +
                 `Web order` + `Gender=male` + Address_is_res,
                 data=tayko_train, family="binomial")

tayko_prob2 <- predict(log_few, tayko_test, type="response")
tayko_pred2 <- rep(0, nrow(tayko_test))
tayko_pred2[tayko_prob2>0.5] = 1
tayko_pred2 <- factor(tayko_pred2, levels = c(0, 1)) # ręcznie ustawiam poziomy
levels(as.factor(tayko_pred2))

# Create confusion matrix
tayko_mat2 <- confusionMatrix(as.factor(tayko_pred2), as.factor(tayko_test$Purchase))
tayko_mat2


# Sort predicted probability in decreasing order
tayko_test_new2 <- tayko_test %>% mutate(tayko_prob2)
tayko_test_new2 <- tayko_test %>% arrange(desc(tayko_prob2))

# Calculate the cumulative response rate in each decile
percent_data <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
deciles <- length(percent_data)
cumrate2 <- rep(0, deciles)
for (i in 1:deciles) {
  cumrate2[i] = cum_respond(tayko_test_new2, 0.1*(i-1)*nrow(tayko_test_new2))
}

# Create decile
dat2 <- data.frame(cumrate2, percent_data)

# Plot the chart
ggplot(dat2, aes(x=percent_data, y=cumrate2)) + geom_point() + geom_line() +
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1) + 
  xlab("% of Customers") + ylab("% of purchases") +
  ggtitle("Cumlative gains chart in model 2")

# Plot the lift ratio of Model 1 (Model 1 outperforms Model 2)
lift1 <- dat$cumrate / dat$percent_data
plot(lift1[-1] ~ dat[-1,]$percent_data, type = "b", 
     xlab = "% of Customers", ylab = "Lift ratio",
     main = "Lift Chart of Logistic Model") + abline(h=1, col="red")






######### Big Data i uczenie maszynowe - Zadanie 06: 
# Na podstawie artykułu Building Book Recommendation System in R zaimplementuj system
# rekomendacji książek w oparciu o technikę _K-NN_ (K-Nearest Neighbors). 
# Dane wejściowe można pobrać z miejsca wskazanego w artykule: CharlesBookClub.csv.

library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(FNN)

# Load data
setwd("/home/ak/Desktop/_moje/_coding/WSB/06-BDiML/04/") 
tayko <- read_csv("Tayko.csv")

# Data prep - remove redundant columns
tayko <- tayko %>% select(-sequence_number, -Spending)

# Standardize all numerical variables in the train and test set (required for fitting the model with K-NN)
tayko$US <- scale(tayko$US)
tayko$source_a <- scale(tayko$source_a)
tayko$source_b <- scale(tayko$source_b)
tayko$source_c <- scale(tayko$source_c)
tayko$source_d <- scale(tayko$source_d)
tayko$source_e <- scale(tayko$source_e)
tayko$source_m <- scale(tayko$source_m)
tayko$source_o <- scale(tayko$source_o)
tayko$source_h <- scale(tayko$source_h)
tayko$source_r <- scale(tayko$source_r)
tayko$source_s <- scale(tayko$source_s)
tayko$source_t <- scale(tayko$source_t)
tayko$source_u <- scale(tayko$source_u)
tayko$source_p <- scale(tayko$source_p)
tayko$source_x <- scale(tayko$source_x)
tayko$source_w <- scale(tayko$source_w)
tayko$Freq <- scale(tayko$Freq)
tayko$last_update_days_ago <- scale(tayko$last_update_days_ago)
tayko$`1st_update_days_ago` <- scale(tayko$`1st_update_days_ago`)
#tayko$`web order` <- scale(tayko$`web order`)
#tayko$`Gender=male` <- scale(tayko$`Gender=male`)
#tayko$Address_is_res <- scale(tayko$Address_is_res)
#tayko$Purchase <- scale(tayko$Purchase)


# Split train/test
sample_size <- floor(0.7 * nrow(tayko)) 
set.seed(11052024)
train_index <- sample(seq_len(nrow(tayko)), size = sample_size) 
tayko_train <- tayko[train_index, ]
tayko_test <- tayko[-train_index, ]

# Create the function to calculate the accuracy
accu <- function(x) {(x[1,1] + x[2,2]) / (sum(x))}

# Create a vector to contain accuracy value in each loop
k_range = 20
acc <- rep(0, k_range)
names(tayko)
tayko_cols = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)

# Create a for loop to find the “best k”
for(i in 1:k_range) {
  # Fit the k-nn model
  knn.pred <- knn(train = tayko_train[, tayko_cols], 
                  test = tayko_test[, tayko_cols],
                  cl = tayko_train$Purchase,
                  k = i)
  # Create confusion matrix
  mat_knn <- table(knn.pred, tayko_test$Purchase)
  acc[i] = accu(mat_knn)
  }


k <- seq(1, k_range, 1)
dat3 <- data.frame(k, acc)
dat3 # best are k = 5, 7 (for the selected seed)

# Plot the cumulative gains chart
# Fit the knn with the best k
nn <- knn(train = tayko_train[, tayko_cols], 
          test = tayko_test[, tayko_cols],
          cl = tayko_train$Purchase, k = 5, prob=T)

# Calculate the predicted probability for each customer
proba <- data.frame(nn, attr(nn, "prob"))
for(i in 1:nrow(proba)) {
  if(proba[i, ]$nn == 0) {
    proba[i, ]$attr.nn...prob.. = 1-proba[i, ]$attr.nn...prob..
    }
}

# Create function to calculate the cumulative response rate in each decile
cum_respond <- function(data, k) {
  length(which(data[1:k,]$Purchase==1)) / length(which(data$Purchase==1)) 
}

# Plot the cumulative gains chart
tayko_test_new3 <- tayko_test %>% mutate(proba$attr.nn...prob..)
tayko_test_new3 <- tayko_test_new3 %>% arrange(desc(proba$attr.nn...prob..))

percent_data <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
deciles <- length(percent_data)
cumrate3 <- rep(0, deciles)
for (i in 1:deciles) {
  cumrate3[i] = cum_respond(tayko_test_new3, 0.1*(i-1)*nrow(tayko_test_new3))
}

dat_knn <- data.frame(cumrate3, percent_data)

ggplot(dat_knn, aes(x=percent_data, y=cumrate3)) + geom_point() + geom_line() +
  xlim(0, 1) + ylim(0, 1) + geom_abline(intercept=0, slope=1) +
  xlab("% of Customers") + ylab("% of Purchases") +
  ggtitle("Cumlative gains chart in K-NN model")

# Plot the lift ratio of Model knn
lift3 <- dat_knn$cumrate3 / dat_knn$percent_data
plot(lift3[-1] ~ dat_knn[-1,]$percent_data, type = "b", 
     xlab = "% of Customers", ylab = "Lift ratio",
     main = "Lift Chart of k-nn Model") + abline(h=1, col="red")

