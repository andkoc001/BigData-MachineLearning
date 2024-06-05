# Andrzej Kocielski, 11.05.2024

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
#setwd("/home/ak/Desktop/_moje/_coding/WSB/06-BDiML/04/") 
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

