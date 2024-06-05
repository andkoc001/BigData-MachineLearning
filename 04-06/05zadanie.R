# Andrzej Kocielski, 11.05.2024

######### Big Data i uczenie maszynowe - Zadanie 05: 
# Wzorując się na Zadaniu 2 opracuj i zaimplementuj system predykcji zakupów
# (zmienna Purchase) w oparciu o zbiór danych Tayko Software Cataloger
# (Tayko.csv) przy użyciu regresji logistycznej.

library(readr)
library(dplyr)
library(ggplot2)
library(caret)

# Load data
#setwd("/home/ak/Desktop/_moje/_coding/WSB/06-BDiML/04/") 
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

