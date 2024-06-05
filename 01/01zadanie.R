# Andrzej Kocielski, 11.05.2024

# Big Data i uczenie maszynowe - Zadanie 01: 
# Na podstawie artykułu Building Book Recommendation System in R zaimplementuj system
# rekomendacji książek w oparciu o _segmentację_ klientów. Dane wejściowe można pobrać
# z miejsca wskazanego w artykule: CharlesBookClub.csv lub z katalogu z zadaniami.

library(dplyr)
library(caret)
library(ggplot2)
library(randomForest)
library(FNN)
library(readr)
library(tidyr)

# Load data
setwd("/home/ak/Desktop/_moje/_coding/WSB/06-BDiML/01/") # uwaga: ukośniki odpowiednie dla systemu - w moim przypadku Linux
char_book <- read_csv("CharlesBookClub.csv")

# Data prep - remove redundant columns
char_book <- char_book %>% select(-'Seq#', -'ID#')

## Split the train/test in ratio
smp_size <- floor(0.6 * nrow(char_book)) 
set.seed(11052024)
train_ind <- sample(seq_len(nrow(char_book)), size = smp_size) 
cb.train <- char_book[train_ind,]
cb.test <- char_book[-train_ind,]

# Calculate the response rate in the training set
m <- sum(as.integer(cb.train$Florence)) / nrow(cb.train)
m

# Create function to calculate response rate base on r,f,m (code taken from the exercise description)
respondrate <- function(r, f, m, data, x=0, y=0)
{
  x <- sum(data$Rcode==r & data$Fcode==f & data$Mcode==m)
  y <- sum(data$Rcode==r & data$Fcode==f & data$Mcode==m & data$Florence==1)
  if (x > 0)
    y/x
  else
    0
}
# Example for values of Rcode, Fcode, Mcode passed on as shown
respondrate(r=1, f=1, m=4, data=cb.train)

# Code to find RFM combination in segment 1 (code taken from the exercise description)
l <- cb.train %>% 
  group_by(Rcode, Fcode, Mcode) %>%
  summarize(rr=respondrate(Rcode, Fcode, Mcode, cb.train))
l1 <- filter(l, rr > 2*m)
l2 <- filter(l, rr > m & rr <= 2*m)

# Print out the RFM combinations in segment 1
segment1 <- numeric()
for(i in 1:nrow(l1)) {
  print(l1[i,]$Rcode * 100+l1[i,]$Fcode * 10+l1[i,]$Mcode)
  # Attach all of these combinations in a vector
  wynik <- l1[i,]$Rcode * 100+l1[i,]$Fcode * 10+l1[i,]$Mcode
  segment1 <- c(segment1, wynik)
}

# Print out the RFM combinations in segment 2
segment2 <- numeric()
for(i in 1:nrow(l2)) {
  print(l2[i,]$Rcode * 100+l2[i,]$Fcode * 10+l2[i,]$Mcode)
  # Attach all of these combinations in a vector
  wynik <- l2[i,]$Rcode * 100+l2[i,]$Fcode * 10+l2[i,]$Mcode
  segment2 <- c(segment2, wynik)
  }

# And RFM combinations in segment 3 are the rest
all_combinations <- l$Rcode * 100+l$Fcode * 10+l$Mcode
segment3 <- setdiff(all_combinations, c(segment1, segment2))

# Check if number of elements is correct
nrow(l) == length(segment1) + length(segment2) + length(segment3)


# Calculate the response rate in the test set
m2 = sum(as.integer(cb.test$Florence)) / nrow(cb.test)
m2

# Create one column called combination
cb.test <- cb.test %>% mutate(combination = Rcode*100 + Fcode*10 + Mcode) 
cb.test$combination <- as.factor(cb.test$combination)

# Calculate the response rate and size of segment 1 on the test set
v <- cb.test %>% 
  filter((combination %in% segment1)) %>% 
  select(Florence)
rate1 <- length(which(v==1)) / nrow(v) # Group 1
size1 <- nrow(v)

# Calculate the response rate and size of segment 2 on the test set
v <- cb.test %>% 
  filter((combination %in% segment2)) %>% 
  select(Florence)
rate2 <- length(which(v==1)) / nrow(v) # Group 2
size2 <- nrow(v)

# Calculate the response rate and size of segment 3 on the test set
'%notin%' <- Negate('%in%')
v <- cb.test %>%
  filter((combination %notin% segment1) & (combination %notin% segment2)) %>%
  select(Florence)
rate3 <- length(which(v==1)) / nrow(v) # Group 3
size3 <- nrow(v)

# Calculate the lift ratio in each segment and plot it
lift1 <- rate1 / (size1/nrow(cb.test))
lift2 <- rate2 / (size2/nrow(cb.test))
lift3 <- rate3 / (size3/nrow(cb.test))

# Plot the lift ratio
plot(
  c(lift1, lift2, lift3) ~ c(size1/nrow(cb.test), size2/nrow(cb.test), size3/nrow(cb.test)), 
  type = "b", xlab="% of Customers Contacted", ylab = "Lift ratio", 
  main="Lift Chart of Segmentation Model") + abline(h=1, col="red")

# End 01
########

# Big Data i uczenie maszynowe - Zadanie 02: 
# Na podstawie artykułu Building Book Recommendation System in R zaimplementuj system
# rekomendacji książek w oparciu o _regresję logistyczną_ w 2 wariantach: 1) model oparty 
# o wszystkie predyktory, 2) model oparty tylko o predyktory R, M, F. 
# Dane wejściowe można pobrać z miejsca wskazanego w artykule: CharlesBookClub.csv.

# Model 1: Logistic model with all predictors
#Split train/test
smp_size <- floor(0.7 * nrow(char_book))
set.seed(11052024)
train_ind <- sample(seq_len(nrow(char_book)), size = smp_size)
cb.train <- char_book[train_ind, ]
cb.test <- char_book[-train_ind, ]

#Choose all the predictors
log_all <- glm(formula = Florence ~ Gender + M + F + R + FirstPurch + ChildBks + 
                 YouthBks + CookBks + DoItYBks + RefBks + ArtBks + GeogBks + 
                 ItalCook + ItalAtlas + ItalArt + `Related Purchase`, 
               family = "binomial", data = cb.train) # do zmiennej ze spacją w nazwie stosujemy odwrotne apostrofy: `

cb_prob <- predict(log_all, cb.test, type="response")
cb_pred <- rep(0, nrow(cb.test))
cb_pred[cb_prob>0.5] = 1

# Create confusion matrix
cb_mat <- confusionMatrix(as.factor(cb_pred), as.factor(cb.test$Florence)) # cb.test trzeba również zamienić na czynnik
cb_mat

# Sort predicted probability in decreasing order
cb.test_new <- cb.test %>% mutate(cb_prob)
cb.test_new <- cb.test %>% arrange(desc(cb_prob))

# Create function to calculate the cumulative response rate in each decile
cum_respond <- function(data, k) {
  length(which(data[1:k,]$Florence==1)) / length(which(data$Florence==1)) 
  }

# Calculate the cumulative response rate in each decile
cumrate <- rep(0, 11)
for (i in 1:11) {
  cumrate[i] = cum_respond(cb.test_new, 0.1*(i-1)*nrow(cb.test_new))
}

# Create decile
percent_data <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
dat <- data.frame(cumrate, percent_data)

# Plot the chart
ggplot(dat, aes(x=percent_data, y=cumrate)) + geom_point() + geom_line() +
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1) + 
  xlab("% of Customers Contacted") + ylab("% of Responders Reached") +
  ggtitle("Cumlative gains chart in model 1")

# Model 2: Logistic model with only R, F and M predictors
#Fit the model with logistic regression
log_rfm <- glm(Florence ~ M + F + R, data=cb.train, family="binomial")

cb_prob2 <- predict(log_rfm, cb.test, type="response")
cb_pred2 <- rep(0, nrow(cb.test))
cb_pred2[cb_prob2>0.5] = 1
cb_pred2 <- factor(cb_pred2, levels = c(0, 1)) # ręcznie ustawiam poziomy
levels(as.factor(cb_pred2))

# Create confusion matrix
cb_mat2 <- confusionMatrix(as.factor(cb_pred2), as.factor(cb.test$Florence))
cb_mat2


# Sort predicted probability in decreasing order
cb.test_new2 <- cb.test %>% mutate(cb_prob2)
cb.test_new2 <- cb.test %>% arrange(desc(cb_prob2))

# Calculate the cumulative response rate in each decile
cumrate2 <- rep(0, 11)
for (i in 1:11) {
  cumrate2[i] = cum_respond(cb.test_new2, 0.1*(i-1)*nrow(cb.test_new2))
}

# Create decile
#percent_data <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
dat2 <- data.frame(cumrate2, percent_data)

# Plot the chart
ggplot(dat2, aes(x=percent_data, y=cumrate2)) + geom_point() + geom_line() +
  xlim(0,1) + ylim(0,1) + geom_abline(intercept = 0, slope = 1) + 
  xlab("% of Customers Contacted ") + ylab("% of Responders Reached") +
  ggtitle("Cumlative gains chart in model 2")


# Plot the lift ratio of Model 1 (Model 1 outperforms Model 2)
lift1 <- dat$cumrate / dat$percent_data
plot(lift1[-1] ~ dat[-1,]$percent_data, type = "b", 
     xlab = "% of Customers Contacted", ylab = "Lift ratio",
     main="Lift Chart of Logistic Model") + abline(h=1, col="red")


# End 02
########

# Big Data i uczenie maszynowe - Zadanie 03: 
# Na podstawie artykułu Building Book Recommendation System in R zaimplementuj system
# rekomendacji książek w oparciu o technikę _K-NN_ (K-Nearest Neighbors). 
# Dane wejściowe można pobrać z miejsca wskazanego w artykule: CharlesBookClub.csv.

# Standardize all numerical variables in the train and test set (required for fitting the model with K-NN)
cb.train$R<-scale(cb.train$R)
cb.train$F<-scale(cb.train$F)
cb.train$M<-scale(cb.train$M)
cb.train$FirstPurch<-scale(cb.train$FirstPurch)
cb.train$Related.Purchase<-scale(cb.train$`Related Purchase`)
cb.train$Florence<-as.factor(cb.train$Florence)
cb.test$R<-scale(cb.test$R)
cb.test$F<-scale(cb.test$F)
cb.test$M<-scale(cb.test$M)
cb.test$FirstPurch<-scale(cb.test$FirstPurch)
cb.test$Related.Purchase<-scale(cb.test$`Related Purchase`)
cb.test$Florence<-as.factor(cb.test$Florence)

# Create the function to calculate the accuracy
accu <- function(x) {(x[1,1] + x[2,2]) / (sum(x))}

# Create a vector to contain accuracy value in each loop
acc <- rep(0, 11)

# Create a for loop to find the “best k”
for(i in 1:11) {
  # Fit the k-nn model
  knn.pred <- knn(train = cb.train[, c(4,5,6,7,19)], 
                  test = cb.test[, c(4,5,6,7,19)],
                  cl = cb.train$Florence,
                  k = i)
  # Create confusion matrix
  mat <- table(knn.pred, cb.test$Florence)
  acc[i] = accu(mat)
  }

k <- seq(1, 11, 1)
dat3 <- data.frame(k, acc)
dat3 # best are k = 6, 10, 11 (for the selected seed)

# Plot the cumulative gains chart
# Fit the knn with k=6
nn <- knn(train = cb.train[, c(4,5,6,7,19)], test = cb.test[, c(4,5,6,7,19)],
          cl = cb.train$Florence, k = 6, prob=T)

# Calculate the predicted probability for each customer
proba <- data.frame(nn, attr(nn, "prob"))
for(i in 1:nrow(proba)) {
  if(proba[i, ]$nn == 0) {
    proba[i, ]$attr.nn...prob.. = 1-proba[i, ]$attr.nn...prob..
    }
}

# Plot the cumulative gains chart
cb.test_new3 <- cb.test %>% mutate(proba$attr.nn...prob..)
cb.test_new3 <- cb.test_new3 %>% arrange(desc(proba$attr.nn...prob..))
cumrate <- rep(0, 11)
for (i in 1:11) {
  cumrate[i] = cum_respond(cb.test_new3, 0.1*(i-1)*nrow(cb.test_new3))
}  
percent_data < -c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
dat3 <- data.frame(cumrate, percent_data)

ggplot(dat3, aes(x=percent_data, y=cumrate)) + geom_point() + geom_line() +
  xlim(0, 1) + ylim(0, 1) + geom_abline(intercept=0, slope=1) +
  xlab("% of Customers Contacted") + ylab("% of Responders Reached") +
  ggtitle("Cumlative gains chart in K-NN model")

