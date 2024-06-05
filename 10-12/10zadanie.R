# Andrzej Kocielski, 18.05.2024

# Big Data i uczenie maszynowe - Zadanie 10: 
# Wzorując się na Zadaniu 7 opracuj i zaimplementuj system rekomendacji filmów
# w oparciu o filtrowanie oparte na treści (content-based filtering).
# Dane wejściowe należy pobrać z Movie Tweetings (MovieTweetings.zip).


# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(cluster)
library(tidyr)
library(stringr)

#### 1. Load and prepare data

#setwd("/home/ak/Desktop/_moje/_coding/WSB/06-BDiML/10/") 

lines <- readLines("movies.dat")
movies <- lapply(lines, function(x) strsplit(x, "::", fixed = TRUE)[[1]])
movies <- as.data.frame(do.call(rbind, movies))
colnames(movies) <- c("movie_id", "movie", "movie_genre")

lines <- readLines("ratings.dat")
ratings <- lapply(lines, function(x) strsplit(x, "::", fixed = TRUE)[[1]])
ratings <- as.data.frame(do.call(rbind, ratings))
colnames(ratings) <- c("user_id", "movie_id", "rating", "rating_timestamp")

lines <- readLines("users.dat")
users <- lapply(lines, function(x) strsplit(x, "::", fixed = TRUE)[[1]])
users <- as.data.frame(do.call(rbind, users))
colnames(users) <- c("user_id", "twitter_id")


# Split genres into separate columns
split_genres <- function(df) {
  df_split <- 
    separate(df, movie_genre, 
             into = paste0("genre", 1:max(str_count(df$movie_genre, "\\|"))), 
             sep = "\\|", fill = "right")
  
  # Make new columns for new genre
  for (col in colnames(df_split)[-c(1:2)]) {
    if (!(col %in% colnames(df))) {
      df[[col]] <- NA
    }
  }
  
  # Assign genres to relevant columns
  for (i in 1:nrow(df_split)) {
    for (j in 3:ncol(df_split)) {
      if (!is.na(df_split[i, j])) {
        df[i, df_split[i, j]] <- df_split[i, j]
      }
    }
  }
  
  return(df)
}

# Function execution
movies <- split_genres(movies)
movies$movie_genre <- NULL

# Data prep - remove redundant columns
movies <- movies %>% 
  select(-genre1, -genre2, -genre3, -genre4, -genre5, -genre6)
movies <- movies %>% 
  select(-starts_with("0"), -starts_with("1"), -starts_with("2"), -starts_with("3"))


# Split movie name and production year
movies$movie_title <- sapply(strsplit(movies$movie, "\\("), function(x) x[1])
movies$movie_year <- sapply(strsplit(movies$movie, "\\("), function(x) gsub("\\)", "", x[2]))
movies$movie <- NULL

# Change columns order
movies <- movies[, c(1, 27, 28, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
                     14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)]

# Fixing the columns names - additional ID in some cases
movies$movie_id = paste0("movie_id_", movies$movie_id)
users$user_id = paste0("user_id_", users$user_id)
users$twitter_id = paste0("twitter_id_", users$twitter_id)
ratings$movie_id = paste0("movie_id_", ratings$movie_id)
ratings$user_id = paste0("user_id_", ratings$user_id)

# Convert NA into 0 and text into 1 
convert_into_01 <- function(df, cols) {
  new_df <- mutate_at(df, vars(one_of(cols)), ~ifelse(is.na(.), 0, 1))
  return(new_df)
}

#names(movies)
cols_selected_to_convert <- c(
  "Crime"      ,
  "Drama"     ,  "Comedy"      , "Short"       , "Romance"    ,
  "Action"    ,  "Adventure"   , "Family"      , "Mystery"    ,
  "Thriller"  ,  "Documentary" , "Horror"      , "Fantasy"    ,
  "History"   ,  "Western"     , "Sci-Fi"      , "War"        ,
  "Biography" ,  "Musical"     , "Film-Noir"   , "Animation"  ,
  "Sport"     ,  "Music"       , "Adult"       , "News"  
)

movies_01 <- convert_into_01(movies, cols_selected_to_convert)
#sample_n(movies_01, 1)





#### 2. Data pre-processing for content-based filtering

# Ratings distribution
ratings %>%
  group_by(rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(rating, cases)) + geom_col() +
    theme_minimal() + 
    #scale_x_continuous(breaks = 0:10) 
    scale_x_discrete(breaks = 0:10)


# Remove users who have given 3 recommendations or less
ratings_sum = ratings %>%
  group_by(user_id) %>%
  count() 
user_index = ratings_sum$user_id[ratings_sum$n>4]

users = users[users$user_id %in% user_index, ]
ratings = ratings[ratings$user_id %in% user_index, ]
movies_01 = movies_01[movies_01$movie_id %in% ratings$movie_id, ]
rm(ratings_sum, user_index)

# limit number of movies
lim_rows <- 10000
if (nrow(movies_01) < lim_rows) {lim_rows <- nrow(movies_01)}
movie_feature <- movies_01[1:lim_rows, c("movie_id", "movie_title", "movie_year")]

# convert to factors
movie_feature[,1] <- as.factor(movie_feature[,1])
movie_feature[,2] <- as.factor(movie_feature[,2])
movie_feature[,3] <- as.factor(movie_feature[,3])

dissimilarity = daisy(movie_feature, metric = "gower", weights = c(2, 0.5, 1))
dissimilarity = as.matrix(dissimilarity)

row.names(dissimilarity) <- movies_01$movie_id[1:lim_rows] # was 1:10000
colnames(dissimilarity) <- movies_01$movie_id[1:lim_rows] # was 1:10000

dissimilarity[15:20,15:20] # we are talking about dissimilarity, so if the value is 0, it means that those books are the same, while if it is 1, they have nothing in common.


# Create dataset that keeps the review users have watched
sample_user <- sample(users[,1], 1) # get a random user
user_movies = ratings %>%
  filter(user_id == sample_user & movie_id %in% movies_01$movie_id) %>%
  arrange(desc(rating))

selected_movies = user_movies[, c("movie_id", "rating")]


# Recommending engine
recomendar = function(selected_movies, dissimilarity_matrix, 
                      movies_01, n_recommendations = 5) {
  
  selected_movie_indexes = which(colnames(dissimilarity_matrix) %in% selected_movies$movie_id)
  
  results = data.frame(dissimilarity_matrix[, selected_movie_indexes], 
                       recommended_movie = row.names(dissimilarity_matrix),
                       stringsAsFactors = FALSE)
  
  recomendaciones = results %>%
    pivot_longer(cols = c(-"recommended_movie"), names_to = "wached_movie", 
                 values_to = "dissimilarity") %>%
    left_join(selected_movies, by = c("recommended_movie" = "movie_id")) %>%
    arrange(desc(dissimilarity)) %>%
    filter(recommended_movie != wached_movie) %>%
    
    mutate(
      similarity = 1 - dissimilarity 
    ) %>%
    # was:
    #mutate(
    #  similarity = 1 - dissimilarity,
    #  weighted_score = similarity * ratings) %>%
    arrange(desc(similarity)) %>% # was: arrange(desc(weighted_score)) %>%
    filter(similarity>0) %>%  # was: filter(weighted_score>0) %>% 
    group_by(recommended_movie) %>%
    slice(1:n_recommendations) %>%
    ungroup() %>%
    left_join(movies_01, by = c("recommended_movie" = "movie_id"))
  
  return(recomendaciones)
}

recomendaciones = recomendar(selected_movies, dissimilarity, movies_01)
recomendaciones


# Show the recommendations (code taken from the exercise description)
visualizar_recomendacion = function(recomendation,
                                    recommended_movies, image, n_recom = 5) {
  if(n_recom > nrow(recomendation)) {
    n_recom = nrow(recomendation)}
  
  cat("---REKOMENDACJE--- dla użytkownika:", sample_user, "\n")
  
  for(i in 1:n_recom) {
    title <- as.character(recomendation[i,'movie_title'])
    year <- as.character(recomendation[i,'movie_year'])
    cat(i, ": ", title, ", ", year, "\n")
  }
}

visualizar_recomendacion(recomendaciones, "Movie", "Year")

