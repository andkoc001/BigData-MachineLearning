# Andrzej Kocielski, 18.05.2024

# Big Data i uczenie maszynowe - Zadanie 12: 
# Wzorując się na Zadaniu 9 opracuj i zaimplementuj system rekomendacji filmów
# w oparciu o filtrowanie kolaboratywne oparte na użytkownikach 
# (user-based collaborative filtering).


# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(cluster)
library(tidyr)
library(stringr)

#### Load and prepare data

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



#### Data processing for user-based filtering

# Create the User-Product matrix with the pivot_wider function (from tidyr)
movie_user_item <- ratings %>%
  top_n(5000) %>% 
  pivot_wider(names_from = movie_id, values_from = rating) %>%
  as.data.frame()

# Data pre-processing: assign user_id to row number and remove user_id, then convert to matrix
rownames_to_keep <- row.names(movie_user_item)
movie_user_item <- movie_user_item[, -which(names(movie_user_item) == "user_id")]
movie_user_item <- as.matrix(movie_user_item)
movie_user_item <- apply(movie_user_item, 2, function(x) as.numeric(as.character(x)))
rownames(movie_user_item) <- rownames_to_keep

# Find the similarity between products using cosine similarity
cos_similarity = function(A, B) {
  num = sum(A * B, na.rm = TRUE)
  den = sqrt(sum(A^2, na.rm = TRUE)) * sqrt(sum(B^2, na.rm = TRUE)) 
  result = num/den
  return(result)
}


# Function to calculate the similarity only on the product id that we choose.
item_recommendation = function(movie_id, rating_matrix = movie_user_item, n_recommendations = 5) {
  
  movie_index = which(colnames(rating_matrix) == movie_id)
  
  similarity = apply(rating_matrix, 2, FUN = function(y) cos_similarity(rating_matrix[, movie_index], y))
  
  recommendations = tibble(movie_id = names(similarity), similarity = similarity) %>%
    #filter(movie_id != movie_id) %>% 
    top_n(n_recommendations, similarity) %>%
    arrange(desc(similarity)) 
  
  return(recommendations)
}

# Create User-Item matrix, at row level in this case, instead of calculating the distances at the column level 
user_recommendation = function(user_id, movie_user_item_matrix = movie_user_item,
                               ratings_matrix = ratings,
                               n_recommendations = 5,
                               threshold = 1,
                               nearest_neighbors = 10){
  
  user_index = which(rownames(movie_user_item_matrix) == user_id)
  
  similarity = apply(movie_user_item_matrix, 1, FUN = function(y) 
    cos_similarity(movie_user_item_matrix[user_index,], y))
  
  similar_users = tibble(user_id = names(similarity), 
                         similarity = similarity) %>%
    filter(user_id != user_id) %>% 
    arrange(desc(similarity)) %>%
    top_n(nearest_neighbors, similarity)
  
  
  watched_movies_user = ratings_matrix$movie_id[ratings_matrix$user_id == user_id]
  
  recommendations = ratings_matrix %>%
   # filter(user_id %in% similar_users$user_id & !(movie_id %in% watched_movies_user)) %>%
    group_by(movie_id) %>%
    summarise(
      count = n(),
      rating = mean(rating)
    ) %>%
    #filter(count > threshold) %>%
    arrange(desc(rating), desc(count)) %>%
    head(n_recommendations)
  
  return(recommendations)
}

#sample_movie <- sample(colnames(movie_user_item), 1) 
sample_user <- sample(users[,1], 1)
recom_cf_user = user_recommendation(sample_user)
recom_cf_user

# Apply user-based collaborative recommendation system
recom_cf_user = recom_cf_user %>%
  left_join(movies_01, by = c("movie_id" = "movie_id"))


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

visualizar_recomendacion(recom_cf_user[!is.na(recom_cf_user$movie_title),],
                         "Movie", "Year")
