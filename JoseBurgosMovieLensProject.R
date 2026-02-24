##########################################################
# Create edx and final_holdout_test sets (as provided by the course)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



##########################################################
# Data Exploration
##########################################################

# Make a copy of the "edx" dataset and create new columns derived from the raw columns provided to extract and explore information that could be used as variables in the model 
# Mutations include:
# rating_week: rating timestamp rounded to the nearest week
# rating_year: extraction of the year from the rating timestamp to know what year the rating was made
# movie_year: extraction of the year from the title to know what year the movie was made
# years_since_release: difference between rating_year and movie_year to know how long after movie release the user rated the movie
# 19 columns representing if a given genre is present in the genres string
edx_mutated_exp <- edx %>% mutate(
  rating_week = floor_date(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"), "week"), 
  rating_year = as.integer(year(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"))), 
  movie_year =  as.integer(substr(title, nchar(title) - 4, nchar(title) - 1)),
  years_since_release = rating_year - movie_year,
  Action = str_detect(edx$genres, "Action"),
  Adventure = str_detect(edx$genres, "Adventure"),
  Animation = str_detect(edx$genres, "Animation"),
  Children = str_detect(edx$genres, "Children"),
  Comedy = str_detect(edx$genres, "Comedy"),
  Crime = str_detect(edx$genres, "Crime"),
  Documentary = str_detect(edx$genres, "Documentary"),
  Drama = str_detect(edx$genres, "Drama"),
  Fantasy = str_detect(edx$genres, "Fantasy"),
  Film_Noir = str_detect(edx$genres, "Film-Noir"),
  Horror = str_detect(edx$genres, "Horror"),
  IMAX = str_detect(edx$genres, "IMAX"),
  Musical = str_detect(edx$genres, "Musical"),
  Mystery = str_detect(edx$genres, "Mystery"),
  Romance = str_detect(edx$genres, "Romance"),
  Sci_Fi = str_detect(edx$genres, "Sci-Fi"),
  Thriller = str_detect(edx$genres, "Thriller"),
  War = str_detect(edx$genres, "War"),
  Western = str_detect(edx$genres, "Western")
)
edx_mutated_exp <- as.data.frame(edx_mutated_exp)

# Extract the mean of the entire "edx_" dataset for exploration visualizations
mu_exp <- mean(edx_mutated_exp$rating)

# Histogram of the average ratings of each movie compared to the "edx" mean
edx_mutated_exp %>% group_by(movieId) %>% 
  summarize(rating = mean(rating)) %>% 
  ggplot(aes(x = rating)) + 
    geom_histogram(binwidth = 0.1, color = "black") +
    geom_vline(xintercept = mu_exp, color = "black", linetype = "dashed", linewidth = 1) + 
    labs(title = "Distribution of Average Ratings per Movie", x = "Rating", y = "Counts")
# Histogram of total rating counts for each movie on log10 scale
edx_mutated_exp %>% group_by(movieId) %>% 
  summarize(counts = n()) %>% 
  ggplot(aes(x = counts)) +
    coord_trans(x = "log10") + 
    scale_x_continuous(breaks = c(1, 10, 100, 1000, 10000, 20000, 40000)) +
    geom_histogram(binwidth = 1, color = "black") +
    labs(title = "Distribution of Number of Ratings per Movie", x = "Number of Ratings per Movie", y = "Counts")

# Histogram of the average ratings of each user compared to the "edx" mean
edx_mutated_exp %>% group_by(userId) %>% 
  summarize(rating = mean(rating)) %>% 
  ggplot(aes(x = rating)) + 
    geom_histogram(binwidth = 0.1, color = "black") +
    geom_vline(xintercept = mu_exp, color = "black", linetype = "dashed", linewidth = 1) + 
    labs(title = "Distribution of Average Ratings per User", x = "Rating", y = "Counts")
# Histogram of the distribution of rating counts for each user on log10 scale
edx_mutated_exp %>% group_by(userId) %>% 
  summarize(counts = n()) %>% 
  ggplot(aes(x = counts)) +
    coord_trans(x = "log10") + 
    scale_x_continuous(breaks = c(1, 10, 50, 100, 500, 1000, 2000, 3000, 5000)) +
    geom_histogram(binwidth = 1, color = "black") +
    labs(title = "Distribution of Number of Ratings per User",  x = "Number of Ratings per User",  y = "Counts")

# Histogram of the average ratings of each genre combination compared to the "edx" mean
edx_mutated_exp %>% group_by(genres) %>% 
  summarize(rating = mean(rating)) %>% 
  ggplot(aes(x = rating)) + 
    geom_histogram(binwidth = 0.1, color = "black") +
    geom_vline(xintercept = mu_exp, color = "black", linetype = "dashed", linewidth = 1) + 
    labs(title = "Distribution of Average Ratings per Genre Combination", x = "Rating", y = "Counts")

# Scatter plot of the average rating for each genre as a function of number of ratings per genre
edx_mutated_exp %>% 
  # Select only the genre columns and ratings without any additional information
  select(Action, Adventure, Animation, Children, Comedy, Crime, 
         Documentary, Drama, Fantasy, Film_Noir, Horror, IMAX, Musical, 
         Mystery, Romance, Sci_Fi, Thriller, War, Western, rating) %>%
  # Make a table that takes each row and splits it into 19 rows that show a rating, each genre, and whether or not it was present in the row
  # Then concatenate all the new tables together into one big column of rating, genre, and binary_value (TRUE or FALSE)
  pivot_longer(cols = c(Action, Adventure, Animation, Children, Comedy, Crime, 
                        Documentary, Drama, Fantasy, Film_Noir, Horror, IMAX, Musical, 
                        Mystery, Romance, Sci_Fi, Thriller, War, Western), names_to = "genre", values_to = "binary_value") %>%
  # Get average TRUE and FALSE ratings as well as TURE and FALSE total counts (FALSE totals will be 0) for each of the 19 genres
  group_by(genre, binary_value) %>%
  summarize(total_count = sum(binary_value, na.rm = TRUE), avg_rating = mean(rating, na.rm = TRUE),  .groups = "drop") %>%
  pivot_wider(names_from = binary_value, 
              values_from = c(avg_rating, total_count),
              names_glue = "{.value}_{binary_value}",
              names_prefix = "avg_rating_") %>% 
  # Rename the total_count_TRUE and avg_rating_TRUE to remove the unnecessary _TRUE suffixes prior to plotting
  rename(total_count = total_count_TRUE, avg_rating = avg_rating_TRUE) %>% 
  ggplot(aes(x = total_count, y = avg_rating, label = genre)) +
    geom_point() + 
    geom_text(vjust = 1.5, hjust = -0.075, size = 2) + 
    geom_hline(yintercept = mu_exp, color = "black", linetype = "solid", linewidth = 1) + 
    labs(title = "Average Ratings vs Number of Ratings for Each Genre", x = "Number of Ratings", y = "Average Rating")


# Create a filtered_genres column in the "edx" exploration copy that detects and keeps genres of interest in a string, with undesired genres commented out
edx_mutated_exp %>% mutate(
  filtered_genres = paste0(
    ifelse(str_detect(genres, "Action"), "Action ", ""),
    ifelse(str_detect(genres, "Adventure"), "Adventure ", ""),
    ifelse(str_detect(genres, "Animation"), "Animation ", ""),
    ifelse(str_detect(genres, "Children"), "Children ", ""),
    ifelse(str_detect(genres, "Comedy"), "Comedy ", ""),
    ifelse(str_detect(genres, "Crime"), "Crime ", ""),
    ifelse(str_detect(genres, "Documentary"), "Documentary ", ""),
    ifelse(str_detect(genres, "Drama"), "Drama ", ""),
    ifelse(str_detect(genres, "Fantasy"), "Fantasy ", ""),
    ifelse(str_detect(genres, "Film-Noir"), "Film-Noir ", ""),
    ifelse(str_detect(genres, "Horror"), "Horror ", ""),
    #ifelse(str_detect(genres, "IMAX"), "IMAX ", ""),    # IMAX category commented out from string detection formula
    ifelse(str_detect(genres, "Musical"), "Musical ", ""),
    ifelse(str_detect(genres, "Mystery"), "Mystery ", ""),
    ifelse(str_detect(genres, "Romance"), "Romance ", ""),
    ifelse(str_detect(genres, "Sci-Fi"), "Sci-Fi ", ""),
    ifelse(str_detect(genres, "Thriller"), "Thriller ", ""),
    ifelse(str_detect(genres, "War"), "War ", ""),
    ifelse(str_detect(genres, "Western"), "Western ", "")
  )) %>% 
  # Histogram of the average ratings of each genre combination with "IMAX" filtered out compared to the "edx" mean
  group_by(filtered_genres) %>% 
  summarize(rating = mean(rating)) %>% 
  ggplot(aes(x = rating)) + 
    geom_histogram(binwidth = 0.1, color = "black") +
    geom_vline(xintercept = mu_exp, color = "black", linetype = "dashed", linewidth = 1) + 
    labs(title = "Distribution of Average Ratings per Filtered Genre Combination", x = "Rating", y = "Counts")

# Scatter plot of the number of ratings per rating week
edx_mutated_exp %>% group_by(rating_week) %>% 
  summarize(rating = mean(rating)) %>% 
  ggplot(aes(x = rating_week, y = rating)) + 
    geom_point() +
    geom_hline(yintercept = mu_exp, color = "black", linetype = "solid", linewidth = 1) + 
    labs(title = "Average Ratings per Rating Week", x = "Rating Week", y = "Average Rating")

# Scatter plot of average ratings as function of number of years between movie release and rating
edx_mutated_exp %>% group_by(years_since_release) %>% 
  summarize(rating = mean(rating)) %>% 
  ggplot(aes(x = years_since_release, y = rating)) +
    geom_point() + 
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    geom_hline(yintercept = mu_exp, color = "black", linetype = "solid", linewidth = 1) + 
    labs(title = "Average Ratings per Number of Years Since Release", x = "Years Since Release", y = "Average Rating")
# Scatter plot of number of ratings as function of number of years between movie release and rating
edx_mutated_exp %>% group_by(years_since_release) %>% 
  summarize(counts = n()) %>% 
  ggplot(aes(x = years_since_release, y = counts)) +
    geom_point() +
    geom_line() + 
    scale_x_continuous(breaks = seq(0, 100, by = 10)) +
    labs(title = "Number of Ratings per Number of Years Since Release", x = "Years Since Release", y = "Number of Ratings")



##########################################################
# Model Development
##########################################################

# Split edx data into training and validation test sets, 90/10 ratio
set.seed(1, sample.kind="Rounding") # Seed set for reproducibility of this report and model
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-edx_test_index,]
temp2 <- edx[edx_test_index,]

# Make sure userId and movieId in test set are also in train set
test <- temp2 %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into train set
removed2 <- anti_join(temp2, test)
train <- rbind(train, removed2)

# Remove unnecessary data and values from setup and data exploration
rm(edx, edx_test_index, temp2, removed2, edx_mutated_exp, mu_exp, movies_file, ratings_file)

# Define RMSE function:
RMSE <- function(true_ratings, predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}


# Define the project goal RMSE and the mean of "train" set
goal_rmse <- "0.86490"
mu <- mean(train$rating)
# Calculate RMSE of "test" if the mean of "train" is the prediction for all ratings in "test"
mu_rmse <- round(RMSE(test$rating, mu), 5)
# Create an RMSE results table for different models
# Seed with the course goal and the RMSE of "test" if the mean of "train" is the prediction for all ratings in "test"
rmse_results_table <- data.frame(Model = c("Course Goal RMSE", "Average"), RMSE = c(goal_rmse, mu_rmse)) 
rmse_results_table


# Adding movie effects to the average model:
# Make a table of movie-grouped average residuals between ratings and the "train" average to serve as the added movie bias
b_movie_table <- train %>% 
  group_by(movieId) %>% 
  summarize(b_movie = mean(rating - mu))
# Join the movie bias table to the test set and evaluate RMSE
b_movie <- left_join(test, b_movie_table, by = "movieId") %>%
  mutate(pred = mu + b_movie) %>%
  pull(pred)
# Evaluate RMSE
mu_movie_rmse <- round(RMSE(b_movie, test$rating), 5)
# Add new model RMSE to RMSE results table for comparison
rmse_results_table <- rmse_results_table %>% rbind(c("Average w/ Movie Effects", mu_movie_rmse))
rmse_results_table


# Adding user effects to the average w/ movie effects model:
# Make a table of user-grouped average residuals between ratings and the "train" average with movie bias predictions to serve as the added user bias
b_user_table <- train %>% 
  left_join(b_movie_table, by='movieId') %>% 
  group_by(userId) %>% 
  summarize(b_user = mean(rating - mu - b_movie))
# Join the user bias table to the test set and evaluate RMSE
b_movie_user <- test %>% 
  left_join(b_movie_table, by = "movieId") %>% 
  left_join(b_user_table, by = "userId") %>% 
  mutate(pred = mu + b_movie + b_user) %>%
  pull(pred)
# Evaluate RMSE
mu_movie_user_rmse <- round(RMSE(b_movie_user, test$rating), 5)
# Add new model RMSE to RMSE results table for comparison
rmse_results_table <- rmse_results_table %>% rbind(c("Average w/ Movie + User Effects", mu_movie_user_rmse))
rmse_results_table


# Adding genre combination effects to the average w/ movie + user effects model: raw vs filtered
# Raw genres: use genres column as-is without any removal of relatively infrequent genres that may add unnecessary genre combinations
# Make a table of raw genre-grouped average residuals between ratings and the "train" average w/ movie + user bias to serve as the added genres bias
b_genres_table <- train %>% 
  left_join(b_movie_table, by='movieId') %>% 
  left_join(b_user_table, by='userId') %>% 
  group_by(genres) %>% 
  summarize(b_genres = mean(rating - mu - b_movie - b_user))
# Join the raw genre combination bias table to "test" and evaluate RMSE
b_movie_user_genres <- test %>% 
  left_join(b_movie_table, by = "movieId") %>% 
  left_join(b_user_table, by = "userId") %>% 
  left_join(b_genres_table, by = "genres") %>% 
  mutate(pred = mu + b_movie + b_user + b_genres) %>%
  pull(pred)
# Evaluate RMSE
mu_movie_user_genres_rmse <- round(RMSE(b_movie_user_genres, test$rating), 5)
# Add new model RMSE to RMSE results table for comparison
rmse_results_table <- rmse_results_table %>% rbind(c("Average w/ Movie + User + Genres Effects", mu_movie_user_genres_rmse))
rmse_results_table

# Filtered genres: "train" and "test" sets need a new filtered genre combination column that filters out IMAX from the genres column
train <- train %>% mutate(
  filtered_genres = paste0(
    ifelse(str_detect(genres, "Action"), "Action ", ""),
    ifelse(str_detect(genres, "Adventure"), "Adventure ", ""),
    ifelse(str_detect(genres, "Animation"), "Animation ", ""),
    ifelse(str_detect(genres, "Children"), "Children ", ""),
    ifelse(str_detect(genres, "Comedy"), "Comedy ", ""),
    ifelse(str_detect(genres, "Crime"), "Crime ", ""),
    ifelse(str_detect(genres, "Documentary"), "Documentary ", ""),
    ifelse(str_detect(genres, "Drama"), "Drama ", ""),
    ifelse(str_detect(genres, "Fantasy"), "Fantasy ", ""),
    ifelse(str_detect(genres, "Film-Noir"), "Film-Noir ", ""),
    ifelse(str_detect(genres, "Horror"), "Horror ", ""),
    #ifelse(str_detect(genres, "IMAX"), "IMAX ", ""),    # IMAX category commented out from string detection formula
    ifelse(str_detect(genres, "Musical"), "Musical ", ""),
    ifelse(str_detect(genres, "Mystery"), "Mystery ", ""),
    ifelse(str_detect(genres, "Romance"), "Romance ", ""),
    ifelse(str_detect(genres, "Sci-Fi"), "Sci-Fi ", ""),
    ifelse(str_detect(genres, "Thriller"), "Thriller ", ""),
    ifelse(str_detect(genres, "War"), "War ", ""),
    ifelse(str_detect(genres, "Western"), "Western ", "")
  ))
train <- as.data.frame(train)
test <- test %>% mutate(
  filtered_genres = paste0(
    ifelse(str_detect(genres, "Action"), "Action ", ""),
    ifelse(str_detect(genres, "Adventure"), "Adventure ", ""),
    ifelse(str_detect(genres, "Animation"), "Animation ", ""),
    ifelse(str_detect(genres, "Children"), "Children ", ""),
    ifelse(str_detect(genres, "Comedy"), "Comedy ", ""),
    ifelse(str_detect(genres, "Crime"), "Crime ", ""),
    ifelse(str_detect(genres, "Documentary"), "Documentary ", ""),
    ifelse(str_detect(genres, "Drama"), "Drama ", ""),
    ifelse(str_detect(genres, "Fantasy"), "Fantasy ", ""),
    ifelse(str_detect(genres, "Film-Noir"), "Film-Noir ", ""),
    ifelse(str_detect(genres, "Horror"), "Horror ", ""),
    #ifelse(str_detect(genres, "IMAX"), "IMAX ", ""),    # IMAX category commented out from string detection formula
    ifelse(str_detect(genres, "Musical"), "Musical ", ""),
    ifelse(str_detect(genres, "Mystery"), "Mystery ", ""),
    ifelse(str_detect(genres, "Romance"), "Romance ", ""),
    ifelse(str_detect(genres, "Sci-Fi"), "Sci-Fi ", ""),
    ifelse(str_detect(genres, "Thriller"), "Thriller ", ""),
    ifelse(str_detect(genres, "War"), "War ", ""),
    ifelse(str_detect(genres, "Western"), "Western ", "")
  )) 
test <- as.data.frame(test)
# Make a table of filtered genre-grouped average residuals between ratings and the "train" set average w/ movie + user bias to serve as the added filtered genres bias
b_filtered_genres_table <- train %>% 
  left_join(b_movie_table, by='movieId') %>% 
  left_join(b_user_table, by='userId') %>% 
  group_by(filtered_genres) %>% 
  summarize(b_filtered_genres = mean(rating - mu - b_movie - b_user))
# Join the filtered genre combination bias table to "test" and evaluate RMSE
b_movie_user_filtered_genres <- test %>% 
  left_join(b_movie_table, by = "movieId") %>% 
  left_join(b_user_table, by = "userId") %>% 
  left_join(b_filtered_genres_table, by = "filtered_genres") %>% 
  mutate(pred = mu + b_movie + b_user + b_filtered_genres) %>%
  pull(pred)
# Evaluate RMSE
mu_movie_user_filtered_genres_rmse <- round(RMSE(b_movie_user_filtered_genres, test$rating), 5)
# Add new model RMSE to RMSE results table for comparison
rmse_results_table <- rmse_results_table %>% rbind(c("Average w/ Movie + User + Filtered Genres Effects", mu_movie_user_filtered_genres_rmse))
rmse_results_table
# The table shows that filtering is not effective compared to using genres as-is
# Filtered genres-related results not needed going forwards and can be removed from the results table
rmse_results_table <- rmse_results_table[-nrow(rmse_results_table), ]
rm(b_filtered_genres_table, b_movie_user_filtered_genres, mu_movie_user_filtered_genres_rmse)


# Adding release year effects to the average w/ movie + user + genres effects model:
# "Train" and "test" sets need a new movie year column that extracts release year from movie titles
train <- train %>% mutate(movie_year =  as.integer(substr(title, nchar(title) - 4, nchar(title) - 1))) 
train <- as.data.frame(train)
test <- test %>% mutate(movie_year =  as.integer(substr(title, nchar(title) - 4, nchar(title) - 1))) 
test <- as.data.frame(test)
# Make a table of release year-grouped average residuals between ratings and the "train" set average w/ movie + user + genres bias to serve as the added movie year bias
b_movie_year_table <- train %>% 
  left_join(b_movie_table, by='movieId') %>% 
  left_join(b_user_table, by='userId') %>% 
  left_join(b_genres_table, by='genres') %>% 
  group_by(movie_year) %>% 
  summarize(b_movie_year = mean(rating - mu - b_movie - b_user - b_genres))
# Join the movie release year bias table to "test" and evaluate RMSE
b_movie_user_genres_movie_year <- test %>% 
  left_join(b_movie_table, by = "movieId") %>% 
  left_join(b_user_table, by = "userId") %>% 
  left_join(b_genres_table, by = "genres") %>% 
  left_join(b_movie_year_table, by = "movie_year") %>% 
  mutate(pred = mu + b_movie + b_user + b_genres + b_movie_year) %>%
  pull(pred)
# Evaluate RMSE
mu_movie_user_genres_movie_year_rmse <- round(RMSE(b_movie_user_genres_movie_year, test$rating), 5)
# Add new model RMSE to RMSE results table for comparison
rmse_results_table <- rmse_results_table %>% rbind(c("Average w/ Movie + User + Genres + Release Year Effects", mu_movie_user_genres_movie_year_rmse))
rmse_results_table 


# Regularizing the "train" average w/ movie + user + genres + release year effects model:
# Define a set of penalized least squares lambdas for cross-validation
lambdas <- seq(0, 6, 0.2)
# Apply regularization to the model and create a table of RMSE's for each cross-validated lambda
rmses <- sapply(lambdas, function(lambda){
  # Make a table of movie-grouped penalized average residuals between ratings and the "train" set average to serve as the added regularized movie bias
  b_reg_movie_table <- train %>%
    group_by(movieId) %>%
    summarise(b_reg_movie = sum(rating - mu) / (n() + lambda))
  # Make a table of user-grouped penalized average residuals between ratings and the "train" average w/ regularized movie effects to serve as the added regularized user bias
  b_reg_user_table <- train %>%
    left_join(b_reg_movie_table, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_reg_user = sum(rating - mu - b_reg_movie) / (n() + lambda))
  # Make a table of genres-grouped penalized average residuals between ratings and "train" average w/ regularized movie + user bias to serve as the added regularized genres bias
  b_reg_genres_table <- train %>%
    left_join(b_reg_movie_table, by="movieId") %>%
    left_join(b_reg_user_table, by="userId") %>%
    group_by(genres) %>%
    summarise(b_reg_genres = sum(rating - mu - b_reg_movie - b_reg_user) / (n() + lambda))
  # Make a table of movie year-grouped penalized average residuals between ratings and "train" average w/ regularized movie + user + genres bias to serve as the added movie year bias
  b_reg_movie_year_table <- train %>%
    left_join(b_reg_movie_table, by="movieId") %>%
    left_join(b_reg_user_table, by="userId") %>%
    left_join(b_reg_genres_table, by="genres") %>%
    group_by(movie_year) %>%
    summarise(b_reg_movie_year = sum(rating - mu - b_reg_movie - b_reg_user - b_reg_genres) / (n() + lambda))
  # Join the regularized bias tables to the "test" set and evaluate RMSE
  b_reg_movie_user_genres_movie_year <- test %>%
    left_join(b_reg_movie_table, by="movieId") %>%
    left_join(b_reg_user_table, by="userId") %>%
    left_join(b_reg_genres_table, by="genres") %>%
    left_join(b_reg_movie_year_table, by="movie_year") %>%
    mutate(b_reg_movie_user_genres_movie_year = mu + b_reg_movie + b_reg_user + b_reg_genres + b_reg_movie_year) %>%
    pull(b_reg_movie_user_genres_movie_year)
  # Evaluate RMSE
  return(RMSE(b_reg_movie_user_genres_movie_year, test$rating))
})
reg_lambdas <- data.frame(lambdas, rmses)

# Identify the lambda associated with the lowest RMSE
best_lambda <- lambdas[which.min(rmses)]

# Scatter plot of regularized RMSE's as a function of each lambda screened
reg_lambdas %>% ggplot(aes(x = lambdas, y = rmses)) +
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 6, by = 0.5)) +
  geom_vline(xintercept = best_lambda, color = "black", linetype = "solid", linewidth = 1) + 
  labs(title = "Optimal Lambda for Model Regularization", x = "Lambda", y = "RMSE")

# Apply best lambda with the minimized RMSE to the regularized model
# Make a table of movie-grouped penalized average residuals between ratings and the "train" set average to serve as the added regularized movie bias
b_reg_movie_table <- train %>%
  group_by(movieId) %>%
  summarise(b_reg_movie = sum(rating - mu) / (n() + best_lambda))
# Make a table of user-grouped penalized average residuals between ratings and the "train" average w/ regularized movie effects to serve as the added regularized user bias
b_reg_user_table <- train %>%
  left_join(b_reg_movie_table, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_reg_user = sum(rating - mu - b_reg_movie) / (n() + best_lambda))
# Make a table of genres-grouped penalized average residuals between ratings and "train" average w/ regularized movie + user bias to serve as the added regularized genres bias
b_reg_genres_table <- train %>%
  left_join(b_reg_movie_table, by="movieId") %>%
  left_join(b_reg_user_table, by="userId") %>%
  group_by(genres) %>%
  summarise(b_reg_genres = sum(rating - mu - b_reg_movie - b_reg_user) / (n() + best_lambda))
# Make a table of movie year-grouped penalized average residuals between ratings and "train" average w/ regularized movie + user + genres bias to serve as the added movie year bias
b_reg_movie_year_table <- train %>%
  left_join(b_reg_movie_table, by="movieId") %>%
  left_join(b_reg_user_table, by="userId") %>%
  left_join(b_reg_genres_table, by="genres") %>%
  group_by(movie_year) %>%
  summarise(b_reg_movie_year = sum(rating - mu - b_reg_movie - b_reg_user - b_reg_genres) / (n() + best_lambda))
# Join the regularized bias tables to the "test" set and evaluate RMSE
b_reg_movie_user_genres_movie_year <- test %>%
  left_join(b_reg_movie_table, by="movieId") %>%
  left_join(b_reg_user_table, by="userId") %>%
  left_join(b_reg_genres_table, by="genres") %>%
  left_join(b_reg_movie_year_table, by="movie_year") %>%
  mutate(b_reg_movie_user_genres_movie_year = mu + b_reg_movie + b_reg_user + b_reg_genres + b_reg_movie_year) %>%
  pull(b_reg_movie_user_genres_movie_year)
# Evaluate RMSE
mu_reg_movie_user_genres_movie_year_rmse <- round(RMSE(b_reg_movie_user_genres_movie_year, test$rating), 5)
# Add regularized RMSE to RMSE results table for comparison
rmse_results_table <- rmse_results_table %>% rbind(c("Average w/ Regularized Movie + User + Genres + Release Year Effects", mu_reg_movie_user_genres_movie_year_rmse))
rmse_results_table

# With regularization shown to be effective, unregularized bias tables can be removed
rm(b_movie_table, b_user_table, b_genres_table, b_movie_year_table, b_movie, b_movie_user, b_movie_user_genres, b_movie_user_genres_movie_year)

# Adding rating timestamp effects to the average w/ regularized movie + user + genres + movie year effects model:
# Since timestamps are continuous and do not have guaranteed matches between training and test sets, grouped average table joining approach will not work
# Instead, make a column in the training set of residuals between actual ratings and predicted ratings from the regularized model
train <- train %>% 
  left_join(b_reg_user_table, by = 'userId') %>%
  left_join(b_reg_movie_table, by = 'movieId') %>%
  left_join(b_reg_genres_table, by = 'genres') %>%
  left_join(b_reg_movie_year_table, by = 'movie_year') %>%
  mutate(residual = rating - mu - b_reg_movie - b_reg_user - b_reg_genres - b_reg_movie_year)
# Histogram of the residuals from the regularized model
train %>% ggplot(aes(x = residual)) + 
  geom_histogram(binwidth = 0.1, color = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1) + 
  labs(title = "Distribution of Residuals from Best Regularized Model", x = "Residual", y = "Counts")
# Apply a linear fit between residuals and rating timestamps and define the slope and intercept
timestamp_all_coefs <- coef(lm(train$residual ~ train$timestamp))
timestamp_slope <- as.numeric(timestamp_all_coefs[2])
timestamp_intercept <- as.numeric(timestamp_all_coefs[1])
# Join the regularized bias tables to the test set
# Create a timestamp residual column using the slope and intercept 
# Create a predicted rating column that adds a given rating's predicted timestamp residual to the training set average and regularized effects
test <- test %>%
  left_join(b_reg_movie_table, by="movieId") %>%
  left_join(b_reg_user_table, by="userId") %>%
  left_join(b_reg_genres_table, by="genres") %>%
  left_join(b_reg_movie_year_table, by="movie_year") %>% mutate(
    timestamp_residual = (timestamp_slope * timestamp) + timestamp_intercept,
    pred_rating =  mu + b_reg_user + b_reg_movie + b_reg_genres + b_reg_movie_year + timestamp_residual)
# Evaluate RMSE
mu_reg_movie_user_genres_movie_year_res_timestamp_rmse <- round(RMSE(test$rating, test$pred_rating), 5)
# Add new model RMSE to RMSE results table for comparison
rmse_results_table <- rmse_results_table %>% rbind(c("Average w/ Regularized Movie + User + Genres + Release Year Effects & Timestamp Residual Fit", mu_reg_movie_user_genres_movie_year_res_timestamp_rmse))
rmse_results_table


##########################################################
# FINAL HOLDOUT TEST
##########################################################

# Final holdout test set needs a new movie year column that extracts release year from movie titles for the movie year component of the final model to join onto
final_holdout_test_model <- final_holdout_test %>% mutate(movie_year =  as.integer(substr(title, nchar(title) - 4, nchar(title) - 1))) 
# Join the regularized bias tables to the test set
# Create an estimated timestamp residual column using the "train" set fitted slope and intercept formula
# Create a predicted rating column summing the "train" average, regularized biases, and estimated timestamp residual
final_holdout_test_pred <- final_holdout_test_model %>%
  left_join(b_reg_movie_table, by = "movieId") %>% 
  left_join(b_reg_user_table, by = "userId") %>% 
  left_join(b_reg_genres_table, by = "genres") %>% 
  left_join(b_reg_movie_year_table, by = "movie_year") %>% mutate(
    timestamp_residual = (timestamp_slope * timestamp) + timestamp_intercept,
    pred_rating = mu + b_reg_movie + b_reg_user + b_reg_genres + b_reg_movie_year + timestamp_residual)
# Evaluate RMSE
final_holdout_test_rmse <- round(RMSE(final_holdout_test_pred$pred_rating, final_holdout_test$rating), 5)

# Table to verify final holdout test set RMSE is below the course goal RMSE
final_rmse_results_table <- data.frame(Model = c("Course Goal RMSE", "Best Model Training RMSE", "Best Model Final Holdout Set RMSE"), RMSE = c(goal_rmse, mu_reg_movie_user_genres_movie_year_res_timestamp_rmse, final_holdout_test_rmse)) 
final_rmse_results_table 

# Histogram of the final holdout test prediction residuals
final_holdout_test_pred %>% mutate(residual = pred_rating - rating) %>%
  ggplot(aes(x = residual)) + 
    geom_histogram(binwidth = 0.1, color = "black") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1) + 
    labs(title = "Distribution of Final Holdout Test Residuals from Final Model", x = "Residual", y = "Counts")

