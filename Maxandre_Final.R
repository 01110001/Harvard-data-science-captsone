##########################################################
# Create edx and final_holdout_test sets 
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
validation <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
validation_set <- validation %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(validation, validation_set)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, validation, movielens, removed)


# Analyzing the data --------------------------------------------------------

#check if there are any missing data in the trainning set
print(sum(is.na(edx)))

#check if there is any missing data in the validation set
print(sum(is.na(validation_set)))

str(edx) # data type

# Data exploration ----------------------------------------------------------

# Create a new data frame to store summary information
df_summary <- data.frame(
  "Number of Movies" = n_distinct(edx$movieId),      # Calculate the number of distinct movies
  "Number of Users" = n_distinct(edx$userId),        # Calculate the number of distinct users
  "Number of movies genres" = n_distinct(edx$genres) # Calculate the number of distinct movie genres
)

# Render the "df_summary" data frame as a nicely formatted table
knitr::kable(df_summary)

# Top 10 most rated movies: Calculate the top 10 movies with the highest number of ratings.
numRatings <- edx %>% group_by(movieId) %>% 
  summarize(numRatings = n(), movieTitle = first(title)) %>%
  arrange(desc(numRatings)) %>%
  top_n(10, numRatings) %>%
  knitr::kable(caption = "Top 10 most rated movies")

# Top 10 best movies by their ratings: Find the top 10 movies with the highest average ratings.
best.rated <- edx %>% group_by(movieId) %>% 
  summarize(mean_rating = mean(rating), movieTitle = first(title)) %>%
  arrange(desc(mean_rating)) %>%
  top_n(10, mean_rating) %>%
  knitr::kable(caption = "Top 10 best movies by their ratings")

# Top 10 worst movies: Identify the top 10 movies with the lowest average ratings.
worst.rated <- edx %>% group_by(movieId) %>% 
  summarize(mean_rating = mean(rating), movieTitle = first(title)) %>%
  arrange(mean_rating) %>%
  top_n(10, mean_rating) %>%
  knitr::kable(caption = "Top 10 worst movies")


# Calculate the Spearman correlation matrix for "rating," "timestamp," and "userId" columns in the "edx" dataset.
correlation_matrix <- cor(edx[, c("rating", "timestamp", "userId")], method = "spearman")


# Display the correlation matrix in a formatted table using `knitr::kable()`.
knitr::kable(correlation_matrix, caption = "Correlation matrix")


# Create summarized data frame
# Create a summarized data frame "movie_counts" with the count of each movieId in the "edx" dataset.
movie_counts <- edx %>% count(movieId)

# Create a plot using ggplot to visualize the distribution of the number of ratings per movie.
ggplot(movie_counts, aes(n)) +
  geom_histogram(bins=30, color="black") +
  scale_x_log10() +
  labs(x = "Movies", y = "Number of Ratings", caption = "Source: edx dataset") +
  ggtitle("Distribution of Number of Ratings per Movie (Log Scale)")

# Create a new data frame "movie_rating_avg" that calculates the average rating for each movieId in the "edx" dataset.
movie_rating_avg <- edx %>% select(rating, movieId) %>% group_by(movieId) %>% summarise(avg_rating = mean(rating, na.rm = TRUE))

# Create a plot using ggplot to visualize the distribution of average ratings per movie.
ggplot(movie_rating_avg, aes(avg_rating)) +
  geom_histogram(bins = 30, color = "black") +
  labs(x = "Average rating", y = "Number of movies", caption = "Source: edx dataset") +
  ggtitle("Distribution of Average Ratings per Movie")

# Number of ratings by year: Calculate the count of ratings per year in the "edx" dataset.
n_movie_year <- edx %>%
  select(rating, timestamp) %>%
  mutate(year = year(as.POSIXct(timestamp, origin = "1970-01-01"))) %>%
  group_by(year) %>%
  summarise(count = n())

# Create a plot using ggplot to visualize the number of movie ratings per year.
ggplot(n_movie_year, aes(x = year, y = count)) +
  geom_col() +
  labs(x = "Year", y = "Number of Ratings", 
       title = "Number of Movie Ratings per Year", 
       caption = "Source: edx dataset")


# Average rating by year: Calculate the average rating per year in the "edx" dataset.
mean_movie_year <- edx %>%
  select(rating, timestamp) %>%
  mutate(year = year(as.POSIXct(timestamp, origin = "1970-01-01"))) %>%
  group_by(year) %>%
  summarise(avg_rating = mean(rating))

# Create a plot using ggplot to visualize the average movie ratings per year.
ggplot(mean_movie_year, aes(x = year, y = avg_rating)) +
  geom_col() +
  labs(x = "Year", y = "Average of Ratings", 
       title = "Average of Movie Ratings per Year", 
       caption = "Source: edx dataset")


# Number of movie ratings by released year: Calculate the count of ratings for each released year in the "edx" dataset.
n_movie_rel_year <- edx %>%
  mutate(year = str_extract(title, "\\((\\d{4})\\)")) %>%
  mutate(year = str_sub(year, 2, -2)) %>%
  group_by(year) %>%
  summarise(n = n())

# Create a plot using ggplot to visualize the number of movie ratings by release year.
ggplot(n_movie_rel_year, aes(x = year, y = n)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +  # format y-axis labels with commas for thousands
  scale_x_discrete(breaks = seq(min(n_movie_rel_year$year, na.rm = TRUE), max(n_movie_rel_year$year, na.rm = TRUE), by = 10)) +  # adjust x-axis breaks
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotate x-axis labels
  labs(x = "Release Year", y = "Number of Ratings", 
       title = "Number of Movie Ratings by Release Year", 
       caption = "Source: edx dataset")


# Average rating by release year: Calculate the average rating for each released year in the "edx" dataset.
mean_movie_rel_year <- edx %>%
  mutate(year = str_extract(title, "\\((\\d{4})\\)")) %>%
  mutate(year = str_sub(year, 2, -2)) %>%
  group_by(year) %>%
  summarise(avg_rating = mean(rating))

# Create a plot using ggplot to visualize the average movie ratings by release year.
ggplot(mean_movie_rel_year, aes(x = year, y = avg_rating)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +  # format y-axis labels with commas for thousands
  scale_x_discrete(breaks = seq(min(mean_movie_rel_year$year, na.rm = TRUE), max(mean_movie_rel_year$year, na.rm = TRUE), by = 10)) +  # adjust x-axis breaks
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotate x-axis labels
  labs(x = "Release Year", y = "Average of Ratings", 
       title = "Average Movie Ratings by Release Year", 
       caption = "Source: edx dataset")


# Most rated genre by number of ratings (n()):
best_genre <- edx %>%
  select(rating, genres) %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

# Create a plot using ggplot to visualize the top 10 genres with the highest number of ratings.
ggplot(best_genre, aes(x = reorder(genres, count), y = count)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Genre", y = "Number of Ratings", 
       title = "Top 10 Genres by Number of Ratings", 
       caption = "Source: edx dataset")


# Top 10 genre by their average rating:
mean_genre <- edx %>%
  select(rating, genres) %>%
  group_by(genres) %>%
  summarise(mean = mean(rating)) %>%
  arrange(desc(mean)) %>%
  head(10)

# Create a plot using ggplot to visualize the top 10 genres with the highest average ratings.
ggplot(mean_genre, aes(x = reorder(genres, mean), y = mean)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Genre", y = "Mean of Ratings", 
       title = "Top 10 Genres by Mean of Ratings", 
       caption = "Source: edx dataset")


# Result by Model ---------------------------------------------------------
#This is the formula for the RMSE
rmse <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


#First model the mean of all ratings
mean_rating <- mean(edx$rating)
mean_rating

# Now if we test it on the validation test
only_mean_rmse <- rmse(validation_set$rating,mean_rating)
only_mean_rmse

rmse_table_results <- data.frame(model="Model 1",RMSE=only_mean_rmse)
rmse_table_results %>% knitr::kable(caption="Table of all model result")


b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mean_rating))

model_2_pred_ratings <- validation_set %>%
  left_join(b_i, by='movieId') %>%
  mutate(pred = mean_rating + b_i) %>%
  pull(pred)

RMSE_model_2 <- rmse(model_2_pred_ratings, validation_set$rating)
RMSE_model_2

rmse_table_results <- bind_rows(rmse_table_results,
                                data.frame(Model="Model 2",RMSE=RMSE_model_2))

rmse_table_results %>% knitr::kable()



b_u <- edx %>% 
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mean_rating - b_i))

model_3_pred_ratings <- validation_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = mean_rating + b_i + b_u) %>%
  pull(pred)

RMSE_model_3 <- rmse(model_3_pred_ratings, validation_set$rating)
RMSE_model_3

rmse_table_results <- bind_rows(rmse_table_results,
                                data.frame(Model="Model 3",RMSE=RMSE_model_3))

rmse_table_results %>% knitr::kable()

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(lambda){
  
  b_i <- edx %>% # Movie Effect (Regularized)
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mean_rating)/(n()+lambda))
  
  b_u <- edx %>% # Movie and User Effect (Regularized)
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mean_rating)/(n()+lambda))
  
  model_4_pred_ratings <- validation_set %>% # Predicted Ratings based on Model 4
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mean_rating + b_i + b_u) %>%
    pull(pred) # We want to use predictions based on Model 4
  
  return(rmse(model_4_pred_ratings, validation_set$rating)) # We want RMSE
})


lambda_min <- lambdas[which.min(rmses)]
lambda_min
qplot(lambdas,rmses)


RMSE_model_4 <- min(rmses)
RMSE_model_4

rmse_table_results <- bind_rows(rmse_table_results,
                                data.frame(Model="Model 4",RMSE=RMSE_model_4))

rmse_table_results %>% knitr::kable()



# Final result ----------------------------------------------------------
rmse_table_results


# Thank you.
















































































