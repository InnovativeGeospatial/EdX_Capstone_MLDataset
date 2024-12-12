
#Workspace Prep
Packages <- c("pander", "vctrs", "readr", "caTools", "magrittr", "ggpubr","ggplot2","corrplot", "party", "MuMIn", "lme4", "drat", "xgboost", "caret", "dplyr", "plyr", "stringr", "gsubfn", "lubridate", "xts", "fpp2", "data.table", "sf", "spData", "tmap", "leaflet", "fuzzyjoin", "mapdeck", "data.table", "summarytools", "rsample", "tinytex", "stringr", "recosystem")

for (pkg in Packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)  }
  print("package available")
}
if (!requireNamespace("tinytex", quietly = TRUE)) install.packages("tinytex")
tinytex::install_tinytex() #does not always install with others above.

options(max.print=5000)
options(timeout = 120)
options(scipen = 999)#Remove scientific notation from plots below.
#memory.limit(size = 100000)


Working_Dir <- "C:/Users/Justin.White\\OneDrive - afacademy.af.edu\\Desktop\\DownloadTime\\HarvardStats_2024\\DataScience_Capstone_2024\\Capstone_MovieLens"
setwd(Working_Dir)



#########################
#Acquire and Wrangle Data
#########################

#Download file.
URL <- "https://files.grouplens.org/datasets/movielens/ml-10m.zip"
FiletoDownload <- "ml-10m.zip"
download.file(URL, FiletoDownload, mode="wb")

#Unzip the file if it was downloaded properly. Print outcome.
unzip(FiletoDownload, exdir = Working_Dir)

#Delete zipped folder
unlink(FiletoDownload)

#Configure Movies_Ratings object
Movie_Ratings <- as.data.frame(str_split(read_lines("./ml-10M100K/ratings.dat"), fixed("::"), simplify = TRUE), stringsAsFactors = FALSE)

colnames(Movie_Ratings) <- c("userId", "movieId", "rating", "timestamp")
Movie_Ratings <- Movie_Ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

#Configure Movies_Data object
Movies_Data <- as.data.frame(str_split(read_lines("./ml-10M100K/movies.dat"), fixed("::"), simplify = TRUE), stringsAsFactors = FALSE)

colnames(Movies_Data) <- c("movieId", "title", "genres")

Movies_Data <- Movies_Data %>% 
  mutate(movieId = as.integer(movieId), 
         title = as.character(title), 
         genres = as.character(genres))


#Combine movies with ratings
MovieLens_Dataset <- left_join(Movie_Ratings, Movies_Data, by = "movieId")
nrow(MovieLens_Dataset) #Ensure we have the complete dataset.

#Convert time into something discernible
MovieLens_Dataset$timestamp <- as.POSIXct(MovieLens_Dataset$timestamp, origin="1970-01-01")
MovieLens_Dataset$Year <- format(MovieLens_Dataset$timestamp, "%Y")
MovieLens_Dataset$Month <- format(MovieLens_Dataset$timestamp, "%B")
MovieLens_Dataset <- MovieLens_Dataset %>% mutate(Age = 2024 - as.numeric(Year))
MovieLens_Dataset$Age <- as.integer(MovieLens_Dataset$Age)
str(MovieLens_Dataset)


#########################
#Visually Explore Dataset
#########################

head(MovieLens_Dataset)

#Visualize the distribution of movie ratings.
Figure.1 <- ggplot(MovieLens_Dataset, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "grey", color = "blue") +
  labs(title = "Histogram Distribution of Movie Ratings", x = "Movie Rating", y = "Frequency", caption = "Data Source: MovieLens") +
  theme_light()
Figure.1

#Examine the frequency of movies per year.
MovieLens_Dataset$rating <-as.numeric(MovieLens_Dataset$rating)
Figure.2 <- ggplot(MovieLens_Dataset, aes(x = as.numeric(Year))) +
  geom_histogram(binwidth = 0.5, fill = "grey", color = "blue") +
  labs(title = "Distribution of Movie Ratings per Year", x = "Rating", y = "Frequency", caption = "Data Source: MovieLens") +
  theme_light()
Figure.2

#Visualize the average movie numerical rating per year.
Ave_Annual_Rating <- aggregate(rating ~ Year, data = MovieLens_Dataset, FUN = mean)
Figure.3 <- ggplot(Ave_Annual_Rating, aes(x = as.numeric(Year), y = rating)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Average Movie Ratings by Year", x = "Year", y = "Average Movie Rating (0.0 - 5.0)", caption = "Data Source: MovieLens") +
  expand_limits(y = c(3, 4)) +
  theme_light()
Figure.3


#Sample 50k rows for ease of graphing. Older movies have more time to be rated.
SubSample_50k <- MovieLens_Dataset[sample(nrow(MovieLens_Dataset), size = 50000), ]


# Average Movie Ratings By Month - Any monthly biases?
month_order <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")
Ave_Monthly_Rating <- aggregate(rating ~ Month, data = MovieLens_Dataset, FUN = mean)
Ave_Monthly_Rating$Month <- factor(Ave_Monthly_Rating$Month, levels = month_order)


Figure.4 <- ggplot(Ave_Monthly_Rating, aes(x = Month, y = rating, group = 1)) +
  geom_line(color = "blue", size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Adjust angle and horizontal justification
  expand_limits(y = c(3, 4)) +
  ggtitle("Average Movie Ratings by Month", subtitle = "Spanning 1995 to 2009") +
  xlab("Month") +
  ylab("Average Movie Rating (0.0 - 5.0)") +
  labs(caption = "Data Source: MovieLens") +
  theme_light()
Figure.4


#Examine the frequency of ratings per user.
SubSample_50k <- MovieLens_Dataset[sample(nrow(MovieLens_Dataset), size = 50000), ]
User_and_Ratings <- aggregate(rating ~ userId, data = SubSample_50k, FUN = length)
colnames(User_and_Ratings)[2] <- "Number_Ratings"
Figure.5 <- ggplot(User_and_Ratings, aes(x = Number_Ratings)) +
  geom_histogram(binwidth = 1, fill = "light blue", color = "black") +  ggtitle("Distribution of the Number of Ratings per User", subtitle = "Subsample of 50,000 records")+
  xlab("Frequency of Ratings")+
ylab("Frequency of User") +
  theme_minimal()
Figure.5


#Examine the relationship between movie age and rating received.
Figure.6 <- ggplot(SubSample_50k, aes(x = Age, y = rating)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", se = TRUE) + 
  labs(title = "Relationship Between Movie Age and Rating Received",
       x = "Movie Age", y = "Rating", caption = "Data Source: MovieLens") +
  theme_minimal()
Figure.6

#Examine frequencies of genres
GenreCount.Dataframe <- MovieLens_Dataset %>% group_by(genres) %>% tally() %>% mutate(proportion = n/sum(n)) %>% top_n(25)  %>% filter(proportion > 0.01) %>% arrange(proportion)
as.data.frame(GenreCount.Dataframe)

Figure.7 <- GenreCount.Dataframe %>% ggplot(aes(x =reorder(genres,proportion, sum), y = proportion, fill = proportion)) + geom_bar(stat = "identity") + scale_fill_gradient(low = "blue", high = "red") +coord_flip() + labs(x="Genre", y="Proportion of total movies", title="Proportion of movie genres \n comprising the MovieLens Dataset (top 15)" , caption = "Data Source: MovieLens")
Figure.7

#Generate basic summaries of entire dataset
Data_Summary <- MovieLens_Dataset %>% select(-c(userId, timestamp, movieId))
Figure.8 <- stview(dfSummary(Data_Summary,
                 graph.col = TRUE,
                 style = "grid",
                 graph.magnif = 1,
                 tmp.img.dir  = "."))
Figure.8
####################################################

# Final hold-out test dataset will be 10% of MovieLens data
set.seed(1) 
test_index <- createDataPartition(y = MovieLens_Dataset$rating, times = 1, p = 0.1, list = FALSE)
edx <- MovieLens_Dataset[-test_index,]
temp <- MovieLens_Dataset[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- suppressMessages(anti_join(temp, final_holdout_test))
edx <- rbind(edx, removed)


#Create Training and Testing Dataset to investigate various types of preliminary models.
set.seed(123)  
MovieLens_split <- group_initial_split(edx, group = userId, prop = 0.8)
training_dataset <- training(MovieLens_split)
testing_dataset <- testing(MovieLens_split)
nrow(training_dataset)
nrow(testing_dataset)

rm(Movie_Ratings, Movies_Data, test_index, MovieLens_split)

#Ensure the same movies are in both the training and testing datasets. 
testing_dataset <- testing_dataset |> 
  semi_join(training_dataset, by = "movieId")
training_dataset <- testing_dataset |> 
  semi_join(testing_dataset, by = "movieId")


#Create basic RMSE function for upcoming testing
RMSE <- function(true_ratings, predicted_ratings) {
  rmse_value <- sqrt(mean((true_ratings - predicted_ratings)^2))
  return(rmse_value)
}


#Basic model from textbook that predicts movie rating with no influence from user. 
mu_train <- mean(training_dataset$rating)

training_dataset <- training_dataset %>% mutate(mu = mu_train)
RMSE_Naive <- RMSE(testing_dataset$rating, mu_train)

#We can tell that the mean model is better than a model including any other number, which isn't surprising. If we create a vector where each value is 3.4 (very similar to the mean), and then examine the RMSE, we can see it gets worse. 
Prediction_3.4 <- rep(3.4, nrow(testing_dataset))
RMSE_3.4 <- RMSE(testing_dataset$rating, Prediction_3.4)

RMSE_Table <- tibble(Model = c("Mean Model", "Single Value (3.4) Model"),
  RMSE = format(c(RMSE_Naive, RMSE_3.4), nsmall = 4))
RMSE_Table

#Some films have different rating distributions than others simply because of a variety of biases at play. So, lets include a model term to include the average rating of film i. 
Movie_Bias <- training_dataset %>% dplyr::group_by(movieId) %>%
  dplyr::summarise(Movie_Bias = mean(rating - mu_train))

#Distribution of movie_bias indicates that we were correct: biases vary between movies adding error to our original prediction.
ggplot(Movie_Bias, aes(x = Movie_Bias)) +
  geom_histogram(bins = 20, fill = "darkblue", color = "black") +
  labs(title = "Distribution of Movie Biases", x = "Movie Bias", y = "Count") +
  theme_light()

Prediction_MovBi <- testing_dataset %>% left_join(Movie_Bias, by = "movieId") %>%  mutate(prediction = mu_train + Movie_Bias) %>%  .$prediction
RMSE_Movie_Bias <- RMSE(testing_dataset$rating, Prediction_MovBi)

RMSE_Table <- tibble(Model = c("Mean Model", "Single Value (3.4) Model", "Movie Bias Model"), RMSE = format(c(RMSE_Naive, RMSE_3.4, RMSE_Movie_Bias), nsmall = 4))
RMSE_Table

#Similar to bias in the films themselves, users may have be more likely to rate a movie higher or lower more frequently than other viewers.
# Ensure 'userId' is a character 

User_Bias <- training_dataset %>% left_join(Movie_Bias, by = "movieId") %>% dplyr::group_by(userId) %>%
  dplyr::summarise(User_Bias = mean(rating - mu_train - Movie_Bias, na.rm = TRUE))


Prediction_UserBi <- testing_dataset %>%
  left_join(Movie_Bias, by = "movieId") %>%
  left_join(User_Bias, by = 'userId') %>%
  mutate(prediction2 = mu_train + Movie_Bias + User_Bias) %>%
  .$prediction2

RMSE_User_Bias <- RMSE(testing_dataset$rating, Prediction_UserBi)

RMSE_Table <- tibble(Model = c("Mean Model", "Single Value (3.4) Model", "Movie Bias Model", "Movie+User Bias Model"), RMSE = format(c(RMSE_Naive, RMSE_3.4, RMSE_Movie_Bias, RMSE_User_Bias), nsmall = 4))
RMSE_Table


#Similar to biases above, genres vary from mu, which could further inform a prediction.

Genre_Bias <- training_dataset %>% left_join(Movie_Bias, by = "movieId") %>%
  left_join(User_Bias, by = 'userId') %>%
  dplyr::group_by(genres) %>%
  dplyr::summarise(Genre_Bias = mean(rating - mu_train - Movie_Bias - User_Bias, na.rm = TRUE))

 
Prediction_GenreBi <- testing_dataset %>%
  left_join(Movie_Bias, by = "movieId") %>%
  left_join(User_Bias, by = 'userId') %>%
  left_join(Genre_Bias, by = 'genres') %>%
  mutate(prediction3 = mu_train + Movie_Bias + User_Bias + Genre_Bias) %>%
  .$prediction3

RMSE_Genre_Bias <- RMSE(testing_dataset$rating, Prediction_GenreBi)

RMSE_Table <- tibble(Model = c("Mean Model", "Single Value (3.4) Model", "Movie Bias Model", "Movie+User Bias Model", "Movie+User+Genre Bias Model"), RMSE = format(c(RMSE_Naive, RMSE_3.4, RMSE_Movie_Bias, RMSE_User_Bias, RMSE_Genre_Bias), nsmall = 4))
RMSE_Table


#Similar to biases above, movie age adds bias because some movies have had more time to be viewed and rated by users. 

MovieAge_Bias <- training_dataset %>% 
  left_join(Movie_Bias, by = "movieId") %>%
    left_join(User_Bias, by = "userId") %>%
  left_join(Genre_Bias, by = 'genres') %>%
  dplyr::group_by(Age) %>%
  dplyr::summarise(Age_Bias = mean(rating - mu_train - Movie_Bias - User_Bias - Genre_Bias, na.rm = TRUE))


Prediction_AgeBi <- testing_dataset %>%
  left_join(Movie_Bias, by = "movieId") %>%
  left_join(User_Bias, by = 'userId') %>%
  left_join(Genre_Bias, by = 'genres') %>%
  left_join(MovieAge_Bias, by = 'Age') %>%
  mutate(prediction4 = mu_train + Movie_Bias + User_Bias + Genre_Bias + Age_Bias) %>% .$prediction4

RMSE_Age_Bias <- RMSE(testing_dataset$rating, Prediction_AgeBi)

RMSE_Table <- tibble(Model = c("Mean Model", "Single Value (3.4) Model", "Movie Bias Model", "Movie+User Bias Model", "Movie+User+Genre Bias Model", "Movie+Age+User+Genre+Age Bias Model"), RMSE = format(c(RMSE_Naive, RMSE_3.4, RMSE_Movie_Bias, RMSE_User_Bias, RMSE_Genre_Bias, RMSE_Age_Bias), nsmall = 4))
RMSE_Table


#We can see that with the last couple of models we begin to have indications of overfitting. Overfitting is when incorporating too many variables in a model results in a good fit of the training data as it often includes underlying patterns and noise and random fluctuations but poorer performance on new, unseen validation/test data.


#Matrix Factorization is recommended in Chapter 33 of the textbook 
# Install and load recosystem
# You may need to recreate test+training partition (where object 'MovieLens_split' is created above) here to get a fresh training/testing dataset IF you have run it once already.

set.seed(123)
training_dataset <- training_dataset %>%
  left_join(Movie_Bias, by = "movieId") %>%
  left_join(User_Bias, by = "userId") %>%
  left_join(Genre_Bias, by = "genres")

# 1. Adjust Ratings in the Training Dataset
training_dataset <- training_dataset %>%
  dplyr::mutate(adjusted_rating = rating - Movie_Bias - User_Bias - Genre_Bias)

# Convert the training data to the required format
train_data <- data_memory(
  user_index = training_dataset$userId,
  item_index = training_dataset$movieId,
  rating = training_dataset$adjusted_rating)

recommender <- Reco()
recommender$train(
  train_data,
  opts = list(
    dim = 10,          # Number of latent factors
    costp_l1 = 0.1,    # L1 regularization for user factors
    costq_l1 = 0.1,    # L1 regularization for item factors
    lrate = 0.1,       # Learning rate
    niter = 50,        # Number of iterations
    verbose = TRUE     # Print training progress
  )
)

test_data <- data_memory(
  user_index = testing_dataset$userId,
  item_index = testing_dataset$movieId)

# 6. Make Predictions with Adjustments
predicted_adjusted_ratings <- recommender$predict(test_data, out_memory())

testing_dataset <- testing_dataset %>%
  dplyr::mutate(predicted_adjusted_ratings = predicted_adjusted_ratings) %>%
  left_join(Movie_Bias, by = "movieId") %>%
  left_join(User_Bias, by = "userId") %>%
  left_join(Genre_Bias, by = "genres")

testing_dataset <- testing_dataset %>%
  dplyr::mutate(predicted_ratings = predicted_adjusted_ratings + 
           Movie_Bias + User_Bias + Genre_Bias)


# Actual ratings from the testing dataset
actual_ratings <- testing_dataset$rating
predicted_ratings <- testing_dataset$predicted_ratings

RMSE_Factorization <- RMSE(actual_ratings, predicted_ratings)

RMSE_Table <- tibble(Model = c("Mean Model", "Single Value (3.4) Model", "Movie Bias Model", "Movie+User Bias Model", "Movie+User+Genre Bias Model", "Factorization Model"), RMSE = format(c(RMSE_Naive, RMSE_3.4, RMSE_Movie_Bias, RMSE_User_Bias, RMSE_Genre_Bias, RMSE_Factorization), nsmall = 4))
RMSE_Table



###### Now let's run it on Final_Holdout_Test 

test_data_holdout <- data_memory(
  user_index = final_holdout_test$userId,
  item_index = final_holdout_test$movieId)

# 6. Make Predictions with Adjustments
predicted_adjusted_ratings_holdout <- recommender$predict(test_data_holdout, out_memory())

final_holdout_test <- final_holdout_test %>%
  dplyr::mutate(predicted_adjusted_ratings_holdout = predicted_adjusted_ratings_holdout) %>%
  left_join(Movie_Bias, by = "movieId") %>%
  left_join(User_Bias, by = "userId") %>%
  left_join(Genre_Bias, by = "genres")

final_holdout_test <- final_holdout_test %>%
  dplyr::mutate(predicted_ratings = predicted_adjusted_ratings_holdout + 
                  Movie_Bias + User_Bias + Genre_Bias) %>%
  dplyr::filter(!is.na(predicted_ratings))

# Actual ratings from the testing dataset
actual_ratings <- final_holdout_test$rating
predicted_ratings <- final_holdout_test$predicted_ratings


RMSE_Factor_HoldOut <- RMSE(actual_ratings, predicted_ratings)

RMSE_Table <- tibble(Model = c("Mean Model", "Single Value (3.4) Model", "Movie Bias Model", "Movie+User Bias Model", "Movie+User+Genre Bias Model", "Factorization Model", "Factorization Model Holdout"), RMSE = c(RMSE_Naive, RMSE_3.4, RMSE_Movie_Bias, RMSE_User_Bias, RMSE_Genre_Bias, RMSE_Factorization, RMSE_Factor_HoldOut))
RMSE_Table


# Reorder RMSE values and plot
RMSE_Table <- RMSE_Table %>%
  dplyr::mutate(Model = factor(Model, levels = Model[order(-RMSE)]))

ggplot(RMSE_Table, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.865, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = round(RMSE, 4)), vjust = -0.5) +  # Add RMSE values above bars
  labs(title = "RMSE of Different Models", x = "Model", y = "RMSE") +
  coord_cartesian(ylim = c(0.6,1.15)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
 


############################################################

# Test with XGBoost 
#Convert from int to num for XGBoost
training_dataset <- training_dataset %>%
  dplyr::mutate(user_index = as.numeric(as.factor(userId)),
                movie_index = as.numeric(as.factor(movieId)))
final_holdout_test <- final_holdout_test %>%
  dplyr::mutate(user_index = as.numeric(as.factor(userId)),
    movie_index = as.numeric(as.factor(movieId)))

# Features: user_index and movie_index
train_matrix <- as.matrix(training_dataset %>% select(user_index, movie_index))
# Labels: adjusted_rating
train_labels <- training_dataset$adjusted_rating

# Define XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # Regression with squared loss
  eta = 0.1,                       # Learning rate
  max_depth = 6,                   # Maximum depth of a tree
  subsample = 0.8,                 # Subsample ratio of the training instances
  colsample_bytree = 0.8           # Subsample ratio of columns when constructing each tree
)

# Train the model
xgb_model <- xgboost(
  data = train_matrix,
  label = train_labels,
  params = params,
  nrounds = 400,                   # Number of boosting rounds
  verbose = 0                      # Print training log
)

#Apply the same encoding to the testing data. Ensure that the factor levels match those in the training data.
test_matrix <- as.matrix(final_holdout_test %>% select(user_index, movie_index))

XGB_predicted_ratings <- predict(xgb_model, test_matrix)

final_holdout_test <- final_holdout_test %>%
  mutate(XGB_predicted_rating = XGB_predicted_ratings + User_Bias + Movie_Bias + Genre_Bias)

# Actual ratings
XGB_actual_ratings <- final_holdout_test$rating

# Predicted ratings
XGB_predicted_ratings <- final_holdout_test$XGB_predicted_rating
 
RMSE_XGB_HoldOut <- RMSE(XGB_actual_ratings, XGB_predicted_ratings)

RMSE_Table <- tibble(Model = c("Mean Model", "Single Value (3.4) Model", "Movie Bias Model", "Movie+User Bias Model", "Movie+User+Genre Bias Model", "Factorization Model", "Factorization Model Holdout", "XGBoost Model Holdout"), RMSE = format(c(RMSE_Naive, RMSE_3.4, RMSE_Movie_Bias, RMSE_User_Bias, RMSE_Genre_Bias, RMSE_Factorization, RMSE_Factor_HoldOut, RMSE_XGB_HoldOut), nsmall = 4))
pander(RMSE_Table)

# Reorder RMSE values and plot. 
RMSE_Table <- RMSE_Table %>%
  dplyr::mutate(RMSE = as.numeric(RMSE))
RMSE_Table <- RMSE_Table %>%
  dplyr::mutate(Model = factor(Model, levels = Model[order(-RMSE)]))

ggplot(RMSE_Table, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.865, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = round(RMSE, 4)), vjust = -0.5) +  # Add RMSE values above bars
  labs(title = "RMSE of Different Models", x = "Model", y = "RMSE") +
  coord_cartesian(ylim = c(0.6,1.15)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



# As a spatial analyst, I wanted to see which country names appeared in movie titles as well. Here, I join tables to map countries based on country names found in movie title.

#Read in country list
data(World, package = "tmap")
country_names <- World %>% pull(name)
country_names <- country_names[country_names != "Georgia"]
country_pattern <- paste(country_names, collapse = "|")

# Filter movie titles containing country names
Movie_Titles_Map <- tibble(title = unique(MovieLens_Dataset$title))
titles_with_countries <- Movie_Titles_Map %>%
  filter(str_detect(title, regex(country_pattern, ignore_case = TRUE)))

# Initialize a vector to store matching countries
matching_countries <- c()

# Iterate over each country name
for (country in country_names) {
  # Check if the country name is present in any of the filtered titles
  if (any(str_detect(titles_with_countries$title, regex(country, ignore_case = TRUE)))) {
    matching_countries <- c(matching_countries, country)
  }
}

# Remove duplicates
matching_countries <- unique(matching_countries)
world_map_data <- World %>%
  filter(name %in% matching_countries)

# Set tmap mode to plotting
suppressMessages(tmap_mode("plot"))

# Create the map with the title at the bottom center
tm_shape(World) +
  tm_polygons(col = "darkgrey", border.col = "white") +  # Basemap with other countries
  tm_shape(world_map_data) +
  tm_polygons(col = "darkgreen", border.col = "lightblue") +  # Highlighted countries
  tm_text("name", size = 0.5, col = "white", remove.overlap = TRUE) +  # Country names
  tm_layout(frame = FALSE) +
  tmap_options(bg.color = "black")
