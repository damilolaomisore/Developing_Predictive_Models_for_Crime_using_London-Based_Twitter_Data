# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
Crime_data <- read.csv(file.choose())

en_data <- model.matrix(~ Offence.Group -1, data = Crime_data)

Crime_data2 <- cbind(Crime_data, en_data)
library(csv)
write.csv(Crime_data2, "CrimeData.csv")

# load libraries
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)
library(caret)

#Note: Replace below with your credentials following above reference/
api_key <- "DAOQqHxAWvxf44mjnmFgyQUQy"
api_secret <- "sbA5GfkygkfMrS6QT71qln2CjNbHH2trAy7zAHe1FgR4I1iOMn"
access_token <- "1113956173525528578-tAbEpTxbCjRPuh4rsNFHcw7xCqDctG"
access_token_secret <- "c5wnIVZAk79X1vnP2UFz5t4Bb0Ac6go4vQ0MB73FVsNfh"
#Note: This will ask us permission for direct authentication, type '1' for yes:
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# extracting tweets related to crime topic in London
hashtags <- c("#LondonCrime", "#CrimeLondon", "#LondonSafety", "#SafeLondon", "#LondonViolence", "#CrimeWaveLondon",
              "#LondonPolice", "#LondonSecurity", "#LondonLawEnforcement", "#LondonCommunitySafety", "#LondonJustice",
              "#LondonCrimes", "#LondonSafetyAlert", "#LondonCrimeStats", "#LondonCrimePrevention", "#Crime + #London",
              "#truecrime + #London", "#detective + #London", "#knife + #London", "#knifecrime + #London", "#knifefree + #London", 
              "#endknifecrime + #London", "#Arson + #London", "#Drug + #London", "#Burglary + #London", "#Assault + #London", 
              "#homicide + #London", "#endknifecrime", "#putdowntheknives + #London", "#putdowntheknives", 
              "MPSBarkDag", "MPSBarnet", "MPSBexley", "MPSBrent", "MPSBromley", "MPSCamden", "MPSCroydon", "ealingMPS",
              "MPSEnfield", "MPSGreenwich", "MPSHackneyCentr", "MPSHammFul", "MPSHaringey", "CrimeLdn")
all_tweets <- list()
for (hashtags in hashtags) {
  tweets <- searchTwitter(hashtags, n=40000, lang="en", since = "2023-04-01", until = "2023-05-31", retryOnRateLimit = 12000)
  all_tweets <- c(all_tweets, tweets)
}

n.tweet <- length(all_tweets)

# convert tweets to a data frame
tweets.df <- twListToDF(all_tweets)
write.csv(tweets.df, file="twitter2.csv")

tweets.txt <- sapply(tweets, function(t)t$getText())
# Ignore graphical Parameters to avoid input errors
tweets.txt <- str_replace_all(tweets.txt,"[^[:graph:]]", " ")

## pre-processing text:
clean.text = function(x)
{
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

cleanText <- clean.text(tweets.txt)
# remove empty results (if any)
idx <- which(cleanText == " ")
cleanText <- cleanText[cleanText != " "]

tweets.df %<>% 
  mutate(
    created = created %>% 
      # Remove zeros.
      str_remove_all(pattern = '\\+0000') %>%
      # Parse date.
      parse_date_time(orders = '%y-%m-%d %H%M%S')
  )

tweets.df %<>% 
  mutate(Created_At_Round = created%>% round(units = 'hours') %>% as.POSIXct())

tweets.df %>% pull(created) %>% min()
tweets.df %>% pull(created) %>% max()

tweets.df <- read.csv(file.choose())
# plot number of tweets per hour
plt <- tweets.df %>% 
  dplyr::count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Date') +
  ylab(label = NULL) +
  ggtitle(label = 'Number of Tweets per Hour')

plt %>% ggplotly()

#-----2. Data Exploration-------------------------------------------
# read in data from csv file
Crime_data <- read.csv(file.choose())
Crime_data[11:17]
summary(Crime_data)
attach(Crime_data)
detach(Crime_data3)
attach(Crime_data2)

# check for missing data
apply(Crime_data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(Crime_data, y.labels =Area_name, col = c("black", "pink"), legend = TRUE)

# remove duplicates
Crime_data <- unique(Crime_data)

# Significance testing for normality
ks.test(Crime_Count,"pnorm", mean(Crime_Count), sd(Crime_Count))

Crime_data2 <- Crime_data[11:17]
sd(Population)
# boxplot for Crime_Count
boxplot(Crime_data2[1], xlab="Crime_Count", ylab="Count", col = "pink", ylim = c(0,80))

# boxplot for tweets_count
boxplot(Crime_data2[2], xlab="Tweets_count", ylab="Count", col = "pink", ylim = c(0,8))


#histogram for Crime_Count
hist(Crime_Count,  col = "pink", border = "dark green", freq = F,
     xlab = "Crime_Count", main = "Histogram")
rug (Crime_Count)
lines (density(sort(Crime_Count)))
xfit <- seq(from = min(Crime_Count), to = max(Crime_Count), by = 0.1)
yfit = dnorm(xfit, mean(Crime_Count), sd(Crime_Count))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

#histogram for Tweets_count
hist(Tweets_count,  col = "pink", border = "dark green", freq = F,
     xlab = "Tweets_count", main = "Histogram")
rug (Tweets_count)
lines (density(sort(Tweets_count)))
xfit <- seq(from = min(Tweets_count), to = max(Tweets_count), by = 0.1)
yfit = dnorm(xfit, mean(Tweets_count), sd(Tweets_count))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# basic multivariate scatterplot matrix
pairs(~ Crime_Count + Tweets_count + Retweets_count + Likes_count + Population,  data = Crime_data2,
      main = "multivariate scatterplot matrix")

anova(Crime_Count, Tweets_count, test = "F")

# correlation of all the variables
# correlation matrix
corr_test2 <- cor(Crime_data[11:17], method = "spearman")
corr_test2 <- round(corr_test, digits = 2)

# correlation plot
install.packages("rlang")
update.packages("rlang")
library(ggplot2)
library(GGally)
GGally::ggpairs(Crime_data2)
ggpairs(Crime_data2, columns = 1:6,
        ggplot2::aes(colour="blue"),
        upper = list(continuous = "cor"),
        title = "Correlation plot")


 library(corrplot)
corrplot(corr_test)



# make Crime_level a factor
Crime_data2$Crime_level <- factor(Crime_data2$Crime_level)
str(Crime_data2)

# confirm data is balanced (For imbanced, take not of Accuracy & Sensitivity)
table(Crime_data2$Crime_level)

# data partitioning
set.seed(246)
part <- sample(2, nrow(Crime_data2), replace = T, prob = c(0.7, 0.3))

train_data <- Crime_data2[part==1,]
test_data <- Crime_data2[part==2,]

#OVERSAMPLING
library(ROSE)
oversampled_data <- ovun.sample(Crime_level ~ ., data = Crime_data2, method = "over")
table(oversampled_data[["data"]]$Crime_level)
oversampled_data <- oversampled_data[["data"]]
oversampled_data$Crime_level <- factor(oversampled_data$Crime_level)

part2 <- sample(2, nrow(oversampled_data), replace = T, prob = c(0.7, 0.3))

train_data3 <- oversampled_data[part==1,]
test_data3 <- oversampled_data[part==2,]
str(oversampled_data)


#-----6. K NEAREST NEIGHBOUR-------------------------------------------
install.packages("class")
library(class)

k_values <- data.frame(k = seq(1, 20, by = 2))

attach(Crime_data3)
attach(oversampled_data)
detach(oversampled_data)
Crime_data3 <- cbind(scale(cbind(Crime_Count, Tweets_count, Retweets_count, Likes_count, Population, Crime_per_TP)),Crime_data2[7])
Crime_data4 <- cbind(scale(cbind(Crime_Count, Tweets_count, Retweets_count, Likes_count, Population, Crime_per_TP)),oversampled_data[7])
#Crime_data3 <- merge(Crime_data3, Crime_data2[7])   
#rm(Crime_data3)

# data partitioning
set.seed(246)
part3 <- sample(2, nrow(Crime_data4), replace = T, prob = c(0.7, 0.3))

train_data2 <- Crime_data4[part==1,]
test_data2 <- Crime_data4[part==2,]

# Create a KNN model using cross-validation and grid search
knn_model1 <- train(Crime_level ~ ., data = train_data2, method = "knn", 
                    trControl = trainControl(method = "cv", number = 10),
                    tuneGrid = k_values)
knn_model1$results

# Plot the accuracy as a function of K
ggplot(data = knn_model1$results, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "K", y = "Accuracy") +
  ggtitle("KNN Model 1")

# Predict the test set using the trained model
knn_pred1 <- predict(knn_model1, test_data2)
knn_pred1

conf_matrix4 <- confusionMatrix(knn_pred1, test_data2$Crime_level, positive = "0")
conf_matrix4

knn_f1_1 <- conf_matrix4$byClass["F1"]
# test for multicollinearity
library(car)
vif(log_model1)
sqrt(vif(log_model1)) > 2 


#-----7. NAIVE BAYES-------------------------------------------
install.packages("naivebayes")
install.packages("e1071")
library(naivebayes)
library(caret)
library(e1071)

# Train the Naive Bayes model
nb_model1 <- train(Crime_level ~ ., data = train_data3, method = "naive_bayes", trControl = trainControl(method = "cv", number = 10), na.action = na.pass, tuneLength = 10)
nb_model1

# Predict the test set using the trained model
nb_pred1 <- predict(nb_model1, test_data3)
nb_pred1

# Confusion matrix
conf_matrix7 <- confusionMatrix(nb_pred1, test_data3$Crime_level, positive = "0")
conf_matrix7

nb_f1_1 <- conf_matrix7$byClass["F1"]



#-----5. RANDOM FOREST-------------------------------------------
install.packages("randomForest")
library(randomForest)

rf_model1 = randomForest(Crime_level ~., train_data3)
plot(rf_model1)
print(rf_model1)

rf_pred1 = predict(rf_model1, test_data3)
rf_pred1 = factor(ifelse(rf_pred1 > 0.5, 1, 0), levels = c(1, 0))
rf_pred1

conf_matrix6 <- confusionMatrix(rf_pred1, test_data3$Crime_level, positive = "0")
conf_matrix6

#-----8. SVM----------------------------------------------------
install.packages("e1071")
library(e1071)

# Assuming 'target' is your binary target variable
svm_model <- svm(Crime_level ~ ., data = train_data3, kernel = "radial", cost = 1)
summary(svm_model)

# Predict on test data
svm_pred <- predict(svm_model, test_data3)

conf_matrix9 <- confusionMatrix(svm_pred, test_data3$Crime_level, positive = "0")
conf_matrix9
