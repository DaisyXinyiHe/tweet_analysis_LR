library('dplyr')
library('lubridate')
library('tidytext')
library('stringr')

# 2. 
trump_data = readr::read_tsv('./data/trump_data.tsv', col_names = FALSE)
tweets = trump_data  %>% 
  rename(
    author = X1,
    date_and_time= X2,
    text  = X3
  )

tweets = tweets%>%mutate(
text = tolower(text),
x_1 = lubridate::hour(date_and_time),
x_2 = stringr::str_count(text, "a"),
x_3= stringr::str_length(text)
)

tweets = subset(tweets, select = -c(date_and_time) )

#3. 
set.seed(2048)
training_tweets = tweets[1:(0.8*nrow(tweets)),]
testing_tweets = setdiff(tweets,training_tweets)

#4. 
train_factor = as.factor(training_tweets$author)
pred_train = class::knn(training_tweets[,c(3,4,5)], training_tweets[,c(3,4,5)], cl = train_factor ,k = 5, use.all = TRUE)
test_factor = as.factor(testing_tweets$author)
pred_test = class::knn(testing_tweets[,c(3,4,5)], testing_tweets[,c(3,4,5)], cl = test_factor ,k = 5, use.all = TRUE)
train_acc = mean(train_factor==pred_train)
test_acc = mean(test_factor==pred_test)

######
#1. 
word_author_table <- training_tweets %>%
  dplyr::select(author, text) %>%
  tidytext::unnest_tokens(word, text)

#2.
prop_staff= word_author_table%>%group_by(word)%>%summarise(count_staff = sum(author=='Staff'),prop_staff = count_staff/n())%>% filter(count_staff >= 10) %>%arrange(desc(prop_staff)) 



#3. 
training_tweets_aug = training_tweets
training_tweets_aug[,6:25]= array(0)
names(training_tweets_aug) = c(names(training_tweets_aug[1:5]), paste0("training_tweet_", prop_staff$word[1:20]))
for (w in 1:length(prop_staff$word[1:20])){
  word_index = grep(prop_staff$word[w], training_tweets_aug$text, ignore.case = TRUE)
  training_tweets_aug[word_index,w+5] = 1
  
}
testing_tweets_aug = testing_tweets
testing_tweets_aug[,6:25]= array(0)
names(testing_tweets_aug) = c(names(testing_tweets_aug[1:5]), paste0("testing_tweet_", prop_staff$word[1:20]))
for (w in 1:length(prop_staff$word[1:20])){
  word_index = grep(prop_staff$word[w], testing_tweets_aug$text, ignore.case = TRUE)
  testing_tweets_aug[word_index,w+5] = 1
  
}
#4. 
train_aug_factor = as.factor(training_tweets_aug$author)
pred_train_aug = class::knn(training_tweets_aug[,3:25], training_tweets_aug[,3:25], cl = train_aug_factor ,k = 5, use.all = TRUE)
test_aug_factor = as.factor(testing_tweets_aug$author)
pred_test_aug = class::knn(testing_tweets_aug[,3:25], testing_tweets_aug[,3:25], cl = test_aug_factor ,k = 5, use.all = TRUE)
train_aug_acc = mean(train_aug_factor==pred_train_aug)
test_aug_acc = mean(test_aug_factor==pred_test_aug)

#5
training_tweets_aug$x_1_stan = (training_tweets_aug$x_1 -  mean(training_tweets_aug$x_1))/sd(training_tweets_aug$x_1)
training_tweets_aug$x_2_stan = (training_tweets_aug$x_2 -  mean(training_tweets_aug$x_2))/sd(training_tweets_aug$x_2)
training_tweets_aug$x_3_stan = (training_tweets_aug$x_3 -  mean(training_tweets_aug$x_3))/sd(training_tweets_aug$x_3)
testing_tweets_aug$x_1_stan = (testing_tweets_aug$x_1 -  mean(testing_tweets_aug$x_1))/sd(testing_tweets_aug$x_1)
testing_tweets_aug$x_2_stan = (testing_tweets_aug$x_2 -  mean(testing_tweets_aug$x_2))/sd(testing_tweets_aug$x_2)
testing_tweets_aug$x_3_stan = (testing_tweets_aug$x_3 -  mean(testing_tweets_aug$x_3))/sd(testing_tweets_aug$x_3)

pred_train_aug_stan = class::knn(training_tweets_aug[,6:28], training_tweets_aug[,6:28], cl = train_aug_factor ,k = 5, use.all = TRUE)
pred_test_aug_stan = class::knn(testing_tweets_aug[,6:28], testing_tweets_aug[,6:28], cl = test_aug_factor ,k = 5, use.all = TRUE)
train_aug_acc_stan = mean(train_aug_factor==pred_train_aug_stan)
test_aug_acc_stan = mean(test_aug_factor==pred_test_aug_stan)



