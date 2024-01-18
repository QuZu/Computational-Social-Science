
library(tidyverse)
library(gutenbergr)
library(tidytext)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(topicmodels)

# Using the download.file() function we can download a file from the internet: 
romeo <- gutenberg_works(title == 'Romeo and Juliet') %>%
  gutenberg_download(meta_fields = 'title')

# We can see some of the content of the first lines of the book 
head(romeo)
# Packages used in this step: tidytext

# Tokenization:
words <- romeo %>% unnest_tokens(word,text) # we send the dataframe "romeo" into a pipeline %>% to the function unnest_tokens
head(words)

romeo_corpus <- Corpus(VectorSource(words))
# Packages used in this step: wordcloud = a word-cloud generator; RColorBrewer = for color palettes

# Preparations for the word-cloud 

# 1. Turn romeo_corpus into a mathematical matrix describing the frequency of terms occurring in it:
dtm <- TermDocumentMatrix(romeo_corpus) # transforms our word collection into a list, describing the frequency of terms
m <- as.matrix(dtm) # turns it into a matrix

# 2. Inspect m 
print(m) 

# 3. Sort the terms in m in decreasing order: 
v <- sort(rowSums(m),decreasing=TRUE)
print(v)

# 4. Convert v into a dataframe 
d <- data.frame(word = names(v),freq=v)
# Have a look at d by clicking on the object in the global environement 

# Wordcloud
set.seed(1234) # create reproducible results when writing code that involves creating objects that take on random values
wordcloud(words = d$word, # use the column "words" stored in object "d" as the words in the wordcloud
          freq = d$freq, # use the frequency count from the object "d" as the count for word frequency
          min.freq = 1, # only include words that occur at least one time
          max.words=200, # only inlcude a maximum of 200 words in total in the wordcloud
          random.order=FALSE, # plot words according to their frequency (i.e. not random)
          rot.per=0.35, # percentage of words rotated 90 degrees in the wordcloud
          colors=brewer.pal(8, "Dark2")) # specify the colors we want to use (as stored in RColorBrewer)
# Ignore the warning messages
# Convert the text to lower case in order to avoid doubles
romeo_stem <- tm_map(romeo_corpus, content_transformer(tolower))
# Remove numbers
romeo_stem <- tm_map(romeo_stem, removeNumbers)
# Remove common stopwords
romeo_stem <- tm_map(romeo_stem, removeWords, stopwords("english"))
# Remove punctuation
romeo_stem <- tm_map(romeo_stem, removePunctuation)
# Eliminate extra white spaces
romeo_stem <- tm_map(romeo_stem, stripWhitespace)

dtm <- TermDocumentMatrix(romeo_stem)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

# Wordcloud 2
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
romeo_stem2 <- tm_map(romeo_stem, removeWords, c("juliet","romeo")) #remove the words in the vector c( )

# Wordcloud 3
# Note that we are overriding the objects we created earlier. 
dtm <- TermDocumentMatrix(romeo_stem2)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))  

gutenberg_works(author == "Aristotle")
aristotle <- gutenberg_download(c(1974, 2412, 6762, 6763, 8438))
head(aristotle)

#tokenize 
aristotle_words <- aristotle %>% unnest_tokens(word,text) 
#make corpus
aristotle_corpus <- Corpus(VectorSource(aristotle_words))

#Data cleaning:
# Convert the text to lower case
aristotle_stem <- tm_map(aristotle_corpus, content_transformer(tolower))
# Remove numbers
aristotle_stem <- tm_map(aristotle_stem, removeNumbers)
# Remove english common stopwords
aristotle_stem <- tm_map(aristotle_stem, removeWords, stopwords("english"))
# Remove punctuations
aristotle_stem <- tm_map(aristotle_stem, removePunctuation)
# Eliminate extra white spaces
aristotle_stem <- tm_map(aristotle_stem, stripWhitespace)

# Make wordcloud
# Note that we overwrite the former objects. 
dtm <- TermDocumentMatrix(aristotle_stem)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(height=head(d,10)$freq, names.arg=head(d,10)$word, # plot the frequency of the top 10 words in the data frame "d" using the function head selecting the head of the df + label the bars accordingly with the top 10 words using the names.arg argument
        space = 0.4, # space between the bars, edit this number if your plot is too narrow to see it correctly
        xlab="words", # label x-axis
        ylab="apparitions", # label y-axis
        col="#973232", #pick a colour
        main="Aristotle") # title of the graph

# Packages used in this step: SnowballC

aristotle_books <- aristotle %>% unnest_tokens(word,text, token = "regex", pattern="\\s+|[[:punct:]]+") %>% #already remove punctuation
  mutate(word = str_to_lower(word)) %>% # change to lower case
  mutate(stem = wordStem(word)) %>%   # stems words
  filter(!grepl('[0-9]', word)) %>%   # remove numbers
  add_count(gutenberg_id,name = "total_words") %>% # add a variable that counts the ttal words in each of the books 
  group_by(gutenberg_id, total_words) %>% 
  count(word, sort = TRUE) %>% # sorts by word count (most common word on top)
  ungroup() 

head(aristotle_books)

aristotle_books <- aristotle_books %>% # we make changes to our df aristotle_books by sending it into a pipe
  select(-total_words) %>% 
  bind_tf_idf(term = word, document = gutenberg_id, n = n) #

# If you want to learn more about the bind_tf_idf function call
?bind_tf_idf

head(aristotle_books)

# Function for a well-organised graph 
facet_bar <- function(df, y, x, by, nrow = 2, ncol = 2, scales = "free") {
  mapping <- aes(y = reorder_within({{ y }}, {{ x }}, {{ by }}), 
                 x = {{ x }}, 
                 fill = {{ by }})
  
  facet <- facet_wrap(vars({{ by }}), 
                      nrow = nrow, 
                      ncol = ncol,
                      scales = scales) 
  
  ggplot(df, mapping = mapping) + 
    geom_col(show.legend = FALSE) + 
    scale_y_reordered() + 
    facet + 
    ylab("")
}

#Plot the facet bar
aristotle_books %>% # send the df into a pipe 
  group_by(gutenberg_id) %>% # group by the data for each book 
  top_n(15) %>%# select the top 15 words 
  ungroup() %>% # ungroup 
  facet_bar(y = word, # use the graphing function created earlier, specify the y-axis of your graphs = the word
            x = tf_idf, # specify the x-axis = the frequency of the word
            by = gutenberg_id, # group so that you get one plot per book 
            nrow = 3) 

# Removes the stopwords
aristotle_books_no_sw <- anti_join(aristotle_books,stop_words)

# Plot the facet bars
aristotle_books_no_sw %>% 
  group_by(gutenberg_id) %>% 
  top_n(15) %>%
  ungroup() %>%
  facet_bar(y = word, 
            x = tf_idf, 
            by = gutenberg_id, 
            nrow = 3)
#PART2:
tweets  <- readRDS(gzcon(url("https://github.com/cjbarrie/CTA-ED/blob/main/data/sentanalysis/newstweets.rds?raw=true")))
head(tweets)
colnames(tweets)

tweets <- tweets %>%
  select(user_username, text, user_name) %>% # selecting the 3 variables: user_username, text, user_name
  rename(username = user_username, # rename the variables (newname = oldname)
         newspaper = user_name,
         tweet = text)

head(tweets)

tidy_tweets <- tweets %>% 
  mutate(desc = tolower(tweet)) %>% # change all letters to lower case 
  unnest_tokens(word, desc, token = "regex", pattern="\\s+|[[:punct:]]+|http.+ |http.+$") %>% # remove punctuation + tokensize
  mutate(stem = wordStem(word)) %>%   # stems words
  filter(str_detect(word, "[a-z]")) # select only tokens (words) starting with a letter a-z, aka get rig of emojis etc.

head(tidy_tweets)

tidy_tweets <- anti_join(tidy_tweets,stop_words)
head(tidy_tweets)

sentiment <- get_sentiments(lexicon = "bing")
head(sentiment)

# Recode the categories from positive/negative" to 1/-1
sentiment <- sentiment %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))

head(sentiment)

tweets_sent <- inner_join(x = tidy_tweets, y = sentiment) # merge the two dataframes
head(tweets_sent)

tweets_sent %>% group_by(newspaper, tweet) %>% # group by tweet and newspaper 
  summarise(sentiment = sum(valor)) %>% # add the values of the tokens in each tweet together
  head() # look at the head

sentiment_percentage <- tweets_sent %>% group_by(newspaper, tweet) %>%
  summarise(sentiment = sum(valor)) %>%
  group_by(newspaper) %>%
  summarise(pos = 100 * sum(sentiment > 0) / n(),
            neutral = 100 * sum(sentiment == 0) / n(),
            neg = 100 * sum(sentiment  < 0) / n())

head(sentiment_percentage)

sentiment_percentage %>% ungroup() %>%
  gather(key = "sentiment", value = "valor", -newspaper) %>%
  ggplot(aes(x = newspaper, y = valor, fill = sentiment)) + 
  geom_col(position = "dodge", color = "black") + coord_flip() +
  theme_bw()

tidy_tweets <- tidy_tweets %>% dplyr::count(tweet, word)

dtm_tweets <- tidy_tweets %>% cast_dtm("tweet","word","n")
dtm_tweets

system2('sudo', 'apt-get install libgsl0-dev')
install.packages('topicmodels', repos='http://cran.rstudio.com/')
library(topicmodels)

# Note: The calculation will take a while

# LDA model of 5 topics:
lda_5 <- LDA(dtm_tweets, # input object
             k = 5, # k specifies the number of topics we want to extract
             method = 'Gibbs', # method used for fitting
             control = list(seed = 1234)) # set a seed so that the output of the model reproducible 

#Top 10 terms or words under each topic
top10terms_5 <- as.matrix(terms(lda_5,10))
top10terms_5

#number of topics found out by our model:
lda.topics_5 <- as.matrix(topics(lda_5))

summary(as.factor(lda.topics_5[,1]))

#We can also get document wise probability of each topic
topicprob_5 <- as.matrix(lda_5@gamma)

head(topicprob_5,1)
