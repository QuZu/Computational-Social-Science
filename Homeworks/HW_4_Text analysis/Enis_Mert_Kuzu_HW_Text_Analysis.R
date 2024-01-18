library(tidyverse)
library(gutenbergr)
library(tidytext)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(topicmodels)

factchecks <- read.csv("factchecks.csv")
head(factchecks )

words<- factchecks %>% unnest_tokens(word,claim_reviewed)

fc_corpus <- Corpus(VectorSource(words$word))

fc_stem <- tm_map(fc_corpus, content_transformer(tolower))
fc_stem<-tm_map(fc_stem,removeNumbers)
fc_stem<-tm_map(fc_stem,removeWords,stopwords("english"))
fc_stem<-tm_map(fc_stem,removePunctuation)
fc_stem<-tm_map(fc_stem,stripWhitespace)
fc_stem <- tm_map(fc_stem, content_transformer(gsub), pattern = "http\\S+", replacement = "")

dtm <- TermDocumentMatrix(fc_stem) # transforms our word collection into a list, describing the frequency of terms
m <- as.matrix(dtm) 
v <- sort(rowSums(m),decreasing=TRUE)

d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

small_fact <- factchecks %>%
  select(author_name, claim_reviewed, review_rating)

head(small_fact)
tidy_fact <- small_fact %>% 
  mutate(desc = tolower(claim_reviewed)) %>% # change all letters to lower case 
  unnest_tokens(word, desc, token = "regex", pattern="\\s+|[[:punct:]]+|http.+ |http.+$") %>% # remove punctuation + tokensize
  mutate(stem = wordStem(word)) %>%   # stems words
  filter(str_detect(word, "[a-z]")) # select only tokens (words) starting with a letter a-z, aka get rig of emojis etc.

head(tidy_fact)
tidy_fact <- anti_join(tidy_fact,stop_words)

tidy_fact <- tidy_fact %>% dplyr::count(claim_reviewed, word)

dtm_facts <- tidy_fact %>% cast_dtm("claim_reviewed","word","n")
dtm_facts

library(topicmodels)
lda_non <- LDA(dtm_facts, # input object
             k = 10, # k specifies the number of topics we want to extract
             method = 'Gibbs', # method used for fitting
             control = list(seed = 1234)) # set a seed so that the output of the model reproducible 

#Top 10 terms or words under each topic
top10terms_5 <- as.matrix(terms(lda_non,10))
top10terms_5

#number of topics found out by our model:
lda.topics_non <- as.matrix(topics(lda_non))

summary(as.factor(lda.topics_non[,1]))
lda_non@alpha
lda_05 <- LDA(dtm_facts,
             k = 15, 
             method = 'Gibbs', 
             control = list(seed = 1234,alpha=0.5)) 
lda_05@alpha
top10terms_05 <- as.matrix(terms(lda_05,10))
top10terms_05

lda_03 <- LDA(dtm_facts, 
              k = 20, 
              method = 'Gibbs', 
              control = list(seed = 1234,alpha=0.3)) 
lda_03@alpha
top10terms_03 <- as.matrix(terms(lda_03,10))
top10terms_03
# Firstly, I wanted to experiment with 10 subjects as a general algorithm. 
# It was a bit far from being interpretable in the resulting topics,
# and its alpha value was also 5.Then, I tried with 15 topics and lower alpha 
# value At this point, more topics started to become clear, for example,
# I could clearly see that topic 12 was more about elections so I could see 
# democrats and republicans words in the topic. I could see that Topic 6 can be
# summed up with words related to Trump and Obama's election campaigns.


# Then, when I designed an algorithm with 20 topics and a smaller alpha value,
# I saw that I got even more reasonable and important topics. 
# For example, I thought that 2 topics might be related to the money lost
# in the trade between America and China. Apart from that,I saw that the Topic 13
# was gathered about health systems and the Obamacare law. I was able to see new 
# topics about immigration and marijuana becoming legal in California.

# As a result, I can say that I have obtained more comprehensive and 
# open interpretation results with higher topic numbers and lower alpha values.
# I can say that the most important step in ensuring model quality is the 
# preprocessing process while keeping the topic number wide enough, 
# because thanks to preprocessing, we get rid of unnecessary short and meaningless
# words and design a better model.