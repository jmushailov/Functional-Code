rm(list=ls())

library(readxl)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(tm)
library(wordcloud)
library(rvest)
library(XML)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RWeka)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scraping (If Necessary) ~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
url_to_pull <- 'https://www.website.com'
scrape <- read_html(url)

x <- scrape %>%
  html_nodes('.review-body') %>%
  html_text()

# For looping thro pages... start with base page prior to concat numbers
for(i in 151:197){
  url <- paste0(url_to_pull,"page/", i)
  scrape <- read_html(url)
  
  y <- scrape %>%
    html_nodes('.review-body') %>%
    html_text()
  
  x <- c(x,y)
}
# For looping thro pages




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Cleaning ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Using Regex  to clean --> DANGEROUS!
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#Second Approach using strip_html function
strip_html <- function(s) {
  html_text(read_html(s))
}

x <- cleanFun(x)
x <- as.data.frame(x)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create Corpus for analysis
x <- Corpus(VectorSource(dat))

#~~~~~~~~~~~~~~~~~~~~~~~~ Data Cleaning
x <- tm_map(x, removeWords, stopwords("english"))
x <- tm_map(x, tolower)
x <- tm_map(x, stemDocument) #Stemming may not be necessary. Truncating words can butcher meaning
x <- tm_map(x, removeNumbers)
x <- tm_map(x, removePunctuation)
x <- tm_map(x, stripWhitespace)

# Or use this function:
cleaner <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  return(corpus)
}

#~~~~~~~~~~~~~~~~~~~~~~~~ Wordcloud
dtm <- TermDocumentMatrix(x)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 20)

wordcloud(word = d$word, freq = d$freq, min.freq = 1, 
          max.words = 200, random.order = FALSE, rot.per= 0.35,
          colors = brewer.pal(8, "Dark2"))
          
          
#~~~~~~~~~~~~~~~~~~~~~~~~ Word Correlation          
findFreqTerms(dtm, lowfreq = 4)
w_corr <- findAssocs(dtm, terms = "WORD", corlimit = 0.5)

# ~~~~~~~~~~~~~~~~~~~~~~~ Plot the correlations
toi <- "" # term of interest
corlimit <- 0.1 # lower correlation bound limit.
graph <- data.frame(corr = findAssocs(dtm, toi, corlimit)[[1]],
                    terms = names(findAssocs(dtm, toi, corlimit)[[1]]))
graph$terms <- factor(graph$terms ,levels = graph$terms)

require(ggplot2)
ggplot(graph, aes( y = terms  ) ) +
  geom_point(aes(x = corr), data = graph, colour = 'blue') +
  xlab(paste0("Correlation with the term ", "\"", toi, "\""))
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~ Top 10 words
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
        
        
# ~~~~~~~~~~~~~~~~~~~~~~~ Sentiment Analysis using MSFT lexicon
tokens <- as.data.frame(d$word)
colnames(tokens) <- "word"

wordSentiment <- 
  tokens %>%
  inner_join(get_sentiments("bing"))

sentiments <-
  tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative)
# # of positive words - # of negative owrds


print(paste0(sentiments[1], " Negative words, ", sentiments[2],
             " Positive words, ", sentiments[3], " Neutral words. Positive-to-negative ratio: ", sentiments[2] / sentiments[1] ))


# ~~~~~~~~~~~~~~~~~~~~~~~ Ngram Analysis
library("quanteda")
dat <- data.frame(dat, stringsAsFactors = F)
c <- as.vector(dat$toanalyze)

# Change "n=" to whatever ngram you want to see
myDfm <- tokens(c) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_ngrams(n =3) %>%
  dfm()


featnames(myDfm)
top.features <- topfeatures(myDfm, n = 25)
topFeat <- data.frame(top.features)

# Plotting. First sum up all bigrams
big.freq <- colSums(myDfm)
big.freq <- sort(big.freq, decreasing = TRUE)

# Show the top features
topFeat

# Make it into an analyzable dataframe
topFeat$feature <- rownames(topFeat)
rownames(topFeat) <- NULL

# Convert the list to character vector
topFeat$feature <- as.character(topFeat$feature)

# Make it pretty!
topFeat$feature <- lapply(topFeat$feature, function(x) gsub("_"," ", x))

