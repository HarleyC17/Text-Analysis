library(dplyr)
library(tm)
library(SnowballC)


tweetdata<- read.csv("tweets.csv", header = T)
str(tweetdata)
View(tweetdata)

### select columns for the analysis
sentiment<- select(tweetdata, airline_sentiment, negativereason, airline, text)
str(sentiment)
View(sentiment)
levels(sentiment$airline)

# cross-tab breaks down the negative reasons by airlines
table(sentiment$negativereason, sentiment$airline)


######### extract NEGATIVE tweets about American Airline ############
negativeText<- subset(sentiment, airline_sentiment== "negative"&airline== "American")
str(negativeText)
table(negativeText$airline_sentiment, negativeText$airline)  # to check



######### pre-processing ##########

# clean the data by removing unhelful words and puncuations
negcorpus<- Corpus(VectorSource(negativeText$text))
negcorpus[[8]]$content

negcorpus<- tm_map(negcorpus, tolower)
negcorpus<- tm_map(negcorpus, removePunctuation)
negcorpus<- tm_map(negcorpus, removeWords, c('americanair','thanks', stopwords('en')))

# eliminate URLs
negcorpus<- tm_map(negcorpus, content_transformer(function(x) gsub('http[[:alnum:]]*', '', x)))

negcorpus<- tm_map(negcorpus, stemDocument)
negcorpus[[8]]$content

# create text matrix
negcorpus<- DocumentTermMatrix(negcorpus)
negcorpus

# remove sparsities
negcorpus<- removeSparseTerms(negcorpus, 0.95) # remove 5% of the sprasities in the matrix


######### clustering analysis #########
d<- dist(t(as.matrix(negcorpus)), method = "euclidean")
d

# compare two Hierarchical approaches ("complete" and "ward")
hc<- hclust(d, method = "ward.D")
plot(hc, hang = -1, main = "Negative Tweets", xlab = "")
rect.hclust(hc, k=6, border = "red")


# Summary: the major complaints relate to customer services and flight issues. When we break the 
#          the dendrogram further down, by the given words in clusters such as hour, wait, time,
#          hold, delay, etc., we can imply time is the most concern from customers.
