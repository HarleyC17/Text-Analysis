library(dplyr)
library(tm)
library(SnowballC)


tweetdata<- read.csv("tweets.csv", header = T)
str(tweetdata)
View(tweetdata)


sentiment<- select(tweetdata, airline_sentiment, negativereason, airline, text)
str(sentiment)
View(sentiment)
levels(sentiment$airline)

######### extract NEGATIVE tweets about American Airline ############
negativeText<- subset(sentiment, airline_sentiment== "negative"&airline== "American")
str(negativeText)
table(negativeText$airline_sentiment, negativeText$airline)


######### pre-processing ##########

# clean the data by removing unhelful words and puncuations
negcorpus<- Corpus(VectorSource(negativeText$text))
negcorpus[[8]]$content

negcorpus<- tm_map(negcorpus, tolower)
negcorpus<- tm_map(negcorpus, removePunctuation)
negcorpus<- tm_map(negcorpus, removeWords, c('americanair','thanks', stopwords('en')))
negcorpus<- tm_map(negcorpus, content_transformer(function(x) gsub('http[[:alnum:]]*', '', x))) # eliminate URLs
negcorpus = tm_map(negcorpus, stemDocument)
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
hc1<- hclust(d)
plot(hc1, hang = -1, main = "Negative Tweets", xlab = "")
rect.hclust(hc1, k=6, border = "red")

hc2<- hclust(d, method = "ward.D")
plot(hc2, hang = -1, main = "Negative Tweets", xlab = "")
rect.hclust(hc2, k=6, border = "red")


