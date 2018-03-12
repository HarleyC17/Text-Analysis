library(ggplot2)
library(dplyr)


tweetdata<- read.csv("tweets.csv", header = T)
str(tweetdata)


################## Bar Chart ##################

# a data frame to plot overall tweets with the sentiment types by airlines
tweetSentiment<- as.data.frame(table(tweetdata$airline_sentiment, tweetdata$airline))
colnames(tweetSentiment)<- c("Sentiments","Airline", "Count")

# stacked bar
ggplot(tweetSentiment, aes(x= Airline, y= Count, fill=Sentiments)) + 
  geom_bar(stat = "identity", position = position_stack(reverse = T)) +
  geom_text(aes(label= Count, group= Airline), position = position_stack(reverse = T, vjust = 0.5)) + 
  ggtitle("Proportion of Sentiment per Airline") +
  theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5))



################# Pie Charts ###################

####### a data frame for the percent of tweets per airline
tweetsPercent<- as.data.frame(round(prop.table(table(tweetdata$airline)), digits = 2))
colnames(tweetsPercent)<- c("Airline", "Count")

ggplot(tweetsPercent, aes(x="", y=Count, fill= Airline)) +
  geom_bar(stat = "identity") + coord_polar("y") +
  geom_text(aes(label=Count),position = position_stack(vjust = 0.5), size=5)



####### a breakdown of the sentiment with each airline
# generate dataset with PROPORTION of the sentiment by airlines
propSentiment = as.data.frame(prop.table(table(tweetdata$airline_sentiment, tweetdata$airline), margin = 2))
colnames(propSentiment)<- c("Sentiments","Airline", "Count")

# American Airlines
AA<- subset(propSentiment, Airline== "American") 

ggplot(AA, aes(x="", y=Count, fill= Sentiments)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label=round(Count, digits = 3)), position = position_stack(vjust = 0.5), size=6) +
  ggtitle("American Airline") + theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5))


# Delta
Delta<- subset(propSentiment, Airline== "Delta")
  
ggplot(Delta, aes(x="", y=Count, fill= Sentiments)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label=round(Count, digits = 3)), position = position_stack(vjust = 0.5), size=6) +
  ggtitle("Delta") + theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5))


# Southwest
Southwest<- subset(propSentiment, Airline== "Southwest")

ggplot(Southwest, aes(x="", y=Count, fill= Sentiments)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label=round(Count, digits = 3)), position = position_stack(vjust = 0.5), size=6) +
  ggtitle("Southwest") + theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5))


# United Airlines
UA<- subset(propSentiment, Airline== "United")

ggplot(UA, aes(x="", y=Count, fill= Sentiments)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label=round(Count, digits = 3)), position = position_stack(vjust = 0.5), size=6) +
  ggtitle("United Airlines") + theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5))


# US Airways
USAirways<- subset(propSentiment, Airline== "US Airways")

ggplot(USAirways, aes(x="", y=Count, fill= Sentiments)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label=round(Count, digits = 3)), position = position_stack(vjust = 0.5), size=6) +
  ggtitle("US Airways") + theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5))


# Virgin America
VA<- subset(propSentiment, Airline== "Virgin America")

ggplot(VA, aes(x="", y=Count, fill= Sentiments)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label=round(Count, digits = 3)), position = position_stack(vjust = 0.5), size=6) +
  ggtitle("Virgin America") + theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5))


### Summary: Airlines with the least proportion of negative comments are Virgin America (36%)
#            Delta(43%), and Southwest(49%). But the total tweets of Virgin America are only
#            accountable for 3% of overall tweets.
#            Most of the tweets are negative when it comes to US Ariways (78%), 
#            American Airlines(71%), and United Airlines(69%).  It really rises red flags to them.