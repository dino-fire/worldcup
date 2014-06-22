
##############################################################################
## Run this script first
## Load packages, connect to Twitter API
##############################################################################

# Load packages for text mining applications
library(XML)
library(tm)
library(ggplot2)
library(sentiment)
library(plyr)
library(wordcloud)
library(RColorBrewer)
library(Rstem)
library(twitteR)
library(SnowballC)
library(corrplot)
library(igraph)
library(stringr)

##############################################################################
## Twitter API
##############################################################################

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

cred <- OAuthFactory$new(consumerKey='consumer_key',
                         consumerSecret='consumer_secret',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='http://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")
registerTwitterOAuth(cred)

##############################################################################
## Collect some tweets
##############################################################################

# Enter the search term
subject <- '#WorldCup OR #worldcup2014 OR #brazil'

# Search in Ohio
tweets <- searchTwitter(subject, n = 5000, geocode='39.961176,-82.998794,200mi', cainfo="cacert.pem")

# Save as a text document
tweets_txt <- sapply(tweets, function(x) x$getText())

##############################################################################

# Store it for future use if necessary, create an empty directory first
# I strongly suggest you do this because it saves you some pain if you need to replicate your work later
write(tweets_txt, './tweets/tweets.txt')

# Do one of the two imports below, but not both:

        # Restore tweets from a file if necessary, in text mining format 
        place <- DirSource('./tweets/') #input path for documents
        corpus <- Corpus(place, readerControl=list(reader=readPlain))

	# Or simply create a corpus
	corpus <- Corpus(VectorSource(tweets_txt))

# Clean up the tweets
badwordlist <- read.csv('badwordlist.csv', sep = ',', header = T) # previously created
commonwords <- c('World Cup', 'world cup', 'worldcup', 'WorldCup', 'Brazil', 'brazil', 'WorldCup2014', 
		'worldcup2014', 'http://', 'http')
shortwords <- c('the', 'you', 'are', 'and', 'been', 'but', 'can', 'cant', 'dont',  'from', 'had', 'has', 
		'its', 'did', 'etc', 'for', 'this', 'was', 'would', 'does', 'get', 'have', 'isnt', 'just', 
		'that', 'too', 'you', 'your', 'to', 'be', 'of', 'on', 'so', 'rt', 'll', 'RT')
		# or use stopwords('English') from the tm package, or both
		
corpus <- tm_map(corpus, removeWords, badwordlist)
corpus <- tm_map(corpus, removeWords, commonwords)
corpus <- tm_map(corpus, removeWords, shortwords)
corpus <- tm_map(corpus, removeWords, stopwords('English'))

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, stemDocument) 
corpus <- tm_map(corpus, PlainTextDocument)
inspect(corpus)

# Colored dendrogram: remove sparse terms to simplify the cluster plot
# Note: tweak the sparse parameter to determine the number of words.
# 20-30 words is good.

# Create Term Document Matrix
tweets_tdm <- TermDocumentMatrix(corpus)
tweets_tdm <- removeSparseTerms(tweets_tdm, sparse = 0.97)
nrow(tweets_tdm)

# Create Document Term Matrix
tweets_dtm <- DocumentTermMatrix(corpus)
