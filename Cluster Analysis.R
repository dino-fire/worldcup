
##############################################################################
## Cluster Analysis on Tweets
##############################################################################

##############################################################################
## Collect some tweets
##############################################################################

# Do one of the two imports below, but not both:

        # Restore tweets from a file if you previously saved them there
        place <- DirSource('./tweets/') #input path for documents
        corpus <- Corpus(place, readerControl=list(reader=readPlain))

	# Or simply create a corpus from your current session
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

# convert the sparse term-document matrix to a standard data frame
hca.df <- as.data.frame(inspect(tweets_tdm))

# inspect dimensions of the data frame
nrow(hca.df)
ncol(hca.df)

hca.df.scale <- scale(hca.df)
d <- dist(hca.df.scale, method = "euclidean") 
fit <- hclust(d, method="ward.D2")

op = par(bg='#a4ffff') # background color

png('./graphics/cluster.png', width=12, height=8, units='in', res=300)
plot(fit, col="#487AA1", col.main="#45ADA8", col.lab="#7C8071",
     col.axis="#F38630", lwd=1, lty=2, sub='', hang=0, axes=FALSE)

axis(side=2, at=seq(0, 400, 100), col="#F38630",
     labels=FALSE, lwd=2)
mtext(seq(0, 400, 100), side=2, at=seq(0, 400, 100),
      line=1, col="#A38630", las=2)

groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")

dev.off()

par(op)














