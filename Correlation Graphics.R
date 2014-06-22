
###########################################################################################
## Correlation Graphics
###########################################################################################

ibrary(corrplot)

## Setup the document term matrix and remove sparse terms
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)
ncol(dtm)

v <- apply(top10,2,sum)
v <- sort(v, decreasing = TRUE)
v1 <- sort(v[1:10])

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
                           "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", 
                           "cyan", "#007FFF", "blue", "#00007F"))

## Get the names of the 10 words that correlate the highest with "england"
words <- names(findAssocs(dtm, "england", 0.0)[,1])
oi <- as.matrix(dtm)
find <- colnames(oi) %in% words
corr <- cor(oi[,find])

png('./ohio/data/correlations.png', width=12, height=8, units='in', res=300)
corrplot(corr, method = 'color', order = 'hclust', addrect = 2, title = 'Correlations with the Word "England"', cl.ratio = 0.2, mar = c(1,0,2,0))
dev.off()






















# Create Document Term Matrix
tweets_dtm <- DocumentTermMatrix(corpus)

# get the top 15 words and make a basic graph
top15 <- as.matrix(tweets_dtm)
v.1 <- apply(top15,2,sum)
v.1 <- sort(v.1, decreasing = TRUE)
v1.1 <- sort(v.1[1:15])
barplot(v1.1, horiz=TRUE, cex.names = 0.7, las = 1, col=heat.colors(10), main="Top 15 World Cup Terms for Youngstown")


# get the names of the 15 words that correlate the highest with "England"
words <- names(findAssocs(tweets_dtm, "england", .2)[2:16])
oi <- as.matrix(tweets_dtm)
find <- colnames(oi) %in% words
corr <- cor(oi[,find])

png('./ohio/data/corrleations.png', width=12, height=8, units='in', res=300)
corrplot(corr)
dev.off()

png('corr11.png')
corrgram(corr, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main='Top Term Correlations with "UPDATE"')
dev.off()








