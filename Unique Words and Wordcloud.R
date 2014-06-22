###################################################################
## Wordcloud, Frequency of Unique Words
###################################################################

# how many unique words per tweet
words_list = strsplit(DATA, " ")
uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
png('./DATA/data/DATA unique words.png', width=12, height=8, units='in', res=300)
barplot(table(uniq_words_per_tweet), main='Distribution of Unique Words per Tweet', cex.main=1, col = 'red', border = 'black', cex.names = 0.95, cex.axis = 0.75, xlab = 'Words', ylab = 'Frequency')
dev.off()

#Simple wordcloud
png('./DATA/data/DATA simple wordcloud.png', width=12, height=8, units='in', res=300)
wordcloud(corpus, random.color = TRUE, max.words = 150, random.order=FALSE, rot.per=0.35, use.r.layout=T, colors=brewer.pal(8, 'Dark2'))
dev.off()

