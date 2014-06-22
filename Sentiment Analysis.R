
#######################################################################
## Sentiment Analysis
#######################################################################

library(sentiment)

# remove retweet entities
DATA = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", DATA)
# remove at people
DATA = gsub("@\\w+", "", DATA)
# remove punctuation
DATA = gsub("[[:punct:]]", "", DATA)
# remove numbers
DATA = gsub("[[:digit:]]", "", DATA)
# remove html links
DATA = gsub("http\\w+", "", DATA)
# remove unnecessary spaces
DATA = gsub("[ \t]{2,}", "", DATA)
DATA = gsub("^\\s+|\\s+$", "", DATA)

# define "tolower error handling" function 
try.error = function(x)
{
        # create missing value
        y = NA
        # tryCatch error
        try_error = tryCatch(tolower(x), error=function(e) e)
        # if not an error
        if (!inherits(try_error, "error"))
                y = tolower(x)
        # result
        return(y)
}
# lower case using try.error with sapply 
DATA = sapply(DATA, try.error)

# remove NAs in DATA
DATA = DATA[!is.na(DATA)]
names(DATA) = NULL

# classify emotion
class_emo <- classify_emotion(DATA, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol <- classify_polarity(DATA, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=DATA, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)

head(sent_df, 20)

# sort data frame
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
png('./DATA/data/DATA emotions.png', width=12, height=8, units='in', res=300)
ggplot(sent_df, aes(x=emotion)) +
        geom_bar(aes(y=..count.., fill=emotion)) +
        scale_fill_brewer(palette="Dark2") +
        labs(x="Emotion Categories", y="Number of Tweets")
dev.off()

# plot distribution of polarity
png('./DATA/data/DATA polarity.png', width=12, height=8, units='in', res=300)
ggplot(sent_df, aes(x=polarity)) +
        geom_bar(aes(y=..count.., fill=polarity)) +
        scale_fill_brewer(palette="Dark2") +
        labs(x="Polarity Categories", y="Number of Tweets")
dev.off()

