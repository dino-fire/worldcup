
########################################################################
## Collect some data
########################################################################

# Ohio tweets
ohio <- searchTwitter('#WorldCup OR #worldcup2014 OR #brazil', n=500, geocode='39.961176,-82.998794,200mi', lang = 'en', cainfo='cacert.pem')
# How many tweets
length(ohio)

# Pennsylvania tweets
penn <- searchTwitter('#WorldCup OR #worldcup2014 OR #brazil', n=500, geocode='41.203322,-77.194525,200mi', lang = 'en',cainfo='cacert.pem')
length(penn)

# Indiana tweets
indiana <- searchTwitter('#WorldCup OR #worldcup2014 OR #brazil', n=500, geocode='39.768403,-86.158068,200mi', lang = 'en', cainfo='cacert.pem')
length(indiana)

# West Virginia tweets
wva <- searchTwitter('#WorldCup OR #worldcup2014 OR #brazil', n = 500, geocode='38.565348,-80.035400,200mi', lang = 'en', cainfo='cacert.pem')
length(wva)

# Michigan tweets
mich <- searchTwitter('#WorldCup OR #worldcup2014 OR #brazil', n = 500, geocode='42.732535,-84.555535,200mi', lang = 'en', cainfo='cacert.pem')
length(mich)

ohio_txt <- sapply(ohio, function(x) x$getText())
penn_txt <- sapply(penn, function(x) x$getText())
mich_txt <- sapply(mich, function(x) x$getText())
indiana_txt <- sapply(indiana, function(x) x$getText())
wva_txt <- sapply(wva, function(x) x$getText())

# save the tweets
write(ohio_txt, './tweets/ohio.txt')
write(wva_txt, './tweets/wva.txt')
write(penn_txt, './tweets/penn.txt')
write(mich_txt, './tweets/mich.txt')
write(indiana_txt, './tweets/indiana.txt')

# Restore tweets from a file if necessary, in text mining format (corpus)
# make sure there's nothing else in this directory except the tweet files
place <- DirSource("yourdirectoryname/") #input path for documents
YourCorpus <- Corpus(place, readerControl=list(reader=readPlain))

# Comparison wordcloud

# clean texts
clean.text = function(x)
{
        # tolower
        x = tolower(x)
        # remove rt
        x = gsub('rt', '', x)
        # remove at
        x = gsub('@\\w+', '', x)
        # remove punctuation
        x = gsub('[[:punct:]]', '', x)
        # remove numbers
        x = gsub('[[:digit:]]', '', x)
        # remove links http
        x = gsub('http\\w+', '', x)
        # remove tabs
        x = gsub('[ |\t]{2,}', '', x)
        # remove blank spaces at the beginning
        x = gsub('^ ', '', x)
        # remove blank spaces at the end
        x = gsub(' $', '', x)
        return(x)
} 

ohio_clean <- clean.text(ohio_txt)
penn_clean <- clean.text(penn_txt)
mich_clean <- clean.text(mich_txt)
indiana_clean <- clean.text(indiana_txt)
wva_clean <- clean.text(wva_txt)

# Join texts in a vector
oh <- paste(ohio_clean, collapse=' ')
pa <- paste(penn_clean, collapse=' ')
mi <- paste(mich_clean, collapse=' ')
ind <- paste(indiana_clean, collapse=' ')
wv <- paste(wva_clean, collapse=' ')

# put everything in a single vector
all <- c(oh, pa, mi, ind, wv)

all <- removeWords(all, badwordlist)
all <- removeWords(all, commonwords)
all <- removeWords(all, shortwords)

# create corpus
corpus <- Corpus(VectorSource(all))

# create term-document matrix
tdm <- TermDocumentMatrix(corpus)

# convert as matrix
tdm <- as.matrix(tdm)

# add column names
colnames(tdm) <- c('Ohio', 'Pennsylvania', 'Michigan', 'Indiana', 'West Virginia')

# comparison cloud
png('./ohio/data/comparison wordcloud 3.png', width=12, height=8, units='in', res=300)
comparison.cloud(tdm, random.order=FALSE, colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "blue"), title.cex = .5, max.words=300)
dev.off()