library('tm')
theData=read.csv('techblogs.csv')

#Get only unique titles
theData=theData[!duplicated(theData$title.text),]

#take data that only mentions start (to cover variation of startup)
StartUp1=subset(theData, grepl("start", newData$tag.text,ignore.case=TRUE))
StartUp2=subset(theData, grepl("start", newData$title.text,ignore.case=TRUE))

#Combine the datasets remove duplicates and remove the post that have nothing to do with startups
StartUps=rbind(StartUp1,StartUp2)
StartUps=StartUps[!duplicated(StartUps$title.text),]
StartUps=StartUps[-c(46,48,50,51,52,54,55,57,58,59,60,62,63,64,65,66,67,68,72,73,74,75,78,91,95,96,97,103,104,105,106),]

#=================== CREATE TERM-DOCUMENT MATRIX =====================================================================
# Create Corpus of Tweets
b=enc2utf8(theData$title.text)
b=iconv(theData$title.text,to="ASCII",sub="")
b <-Corpus(VectorSource(b))
corp=tm_map(b,removeWords, c('the', 'how','for','you','what', 'why'))
corp= tm_map(corp,removeWords, stopwords("SMART"))

# Create a Text-Document Matrix, Remove Stopwords, Convert Text to Lowercase
tdm<-TermDocumentMatrix(corp,control=list(removePunctuation=TRUE,stopwords='english',tolower=TRUE))

# Find Words Associated with Keyword
safcom <-findAssocs(tdm,'safaricom',0.1)
samsung <-findAssocs(tdm,'samsung',0.1)
orange <-findAssocs(tdm,'orange',0.1)
nokia<-findAssocs(tdm,'nokia',0.1)
facebook <-findAssocs(tdm,'facebook',0.1)
airtel <-findAssocs(tdm,'airtel',0.1)

#find the frequent terms, save only those that appear more that a certain number of time
findFreqTerms(x=tdm,lowfreq=50)
freq <- sort(rowSums(as.matrix(tdm)), decreasing =TRUE)
head(freq, 50)
words =names(freq)
textfreq=data.frame(word = names(freq),freq=freq)
textfreq=textfreq[(textfreq$freq>20),]
textfreq=textfreq[-c(2,25,30,40,43,46),]

#plot the frequent terms
qplot(x = textfreq$word[0:15],y = textfreq$freq[0:15],fill=textfreq$freq[0:15], xlab = 'Words',ylab = 'Term Frequency')+coord_flip()+geom_bar(stat="identity")

#create a word cloud
wordcloud(words = textfreq$word, freq = textfreq$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
