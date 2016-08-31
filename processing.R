#nobelprize analysis with statistical techniques.
#explorative analysis, including machine learning, quantitative text analysis.
#processing and computation

#set boolean switch to execute full control flow for nobel_literature and then re-run for nobel_peace
#switched <- FALSE

#write a loop for 2nd nobel prize for peace.
# for (switched == FALSE, 1:2) {
#   nobel_literature <- nobel_literature
#   
# } else {
#   nobel_literature <- nobel_peace }

#preprocessing
#remove number, capitalisation, common words, puntucation, other meaningless bits.

nobel_literature  <- tm_map(nobel_literature, removePunctuation)
nobel_literature  <- tm_map(nobel_literature, removeNumbers)
nobel_literature  <- tm_map(nobel_literature, tolower)#lowercase
nobel_literature  <- tm_map(nobel_literature, removeWords, stopwords("english"))
nobel_literature  <- tm_map(nobel_literature, stemDocument)
nobel_literature  <- tm_map(nobel_literature, stripWhitespace)
nobel_literature  <- tm_map(nobel_literature, PlainTextDocument)

#stage the data
dtm  <- DocumentTermMatrix(nobel_literature)
tdm  <- TermDocumentMatrix(nobel_literature)

#explore data
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)

#removing sparse terms
dtms <- removeSparseTerms(dtm, 0.1)

#frequencies in multiple ways.
freq[head(ord)]
freq[tail(ord)]   
head(table(freq), 20)   
tail(table(freq), 20)   

freq <- colSums(as.matrix(dtms))   
freq
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
mostfrequentwords <- head(freq, 15)   

findFreqTerms(dtm, lowfreq=50) 
wf    <- data.frame(word=names(freq), freq=freq)   
head(wf)  

frequencyplot <- ggplot(subset(wf, freq>50), aes(word, freq))    
frequencyplot <- frequencyplot + geom_bar(stat="identity")   
frequencyplot <- frequencyplot + theme(axis.text.x=element_text(angle=45, hjust=1))   



#Relationships Between Terms Term Correlations

#word clouds
set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloudcolored <- wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)   

#Clustering by Term Similarity
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   

#Hierarchal Clustering
d     <- dist(t(dtmss), method="euclidian")   
fit   <- hclust(d=d, method="ward")   
#fit   
#plot(fit, hang=-1)   
#plot.new()
# plot(fit, hang=-1)
# groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
# rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   
# 
# #k-means clustering
# dclust  <- dist(t(dtmss), method="euclidian")   
# kfit    <- kmeans(dclust, 5)   
# clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

#For loop switch condition
# switched <- TRUE
# 
#}
