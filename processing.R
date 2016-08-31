#nobelprize analysis with statistical techniques.
#explorative analysis, including machine learning, quantitative text analysis.
#processing and computation


#preprocessing
#remove number, capitalisation, common words, puntucation, other meaningless bits.
#write a loop for 2nd nobel prize for peace.

nobel_literature  <- tm_map(nobel_literature, removePunctuation)
nobel_literature  <- tm_map(nobel_literature, removeNumbers)
nobel_literature  <- tm_map(nobel_literature, tolower)#lowercase
nobel_literature  <- tm_map(nobel_literature, removeWords, stopwords("english"))
nobel_literature  <- tm_map(nobel_literature, stemDocument)
nobel_literature  <- tm_map(nobel_literature, stripWhitespace)
nobel_literature  <- tm_map(nobel_literature, PlainTextDocument)
#str(nobel_literature)


#stage the data
dtm  <- DocumentTermMatrix(nobel_literature)
dtm  <- TermDocumentMatrix(nobel_literature)

#explore data
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)

#removing sparse terms
dtms <- removeSparseTerms(dtm, 0.1)

freq[head(ord)]

inspect(dtms)
nobel_literature
inspect(nobel_literature)
str(nobel_literature)
summary(nobel_literature)
