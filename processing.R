#nobelprize analysis with statistical techniques.
#explorative analysis, including machine learning, quantitative text analysis.
#processing and computation

#plotting frame for all nobel prizes
years          <- seq(1940, 2010, 10)
frequencies    <- c(1:length(years))

#############Part A: processing peace prize

#all speeches
nobel_prize    <- nobel_peace

#preprocessing
#remove number, capitalisation, common words, puntucation, other meaningless bits.
nobel_prize  <- tm_map(nobel_prize, removePunctuation)
nobel_prize  <- tm_map(nobel_prize, removeNumbers)
nobel_prize  <- tm_map(nobel_prize, tolower)#lowercase
nobel_prize  <- tm_map(nobel_prize, removeWords, stopwords("english"))
nobel_prize  <- tm_map(nobel_prize, stemDocument)
nobel_prize  <- tm_map(nobel_prize, stripWhitespace)
nobel_prize  <- tm_map(nobel_prize, PlainTextDocument)

#stage the data
dtm   <- DocumentTermMatrix(nobel_prize)
tdm   <- TermDocumentMatrix(nobel_prize)

#removing sparse terms
dtms  <- removeSparseTerms(dtm, 0.1)

#Clustering by Term Similarity
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.

#findFreqTerms(dtms, lowfreq=50)
freq  <- colSums(as.matrix(dtms))
freq
freq  <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
wf    <- data.frame(word = names(freq), freq = freq)

#frequencies are key outcome of the pre-processing
mostfrequentwords               <- wf[1:50,]
peace_mostfrequentwords15       <- freq[tail(ord)]

##### Sub-analysis for frequencies by decade
#top 5 peace word frequencies per year
frequency_war             <- frequencies #red
frequency_world           <- frequencies #blue
frequency_peace           <- frequencies #green
frequency_international   <- frequencies #black
frequency_one             <- frequencies #white

for (i in 1:length(nobel_peace_bydecade)) {
  count       <- i
  nobel_prize <- nobel_peace_bydecade[[count]]
  
  #preprocessing
  #remove number, capitalisation, common words, puntucation, other meaningless bits.
  nobel_prize  <- tm_map(nobel_prize, removePunctuation)
  nobel_prize  <- tm_map(nobel_prize, removeNumbers)
  nobel_prize  <- tm_map(nobel_prize, tolower)#lowercase
  nobel_prize  <- tm_map(nobel_prize, removeWords, stopwords("english"))
  nobel_prize  <- tm_map(nobel_prize, stemDocument)
  nobel_prize  <- tm_map(nobel_prize, stripWhitespace)
  nobel_prize  <- tm_map(nobel_prize, PlainTextDocument)
  
  #stage the data
  dtm   <- DocumentTermMatrix(nobel_prize)
  tdm   <- TermDocumentMatrix(nobel_prize)
  
  #removing sparse terms
  dtms  <- removeSparseTerms(dtm, 0.1)

  #findFreqTerms(dtms, lowfreq=50)
  freq  <- colSums(as.matrix(dtms))
  freq
  freq  <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
  wf    <- data.frame(word = names(freq), freq = freq)
  mostfrequentwords <- wf[1:50,]
  
  #create vectors of top 5 most freq words
  frequency_war[count]            <- mostfrequentwords['war','freq']
  frequency_world[count]          <- mostfrequentwords['world','freq']
  frequency_peace[count]          <- mostfrequentwords['peace','freq']
  frequency_international[count]  <- mostfrequentwords['international','freq']
  frequency_one[count]            <- mostfrequentwords['one','freq']
}


#data frames for plotting
peace_word_frequencies_topfive  <- data.frame(years, frequency_war, frequency_world, frequency_peace, frequency_international, frequency_one)
peace_frequencies_all           <- data.frame(frequency_war, frequency_world, frequency_peace, frequency_international, frequency_one)

#scatterplotmatrix 
peace_relationshipsbetweenwords <- pairs.panels(peace_frequencies_all)








#############Part B: processing literature prize


#top 5 literature word frequencies per year
# frequency_war            <- frequencies #red
# frequency_world          <- frequencies #blue
# frequency_peace          <- frequencies #green
# frequency_international  <- frequencies #black
# frequency_one            <- frequencies #white

for (i in 1:length(nobel_literature_bydecade)) {
  count       <- i
  nobel_prize <- nobel_literature_bydecade[[count]]
  
  #preprocessing
  #remove number, capitalisation, common words, puntucation, other meaningless bits.
  nobel_prize  <- tm_map(nobel_prize, removePunctuation)
  nobel_prize  <- tm_map(nobel_prize, removeNumbers)
  nobel_prize  <- tm_map(nobel_prize, tolower)#lowercase
  nobel_prize  <- tm_map(nobel_prize, removeWords, stopwords("english"))
  nobel_prize  <- tm_map(nobel_prize, stemDocument)
  nobel_prize  <- tm_map(nobel_prize, stripWhitespace)
  nobel_prize  <- tm_map(nobel_prize, PlainTextDocument)
  
  #stage the data
  dtm  <- DocumentTermMatrix(nobel_prize)
  tdm  <- TermDocumentMatrix(nobel_prize)
  
  #removing sparse terms
  dtms <- removeSparseTerms(dtm, 0.1)
  
  #findFreqTerms(dtms, lowfreq=50)
  freq <- colSums(as.matrix(dtms))
  freq
  freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
  wf    <- data.frame(word = names(freq), freq = freq)
  mostfrequentwords <- wf[1:50,]
  
  #create vectors of top 5 most freq words
  frequency_war[count]            <- mostfrequentwords['war','freq']
  frequency_world[count]          <- mostfrequentwords['world','freq']
  frequency_peace[count]          <- mostfrequentwords['peace','freq']
  frequency_international[count]  <- mostfrequentwords['international','freq']
  frequency_one[count]            <- mostfrequentwords['one','freq']
}

#data frames for plotting
literature_word_frequencies_topfive  <- data.frame(years, frequency_war, frequency_world, frequency_peace, frequency_international, frequency_one)
literature_frequencies_all           <- data.frame(frequency_war, frequency_world, frequency_peace, frequency_international, frequency_one)

#scatterplotmatrix 
literature_relationshipsbetweenwords <- pairs.panels(literature_frequencies_all)


#########Part C: Sentiment analysis in future
# Sentiment analysis[edit]
# Sentiment analysis may involve analysis of movie reviews for estimating how favorable a review is for a movie.[20] Such an analysis may need a labeled data set or labeling of the affectivity of words. Resources for affectivity of words and concepts have been made for WordNet[21] and ConceptNet,[22] respectively.
#
# # Text has been used to detect emotions in the related area of affective computing.[23] Text based approaches to affective computing have been used on multiple corpora such as students evaluations, children stories and news stories.
# install.packages('syuzhet')
# library('syuzhet')
#
# get_dct_transform(dtm, low_pass_size = 5, x_reverse_len = 100, scale_vals = FALSE, scale_range = FALSE)
#
# str(nobel_literature)
# head(nobel_literature)
# get_nrc_sentiment(nobel_literature)

### end of processing 

#### Plotting all 

#plot


