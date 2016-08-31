#nobelprize analysis with statistical techniques.
#explorative analysis, including machine learning, quantitative text analysis.
#source file

#libraries, installed only once.
#neededpackages   <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
#install.packages(neededpackages, dependencies=TRUE)   

library('tm')
library('wordcloud')   
library('SnowballC')
library('ggplot2')   
library('cluster')   
library('fpc')   

#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")   


# # devtools required to install quanteda from Github
# if (!require(devtools)) install.packages("devtools")
# library(devtools)   
# # install the latest version quanteda from Github
# install_github("quanteda", username="kbenoit", dependencies=TRUE, quick=TRUE)
# 
# 
# 
# library(quanteda)
# # create a corpus from the immigration texts from UK party platforms
# uk2010immigCorpus <- corpus(uk2010immig,
#                             docvars=data.frame(party=names(uk2010immig)),
#                             notes="Immigration-related sections of 2010 UK party manifestos",
#                             enc="UTF-8")
# 
#setwd
#getwd()

#load texts
peacedata           <-   file.path("nobelprize_pea/1940_2016")   
literaturedata      <-   file.path("nobelprize_lit/1949_2016")   

nobel_peace         <- Corpus(DirSource(peacedata))
nobel_literature    <- Corpus(DirSource(literaturedata))

#summary(nobel_literature)

