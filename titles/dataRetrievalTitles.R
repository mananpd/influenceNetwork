library(RISmed)
library(qdap)

rm(list = ls())
setwd('~/Documents/Code/mckerrowInfluence/influenceNetwork/titles')

#pubmed search term
pubmed_query <- EUtilsSummary('McKerrow JH', type='esearch', db='pubmed')
summary(pubmed_query)

#retrieving data from pubmed
data <- EUtilsGet(pubmed_query)

#Function for getting Title info for search term and determining most frequent of words
TitleFrequency <- function(count){
  titles <- data.frame('Title'=ArticleTitle(data))
  TitlesOnly <- as.character(titles$Title)
  TitlesOnly <- paste(TitlesOnly, sep="", collapse="")
  TitlesOnly <- as.vector(TitlesOnly)
  stsp <- rm_stopwords(TitlesOnly, stopwords = qdapDictionaries::Top200Words)
  stsp <- rm_nchar_words(stsp[[1]], n = 1)
  stsp <- rm_non_words(stsp)
  ord <- as.data.frame(table(stsp))
  ord <- ord[order(ord$Freq, decreasing=TRUE),]
  topTermTitle <- ord[1:count,1:2]
  topTermTitle <- topTermTitle[-1,]
  rownames(topTermTitle) <- 1:nrow(topTermTitle)
  return(topTermTitle)
}

#Getting counts for most frequent terms in Titles
count <- 60
TermFreq <- TitleFrequency(count)
TermFreq

#Saving list of most common terms in titles
write.csv(TermFreq,"titlesTermFreq.csv", row.names = FALSE)


#Saving list of titles to add corresponding school manually 
#Will go through each individual entry and determine what school was affiliated with the search term
titles <- data.frame('Title'=ArticleTitle(data))
write.csv(titles,"titles.csv", row.names = FALSE)
