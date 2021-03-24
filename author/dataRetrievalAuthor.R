library(RISmed)
library(qdap)

rm(list = ls())

#pubmed search term
pubmed_query <- EUtilsSummary('McKerrow JH', type='esearch', db='pubmed')
summary(pubmed_query)

#retrieving data from pubmed
data <- EUtilsGet(pubmed_query)

#Function for getting Title info for search term and determining most frequent of words
authorFreq <- function(count){
  author <- Author(data)
  LastNameOnly <- vector()
  for(df in author){
    for(lastName in df$LastName){
      LastNameOnly  <- c(LastNameOnly, lastName)
    }
  }
  ord <- as.data.frame(table(LastNameOnly))
  ord <- ord[order(ord$Freq, decreasing=TRUE),]
  topTermTitle <- ord[1:count,1:2]
  rownames(topTermTitle) <- 1:nrow(topTermTitle)
  return(topTermTitle)
}

#Getting counts for most frequent terms in Titles
count <- 60
TermFreq <- authorFreq(count)
TermFreq


#
author <- Author(data)

