rm(list = ls())
setwd('~/Documents/Code/mckerrowInfluence/influenceNetwork/titles')

termFreq <- read.csv(file = 'titlesTermFreq.csv')
titles <- read.csv(file = 'titlesSchools.csv')

#makeing nodes
id <- c(paste('s0', 1:9, sep = ""), #ids for terms
        paste('s', 10:nrow(termFreq), sep = ""))
id <- c(id, 
        paste())


