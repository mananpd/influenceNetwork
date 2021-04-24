rm(list = ls())

library("igraph")

setwd('~/Documents/Code/mckerrowInfluence/influenceNetwork/titles')

termFreq <- read.csv(file = 'titlesTermFreq.csv')
titles <- read.csv(file = 'titlesSchools.csv')

#making nodes
id1 <- c(paste('s0', 1:9, sep = ""), #ids for terms
        paste('s', 10:nrow(termFreq), sep = ""))
nodes <- data.frame(id = id1, term = termFreq$stsp, freq = termFreq$Freq)
id2 <- c(paste('U0', 1:length(unique(titles$School)), sep = "")) #ids for titles
nodes <- rbind(nodes,
               data.frame(id = id2, term = unique(titles$School), freq = NA))

#making links
links <- matrix(0, ncol = length(id2), nrow = length(id1))
rownames(links) <- id1 
colnames(links) <- id2
for(i in 1:nrow(termFreq)){
  for(k in 1:nrow(titles)){
    if(grepl(termFreq[i,1], titles[k,1], fixed = TRUE)){
      for(l in 1:length(unique(titles$School))){
        if(unique(titles$School)[l] == titles[k,2]){
          links[i,l] <- 1
          }
      }
    }
  }
}

#dropping terms with no connection
drop_term <- rownames(links)[apply(links, 1, sum) == 0]
drop_idx <- c()
for(i in 1:length(drop_term)){
  drop_idx <- c(drop_idx, which(nodes[1] == drop_term[i]))
}
nodes <- nodes[-c(drop_idx),]
termFreq <- termFreq[-c(drop_idx),]

links <- links[apply(links, 1, sum) != 0,]



net <- graph_from_incidence_matrix(links)

V(net)$color <- c("white", "orange")[V(net)$type+1]
V(net)$shape <- c("none", "circle")[V(net)$type+1]
V(net)$label[V(net)$type==T] <- nodes$term[V(net)$type==T] 
V(net)$label[V(net)$type==F] <- nodes$term[V(net)$type==F] 
V(net)$label.font=2
V(net)$label.cex=.6
V(net)$label.cex[V(net)$type==F] <- termFreq$Freq * 0.05



layout <- layout_on_sphere(net)
plot(net) 
