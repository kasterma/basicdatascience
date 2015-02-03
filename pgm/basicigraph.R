library(igraph)
library(pryr)

G1 <- graph.empty() +
      vertex("hi") + vertex("ho") + vertex("ha") +
      edge(c("hi", "ho")) + edge(c("hi", "ha"))

G2 <- graph.empty() +
      vertex("tra") + vertex("la") + vertex("di") + vertex("da") +
      edge(c("tra", "la")) + edge(c("la", "di")) + edge(c("di", "da"))

plot(G1)

plot(G1 %du% G2)

set.seed(4)
l1 <- layout.auto(G1)
l2 <- layout.auto(G2)
plot(G1 %du% G2, layout=rbind(l1,l2))

l1 <- l1 + matrix(c(1,1,1,2,2,2), ncol=2)
plot(G1 %du% G2, layout=rbind(l1,l2))

GenGraph <- graph.empty() +
  vertex("source", color="green") +
  vertex("sink", color="red") +
  vertices("I", "II", "III", "IV", "V",
           "likelyA", "nolikelyA") +
  edges(c("source", "I",
          "source", "II"),
        feature="feat1",
        value=c("1A", "1B"),
        prob=c(0.3, 0.7)) +
  edge(c("I", "II"),
       feature="feat2",
       value="X",
       prob=0.3) + # Note: next three are also paths away from "I"
  edges(c("I", "III",
          "I", "IV",
          "I", "V"),
        feature="feat3",
        value=c("3A", "3B", "4C"),
        prob=c(0.2, 0.3, 0.2)) +
  edges(c("II", "III",
          "II", "IV"),
        feature="feat3",
        value=c("3A", "3B"),
        prob=c(0.5, 0.5)) +
  edges(c("III", "likelyA",
          "III", "nolikelyA",
          "IV", "likelyA",
          "IV", "nolikelyA",
          "V", "likelyA",
          "V", "nolikelyA"),
        feature="feat4",
        value=c("4A", "4B"),
        prob=c(0.2, 0.8)) +
  edges(c("likelyA", "sink",
          "likelyA", "sink"),
        feature="A",
        value=c(TRUE, FALSE),
        prob=c(0.9, 0.1)) +
  edges(c("nolikelyA", "sink",
          "nolikelyA", "sink"),
        feature="A",
        value=c(TRUE, FALSE),
        prob=c(0.1, 0.9))

plot(GenGraph)

generate <- function(feature.graph) {
  cur.node <- V(feature.graph)["source"]
  featvals <- list()

  while(length(out.edge.ids <- incident(feature.graph,
                                        cur.node,
                                        mode="out")) > 0) {
    if(length(out.edge.ids) == 1) {
      follow.edge.id <- out.edge.ids[1]
      edge <- E(feature.graph)[follow.edge.id]
    } else {
      follow.edge.id <- sample(out.edge.ids,
                               size=1,
                               prob=E(feature.graph)[out.edge.ids]$prob)
      edge <- E(feature.graph)[follow.edge.id]
    }
    featvals[edge$feature] <- edge$value
    cur.node <- get.edge(feature.graph,follow.edge.id)[2]
  }

  as.data.frame(featvals)
}

lapply(seq(10), f(x, generate(GenGraph)))