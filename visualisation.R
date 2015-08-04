#subset the data using a keyword
filData=subset(newData, grepl("airtel", newData$title.text,ignore.case=TRUE))

# Create Corpus of Tweets
corpData=iconv(filData$title.text,to="ASCII",sub="")
corpData <-Corpus(VectorSource(corpData))
corp=tm_map(corpData,removeWords, c('the', 'how','for','you','what', 'why'))
corp= tm_map(corp,removeWords, stopwords("SMART"))

# Create a Text-Document Matrix, Remove Stopwords, Convert Text to Lowercase
tdm2<-TermDocumentMatrix(corp,control=list(removePunctuation=TRUE,stopwords='english',tolower=TRUE,bounds = list(global = c(3,Inf))))


mtrx=as.matrix(tdm2)
mtrx[mtrx>=1] <- 1
mtrx2<- mtrx %*% t(mtrx)
# build graph with igraph ####
library('igraph')
# build adjacency graph
adjg <- graph.adjacency(mtrx2, weighted=TRUE, mode="undirected",
                        add.rownames=TRUE)
# remove loops
adjg<- simplify(adjg)

# superimpose a cluster structure with k-means clustering
kmg = kmeans(mtrx2, centers=8)
gk = kmg$cluster

# create nice colors for each cluster
gbrew = c("red", brewer.pal(8, "Dark2"))
gpal = rgb2hsv(col2rgb(gbrew))
gcols = rep("", length(gk))
for (k in 1:8) {
  gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
}

# prepare ingredients for plot
V(adjg)$size = 10
V(adjg)$label = V(adjg)$name
V(adjg)$degree = degree(adjg)
#V(g)$label.cex = 1.5 * log10(V(g)$degree)
V(adjg)$label.color = hsv(0, 0, 0.2, 0.55)
V(adjg)$frame.color = NA
V(adjg)$color = gcols
E(adjg)$color = hsv(0, 0, 0.7, 0.3)


# plot
glay = layout.fruchterman.reingold(adjg)
plot(adjg, layout=glay)
title("\nStartup Word Relations",
      col.main="gray40", cex.main=1.5, family="serif")
