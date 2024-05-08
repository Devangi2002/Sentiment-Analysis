#Packages
install.packages("tm")
install.packages("wordcloud")
install.packages("ggplot2")

#Libraries
library(tm)
library(wordcloud)
library(ggplot2)


#read file
dataset <- read.csv(file.choose(), header=T)
str(dataset)
View(dataset)

#Build corpus
corpus <- iconv(dataset$text, to ="utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Clean text
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords,stopwords("english"))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))


#remove common word
cleanset <- tm_map(cleanset, removeWords , c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub, pattern = 'stocks', replacement = 'stock')
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:10]

#Bar plot
graph <- rowSums(tdm)
graph <- subset(graph,graph>=20)

barplot(graph,
        las = 2,
        col = rainbow(50))

#Word cloud
graph <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
#plot(1:30)
wordcloud(words = names(graph), freq = graph,ma.words=60,random.order=FALSE,min.freq=30,colors=brewer.pal(8,'Dark2'),scale = c(5,0.3),rot.per=0.3)


#WORDCLOUD2: OPTIONAL


#Sentiment Analysis:Read file
dataset <- read.csv(file.choose(), header=T)
tweets <- iconv(dataset$text, to ="utf-8")


#Sentiment Scores
s<- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment(tweets[8])
tweets[8]

#Bar plot
barplot(colSums(s),las=2,col=rainbow(10),ylab='Measure',main = 'Sentiment Scores for apple tweets')

#SOCIAL NETWORK ANALYSIS
g <- graph(c("Dabu","Aavu","Shanu","Dabu","Sauru","Abhignia","Abhignia","Aavu"))
plot(g,
     vertex.color="green",
     vertex.size = 40,
     edge.color = 'red')
g[]
g
#Network measures
degree(g, mode="all")
degree(g, mode="in")
degree(g, mode="out")

diameter(g, directed= F, weights = NA)
edge_density(g,loops=F)
ecount(g)/(vcount(g)*(vcount(g)-1))

reciprocity(g)
closeness(g,mode="all",weights = NA)
betweenness(g,directed=T,weights=NA)
edge.betweenness(g,directed=T,weights=NA)


#Read Data File
dataset <- read.csv(file.choose(),header=T)
y<- data.frame(dataset$first,dataset$second)

#Create Network
graph.data.frame(y, directed=T)
V(net)
