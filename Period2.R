# AIMS Network
install.packages("network")
install.packages("sna")
install.packages("igraph")
install.packages('formattable')
library( network )
library( sna )
library( igraph)
library( formattable )
library( ggplot2 ) 
library( reshape2 )
setwd('/home/emily/Desktop/From Ubuntu/AIMS Essay 2017/AIMS Data')
getwd()

######################################## Data
### Creating Adjacency Matrix Function
adjmatrix <- function(x, n){ # Input data file directory and number of edges to import
  data <- read.table(x, header=TRUE, sep=',')
  apr <- matrix(0, nrow = dim(data)[1], ncol = dim(data)[1])
  for (i in 1:n)
    {
    s <- i + 6
    f1 <- data.frame(data$Node, data[s])
    f2 <- graph.data.frame(f1)
    f3 <- delete_vertices(f2, 'NA')
    f4 <- as_adj(f3)
    apr = apr + f4
  }
  return(as.matrix( apr ))
}

# Function to calculate individual reciprocity
recip <- function(x){ ###input matrix
  column <- c()
  for (i in 1:41) { 
    s = 0
    for (j in 1:41) {
      s = s + x[i,][j][[1]]*x[,i][j][[1]]
    } 
    column <- c(column, s)
  }
  return(column)
}

data <- read.table("DecemberCut.csv", header=TRUE, sep=',')

### december data
dec <- adjmatrix("DecemberCut.csv", 8)
decgraph <- graph_from_adjacency_matrix(dec)
### april data
apr <- adjmatrix("AprilCut.csv", 8)
aprgraph <- graph_from_adjacency_matrix(apr)

write.table(dec,file="decmatrix.txt",row.names=FALSE)
write_graph(decgraph, file="decedge.txt", format = c("edgelist"))

write.table(apr,file="aprmatrix.txt",row.names=FALSE)
write_graph(aprgraph, file="apredge.txt", format = c("edgelist"))

#"pajek", "ncol", "lgl",
#"graphml", "dimacs", "gml", "dot", "leda"

### Add Attributes to Graph (assumed constant for each time period)
addattributes <- function(x){
  x <- set.vertex.attribute(x, "Country", index=V(x), data$Country)
  x <- set.vertex.attribute(x, "FoS", index=V(x), data$Field.of.Study)
  x <- set.vertex.attribute(x, "Sex", index=V(x), data$Sex)
  x <- set.vertex.attribute(x, "Language", index=V(x), data$Language)
}

decgraph <- addattributes(decgraph)
aprgraph <- addattributes(aprgraph)
get.vertex.attribute(decgraph, "Country")

###################################### Drawing Networks
par( mfrow = c( 1, 2 ) )             # draw in the same frame
decnet <- as.network( dec )          # convert to network
plot( decnet)                        # plot network
aprnet <- as.network( apr )          # convert to network 
plot( aprnet )                       # plot network

### Network Plots by Country and Sex
# Size by out-degree
deg <- degree(decgraph, mode="out")
V(decgraph)$size <- deg*300/sum(dec)
deg <- degree(aprgraph, mode="out")
V(aprgraph)$size <- deg*300/sum(dec)

# Size by in-degree
deg <- degree(decgraph, mode="in")
V(decgraph)$size <- deg*300/sum(dec)
deg <- degree(aprgraph, mode="in")
V(aprgraph)$size <- deg*300/sum(apr)

# Size by reciprocity
recipdec <- recip(dec) 
V(decgraph)$size <- recipdec*5
recipapr <- recip(apr)
V(aprgraph)$size <- recipapr*5

# Size by transitivity 
trans <- transitivity(decgraph, type = 'local')
trans[is.na(trans)] <- 0
V(decgraph)$size <- trans*30
trans <- transitivity(decgraph, type = 'local')
trans[is.na(trans)] <- 0
V(aprgraph)$size <- trans*30

# Shape by Sex
shape <- c("square", "circle")
V(decgraph)$shape <- shape[V(decgraph)$Sex]
V(aprgraph)$shape <- shape[V(decgraph)$Sex]

# Shape by Language
shape <- c("square", "circle", "rectangle")
V(aprgraph)$shape <- shape[V(decgraph)$Language]
V(aprgraph)$shape <- shape[V(decgraph)$Language]

# Colour by Country
colrs <- palette(rainbow(15))
V(decgraph)$color <- colrs[V(decgraph)$Country]
V(aprgraph)$color <- colrs[V(aprgraph)$Country]

# Colour by Language
colrs <- palette(rainbow(3))
V(decgraph)$color <- colrs[V(decgraph)$Language]
V(aprgraph)$color <- colrs[V(aprgraph)$Language]

# Colour by FoS
colrs <- palette(rainbow(7))
V(decgraph)$color <- colrs[V(decgraph)$FoS]
V(aprgraph)$color <- colrs[V(aprgraph)$FoS]

# Colour by sex
colrs <- palette(rainbow(2))
V(decgraph)$color <- colrs[V(decgraph)$Sex]
V(aprgraph)$color <- colrs[V(aprgraph)$Sex]

# Plot graphs
plot(decgraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='December')
plot(aprgraph, layout = layout_nicely, edge.arrow.size=.1, vertex.label=NA, xlab='April')
?plot
?igraph

countrylegend <- data.frame(data$Country, ID = get.vertex.attribute(decgraph, "Country"))
countrylegend <- count(countrylegend)
xtable(countrylegend)
colrs

png(file="images/decaprilarrows.png",width=800,height=700, res=72)
par( mfrow = c( 1, 2 ) )  
plot(decgraph, layout = layout_nicely, edge.arrow.size=.5,vertex.label=NA, xlab='December')
legend("bottomleft", legend=countrylegend$data.Country  , col = colrs , bty = "n", pch=20 , pt.cex = 3,  text.col=colrs , horiz = FALSE, inset = c(-0.2, 0.3))
legend("bottomleft", legend=c('Male', 'Female') , bty = "n", pch=c(1,0) , pt.cex = 2 , horiz = FALSE, inset = c(-0.2, 0.24))
plot(aprgraph, layout = layout_nicely, edge.arrow.size=.5,vertex.label=NA, xlab='April')
dev.off()



+
  plot(aprgraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='April')
# Fixing plot system
coordin <-  plot( decgraph, vertex.cex = (deg +1 )/1.5 )

### Adding attribute data to plots
### decnet %v% "Country" <- data[1:2]
### aprnet %v% "Country" <- data[1:2]

### decnet %v% "FoS" <- data[1],data[3]
### aprnet %v% "FoS" <- data[1,3]

### Plotting the difference
### diff <- abs(apr - dec)
### diffnet <- as.network( diff )
### diffnet %v% "Country" <- data[1:2]
### sum(diff)
### plot( diffnet, coord = coordin, vertex.col = "Country", xlab = 'friendship t1')

### plot( decnet, coord = coordin, vertex.col = "Country", xlab = 'friendship t1')
### plot( aprnet, coord = coordin, vertex.col = "Country", xlab = 'friendship t1')

### plot( decnet, vertex.col = "Language", xlab = 'friendship t1', layout=layout_nicely)
### plot( aprnet, vertex.col = "Language", xlab = 'friendship t1')

#gplot( decnet, vertex.col = "Country", xlab = 'friendship t1', mode='circrand', size)
#gplot( aprnet, vertex.col = "Country", xlab = 'friendship t1', mode='spring', size)

###################################### Basic network statistics
descriptivestats <- function(x){     # input adjacency matrix
  y <- graph_from_adjacency_matrix(x)
  size <- dim( x )[1]                # number of nodes
  edg <- sum(x)                      # number of edges
  avgdeg <- ave(degree(y, mode='out'))[1][[1]]   # average outdegree
  den <- gden( x )                   # density
  recip <- 2*dyad.census( y )$mut/edg# reciprocity
  trans1 <- gtrans( x )              # transitivity (as per Snijders)
  trans <- globalclustering(x)       # transitivity (as per Wasserman) ?what's the difference
  dist <- harmonicdistance(x)        # distance
  return(c(size, edg, avgdeg, den, recip, trans, dist))
  }

### Row Labels
statsnames <- c('Number of Nodes', 'Number of Edges', 'Average Degree','Density'
                ,'Reciprocity', 'Global Clustering', 'Harmonic Mean Distance')

### Data Frame for time points
stats <- data.frame(row.names = statsnames, december = descriptivestats(dec), 
                    april=descriptivestats(apr))
stats
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
xtable(stats)

### Nice Data Table
#formattable(format(stats, digits = 2), list(
#  Name=formatter(
#    "span",
#    style = x ~ ifelse(x == "Technology", 
#                       style(font.weight = "bold"), NA)),
#  Value = color_tile("white", "orange"),
#  Change = formatter(
#    "span",
#    style = x ~ style(color = ifelse(x < 0 , "red", "green")),
#    x ~ icontext(ifelse(x < 0, "arrow-down", "arrow-up"), x)))
#)

# comparing degree distributions

two <- c(c(0:10))
levls <- 0:10
a <- apply(dec, 2, sum)
outdegdec <- sapply(levls, function(dec) {sum(a <= dec)})\
a <- apply(apr, 2, sum)
outdegapr <- sapply(levls, function(apr) {sum(a <= apr)})
one <- data.frame(x = two, dec = outdegdec, apr = outdegapr)
p <- ggplot(one, aes(x=x, y=dec)) + geom_line() +
  stat_summary(data = one, geom='point', aes(x = x, y = apr), size=2, color = 'red')
p

two <- c(c(0:10))
levls <- 0:10
a <- apply(dec, 2, sum)
outdegdec <- sapply(levls, function(dec) {sum(a <= dec)})
a <- apply(apr, 2, sum)
outdegapr <- sapply(levls, function(apr) {sum(a <= apr)})
one <- data.frame(dec = outdegdec, apr = outdegapr)
one <- melt(one)
one <- data.frame(x = c(two, two), one)
p <- ggplot(one, aes(x=x, y=value, group=variable, colour=variable)) + geom_line()+
  + geom_area(one, aes(y = value))
p + xlab('Out-Degree')+ylab('Frequency')+ ggtitle('Out-Degree Distribution')+
  theme(legend.position="none")






stat_summary(data = violinobs, geom="point", aes(x = variable, y = value), size=2, color = 'red') +
  stat_summary(fun.y=mean, geom="point", size=2, color = 'black') + theme_classic()+
  stat_summary(fun.y=median, geom="point", size=1, color = 'green')
png(file="images/outdegmultiIII.png",width=800,height=600, res=100)
p + theme(legend.position='none')+ 
  scale_x_discrete(name= 'Out-Degree Count',labels=c("X1" = "0", 'X2'='1','X3'='2','X4'='3','X5'='4','X6'='5', 'X7'='6','X8'='7', 'X9'='8','X10'='9')) +
  ggtitle(expression(paste("Out-Degree: ", gamma[1]))) +ylab('Frequency') +
  annotate(geom="text", x=3, y=35, label="hotellings: 0.03",
           color="blue")+
  #annotate(geom="text", x=3, y=32, label="sign: 0.44",
  #         color="blue")+
  annotate(geom="text", x=3, y=32, label="sign: 0.71",
           color="blue")
dev.off()
























png(file="images/histdeclog10.png",width=800,height=700, res=72)
ggplot( outdegree, aes(outdegree) ) + 
geom_histogram(aes(y=cumsum(..count..)), 
               col="black", 
               fill="black") + 
  labs(title="December", size='5') +
  labs(x="Out-Degree", y='') +
  ylim(c(0,41)) +
  scale_x_log10() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20), plot.title=element_text(size=20))
dev.off()

outdegree <- degree(aprgraph, mode='out')
outdegree <- data.frame(outdegree) #outgoing ties of each note





png(file="images/histaprlog10.png",width=800,height=700, res=72)
ggplot( outdegree, aes(outdegree) ) + 
  #geom_histogram(aes(y=cumsum(..count..)), 
  #               col="black", 
  #               fill="black") + 
  geom_density(alpha=.1, fill="#FF6666")  +
  labs(title="April") +
  labs(x="Out-Degree", y='') +
  #ylim(c(0,41)) +
  scale_x_log10() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20), plot.title=element_text(size=20))
dev.off()

indegree <- degree(decgraph, mode = "in" )
indegree <- data.frame(indegree) #outgoing ties of each note

png(file="images/histdecin.png",width=800,height=700, res=72)
ggplot( indegree, aes(indegree) ) + 
  geom_histogram(breaks=seq(0, 11, by = 1), 
                 col="black", 
                 fill="black") + 
  labs(title="December", size='5') +
  labs(x="In-Degree", y='') +
  ylim(c(0,11)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20), plot.title=element_text(size=20))
dev.off()

indegree <- degree(aprgraph, mode = "in" )
indegree <- data.frame(indegree) #outgoing ties of each note

png(file="images/histaprin.png",width=800,height=700, res=72)
ggplot( indegree, aes(indegree) ) + 
  geom_histogram(breaks=seq(0, 11, by = 1), 
                 col="black", 
                 fill="black") + 
  labs(title="April", size='5') +
  labs(x="In-Degree", y='') +
  ylim(c(0,11)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20), plot.title=element_text(size=20))
dev.off()


