load("RSienaII.R")

######################################################### MULTIVARIATE ANALYSIS
install.packages('MVN')
install.packages('mvoutlier')
install.packages('vegan')
install.packages('pcaPP')
install.packages('cramer')
install.packages('ICSNP')
installed.packages('car')
install.packages( 'ggplot2' )
library(car)
library(cramer)
library( rrcov )
library(MVN)
library(mvoutlier)
library( MASS )
library( vegan )
library( pcaPP )
library( ICSNP )



############################### In-Degree Distribution
# Create observed matrix
obsindeg <-  matrix(, nrow = 1, ncol = 8)
a <- apply(apr, 2, sum)
levls <- 0:13
iddi <- sapply(levls, function(apr) {sum(a <= apr)})
obsindeg[1,] <- iddi[0:8]

# Create simulated matrix
indeg <- function(n, obsData , sims, period, groupName, varName, 
                  levls=0:13, cumulative = TRUE){
  m <- matrix(, nrow = 1000, ncol = 14)
  for (i in 1:n) {
    x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                                varName)
    a <- apply(x, 2, sum)
    if (cumulative) {
      iddi <- sapply(levls, function(i) {
        sum(a <= i)
      })
    }
    else {
      iddi <- sapply(levls, function(i) {
        sum(a == i)
      })
    }
    m[i,] <- iddi
  }
  m[is.na(m)] <- 0
  return(m)
}
simindeg <- indeg(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends', cumulative = TRUE)
simindegcount <- indeg(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends', cumulative = FALSE)

sub <- simindeg[,(0:8)]
sub1 <- simindeg[,(1:8)]
sub2 <- simindeg[,(2:7)]

# Check Covariance Scatter Plot
scatterplotMatrix(simindeg)
scatter <- scatterplotMatrix(simindegcount)
png(file="images/scatterindegcount.png",width=800,height=700, res=75)
par( mfrow = c( 1, 2 ) )  
scatterplotMatrix(simindegcount)
dev.off()



# Check Multivariate Normal Distribution
#chisq.plot(simindeg[,(2:8)])
# all data
mardiaTest(simindeg, qqplot = TRUE )
hzTest(simindeg , qqplot=TRUE)
roystonTest(simindeg, qqplot=TRUE)
# 0:8
mardiaTest(sub, qqplot = TRUE )
hzTest(sub , qqplot=TRUE)
roystonTest(sub, qqplot=TRUE)
# 1:8
mardiaTest(sub1, qqplot = TRUE )
hzTest(sub1 , qqplot=TRUE)
roystonTest(sub1, qqplot=TRUE)
# 2:7
mardiaTest(sub2, qqplot = TRUE )
hzTest(sub2 , qqplot=TRUE)
roystonTest(sub2, qqplot=TRUE)

uniPlot(sub, type = 'qqplot')
uniNorm(sub, type = "SW", desc = TRUE)

# Q? What proportion of the data lies in 2:7?
prop <- apply( sub2, MARGIN = 1, function(x){(sub2[,6]-sub2[,1])/41})
mean(prop) #68%

# Q? What proportion of the data lies in 0:8?
prop <- apply( sub, MARGIN = 1, function(x){(sub[,8]-sub[,1])/41})
mean(prop) #84%

# check homogeneity of variance
# can we ignore this because our variance is only being taken from one sample

indeg_dist <- dist(sub)
indeg_mhv <- (betadisper(indeg_dist, group = rep(1,1000) ))
set.seed(100)
permutest(indeg_mhv )
covPC(sub)


HotellingsT2(sub, mu = obsindeg[1,])
###################### DISTRIBUTION IS CLEARLY NOT MVN #########################
cramer.test(sub, obsindeg )









b <- sort(betweenness(graph_from_adjacency_matrix(apr)))
obsbetween <- matrix(0, nrow = 1, ncol = 41)
obsbetween[1,] <- b

betweennessdist <- function(n, obsData , sims, period, groupName, varName, 
                            levls=0:40, cumulative = TRUE){
  m <- matrix(0, nrow = n, ncol = 41)
  for (i in 1:n) {
    x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                                varName)
    require(igraph)
    b <- sort(betweenness(graph_from_adjacency_matrix(x)))
    names(b) <- as.character(levls)
    m[i,] <- b }
  return(m)
}
?betweenness
simbetween <- betweennessdist(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

qqnorm(simbetween[,11])

par( mfrow = c( 1, 1) )  
sub <- simbetween[,(33:41)]
mardiaTest(simbetween, qqplot = TRUE )
uniPlot(sub)
uniPlot(sub, type = 'qqplot')
hzTest(sub , qqplot=TRUE)

violin <- data.frame(simbetween)
violinII <- melt(violin)
violinobs <- melt(data.frame(obsbetween))

p <- ggplot(violinII, aes(x = variable, y = value)) + geom_violin(scale = "width",aes(fill = 1)) +
  stat_summary(data = violinobs, geom="point", aes(x = variable, y = value), size=2, color = 'red') +
  stat_summary(fun.y=mean, geom="point", size=2, color = 'black') + theme_classic()+
  stat_summary(fun.y=median, geom="point", size=1, color = 'green')
png(file="images/closemulti.png",width=800,height=600, res=100)
p + theme(legend.position='none')+ 
  scale_x_discrete(name= 'Ordered Nodes',labels=NULL) +
  ggtitle('Closeness') +ylab('Closeness Centrality') +
  annotate(geom="text", x=8, y=0.6, label="hotellings: 0.99",
           color="blue")+
  annotate(geom="text", x=8, y=0.56, label="sign: 0.47",
           color="blue")+
  annotate(geom="text", x=8, y=0.52, label="sign*: 0.72",
           color="blue")
dev.off()



### old formula for local clustering/less efficient

localclustering <- function(m){
  m2 <- m %*% m
  m3 <- m2
  n <- nrow(m)
  c <- c()
  for (i in 1:41){c <- c(c,sum(m2[i,])-m2[i,][i])}
  m3 <- m2/m2
  m3[is.na(m3)] <- 0
  m3 <- hadamard.prod((m3 - (matrix(1, nrow=41, ncol = 41)-m) ), m2)
  #for (i in 1:n) {
  #  for (j in 1:n) {
  #    ifelse( m[i,][j]!=0, m[i,][j], m3[i,][j] <- 0 )
  #  }}
  #d <- c()
  d <- rowSums(m3)
  #for (i in 1:41){d <- c(d,sum(m3[i,])-m3[i,][i])}
  r <- d/c
  r[is.na(r)] <- 0
  return(sort(r))
}


#### Testing if closeness code in R is same as essay definition.
b <- closeness(graph_from_adjacency_matrix(apr), mode = 'out')*40
Rcloseness <- matrix(0, nrow = 1, ncol = 41)
Rcloseness[1,] <- b

essaycloseness <- function( x ){
  n <- dim( x )[1]
  c <- c()
  for (i in 1:n){  
    l <- 0
    for (j in 1:n){
      ifelse( i == j, length )
      length <- length(shortest_paths(graph_from_adjacency_matrix( x ),
                                      i, j, mode='out', output = 'vpath')$vpath[[1]])
      ifelse( length == 0 , l <- l + 0, l <- l + length)
      #l <- l + (length(shortest_paths(graph_from_adjacency_matrix( x ),
      #                               i, j, mode='out', output = 'vpath')$vpath[[1]]))
    }
    ifelse( l == 0, c <- c(c, 0), ifelse( l == 1, c<- c(c,0), c <- c(c, l + 1)))
  }
  return( (n-1)/c ) 
}

essayclose <- essaycloseness( apr )
Rcloseness - essayclose

two <- c(c(1:41))
one <- data.frame(x = two, y = Rcloseness[1,], z = essayclose)
p <- ggplot(one, aes(x=one$x, y=one$y)) + geom_line() +
  stat_summary(data = one, geom='point', aes(x = x, y = z), size=2, color = 'red')
p

?mean_distance

?closeness
l <- c()
for (j in 1:41){
  l <- c(l, length(shortest_paths(graph_from_adjacency_matrix( apr ),
                                  41, 1, mode='out', output = 'vpath')$vpath[[1]]))
}
l
1/((sum(l))/40)

1/(Rcloseness/40)

length(shortest_paths(graph_from_adjacency_matrix( apr ),
                      40, 40, mode='out', output = 'vpath')$vpath[[1]])
######################################### REDUNDANT
######################################### Centrality Distributions
#library('sna')
### Betweenness 
BetweennessDistribution <- function(i, obsData , sims, period, groupName, varName, 
                                    levls=0:40, cumulative = TRUE){
  x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                              varName)
  require(igraph)
  b <- sort(betweenness(graph_from_adjacency_matrix(x)))
  names(b) <- as.character(levls)
  b
  return(b)
}

BetweennessDistribution(1, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

gof1.bd <- sienaGOF(ans, verbose=TRUE,
                    varName="friends", BetweennessDistribution)
plot(gof1.bd)

### Closeness
ClosenessDistribution <- function(i, obsData , sims, period, groupName, varName, 
                                  levls=0:40, cumulative = TRUE){
  x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                              varName)
  require(igraph)
  b <- sort(closeness(graph_from_adjacency_matrix(x)))
  names(b) <- as.character(levls)
  b
  return(b)
}

ClosenessDistribution(9, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

gof1.cd <- sienaGOF(ans, verbose=TRUE,
                    varName="friends", ClosenessDistribution)

plot(gof1.cd)

### Local Clustering
LocalClustering <- function(i, obsData , sims, period, groupName, varName, 
                            levls=0:40, cumulative = TRUE){
  x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                              varName)
  require(igraph)
  b <- transitivity(graph_from_adjacency_matrix(x), type = 'local')
  b[is.na(b)] <- 0
  b <- sort(b)
  names(b) <- as.character(levls)
  return(b)
}

LocalClustering(9, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')
gof1.lc <- sienaGOF(ans, verbose=TRUE,
                    varName="friends", LocalClustering)

plot(gof1.lc)

### Eigenvector centrality
EigenDistribution <- function(i, obsData , sims, period, groupName, varName, 
                              levls=0:40, cumulative = TRUE){
  x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                              varName)
  require(igraph)
  b <- sort(eigen_centrality(graph_from_adjacency_matrix(x))$vector)
  names(b) <- as.character(levls)
  b
  return(b)
}

EigenDistribution(1, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

gof1.ed <- sienaGOF(ans, verbose=TRUE,
                    varName="friends", EigenDistribution)

plot(gof1.ed)

### PageRank
### Eigenvector centrality
PageDistribution <- function(i, obsData , sims, period, groupName, varName, 
                             levls=0:40, cumulative = TRUE){
  x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                              varName)
  require(igraph)
  b <- sort(page_rank(graph_from_adjacency_matrix(x))$vector)
  names(b) <- as.character(levls)
  b
  return(b)
}

PageDistribution(1, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

gof1.pd <- sienaGOF(ans, verbose=TRUE,
                    varName="friends", PageDistribution)

plot(gof1.pd)

######################################### Goodness of Fit
### Goodness of fit wrt to In-Degree
par( mfrow = c( 2, 2) )  
gof1.id <- sienaGOF(ans, verbose=TRUE,
                    varName="friends", IndegreeDistribution)
gof1.id
plot(gof1.id)

### Goodness of fit wrt Out-Degree
gof1.od <- sienaGOF(ans, verbose=TRUE, varName="friends",
                    OutdegreeDistribution, levls=0:8)
gof1.od
plot(gof1.od)

### Goodness of fit wrt Geodesic Distribution
GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

gof1.gd <- sienaGOF(ans, verbose=TRUE,
                    varName="friends", GeodesicDistribution)
gof1.gd
plot(gof1.gd)

descriptives.sienaGOF(gof1.gd, showAll=TRUE)
?sienaGOF
### Goodness of fit wrt triads

TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
  unloadNamespace("igraph") # to avoid package clashes
  require(sna)
  require(network)
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  if (network.edgecount(x) <= 0){x <- symmetrize(x)}
  # because else triad.census(x) will lead to an error
  tc <- sna::triad.census(x)[1,levls]
  # triad names are transferred automatically
  tc
}

gof1.tc <- sienaGOF(ans, verbose=TRUE,
                    varName="friends", TriadCensus)
gof1.tc

# Since the triad counts are wildly different in average and scale,
# we plot them using the scale and center options:
plot(gof1.tc, scale=TRUE, center=TRUE)



# old closeness
## closeness
Ecloseness <- function(apr){
  n <- nrow(apr)
  c <- matrix(0, nrow = n, ncol = n)
  #e <- c()
  for (i in 1:n){
    d <- c()
    for (j in 1:n){
      ifelse(i == j, d <- c(d,0), d <- c(d, length(shortest_paths(graph_from_adjacency_matrix(apr), i, j, mode='out', output = 'vpath')$vpath[[1]])))
    }
    c[i,] <- d
    #e <- c(e,(41-length(which(c[i,]==0))))
  }
  f <- rowSums(c)/40
  g <- 1/f
  g[which(!is.finite(g))] <- 1/40
  return(g)
}

Eclose <- Ecloseness(apr)
matrix <- Ecloseness(apr)
Eclose - Rcloseness
warnings()
geod
essaycloseness <- function( x ){
  n <- dim( x )[1]
  c <- c()
  for (i in 1:n){  
    l <- 0
    for (j in 1:n){
      l <- l + (length(shortest_paths(graph_from_adjacency_matrix( x ), i, j, mode='out', output = 'vpath')$vpath[[1]])-1)
    }
    c <- c(c, l)
  }
  return( (n-1)/c ) 
}

shortest_paths(graph_from_adjacency_matrix( x ), 1, 3, mode='out', output = 'vpath')$vpath[[1]]


#### local mean distance

localmeandistance <- function( x ){
  y <- graph_from_adjacency_matrix( x )
  n <- dim( x )[1]
  c <- c()
  for (i in 1:n){
    l <- 0
    num <- 0
    for (j in 1:n){
      length <- length(shortest_paths(y, i, j, mode='out', output = 'vpath')$vpath[[1]])
      ifelse( length == 0 , l <- l + 0, l <- l + length - 1 )
      ifelse( length == 0, num <- num, ifelse( length ==1, num <- num, num <- num + 1))
    }
    c <- c(c, l/num)
  }
  return( mean(c) )
}

localmeandistance( apr )

warnings()

dim(x)
?mean_distance
shortest_paths(graph_from_adjacency_matrix( apr ),
               18, 5, mode='out', output = 'vpath')
g <- shortest.paths( aprgraph , 17, mode = "out",
                     weights = NULL)
g[is.infinite(g)] <- 0 
g[-17]