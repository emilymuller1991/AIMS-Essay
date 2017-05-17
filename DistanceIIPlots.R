load("RSienaII.R")

install.packages("SpatialNP")
install.packages('MVN')
install.packages('ICSNP')
install.packages('vioplot')
install.packages('matrixcalc')
library(matrixcalc)
library(vioplot)
library(SpatialNP)
library(MVN)
library( ICSNP )

############################################################## In-Degree Distribution
# Actual
obsindeg <-  matrix(0, nrow = 1, ncol = 14)
a <- apply(apr, 2, sum)
levls <- 0:13
iddi <- sapply(levls, function(apr) {sum(a <= apr)})
obsindeg[1,] <- iddi

# Simulated
indeg <- function(n, obsData , sims, period, groupName, varName, 
                  levls=0:13, cumulative = TRUE){
  m <- matrix(0, nrow = 1000, ncol = 14)
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
#simindegcount <- indeg(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends', cumulative = FALSE)

### Choosing Proportion of the Data
subsimindeg <- simindeg[,(0:10)]
subobsindeg <-  matrix(, nrow = 1, ncol = 10)
subobsindeg[1,] <- obsindeg[,(0:10)]

prop <- apply( subsimindeg, MARGIN = 1, function(x){(subsimindeg[,10]-subsimindeg[,1])/41})
mean(prop) # 96%

### Testing the Data
mardiaTest(subsimindeg, qqplot = TRUE )
HotellingsT2(subsimindeg, Y = subobsindeg)
sr.loc.test(X = subsimindeg, score="sign", Y = subobsindeg )
sr.loc.test(X = subsimindeg, score="sign", Y = subobsindeg,  cond = TRUE)

##################################### Violin Plots
violin <- data.frame(subsimindeg)
violinII <- melt(violin)
violinobs <- melt(data.frame(subobsindeg))

p <- ggplot(violinII, aes(x = variable, y = value)) + geom_violin(scale = "width",aes(fill = 1)) +
    stat_summary(data = violinobs, geom="point", aes(x = variable, y = value), size=2, color = 'red') +
    stat_summary(fun.y=mean, geom="point", size=2, color = 'black') + theme_classic()+
  stat_summary(fun.y=median, geom="point", size=1, color = 'green')
png(file="images/indegmultiIII.png",width=800,height=600, res=100)
p + theme(legend.position='none')+ 
  scale_x_discrete(name= 'In-Degree Count',labels=c("X1" = "0", 'X2'='1','X3'='2','X4'='3','X5'='4','X6'='5', 'X7'='6','X8'='7', 'X9'='8','X10'='9')) +
  ggtitle(expression(paste("In-Degree: ", gamma[2]))) +ylab('Frequency') +
  annotate(geom="text", x=3, y=35, label="hotellings: 0.20",
           color="blue")+
  #annotate(geom="text", x=3, y=32, label="sign: 0.44",
  #         color="blue")+
  annotate(geom="text", x=3, y=32, label="sign: 0.40",
           color="blue")
dev.off()

############################################################## Out-Degree Distribution
# Actual
obsoutdeg <-  matrix(, nrow = 1, ncol = 14)
a <- apply(apr, 1, sum)
levls <- 0:13
oddi <- sapply(levls, function(apr) {sum(a <= apr)})
obsoutdeg[1,] <- oddi

# Simulated
outdeg <- function(n, obsData , sims, period, groupName, varName, 
                  levls=0:13, cumulative = TRUE){
  m <- matrix(, nrow = 1000, ncol = 14)
  for (i in 1:n) {
    x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                                varName)
    a <- apply(x, 1, sum)
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
simoutdeg <- outdeg(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends', cumulative = TRUE)
#simindegcount <- indeg(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends', cumulative = FALSE)

### Choosing Proportion of the Data
subsimoutdeg <- simoutdeg[,(0:10)]
subobsoutdeg <-  matrix(, nrow = 1, ncol = 10)
subobsoutdeg[1,] <- obsoutdeg[,(0:10)]

prop <- apply( subsimoutdeg, MARGIN = 1, function(x){(subsimoutdeg[,10]-subsimoutdeg[,1])/41})
mean(prop) # 92%

### Testing the Data
mardiaTest(subsimoutdeg, qqplot = TRUE )
HotellingsT2(subsimoutdeg, Y = subobsoutdeg)
sr.loc.test(X = subsimoutdeg, score="sign", Y = subobsoutdeg )
sr.loc.test(X = subsimoutdeg, score="sign", Y = subobsoutdeg,  cond = TRUE)

##################################### Violin Plots
violin <- data.frame(subsimoutdeg)
violinII <- melt(violin)
violinobs <- melt(data.frame(subobsoutdeg))

p <- ggplot(violinII, aes(x = variable, y = value)) + geom_violin(scale = "width",aes(fill = 1)) +
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

################################ Closeness 
# Actual
#b <- sort(closeness(graph_from_adjacency_matrix(apr))*40)
#obscd <- matrix(0, nrow = 1, ncol = 41)
#obscd[1,] <- b
# Observed
#ClosenessDistribution <- function(n, obsData , sims, period, groupName, varName, 
#                                  levls=0:40, cumulative = TRUE){
#  m <- matrix(0, nrow=n, ncol = 41)
#  for (i in 1:n) {
#  x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
#                              varName)
#  require(igraph)
#  b <- sort(closeness(graph_from_adjacency_matrix(x))*40)
#  names(b) <- as.character(levls)
#  m[i,] <- b }
#  return(m)
#}
#simcd <- ClosenessDistribution(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

### Testing the Data
#mardiaTest(simcd, qqplot = TRUE )
#HotellingsT2(simcd, Y = obscd)
#sr.loc.test(X = simcd, score="sign", Y = obscd)
#sr.loc.test(X = simcd, score="sign", Y = obscd,  cond = TRUE)

##################################### Violin Plots
#violin <- data.frame(simcd)
#violinII <- melt(violin)
#violinobs <- melt(data.frame(obscd))

#p <- ggplot(violinII, aes(x = variable, y = value)) + geom_violin(scale = "width",aes(fill = 1)) +
#  stat_summary(data = violinobs, geom="point", aes(x = variable, y = value), size=2, color = 'red') +
#  stat_summary(fun.y=mean, geom="point", size=2, color = 'black') + theme_classic()+
#  stat_summary(fun.y=median, geom="point", size=1, color = 'green')
#png(file="images/closemultiIII.png",width=800,height=600, res=100)
#p + theme(legend.position='none')+ 
#  scale_x_discrete(name= 'Ordered Nodes',labels=NULL) +
#  ggtitle('Closeness') +ylab('C loseness Centrality') +
#  annotate(geom="text", x=8, y=0.6, label="hotellings: 0.78",
#           color="blue")+
#  #annotate(geom="text", x=8, y=0.56, label="sign: 0.47",
#  #         color="blue")+
#  annotate(geom="text", x=8, y=0.56, label="sign: 0.05",
#           color="blue")
#dev.off()

################################ ESSAY DEFINITION Closeness 
# Actual
b <- sort(harmoniccloseness( apr ))
obscd <- matrix(0, nrow = 1, ncol = 41)
obscd[1,] <- b
# Observed
ClosenessDistribution <- function(n, obsData , sims, period, groupName, varName, 
                                  levls=0:40, cumulative = TRUE){
  m <- matrix(0, nrow=n, ncol = 41)
  for (i in 1:n) {
    x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                                varName)
    require(igraph)
    b <- sort(harmoniccloseness( as.matrix( x )))
    names(b) <- as.character(levls)
    m[i,] <- b }
  return(m)
}
simcd <- ClosenessDistribution(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

### Testing the Data
mardiaTest(simcd, qqplot = TRUE )
HotellingsT2(simcd, Y = obscd)
sr.loc.test(X = simcd, score="sign", Y = obscd)
sr.loc.test(X = simcd, score="sign", Y = obscd,  cond = TRUE)

##################################### Violin Plots
violin <- data.frame(simcd)
violinII <- melt(violin)
violinobs <- melt(data.frame(obscd))

p <- ggplot(violinII, aes(x = variable, y = value)) + geom_violin(scale = "width",aes(fill = 1)) +
  stat_summary(data = violinobs, geom="point", aes(x = variable, y = value), size=2, color = 'red') +
  stat_summary(fun.y=mean, geom="point", size=2, color = 'black') + theme_classic()+
  stat_summary(fun.y=median, geom="point", size=1, color = 'green')
png(file="images/closemultiIII.png",width=800,height=600, res=100)
p + theme(legend.position='none')+ 
  scale_x_discrete(name= 'Ordered Nodes',labels=NULL) +
  ggtitle(expression(paste("Harmonic Closeness: ", gamma*minute[4]))) +ylab('Harmonic Closeness Centrality') +
  annotate(geom="text", x=8, y=0.6, label="hotellings: 0.78",
           color="blue")+
  #annotate(geom="text", x=8, y=0.56, label="sign: 0.47",
  #         color="blue")+
  annotate(geom="text", x=8, y=0.56, label="sign: 0.06",
           color="blue")
dev.off()

#################################### Local Clustering as per R
#b <- sort(transitivity(graph_from_adjacency_matrix(apr), type = 'local'))
#b[is.na(b)] <- 0
#obslc <- matrix(0, nrow = 1, ncol = 41)
#obslc[1,] <- b

#LocalClustering <- function(n, obsData , sims, period, groupName, varName, 
#                            levls=0:40, cumulative = TRUE){
#  m <- matrix(0, nrow=n, ncol = 41)
#  for (i in 1:n) {
#  x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
#                              varName)
#  require(igraph)
#  b <- transitivity(graph_from_adjacency_matrix(x), type = 'local')
#  b[is.na(b)] <- 0
#  b <- sort(b)
#  names(b) <- as.character(levls)
#  m[i,] <- b }
#  return(m)
#}

#simlc <- LocalClustering(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

#mardiaTest(simcd, qqplot = TRUE )

#HotellingsT2(simlc, mu = obslc[1,])
#sr.loc.test(sub,nullvalue = obsoutdeg,score="rank")
#sr.loc.test(simlc, score="sign", nullvalue = obslc)
#sr.loc.test(simlc, score="sign", nullvalue = obslc,  cond = TRUE)

##################################### Violin Plots
#violin <- data.frame(simlc)
#violinII <- melt(violin)
#violinobs <- melt(data.frame(obslc))
#violin
#c <- c()
#means <- for (i in 1:40){ c <- c(c,mean(simlc[,i]))}
#mpg_se = sqrt(var(mpg)/length(mpg)))

#p <- ggplot(violinII, aes(x = variable, y = value)) + geom_violin(scale = "width",aes(fill = 1)) +
#  stat_summary(data = violinobs, geom="point", aes(x = variable, y = value), size=2, color = 'red') +
#  stat_summary(fun.y=mean, geom="point", size=2, color = 'black') + theme_classic()+
#  geom_errorbar(aes(y = mean , ymin = mean-mpg_se, ymax = mpg_mean+mpg_se), 
#                color = "black", width = 0.2, data = mtcarsSummary) + 
  
#p + theme(legend.position='none')+ 
#  scale_x_discrete(name= 'Ordered Nodes',labels=NULL) +
#  ggtitle('Closeness') +ylab('Closeness Centrality') #+
#annotate(geom="text", x=3, y=35, label="hotellings: < 2.2e-16",
#color="blue")+
#annotate(geom="text", x=3, y=32, label="signed-rank: 0.15",
#color="blue")+
#annotate(geom="text", x=3, y=29, label="sign: 0.44",
#color="blue")+
#annotate(geom="text", x=3, y=26, label="sign*: 0.98",
#       color="blue")

#################################### Local Clustering as per essay
#localclustering <- function(m){
#  m2 <- m %*% m
#  m3 <- m2
#  n <- nrow(m)
#  c <- c()
#  for (i in 1:41){c <- c(c,sum(m2[i,])-m2[i,][i])}
#  m3 <- m2/m2
#  m3[is.na(m3)] <- 0
#  m3 <- hadamard.prod((m3 - (matrix(1, nrow=41, ncol = 41)-m) ), m2)
  #for (i in 1:n) {
  #  for (j in 1:n) {
  #    ifelse( m[i,][j]!=0, m[i,][j], m3[i,][j] <- 0 )
  #  }}
  #d <- c()
#  d <- rowSums(m3)
  #for (i in 1:41){d <- c(d,sum(m3[i,])-m3[i,][i])}
#  r <- d/c
#  r[is.na(r)] <- 0
#  return(sort(r))
#}

clustering <- function(m, n, hood){
  s <- 0
  for (i in 2:n) {
    for (j in 2:n) {
      ifelse(m[hood[i],][hood[j]] == 1, s <- s + 1, 0) }}
  clust <- s/((n-1)*(n-2))
  return(clust)
}
localclustering <- function(m){
  c <- c()
  g <- nrow(m)
  mgraph <- graph_from_adjacency_matrix(m)
  for (i in 1:g){
    hood <- neighborhood(mgraph, order = 1, nodes = i, mode = 'all')[[1]]
    n <- length(hood)
    ifelse(n == 1, clust <- 0, clust <- clustering(m, n, hood))
    c <- c(c,clust)
  }
  c[is.na(c)] <- 0
  return(c)
}

five <- localclustering(apr)

mulocalclustering <- function(n, obsData , sims, period, groupName, varName, 
                            levls=0:40, cumulative = TRUE){
  m <- matrix(0, nrow=n, ncol = 41)
  for (i in 1:n) {
    x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                                varName)
    require(igraph)
    b <- localclustering(as.matrix(x))
    b[is.na(b)] <- 0
    b <- sort(b)
    names(b) <- as.character(levls)
    m[i,] <- b }
  return(m)
}

# Actual
b <- sort(localclustering(apr))
b[is.na(b)] <- 0
obslcII <- matrix(0, nrow = 1, ncol = 41)
obslcII[1,] <- b

# Observed
simlcII <- mulocalclustering(1000, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

### Choosing Proportion of the Data
subsimlc <- simlcII[,(13:41)]
subobslc <-  matrix(, nrow = 1, ncol = 29)
subobslc[1,] <- obslcII[,(13:41)]

### Testing the Data
mardiaTest(simlcII, qqplot = TRUE )
HotellingsT2(simlcII, Y = obslcII)
sr.loc.test(X = simlcII, score="sign", Y = obslcII)
sr.loc.test(X = simlcII, score="sign", Y = obslcII, cond = TRUE )

##################################### Violin Plots
library(gridExtra)
violin <- data.frame(simlcII)
violinII <- melt(violin)
violinobs <- melt(data.frame(obslcII))
#violin
#c
#c <- c()
#for (i in 1:29){ c <- c(c,mean(subsimlc[,i]))}
#d <- c()
#for (i in 1:41){ d <- c(d, sqrt(var(simlcII[,i])/length(simlcII[,i]))) }
#d
#error <- data.frame(m = c, s = d)

p <- ggplot(violinII, aes(x = variable, y = value)) + geom_violin(scale = "width",aes(fill = 1)) +
  stat_summary(data = violinobs, geom="point", aes(x = variable, y = value), size=2, color = 'red') +
  stat_summary(fun.y=mean, geom="point", size=2, color = 'black') + theme_classic()+
  stat_summary(fun.y=median, geom="point", size=1, color = 'green')
  #geom_errorbar(error, aes(y = m, ymin = m-s, ymax = m+s), color = "black", width = 0.4)
png(file="images/clustmultiIII.png",width=800,height=600, res=100)
p + theme(legend.position='none')+ 
  scale_x_discrete(name= 'Ordered Nodes',labels=NULL) +
  ggtitle(expression(paste("Local Clustering: ", gamma[3]))) +ylab('Local Clusteting Coefficient') +
  annotate(geom="text", x=8, y=0.8, label="hotellings: 0.30", color="blue")+
  #annotate(geom="text", x=8, y=0.75, label="sign: 0.47", color="blue")+
  annotate(geom="text", x=8, y=0.75, label="sign: 0.98",color="blue")
dev.off()


########################################################### Testing Closeness
two <- c(c(1:41))
one <- data.frame(x = two, y = simcd[(31),], z = obscd[1,])
p <- ggplot(one, aes(x=x, y=y)) + geom_line() +
    stat_summary(data = one, geom='point', aes(x = x, y = z), size=2, color = 'red')
p

######## Plotting Networks Above Mean
one <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(2, ans$f, ans$sims, 1, groupName='Data1', 'friends')))
eight <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(3, ans$f, ans$sims, 1, groupName='Data1', 'friends')))
ten <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(4, ans$f, ans$sims, 1, groupName='Data1', 'friends')))
twelve <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(5, ans$f, ans$sims, 1, groupName='Data1', 'friends')))
fifteen <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(9, ans$f, ans$sims, 1, groupName='Data1', 'friends')))

onegraph <- addattributes(one)
eightgraph <- addattributes(eight)
tengraph <- addattributes(ten)
twelvegraph <- addattributes(twelve)
fifteengraph <- addattributes(fifteen)

# Size by closeness
closen <- closeness(onegraph)
trans[is.na(trans)] <- 0
V(onegraph)$size <- closen*1500
closen <- closeness(eightgraph)
trans[is.na(trans)] <- 0
V(eightgraph)$size <- closen*1500
closen <- closeness(tengraph)
trans[is.na(trans)] <- 0
V(tengraph)$size <- closen*1500
closen <- closeness(twelvegraph)
trans[is.na(trans)] <- 0
V(twelvegraph)$size <- closen*1500
closen <- closeness(fifteengraph)
trans[is.na(trans)] <- 0
V(fifteengraph)$size <- closen*1500

# Size by in-degree
closen <- degree(onegraph, mode="in")
trans[is.na(trans)] <- 0
V(onegraph)$size <- closen*30/degree(onegraph)
closen <- degree(eightgraph, mode="in")
trans[is.na(trans)] <- 0
V(eightgraph)$size <- closen*30/degree(eight)
closen <- degree(tengraph, mode="in")
trans[is.na(trans)] <- 0
V(tengraph)$size <- closen*30/degree(ten)
closen <- degree(twelvegraph, mode="in")
trans[is.na(trans)] <- 0
V(twelvegraph)$size <- closen*30/degree(twelve)
closen <- degree(fifteengraph, mode="in")
trans[is.na(trans)] <- 0
V(fifteengraph)$size <- closen*30/degree(fifteen)

# Shape by Sex
shape <- c("square", "circle")
V(onegraph)$shape <- shape[V(onegraph)$Sex]
V(eightgraph)$shape <- shape[V(eightgraph)$Sex]
V(tengraph)$shape <- shape[V(tengraph)$Sex]
V(twelvegraph)$shape <- shape[V(twelvegraph)$Sex]
V(fifteengraph)$shape <- shape[V(fifteengraph)$Sex]

# Colour by Country
colrs <- palette(rainbow(15))
V(onegraph)$color <- colrs[V(onegraph)$Country]
V(eightgraph)$color <- colrs[V(eightgraph)$Country]
V(tengraph)$color <- colrs[V(tengraph)$Country]
V(twelvegraph)$color <- colrs[V(twelvegraph)$Country]
V(fifteengraph)$color <- colrs[V(fifteengraph)$Country]

plot(onegraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Above')
plot(eightgraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Above')
plot(tengraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Above')
plot(twelvegraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Above')
plot(fifteengraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Above')

par( mfrow = c( 2, 5) )
######## Plotting Networks Above Mean
one <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(10, ans$f, ans$sims, 1, groupName='Data1', 'friends')))
eight <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(11, ans$f, ans$sims, 1, groupName='Data1', 'friends')))
ten <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(12, ans$f, ans$sims, 1, groupName='Data1', 'friends')))
twelve <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(28, ans$f, ans$sims, 1, groupName='Data1', 'friends')))
fifteen <- graph_from_adjacency_matrix(as.matrix(sparseMatrixExtraction(31, ans$f, ans$sims, 1, groupName='Data1', 'friends')))

onegraph <- addattributes(one)
eightgraph <- addattributes(eight)
tengraph <- addattributes(ten)
twelvegraph <- addattributes(twelve)
fifteengraph <- addattributes(fifteen)

# Size by closeness
closen <- closeness(onegraph)
trans[is.na(trans)] <- 0
V(onegraph)$size <- closen*1500
closen <- closeness(eightgraph)
trans[is.na(trans)] <- 0
V(eightgraph)$size <- closen*1500
closen <- closeness(tengraph)
trans[is.na(trans)] <- 0
V(tengraph)$size <- closen*1500
closen <- closeness(twelvegraph)
trans[is.na(trans)] <- 0
V(twelvegraph)$size <- closen*1500
closen <- closeness(fifteengraph)
trans[is.na(trans)] <- 0
V(fifteengraph)$size <- closen*1500

# Size by in-degree
closen <- degree(onegraph, mode="in")
trans[is.na(trans)] <- 0
V(onegraph)$size <- closen*30/degree(onegraph)
closen <- degree(eightgraph, mode="in")
trans[is.na(trans)] <- 0
V(eightgraph)$size <- closen*30/degree(eight)
closen <- degree(tengraph, mode="in")
trans[is.na(trans)] <- 0
V(tengraph)$size <- closen*30/degree(ten)
closen <- degree(twelvegraph, mode="in")
trans[is.na(trans)] <- 0
V(twelvegraph)$size <- closen*30/degree(twelve)
closen <- degree(fifteengraph, mode="in")
trans[is.na(trans)] <- 0
V(fifteengraph)$size <- closen*30/degree(fifteen)

# Shape by Sex
shape <- c("square", "circle")
V(onegraph)$shape <- shape[V(onegraph)$Sex]
V(eightgraph)$shape <- shape[V(eightgraph)$Sex]
V(tengraph)$shape <- shape[V(tengraph)$Sex]
V(twelvegraph)$shape <- shape[V(twelvegraph)$Sex]
V(fifteengraph)$shape <- shape[V(fifteengraph)$Sex]

# Colour by Country
colrs <- palette(rainbow(15))
V(onegraph)$color <- colrs[V(onegraph)$Country]
V(eightgraph)$color <- colrs[V(eightgraph)$Country]
V(tengraph)$color <- colrs[V(tengraph)$Country]
V(twelvegraph)$color <- colrs[V(twelvegraph)$Country]
V(fifteengraph)$color <- colrs[V(fifteengraph)$Country]

plot(onegraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Below')
plot(eightgraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Below')
plot(tengraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Below')
plot(twelvegraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Below')
plot(fifteengraph, layout = layout_nicely, edge.arrow.size=.1,vertex.label=NA, xlab='Below')


############################################ Closeness as per Essay Def
################################ Closeness 
# Actual
b <- sort(closeness(graph_from_adjacency_matrix(apr))*41)
obscd <- matrix(0, nrow = 1, ncol = 41)
obscd[1,] <- b
# Observed
ClosenessDistribution <- function(n, obsData , sims, period, groupName, varName, 
                                  levls=0:40, cumulative = TRUE){
  m <- matrix(0, nrow=n, ncol = 41)
  for (i in 1:n) {
    x <- sparseMatrixExtraction(i, obsData, sims, period, groupName, 
                                varName)
    require(igraph)
    b <- sort(Ecloseness(x))
    names(b) <- as.character(levls)
    m[i,] <- b }
  return(m)
}
simcdII <- ClosenessDistribution(10, ans$f, ans$sims, 1 ,groupName = 'Data1', varName = 'friends')

### Testing the Data
mardiaTest(simcd, qqplot = TRUE )
HotellingsT2(simcd, Y = obscd)
sr.loc.test(X = simcd, score="sign", Y = obscd)
sr.loc.test(X = simcd, score="sign", Y = obscd,  cond = TRUE)

##################################### Violin Plots
violin <- data.frame(simcdII[1:38])
violinII <- melt(violin)
violinobs <- melt(data.frame(obscd))

p <- ggplot(violinII, aes(x = variable, y = value)) + geom_violin(scale = "width",aes(fill = 1)) +
  stat_summary(data = violinobs, geom="point", aes(x = variable, y = value), size=2, color = 'red') +
  stat_summary(fun.y=mean, geom="point", size=2, color = 'black') + theme_classic()+
  stat_summary(fun.y=median, geom="point", size=1, color = 'green')
png(file="images/closemulti.png",width=800,height=600, res=100)
p + theme(legend.position='none')+ 
  scale_x_discrete(name= 'Ordered Nodes',labels=NULL) +
  ggtitle('Closeness') +ylab('C loseness Centrality') +
  annotate(geom="text", x=8, y=0.6, label="hotellings: 0.99",
           color="blue")+
  #annotate(geom="text", x=8, y=0.56, label="sign: 0.47",
  #         color="blue")+
  annotate(geom="text", x=8, y=0.56, label="sign: 0.72",
           color="blue")
dev.off()


