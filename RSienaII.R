load("Period2.R")
install.packages("RSiena", repos="http://R-Forge.R-project.org")
library(RSiena)

# A number of objects need to be created in R, as preparations to letting
# siena07 execute the estimation. This will be indicated by
# A: dependent variables;
# B: explanatory variables;
# C: combination of dependent and explanatory variables;
# D: model specification.

setwd('/home/emily/Desktop/From Ubuntu/AIMS Essay 2017/AIMS Data')

# Dependent Variables
friends <- sienaDependent(array( c( dec, apr),dim = c( 41, 41, 2 ) ) )
class(friends)
dim(friends)
friends

### Explanatory Variables
sex <- get.vertex.attribute(decgraph, "Sex")
sex <- coCovar( sex )

country <- get.vertex.attribute(decgraph, "Country")
country <- coCovar( country )
?coCovar

lang <- get.vertex.attribute(decgraph, "Language")
lang <- coCovar( lang, centered=FALSE )

fos <- get.vertex.attribute(decgraph, "FoS")
fos <- coCovar( fos , centered = FALSE)

### Combining Dataset
mydata <- sienaDataCreate( friends, country, sex) 
mydata
myeff <- getEffects( mydata )
print01Report( mydata, modelname = 'Period2Prelim' )

### Including Effects Backward Selection
myeff <- includeEffects( myeff, transTrip ,transTies, cycle3, nbrDist2, balance )
#myeff <- includeEffects( myeff, inPop, outPop, outAct )
myeff <- includeEffects( myeff, egoX, altX, sameX, interaction1 = 'sex')
myeff <- includeEffects( myeff, egoX, altX, sameX, interaction1 = 'country')
myeff

### Run Algorithm
betatest <- sienaAlgorithmCreate(projname = 'betatest')
ans <- siena07( betatest , data = mydata, effects = myeff, returnDeps = TRUE)
ans
summary(ans)

# t-statistics
tval <- for (i in 1:12){
  print(c(ans$theta[i] / sqrt(ans$covtheta[i,][i]) ) ) 
}
# p-values
pval <- for (i in 1:13){
  print(c(i,2*pnorm(-abs(ans$theta[i] / (sqrt(ans$covtheta[i,][i]))))  ))
}


?Wald.RSiena
xtable(ans)
tvale
### Backward Elimination
# Phase I
myeff <- includeEffects( myeff, sameX, egoX, altX, interaction1 = 'sex', include = FALSE)
myeff <- includeEffects( myeff, cycle3, transTies, include = FALSE )
myeff <- includeEffects( myeff, altX,  interaction1 = 'country', include = FALSE)

### Forward Selection
# degree related pop and activity
myeff <- setEffect(myeff, inPop , fix=TRUE, test=TRUE, include=FALSE)
myeff <- setEffect(myeff, outPop , fix=TRUE, test=TRUE, include=FALSE)
myeff <- setEffect(myeff, outAct , fix=TRUE, test=TRUE, include=FALSE)
myeff <- setEffect(myeff, inAct , fix=TRUE, test=TRUE, include = FALSE)
# degree related assortativity
myeff <- setEffect(myeff, outInAss , fix=TRUE, test=TRUE, include=FALSE)
myeff <- setEffect(myeff, inInAss , fix=TRUE, test=TRUE, include=FALSE)
myeff <- setEffect(myeff, outOutAss , fix=TRUE, test=TRUE, include=FALSE)

### Testing Effects
myeff <- setEffect(myeff, altX, interaction1 = 'sex' , fix=TRUE, test=TRUE, include = FALSE)
myeff <- setEffect(myeff, between , fix=TRUE, test=TRUE, include = FALSE)
myeff <- setEffect(myeff, reciPop , fix=TRUE, test=TRUE, include = FALSE)

myeff <- setEffect(myeff, diffXInPop, interaction1 = 'sex', fix=TRUE, test=TRUE, include = FALSE)
myeff <- setEffect(myeff, sameXOutAct, interaction1 = 'sex', fix=TRUE, test=TRUE, include = FALSE)
myeff <- setEffect(myeff, diffXOutAct, interaction1 = 'sex', fix=TRUE, test=TRUE, include = FALSE)
myeff <- setEffect(myeff, homXOutAct, interaction1 = 'sex', fix=TRUE, test=TRUE, include = FALSE) #p 0.1741
myeff <- setEffect(myeff, altXOutAct, interaction1 = 'sex', fix=TRUE, test=TRUE, include = FALSE)

myeff <- setEffect(myeff, diffXInPop, interaction1 = 'country', fix=TRUE, test=TRUE, include = FALSE)
myeff <- setEffect(myeff, sameXOutAct, interaction1 = 'country', fix=TRUE, test=TRUE, include = FALSE)
myeff <- setEffect(myeff, diffXOutAct, interaction1 = 'country', fix=TRUE, test=TRUE, include = FALSE) 
myeff <- setEffect(myeff, homXOutAct, interaction1 = 'country', fix=TRUE, test=TRUE, include = FALSE) 
myeff <- setEffect(myeff, altXOutAct, interaction1 = 'country', fix=TRUE, test=TRUE, include = FALSE)

# Country Ego p = -1.9
myeff <- setEffect(myeff, egoX, interaction1 = 'country', fix=FALSE, test=FALSE, include = FALSE)
effectsDocumentation()

# removing country effects
myeff <- includeEffects( myeff, altX, egoX, interaction1 = 'country', include = FALSE)

######################################### FINAL MODEL
myeff <- includeEffects( myeff, transTrip , nbrDist2, balance)
myeff <- includeEffects( myeff, sameX, interaction1 = 'country')
betatest <- sienaAlgorithmCreate(projname = 'betatest')
ans <- siena07( betatest , data = mydata, effects = myeff, returnDeps = TRUE)
ans
summary(ans)
for (i in 1:14){
  print(c(i,ans$theta[i] /sqrt(ans$covtheta[i,][i])))
}

# p-values
pval <- for (i in 1:12){
  print(c(2*pnorm(-abs(ans$theta[i] / (sqrt(ans$covtheta[i,][i]))))  ))
}

ans$sims[1]

################################# Attempt at Reconciling Closeness Distribution
myeffII <- includeEffects( myeff, transTrip , nbrDist2, balance)
myeffII <- includeEffects( myeff, sameX, interaction1 = 'country')
myeffII <- includeEffects( myeffII, outInAss , include = FALSE)
myeffII <- includeEffects( myeffII, inInAss, include = FALSE )
myeffII <- includeEffects( myeffII, outOutAss, include = FALSE )
myeffII <- includeEffects( myeffII, inOutAss, outPop )
betatestII <- sienaAlgorithmCreate(projname = 'betatestII')
ansII <- siena07( betatestII , data = mydata, effects = myeffII, returnDeps = TRUE)
ansII

summary(ans)
for (i in 1:14){
  print(c(i,ans$theta[i] /sqrt(ans$covtheta[i,][i])))
}

# p-values
pval <- for (i in 1:12){
  print(c(2*pnorm(-abs(ansII$theta[i] / (sqrt(ansII$covtheta[i,][i]))))  ))
}

ans$sims[1]


################################# Covariate Effects
ans$theta[7]

# First define a function that incorporates the relevant part
# of the evaluation function, dependent on the parameters b1, b2, b3,
# the overall average v_av, the similarity average sim_av,
# and the range ran_v
obj_n <- function(vi, vj){
  b1*(vi-v_av) + b2*(vj-v_av) + b3*(1 - abs(vi-vj)/ran_v - sim_av)
}
# Now fill in the values of the parameter estimates and the averages.
v_av <- attr( mydata$cCovars$country, 'mean')
sim_av <- attr( mydata$cCovars$country, 'simMean')
ran_v <- attr( mydata$cCovars$country, 'range')
b1 <- ans$theta[7]
b2 <- ans$theta[6]
b3 <- ans$theta[8]
# Define the value of v for which the table is to be given.
vv <- c(1:15)
# And calculate the table
sel_tab <- outer(vv, vv, obj_n)
# It can be displayed
sel_tab
# and if package xtable is loaded, also be written
# to a latex or html file. For example,
tab_sel <- xtable(sel_tab)
print(tab_sel,file="tab_sel.htm", type="html",
      html.table.attributes = "rules = none")
# The html.table.attributes option gives the <table> tag
# used in the html file.
country


countrylegend <- data.frame(data$Country, ID = get.vertex.attribute(decgraph, "Country"))
countrylegend <- count(countrylegend)
xtable(countrylegend)
colrs
######################################## Testing transitivity
myeffII <- includeEffects( myeff, cycle3, transtrip1, transTrip2, transMedTrip, transRecTrip, transRecTrip2)
betatestII <- sienaAlgorithmCreate(projname = 'betatest')
ansII <- siena07( betatest , data = mydata, effects = myeffII, returnDeps = TRUE)
ansII
summary(ans)
for (i in 1:14){
  print(c(i,ansII$theta[i] /sqrt(ansII$covtheta[i,][i])))
}

# p-values
pval <- for (i in 1:12){
  print(c(2*pnorm(-abs(ansII$theta[i] / (sqrt(ansII$covtheta[i,][i]))))  ))
}

