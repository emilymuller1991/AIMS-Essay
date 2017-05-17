load("Period2.R")

outdegreedec <- degree(decgraph, mode='out')
outdegreeapr <- degree(aprgraph, mode='out')
outdegree <- data.frame( ec = outdegreedec, apr = outdegreeapr )
outdegree <- melt(outdegree)

png(file="images/gamma1.png",width=800,height=700, res=72)
ggplot(outdegree, aes(x=value, fill=variable)) + geom_density(alpha=.3)+
  labs(title = expression(paste("Out-Degree: ", gamma[1])), x = 'Out-Degree', y='Density')+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank() )+
  theme(legend.position="none")+  
  theme(axis.text=element_text(size=14), axis.title=element_text(size=22), plot.title=element_text(size=22))
dev.off()

#ggplot(outdegree, aes(x=value,fill=variable))+ 
#  stat_ecdf(aes(ymin=0,ymax=..y..),geom="ribbon", alpha=.3)+
#  labs(title = 'Out-Degree Distribution', x = 'Out-Degree', y='Cumulative Distribution')

indegreedec <- degree(decgraph, mode='in')
indegreeapr <- degree(aprgraph, mode='in')
indegree <- data.frame( Dec = indegreedec, Apr = indegreeapr )
indegree <- melt(indegree)

png(file="images/gamma2.png",width=800,height=700, res=72)
ggplot(indegree, aes(x=value, fill=variable)) + geom_density(alpha=.3)+
  labs(title = expression(paste("In-Degree: ", gamma[2])), x = 'In-Degree', y='Density')+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank() )+
  guides(fill=guide_legend(title=""))+  
  theme(legend.text=element_text(size=20),axis.text=element_text(size=14), axis.title=element_text(size=22), plot.title=element_text(size=22))
dev.off()

#ggplot(indegree, aes(x=value,fill=variable))+ 
#  stat_ecdf(aes(ymin=0,ymax=..y..),geom="ribbon", alpha=.3)+
#  labs(title = 'In-Degree Distribution', x = 'In-Degree', y='Cumulative Distribution')

localclusteringdec <- sort(localclustering(dec))
localclusteringapr <- sort(localclustering(apr))
localclusteringplot <- data.frame( dec = localclusteringdec, apr = localclusteringapr)
localclusteringplot <- melt(localclusteringplot)

png(file="images/gamma3.png",width=800,height=700, res=72)
ggplot(localclusteringplot, aes(x=value, fill=variable)) + geom_density(alpha=.3)+
  labs(title = expression(paste("Local Clustering: ", gamma[3])), x = 'Local Clustering', y='Density')+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank() )+
  theme(legend.position="none")+  
  theme(axis.text=element_text(size=14), axis.title=element_text(size=22), plot.title=element_text(size=22))
dev.off()

harmonicclosenessdec <- sort(harmoniccloseness(dec))
harmonicclosenessapr <- sort(harmoniccloseness(apr))
harmonicclosenessplot <- data.frame( dec = harmonicclosenessdec, apr = harmonicclosenessapr)
harmonicclosenessplot <- melt(harmonicclosenessplot)

png(file="images/gamma4.png",width=800,height=700, res=72)
ggplot(harmonicclosenessplot, aes(x=value, fill=variable)) + geom_density(alpha=.3)+
  labs(title = expression(paste("Harmonic Closeness: ", gamma*minute[4])), x = 'Closeness Centrality', y='Density')+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank() )+xlim(c(0,0.6))+
  theme(legend.position="none")+  
  theme(axis.text=element_text(size=14), axis.title=element_text(size=22), plot.title=element_text(size=22))
dev.off()
