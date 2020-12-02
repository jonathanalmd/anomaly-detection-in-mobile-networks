
elbowTest <- function(df){
  # Find optimal number of clusters
  # Find the vector of sum of squared error(SSE) for each cluster selection
  wcss <- vector()
  for(i in 1:20){
    wcss[i] = sum(kmeans(df,i)$withinss)
  }
  clusterFrame <- data.frame(withinss=wcss,Cluster=seq(1:20))
  
  # When number of cluster increases then the SSE will be reduced.
  ggplot(data=clusterFrame, aes(x=Cluster, y=withinss, group=1)) + geom_line(colour="red", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="white") + xlab("Number of clusters") + ylab("Withinss")+ theme_bw()
  
}

applySpectralClustering <- function(df, nclusters){
  specDF <- subset(df,select=c("internet_traffic","x","y"))
  specDF <- as.matrix(specDF)
  specModel <- specc(specDF,  kernel = "rbfdot", kpar = "automatic", centers=nclusters)
  print(summary(specModel))
  
  # Append the identified cluster to the input dataframe
  df$square_id <- as.factor(df$square_id)
  df$cluster <- as.factor(specModel)
  
  # clustered map
  p <- ggplot(df, aes(x,y))
  print(p + geom_point(aes(colour=cluster), size=3)+ theme_bw())
  
  return (df)
}

applyKmeans <- function(df, nclusters, psize = 1){
  kmeansModel <- kmeans(select(df,-c(activity_date)),nclusters,nstart=30,algorithm = "Forgy")
  print(summary(kmeansModel))
  
  # Append the identified cluster to the input dataframe
  df$cluster <- as.factor(kmeansModel$cluster)
  df$square_id <- as.factor(df$square_id)
  
  # clustered map
  # p <- ggplot(df, aes(x,y))
  # print(p + geom_point(aes(colour=cluster), size=psize)  + labs(fill = "Cluster")+  scale_color_npg())#scale_color_manual(values=c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))) ##scale_color_manual(values = c("#FC4E07", "#E7B800",  "#00AFBB", "#4E84C4", "#52854C")))# + scale_color_brewer(palette="Set2"))
  
  return(df)
}


