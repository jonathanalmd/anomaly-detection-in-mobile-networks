
plotHeatMap <- function(df){
  # heatmap
  p <- ggplot(df, aes(x,y, fill=internet_traffic))
  print(p + geom_tile() + scale_fill_material("indigo") + coord_fixed(ratio = 1) + labs(fill = "Internet traffic")+ xlab("Square.x") + ylab("Square.y")+ theme_bw())
  
}

plotHeatMap2 <- function(df){
  # heatmap
  df$weekday = factor(df$weekday, levels=c(1,0))
  
  weekday.labs <- c("Weekend","Weekday")
  names(weekday.labs) <- c(0,1)
  
  p <- ggplot(df, aes(x,y, fill=internet_traffic))
  print(p + geom_tile() + facet_grid(rows = vars(weekday), labeller = labeller(weekday = weekday.labs)) + coord_fixed(ratio = 1)+ scale_fill_material("indigo")  + labs(fill = "Internet traffic") + xlab("Square.x") + ylab("Square.y")+ theme_bw() +
    theme(aspect.ratio=1))
  
}

barplotActivityCluster <- function(df, nclusters, divide = TRUE){
  if(divide){
    for(i in 1:nclusters){
      activityCluster <- filter(df, cluster == i)
      print(ggplot(aggregate(internet_traffic ~ activity_time,activityCluster ,FUN=sum), aes(x=activity_time, y=internet_traffic/nrow(activityCluster))) + 
              geom_bar(stat="identity") +
              xlab("Hour of day") + ylab("Internet traffic") + theme_bw()) 
    }
  }else{
    for(i in 1:nclusters){
      activityCluster <- filter(df, cluster == i)
      print(ggplot(aggregate(internet_traffic ~ activity_time,activityCluster ,FUN=sum), aes(x=activity_time, y=internet_traffic)) + 
              geom_bar(stat="identity") + 
              xlab("Hour of day") + ylab("Internet traffic")+ theme_bw())
    }
  }
  print(ggplot(data=df, aes(x=activity_time, y=internet_traffic)) +
          geom_bar(stat="identity") +
          xlab("Hour of day") + ylab("Internet traffic") +
          facet_grid(rows = vars(cluster))+ theme_bw())
}

boxplotActivityCluster <- function(df, nclusters, facet = TRUE){
  for(i in 1:nclusters){
    activityCluster <- filter(df, cluster == i)
    print(ggplot(data=activityCluster, aes(x=activity_time, y=internet_traffic)) +
            geom_boxplot() +
            xlab("Hour of day") + ylab("Internet traffic")+ theme_bw()) 
  }
  
  if(facet){
    # Boxplot + Clusters normalized with facet
    print(ggplot(data=df, aes(x=activity_time, y=internet_traffic)) +
            geom_boxplot() +
            xlab("Hour of day") + ylab("Internet traffic") +
            facet_grid(rows = vars(cluster))+ theme_bw()) 
  }
}



