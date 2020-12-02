
aggragateTrafficXY <- function(df){
  df <- subset(df,select=c("square_id","internet_traffic","weekday","activity_date"))
  df <- aggregate(internet_traffic ~ square_id + weekday + activity_date, df, FUN=sum)
  df$square_id <- as.numeric(df$square_id) - 1
  df$x <- as.numeric(df$square_id) %% 100 
  df$y <- floor(as.numeric(df$square_id)/100) 
  
  return (df)
}

subMap <- function(df, x_max, x_min, y_max, y_min){
  filteredMap <- filter(df, (x > x_min & y > y_min) & (x < x_max & y < y_max))
  print(filteredMap)
  return (filteredMap)
}

mergeClusterActivityTime <- function(df_full, df_cluster,day_type=FALSE){
  # add activity time
  if(day_type){
    subdf <- subset(df_full,select=c("square_id","activity_time","internet_traffic","weekday","activity_date"))
    subdf <- aggregate(internet_traffic ~ square_id + activity_time + weekday + activity_date, subdf, FUN=sum)
    
    return (merge(subset(df_cluster, select=c("square_id","cluster","weekday","activity_date")), subdf, by=c("square_id","weekday","activity_date")))
  }else{
    subdf <- subset(df_full,select=c("square_id","activity_time","internet_traffic", "activity_date"))
    subdf <- aggregate(internet_traffic ~ square_id + activity_time + activity_date, subdf, FUN=sum)
    
    return (merge(subset(df_cluster, select=c("square_id","cluster","activity_date")), subdf, by=c("square_id","activity_date")))
  }
}

normalizeActivity <- function(df){
  df$total_activity <- normalize(df$total_activity)
  return (df)
}

