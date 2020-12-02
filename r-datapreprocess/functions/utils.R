
getMeanPerTime <- function(df, nclusters,weekday=FALSE){
  if(weekday){
    return (aggregate(internet_traffic ~ activity_time + cluster + weekday,df ,FUN=mean))  
  }else{
    return (aggregate(internet_traffic ~ activity_time + cluster,df ,FUN=mean))
  }
}

getSdPerTime <- function(df, nclusters, sep_date=FALSE){
  if(sep_date){
    return (aggregate(internet_traffic ~ activity_time + activity_date, df, function(x) sd(x)))
  }else{
    if(nclusters > 1){
      return (aggregate(internet_traffic ~ activity_time + cluster, df , function(x) sd(x)))
    }else{
      return (aggregate(internet_traffic ~ activity_time, df , function(x) sd(x)))
    }
  }
}

getICPerTime <- function(df, nclusters, sep_date=FALSE){
  if(nrow(df) < 20000){ # t-test
    if(sep_date){
      return (aggregate(internet_traffic ~ activity_time + activity_date, df , function(x) t.test(x)$stderr))
    }else{
      if(nclusters > 1){
        # aggregate(internet_traffic ~ activity_time + cluster, df , function(x) t.test(x)$stderr)
        return (aggregate(internet_traffic ~ activity_time + cluster, df , function(x) t.test(x)$stderr))
      }else{
        return (aggregate(internet_traffic ~ activity_time, df , function(x) t.test(x)$stderr))
      }
    }
  }else{ # z-test
    if(sep_date){
      return (aggregate(internet_traffic ~ activity_time + activity_date, df , function(x) z.test(x, sigma.x=sd(x))$conf.int[2] - z.test(x, sigma.x=sd(x))$estimate))
    }else{
      if(nclusters > 1){
        # aggregate(internet_traffic ~ activity_time + cluster, df , function(x) t.test(x)[7])
        return (aggregate(internet_traffic ~ activity_time + cluster, df , function(x) z.test(x, sigma.x=sd(x))$conf.int[2] - z.test(x, sigma.x=sd(x))$estimate))
      }else{
        return (aggregate(internet_traffic ~ activity_time, df , function(x) z.test(x, sigma.x=sd(x))$conf.int[2] - z.test(x, sigma.x=sd(x))$estimate))
      }
    }
  }
}

normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
