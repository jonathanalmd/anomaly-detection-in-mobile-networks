# Set dir
set_wdir <- function(){
  library(rstudioapi) 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}
# Set directory
set_wdir()


# Libraries
library(ggplot2)
library(reshape)
library(dplyr)
library(plyr)
library(tidyr)
library(kernlab)
library(ggsci)
library(BSDA) # z.test
set.seed(2511) 

# sources
source("functions/data_cleaning.R")
source("functions/data_manipulation.R")
source("functions/utils.R")
source("functions/plot.R")
source("functions/clustering.R")

# run all days together - merge data
# input_dir <- "data/merge/test"
# results_dir <- paste(input_dir,"merged-results",sep="/")
# weekday_files <- list.files(paste(input_dir, "weekday",sep="/"))
# weekend_files <- list.files(paste(input_dir, "weekend",sep="/"))
# mergeData(input_dir, results_dir, weekday_files, weekend_files)


getDataFrame <- function(data_dir, data_clean=FALSE, weekend_sep=FALSE){
  if(data_clean){
    initialCleaning()
  }

  df_full <- read.csv(data_dir,sep=",",header=T)
  
  
  df_full <- subset(df_full, select=c("square_id", "internet_traffic", "activity_date","activity_time","weekday"))
  df_full$activity_time <- as.factor(df_full$activity_time)
  df_full$activity_date <- as.factor(df_full$activity_date)
  
  return(df_full)
}

writeClustersCsv <- function(df_full_clustered, nclusters, results_dir_ml){
  
  days <- unique(df_full_clustered$activity_date)
  for(d in days){
    df_filtered <- filter(df_full_clustered, activity_date == d)
    for(i in 1:nclusters){
      for(j in 0:1){ # weekday-weekend
        df_filtered <- filter(df_full_clustered, cluster == i & weekday == j)
        df_filtered <- select(df_filtered, -c(1,2,3))
        if(j == 1){ # weekday
          write.csv(df_filtered, file = paste(results_dir_ml,"weekday", paste(d,"df_full_cluster",i,".csv",sep=""), sep="/"))
        }else{ # weekend
          write.csv(df_filtered, file = paste(results_dir_ml,"weekend", paste(d,"df_full_cluster",i,".csv",sep=""), sep="/"))
        }
      }
    }
  }
  
}

runAnalysis <- function(df_full, results_dir, simulation_name, simulation_type, nclusters){
  
  
  cur_time <- Sys.time()
  simulation_time <- strftime(cur_time, format="%Y-%m-%d_%H-%M")
  
  dir.create(file.path(results_dir), showWarnings = FALSE)
  dir.create(file.path(results_dir,simulation_name), showWarnings = FALSE)
  dir.create(file.path(results_dir,simulation_name,simulation_time), showWarnings = FALSE)
  dir.create(file.path(results_dir,simulation_name,simulation_time, simulation_type), showWarnings = FALSE)
  dir.create(file.path(results_dir,simulation_name,simulation_time, simulation_type,"csv"), showWarnings = FALSE)
  dir.create(file.path(results_dir,simulation_name,simulation_time, simulation_type,"pdf"), showWarnings = FALSE)
  # dir.create(file.path(results_dir,simulation_name,simulation_time, simulation_type,"ml-inputdata"), showWarnings = FALSE)
  # dir.create(file.path(results_dir,simulation_name,simulation_time, simulation_type,"ml-inputdata","weekend"), showWarnings = FALSE)
  # dir.create(file.path(results_dir,simulation_name,simulation_time, simulation_type,"ml-inputdata","weekday"), showWarnings = FALSE)
  

  results_dir_full = paste(results_dir, simulation_name, simulation_time, simulation_type, sep="/")
  results_dir_full_pdf = paste(results_dir_full, "pdf", sep="/")
  results_dir_full_csv = paste(results_dir_full, "csv", sep="/")
  # results_dir_ml = paste(results_dir_full, "ml-inputdata", sep="/")
  
  
  # Aggregate XY
  df_internet_ag_sum <- aggragateTrafficXY(df_full)
  
  if(simulation_type == "milano"){
    x_max <- 80
    x_min <- 40
    y_max <- 75
    y_min <- 35
    # df_internet_ag_sum_fullmap <- df_internet_ag_sum
    df_internet_ag_sum <- subMap(df_internet_ag_sum, x_max, x_min, y_max, y_min)
  }else if (simulation_type == "anomaly"){
    x_max <- 61+1
    x_min <- 59-1
    y_max <- 51+1
    y_min <- 50-1
    # df_internet_ag_sum_fullmap <- df_internet_ag_sum
    df_internet_ag_sum <- subMap(df_internet_ag_sum, x_max, x_min, y_max, y_min)
  }
  
  
  
  # START SIMULATION
  pdf(paste(results_dir_full_pdf,"plots.pdf", sep="/"))
  # elbow test
  elbowTest(select(df_internet_ag_sum,-c(activity_date)))
  
  # plot heat map
  norm_df_internet_ag_sum <- df_internet_ag_sum
  norm_df_internet_ag_sum$internet_traffic <- normalize(df_internet_ag_sum$internet_traffic)
  norm_weekday_df_internet_ag_sum <- filter(df_internet_ag_sum, weekday == 1)
  norm_weekend_df_internet_ag_sum <- filter(df_internet_ag_sum, weekday == 0)
  
  plotHeatMap(norm_df_internet_ag_sum)
  plotHeatMap2(norm_df_internet_ag_sum)
  
  # write.csv(df_internet_ag_sum, file = paste(results_dir_full_csv, "df_internet_ag_sum.csv", sep="/"))
  
  
  # Clustering
  df_internet_ag_sum_clustered <- applyKmeans(df_internet_ag_sum, nclusters=5) # Forgy
  
  pdf(paste(results_dir_full_pdf, "clusters.pdf",sep="/"))
  p <- ggplot(df_internet_ag_sum_clustered, aes(x,y))
  print(p + geom_point(shape = 15, aes(colour=cluster), size=3)+ coord_fixed(ratio = 1)  + labs(colour = "Cluster")+ xlab("Square.x") + ylab("Square.y")+scale_color_npg()+ theme_bw())#scale_color_manual(values=c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))) ##scale_color_manual(values = c("#FC4E07", "#E7B800",  "#00AFBB", "#4E84C4", "#52854C")))# + scale_color_brewer(palette="Set2"))
  dev.off()
  
  # write.csv(df_internet_ag_sum_clustered, file = paste(results_dir_full_csv,"df_internet_ag_sum_clustered.csv", sep="/"))
  
  
  
  # Boxplot (normalized)
  df_internet_full_clustered <- mergeClusterActivityTime(df_full, df_internet_ag_sum_clustered, TRUE)
  # write.csv(df_internet_full_clustered, file = paste(results_dir_full_csv,"df_internet_full_clustered.csv", sep="/"))
  
  
  
  df_internet_full_clustered_norm <- df_internet_full_clustered
  #df_internet_full_clustered_norm$internet_traffic <- normalize(df_internet_full_clustered$internet_traffic)
  df_internet_full_clustered_norm$internet_traffic <- scales::rescale(df_internet_full_clustered_norm$internet_traffic, to=c(0,1))
  
  # Write CSV for full data clustered (norm)
  # write.csv(df_internet_full_clustered_norm, file = paste(results_dir_full_csv,"df_internet_full_clustered-norm.csv", sep="/"))
  # Write filtered full data clustered (separated by cluster and day type) (norm) 
  # writeClustersCsv(df_internet_full_clustered_norm, nclusters, results_dir_ml)


  # for(i in 0:1){
  #   boxplotActivityCluster(filter(df_internet_full_clustered_norm, weekday==i), nclusters)
  #   if(i==0){
  #     write.csv(filter(df_internet_full_clustered_norm, weekday==i), file = paste(results_dir_full_csv,"weekend-df_internet_clustered_norm.csv", sep="/"))
  #   }else{
  #     write.csv(filter(df_internet_full_clustered_norm, weekday==i), file = paste(results_dir_full_csv,"weekday-df_internet_clustered_norm.csv", sep="/"))
  #   }
  # }
  
  
  df_internet_full_sum_clustered <- aggregate(internet_traffic ~ weekday + cluster + activity_time, select(df_internet_full_clustered,-c(activity_date)), FUN=mean)
  df_internet_full_sum_clustered$internet_traffic <- normalize(df_internet_full_sum_clustered$internet_traffic)
  # # Separated by date
  # df_internet_full_sum_clustered_wdate <- aggregate(internet_traffic ~ weekday + cluster + activity_time + activity_date, df_internet_full_clustered, FUN=mean)
  # df_internet_full_sum_clustered_wdate$internet_traffic <- normalize(df_internet_full_sum_clustered_wdate$internet_traffic)
  # for(i in 0:1){
  #   
  #   barplotActivityCluster(filter(df_internet_full_sum_clustered, weekday==i), nclusters, divide=FALSE)
  #   if(i == 0){
  #     write.csv(filter(df_internet_full_sum_clustered, weekday==i), file = paste(results_dir_full_csv,"weekend-df_internet_sum_clustered.csv", sep="/"))
  #   }else{
  #     write.csv(filter(df_internet_full_sum_clustered, weekday==i), file = paste(results_dir_full_csv,"weekday-df_internet_sum_clustered.csv", sep="/"))
  #   }
  #   
  # }
  
  df_internet_full_sum_clustered_sd <- data.frame(weekday=factor(),
                                                  cluster=factor(),
                                                  activity_time=factor(),
                                                  internet_traffic=numeric(),
                                                  internet_traffic_sd=numeric(),
                                                  stringsAsFactors=FALSE)
  for(i in 1:nclusters){
    for(j in 0:1){
      df_act <- subset(filter(filter(df_internet_full_sum_clustered, weekday==j), cluster == i), select=c("activity_time","internet_traffic"))
      df_act <- merge(getICPerTime(filter(select(df_internet_full_clustered_norm,-c(activity_date)), cluster == i), 1), df_act, by=c("activity_time"))
      df_act <- df_act[order(df_act$activity_time),]
      df_act <- df_act[,c(1,3,2)]
      colnames(df_act) <- c("activity_time","internet_traffic","internet_traffic_sd")
      
      rows = data.frame(rep(j, nrow(df_act)), rep(i, nrow(df_act)), df_act$activity_time, df_act$internet_traffic, df_act$internet_traffic_sd)
    
      colnames(rows) <- c("weekday","cluster","activity_time","internet_traffic","internet_traffic_sd")
      df_internet_full_sum_clustered_sd <- rbind(df_internet_full_sum_clustered_sd, rows)
      if(j == 0){
        write.csv(df_act, file = paste(results_dir_full_csv, paste("weekend-cluster",i,".csv", sep=""), sep="/"))
      }else{
        write.csv(df_act, file = paste(results_dir_full_csv, paste("weekday-cluster",i,".csv", sep=""), sep="/"))
      }
    }
  }
  
  dev.off()
  
  
  
  

  # #df_internet_full_sum_clustered_sd <- aggregate(internet_traffic ~ weekday + cluster + activity_time, select(df_internet_full_sum_clustered_wdate_sd,-c(activity_date)), FUN=mean)
  # #df_internet_full_sum_clustered_sd$internet_traffic_sd <- (aggregate(internet_traffic_sd ~ weekday + cluster + activity_time, select(df_internet_full_sum_clustered_wdate_sd,-c(activity_date)), FUN=mean))$internet_traffic_sd
  
  # df_internet_full_sum_clustered_wdate_sd %>%
  #   group_by(weekday, cluster, activity_time) %>%
  #   summarise_at(vars(-activity_date), funs(mean(., na.rm=TRUE)))

  weekday.labs <- c("Weekend","Weekday")
  names(weekday.labs) <- c(0,1)
  df_internet_full_sum_clustered_sd$weekday = factor(df_internet_full_sum_clustered_sd$weekday, levels=c(1,0))
  #df_internet_full_sum_clustered_sd$activity_time <- as.numeric(levels(df_internet_full_sum_clustered_sd$activity_time))[df_internet_full_sum_clustered_sd$activity_time]
  pdf(file = paste(results_dir_full_pdf,"resume.pdf", sep="/"), width = 21, height = 6 )	# numbers are cm 
  print(ggplot(data=df_internet_full_sum_clustered_sd, aes(x=activity_time, y=internet_traffic)) +
          geom_bar(stat="identity") +
          xlab("Hour of day") + ylab("Internet traffic")+ scale_x_discrete(breaks=seq(0,24,1))+ #scale_x_continuous(limits=c(0, 24),breaks=seq(0,24,1))+
          facet_grid(weekday~cluster, labeller = labeller(weekday = weekday.labs))+theme_bw()+
          geom_errorbar(aes(ymin=internet_traffic-internet_traffic_sd, ymax=internet_traffic+internet_traffic_sd), width=.2,
                        position=position_dodge(.9)))
  dev.off() 
  write.csv(df_internet_full_sum_clustered_sd, file = paste(results_dir_full_csv, paste("fresult-df_internet_full_sum_clustered_sd.csv", sep=""), sep="/"))
  
  
  
  
  
  # Separated by date
  df_internet_full_sum_clustered_wdate <- aggregate(internet_traffic ~ weekday + cluster + activity_time + activity_date, df_internet_full_clustered, FUN=mean)
  df_internet_full_sum_clustered_wdate$internet_traffic <- normalize(df_internet_full_sum_clustered_wdate$internet_traffic)
  sep_date <- TRUE
  if(sep_date){
    df_internet_full_sum_clustered_wdate_sd <- data.frame(weekday=factor(),
                                                    cluster=factor(),
                                                    activity_date=factor(),
                                                    activity_time=factor(),
                                                    internet_traffic=numeric(),
                                                    internet_traffic_sd=numeric(),
                                                    stringsAsFactors=FALSE)
    for(i in 1:nclusters){
      for(j in 0:1){
        df_act <- subset(filter(filter(df_internet_full_sum_clustered_wdate, weekday==j), cluster == i), select=c("activity_date","activity_time","internet_traffic"))
        df_act <- merge(getICPerTime(filter(df_internet_full_clustered_norm, weekday==j, cluster == i), 1, TRUE), df_act, by=c("activity_date","activity_time"))
        df_act <- df_act[order(df_act$activity_time),]
        df_act <- df_act[,c(1,2,4,3)]
        colnames(df_act) <- c("activity_date","activity_time","internet_traffic","internet_traffic_sd")
        
        rows = data.frame(rep(j, nrow(df_act)), rep(i, nrow(df_act)), df_act$activity_date, df_act$activity_time, df_act$internet_traffic, df_act$internet_traffic_sd)
        
        colnames(rows) <- c("weekday","cluster","activity_date","activity_time","internet_traffic","internet_traffic_sd")
        df_internet_full_sum_clustered_wdate_sd <- rbind(df_internet_full_sum_clustered_wdate_sd, rows)
        
        if(j == 0){
          write.csv(df_act, file = paste(results_dir_full_csv, paste("weekend-sepdays-cluster",i,".csv", sep=""), sep="/"))
        }else{
          write.csv(df_act, file = paste(results_dir_full_csv, paste("weekday-sepdays-cluster",i,".csv", sep=""), sep="/"))
        }
      }
    }
    

    # weekday.labs <- c("Weekend","Weekday")
    # names(weekday.labs) <- c(0,1)
    # df_internet_full_sum_clustered_wdate_sd$weekday = factor(df_internet_full_sum_clustered_wdate_sd$weekday, levels=c(1,0))
    # for(d in unique(df_internet_full_sum_clustered_wdate[["activity_date"]])){
    #   
    #   pdf(file = paste(results_dir_full_pdf,"/resume",d,".pdf", sep=""), width = 21, height = 6 )	# numbers are cm 
    #   print(ggplot(data=filter(df_internet_full_sum_clustered_wdate_sd, activity_date == d), aes(x=activity_time, y=internet_traffic)) +
    #           geom_bar(stat="identity") +
    #           xlab("Hour of day") + ylab("Internet traffic") +
    #           facet_grid(weekday~cluster, labeller = labeller(weekday = weekday.labs))+theme_bw()+
    #           geom_errorbar(aes(ymin=internet_traffic-internet_traffic_sd, ymax=internet_traffic+internet_traffic_sd), width=.2,
    #                         position=position_dodge(.9)))
    #   dev.off() 
    # }
    
    
    weekday.labs <-unique(df_internet_full_sum_clustered_wdate$activity_date)
    weekday.labs <- weekday.labs[-1]
    weekday.labs <- sort(weekday.labs)
    names(weekday.labs) <- c(1:length(weekday.labs[-1]))
    df_internet_full_sum_clustered_wdate_sd$activity_date = factor(df_internet_full_sum_clustered_wdate_sd$activity_date)
    df_internet_full_sum_clustered_wdate_sd <- filter(df_internet_full_sum_clustered_wdate_sd, activity_date != sort(unique(df_internet_full_sum_clustered_wdate_sd$activity_date))[1])
    
    # weekday.labs <-unique(df_internet_full_sum_clustered_wdate$activity_date)
    # weekday.labs <- weekday.labs
    # names(weekday.labs) <- c(1:length(weekday.labs))
    # df_internet_full_sum_clustered_wdate_sd$activity_date = factor(df_internet_full_sum_clustered_wdate_sd$activity_date)
    
    pdf(file = paste(results_dir_full_pdf,"resume-sep-days.pdf", sep="/"), width = 21, height = 30 )	# numbers are cm 
    print(ggplot(data=filter(df_internet_full_sum_clustered_wdate_sd), aes(x=activity_time, y=internet_traffic)) +
            geom_bar(stat="identity") +
            xlab("Hour of day") + ylab("Internet traffic")+ scale_x_discrete(breaks=seq(0,24,1))+#scale_x_continuous(limits=c(0, 24),breaks=seq(0,24,1))+
            facet_grid(activity_date~cluster, labeller = labeller(activity_day = weekday.labs))+theme_bw()+
            geom_errorbar(aes(ymin=internet_traffic-internet_traffic_sd, ymax=internet_traffic+internet_traffic_sd), width=.2,
                          position=position_dodge(.9)))
    dev.off() 
    write.csv(df_internet_full_sum_clustered_wdate_sd, file = paste(results_dir_full_csv, paste("fresult-df_internet_full_sum_clustered_wdate_sd.csv", sep=""), sep="/"))
    
  }
  
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster==1]  <- "c4"
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster==4]  <- "c1"
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster=="c1"]  <- 1
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster=="c4"]  <- 4
  # 
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster==1]  <- "Cluster 1"
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster==2]  <- "Cluster 2"
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster==3]  <- "Cluster 3"
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster==4]  <- "Cluster 4"  
  # df_internet_full_sum_clustered_sd$cluster[df_internet_full_sum_clustered_sd$cluster==5]  <- "Cluster 5"

}


# Set simulation parameters
results_dir <- "results"
simulation_name <- "1day" #week, 5days...
simulation_type <- "anomaly" # milano / fullmap / trento


data_dir <- paste("data/","sms-call-internet-mi-2013-11-18.txt-minutes_cln.csv",sep="")



nclusters <- 5
# Get data
# df_full <- getDataFrame(data_dir)
df_full <- subset(df_full, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
df_full$activity_time <- as.factor(df_full$activity_time)
df_full$activity_date <- as.factor(df_full$activity_date)
df_full$activity_time <- round(as.numeric(levels(df_full$activity_time))[as.integer(df_full$activity_time)],2)
df_full$activity_time <- as.factor(df_full$activity_time)
df_full$weekday <- 1

# Run simulation
runAnalysis(df_full, results_dir, simulation_name, simulation_type, nclusters)
















# # run all days separated
# input_dir <- "data/29jan2020/test"
# 
# weekday_files <- list.files(paste(input_dir,"weekday/", sep="/"))
# weekend_files <- list.files(paste(input_dir,"weekend/", sep= "/"))
# 
# i <- 1
# for(file in weekday_files[-1]){
#   results_dir <- "results"
#   simulation_name <- "oneday" #week, 5days...
#   data_clean <- FALSE
#   simulation_type <- "milano" # milano / fullmap
#   
#   weekday_data_dir <- paste(input_dir,"weekday", weekday_files[i], sep="/")
#   weekend_data_dir <- paste(input_dir,"weekend", weekend_files[i], sep="/")
#   # Test
#   # weekday_data_dir <- "data/cleaned/ml-input/oneday/test/oneday-weekday-test_cln.csv"
#   # weekend_data_dir <- "data/cleaned/ml-input/oneday/test/oneday-weekend-test_cln.csv"
#   
#   nclusters <- 5
#   # Get data  
#   df_full <- getDataFrame(weekday_data_dir, weekend_data_dir, data_clean)
#   
#   # Run simulation
#   runAnalysis(df_full, results_dir, simulation_name, simulation_type, nclusters)
#   
#   i <- i + 1
# }






