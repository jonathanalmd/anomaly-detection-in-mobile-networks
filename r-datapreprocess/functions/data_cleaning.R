# Data cleaning portion based on: https://rstudio-pubs-static.s3.amazonaws.com/343521_19d968f5a54141a49f6db3eb7b6ff099.html

cleanData <- function(cdr_input, hour=TRUE){
  # clean multiple csvs and merge them
  # Get data
  # cdr_input <- read.csv(input_dir,sep="\t",header=F)

  # # Rename col names
  colnames(cdr_input) <- c("square_id","time_interval","country_code","sms_in","sms_out","call_in","call_out","internet_traffic")
  
  cdr_input_subset_df <- cdr_input
  
  # filter NAs from internet_traffic column 
  cdr_input_subset_df <- cdr_input_subset_df[!is.na(cdr_input_subset_df$internet_traffic),]
  
  cdr_input_subset_df_cln <- cdr_input_subset_df%>% group_by(country_code) %>%mutate(sms_in = ifelse(is.na(sms_in),as.integer(mean(sms_in, na.rm = TRUE)), sms_in))
  cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(sms_in = ifelse(is.na(sms_in),0, sms_in))
  cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(sms_out = ifelse(is.na(sms_out),as.integer(mean(sms_out, na.rm = TRUE)), sms_out))
  cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(sms_out = ifelse(is.na(sms_out),0, sms_out))
  cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(call_in = ifelse(is.na(call_in),as.integer(mean(call_in, na.rm = TRUE)), call_in))
  cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(call_in = ifelse(is.na(call_in),0, call_in))
  cdr_input_subset_df_cln <- cdr_input_subset_df_cln%>% group_by(country_code) %>%mutate(call_out = ifelse(is.na(call_out),as.integer(mean(call_out, na.rm = TRUE)), call_out))
  cdr_input_subset_df_cln  <- cdr_input_subset_df_cln %>% mutate(call_out = ifelse(is.na(call_out),0, call_out))
  
  # print(summary(cdr_input_subset_df_cln))
  # print(summary(cdr_input_subset_df))
  
  factorColumns <- c("square_id","country_code")
  cdr_input_subset_df_cln[factorColumns] <- lapply(cdr_input_subset_df_cln[factorColumns],as.factor)
  # head(cdr_input_subset_df_cln)
  
  val <- cdr_input_subset_df_cln$time_interval/1000
  cdr_input_subset_df_cln$outputTime <- as.POSIXct(val, origin="1970-01-01",tz="UTC")
  # print(head(cdr_input_subset_df_cln))
  
  # Transform time
  # derive activity start date and hour from time interval
  cdr_input_subset_df_cln$activity_start_time <- cdr_input_subset_df_cln$outputTime 
  #cdr_input_subset_df$activity_date <- as.Date(as.POSIXct(cdr_input_subset_df$activity_start_time,origin="1970-01-01"))
  cdr_input_subset_df_cln$activity_date <- as.Date(cdr_input_subset_df_cln$activity_start_time)
  if(hour){
    # print("Hours")
    cdr_input_subset_df_cln$activity_time <- format(cdr_input_subset_df_cln$activity_start_time,"%H")  
  }else{
    # print("Minutes")
    cdr_input_subset_df_cln$activity_time <- as.numeric(format(cdr_input_subset_df_cln$activity_start_time,"%H")) + (as.numeric(format(cdr_input_subset_df_cln$activity_start_time,"%M"))/60) 
    cdr_input_subset_df_cln$activity_time <- round(cdr_input_subset_df_cln$activity_time, 2)
  }
  
  
  
  # derive total activity from sms in and out, call in and out and internet traffic activity 
  cdr_input_subset_df_cln$total_activity <- rowSums(cdr_input_subset_df_cln[, c(4,5,6,7,8)],na.rm=T)
  cdr_input_subset_df_cln <- subset(cdr_input_subset_df_cln, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
  
  return (cdr_input_subset_df_cln)
}


mergeData <- function(input_dir, results_dir, weekday_files, weekend_files, filename){
  # temporary script
  # weekday_files <- list.files("data/weekday")
  # weekend_files <- list.files("data/weekend")
  # input_dir <- "data/29jan2020/merged-test"
  # results_dir <- paste(input_dir,"merged-results",sep="/")
  dir.create(file.path(results_dir), showWarnings = FALSE)

  df_weekday_full <- read.csv(paste(input_dir,"weekday",weekday_files[1],sep="/"),sep=",",header=T)
  df_weekday_full <- subset(df_weekday_full, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
  
  # df_weekday_full$day <- i
  for(file in weekday_files[-1]){
    print(file)
    
    data_dir <- paste(input_dir,"weekday",file,sep="/")
    
    cdr_input <- read.csv(data_dir,sep=",",header=T)
    cdr_input <- subset(cdr_input, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
    
    # cdr_input$day <- i
    df_weekday_full <- rbind(df_weekday_full, cdr_input)
  }
  # Clean data
  # df_full <- cleanData(df_weekday_full_raw)
  
  # reduced data 
  df_weekday_full$weekday <- 1
  df_weekday_full <- subset(df_weekday_full, select=c("square_id", "internet_traffic", "activity_date","activity_time","weekday"))
  # write.csv(df_weekday_full, file = paste(results_dir,"weekday_merged-byday-6-7-8-11-12-13-14-15-18-19-20-21-22-23-24-25-26-27-28-29.csv", sep= "/"))
  
  
  
  df_weekend_full <- read.csv(paste(input_dir,"weekend",weekend_files[1],sep="/"),sep=",",header=T)
  df_weekend_full <- subset(df_weekend_full, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
  # df_weekend_full$day <- i
  for(file in weekend_files[-
                            1]){
    print(file)
    
    data_dir <- paste(input_dir,"weekend",file,sep="/")
    
    cdr_input <- read.csv(data_dir,sep=",",header=T)
    cdr_input <- subset(cdr_input, select=c("square_id", "internet_traffic", "activity_date","activity_time"))
    # cdr_input$day <- i
    df_weekend_full <- rbind(df_weekend_full, cdr_input)
  }
  # Clean data
  # df_full <- cleanData(df_weekend_full_raw)
  
  # reduced data 
  df_weekend_full$weekday <- 0
  df_weekend_full <- subset(df_weekend_full, select=c("square_id", "internet_traffic", "activity_date","activity_time","weekday"))
  # write.csv(df_weekend_full, file = paste(results_dir,"weekend_merged-byday-3-9-10-23-24-30.csv", sep= "/"))
  
  df_full <- rbind(df_weekday_full, df_weekend_full)
  rm(df_weekday_full,df_weekend_full)
  write.csv(df_full, file = paste(results_dir,filename, sep= "/"))
  

}



