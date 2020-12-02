# Temporary script

# Set dir
set_wdir <- function(){
  library(rstudioapi) 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}
# Set directory
set_wdir()

library(dplyr)
library(tidyr)

# sources
source("functions/data_cleaning.R")

input_dir <- "data/uncleaned/input/"
output_dir <- "data/uncleaned/cleaned-minutes/"


dir.create(file.path(output_dir), showWarnings = FALSE)

files <- list.files(input_dir)

for(file in files){
  data_raw <- read.csv(paste(input_dir,file, sep="/"),sep="\t",header=FALSE)
  data_cln <- cleanData(data_raw, hour=FALSE) # hour=FALSE -> minutes
  file_cln_name <- paste(file, "-minutes_cln.csv",sep="")
  write.csv(data_cln, paste(output_dir, file_cln_name))
}



# run all days together - merge data
input_dir <- "data/merge/hour-minute/train"
results_dir <- paste(input_dir,"merged-results",sep="/")
weekday_files <- list.files(paste(input_dir, "weekday",sep="/"))
weekend_files <- list.files(paste(input_dir, "weekend",sep="/"))
mergeData(input_dir, results_dir, weekday_files, weekend_files, "2weeks-nov17todec2-test.csv")
zxa# 5weeks-nov18todec22-test.csv 2weeks-nov1to17-train


# data_weekday_raw <- read.csv(paste('data',input_dir,"weekday",weekday_files[1], sep="/"),sep="\t",header=F)
# data_weekend_raw <- read.csv(paste('data',input_dir,"weekend",weekend_files[1], sep="/"),sep="\t",header=F)
# 
# data_weekday_cln <- cleanData(data_weekday_raw)
# data_weekend_cln <- cleanData(data_weekend_raw)
# 
# 
# write.csv(data_weekday_cln, "data/ml-training/cleaned/weekday/")
# write.csv(data_weekend_cln, "data/ml-training/cleaned/weekend/")
