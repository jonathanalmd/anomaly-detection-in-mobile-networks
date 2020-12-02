# Set dir
set_wdir <- function(){
  library(rstudioapi) 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}
# Set directory
set_wdir()
if (!require("pacman")) install.packages("pacman")
pacman::p_load("rgdal", "geojsonio", "sp", "maps", "ggmap", "maptools")

data_file <- "data/milano-grid.geojson"
data <- geojson_read(data_file, what = "sp", parse=TRUE)

library("rjson")

# Give the input file name to the function.
result <- fromJSON(file = data_file)
