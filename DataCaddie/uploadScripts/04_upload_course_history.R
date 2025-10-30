library(readxl)
library(dplyr)
source("uploadUtils/db_connection.R")

upload_course_history <- function() {
  df <- read_excel("data/course_history/courseHistory.xlsx")
  
  if(ncol(df) >= 6) {
    names(df)[2:6] <- paste0("minus", 1:5)
  } else {
    stop("The file must have at least 6 columns to rename")
  }
  
  col <- get_mongo_collection("coursehistories")
  col$remove('{}')
  col$insert(df)
  
  cat(paste0("✅ Course history uploaded successfully (", nrow(df), " rows)\n"))
}