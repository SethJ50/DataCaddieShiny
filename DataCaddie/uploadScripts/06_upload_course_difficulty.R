library(readxl)
library(dplyr)
source("uploadUtils/db_connection.R")

# File Name -> Pretty Name
name_conversion <- c(
  'ACCORDIA GOLF Narashino Country Club' = 'Accordia Golf Narashino Country Club',
  'Hamilton Golf &amp; Country Club' = 'Hamilton Golf and Country Club',
  "Arnold Palmer's Bay Hill Club &amp; Lodge" = "Arnold Palmer's Bay Hill Club & Lodge",
  'Hamilton Golf & Country Club' = 'Hamilton Golf and Country Club',
  'Pinehurst Resort & Country Club (Course No. 2)' = 'Pinehurst Resort and Country Club',
  "Arnold Palmer's Bay Hill Club & Lodge" = "Arnold Palmer's Bay Hill Club & Lodge"
)

upload_course_difficulty <- function(){
  df <- read_excel("data/course_difficulty/dg_course_table.xlsx")
  
  # XLSX Name = Mongo Name
  column_map <- c(
    "course" = "course",
    "adj_score_to_par" = "difficulty",
    "yardage" = "yardage"
  )
  
  missing_cols <- setdiff(names(column_map), names(df))
  if(length(missing_cols) > 0) {
    stop("Missing required columns in course difficulty file:\n", paste(missing_cols, collapse = ", "))
  }
  
  df <- df %>% rename(
    course = course,
    difficulty = adj_score_to_par
  )
  
  df$course <- ifelse(df$course %in% names(name_conversion),
                      name_conversion[df$course],
                      df$course)
  
  #df <- df %>% select(course, year, par, yardage,  difficulty)
  
  col <- get_mongo_collection("coursedifficulties")
  col$remove('{}')
  col$insert(df)
  
  cat(paste0("✅ Course difficulty uploaded successfully (", nrow(df), " rows)\n"))
}



