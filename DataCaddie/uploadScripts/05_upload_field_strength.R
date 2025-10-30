library(readxl)
library(dplyr)
source("uploadUtils/db_connection.R")

upload_field_strength <- function() {
  df <- read_excel("data/field_strength/StrengthOfField.xlsx")
  
  required_cols <- c("tournament", "year", "strength")
  missing_cols <- setdiff(required_cols, names(df))
  if(length(missing_cols) > 0) {
    stop("Missing required columns in field strength file:\n", paste(missing_cols, collapse = ", "))
  }
  
  col <- get_mongo_collection("fieldstrengths")
  col$remove('{}')
  col$insert(df)
  
  cat(paste0("✅ Field strength uploaded successfully (", nrow(df), " rows)\n"))
}
