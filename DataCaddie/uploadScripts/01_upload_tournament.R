library(readxl)
library(dplyr)
source("uploadUtils/db_connection.R")

upload_tournament <- function(file_path) {
  df <- read_excel(file_path)
  
  column_map <- c(
    "Player" = "player",
    "Finish" = "finish",
    "Tournament" = "tournament",
    "Course" = "course",
    "Dates" = "dates",
    "Round" = "Round",
    "SG: Off The Tee" = "sgOtt",
    "SG: Approach to Green" = "sgApp",
    "SG: Around the Green" = "sgArg",
    "SG: Putting" = "sgPutt",
    "SG: Tee to Green" = "sgT2G",
    "SG: Total" = "sgTot",
    "Driving Accuracy" = "drAcc",
    "Driving Distance" = "drDist",
    "Longest Drive" = "longDr",
    "Greens in Regulation" = "gir",
    "Sand Saves" = "sandSaves",
    "Scrambing" = "scrambling",
    "Putts per GIR" = "puttsPerGir",
    "Total Putts" = "totPutts",
    "Eagles" = "eagles",
    "Birdies" = "birdies",
    "Pars" = "pars",
    "Bogeys" = "bogeys",
    "Double Bogeys" = "doubleBogeys",
    "Other" = "other"
  )
  missing_cols <- setdiff(names(column_map), names(df))
  if(length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required columns in the Excel file:\n",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  df <- df %>% rename_with(~ column_map[.x], .cols = names(column_map))
  
  col <- get_mongo_collection("tournamentrows")
  
  col$insert(df)
  cat(paste0("✅ TournamentRow uploaded successfully (", nrow(df), " rows)\n"))
}
