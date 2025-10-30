library(readxl)
library(dplyr)
source("uploadUtils/db_connection.R")

upload_pgatour <- function(){
  df <- read_excel("data/pgatour/pgatourStats.xlsx")
  names(df)
  
  required_cols <- c(
    "player", "sgPutt", "sgPuttRank", "sgArg", "sgArgRank", "sgApp", "sgAppRank", "sgOtt",
    "sgOttRank", "sgT2G", "sgT2GRank", "sgTot", "sgTotRank", "drDist", "drDistRank", "drAcc",
    "drAccRank", "gir", "girRank", "sandSave", "sandSaveRank", "scrambling", "scramblingRank", "app50_75",
    "app50_75Rank", "app75_100", "app75_100Rank", "app100_125", "app100_125Rank", "app125_150", "app125_150Rank",
    "app150_175", "app150_175Rank", "app175_200", "app175_200Rank", "app200_up", "app200_upRank", "bob", "bobRank",
    "bogAvd", "bogAvdRank", "par3Scoring", "par3ScoringRank", "par4Scoring", "par4ScoringRank", "par5Scoring",
    "par5ScoringRank", "prox", "proxRank", "roughProx", "roughProxRank", "puttingBob", "puttingBobRank",
    "threePuttAvd", "threePuttAvdRank", "bonusPutt", "bonusPuttRank"
  )
  missing_cols <- setdiff(required_cols, names(df))
  if(length(missing_cols) > 0) {
    stop("Missing required columns in PGATour file:\n", paste(missing_cols, collapse = ", "))
  }
  
  col <- get_mongo_collection("pgatours")
  col$remove('{}')
  col$insert(df)
  
  cat(paste0("✅ PGATour stats uploaded successfully (", nrow(df), " rows)\n"))
}
