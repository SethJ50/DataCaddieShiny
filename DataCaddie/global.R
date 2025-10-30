library(mongolite)

source("uploadUtils/db_connection.R")

#### DB Connection and Data

# Connect to local MongoDB
#db_url = "mongodb://localhost"

## Connect to Digital Ocean MongoDB
# ssh into droplet using the following: ssh -L 27017:localhost:27017 root@64.23.155.37
#db_url = "mongodb://localhost:27017"

# ---- DB Connections ----
tournament_conn <- get_mongo_collection("tournamentrows")
salaries_conn <- get_mongo_collection("salaries")
pgatour_conn <- get_mongo_collection("pgatours")
course_history_conn <- get_mongo_collection("coursehistories")
course_stats_conn <- get_mongo_collection("coursedifficulties")
field_strength_conn <- get_mongo_collection("fieldstrengths")

# ---- Load data once at app startup ----
data <- tournament_conn$find('{}')
salaries <- salaries_conn$find('{}')
pgaData <- pgatour_conn$find('{}')
courseHistoryData <- course_history_conn$find('{}')
courseStatsData <- course_stats_conn$find('{}')
fieldStrengthData <- field_strength_conn$find('{}')

