library(mongolite)

get_mongo_collection <- function(collection_name, db_name = "data_caddy") {
  if(file.exists(".Renviron")) {
    readRenviron(".Renviron")
  }
  
  username <- "admin"
  password <- Sys.getenv("ADMIN_PWD")
  host <- "localhost"
  port <- 27017
  auth_db <- "admin"
  
  db_url <- sprintf(
    "mongodb://%s:%s@%s:%s/%s?authSource=%s",
    username, password, host, port, db_name, auth_db
  )
  
  conn <- mongo(
    collection = collection_name,
    db = db_name,
    url = db_url
  )
  
  tryCatch({
    conn$count()
    cat(paste0("✅ Connected to MongoDB collection: ", collection_name, "\n"))
  }, error = function(e) {
    stop("❌ Connection failed: ", e$message)
  })
  
  return(conn)
}
