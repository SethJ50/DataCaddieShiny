library(mongolite)

get_mongo_collection <- function(collection_name, db_name = "data_caddy") {
  if(file.exists(".Renviron")) {
    readRenviron(".Renviron")
  }
  
  db_url <- Sys.getenv("MONGO_ATLAS_URL")
  
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
