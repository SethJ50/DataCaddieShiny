library(mongolite)

get_mongo_collection <- function(collection_name, db_name = "data_caddy") {
  docker_env_path <- "/srv/shiny-server/.Renviron"
  if (file.exists(docker_env_path)) {
    readRenviron(docker_env_path)
  }
  
  db_url <- Sys.getenv("MONGO_URL")
  print(db_url)
  
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
