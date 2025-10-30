library(readxl)
library(dplyr)
source("uploadUtils/db_connection.R")
source("uploadScripts/01_upload_tournament.R")
source("uploadScripts/02_upload_salaries.R")
source("uploadScripts/03_upload_pgatour.R")
source("uploadScripts/04_upload_course_history.R")
source("uploadScripts/05_upload_field_strength.R")
source("uploadScripts/06_upload_course_difficulty.R")

run_tournament <- TRUE

cat("🚀 Starting full upload pipeline...\n")

safe_upload <- function(func, name, ...) {
  tryCatch({
    func(...)
    cat(paste0("✅ ", name, " uploaded successfully.\n"))
    TRUE
  }, error = function(e) {
    cat(paste0("❌ Error in ", name, ": ", e$message, "\n"))
    FALSE
  })
}

# 1️⃣ Tournament rows (conditionally)
if(run_tournament) {
  # Find the most recent .xlsx file in data/tournament
  tournament_files <- list.files("data/tournament", pattern = "\\.xlsx$", full.names = TRUE)
  
  if(length(tournament_files) == 0) {
    stop("No tournament files found in data/tournament/")
  }
  
  # Pick the newest file by modification time
  latest_file <- tournament_files[which.max(file.info(tournament_files)$mtime)]
  cat(paste0("⏩ Uploading tournament rows from: ", latest_file, "\n"))
  
  safe_upload(upload_tournament, "tournament rows", file_path = latest_file)
} else {
  cat("⏭ Skipping tournament rows upload (run_tournament = FALSE)\n")
}

# 2️⃣ Salaries
safe_upload(upload_salaries, "salaries")

# 3️⃣ PGATour stats
safe_upload(upload_pgatour, "PGATour stats")

# 4️⃣ Course history
safe_upload(upload_course_history, "course history")

# 5️⃣ Field strength
safe_upload(upload_field_strength, "field strength")

# 6️⃣ Course difficulty
safe_upload(upload_course_difficulty, "course difficulty")

cat("🚀 Upload pipeline finished.\n")