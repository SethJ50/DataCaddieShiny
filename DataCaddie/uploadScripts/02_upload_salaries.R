library(readxl)
library(dplyr)
source("uploadUtils/db_connection.R")

# Manual corrections: DK name -> FD canonical name
name_corrections <- c(
  # "Name Last" = "Name Last Jr." DK -> FD
)

upload_salaries <- function() {
  # Read FanDuel, DraftKings salaries files
  fd <- read_excel("data/salaries/fanduel_salaries.xlsx") %>% 
    select(fd_player = Nickname, fdSalary = Salary)
  
  dk <- read_excel("data/salaries/draftkings_salaries.xlsx") %>% 
    select(dk_player = Name, dkSalary = Salary)
  
  dk <- dk %>% 
    mutate(dk_player = ifelse(dk_player %in% names(name_corrections),
                           name_corrections[dk_player],
                           dk_player))
  
  dk_orig <- dk
  
  salaries_df <- full_join(fd, dk, by = c("fd_player" = "dk_player"))
  
  unmatched <- salaries_df %>%
    filter(is.na(fdSalary) | is.na(dkSalary)) %>%
    # Create columns showing both FD and DK original names
    mutate(player_fd = fd_player,
           player_dk = ifelse(is.na(fd_player), dk_orig$dk_player[match(dkSalary, dk_orig$dkSalary)], NA)) %>%
    select(player_fd, player_dk, fdSalary, dkSalary)
  
  if(nrow(unmatched) > 0) {
    View(unmatched)  # opens a spreadsheet-like viewer
    stop("⚠️ There are unmatched players between FD and DK. Please resolve before uploading.")
  }
  
  final_df <- salaries_df %>%
    rename(player = fd_player)
  
  col <- get_mongo_collection("salaries")
  col$remove('{}')
  col$insert(final_df)
  
  cat("✅ Salaries uploaded successfully (", nrow(final_df), " rows)\n")
}
