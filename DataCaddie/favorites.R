library(dplyr)
source("utils.R")

serverFavorites <- function(input, output, session, favorite_players,
                              playersInTournament, playersInTournamentTourneyNameConv,
                              playersInTournamentPgaNames) {
  
  table_data <- salaries %>% 
    filter(player %in% playersInTournament) %>% 
    select(player, fdSalary, dkSalary) %>% 
    mutate(Player = player)
  
  favs <- favorite_players$names
  
  table_data <- table_data %>% 
    mutate(
      isFavorite = player %in% favs
    )
  
  # Create .favorite column to hold star	
  table_data$.favorite <- NA
  
  # Rearrange .favorite to be the first column
  table_data <- table_data[, c(".favorite", setdiff(names(table_data), ".favorite"))]
  
  table_data <- table_data %>% 
    filter(isFavorite == TRUE) %>% 
    select(-player) %>% 
    select(isFavorite, `.favorite`, Player, fdSalary, dkSalary)
  
  table_cols <- list(
    Player = "Player", fdSalary = "FD Salary", dkSalary = "DK Salary"
  )
  
  # Make column defs
  col_defs <- lapply(names(table_cols), function(col) {
    norm_data <- NULL
    col_func <- NULL
    
    col_inner <- table_cols[[col]]
    
    width <- case_when(
      col_inner %in% c("Player") ~ 200,
      TRUE ~ 75
    )
    
    makeColDef(
      col_name = col,
      display_name = col_inner,
      width = width,
      color_func = col_func,
      norm_data = norm_data,
      header_size = 12
    )
  })
  
  names(col_defs) <- names(table_cols)
  
  col_defs[["isFavorite"]] <- colDef(show = FALSE)
  
  output$favorites_table <- renderReactable({
    makeBasicTable(table_data, col_defs, "fdSalary", favorite_players, hasFavorites = TRUE)
  })
}