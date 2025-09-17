library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(dplyr)

serverGeneratedLineups <- function(input, output, session, favorite_players,
                                   playersInTournament, playersInTournamentTourneyNameConv,
                                   playersInTournamentPgaNames, optimizerData) {

  
  # Setup Locked Players Input
  updatePickerInput(session, "locked_players", choices = optimizerData$player, selected = NULL)
  
  # Setup Player Pool Input
  updatePickerInput(session, "player_pool", choices = optimizerData$player, selected = optimizerData$player)
  
  # Observe Changes of Locked Player
  observeEvent(input$locked_players, {
    
    # Automatically set all locked players to selected
    current_pool <- input$player_pool %||% character(0)
    updated_pool <- union(current_pool, input$locked_players)
    
    updatePickerInput(session, "player_pool", choices = optimizerData$player, selected = updated_pool)
    
  })
  
  # Observe Generate Optimal Lineups Button Click - Generate Lineups
  observeEvent(input$generate_lineups_official, {
    # Get optimal lineups
    optimalLineupsDf <- generateOptimalLineups(optimizerData, input$num_lineups, input$locked_players, input$player_pool)
    
    # Get Ownership Dataframe
    ownershipDf <- makeOwnershipDf(optimalLineupsDf)

    # Populate Lineups Table
    output$optimizer_results_tbl <- renderReactable({
      makeLineupsTable(optimalLineupsDf)
    })
    
    # Populate Ownership Table
    output$optimizer_ownership_tbl <- renderReactable({
      makeOwnershipTable(ownershipDf)
    })
    
  })
  
}




# Supplementary Functions ------------------------------------------------------
makeLineupsTable <- function(optimalLineupsDf) {
  optimalLineupsDf <- optimalLineupsDf %>% 
    select(-lineup_id)
  
  table <- reactable(
    optimalLineupsDf,
    searchable = FALSE,
    resizable = TRUE,
    pagination = FALSE,
    showPageInfo = FALSE,
    onClick = NULL,
    outlined = TRUE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    fullWidth = TRUE,
    wrap = FALSE,
    defaultSortOrder = "desc",
    defaultSorted = "total_rating",
    showSortIcon = FALSE,
    theme = reactableTheme(
      style = list(fontSize = "12px"),
      rowStyle = list(height = "20px")
    ),
    defaultColDef = colDef(vAlign = "center", align = "center", width = 100),
    columns = list(
      player1 = colDef(
        name = "Player 1",
        width = 160
      ),
      player2 = colDef(
        name = "Player 2",
        width = 160
      ),
      player3 = colDef(
        name = "Player 3",
        width = 160
      ),
      player4 = colDef(
        name = "Player 4",
        width = 160
      ),
      player5 = colDef(
        name = "Player 5",
        width = 160
      ),
      player6 = colDef(
        name = "Player 6",
        width = 160
      ),
      total_salary = colDef(
        name = "Salary",
        width = 75
      ),
      total_rating = colDef(
        name = "Rating",
        width = 75
      )
    )
  )
  
  return(table)
}

makeOwnershipTable <- function(ownershipDf) {
  
  table <- reactable(
    ownershipDf,
    searchable = FALSE,
    resizable = TRUE,
    pagination = FALSE,
    showPageInfo = FALSE,
    onClick = NULL,
    outlined = TRUE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    fullWidth = TRUE,
    wrap = FALSE,
    defaultSortOrder = "desc",
    defaultSorted = "Ownership",
    showSortIcon = FALSE,
    theme = reactableTheme(
      style = list(fontSize = "15px"),
      rowStyle = list(height = "25px")
    ),
    defaultColDef = colDef(vAlign = "center", align = "center", width = 100),
    columns = list(
      Player = colDef(
        width = 260
      ),
      Ownership = colDef(
        width = 100
      )
    )
  )
  
  return(table)
}

generateOptimalLineups <- function(optimizerData, num_lineups, locked_players, player_pool, verbose = FALSE) {
  
  # Setup for Fantasy Platform
  if("fdSalary" %in% names(optimizerData)) {
    salary_col <- "fdSalary"
    salary_cap <- 60000
  } else if("dkSalary" %in% names(optimizerData)) {
    salary_col <- "dkSalary"
    salary_cap <- 50000
  } else {
    stop("ERROR: Optimizer Data must contain either 'fdSalary' or 'dkSalary'")
  }
  
  # If player pool is NULL, assume all players
  if (is.null(player_pool)) player_pool <- optimizerData$player
  
  # Locked players salary and count
  locked_players_data <- optimizerData %>% filter(player %in% locked_players)
  locked_salary <- sum(locked_players_data[[salary_col]])
  locked_count  <- nrow(locked_players_data)
  
  # Case: all 6 players are locked
  if (locked_count == 6) {
    # Sort locked players by descending salary
    sorted_locked <- locked_players_data %>% arrange(desc(.data[[salary_col]]))
    
    # Build single-row dataframe
    lineup_df <- data.frame(
      lineup_id    = 1,
      player1      = sorted_locked$player[1],
      player2      = sorted_locked$player[2],
      player3      = sorted_locked$player[3],
      player4      = sorted_locked$player[4],
      player5      = sorted_locked$player[5],
      player6      = sorted_locked$player[6],
      total_salary = sum(sorted_locked[[salary_col]]),
      total_rating = sum(sorted_locked$Rating),
      stringsAsFactors = FALSE
    )
    
    return(lineup_df)
  }
  
  # Adjust remaining salary and number of players to select
  remaining_salary <- salary_cap - locked_salary
  remaining_players <- 6 - locked_count
  
  # Filter optimizerData to only players in pool that are not already locked
  available_players <- optimizerData %>% 
    filter(player %in% player_pool, !(player %in% locked_players))
  
  # Limit num_lineups to number of unique lineups possible
  max_lineups <- choose(nrow(available_players), remaining_players)
  if (max_lineups == 0) {
    showNotification(
      "Not enough available players to form any lineup. Returning empty dataframe.",
      type = "warning",
      duration = 5
    )
    return(data.frame(
      lineup_id = integer(0),
      player1 = character(0),
      player2 = character(0),
      player3 = character(0),
      player4 = character(0),
      player5 = character(0),
      player6 = character(0),
      total_salary = numeric(0),
      total_rating = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  num_lineups <- min(num_lineups, max_lineups)
  
  lineups_df <- generateOptimalLineupsHelper(available_players, num_lineups, remaining_players, remaining_salary, salary_col, verbose)
  
  # Add locked players and sort by descending salary
  if (locked_count > 0) {
    for (i in seq_len(nrow(lineups_df))) {
      # Combine locked + selected players
      selected_players <- unlist(lineups_df[i, paste0("player", seq_len(remaining_players))])
      combined_players <- c(locked_players_data$player, selected_players)
      
      # Get salaries for combined players and sort descending
      combined_salaries <- optimizerData %>% filter(player %in% combined_players) %>% 
        arrange(desc(.data[[salary_col]]))
      
      # Assign sorted player names back to player1..player6
      lineups_df[i, paste0("player", seq_len(6))] <- combined_salaries$player
      
      # Recalculate totals
      lineups_df$total_salary[i] <- sum(combined_salaries[[salary_col]])
      lineups_df$total_rating[i] <- sum(combined_salaries$Rating)
    }
  }
  
  lineups_df <- lineups_df %>% 
    select(lineup_id, player1, player2, player3, player4, player5, player6, total_salary, total_rating)
  
  return(lineups_df)
}

generateOptimalLineupsHelper <- function(optimizerData, num_lineups = 5, num_players, salary_cap, salary_col, verbose = FALSE) {
  
  # Set Params
  n <- nrow(optimizerData)
  results <- list()
  
  # Create Initial Model
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n, type = "binary") %>% # Init Binary Variables
    add_constraint(sum_expr(x[i], i = 1:n) == num_players) %>% # Sum of 1's must equal players in lineup
    add_constraint(sum_expr(x[i] * optimizerData[[salary_col]][i], i = 1:n) <= salary_cap) %>% # Sum of Salary must be within salary cap
    set_objective(sum_expr(x[i] * optimizerData$Rating[i], i = 1:n), "max") # Set Optimization to maximize total rating
  
  for (k in seq_len(num_lineups)) {
    
    # Grab Result
    result <- solve_model(model, with_ROI(solver = "glpk", verbose = verbose))
  
    # Get Solution
    sol <- get_solution(result, x[i]) %>% filter(value > 0.5)
    selected <- optimizerData[sol$i, ]
    
    # Order Players by Salary (descending)
    selected <- selected[order(-selected[[salary_col]]), ]
    
    # Build lineup row
    player_cols <- setNames(as.list(selected$player), paste0("player", seq_len(num_players)))
    lineup_df <- data.frame(
      lineup_id    = k,
      player_cols,
      total_salary = sum(selected[[salary_col]]),
      total_rating = sum(selected$Rating),
      stringsAsFactors = FALSE
    )
    results[[k]] <- lineup_df
    
    # Add exclusion constraint: prevent the same lineup again
    idx <- sol$i
    model <- model %>%
      add_constraint(sum_expr(x[i], i = idx) <= num_players - 1)
  }
  
  return(do.call(rbind, results))
}

makeOwnershipDf <- function(optimalLineupsDf, num_players = 6) {
  player_cols <- paste0("player", seq_len(num_players))
  
  all_players <- unlist(optimalLineupsDf[, player_cols], use.names = FALSE)
  
  player_counts <- table(all_players)
  
  ownership_df <- data.frame(
    Player = names(player_counts),
    Ownership = round(as.numeric(player_counts) / nrow(optimalLineupsDf) * 100, 2),
    stringsAsFactors = FALSE
  )
  
  ownership_df <- ownership_df %>% 
    arrange(-Ownership)
  
  return(ownership_df)
}