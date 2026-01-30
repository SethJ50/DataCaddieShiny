library(shiny)
library(bslib)
library(reactable)
library(mongolite)
library(tidyverse)
library(shinyWidgets)
library(htmltools)
library(ggplot2)
library(ggrepel)
library(plotly)
library(showtext)
library(reactable)
library(reactable.extras)

source("utils.R")
source("playersDataFunctions.R")
source("generatedLineups.R")
source("basics.R")

modelStatsOptions <- c(
  "SG: PUTT PGA" = "SG Putt PGA",
  "SG: ARG PGA" = "SG Arg PGA",
  "SG: APP PGA" = "SG App PGA",
  "SG: OTT PGA" = "SG Ott PGA",
  "SG: T2G PGA" = "SG T2G PGA",
  "SG: TOT PGA" = "SG Tot PGA",
  
  "SG: PUTT L12" = "sgPutt_12",
  "SG: ARG L12" = "sgArg_12",
  "SG: APP L12" = "sgApp_12",
  "SG: OTT L12" = "sgOtt_12",
  "SG: T2G L12" = "sgT2G_12",
  "SG: TOT L12" = "sgTot_12",
  
  "SG: PUTT L24" = "sgPutt_24",
  "SG: ARG L24" = "sgArg_24",
  "SG: APP L24" = "sgApp_24",
  "SG: OTT L24" = "sgOtt_24",
  "SG: T2G L24" = "sgT2G_24",
  "SG: TOT L24" = "sgTot_24",
  
  "SG: PUTT L36" = "sgPutt_36",
  "SG: ARG L36" = "sgArg_36",
  "SG: APP L36" = "sgApp_36",
  "SG: OTT L36" = "sgOtt_36",
  "SG: T2G L36" = "sgT2G_36",
  "SG: TOT L36" = "sgTot_36",
  
  "SG: PUTT L50" = "sgPutt_50",
  "SG: ARG L50" = "sgArg_50",
  "SG: APP L50" = "sgApp_50",
  "SG: OTT L50" = "sgOtt_50",
  "SG: T2G L50" = "sgT2G_50",
  "SG: TOT L50" = "sgTot_50",
  
  "DR. DIST" = "Dr. Dist",
  "DR. ACC" = "Dr. Acc",
  "BOB%" = "BOB%",
  "BOG AVD" = "Bog. Avd",
  "SAND SAVE %" = "SandSave%",
  "SCRAMBLING" = "Scrambling",
  "PAR 3 AVG" = "Par 3 Score",
  "PAR 4 AVG" = "Par 4 Score",
  "PAR 5 AVG" = "Par 5 Score",
  "GIR %" = "GIR%",
  "PROXIMITY" = "Proximity",
  "ROUGH PROX" = "Rough Prox.",
  
  "APP 50-75" = "App. 50-75",
  "APP 75-100" = "App. 75-100",
  "APP 100-125" = "App. 100-125",
  "APP 125-150" = "App. 125-150",
  "APP 150-175" = "App. 150-175",
  "APP 175-200" = "App. 175-200",
  "APP 200+" = "App. 200_up",
  
  "BONUS PUTT" = "Bonus Putt.",
  "3 PUTT AVD" = "3 Putt Avd",
  
  "SG EASY FIELD" = "SG Easy Field Adjusted",
  "SG MEDIUM FIELD" = "SG Medium Field Adjusted",
  "SG HARD FIELD" = "SG Hard Field Adjusted",
  
  "SG EASY COURSE" = "SG Easy Course Adjusted",
  "SG MEDIUM DIFF COURSE" = "SG Medium Course Adjusted",
  "SG HARD COURSE" = "SG Hard Course Adjusted",
  
  "SG SHORT COURSE" = "SG Short Course Adjusted",
  "SG MEDIUM LEN COURSE" = "SG Medium Length Course Adjusted",
  "SG LONG COURSE" = "SG Long Course Adjusted",
  
  "SG SHORT AVG DRIVE COURSE" = "SG Short Driver Courses Adjusted",
  "SG MEDIUM AVG DRIVE COURSE" = "SG Medium Driver Courses Adjusted",
  "SG LONG AVG DRIVE COURSE" = "SG Long Driver Courses Adjusted",
  
  "SG LOW AVG ACC COURSE" = "SG Low Accuracy Courses Adjusted",
  "SG MEDIUM AVG ACC COURSE" = "SG Medium Accuracy Courses Adjusted",
  "SG HIGH AVG ACC COURSE" = "SG High Accuracy Courses Adjusted",
  
  "SG NARROW FAIRWAYS" = "SG Narrow Fairways Adjusted",
  "SG MEDIUM FAIRWAYS" = "SG Medium Fairways Adjusted",
  "SG WIDE FAIRWAYS" = "SG Wide Fairways Adjusted",
  
  "SG LOW MISS FAIRWAY PENALTY" = "SG Low Miss Penalty Adjusted",
  "SG MEDIUM MISS FAIRWAY PENALTY" = "SG Medium Miss Penalty Adjusted",
  "SG HIGH MISS FAIRWAY PENALTY" = "SG High Miss Penalty Adjusted",
  
  "SG EASY OTT COURSE" = "SG Easy OTT Courses Adjusted",
  "SG MEDIUM OTT COURSE" = "SG Medium OTT Courses Adjusted",
  "SG HARD OTT COURSE" = "SG Hard OTT Courses Adjusted",
  
  "SG EASY APP COURSE" = "SG Easy APP Courses Adjusted",
  "SG MEDIUM APP COURSE" = "SG Medium APP Courses Adjusted",
  "SG HARD APP COURSE" = "SG Hard APP Courses Adjusted",
  
  "SG EASY ARG COURSE" = "SG Easy ARG Courses Adjusted",
  "SG MEDIUM ARG COURSE" = "SG Medium ARG Courses Adjusted",
  "SG HARD ARG COURSE" = "SG Hard ARG Courses Adjusted",
  
  "SG EASY PUTT COURSE" = "SG Easy Putting Courses Adjusted",
  "SG MEDIUM PUTT COURSE" = "SG Medium Putting Courses Adjusted",
  "SG HARD PUTT COURSE" = "SG Hard Putting Courses Adjusted",
  
  "COURSE HISTORY" = "Course History"
)

serverCustomModel <- function(input, output, session, favorite_players,
                              playersInTournament, playersInTournamentTourneyNameConv,
                              playersInTournamentPgaNames) {
  
  # Create a reactive 'dictionary' of weights for model
  rv_weights <- session$userData$sessionModelWeights
  
  # Render Model Stats as Input Boxes below box dropdown
  output$selectedModelStats <- renderUI({
    req(input$modelStatPicker)
    
    tagList(
      lapply(input$modelStatPicker, function(stat) {
        
        weight_val <- if(!is.null(rv_weights[[stat]])) rv_weights[[stat]] else 0
        
        div(
          class = "weight-stat-row",
          fluidRow(
            style = "display: flex; align-items: center; justify-content: space-between;",
            column(8, 
                   div(
                     class = "weight-stat-label",
                     strong(names(modelStatsOptions)[modelStatsOptions == stat])
                   )
            ),
            column(4,
                   div(
                     class = "weight-input",
                     style = "display: flex; align-items: center;",
                     numericInput(
                       inputId = paste0("weight_", stat),
                       label = NULL,
                       value = weight_val,
                       step = 1,
                       width = "80px"
                     ),
                     actionButton(
                       inputId = paste0("remove_", stat),
                       label = "ⓧ",
                       style = "margin-left: 2px; background-color: #fafafa; padding: 2px 2px; min-width: 0; border: none"
                     )
                   )
            )
          )
        )
      })
    )
  })
  
  # On a change of the modelStatPicker dropdown
  observe({
    req(input$modelStatPicker)
    
    # For each stat in the stat picker
    lapply(input$modelStatPicker, function(stat) {
      local({
        s <- stat
        
        # On change of a weight, store the weights for future use
        input_id <- paste0("weight_", s)
        observeEvent(input[[input_id]], {
          rv_weights[[s]] <- input[[input_id]]
        }, ignoreNULL = FALSE)
        
        # On clicks of a remove button, remove the stat from model stat picker
        btn_id <- paste0("remove_", s)
        observeEvent(input[[btn_id]], {
          # Remove the stat from pickerInput
          updated <- setdiff(input$modelStatPicker, s)
          updatePickerInput(session, "modelStatPicker", selected = updated)
        }, ignoreInit = TRUE)
      })
    })
  })
  
  # Create weights dataframe on change of weights
  weights_df <- reactive({
    data.frame(
      stat = input$modelStatPicker,
      weight = sapply(input$modelStatPicker, function(stat) {
        val <- rv_weights[[stat]]
        if (is.null(val) || val == "") 0 else as.numeric(val)
      }, USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
  })
  
  # On change of modelStatPicker, update totalWeight sum
  totalWeight <- reactive({
    req(input$modelStatPicker)
    sum(weights_df()$weight, na.rm = TRUE)
  })
  
  # Place totalWeight sum in output
  output$totalWeight <- renderText({
    totalWeight()
  })
  
  # Get dataframe full of player's data for all stats
  model_data <- getDataForModel(playersInTournament)
  
  # Model table data reactive on change of model stats
  modelTableData <- reactive({
    
    # Grab Player Name, Salaries on each site, and model stats and normalized versions
    player_cols <- c("player", "fdSalary", "dkSalary")
    model_stats <- weights_df()$stat
    selected_cols <- player_cols
    
    for (stat in model_stats) {
      raw_col <- stat
      norm_col <- paste0(stat, "_norm")
      
      # Only include columns that exist in model_data
      if (raw_col %in% names(model_data)) selected_cols <- c(selected_cols, raw_col)
      if (norm_col %in% names(model_data)) selected_cols <- c(selected_cols, norm_col)
    }
    
    model_stats_df <- model_data[, selected_cols, drop = FALSE]
    
    
    # Create weighted average of the stats based on input weights using norm stat values
    norm_cols <- paste0(model_stats, "_norm")
    norm_cols <- norm_cols[norm_cols %in% names(model_stats_df)]
    
    weights_vec <- setNames(weights_df()$weight, paste0(weights_df()$stat, "_norm"))
    weights_vec <- weights_vec[norm_cols]
    
    model_stats_df$weighted_avg <- apply(
      model_stats_df[, norm_cols, drop = FALSE], 1,
      function(row) {
        if (sum(weights_vec) == 0) return(NA)
        sum(row * weights_vec, na.rm = TRUE) / sum(weights_vec)
      }
    )
    
    # Based on this weighted average, get a 'Rating' - the percentile of each weighted
    #   average based on standard normal distribution
    model_stats_df$Rating <- round(pnorm(model_stats_df$weighted_avg) * 100, 1)
    
    # Add Favorites
    favs <- favorite_players$names
    
    model_stats_df <- model_stats_df %>% 
      mutate(
        isFavorite = player %in% favs
      )
    
    model_stats_df$.favorite <- NA
    model_stats_df <- model_stats_df[, c(".favorite", setdiff(names(model_stats_df), ".favorite"))]
    
    model_stats_df
  })
  
  # On 'Submit' Button Press, update table
  observeEvent(input$submitModel, {
    
    # Use model stats to populate table UI
    updateModelTable(modelTableData(), output, favorite_players, input$model_platform)
    
  })
  
  # On Platform dropdown change, update table
  observeEvent(input$model_platform, {
    updateModelTable(modelTableData(), output, favorite_players, input$model_platform)
  }, ignoreInit = TRUE)
  
  # On player favorited, update table
  observeEvent(input$favorite_clicked, {
    updateModelTable(modelTableData(), output, favorite_players, input$model_platform)
  }, ignoreInit = TRUE)
  
  # On Generate Lineups Buttons Clicked, Open Generated Lineups Page and send data to it
  observeEvent(input$generate_lineups, {
    session$userData$optimizerData <- getOptimizerData(modelTableData(), output, favorite_players, input$model_platform)
    
    updateNavbarPage(session, "siteTabs", "Generated Lineups")
  }, ignoreInit = TRUE)
  
}









#### Supplementary Functions ---------------------------------------------------
getOptimizerData <- function(model_stats, output, favorite_players, platform) {
  model_stats <- as.data.frame(model_stats)
  
  if (platform == "FanDuel") {
    grab_platform <- c("fdSalary")
  } else { # DraftKings
    grab_platform <- c("dkSalary")
  }
  
  optimizer_data <- model_stats[, c("player", grab_platform, "Rating", "isFavorite"), drop = FALSE]
  
  return(optimizer_data)
}

updateModelTable <- function(model_stats, output, favorite_players, platform) {
  
  model_stats <- as.data.frame(model_stats)
  
  model_output_data <- model_stats %>% 
    select(-matches("_norm$"), -weighted_avg) %>% 
    mutate(
      `FD Value` = round(1000 * (Rating / fdSalary), 2),
      `DK Value` = round(1000 * (Rating / dkSalary), 2),
    )
  
  if (platform == "FanDuel") {
    fixed_cols <- c("player", "fdSalary", "FD Value", "Rating")
    exclude_cols <- c("dkSalary", "DK Value")
  } else { # DraftKings
    fixed_cols <- c("player", "dkSalary", "DK Value", "Rating")
    exclude_cols <- c("fdSalary", "FD Value")
  }
  
  remaining_cols <- setdiff(names(model_output_data), c(fixed_cols, exclude_cols))
  model_output_data <- model_output_data[, c(fixed_cols, remaining_cols), drop = FALSE]
  
  if (platform == "FanDuel") {
    model_output_data <- model_output_data %>%
      rename(
        "Player" = player,
        "FD Salary" = fdSalary
      )
  } else { # DraftKings
    model_output_data <- model_output_data %>%
      rename(
        "Player" = player,
        "DK Salary" = dkSalary
      )
  }
  
  display_names <- setNames(names(modelStatsOptions), modelStatsOptions)
  
  # Make Cell Coloring Functions
  getColor <- makeColorFunc(min_val = -3, max_val = 3)
  getRatingColor <- makeColorFunc(min_val = 0, max_val = 100)
  
  model_output_data <- model_output_data[, c(".favorite", setdiff(names(model_output_data), ".favorite"))]
  
  col_defs <- lapply(names(model_output_data), function(col) {
    
    display_col <- ifelse(col %in% names(display_names), display_names[[col]], col)
    
    # Primary columns
    if (col %in% c("Player", "FD Salary", "FD Value", "DK Salary", "DK Value", "Rating")) {
      width_val <- switch(col,
                          "Player" = 200,
                          "Rating" = 80,
                          100)
      
      color_func <- if (col == "Rating") getRatingColor else NULL
      
      makeColDef(col, display_name = display_col, width = width_val,
                 color_func = color_func, norm_data = NULL)
    } else {
      # Other stat columns: color by _norm
      norm_col <- paste0(col, "_norm")
      norm_data <- if (norm_col %in% names(model_stats)) {
        model_stats[[norm_col]]
      } else {
        rep(NA_real_, nrow(model_output_data))
      }
      
      makeColDef(col, display_name = display_col, width = 100,
                 color_func = getColor, norm_data = norm_data)
    }
  })
  
  names(col_defs) <- names(model_output_data)
  
  output$model_output <- renderReactable({
    makeBasicTable(model_output_data, col_defs, "Rating", favorite_players, hasFavorites = TRUE)
  })
}

getDataForModel <- function(playersInTournament) {
  last12SGData <- getLastNSg(data, playersInTournament, 12)
  last24SGData <- getLastNSg(data, playersInTournament, 24)
  last36SGData <- getLastNSg(data, playersInTournament, 36)
  last50SGData <- getLastNSg(data, playersInTournament, 50)
  
  fav_players <- c() # TODO
  all_last12 <- get_all_player_data(fav_players, playersInTournament, 12)$data
  all_last24 <- get_all_player_data(fav_players, playersInTournament, 24)$data
  all_last36 <- get_all_player_data(fav_players, playersInTournament, 36)$data
  all_last50 <- get_all_player_data(fav_players, playersInTournament, 50)$data
  
  # Get Levels Data (Field Strength, Course Attrs) and Add Norms
  courseDiffData <- getAllLevelsData(playersInTournament, data, 100)
  courseDiffData <- add_normalized_columns(courseDiffData)
  
  # columns to rename
  sg_cols <- c("sgPutt", "sgArg", "sgApp", "sgOtt", "sgT2G", "sgTot")
  sg_cols_norm <- paste0(sg_cols, "_norm")
  cols_to_rename <- c(sg_cols, sg_cols_norm)
  
  # Custom renamer
  rename_with_suffix <- function(df, N) {
    df %>%
      rename_with(
        .fn = function(cols) {
          # vectorized handling of column names
          case_when(
            cols %in% sg_cols ~ paste0(cols, "_", N),
            cols %in% sg_cols_norm ~ gsub("_norm$", paste0("_", N, "_norm"), cols),
            TRUE ~ cols
          )
        },
        .cols = all_of(cols_to_rename)
      )
  }
  
  all_last12 <- rename_with_suffix(all_last12, 12) %>%
    select(player, matches("_(12|12_norm)$"))
  
  all_last24 <- rename_with_suffix(all_last24, 24) %>%
    select(player, matches("_(24|24_norm)$"))
  
  all_last36 <- rename_with_suffix(all_last36, 36) %>%
    select(player, matches("_(36|36_norm)$"))
  
  all_last50 <- rename_with_suffix(all_last50, 50)
  
  # Join them all
  final_data <- all_last50 %>%
    left_join(all_last12, by = "player") %>%
    left_join(all_last24, by = "player") %>%
    left_join(all_last36, by = "player") %>% 
    left_join(courseDiffData, by = "player")
  
  return(final_data)
}