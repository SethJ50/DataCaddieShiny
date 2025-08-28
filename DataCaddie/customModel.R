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

modelStatsOptions <- c(
  "Display" = "real",
  "SG: PUTT PGA" = "sgPuttPga",
  "SG: ARG PGA" = "sgArgPga",
  "SG: APP PGA" = "sgAppPga",
  "SG: OTT PGA" = "sgOttPga",
  "SG: T2G PGA" = "sgT2GPga",
  "SG: TOT PGA" = "sgTotPga",
  
  "SG: PUTT L12" = "sgPuttL12",
  "SG: ARG L12" = "sgArgL12",
  "SG: APP L12" = "sgAppL12",
  "SG: OTT L12" = "sgOttL12",
  "SG: T2G L12" = "sgT2GL12",
  "SG: TOT L12" = "sgTotL12",
  
  "SG: PUTT L24" = "sgPuttL24",
  "SG: ARG L24" = "sgArgL24",
  "SG: APP L24" = "sgAppL24",
  "SG: OTT L24" = "sgOttL24",
  "SG: T2G L24" = "sgT2GL24",
  "SG: TOT L24" = "sgTotL24",
  
  "SG: PUTT L36" = "sgPuttL36",
  "SG: ARG L36" = "sgArgL36",
  "SG: APP L36" = "sgAppL36",
  "SG: OTT L36" = "sgOttL36",
  "SG: T2G L36" = "sgT2GL36",
  "SG: TOT L36" = "sgTotL36",
  
  "SG: PUTT L50" = "sgPuttL50",
  "SG: ARG L50" = "sgArgL50",
  "SG: APP L50" = "sgAppL50",
  "SG: OTT L50" = "sgOttL50",
  "SG: T2G L50" = "sgT2GL50",
  "SG: TOT L50" = "sgTotL50",
  
  "DR. DIST" = "drDist",
  "DR. ACC" = "drAcc",
  "BOB%" = "bob%",
  "BOG AVD" = "bogAvd",
  "SAND SAVE %" = "sandSave%",
  "SCRAMBLING" = "scrambling",
  "PAR 3 AVG" = "par3Avg",
  "PAR 4 AVG" = "par4Avg",
  "PAR 5 AVG" = "par5Avg",
  "GIR %" = "gir%",
  "PROXIMITY" = "prox",
  "ROUGH PROX" = "roughProx",
  
  "APP 50-75" = "app50_75",
  "APP 75-100" = "app75_100",
  "APP 100-125" = "app100_125",
  "APP 125-150" = "app125_150",
  "APP 150-175" = "app150_175",
  "APP 175-200" = "app175_200",
  "APP 200+" = "app200",
  
  "BONUS PUTT" = "bonusPutt",
  "3 PUTT AVD" = "threePuttAvd",
  
  "SG EASY FIELD" = "sgEasyField",
  "SG MEDIUM FIELD" = "sgMedField",
  "SG HARD FIELD" = "sgHardField",
  
  "SG EASY COURSE" = "sgEasyCourse",
  "SG MEDIUM COURSE" = "sgMedCourse",
  "SG HARD COURSE" = "sgHardCourse",
  
  "COURSE HISTORY" = "courseHistory"
)

serverCustomModel <- function(input, output, session, favorite_players,
                              playersInTournament, playersInTournamentTourneyNameConv,
                              playersInTournamentPgaNames) {
  
  # Create reactive values for weights of the model
  rv_weights <- reactiveValues()
  
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
  
  # Observe changes of weights (save) and clicks of remove stat button
  observe({
    req(input$modelStatPicker)
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
  
  # Reactive total weight as model weights are added
  totalWeight <- reactive({
    weight_ids <- grep("^weight_", names(input), value = TRUE)
    
    vals <- vapply(weight_ids, function(id) {
      val <- input[[id]]
      if (is.null(val) || val == "") {
        0
      } else {
        suppressWarnings(as.numeric(val))
      }
    }, numeric(1))
    
    sum(vals, na.rm = TRUE)
  })
  
  # Update totalWeight text output upon model input changes
  output$totalWeight <- renderText({
    totalWeight()
  })
  
  # Observe 'Submit' Button Press
  observeEvent(input$submitModel, {
    req(input$modelStatPicker)
    
    weights <- sapply(input$modelStatPicker, function(stat) {
      val <- rv_weights[[stat]]
      if (is.null(val) || val == "") 0 else as.numeric(val)
    }, USE.NAMES = TRUE)
    
    weights_df <- data.frame(
      stat = input$modelStatPicker,
      weight = as.numeric(weights),
      stringsAsFactors = FALSE
    )
    
    print(weights_df)
    
    # Grab dataframe full of player's data for all stats
    model_data <- getDataForModel(playersInTournament)
    
    # Grab only the model stats, normalized versions for each value,
    #     alongside player names, salaries on fd and dk
    #     note, for model data sgPuttL36 = sgPutt_36 and sgPutt_norm_36 (normalized)
    player_cols <- c("player", "fdSalary", "dkSalary")
    model_stats <- weights_df$stat
    
    roundSgStats <- c("sgPuttL12", "sgArgL12", "sgAppL12", "sgOttL12", "sgT2GL12", "sgTotL12",
                      "sgPuttL24", "sgArgL24", "sgAppL24", "sgOttL24", "sgT2GL24", "sgTotL24",
                      "sgPuttL36", "sgArgL36", "sgAppL36", "sgOttL36", "sgT2GL36", "sgTotL36",
                      "sgPuttL50", "sgArgL50", "sgAppL50", "sgOttL50", "sgT2GL50", "sgTotL50",
                      )
    
    selected_cols <- player_cols
    
    for (stat in model_stats) {
      if (stat %in% roundSgStats) {
        # For round-based stats: grab stat_N and stat_norm_N
        raw_col <- paste0(stat, "_N")
        norm_col <- paste0(stat, "_norm_N")
      } else {
        # For all other stats: grab stat and stat_norm
        raw_col <- stat
        norm_col <- paste0(stat, "_norm")
      }
      
      # Only include columns that exist in model_data
      if (raw_col %in% names(model_data)) selected_cols <- c(selected_cols, raw_col)
      if (norm_col %in% names(model_data)) selected_cols <- c(selected_cols, norm_col)
    }
    
    model_stats_df <- model_data[, selected_cols, drop = FALSE]
    
    # Create weighted average of the stats based on input weights using norm stat values
    
    # Based on this weighted average, get a 'Rating' - the percentile of each weighted
    #   average based on standard normal distribution
    
    # Save this data as a dataframe
  })
}









#### Supplementary Functions ---------------------------------------------------
getDataForModel <- function(playersInTournament) {
  last12SGData <- getLastNSg(data, playersInTournament, 12)
  last24SGData <- getLastNSg(data, playersInTournament, 24)
  last36SGData <- getLastNSg(data, playersInTournament, 36)
  last50SGData <- getLastNSg(data, playersInTournament, 50)
  
  fav_players <- c() # TODO
  all_last12 <- get_all_player_data(fav_players, playersInTournament, 12)
  all_last12 <- all_last12$data
  all_last24 <- get_all_player_data(fav_players, playersInTournament, 24)
  all_last24 <- all_last24$data
  all_last36 <- get_all_player_data(fav_players, playersInTournament, 36)
  all_last36 <- all_last36$data
  all_last50 <- get_all_player_data(fav_players, playersInTournament, 50)
  all_last50 <- all_last50$data
  
  # For all_lastN for N = 12, 24, 36, and 50 rename the columns in
  #   (sgPutt, sgArg, sgApp, sgOtt, sgT2G, sgTot), and each of these with "_norm" at the end
  #   to append N to the end of each of their names.
  
  # columns to rename
  sg_cols <- c("sgPutt", "sgArg", "sgApp", "sgOtt", "sgT2G", "sgTot")
  sg_cols_norm <- paste0(sg_cols, "_norm")
  cols_to_rename <- c(sg_cols, sg_cols_norm)
  
  rename_with_suffix <- function(df, N) {
    df %>% 
      rename_with(.fn = ~ paste0(.x, "_", N), .cols = all_of(cols_to_rename))
  }
  
  all_last12 <- rename_with_suffix(all_last12, 12) %>%
    select(player, ends_with("_12"))
  all_last24 <- rename_with_suffix(all_last24, 24) %>%
    select(player, ends_with("_24"))
  all_last36 <- rename_with_suffix(all_last36, 36) %>%
    select(player, ends_with("_36"))
  all_last50 <- rename_with_suffix(all_last50, 50)
  
  # For all_lastN for N = 12, 24, 36, only grab those columns name changed...
  # Join all of these with all_last50 by 'player'
  final_data <- all_last50 %>%
    left_join(all_last12, by = "player") %>%
    left_join(all_last24, by = "player") %>%
    left_join(all_last36, by = "player")
  
  return(final_data)
}














