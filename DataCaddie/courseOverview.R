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

source("utils.R")
source("playersDataFunctions.R")

serverCourseOverview <- function(input, output, session, favorite_players,
                                 playersInTournament, playersInTournamentTourneyNameConv,
                                 playersInTournamentPgaNames) {
  
  # Get Round by Round Data
  round_by_round <- data %>%
    filter(Round != "Event") %>% 
    select(course, dates, eagles, birdies, pars, bogeys, doubleBogeys) %>% 
    group_by(course, dates) %>% 
    summarise(
      avg_eagles = mean(eagles, na.rm = TRUE),
      avg_birdies = mean(birdies, na.rm = TRUE),
      avg_pars = mean(pars, na.rm = TRUE),
      avg_bogeys = mean(bogeys, na.rm = TRUE),
      avg_doubleBogeys = mean(doubleBogeys, na.rm = TRUE)
    )

  # Get Data for Course Stats Table
  table_data <- makeCourseStatsTableData(courseStatsData, round_by_round, input$co_course)
  
  # Render Stat Summary Table
  output$coStatsTable <- renderReactable({
    makeCategoryDropdownTable(table_data)
  })
}



makeCourseStatsTableData <- function(all_data, round_data, curr_course) {
  
  # Tourney Name Corrections
  name_conversion <- c(
    'ACCORDIA GOLF Narashino Country Club' = 'Accordia Golf Narashino Country Club',
    'Hamilton Golf & Country Club' = 'Hamilton Golf and Country Club',
    'Pinehurst Resort & Country Club (Course No. 2)' = 'Pinehurst Resort and Country Club',
    "Arnold Palmer's Bay Hill Club & Lodge" = "Arnold Palmer's Bay Hill Club & Lodge"
  )
  
  curr_course <- ifelse(
    curr_course %in% names(name_conversion),
    name_conversion[curr_course],
    curr_course
  )
  
  # Map Internal Names to Stat Labels
  stat_labels <- c(
    # Overview
    par = "Par",
    yardage = "Course Length",
    yardage_4_5 = "Avg. Par 4/5 Length",
    yardage_3 = "Avg. Par 3 Length",
    
    # Scoring
    difficulty = "Avg. Adj. Score to Par",
    adj_par_3_score = "Avg. Adj. Par 3 STP",
    adj_par_4_score = "Avg. Adj. Par 4 STP",
    adj_par_5_score = "Avg. Adj. Par 5 STP",
    avg_eagles = "Avg. Eagles",
    avg_birdies = "Avg. Birdies",
    avg_pars = "Avg. Pars",
    avg_bogeys = "Avg. Bogeys",
    avg_doubleBogeys = "Avg. Double Bogeys",
    adj_penalties = "Penalties/Rd.",
    adj_ob = "Reloads/Rd",
    
    # Off-the-Tee
    adj_driving_distance = "Avg. Dr. Distance",
    adj_sd_distance = "Std Dev. Dr. Distance",
    adj_driving_accuracy = "Avg. Driving Accuracy",
    ott_sg = "Stroke Ease: OTT",
    fw_width = "Avg. Fairway Width",
    fw_diff = "Missed Fairway Penalty",
    rgh_diff = "Rough vs. Fairway Diff.",
    non_rgh_diff = "Non-Rough vs. Fairway Diff",
    miss_fw_pen_frac = "Missed Fairway Penalty %",
    
    # Approach
    app_sg = "Stroke Ease: APP",
    adj_gir = "Adj. GIR %",
    less_150_sg = "Stroke Ease: APP <150",
    greater_150_sg = "Stroke Ease: APP >150",
    
    # Around the Green
    arg_sg = "Stroke Ease: ARG",
    arg_fairway_sg = "Stroke Ease: Fairway ARG",
    arg_rough_sg = "Stroke Ease: Rough ARG",
    arg_bunker_sg = "Stroke Ease: Bunker ARG",
    
    # Putting
    putt_sg = "Stroke Ease: PUTT",
    less_5_ft_sg = "Stroke Ease: PUTT <5 Feet",
    greater_5_less_15_sg = "Stroke Ease: PUTT 5–15 Feet",
    greater_15_sg = "Stroke Ease: PUTT >15 Feet"
  )
  
  # Map Stats to Their Categories for Nested Table
  stat_categories <- c(
    "par" = "Overview", "yardage" = "Overview", "yardage_4_5" = "Overview",
    "yardage_3" = "Overview",
    
    "difficulty" = "Scoring", "adj_par_3_score" = "Scoring", "adj_par_4_score" = "Scoring",
    "adj_par_5_score" = "Scoring", "avg_eagles" = "Scoring", "avg_birdies" = "Scoring",
    "avg_pars" = "Scoring", "avg_bogeys" = "Scoring", "avg_doubleBogeys" = "Scoring",
    "adj_penalties" = "Scoring", "adj_ob" = "Scoring",
    
    "adj_driving_distance" = "Off-the-Tee", "adj_sd_distance" = "Off-the-Tee",
    "adj_driving_accuracy" = "Off-the-Tee", "ott_sg" = "Off-the-Tee", "fw_width" = "Off-the-Tee",
    "fw_diff" = "Off-the-Tee", "rgh_diff" = "Off-the-Tee", "non_rgh_diff" = "Off-the-Tee",
    "miss_fw_pen_frac" = "Off-the-Tee",
    
    "app_sg" = "Approach", 'adj_gir' = "Approach", "less_150_sg" = "Approach",
    "greater_150_sg" = "Approach",
    
    "arg_sg" = "Around the Green", "arg_fairway_sg" = "Around the Green",
    "arg_rough_sg" = "Around the Green", "arg_bunker_sg" = "Around the Green",
    
    "putt_sg" = "Putting", "less_5_ft_sg" = "Putting", "greater_5_less_15_sg" = "Putting",
    "greater_15_sg" = "Putting"
  )
  
  # Function to Calculate the Percentile of a value within a column's data
  calculatePercentile <- function(value, column_data) {
    column_data <- column_data[!is.na(column_data)]
    
    mean(column_data <= value) * 100
  }
  
  # Function to calculate a color based on a percentile
  percentileColor <- function(percentile) {
    if (is.na(percentile)) return("white")
    val_scaled <- percentile / 100
    rgb(
      colorRamp(c("#F83E3E", "white", "#4579F1"))(val_scaled),
      maxColorValue = 255
    )
  }
  
  # Create Round By Round Table for avg of hole finish types
  #   | Stat | Value | ValueColor | Percentile | Category |
  all_round_data <- round_data %>% 
    group_by(course) %>% 
    summarise(
      avg_eagles = mean(avg_eagles, na.rm = TRUE),
      avg_birdies = mean(avg_birdies, na.rm = TRUE),
      avg_pars = mean(avg_pars, na.rm = TRUE),
      avg_bogeys = mean(avg_bogeys, na.rm = TRUE),
      avg_doubleBogeys = mean(avg_doubleBogeys, na.rm = TRUE)
    )
  
  round_course <- all_round_data %>%
    filter(course == curr_course)
  
  round_stats <- c("avg_eagles", "avg_birdies", "avg_pars", "avg_bogeys", "avg_doubleBogeys")
  
  round_table_data <- do.call(rbind, lapply(round_stats, function(stat) {
    
    value <- mean(round_course[[stat]], na.rm = TRUE)
    percentile <- calculatePercentile(value, all_round_data[[stat]])
    
    data.frame(
      Stat = stat_labels[[stat]],
      Value = round(value, 3),
      Percentile = round(percentile, 1),
      ValueColor = percentileColor(percentile),
      Category = stat_categories[[stat]],
      stringsAsFactors = FALSE
    )
  }))
  
  
  # Create table_data: All Course Table Stats
  #   | Stat | Value | ValueColor | Percentile | Category |
  all_stat_data <- all_data %>% filter(year == "all")
  
  stat_course <- all_stat_data %>% filter(course == curr_course)
  
  all_stats <- c("par", "yardage", "yardage_3", "yardage_4_5",
    
                 "difficulty", "adj_par_3_score", "adj_par_4_score",
                 "adj_par_5_score", "adj_penalties", "adj_ob",
                 
                 "adj_driving_distance", "adj_sd_distance",
                 "adj_driving_accuracy", "ott_sg", "fw_width",
                 "fw_diff", "rgh_diff", "non_rgh_diff", "miss_fw_pen_frac",
                 
                 "app_sg", 'adj_gir', "less_150_sg", "greater_150_sg",
                 
                 "arg_sg", "arg_fairway_sg", "arg_rough_sg", "arg_bunker_sg",
                 
                 "putt_sg", "less_5_ft_sg", "greater_5_less_15_sg", "greater_15_sg")
  
  stat_table_data <- do.call(rbind, lapply(all_stats, function(stat) {
    value <- mean(stat_course[[stat]], na.rm = TRUE)
    percentile <- calculatePercentile(value, all_stat_data[[stat]])
    
    data.frame(
      Stat = stat_labels[[stat]],
      Value = round(value, 3),
      Percentile = round(percentile, 1),
      ValueColor = percentileColor(percentile),
      Category = stat_categories[[stat]],
      stringsAsFactors = FALSE
    )
  }))
  
  # Stack Round Table and Stats Table DF's
  table_data <- bind_rows(stat_table_data, round_table_data)
  
  table_data$Category <- factor(
    table_data$Category,
    levels = c(
      "Overview",
      "Scoring",
      "Off-the-Tee",
      "Approach",
      "Around the Green",
      "Putting"
    )
  )
  
  return(table_data)
}

makeCategoryDropdownTable <- function(table_data) {
  
  # Create Category-Level Table
  categories <- unique(table_data$Category)
  category_table <- data.frame(Category = categories, stringsAsFactors = FALSE)
  
  table <- reactable(
    category_table,
    columns = list(Category = colDef(name = "Stat Category", align = "left")),
    defaultExpanded = TRUE,
    details = function(index) {
      cat_name <- category_table$Category[index]
      sub_data <- table_data[table_data$Category == cat_name, c("Stat", "Value", "Percentile", "ValueColor")]
      
      reactable(
        sub_data,
        columns = list(
          Stat = colDef(name = "Stat", align = "center", width = 219),
          Value = colDef(
            name = "Value",
            align = "center",
            width = 80,
            style = function(value, index) {
              color <- sub_data$ValueColor[index]
              list(background = color)
            }
          ),
          Percentile = colDef(name = "%", align = "center", width = 60),
          ValueColor = colDef(show=FALSE)
        ),
        pagination = FALSE,
        showPageInfo = FALSE,
        bordered = TRUE,
        compact = TRUE,
        fullWidth = TRUE,
        highlight = TRUE
      )
    },
    pagination = FALSE,
    showPageInfo = FALSE,
    onClick = NULL,
    outlined = TRUE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    fullWidth = TRUE,
    theme = reactableTheme(
      style = list(fontSize = "12px"),
      headerStyle = list(fontSize = "15px"),
      rowStyle = list(height = "25px")
    )
  )
  
  return(table)
}






