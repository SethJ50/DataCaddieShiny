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
source("basics.R")

serverCourseOverview <- function(input, output, session, favorite_players,
                                 playersInTournament, playersInTournamentTourneyNameConv,
                                 playersInTournamentPgaNames) {
  
  # Get Round by Round Data
  round_by_round <- data %>%
    filter(Round != "Event") %>% 
    select(course, dates, eagles, birdies, pars, bogeys, doubleBogeys) %>%
    mutate(
      eagles = replace_na(eagles, 0),
      birdies = replace_na(birdies, 0),
      pars = replace_na(pars, 0),
      bogeys = replace_na(bogeys, 0),
      doubleBogeys = replace_na(doubleBogeys, 0)
    ) %>%
    group_by(course, dates) %>% 
    summarise(
      avg_eagles = mean(eagles),
      avg_birdies = mean(birdies),
      avg_pars = mean(pars),
      avg_bogeys = mean(bogeys),
      avg_doubleBogeys = mean(doubleBogeys),
      .groups = "drop"
    ) %>%
    mutate(
      year = as.numeric(paste0("20", substr(dates, nchar(dates)-1, nchar(dates))))
    )
  
  # Set Year Selection
  output$co_year_selection <- renderUI({
    req(input$co_course)
    
    possible_years <- courseStatsData %>% 
      filter(course == input$co_course) %>% 
      pull(year) %>% 
      unique()
    
    
    tagList(
      div(style = "display: flex;
                   gap: 0px;
                   align-items: center;
          ",
          shinyWidgets::pickerInput(
            inputId = "co_stat_year",
            label = "Year:",
            choices = possible_years,
            selected = possible_years[1]
          )
      )
    )
  })
  

  # Output Course Stats Table
  output$coStatsTable <- renderReactable({
    curr_course <- input$co_course
    curr_year <- input$co_stat_year
    
    table_data <- makeCourseStatsTableData(
      courseStatsData,
      round_by_round,
      curr_course,
      curr_year
    )
    
    makeCategoryDropdownTable(table_data)
  })
  
  # Make Data for Player Course History Tables
  ch_table_data <- reactive({
    req(input$co_course)
    curr_course <- input$co_course
    
    makeCourseHistoryTableData(curr_course, favorite_players, playersInTournament)
  })
  
  makePlayerTable(input, output, favorite_players, playersInTournament, ch_table_data)
}



makeCourseStatsTableData <- function(all_data, round_data, curr_course, curr_year) {
  
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
  if(curr_year == "all") {
    all_round_data <- round_data %>% 
      group_by(course) %>% 
      summarise(
        avg_eagles = mean(avg_eagles, na.rm = TRUE),
        avg_birdies = mean(avg_birdies, na.rm = TRUE),
        avg_pars = mean(avg_pars, na.rm = TRUE),
        avg_bogeys = mean(avg_bogeys, na.rm = TRUE),
        avg_doubleBogeys = mean(avg_doubleBogeys, na.rm = TRUE)
      )
  } else {
    all_round_data <- round_data %>% 
      filter(as.character(year) == curr_year)
  }
  
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
  if(curr_year == "all") {
    all_stat_data <- all_data %>% filter(year == "all")
  } else {
    all_stat_data <- all_data %>% 
      filter(as.character(year) == curr_year)
  }
  
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

makePlayerTable <- function(input, output, favorite_players, playersInTournament, ch_table_data) {
  output$co_course_hist_tab <- renderUI({
    req(input$co_tables_selected == "Course History")
    
    reactableOutput("co_ch_table")
  })
  
  if(input$co_tables_selected == "Course History") {
    makeCourseHistoryTable(output, ch_table_data(), favorite_players)
  }
  
  output$co_proj_fit_tab <- renderUI({
    req(input$co_tables_selected == "Proj. Course Fit")
    
    
  })
  
  output$co_sim_course_tab <- renderUI({
    req(input$co_tables_selected == "Similar Course Perf.")
    
    
  })
  
  output$co_ott_perf_tab <- renderUI({
    req(input$co_tables_selected == "OTT Course Perf.")
    
    
  })
  
  output$co_score_hist_tab <- renderUI({
    req(input$co_tables_selected == "Scoring History")
    
    
  })
}

makeCourseHistoryTableData <- function(curr_course, favorite_players, playersInTournament) {
  
  # Make Name Conversions
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  playersInTournamentPgaNames <- nameFanduelToPga(playersInTournament)
  
  # Get FanDuel and DraftKigns Salary Data
  salaryData <- salaries %>% 
    filter(player %in% playersInTournament) %>% 
    select(player, fdSalary, dkSalary)
  
  # Function to Calculate the Percentile of a value within a column's data
  calculatePercentile <- function(value, column_data) {
    column_data <- column_data[!is.na(column_data)]
    
    mean(column_data <= value) * 100
  }
  
  # Get SG & finish data for tournament rounds
  #   | player | sgPutt | ... | sgOtt | drAcc | drDist | numRds |
  tournament_data <- data %>% 
    filter(
      player %in% playersInTournamentTourneyNameConv,
      course == curr_course,
      Round != "Event"
    ) %>% 
    arrange(dates) %>% 
    group_by(player) %>% 
    summarise(
      tournament = last(tournament),
      best_fin = {
        x <- min(finish, na.rm = TRUE)
        ifelse(x == 100, "CUT", as.character(x))
      },
      worst_fin = {
        x <- max(finish, na.rm = TRUE)
        ifelse(x == 100, "CUT", as.character(x))
      },
      made_cut_pct = {
        fin_by_event <- distinct(cur_data(), dates, finish)
        round(mean(fin_by_event$finish < 100, na.rm = TRUE) * 100, 1)
      },
      sgPutt = round(mean(sgPutt, na.rm = TRUE), 2),
      sgArg  = round(mean(sgArg,  na.rm = TRUE), 2),
      sgApp  = round(mean(sgApp,  na.rm = TRUE), 2),
      sgOtt  = round(mean(sgOtt,  na.rm = TRUE), 2),
      sgTot = round(mean(sgTot, na.rm = TRUE), 2),
      drAcc  = {
        val <- round(mean(drAcc,  na.rm = TRUE), 2)
        if(is.nan(val)) {
          val <- NA
        }
        
        val
      },
      drDist = {
        val <- round(mean(drDist, na.rm = TRUE), 2)
        if(is.nan(val)) {
          val <- NA
        }
        
        val
      },
      
      numRds = n(),
      .groups = "drop"
    ) %>% 
    mutate(
      Player = player,
      made_cut_pct_perc = round(sapply(made_cut_pct, calculatePercentile, made_cut_pct), 1),
      sgPutt_perc      = round(sapply(sgPutt, calculatePercentile, sgPutt), 1),
      sgArg_perc       = round(sapply(sgArg,  calculatePercentile, sgArg), 1),
      sgApp_perc       = round(sapply(sgApp,  calculatePercentile, sgApp), 1),
      sgOtt_perc       = round(sapply(sgOtt,  calculatePercentile, sgOtt), 1),
      sgTot_perc       = round(sapply(sgTot,  calculatePercentile, sgTot), 1),
      drAcc_perc       = round(sapply(drAcc,  calculatePercentile, drAcc), 1),
      drDist_perc      = round(sapply(drDist, calculatePercentile, drDist), 1)
    )
  
  favs <- favorite_players$names
  
  tournament_data <- tournament_data %>% 
    mutate(
      isFavorite = player %in% favs
    )
  
  # Create .favorite column to hold star	
  tournament_data$.favorite <- NA
  
  # Rearrange .favorite to be the first column
  tournament_data <- tournament_data[, c(".favorite", setdiff(names(tournament_data), ".favorite"))]
  
  return(tournament_data)
}

makeCourseHistoryTable <- function(output, ch_table_data, favorite_players) {
  table_cols <- list(
    Player = "Player", best_fin = "Best",
    worst_fin = "Worst", made_cut_pct = "InCut%",
    sgPutt = "SG: PUTT", sgArg = "SG: ARG", sgApp = "SG: APP", sgOtt = "SG: OTT",
    sgTot = "SG: TOT", drAcc = "DR. ACC", drDist = "DR. DIST", numRds = "Rds"
  )
  
  # Make column defs
  col_defs <- lapply(names(table_cols), function(col) {
    norm_col <- paste0(col, "_perc")
    
    if (norm_col %in% names(ch_table_data)) {
      norm_data <- ch_table_data[[norm_col]]
      col_func <- makeColorFunc(min_val = 0, max_val = 100)
    } else {
      norm_data <- NULL
      col_func <- NULL
    }
    
    col_inner <- table_cols[[col]]
    
    width <- case_when(
      col_inner %in% c("Player", "Tournament") ~ 150,
      col_inner %in% c("Best", "Worst", "InCut%") ~ 45,
      col_inner == "Rds" ~ 35,
      TRUE ~ 60
    )
    
    makeColDef(
      col_name = col,
      display_name = table_cols[[col]],
      width = width,
      color_func = col_func,
      norm_data = norm_data,
      header_size = 10
    )
  })
  
  names(col_defs) <- names(table_cols)
  
  col_defs[["isFavorite"]] <- colDef(show = FALSE)
  
  table_data <- ch_table_data %>% 
    select(`.favorite`, names(table_cols), isFavorite)
  
  output$co_ch_table <- renderReactable({
    makeBasicTable(table_data, col_defs, "sgTot", favorite_players, hasFavorites = TRUE,
                   font_size = 10, row_height = 22)
  })
}


