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
library(shinycssloaders)

source("utils.R")
source("playersDataFunctions.R")
source("basics.R")

serverCourseOverview <- function(input, output, session, favorite_players,
                                 playersInTournament, playersInTournamentTourneyNameConv,
                                 playersInTournamentPgaNames) {
  
  week_course <- "Augusta National Golf Club"
  
  # Set Weekly Course
  shinyWidgets::updatePickerInput(
    session,
    inputId = "co_course",
    selected = week_course
  )
  
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
  
  possible_years <- reactive({
    req(input$co_course)
    
    courseStatsData %>% 
      filter(course == input$co_course) %>% 
      pull(year) %>% 
      unique()
  })
  
  observeEvent(possible_years(), {
    yrs <- possible_years()
    req(length(yrs) > 0)
    
    shinyWidgets::updatePickerInput(
      session,
      inputId = "co_ch_table_year",
      choices = yrs,
      selected = "all"
    )
  })
  
  # Set Year Selection
  output$co_year_selection <- renderUI({
    req(input$co_course)
    
    
    tagList(
      div(style = "display: flex;
                   gap: 0px;
                   align-items: center;
          ",
          shinyWidgets::pickerInput(
            inputId = "co_stat_year",
            label = "Year:",
            choices = possible_years(),
            selected = possible_years()[1]
          )
      )
    )
  })
  
  ## Make Center Visualization Console
  # Make Radar Plot
  output$co_stat_radar <- renderPlotly({
    req(input$co_course, input$co_fit_viz)
    
    makeCoRadarPlot(input, output, input$co_course)
  })
  
  observeEvent(input$co_importance_info, {
    showModal(modalDialog(
      title = "Importance Type Explanation",
      HTML(
        "<b>Scaled:</b> Shows absolute predictive power of each feature, exhibiting,
        for example, that approach is more predictive than putting on all
        courses.
        ==> Use this to develop predictive models for the course, taking into
        account predictive power of each feature<br/>
       <b>Raw:</b> Z-Score Normalizes each feature and plots current courses
        value as number of standard deviations above or below average. Shows
        more clearly the differences in importance of features across courses,
        but doesn't show the absolute importance of the feature in comparison
        to one another. 
        ==> Use this to evaluate differences between courses
          i.e. see how the importance of stats of this course differs from others"
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Output Course Stats Table
  output$coStatsTable <- renderReactable({
    req(input$co_stat_year)
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
  
  course_fit_cache <- reactiveValues()
  
  course_fit_data <- reactive({
    req(input$co_course)
    
    # If cache already has this course, just return it
    if (!is.null(course_fit_cache[[input$co_course]])) {
      return(course_fit_cache[[input$co_course]])
    }
    
    # Otherwise compute and store
    proj_fit_data <- makeProjFitData(
      curr_course = input$co_course,
      favorite_players = favorite_players,
      playersInTournament = playersInTournament
    )
    
    course_fit_cache[[input$co_course]] <- proj_fit_data
    proj_fit_data
  })
  
  ott_cache <- reactiveValues()
  
  ott_data <- reactive({
    req(input$co_course)
    
    if (!is.null(ott_cache[[input$co_course]])) {
      return(ott_cache[[input$co_course]])
    }
    
    data <- makeCourseOttData(
      curr_course = input$co_course,
      playersInTournament = playersInTournament
    )
    
    ott_cache[[input$co_course]] <- data
    data
  })
  
  ott_data <- reactive({
    req(input$co_course)
    
    # If cache already has this course, just return it
    if (!is.null(ott_cache[[input$co_course]])) {
      return(ott_cache[[input$co_course]])
    }
    
    # Otherwise compute and store
    ott_table_data <- makeCourseOttData(input$co_course, favorite_players, playersInTournament)
    
    ott_cache[[input$co_course]] <- ott_table_data
    ott_table_data
  })
  
  # Make Player Tables
  makePlayerTable(input, output, favorite_players, playersInTournament, input$co_course, course_fit_data, ott_data)
}



makeCoRadarPlotData <- function(input, curr_course) {
  
  req(curr_course)
  req(input$co_fit_viz)
  
  if(input$co_fit_viz == "Basic Stats") {
    # Output DataFrame with Stat Labels,
    #   Curr Course Data, Curr Course Data Scaled (Normalized),
    #   Average Course Data, Average Course Data Scaled (Normalized)
    # 	| stat | curr_value | curr_scaled | avg_value | avg_scaled |
    
    # Get Model Features
    all_models <- readRDS("course_history_models.rds")
    curr_model_info <- all_models[[curr_course]]
    if(is.null(curr_model_info)) return(NULL)
    model_features <- curr_model_info$features
    
    # Define global feature list
    global_features <- unique(unlist(
      lapply(all_models, function(x) x$features)
    ))
    
    # Normalizer Function
    normalize <- function(x, min_val, max_val) {
      ifelse(
        max_val == min_val,
        0,
        (x - min_val) / (max_val - min_val)
      )
    }
    
    # Collect SHAP Data for all courses
    #   | course | stat | shap_value | 
    shap_all_courses <- purrr::map_dfr(
      names(all_models),
      function(course) {
        shap_wide <- all_models[[course]]$shap_agg
        if (is.null(shap_wide)) return(NULL)
        
        shap_wide %>%
          mutate(course = course) %>%
          tidyr::pivot_longer(
            cols = all_of(model_features),
            names_to = "stat",
            values_to = "shap_value"
          )
      }
    )
    
    # Collect Model Importance for all courses
    importance_all <- purrr::map_dfr(
      names(all_models),
      function(course) {
        model <- all_models[[course]]$model
        if (is.null(model)) return(NULL)
        
        imp <- xgboost::xgb.importance(
          model = model,
          feature_names = global_features
        )
        
        # Zero-fill missing features
        tibble(stat = global_features) %>%
          left_join(
            imp %>% select(stat = Feature, importance = Gain),
            by = "stat"
          ) %>%
          mutate(
            importance = replace_na(importance, 0),
            course = course
          )
      }
    )
    
    # Current Course Data Values
    curr_course_data <- shap_all_courses %>%
      filter(course == curr_course) %>%
      select(stat, curr_value = shap_value)
    
    # Average Course Values
    avg_course_data <- shap_all_courses %>%
      group_by(stat) %>%
      summarise(
        avg_value = mean(shap_value, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Determine Min, Max ranges for each model stat
    norm_ranges <- shap_all_courses %>%
      group_by(stat) %>%
      summarise(
        min_val = min(shap_value, na.rm = TRUE),
        max_val = max(shap_value, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Determine mean and sd for each feature
    stats_ranges <- shap_all_courses %>%
      group_by(stat) %>%
      summarise(
        mean_val = mean(shap_value, na.rm = TRUE),
        sd_val   = sd(shap_value, na.rm = TRUE),
        .groups = "drop"
      )
    
    pretty_stat_labels <- c(
      sg_putt_l50 = "SG: PUTT",
      sg_arg_l50  = "SG: ARG",
      sg_app_l50  = "SG: APP",
      dr_dist_l50 = "DR. DIST",
      dr_acc_l50  = "DR. ACC"
    )
    
    radar_data <- curr_course_data %>%
      left_join(avg_course_data, by = "stat") %>%
      left_join(stats_ranges, by = "stat") %>%
      mutate(
        curr_scaled = ifelse(sd_val == 0, 0, (curr_value - mean_val) / sd_val),
        avg_scaled  = ifelse(sd_val == 0, 0, (avg_value - mean_val) / sd_val)
      ) %>%
      transmute(
        stat,
        stat_label = pretty_stat_labels[stat] %||% stat,
        curr_value  = round(curr_value, 4),
        curr_scaled = round(curr_scaled, 4),
        avg_value   = round(avg_value, 4),
        avg_scaled  = round(avg_scaled, 4)
      )
    
    
    return(radar_data)
  } else if(input$co_fit_viz == "OTT Strategy") {
    # 	| stat | curr_value | curr_scaled | cluster_value | cluster_scaled | avg_value | avg_scaled |
    
    # Load OTT Course Cluster Data
    ott_clusters <- readRDS("course_ott_clusters.rds")
    if(is.null(ott_clusters)) return(NULL)
    
    curr_course_entry <- ott_clusters %>% 
      filter(course == curr_course)
    if(nrow(curr_course_entry) == 0) return(NULL)
    
    curr_course_cluster <- curr_course_entry %>% 
      pull(cluster) %>% 
      first()
    
    courses_in_cluster <- ott_clusters %>% 
      filter(cluster == curr_course_cluster) %>% 
      pull(course)
    
    # Stats to use
    ott_stats <- c(
      "yardage_4_5",
      "adj_driving_distance",
      "adj_sd_distance",
      "adj_driving_accuracy",
      "ott_sg",
      "fw_width",
      "fw_diff"
    )
    
    # ----- Long Form Data -----
    
    # Get Current Course Statistics
    curr_course_stats <- curr_course_entry %>% 
      select(all_of(ott_stats)) %>% 
      pivot_longer(
        everything(),
        names_to = "stat",
        values_to = "curr_value"
      )
    
    # Get Average Statistics for Course Cluster
    curr_cluster_stats <- ott_clusters %>% 
      filter(cluster == curr_course_cluster) %>%
      summarise(across(all_of(ott_stats), mean, na.rm = TRUE)) %>%
      pivot_longer(
        everything(),
        names_to = "stat",
        values_to = "cluster_value"
      )
    
    # Get Average Statistics Overall
    all_stats <- ott_clusters %>% 
      summarise(across(all_of(ott_stats), mean, na.rm = TRUE)) %>%
      pivot_longer(
        everything(),
        names_to = "stat",
        values_to = "avg_value"
      )
  
    # Global mean & sd for scaling
    global_stats <- ott_clusters %>%
      pivot_longer(
        cols = all_of(ott_stats),
        names_to = "stat",
        values_to = "value"
      ) %>%
      group_by(stat) %>%
      summarise(
        mean_val = mean(value, na.rm = TRUE),
        sd_val   = sd(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Create Pretty Stat Labels
    pretty_stat_labels <- c(
      yardage_4_5 = "Par 4/5 Len",
      adj_driving_distance  = "Avg Dr. Dist",
      adj_sd_distance  = "Dr. Dist Var.",
      adj_driving_accuracy = "Avg. Dr. Acc",
      ott_sg  = "SG:OTT Ease",
      fw_width  = "Avg. FW Width",
      fw_diff  = "Miss FW Penalty"
    )
    
    # Final Radar DataFrame
    radar_data <- curr_course_stats %>%
      left_join(curr_cluster_stats, by = "stat") %>%
      left_join(all_stats, by = "stat") %>%
      left_join(global_stats, by = "stat") %>%
      mutate(
        curr_scaled    = ifelse(sd_val == 0, 0, (curr_value - mean_val) / sd_val),
        cluster_scaled = ifelse(sd_val == 0, 0, (cluster_value - mean_val) / sd_val),
        avg_scaled     = ifelse(sd_val == 0, 0, (avg_value - mean_val) / sd_val)
      ) %>%
      transmute(
        stat,
        stat_label = pretty_stat_labels[stat] %||% stat,
        curr_value,
        curr_scaled,
        cluster_value = round(cluster_value, 3),
        cluster_scaled,
        avg_value,
        avg_scaled
      )
    
    return(radar_data)
  } else {
    return(NULL)
  }
}

makeCoRadarPlot <- function(input, output, curr_course) {
  
  radar_data <- makeCoRadarPlotData(input, curr_course)
  if (is.null(radar_data)) return(NULL)
  
  # Create list of points around plot
  #   'closed' by adding first point again to end to close circle
  if(input$co_fit_viz == "Basic Stats") {
    # Establish Plotting of Raw vs. Scaled
    #   Raw: Shows absolute predictive power of each feature, exhibiting,
    #     for example, that approach is more predictive than putting on all
    #     courses.
    #     ==> Use this to develop predictive models for the course, taking into
    #     account predictive power of each feature
    #   Scaled: Z-Score Normalizes each feature and plots current courses
    #     value as number of standard deviations above or below average. Shows
    #     more clearly the differences in importance of features across courses,
    #     but doesn't show the absolute importance of the feature in comparison
    #     to one another. 
    #     ==> Use this to evaluate differences between courses
    #       i.e. see how the importance of stats of this course differs from others
    if(input$co_importance_type == "raw") {
      r_course <- radar_data$curr_value
      r_all <- radar_data$avg_value
    } else { # Scaled
      r_course <- radar_data$curr_scaled
      r_all    <- radar_data$avg_scaled
    }
    
    # Dynamic Zoom on View
    r_all_points <- c(r_course, r_all)
    
    r_min_raw <- min(r_all_points, na.rm = TRUE)
    r_max_raw <- max(r_all_points, na.rm = TRUE)
    
    range_width <- r_max_raw - r_min_raw
    pad <- 0.05 * range_width  # 5% padding
    
    r_min <- r_min_raw - pad
    r_max <- r_max_raw + pad
    
    r_closed <- c(r_course, r_course[1])
    r_closedAll <- c(r_all, r_all[1])
    
    theta_closed <- c(radar_data$stat_label, radar_data$stat_label[1])
    course_vals_closed <- c(radar_data$curr_value, radar_data$curr_value[1])
    stat_labels_closed <- c(radar_data$stat_label, radar_data$stat_label[1])
    field_vals_closed <- c(radar_data$avg_value, radar_data$avg_value[1])
    
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      showlegend = TRUE
    ) %>% 
      add_trace(
        r = r_closedAll,
        theta = theta_closed,
        name = 'All',
        mode = "lines+markers",
        text = paste0("All: <br>",
                      stat_labels_closed, ": ", field_vals_closed, "<br>"),
        hoverinfo = "text",
        marker = list(color = "#FF6666"),
        line = list(color = "#FF6666"),
        fillcolor = "rgba(255, 102, 102, 0.2)",
        connectgaps = TRUE
      ) %>%
      add_trace(
        r = r_closed,
        theta = theta_closed,
        name = input$co_course,
        mode = "lines+markers",
        text = paste0(input$co_course, ": <br>",
                      stat_labels_closed,": ", course_vals_closed, "<br>"),
        hoverinfo = "text",
        marker = list(color = "navy"),
        line = list(color = "navy"),
        fillcolor = "rgba(0, 0, 128, 0.3)",
        connectgaps = TRUE
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(r_min, r_max),
            showline = FALSE,
            showticklabels = FALSE
          ),
          angularaxis = list(
            tickfont = list(size = 9)
          )
        ),
        margin = list(l = 0, r = 0, t = 30, b = 60),
        width = 300,
        height = 250,
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  } else if(input$co_fit_viz == "OTT Strategy") {
    
    # Set stat for radar radius value
    r_course <- radar_data$curr_scaled
    r_cluster <- radar_data$cluster_scaled
    r_all <- radar_data$avg_scaled
    
    # Dynamic Zoom on View
    r_all_points <- c(r_course, r_cluster, r_all)
    
    r_min_raw <- min(r_all_points, na.rm = TRUE)
    r_max_raw <- max(r_all_points, na.rm = TRUE)
    
    range_width <- r_max_raw - r_min_raw
    pad <- 0.05 * range_width  # 5% padding
    
    r_min <- r_min_raw - pad
    r_max <- r_max_raw + pad
    
    # Create Closed Statistic Lists
    r_closed <- c(r_course, r_course[1])
    r_closedCluster <- c(r_cluster, r_cluster[1])
    r_closedAll <- c(r_all, r_all[1])
    theta_closed <- c(radar_data$stat_label, radar_data$stat_label[1])
    stat_labels_closed <- c(radar_data$stat_label, radar_data$stat_label[1])
    course_vals_closed <- c(radar_data$curr_value, radar_data$curr_value[1])
    cluster_vals_closed <- c(radar_data$cluster_value, radar_data$cluster_value[1])
    field_vals_closed <- c(radar_data$avg_value, radar_data$avg_value[1])
    
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      showlegend = TRUE
    ) %>% 
      add_trace(
        r = r_closedAll,
        theta = theta_closed,
        name = 'All',
        mode = "lines+markers",
        text = paste0("All: <br>",
                      stat_labels_closed, ": ", field_vals_closed, "<br>"),
        hoverinfo = "text",
        marker = list(color = "#FF6666"),
        line = list(color = "#FF6666"),
        fillcolor = "rgba(255, 102, 102, 0.2)",
        connectgaps = TRUE
      ) %>%
      add_trace(
        r = r_closedCluster,
        theta = theta_closed,
        name = 'All',
        mode = "lines+markers",
        text = paste0("All: <br>",
                      stat_labels_closed, ": ", cluster_vals_closed, "<br>"),
        hoverinfo = "text",
        marker = list(color = "#90EE90"),
        line = list(color = "#90EE90"),
        fillcolor = "rgba(144, 238, 144, 0.30)",
        connectgaps = TRUE
      ) %>%
      add_trace(
        r = r_closed,
        theta = theta_closed,
        name = input$co_course,
        mode = "lines+markers",
        text = paste0(input$co_course, ": <br>",
                      stat_labels_closed,": ", course_vals_closed, "<br>"),
        hoverinfo = "text",
        marker = list(color = "navy"),
        line = list(color = "navy"),
        fillcolor = "rgba(0, 0, 128, 0.3)",
        connectgaps = TRUE
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(r_min, r_max),
            showline = FALSE,
            showticklabels = FALSE
          ),
          angularaxis = list(
            tickfont = list(size = 9)
          )
        ),
        margin = list(l = 0, r = 0, t = 30, b = 60),
        width = 300,
        height = 250,
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
    
  } else {
    return(NULL)
  }
  
 
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

makePlayerTable <- function(input, output, favorite_players, playersInTournament, curr_course, course_fit_data, ott_data) {
  
  output$co_ch_table <- renderReactable({
    req(input$co_course)
    req(input$co_ch_table_year)
    
    ch_table_data <- makeCourseHistoryTableData(
      curr_course = input$co_course,
      favorite_players = favorite_players,
      playersInTournament = playersInTournament,
      curr_year = input$co_ch_table_year
    )
    
    if(!is.null(ch_table_data)){
      makeCourseHistoryTable(
        ch_table_data = ch_table_data,
        favorite_players = favorite_players,
        year = input$co_ch_table_year
      )
    }
  })
  
  output$co_proj_fit_tab <- renderUI({
    req(input$co_tables_selected == "Proj. Course Fit")
    
    table_data <- course_fit_data()
    req(table_data)
    
    table_data <- table_data %>%
      mutate(isFavorite = player %in% favorite_players$names)
    
    if(!is.null(table_data)) {
      makeCourseFitTable(
        table_data = table_data,
        favorite_players = favorite_players
      )
    }
  })
  
  output$co_sim_course_tab <- renderUI({
    req(input$co_tables_selected == "Similar Course Perf.")
    
    
  })
  
  output$co_ott_perf_tab <- renderUI({
    req(input$co_tables_selected == "OTT Course Perf.")
    req(input$co_course)
    
    table_data <- ott_data()
    req(table_data)
    
    table_data <- table_data %>%
      mutate(isFavorite = player %in% favorite_players$names)
    
    if(!is.null(table_data)) {
      makeCourseOttTable(table_data, favorite_players)
    }
  })
  
  output$co_score_hist_tab <- renderUI({
    req(input$co_tables_selected == "Scoring History")
    
    
  })
}

makeCourseHistoryTableData <- function(curr_course, favorite_players, playersInTournament, curr_year) {
  
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
  if(curr_year == "all") {
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
      )
    
    if(nrow(tournament_data) < 1) {
      return(NULL)
    }
    
    tournament_data <- tournament_data %>% 
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
  } else {
    #   | player | dates | sgPutt | ... | sgOtt | drAcc | drDist | numRds |
    tournament_data <- data %>% 
      filter(
        Round != "Event"
      ) %>% 
      mutate(
        year = as.numeric(paste0("20", substr(dates, nchar(dates)-1, nchar(dates))))
      ) %>% 
      filter(
        player %in% playersInTournamentTourneyNameConv,
        course == curr_course,
        as.character(year) == curr_year
      ) %>% 
      arrange(dates) %>% 
      group_by(player) %>% 
      summarise(
        tournament = last(tournament),
        date = last(dates),
        finish = last(finish),
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
      )
    
    if(nrow(tournament_data) < 1) {
      return(NULL)
    }
    
    tournament_data <- tournament_data %>% 
      mutate(
        Player = player,
        sgPutt_perc      = round(sapply(sgPutt, calculatePercentile, sgPutt), 1),
        sgArg_perc       = round(sapply(sgArg,  calculatePercentile, sgArg), 1),
        sgApp_perc       = round(sapply(sgApp,  calculatePercentile, sgApp), 1),
        sgOtt_perc       = round(sapply(sgOtt,  calculatePercentile, sgOtt), 1),
        sgTot_perc       = round(sapply(sgTot,  calculatePercentile, sgTot), 1),
        drAcc_perc       = round(sapply(drAcc,  calculatePercentile, drAcc), 1),
        drDist_perc      = round(sapply(drDist, calculatePercentile, drDist), 1)
      )
  }
  
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

makeProjFitData <- function(curr_course, favorite_players, playersInTournament) {
  
  # Make Name Conversions
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  playersInTournamentPgaNames <- nameFanduelToPga(playersInTournament)
  
  
  # Function to Calculate the Percentile of a value within a column's data
  calculatePercentile <- function(value, column_data) {
    column_data <- column_data[!is.na(column_data)]
    
    mean(column_data <= value) * 100
  }
  
  # Load Models
  all_models <- readRDS("course_history_models.rds")
  curr_model_info <- all_models[[curr_course]]
  if(is.null(curr_model_info)) return(NULL)
  curr_model <- curr_model_info$model
  model_features <- curr_model_info$features
  
  # Initialize Results List
  results_list <- list()
  
  today_date <- format(Sys.Date(), "%m/%d/%y")
  
  # Build up dataframe | player | sg {putt / arg / app / sgTot} | dr {dist / acc} | numRds | course fit
  for (curr_player in playersInTournamentTourneyNameConv) {
    
    # Get last 50 rounds for player prior to today
    last50Rds <- getLastNRoundsPriorTo(curr_player, today_date, 50)
    if(nrow(last50Rds) == 0) next
    
    rounds_clean <- last50Rds %>% 
      filter(
        !is.na(sgPutt),
        !is.na(sgArg),
        !is.na(sgApp),
        !is.na(drDist),
        !is.na(drAcc)
      )
    if (nrow(rounds_clean) == 0) next
    
    rounds_clean <- rounds_clean %>% 
      select(-sgTot) %>% 
      left_join(
        data %>% 
          filter(Round == "Event") %>% 
          select(player, tournament, dates, sgTot),
        by = c("player", "tournament", "dates")
      ) %>%
      rename(sg_tot_event = sgTot) %>%
      filter(!is.na(sg_tot_event))
    if (nrow(rounds_clean) == 0) next
    
    agg <- rounds_clean %>%
      summarise(
        sg_putt_l50 = mean(sgPutt),
        sg_arg_l50  = mean(sgArg),
        sg_app_l50  = mean(sgApp),
        dr_dist_l50 = mean(drDist),
        dr_acc_l50  = mean(drAcc),
        sg_tot_l50 = mean(sg_tot_event),
        numRds      = n()
      )
    if (agg$numRds < 8) next
    
    player_data <- agg %>% 
      select(all_of(model_features)) %>% 
      as.matrix()
    
    sgPred <- predict(curr_model, player_data)
    course_fit <- sgPred - agg$sg_tot_l50
    
    results_list[[curr_player]] <- data.frame(
      player = curr_player,
      sg_putt_l50 = round(agg$sg_putt_l50, 2),
      sg_arg_l50  = round(agg$sg_arg_l50, 2),
      sg_app_l50  = round(agg$sg_app_l50, 2),
      dr_dist_l50 = round(agg$dr_dist_l50, 2),
      dr_acc_l50  = round(agg$dr_acc_l50, 2),
      sgTot = round(agg$sg_tot_l50, 2),
      projSgTot = round(sgPred, 2),
      course_fit = round(course_fit, 2),
      numRds = agg$numRds,
      stringsAsFactors = FALSE
    )
  }
  
  result_data <- bind_rows(results_list)
  if(nrow(result_data) == 0) return(NULL)
  
  # Add Percentile Columns for future coloring
  result_data <- result_data %>% 
    mutate(
      Player = player,
      sg_putt_l50_perc      = round(sapply(sg_putt_l50, calculatePercentile, sg_putt_l50), 1),
      sg_arg_l50_perc       = round(sapply(sg_arg_l50,  calculatePercentile, sg_arg_l50), 1),
      sg_app_l50_perc       = round(sapply(sg_app_l50,  calculatePercentile, sg_app_l50), 1),
      dr_dist_l50_perc       = round(sapply(dr_dist_l50,  calculatePercentile, dr_dist_l50), 1),
      dr_acc_l50_perc       = round(sapply(dr_acc_l50,  calculatePercentile, dr_acc_l50), 1),
      sgTot_perc       = round(sapply(sgTot,  calculatePercentile, sgTot), 1),
      projSgTot_perc = round(sapply(projSgTot,  calculatePercentile, projSgTot), 1),
      course_fit_perc       = round(sapply(course_fit,  calculatePercentile, course_fit), 1)
    )
  
  # Mark favorite players
  favs <- favorite_players$names
  result_data <- result_data %>% 
    mutate(
      isFavorite = player %in% favs
    )
  
  # Create .favorite column to hold star	
  result_data$.favorite <- NA
  
  # Rearrange .favorite to be the first column
  result_data <- result_data[, c(".favorite", setdiff(names(result_data), ".favorite"))]
  
  return(result_data)
}

makeCourseOttData <- function(curr_course, favorite_players, playersInTournament) {
  
  req(curr_course)
  
  # Make Name Conversions
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  playersInTournamentPgaNames <- nameFanduelToPga(playersInTournament)
  
  
  # Function to Calculate the Percentile of a value within a column's data
  calculatePercentile <- function(value, column_data) {
    column_data <- column_data[!is.na(column_data)]
    
    mean(column_data <= value) * 100
  }
  
  # Load OTT Course Cluster Data
  ott_clusters <- readRDS("course_ott_clusters.rds")
  if(is.null(ott_clusters)) return(NULL)
  
  curr_course_entry <- ott_clusters %>% 
    filter(course == curr_course)
  if(nrow(curr_course_entry) == 0) return(NULL)
  
  curr_course_cluster <- curr_course_entry %>% 
    pull(cluster) %>% 
    first()
  
  courses_in_cluster <- ott_clusters %>% 
    filter(cluster == curr_course_cluster) %>% 
    pull(course)
  
  # Filter Tournament Data
  tournament_data <- data %>% 
    filter(
      player %in% playersInTournamentTourneyNameConv,
      Round != "Event",
      course %in% courses_in_cluster
    )
  
  if(nrow(tournament_data) == 0) return(NULL)
  
  table_data <- tournament_data %>% 
    group_by(player) %>% 
    summarise(
      sgOtt = round(mean(sgOtt, na.rm = TRUE), 2),
      drDist = ifelse(
        is.nan(mean(drDist, na.rm = TRUE)),
        NA_real_,
        round(mean(drDist, na.rm = TRUE), 2)
      ),
      drAcc = ifelse(
        is.nan(mean(drAcc, na.rm = TRUE)),
        NA_real_,
        round(mean(drAcc, na.rm = TRUE), 2)
      ),
      sgTot = round(mean(sgTot, na.rm = TRUE), 2),
      numRds = n(),
      .groups = "drop"
    )
  
  # Append performance baselines, performance over expectation
  today_date <- format(Sys.Date(), "%m/%d/%y")
  
  player_baselines <- table_data %>%
    distinct(player) %>%
    rowwise() %>%
    mutate(
      last50 = list(getLastNRoundsPriorTo(player, today_date, 50)),
      sgOtt_l50 = round(mean(last50$sgOtt, na.rm = TRUE), 2),
      sgTot_l50 = round(mean(last50$sgTot, na.rm = TRUE), 2)
    ) %>%
    ungroup() %>%
    select(player, sgOtt_l50, sgTot_l50)
  
  table_data <- table_data %>%
    left_join(player_baselines, by = "player") %>%
    mutate(
      sgOttPerfAbvBase = round(sgOtt - sgOtt_l50, 2),
      sgTotPerfAbvBase = round(sgTot - sgTot_l50, 2)
    )
  
  table_data <- table_data %>% 
    mutate(
      Player = player,
      sgOtt_perc = round(sapply(sgOtt, calculatePercentile, sgOtt), 1),
      drDist_perc      = round(sapply(drDist, calculatePercentile, drDist), 1),
      drAcc_perc       = round(sapply(drAcc,  calculatePercentile, drAcc), 1),
      sgTot_perc       = round(sapply(sgTot,  calculatePercentile, sgTot), 1),
      sgOttPerfAbvBase_perc       = round(sapply(sgOttPerfAbvBase,  calculatePercentile, sgOttPerfAbvBase), 1),
      sgTotPerfAbvBase_perc       = round(sapply(sgTotPerfAbvBase,  calculatePercentile, sgTotPerfAbvBase), 1)
    )
  
  # Mark favorite players
  favs <- favorite_players$names
  table_data <- table_data %>% 
    mutate(
      isFavorite = player %in% favs
    )
  
  # Create .favorite column to hold star	
  table_data$.favorite <- NA
  
  # Rearrange .favorite to be the first column
  table_data <- table_data[, c(".favorite", setdiff(names(table_data), ".favorite"))]
  
  return(table_data)
}

makeCourseHistoryTable <- function(output, ch_table_data, favorite_players, year) {
  if(year == "all") {
    table_cols <- list(
      Player = "Player", best_fin = "Best",
      worst_fin = "Worst", made_cut_pct = "InCut%",
      sgPutt = "SG: PUTT", sgArg = "SG: ARG", sgApp = "SG: APP", sgOtt = "SG: OTT",
      sgTot = "SG: TOT", drAcc = "DR. ACC", drDist = "DR. DIST", numRds = "Rds"
    )
  } else {
    table_cols <- list(
      Player = "Player", date = "Date", finish = "Fin",
      sgPutt = "SG: PUTT", sgArg = "SG: ARG", sgApp = "SG: APP", sgOtt = "SG: OTT",
      sgTot = "SG: TOT", drAcc = "DR. ACC", drDist = "DR. DIST", numRds = "Rds"
    )
  }
  
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
  
  table <- makeBasicTable(table_data, col_defs, "sgTot", favorite_players, hasFavorites = TRUE,
                 font_size = 10, row_height = 22)
  
  return(table)
}

makeCourseFitTable <- function(table_data, favorite_players) {
  table_cols <- list(
    Player = "Player", 
    sg_putt_l50 = "SG: PUTT", sg_arg_l50 = "SG: ARG", sg_app_l50 = "SG: APP",
    dr_dist_l50 = "DR. DIST", dr_acc_l50 = "DR. ACC",
    sgTot = "SG: TOT", projSgTot = "Proj. SG: TOT", course_fit = "REL FIT", numRds = "Rds"
  )
  
  # Make column defs
  col_defs <- lapply(names(table_cols), function(col) {
    norm_col <- paste0(col, "_perc")
    
    if (norm_col %in% names(table_data)) {
      norm_data <- table_data[[norm_col]]
      col_func <- makeColorFunc(min_val = 0, max_val = 100)
    } else {
      norm_data <- NULL
      col_func <- NULL
    }
    
    col_inner <- table_cols[[col]]
    
    width <- case_when(
      col_inner %in% c("Player") ~ 150,
      col_inner == "Proj. SG: TOT" ~ 90,
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
  
  table_data <- table_data %>% 
    select(`.favorite`, names(table_cols), isFavorite)
  
  table <- makeBasicTable(table_data, col_defs, "projSgTot", favorite_players, hasFavorites = TRUE,
                          font_size = 10, row_height = 22)
  
  return(table)
}

makeCourseOttTable <- function(table_data, favorite_players) {
  table_cols <- list(
    Player = "Player", 
    sgOtt = "SG: OTT", drDist = "DR. DIST", drAcc = "DR. ACC", sgTot = "SG: TOT",
    sgOttPerfAbvBase = "OTT v. L50", sgTotPerfAbvBase = "TOT v. L50", numRds = "Rds"
  )
  
  # Make column defs
  col_defs <- lapply(names(table_cols), function(col) {
    norm_col <- paste0(col, "_perc")
    
    if (norm_col %in% names(table_data)) {
      norm_data <- table_data[[norm_col]]
      col_func <- makeColorFunc(min_val = 0, max_val = 100)
    } else {
      norm_data <- NULL
      col_func <- NULL
    }
    
    col_inner <- table_cols[[col]]
    
    width <- case_when(
      col_inner %in% c("Player") ~ 150,
      col_inner %in% c("OTT v. L50", "TOT v. L50") ~ 90,
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
  
  table_data <- table_data %>% 
    select(`.favorite`, names(table_cols), isFavorite)
  
  table <- makeBasicTable(table_data, col_defs, "sgOttPerfAbvBase", favorite_players, hasFavorites = TRUE,
                          font_size = 10, row_height = 22)
  
  return(table)
}
