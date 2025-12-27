library(tidyverse)
library(xgboost)
library(Matrix)
library(SHAPforxgboost)

source("playersDataFunctions.R")

trainCourseFitModels <- function() {
  
  # { Course_Name: Course Model }
  course_models <- list()
  
  courses <- unique(courseStatsData$course)
  #courses <- c("Augusta National Golf Club")
  
  for (curr_course in courses) {
    message("Training model for: ", curr_course)
    
    tourney_rows <- data %>% 
      filter(
        course == curr_course,
        Round == "Event"
      )
    
    if (nrow(tourney_rows) == 0) next
    
    rows_list <- vector("list", nrow(tourney_rows))
    
    for (j in seq_len(nrow(tourney_rows))) {
      
      curr_row <- tourney_rows[j, ]
      curr_player <- curr_row$player
      curr_date <- curr_row$dates
      sg_tot <- curr_row$sgTot
      
      if(is.na(sg_tot)) next
      
      # sg_{putt/arg/app/ott}_l50, numRds
      player_df <- getPlayerPredictors(curr_player, curr_date)
      
      if (is.null(player_df)) next
      
      # add sg_tot to end, add as row of training_df
      rows_list[[j]] <- data.frame(
        sg_tot = sg_tot,
        player_df,
        stringsAsFactors = FALSE
      )
    }
    
    training_df <- bind_rows(rows_list)
    
    if (nrow(training_df) < 20) next
    
    # Train xgboost model ~ sg_tot
    y <- training_df$sg_tot
    
    X <- training_df %>% 
      select(-sg_tot) %>% 
      as.matrix()
    
    dtrain <- xgb.DMatrix(data = X, label = y)
    
    params <- list(
      objective = "reg:squarederror",
      max_depth = 3,
      eta = 0.03,
      subsample = 0.7,
      colsample_bytree = 0.7,
      min_child_weight = 10,
      lambda = 1.0,
      alpha = 0.1
    )
    
    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 1000,
      early_stopping_rounds = 30,
      watchlist = list(train = dtrain),
      verbose = 0
    )
    
    # Compute SHAP values for this course
    shap_values <- shap.values(xgb_model = model, X_train = X)
    
    #   rfvalue: raw feature value, stdfvalue: standardized feature value
    #   value: shap value (contribution to prediction)
    #   mean_value: mean of feature (used as baseline)
    shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = X)
    
    # Compute aggregated SHAP importance per feature (mean absolute SHAP)
    #   How many strokes that statistic typically adds or subtracts from a
    #   player's predicted performance on that course, relative to an average
    #   player
    shap_agg <- shap_long %>%
      group_by(variable) %>%
      summarise(mean_abs_shap = mean(abs(value))) %>%
      arrange(desc(mean_abs_shap)) %>% 
      pivot_wider(
        names_from = variable,
        values_from = mean_abs_shap
      )
    
    # Save model, features, SHAP info, and aggregated SHAP importance
    course_models[[curr_course]] <- list(
      model = model,
      features = colnames(X),
      shap_values = shap_values,
      shap_long = shap_long,
      shap_agg = shap_agg
    )
  }
  
  return(course_models)
}

getPlayerPredictors <- function(curr_player, curr_date) {
  rounds <- getLastNRoundsPriorTo(curr_player, curr_date, 50)
  
  if (nrow(rounds) == 0) {
    return(NULL)
  }
  
  rounds_clean <- rounds %>% 
    filter(
      !is.na(sgPutt),
      !is.na(sgArg),
      !is.na(sgApp),
      !is.na(drDist),
      !is.na(drAcc)
    )
  
  if (nrow(rounds_clean) == 0) {
    return(NULL)
  }
  
  agg <- rounds_clean %>%
    summarise(
      sg_putt_l50 = mean(sgPutt),
      sg_arg_l50  = mean(sgArg),
      sg_app_l50  = mean(sgApp),
      dr_dist_l50 = mean(drDist),
      dr_acc_l50  = mean(drAcc),
      numRds      = n()
    )
  
  if (agg$numRds < 10) return(NULL)
  
  agg <- agg %>% select(-numRds)
  
  return(agg)
}
