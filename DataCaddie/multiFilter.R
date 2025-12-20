source("utils.R")

serverMultiFilter <- function(input, output, session, favorite_players,
                              playersInTournament, playersInTournamentTourneyNameConv,
                              playersInTournamentPgaNames) {
  
  rdByRd_playerData <- getRoundByRoundData(playersInTournament, data)
  
  observeEvent(input$stat_view_filter, {
    if (input$stat_view_filter == "Strokes Gained") {
      updateNumericInput(session, "rec_rds_mf", value = 100)
      updateNumericInput(session, "base_rds_mf", value = 250)
    } else if (input$stat_view_filter == "Off-the-Tee") {
      updateNumericInput(session, "rec_rds_mf", value = 100)
      updateNumericInput(session, "base_rds_mf", value = 250)
    } else if (input$stat_view_filter == "Trends") {
      updateNumericInput(session, "rec_rds_mf", value = 24)
      updateNumericInput(session, "base_rds_mf", value = 100)
    } else if (input$stat_view_filter == "Floor/Ceiling") {
      updateNumericInput(session, "rec_rds_mf", value = 100)
      updateNumericInput(session, "base_rds_mf", value = 250)
    } else if (input$stat_view_filter == "SG Above Baseline") {
      # TODO: set defaults here if needed
    }
  })
  
  observe({
    req(input$course_diff_filter)
    req(input$field_strength_filter)
    req(input$stat_view_filter)
    req(input$rec_rds_mf)
    req(input$base_rds_mf)
    
    filtered_data <- rdByRd_playerData
    
    # Apply Course Difficulty Filter
    if(input$course_diff_filter == "Easy") {
      filtered_data <- filtered_data %>% 
        filter(courseDiff == "easy")
    } else if(input$course_diff_filter == "Medium") {
      filtered_data <- filtered_data %>% 
        filter(courseDiff == "medium")
    } else if(input$course_diff_filter == "Hard") {
      filtered_data <- filtered_data %>% 
        filter(courseDiff == "hard")
    }
    
    # Apply Field Strength Filter
    if(input$field_strength_filter == "Weak") {
      filtered_data <- filtered_data %>% 
        filter(fieldType == "easy")
    } else if(input$field_strength_filter == "Medium") {
      filtered_data <- filtered_data %>% 
        filter(fieldType == "medium")
    } else if(input$field_strength_filter == "Strong") {
      filtered_data <- filtered_data %>% 
        filter(fieldType == "hard")
    }
    
    # Display appropriate Stat View
    if(input$stat_view_filter == "Strokes Gained") {
      table_data <- makeStrokesGainedFilterDf(input, filtered_data, rdByRd_playerData)
    } else if(input$stat_view_filter == "Off-the-Tee") {
      table_data <- makeOttFilterDf(input, filtered_data, rdByRd_playerData)
    } else if(input$stat_view_filter == "Trends") {
      table_data <- makeTrendsFilterDf(input, filtered_data, rdByRd_playerData)
    } else if(input$stat_view_filter == "Floor/Ceiling") {
      table_data <- makeFloorCeilFilterDf(input, filtered_data, rdByRd_playerData)
    }
    
    output$multifilter_results_tbl <- renderReactable({
      createMultifilterTable(table_data, favorite_players)
    })
    
  })
  
}



#### Supplementary Functions ===================================================

createMultifilterTable <- function(table_data, favorite_players) {
  base_col_defs <- list(
    Player = colDef(name = "Player", width = 160 ),
    fdSalary = colDef( name = "FD Salary", width = 80),
    dkSalary = colDef(name = "DK Salary", width = 80),
    recRds = colDef(name = "RecRds", width = 70),
    baseRds = colDef(name = "BaseRds", width = 70)
  )
  
  other_cols <- setdiff(names(table_data), names(base_col_defs))
  
  other_col_defs <- setNames(
    lapply(other_cols, function(col) {
      
      col_min <- min(table_data[[col]], na.rm = TRUE)
      col_max <- max(table_data[[col]], na.rm = TRUE)
      
      name <- ifelse(grepl("_diff$", col), "", col)
      width <- ifelse(grepl("_diff$", col), 45, 75)
      color_func <- makeColorFunc(min_val = col_min, max_val = col_max)
      font_size <- ifelse(grepl("_diff$", col), "8px", "12px")
      
      makeColDef(col, name, width = width, color_func = color_func, font_size = font_size)
    }),
    other_cols
  )
  
  favs <- favorite_players$names
  
  table_data <- table_data %>% 
    mutate(
      isFavorite = player %in% favs
    )
  
  table_data$.favorite <- NA
  
  table_data <- table_data[, c(".favorite", setdiff(names(table_data), ".favorite"))]
  
  table_data <- table_data %>% rename(Player = player)
  
  all_col_defs <- c(base_col_defs, other_col_defs)
  
  table <- makeBasicTable(table_data, all_col_defs, "fdSalary", favorite_players,
                 hasFavorites = TRUE, font_size = 12, row_height = 20)
  
  
  return(table)
}

makeStrokesGainedFilterDf <- function(input, filtered_data, full_data) {
  base_rds <- input$base_rds_mf
  rec_rds <- input$rec_rds_mf
  
  full_data_summary <- full_data %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>% 
    select(player, fdSalary, dkSalary, sgPutt, sgArg, sgApp, sgOtt, sgT2G, sgTot) %>% 
    group_by(player) %>% 
    slice_head(n = base_rds) %>% 
    summarise(
      fdSalary = last(fdSalary),
      dkSalary = last(dkSalary),
      sgPutt = round(mean(sgPutt), 2),
      sgArg = round(mean(sgArg), 2),
      sgApp = round(mean(sgApp), 2),
      sgOtt = round(mean(sgOtt), 2),
      sgT2G = round(mean(sgT2G), 2),
      sgTot = round(mean(sgTot), 2),
      baseRds = n(),
      .groups = "drop"
    )
  
  final_data <- filtered_data %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>% 
    select(player, fdSalary, dkSalary, sgPutt, sgArg, sgApp, sgOtt, sgT2G, sgTot) %>% 
    group_by(player) %>% 
    slice_head(n = rec_rds) %>%
    summarise(
      fdSalary = last(fdSalary),
      dkSalary = last(dkSalary),
      sgPutt = round(mean(sgPutt), 2),
      sgArg = round(mean(sgArg), 2),
      sgApp = round(mean(sgApp), 2),
      sgOtt = round(mean(sgOtt), 2),
      sgT2G = round(mean(sgT2G), 2),
      sgTot = round(mean(sgTot), 2),
      recRds = n(),
      .groups = "drop"
    )
  
  final_data <- final_data %>% 
    left_join(full_data_summary, by = "player", suffix = c("", "_full")) %>% 
    mutate(
      sgPutt_diff = round(sgPutt - sgPutt_full, 2),
      sgArg_diff = round(sgArg - sgArg_full, 2),
      sgApp_diff = round(sgApp - sgApp_full, 2),
      sgOtt_diff = round(sgOtt - sgOtt_full, 2),
      sgT2G_diff = round(sgT2G - sgT2G_full, 2),
      sgTot_diff = round(sgTot - sgTot_full, 2)
    ) %>% 
    select(player, fdSalary, dkSalary, 
           sgPutt, sgPutt_diff,
           sgArg, sgArg_diff,
           sgApp, sgApp_diff,
           sgOtt, sgOtt_diff,
           sgT2G, sgT2G_diff,
           sgTot, sgTot_diff,
           recRds, baseRds) %>% 
    rename(
      `SG PUTT` = sgPutt,
      `SG ARG` = sgArg,
      `SG APP` = sgApp,
      `SG OTT` = sgOtt,
      `SG T2G` = sgT2G,
      `SG TOT` = sgTot
    )
  
  return(final_data)
}

makeOttFilterDf <- function(input, filtered_data, full_data) {
  base_rds <- input$base_rds_mf
  rec_rds <- input$rec_rds_mf
  
  full_data_summary <- full_data %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>% 
    select(player, fdSalary, dkSalary, drDist, drAcc, sgOtt) %>% 
    group_by(player) %>%
    slice_head(n = base_rds) %>% 
    summarise(
      fdSalary = last(fdSalary),
      dkSalary = last(dkSalary),
      drDist = round(mean(drDist, na.rm = TRUE), 2),
      drAcc = round(mean(drAcc, na.rm = TRUE), 2),
      sgOtt = round(mean(sgOtt, na.rm = TRUE), 2),
      baseRds = n(),
      .groups = "drop"
    )
  
  final_data <- filtered_data %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>% 
    select(player, fdSalary, dkSalary, drDist, drAcc, sgOtt) %>% 
    group_by(player) %>% 
    slice_head(n = rec_rds) %>% 
    summarise(
      fdSalary = last(fdSalary),
      dkSalary = last(dkSalary),
      drDist = round(mean(drDist, na.rm = TRUE), 2),
      drAcc = round(mean(drAcc, na.rm = TRUE), 2),
      sgOtt = round(mean(sgOtt, na.rm = TRUE), 2),
      recRds = n(),
      .groups = "drop"
    )
  
  final_data <- final_data %>% 
    left_join(full_data_summary, by = "player", suffix = c("", "_full")) %>% 
    mutate(
      drDist_diff = round(drDist - drDist_full, 2),
      drAcc_diff = round(drAcc - drAcc_full, 2),
      sgOtt_diff = round(sgOtt - sgOtt_full, 2)
    ) %>% 
    select(player, fdSalary, dkSalary, 
           drDist, drDist_diff,
           drAcc, drAcc_diff,
           sgOtt, sgOtt_diff,
           recRds, baseRds) %>% 
    rename(
      `DR DIST` = drDist,
      `DR ACC` = drAcc,
      `SG OTT` = sgOtt
    )
  
  return(final_data)
}

makeTrendsFilterDf <- function(input, filtered_data, full_data) {
  base_rds <- input$base_rds_mf
  rec_rds <- input$rec_rds_mf
  
  base_data <- filtered_data %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>% 
    select(player, fdSalary, dkSalary, sgPutt, sgArg, sgApp, sgOtt, sgT2G, sgTot) %>% 
    group_by(player) %>% 
    slice_head(n = base_rds) %>% 
    summarise(
      fdSalary = last(fdSalary),
      dkSalary = last(dkSalary),
      sgPutt = round(mean(sgPutt), 2),
      sgArg = round(mean(sgArg), 2),
      sgApp = round(mean(sgApp), 2),
      sgOtt = round(mean(sgOtt), 2),
      sgT2G = round(mean(sgT2G), 2),
      sgTot = round(mean(sgTot), 2),
      baseRds = n(),
      .groups = "drop"
    )
  
  rec_data <- filtered_data %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>% 
    select(player, fdSalary, dkSalary, sgPutt, sgArg, sgApp, sgOtt, sgT2G, sgTot) %>% 
    group_by(player) %>% 
    slice_head(n = rec_rds) %>%
    summarise(
      fdSalary = last(fdSalary),
      dkSalary = last(dkSalary),
      sgPutt = round(mean(sgPutt), 2),
      sgArg = round(mean(sgArg), 2),
      sgApp = round(mean(sgApp), 2),
      sgOtt = round(mean(sgOtt), 2),
      sgT2G = round(mean(sgT2G), 2),
      sgTot = round(mean(sgTot), 2),
      recRds = n(),
      .groups = "drop"
    )
  
  final_data <- rec_data %>% 
    left_join(base_data, by = "player", suffix = c("", "_full")) %>% 
    mutate(
      sgPutt_diff = round(sgPutt - sgPutt_full, 2),
      sgArg_diff = round(sgArg - sgArg_full, 2),
      sgApp_diff = round(sgApp - sgApp_full, 2),
      sgOtt_diff = round(sgOtt - sgOtt_full, 2),
      sgT2G_diff = round(sgT2G - sgT2G_full, 2),
      sgTot_diff = round(sgTot - sgTot_full, 2),
      sgHeat = round(sgPutt_diff + sgArg_diff + sgApp_diff + sgOtt_diff, 2)
    ) %>% 
    select(player, fdSalary, dkSalary, 
           sgPutt_diff,
           sgArg_diff,
           sgApp_diff,
           sgOtt_diff,
           sgHeat,
           recRds, baseRds) %>% 
    rename(
      `SG PUTT` = sgPutt_diff,
      `SG ARG` = sgArg_diff,
      `SG APP` = sgApp_diff,
      `SG OTT` = sgOtt_diff,
      `SG HEAT` = sgHeat
    )
  
  return(final_data)
}

makeFloorCeilFilterDf <- function(input, filtered_data, full_data) {
  base_rds <- input$base_rds_mf
  rec_rds <- input$rec_rds_mf
  
  full_data_summary <- full_data %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>% 
    group_by(player) %>% 
    slice_head(n = base_rds) %>% 
    ungroup() %>% 
    select(player, fdSalary, dkSalary, sgTot) %>% 
    group_by(player) %>% 
    summarise(
      fdSalary = last(fdSalary),
      dkSalary = last(dkSalary),
      plus0 = round(mean(sgTot >= 0, na.rm = TRUE), 2),
      plus1 = round(mean(sgTot >= 1, na.rm = TRUE), 2),
      plus2 = round(mean(sgTot >= 2, na.rm = TRUE), 2),
      plus3 = round(mean(sgTot >= 3, na.rm = TRUE), 2),
      plus4 = round(mean(sgTot >= 4, na.rm = TRUE), 2),
      plus5 = round(mean(sgTot >= 5, na.rm = TRUE), 2),
      baseRds = n(),
      .groups = "drop"
    )

  
  final_data <- filtered_data %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>% 
    group_by(player) %>% 
    slice_head(n = rec_rds) %>% 
    ungroup() %>% 
    select(player, fdSalary, dkSalary, sgTot) %>% 
    group_by(player) %>% 
    summarise(
      fdSalary = last(fdSalary),
      dkSalary = last(dkSalary),
      plus0 = round(mean(sgTot >= 0, na.rm = TRUE), 2),
      plus1 = round(mean(sgTot >= 1, na.rm = TRUE), 2),
      plus2 = round(mean(sgTot >= 2, na.rm = TRUE), 2),
      plus3 = round(mean(sgTot >= 3, na.rm = TRUE), 2),
      plus4 = round(mean(sgTot >= 4, na.rm = TRUE), 2),
      plus5 = round(mean(sgTot >= 5, na.rm = TRUE), 2),
      recRds = n(),
      .groups = "drop"
    )
  
  final_data <- final_data %>% 
    left_join(full_data_summary, by = "player", suffix = c("", "_full")) %>% 
    mutate(
      plus0_diff = round(plus0 - plus0_full, 2),
      plus1_diff = round(plus1 - plus1_full, 2),
      plus2_diff = round(plus2 - plus2_full, 2),
      plus3_diff = round(plus3 - plus3_full, 2),
      plus4_diff = round(plus4 - plus4_full, 2),
      plus5_diff = round(plus5 - plus5_full, 2)
    ) %>% 
    select(player, fdSalary, dkSalary, 
           plus0, plus0_diff,
           plus1, plus1_diff,
           plus2, plus2_diff,
           plus3, plus3_diff,
           plus4, plus4_diff,
           plus5, plus5_diff,
           recRds, baseRds) %>% 
    rename(
      `SG 0+` = plus0,
      `SG 1+` = plus1,
      `SG 2+` = plus2,
      `SG 3+` = plus3,
      `SG 4+` = plus4,
      `SG 5+` = plus5
    )
  
  return(final_data)
}













