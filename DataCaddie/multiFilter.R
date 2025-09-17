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
  
  reversed_cols = c()
  
  base_col_defs <- list(
    player = colDef(
      name = "Player",
      width = 160
    ),
    fdSalary = colDef(
      name = "FD Salary",
      width = 80
    ),
    dkSalary = colDef(
      name = "DK Salary",
      width = 80
    ),
    recRds = colDef(
      name = "RecRds",
      width = 70
    ),
    baseRds = colDef(
      name = "BaseRds",
      width = 70
    )
  )
  
  other_cols <- setdiff(names(table_data), names(base_col_defs))
  
  other_col_defs <- setNames(
    lapply(other_cols, function(col) {
      
      col_min <- min(table_data[[col]], na.rm = TRUE)
      col_max <- max(table_data[[col]], na.rm = TRUE)
      
      colDef(
        name = ifelse(grepl("_diff$", col), "", col),
        width = ifelse(grepl("_diff$", col), 45, 75),
        style = function(value) {
          if (is.na(value) || col_min == col_max) {
            bg <- "white"
          } else {
            # Scale Value
            val_scaled <- (value - col_min) / (col_max - col_min)
            
            # Reverse scale if column is in reversed_cols
            if (col %in% reversed_cols) val_scaled <- 1 - val_scaled
            
            bg <- rgb(
              colorRamp(c("#F83E3E", "white", "#4579F1"))(val_scaled),
              maxColorValue = 255
            )
          }
          list(
            fontSize = ifelse(grepl("_diff$", col), "8px", "12px"),
            background = bg
          )
        }
      )
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
  
  favorite_col_def <- list(
    .favorite = colDef(
      name = "",
      width = 28,
      sticky = "left",
      sortable = FALSE,
      resizable = FALSE,
      cell = function(value, index) {
        player_name <- table_data$player[index]
        is_fav <- player_name %in% favorite_players$names
        star <- if (is_fav) "★" else "☆"
        
        htmltools::div(
          htmltools::span(
            star,
            style = "cursor:pointer; font-size: 18px; color: gold;",
            onclick = sprintf(
              "Shiny.setInputValue('favorite_clicked', '%s', {priority: 'event'})",
              player_name
            )
          )
        )
      }
    )
  )
  
  all_col_defs <- c(base_col_defs, other_col_defs, favorite_col_def)
  
  all_col_defs[["isFavorite"]] <- colDef(show = FALSE)
  
  table <- reactable(
    table_data,
    searchable = TRUE,
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
    defaultSorted = "fdSalary",
    showSortIcon = FALSE,
    theme = reactableTheme(
      style = list(fontSize = "12px"),
      rowStyle = list(height = "20px")
    ),
    defaultColDef = colDef(vAlign = "center", align = "center", width = 80),
    columns = all_col_defs,
    rowClass = JS("
        function(rowInfo, index) {
          if (rowInfo && rowInfo.row && rowInfo.row.isFavorite) {
            return 'favorite-row';
          }
          return null;
        }
      ")
  )
  
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













