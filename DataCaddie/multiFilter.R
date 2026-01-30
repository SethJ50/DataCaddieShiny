source("utils.R")

serverMultiFilter <- function(input, output, session, favorite_players,
                              playersInTournament, playersInTournamentTourneyNameConv,
                              playersInTournamentPgaNames) {
  
  # Get Round by Round Data for players in the tournament, with variables
  # for each of the different filters
  rdByRd_playerData <- getRoundByRoundData(playersInTournament, data)
  
  # ----- Set Info Boxes ----- 
  observeEvent(input$mf_tool_info, {
    showModal(modalDialog(
      title = "Multi-Filter Tool Info",
      tagList(
        tags$p(
          tags$h5("Overview:")
        ),
        tags$p(
          "This tool allows filtering a player's past rounds by multiple facets to analyze player performance
          in different scenarios, including course setup, field strength, and more. After setting the filters
          of interest, you can select from one of the various views to analyze performance from a different
          perspective. View descriptions for each view by clicking the info icon below the dropdown after you select it."
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$mf_sg_info, {
    showModal(modalDialog(
      title = "Strokes Gained View Info",
      tagList(
        tags$p(
          tags$h5("Overview:")
        ),
        tags$p(
          "This table shows each player's round-by-round average SG statistics in their
          last {RecRds} rounds played within the currently selected filters,
          as well as how these values compare to their performance across their last {BaseRds},
          without a filter  (across all rounds)."
        ),
        tags$hr(),
        tags$p(
          tags$h5("Columns:")
        ),
        tags$ul(
          tags$li(tags$strong("SG Columns:"), " Average round-by-round SG in this category over the past {recRds} rounds for rounds within current filter."),
          tags$li(tags$strong("Unlabeled Columns:"), " These columns aim to show how the player performs in their last {recRds} rounds within the 
                                                       current filter relative to their baseline across ALL rounds (last {BaseRds} rounds). For example,
                                                       the column to the right of SG PUTT can be interpreted as: In this player's most recent {RecRds} rounds
                                                       played with currently set filters, they have gained {Column Value} more strokes putting then their last
                                                      {BaseRds} rounds (without any filters)."),
          tags$li(tags$strong("RecRds:"), " The number of rounds within the currently set filters used to determine averages for SG statistics"),
          tags$li(tags$strong("BaseRds:"), " The number of rounds used to calculate baseline averages (across ALL rounds) for computation of Unlabeled Columns.")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$mf_ott_info, {
    showModal(modalDialog(
      title = "Off-The-Tee View Info",
      tagList(
        tags$p(
          tags$h5("Overview:")
        ),
        tags$p(
          "This table shows each player's round-by-round average off-the-tee related statistics in their
          last {RecRds} rounds played within the currently selected filters,
          as well as how these values compare to their performance across their last {BaseRds},
          without a filter  (across all rounds)."
        ),
        tags$hr(),
        tags$p(
          tags$h5("Columns:")
        ),
        tags$ul(
          tags$li(tags$strong("DR. DIST:"), " Average driving distance."),
          tags$li(tags$strong("DR. ACC:"), " Average driving accuracy."),
          tags$li(tags$strong("SG: OTT:"), " Round-by-round average strokes gained off-the-tee."),
          tags$li(tags$strong("Unlabeled Columns:"), "These columns aim to show how the player's metrics compare in their last {recRds} rounds within the 
                                                       current filter relative to their baseline across ALL rounds (last {BaseRds} rounds). For example,
                                                       the column to the right of DR DIST can be interpreted as: In this player's most recent {RecRds} rounds
                                                       played with currently set filters, their driving distance has been {Column Value} yards longer then their last
                                                      {BaseRds} rounds (without any filters)."),
          tags$li(tags$strong("RecRds:"), " The number of rounds within the currently set filters used to determine averages for DR DIST, DR ACC, and SG OTT values."),
          tags$li(tags$strong("BaseRds:"), " The number of rounds used to calculate baseline averages (across ALL rounds) for computation of Unlabeled Columns.")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$mf_trends_info, {
    showModal(modalDialog(
      title = "Trends View Info",
      tagList(
        tags$p(
          tags$h5("Overview:")
        ),
        tags$p(
          "This table shows how a player's game is trending in their most recent {RecRds} rounds in currently set filters as compared to their baseline
          (their most recent {BaseRds} rounds in currently set filters). It also includes a composite metric, SG HEAT, reflecting how 'hot' a player
          is, i.e., how well they've been playing above their baseline recently as a whole."
        ),
        tags$hr(),
        tags$p(
          tags$h5("Columns:")
        ),
        tags$ul(
          tags$li(tags$strong("SG: PUTT, ARG, APP, OTT:"), " The difference between the player's average round-by-round
                  values in the most recent {RecRds} rounds within set filters and their average round-by-round baseline (most recent {BaseRds} rounds)
                  within set filters."),
          tags$li(tags$strong("SG HEAT:"), " The sum of all SG values relative to baseline, reflecting how the entirety
                  of their game is trending within set filters. Higher values indicate players that are 'trending'."),
          tags$li(tags$strong("RecRds:"), " The number of rounds used to calculate recent averages within set filters for SG stats."),
          tags$li(tags$strong("BaseRds:"), " The number of rounds used to create baseline averages within set filters for a player's overall typical performance.")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$mf_fc_info, {
    showModal(modalDialog(
      title = "Floor/Ceiling View Info",
      tagList(
        tags$p(
          tags$h5("Overview:")
        ),
        tags$p(
          "This table shows each player's round-by-round average off-the-tee related statistics in their
          last {RecRds} rounds played within the currently selected filters,
          as well as how these values compare to their performance across their last {BaseRds},
          without a filter. The goal of this table is to quantify players based on their upside (ceiling)
          — the amount of time they outperform the field — versus their consistency/safety (floor) — the amount of time
          they finish in the better half of the field — and everything in between. This can help make decisions on which
          players to place in lineups depending on your risk tolerance or desire for safety vs. upside."
        ),
        tags$hr(),
        tags$p(
          tags$h5("Columns:")
        ),
        tags$ul(
          tags$li(tags$strong("SG X+:"), " The percentage of rounds, in the player's most recent {RecRds} rounds within currently set filters,
                  in which a player gained X or more strokes on the field."),
          tags$li(tags$strong("Unlabeled Columns:"), "These columns aim to show how the player's SG Distribution compare in their last {recRds} rounds within the 
                                                       current filter relative to their baseline across ALL rounds (last {BaseRds} rounds). 
                                                       For example, the column to the right of SG 0+ can be interpreted as: In their most recent {RecRds} rounds
                                                       within the currently set filters, this player has gained 0 or more strokes on the field in {Column Value} %
                                                       more rounds than their standard baseline (represented by their {BaseRds} most recent rounds across all rounds)."),
          tags$li(tags$strong("RecRds:"), " The number of rounds within currently set filters used to determine SG X+ values."),
          tags$li(tags$strong("BaseRds:"), " The number of rounds used to calculate baseline averages (across ALL rounds) for computation of Unlabeled Columns.")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Set Base Rds and Rec Rds Defaults
  observeEvent(input$stat_view_filter, {
    if (input$stat_view_filter == "Strokes Gained") {
      updateNumericInput(session, "rec_rds_mf", value = 50)
      updateNumericInput(session, "base_rds_mf", value = 100)
    } else if (input$stat_view_filter == "Off-the-Tee") {
      updateNumericInput(session, "rec_rds_mf", value = 50)
      updateNumericInput(session, "base_rds_mf", value = 100)
    } else if (input$stat_view_filter == "Trends") {
      updateNumericInput(session, "rec_rds_mf", value = 24)
      updateNumericInput(session, "base_rds_mf", value = 100)
    } else if (input$stat_view_filter == "Floor/Ceiling") {
      updateNumericInput(session, "rec_rds_mf", value = 50)
      updateNumericInput(session, "base_rds_mf", value = 100)
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
    
    # Design object for filtering data
    filter_spec <- list(
      course_diff_filter = list(col = "courseDiff",
                                map = c(Easy = "easy", Medium = "medium", Hard = "hard")),
      
      course_length_filter = list(col = "courseLength",
                                  map = c(Short = "short", Medium = "medium", Long = "long")),
      
      course_par_filter = list(
        col = "par",
        transform = as.integer   # convert input to numeric
      ),
      
      avg_dr_dist_filter = list(col = "avgDriveLength",
                                     map = c(Short = "short", Medium = "medium", Long = "long")),
      
      avg_dr_acc_filter = list(col = "avgDrivingAcc",
                                    map = c(Low = "low", Medium = "medium", High = "high")),
      
      fw_width_filter = list(col = "avgFwWidth",
                                 map = c(Narrow = "narrow", Medium = "medium", Wide = "wide")),
      
      missed_fw_pen_filter = list(col = "missedFwPenalty",
                                      map = c(Low = "low", Medium = "medium", High = "high")),
      
      sg_ott_ease_filter = list(col = "sgOttEase",
                                map = c(Hard = "hard", Medium = "medium", Easy = "easy")),
      
      sg_app_ease_filter = list(col = "sgAppEase",
                                map = c(Hard = "hard", Medium = "medium", Easy = "easy")),
      
      sg_arg_ease_filter = list(col = "sgArgEase",
                                map = c(Hard = "hard", Medium = "medium", Easy = "easy")),
      
      sg_putt_ease_filter = list(col = "sgPuttEase",
                                 map = c(Hard = "hard", Medium = "medium", Easy = "easy")),
      
      field_strength_filter = list(col = "fieldType",
                                   map = c(Weak = "easy", Medium = "medium", Strong = "hard"))
    )
    
    # Iterate over all inputs, filtering data if applicable
    for (input_name in names(filter_spec)) {
      
      selected <- input[[input_name]] # Current Input Selection
      spec     <- filter_spec[[input_name]] # Specification of column name and map
      
      # Skip "All" or NULL - Don't add any filter for this if 'All' Selected
      if (is.null(selected) || selected == "All") next
      
      if (!is.null(spec$map)) { # If map exists, filter column to specified category
        # Categorical mapping
        if (selected %in% names(spec$map)) {
          filtered_data <- filtered_data %>%
            dplyr::filter(.data[[spec$col]] == spec$map[[selected]])
        }
        
      } else { # Make direct comparison for par
        # Direct comparison (e.g., par)
        value <- spec$transform(selected)
        
        filtered_data <- filtered_data %>%
          dplyr::filter(.data[[spec$col]] == value)
      }
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
  # Create DataFrame comparing SG Statistics in last <rec_rds> rounds
  # IN FILTER to last <base_rds> rounds (ALL Rounds). 'Relative' columns
  # indicate how the player performs under this filter versus overall, where
  # positive indicates being under this filter benefits the player
  
  base_rds <- input$base_rds_mf # Number of Rounds to Sample for All Rounds
  rec_rds <- input$rec_rds_mf # Number of Rounds to Sample for Filter Fitting Rounds
  
  # Get SG Averages Across ALL rounds for Last <base_rds> Rounds
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
  
  # Get SG Averages Across IN FILTER rounds for Last <rec_rds> Rounds
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
  
  
  base_rds <- input$base_rds_mf # Number of Rounds for ALL Rounds baseline
  rec_rds <- input$rec_rds_mf   # Number of Rounds fof IN FILTER Rounds
  
  # Compute Driving Metric Averages across ALL last {base_rds} Rounds (no filters)
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
  
  # Compute Driving Metric Averages across last {rec_rds} IN FILTER Rounds
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
  
  # Join Data and Compute Differences
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
  
  base_rds <- input$base_rds_mf # Number of Rounds Used as Baseline Sample (within filter)
  rec_rds <- input$rec_rds_mf   # Number of Rounds Used as Recent Sample (within filter)
  
  # Compute Baseline averages of SG Statistics for last {base_rds} IN FILTER Rounds
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
  
  # Compute Recent averages of SG Statistics for last {rec_rds} IN FILTER Rounds
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
  
  # Combine and Compute Differences for Trends
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
  
  base_rds <- input$base_rds_mf # Number of Rounds Used For ALL Data Baseline
  rec_rds <- input$rec_rds_mf   # Number of Rounds Used for IN FILTER Data
  
  # Compute Strokes Gained Distribution across ALL last {base_rds} rounds (no filters)
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

  # Compute Strokes Gained Distribution across last IN FILTER {rec_rds} rounds
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
  
  # Combine and compute differences
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













