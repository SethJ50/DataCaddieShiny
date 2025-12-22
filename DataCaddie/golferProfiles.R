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

serverGolferProfiles <- function(input, output, session, favorite_players,
                                 playersInTournament, playersInTournamentTourneyNameConv,
                                 playersInTournamentPgaNames, all_player_data) {
  
  # Get All Player Data
  all_player_data_reactive <- reactive({
    get_all_player_data(c(), playersInTournament, 50)$data
  })
  
  
  # Golfer Profiles Player Picker and Star
  output$gp_picker_with_star <- renderUI({
    
    # Set Dropdown to unique Tournament Player Names
    player_data_sorted <- salaries %>% 
      arrange(-fdSalary)
    players <- unique(player_data_sorted$player)
    players_tourney_names <- vapply(players, nameFanduelToTournament, character(1))
    
    # Set first player as default selected
    selected_player <- input$gp_player
    if (is.null(selected_player) || !(selected_player %in% players_tourney_names)) {
      selected_player <- players_tourney_names[1]
    }
    
    
    favs <- favorite_players$names
    star <- if (selected_player %in% favs) "★" else "☆"
    
    # Set star on click action
    onclick_js <- sprintf(
      "Shiny.setInputValue('favorite_clicked', '%s', {priority: 'event'})",
      selected_player
    )
    
    # Output Star and Player Dropdown Next to each other
    tagList(
      div(style = "display: flex;
                   gap: 0px;
                   align-items: center;
          ",
          tags$span(
            star,
            style = "
              font-size: 35px;
              color: gold;
              user-select: none;
              cursor: pointer;
              display: flex;
              align-items: center;
              height: 50px;
              line-height: 1;
              margin-right: -3px;
              z-index: 10;
            ",
            onclick = onclick_js
          ),
          shinyWidgets::pickerInput(
            inputId = "gp_player",
            label = NULL,
            choices = players_tourney_names,
            selected = selected_player,
            options = list(`live-search` = TRUE)
          )
      )
    )
  })
  
  # Golfer Profiles Salary Cards
  output$gp_salaries <- renderUI({
    req(input$gp_player)
    
    # Convert to FD Name to get salary
    fdName <- nameToFanduel(input$gp_player)
    
    player_salaries <- salaries %>%
      filter(player == fdName) %>%
      slice(1)
    
    if (nrow(player_salaries) == 0) {
      return("No salary data available")
    }
    
    div(
      style = "
      width: 300px;
      margin-left: auto;
      margin-right: auto;
      margin-top: -15px;
      z-index: 10;
    ",
      div(
        style = "
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        justify-content: center;
      ",
        # FanDuel mini-card
        div(
          style = "
          flex: 1 1 80px;
          border: 1px solid #007bff;
          border-radius: 6px;
          padding: 1px 4px 4px 4px;
          color: #007bff;
          text-align: center;
          background-color: #f9f9f9;
        ",
          tags$strong("FanDuel", style = "line-height: 1; font-size: 0.9em;"),
          tags$p(
            paste0("$", player_salaries$fdSalary),
            style = "font-size: 1.1em; font-weight: 600; margin: 0; line-height: 1;"
          )
        ),
        # DraftKings mini-card
        div(
          style = "
          flex: 1 1 80px;
          border: 1px solid #28a745;
          border-radius: 6px;
          padding: 1px 4px 4px 4px;
          color: #28a745;
          text-align: center;
          background-color: #f9f9f9;
        ",
          tags$strong("DraftKings", style = "line-height: 1; font-size: 0.9em;"),
          tags$p(
            paste0("$", player_salaries$dkSalary),
            style = "font-size: 1.1em; font-weight: 600; margin: 0; line-height: 1;"
          )
        )
      )
    )
  })
  
  # Golfer Spider/Radar Plot
  output$gp_spider_plot <- renderUI({
    req(input$gp_player)
    
    # Return Spider Plot Data - A DataFrame with
    #   Stat Labels
    #   Player Data
    #   Player Data Scaled (Norm)
    #   Average Data
    #   Average Data Scaled
    finalData <- get_spider_plot_data(favorite_players, playersInTournament, input$gp_player, all_player_data_reactive)
    
    # r: Radius - the stat value
    # theta - where it is (in terms of angle) on the circle
    
    # Create list of points around plot
    #   'closed' by adding first point again to end to close circle
    r_closed <- c(finalData$player_s, finalData$player_s[1])
    theta_closed <- c(finalData$stat, finalData$stat[1])
    player_vals_closed <- c(finalData$player, finalData$player[1])
    stat_labels_closed <- c(finalData$stat, finalData$stat[1])
    r_closedF <- c(finalData$avg_s, finalData$avg_s[1])
    field_vals_closed <- c(finalData$average, finalData$average[1])
    
    # Output Radar Plot
    output$gp_summary_radar <- renderPlotly({
      plot_ly(
        type = 'scatterpolar',
        fill = 'toself',
        showlegend = TRUE
      ) %>%
        add_trace(
          r = r_closed,
          theta = theta_closed,
          name = input$gp_player,
          mode = "lines+markers",
          text = paste0(input$gp_player, ": <br>",
                        stat_labels_closed,": ", player_vals_closed, "<br>"),
          hoverinfo = "text",
          marker = list(color = "navy"),
          line = list(color = "navy"),
          fillcolor = "rgba(0, 0, 128, 0.3)",
          connectgaps = TRUE
        ) %>%
        add_trace(
          r = r_closedF,
          theta = theta_closed,
          name = 'Field',
          mode = "lines+markers",
          text = paste0("Field: <br>",
                        stat_labels_closed, ": ", field_vals_closed, "<br>"),
          hoverinfo = "text",
          marker = list(color = "#FF6666"),
          line = list(color = "#FF6666"),
          fillcolor = "rgba(255, 102, 102, 0.2)",
          connectgaps = TRUE
        ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(-3.2, 3.2),
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
    })
    
    tagList(
      div(
        style = "width: 100%, max-width: 350px; margin: auto; text-align: center;",
        plotlyOutput("gp_summary_radar", height = "250px", width = "100%")
      )
    )
  })
  
  # Renders Player Stat Summary Table (dropdown table before bar plots)
  render_gp_stat_table(input, output, input$gp_player, playersInTournament, stat_config, all_player_data_reactive)
  
  # Render Bar Plots
  allData <- all_player_data_reactive()
  
  # For each stat. category, attempt to setup 'bar' plots for category (if tab is active)
  # Note: stat_config is in utils.R
  for (stat_category in names(stat_config)) {
    
    # Grab component that configures this bar plot
    config <- stat_config[[stat_category]]
    
    local({
      config_inner <- config
      
      makeBarPlot(stat_category, config_inner, input, output, playersInTournament, all_player_data_reactive)
    })
  }
  
  
  # Render Event and Round by Round SG Table
  renderGpRecentRounds(input, output, playersInTournament)
  
}




#### Helper Functions ----------------------------------------------------------



## Spider Plot -----------------------------------------------------------------
get_spider_plot_data <- function(favorite_players, players_in_tournament, currPlayer, all_player_data_reactive) {
  # Grab All Player Data
  allData <- all_player_data_reactive()
  
  # Get Stat Columns for Spider Plot
  stat_cols <- c("sgPutt", "sgArg", "sgApp", "sgOtt", "Dr. Dist", "Dr. Acc")
  norm_stat_cols <- paste0(stat_cols, "_norm")
  
  # Get Averages Stat Values of Each Column
  averages <- sapply(stat_cols, function(col) {
    round(mean(allData[[col]], na.rm = TRUE), 2)
  })
  
  # Get Player Only Data
  playerData <- allData %>%
    filter(player == currPlayer) %>%
    select(all_of(stat_cols))
  
  # Get Vector of Player Data Stat Values
  playerDataVec <- round(as.numeric(playerData[1, ]), 2)
  
  # Get Player Only Norm Data
  playerNormData <- allData %>%
    filter(player == currPlayer) %>%
    select(all_of(norm_stat_cols))
  
  # Get Vector of Player Data Norm Stat Values
  playerNormVec <- as.numeric(playerNormData[1, ])
  
  # Create Labels for Stats
  stat_labels <- c("SG: PUTT", "SG: ARG", "SG: APP", "SG: OTT", "DR. DIST", "DR. ACC")
  
  # Return DataFrame: Stat Labels, Player Data Vect, Player Norm (scaled) Data Vect,
  #   averages vector, averages scaled vect
  finalData <- data.frame(
    stat = stat_labels,
    player = playerDataVec,
    player_s = playerNormVec,
    average = averages,
    avg_s = rep(0, length(stat_labels))
  )
  
  return(finalData)
}

## Bar Plots -------------------------------------------------------------------
makeBarPlot <- function(stat_category, config, input, output, playersInTournament, all_player_data_reactive) {
  
  # Setup UI Component for Bar Plot
  plot_id <- paste0("gp_", tolower(stat_category), "_multi_plot")
  ui_id <- paste0("gp_stat_", tolower(stat_category))
  
  # Make area for Plot Output if Stat is Selected
  output[[ui_id]] <- renderUI({
    req(input$gp_stat_selected == stat_category)
    
    make_barplot_area(plot_id, config$num_stats, config$title)
  })
  
  # Render Plot Output
  render_barplot_output(input, output, plot_id, playersInTournament, stat_category, config, all_player_data_reactive)
}

make_barplot_area <- function(plot_id, num_stats, title = NULL) {
  # Dynamically generates UI component plot area for plotting stat bars
  
  height_per <- 25
  total_height <- height_per * num_stats
  
  tagList(
    if (!is.null(title)) {
      tags$h5(title, class = "text-dark fw-bold mb-2")
    },
    div(
      style = "margin-top: -25px;",
      plotOutput(plot_id, height = paste0(total_height, "px"))
    )
  )
}

render_barplot_output <- function(input, output, plot_id, playersInTournament, tab_name, config, all_player_data_reactive) {
  
  # Grabs plot data and renders plot in output for 'bar' plots in golfer profiles
  
  output[[plot_id]] <- renderPlot({
    
    req(input$gp_stat_selected == tab_name)
    req(input$gp_player)
    
    plot_data <- getBarPlotData(
      input$gp_player,
      playersInTournament,
      36,
      config,
      all_player_data_reactive
    )
    
    createBarPlot(plot_data)
  })
}

getBarPlotData <- function(currPlayer, playersInTournament, num_rounds, config, all_player_data_reactive) {
  
  # Get all player data
  #allData <- get_all_player_data(c(), playersInTournament, num_rounds)
  #allData <- allData$data
  allData <- all_player_data_reactive()
  
  # Get current player data
  playerData <- allData %>% 
    filter(player == currPlayer)
  
  # Get stat names, normalized stat names
  allStats <- c(config$stats, config$rev_stats)
  normStats <- paste0(allStats, "_norm")
  stat_labels <- config$stat_labels
  
  # Bind together a row for each stat in all stats to create plot data dataframe
  plot_data <- do.call(rbind, lapply(seq_along(allStats), function(i) {
    stat <- allStats[i]
    normStat <-  normStats[i]
    
    column_data <- suppressWarnings(as.numeric(allData[[stat]]))
    if(all(is.na(column_data))) return(NULL)
    
    avg_val <- mean(column_data, na.rm = TRUE)
    avg_std <- 0
    
    player_val <- playerData[[stat]]
    player_std <- playerData[[normStat]]
    
    data.frame(
      stat = stat_labels[stat],
      player = round(player_val, 2),
      player_s = player_std,
      average = round(avg_val, 2),
      avg_s = avg_std
    )
  }))
  
  if (is.null(plot_data)) return(NULL)
  
  plot_data$stat <- factor(plot_data$stat, levels = unique(rev(stat_labels[allStats])))
  
  return(plot_data)
}

createBarPlot <- function(plot_data) {
  # Creates horizontal 'bar' plot to visualize player stats in golfer profiles
  
  plot_data$y_pos <- as.numeric(plot_data$stat) * 0.6  # numeric y for positioning
  
  bar_thickness <- 0.4
  
  seg_thickness <- 5
  
  ggplot(plot_data, aes(y = y_pos)) +
    
    # Background "track"
    geom_segment(
      aes(x = -3, xend = 3, y = y_pos, yend = y_pos),
      color = "#e9e9e9",
      linewidth = seg_thickness,
      lineend = "round"
    ) +
    
    # Player's bar (colored, can extend left or right from zero)
    geom_segment(
      aes(x = pmin(0, player_s), xend = pmax(0, player_s),
          y = y_pos, yend = y_pos, color = player_s),
      linewidth = seg_thickness,
      lineend = "butt"
    ) +
    
    # Rounded cap at the "outer" end of player bar
    geom_point(
      aes(x = player_s, y = y_pos, colour = player_s),
      size = seg_thickness,  # scale dot size to thickness
      shape = 16,                  # filled circle
      stroke = 0
    ) +
    
    # Reference tick
    geom_segment(aes(x = 0, xend = 0, y = y_pos - 0.15, yend = y_pos + 0.15),
                 color = "grey70", linewidth = 0.8) +
    
    geom_text(aes(x = pmax(0, player_s) + 0.175, 
                  label = player),
              hjust = 0, 
              size = 5,
              family = "Almari-Bold"
    ) +
    
    scale_colour_gradient2(
      low = "#F83E3E",
      mid = "grey80",
      high = "#4579F1",
      midpoint = 0,
      guide = "none"
    ) +
    
    scale_x_continuous(limits = c(-3.25, 4), expand = c(0, 0)) +
    
    scale_y_continuous(breaks = plot_data$y_pos, labels = plot_data$stat) +
    
    theme_minimal(base_size = 12) +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(5, 15, 5, 5),
      axis.text.y = element_text(size = 18, family = "Almari-Bold", color = "black") # Stat Names
    )
}

## Summary Table ---------------------------------------------------------------

render_gp_stat_table <- function(input, output, currPlayer, playersInTournament, config, all_player_data_reactive) {
  
  req(input$gp_player)
  
  # Get all players data
  allData <- all_player_data_reactive()
  
  # Filter Data to Current Player Only
  playerData <- allData %>% 
    filter(player == currPlayer)
  playerData <- as.data.frame(playerData)
  
  # Configure UI Component (reactable)
  ui_id <- "gp_stat_summary"
  
  # Make area for Table Output if 'SUMMARY' tab is Selected
  output[[ui_id]] <- renderUI({
    req(input$gp_stat_selected == "SUMMARY")
    
    tagList(
      div(
        style = "margin-top: -5px; overflow-x: hidden;",
        reactableOutput("gpSummaryTable", width = "100%")
      )
    )
  })
  
  # Render Reactable (only if input$gp_stat_selected == "SUMMARY")
  makeGpSummaryTable(input, output, playerData, config)
}

makeGpSummaryTable <- function(input, output, playerData, config) {
  
  output$gpSummaryTable <- renderReactable({
    req(input$gp_stat_selected == "SUMMARY")
    
    stat_labels <- c(
      "sgOtt" = "SG: OTT",
      "Dr. Dist" = "DR. DIST",
      "Dr. Acc" = "DR. ACC",
      
      "sgApp" = "SG: APP",
      "App. 50-75" = "APP 50-75",
      "App. 75-100" = "APP 75-100",
      "App. 100-125" = "APP 100-125",
      "App. 125-150" = "APP 125-150",
      "App. 150-175" = "APP 150-175",
      "App. 175-200" = "APP 175-200",
      "App. 200_up" = "APP 200+",
      "GIR%" = "GIR %",
      "Proximity" = "PROXIMITY",
      "Rough Prox." = "ROUGH PROX",
      
      "sgArg" = "SG: ARG",
      "Scrambling" = "SCRAMBLING",
      "SandSave%" = "SAND SAVE %",
      
      "sgPutt" = "SG: PUTT",
      "Putt BOB%" = "PUTT BOB%",
      "3 Putt Avd" = "3 PUTT AVD",
      "Bonus Putt." = "BONUS PUTT",
      
      "Par 3 Score" = "PAR 3 AVG",
      "Par 4 Score" = "PAR 4 AVG",
      "Par 5 Score" = "PAR 5 AVG",
      "BOB%" = "BOB %",
      "Bog. Avd" = "BOGEY AVD"
    )
    
    # Map stats to categories
    stat_categories <- c(
      "sgOtt" = "Off-the-Tee", "Dr. Dist" = "Off-the-Tee", "Dr. Acc" = "Off-the-Tee",
      "sgApp" = "Approach", "App. 50-75" = "Approach", "App. 75-100" = "Approach",
      "App. 100-125" = "Approach", "App. 125-150" = "Approach", "App. 150-175" = "Approach",
      "App. 175-200" = "Approach", "App. 200_up" = "Approach", "GIR%" = "Approach",
      "Proximity" = "Approach", "Rough Prox." = "Approach",
      "sgArg" = "Around the Green", "Scrambling" = "Around the Green", "SandSave%" = "Around the Green",
      "sgPutt" = "Putting", "Putt BOB%" = "Putting", "3 Putt Avd" = "Putting", "Bonus Putt." = "Putting",
      "Par 3 Score" = "Scoring", "Par 4 Score" = "Scoring", "Par 5 Score" = "Scoring",
      "BOB%" = "Scoring", "Bog. Avd" = "Scoring"
    )
    
    all_stats <- names(stat_labels)
    
    # Generate table rows and background color for each value
    table_data <- do.call(rbind, lapply(all_stats, function(stat) {
      norm_stat <- paste0(stat, "_norm")
      
      value <- if (stat %in% names(playerData)) {
        round(as.numeric(playerData[[stat]]), 2)
      } else {
        NA
      }
      
      percentile <- if (norm_stat %in% names(playerData)) {
        round(pnorm(as.numeric(playerData[[norm_stat]])) * 100)
      } else {
        NA
      }
      
      bg_color <- if (!is.na(percentile)) {
        # scale percentile 0-100 to 0-1
        val_scaled <- percentile / 100
        color <- rgb(colorRamp(c("#F83E3E", "white", "#4579F1"))(val_scaled), maxColorValue = 255)
        color
      } else {
        "white"
      }
      
      data.frame(
        Stat = stat_labels[[stat]],
        Value = value,
        ValueColor = bg_color,
        Percentile = if (!is.na(percentile)) paste0(percentile, "%") else NA,
        Category = stat_categories[[stat]],
        stringsAsFactors = FALSE
      )
    }))
    
    # Create category-level table
    categories <- unique(table_data$Category)
    category_table <- data.frame(Category = categories, stringsAsFactors = FALSE)
    
    reactable(
      category_table,
      columns = list(Category = colDef(name = "Stat Category", align = "left")),
      details = function(index) {
        cat_name <- category_table$Category[index]
        sub_data <- table_data[table_data$Category == cat_name, c("Stat", "Value", "Percentile", "ValueColor")]
        
        reactable(
          sub_data,
          columns = list(
            Stat = colDef(name = "Stat", align = "center", width = 170),
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
  })
}



#### Recent Rounds Table ------------------------------------------------------
renderGpRecentRounds <- function(input, output, playersInTournament) {
  output$gp_recent_rounds <- renderReactable({
    req(input$gp_player)
    
    # Get Event, Round by Round Data
    eventData <- getEventRoundsData(input$gp_player)
    roundData <- getIndRoundsData(input$gp_player)
    
    # Add missing finish cols
    if (!"finish" %in% names(eventData)) {
      eventData$finish <- NA
    }
    if (!"finish" %in% names(roundData)) {
      roundData$finish <- NA
    }
    
    # Create a unique event key to correlate events and rounds
    eventData$event_key <- paste(eventData$Date, eventData$tournament, sep = "_")
    roundData$event_key <- paste(roundData$Date, roundData$tournament, sep = "_")
    
    col_labels <- c(
      "Dates" = "Date",
      "finish" = "Finish",
      "tournament" = "Tournament",
      "sgPutt" = "SG: PUTT",
      "sgArg" = "SG: ARG",
      "sgApp" = "SG: APP",
      "sgOtt" = "SG: OTT",
      "sgT2G" = "SG: T2G",
      "sgTot" = "SG: TOT"
    )
    
    num_stats <- c("sgPutt","sgArg","sgApp","sgOtt","sgT2G","sgTot")
    
    # Create ranges for formatting SG Categories
    event_stat_ranges <- list(
      sgPutt = c(-8, 8),
      sgArg  = c(-5, 5),
      sgApp  = c(-10, 10),
      sgOtt  = c(-5, 5),
      sgT2G  = c(-12, 12),
      sgTot  = c(-20, 20)
    )
    
    round_stat_ranges <- list(
      sgPutt = c(-4, 4),
      sgArg  = c(-3, 3),
      sgApp  = c(-5, 5),
      sgOtt  = c(-3, 3),
      sgT2G  = c(-7, 7),
      sgTot  = c(-10, 10)
    )
    
    getColor <- function(value, range) {
      if (is.na(value)) return("white")
      min_val <- range[1]; max_val <- range[2]
      val_scaled <- (value - min_val) / (max_val - min_val)
      val_scaled <- pmin(pmax(val_scaled, 0), 1)
      rgb(colorRamp(c("#F83E3E", "white", "#4579F1"))(val_scaled), maxColorValue = 255)
    }
    
    style_sg <- function(value, range) {
      value <- round(as.numeric(value), 2)
      list(
        background = getColor(value, range),
        fontSize = "11px",
        overflow = "hidden",
        textOverflow = "ellipsis",
        whiteSpace = "nowrap",
        textAlign = "center",
        padding = "2px 4px"
      )
    }
    
    # Produce Reactable
    reactable(
      eventData,
      columns = list(
        player = colDef(show = FALSE),
        Date = colDef(
          name = "Date",
          width = 95,
          align = "center",
          style = function(value) list(
            fontSize = "11px",
            textAlign = "center",
            padding = "2px 4px"
          ),
          cell = function(value) div(
            style = list(
              overflow = "hidden",
              whiteSpace = "nowrap"
            ),
            title = value,  # shows full text on hover
            value
          )
        ),
        finish = colDef(
          name = "Fin",
          width = 35,
          align = "center"
        ),
        
        tournament = colDef(
          name = "Tournament",
          width = 140,
          align = "center",
          style = function(value) list(
            fontSize = "11px",
            textAlign = "center",
            padding = "2px 2px"
          ),
          cell = function(value) div(
            style = list(
              overflow = "hidden",
              whiteSpace = "nowrap"
            ),
            title = value,  # shows full text on hover
            value
          )
        ),
        
        Round = colDef(
          width = 55,
          style = function(value) list(
            fontSize = "11px",
            textAlign = "center",
            padding = "2px 2px"
          )
        ),
        sgPutt = colDef(name = "SG:PUTT", width = 72,
                        style = function(value) style_sg(value, event_stat_ranges$sgPutt),
                        cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
        ),
        sgArg = colDef(name = "SG:ARG", width = 67,
                       style = function(value) style_sg(value, event_stat_ranges$sgArg),
                       cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
        ),
        sgApp = colDef(name = "SG:APP", width = 67,
                       style = function(value) style_sg(value, event_stat_ranges$sgApp),
                       cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
        ),
        sgOtt = colDef(name = "SG:OTT", width = 67,
                       style = function(value) style_sg(value, event_stat_ranges$sgOtt),
                       cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
        ),
        sgT2G = colDef(name = "SG:T2G", width = 67,
                       style = function(value) style_sg(value, event_stat_ranges$sgT2G),
                       cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
        ),
        sgTot = colDef(name = "SG:TOT", width = 67,
                       style = function(value) style_sg(value, event_stat_ranges$sgTot),
                       cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
        ),
        event_key = colDef(show = FALSE)
      ),
      details = function(index) {
        key <- eventData$event_key[index]
        sub_rounds <- roundData[roundData$event_key == key, ]
        
        sub_rounds$Blank <- ""
        
        desired_order <- c(
          "Blank", "Date", "finish", "tournament", "Round",
          "sgPutt", "sgArg", "sgApp", "sgOtt", "sgT2G", "sgTot",
          "event_key", "player"
        )
        
        desired_order <- intersect(desired_order, names(sub_rounds))
        sub_rounds <- sub_rounds[, desired_order, drop = FALSE]
        
        reactable(
          sub_rounds,
          columns = list(
            player = colDef(show = FALSE),
            Blank = colDef(
              width = 44,
              cell = function(value) ""
            ),
            Date = colDef(
              name = "Date",
              width = 95,
              style = function(value) list(
                fontSize = "11px",
                textAlign = "center",
                padding = "2px 2px"
              ),
              cell = function(value) div(
                style = list(
                  overflow = "hidden",
                  whiteSpace = "nowrap"
                ),
                title = value,  # shows full text on hover
                value
              )
            ),
            finish = colDef(
              name = "Fin",
              width = 35,
              align = "center"
            ),
            tournament = colDef(
              name = "Tournament",
              width = 140,
              style = function(value) list(
                fontSize = "11px",
                textAlign = "center",
                padding = "2px 2px"
              ),
              cell = function(value) div(
                style = list(
                  overflow = "hidden",
                  whiteSpace = "nowrap"
                ),
                title = value,  # shows full text on hover
                value
              )
            ),
            
            Round = colDef(
              width = 55,
              style = function(value) list(
                fontSize = "11px",
                textAlign = "center",
                padding = "2px 4px"
              )
            ),
            sgPutt = colDef(name = "SG:PUTT", width = 72,
                            style = function(value) style_sg(value, round_stat_ranges$sgPutt),
                            cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
            ),
            sgArg = colDef(name = "SG:ARG", width = 67,
                           style = function(value) style_sg(value, round_stat_ranges$sgArg),
                           cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
            ),
            sgApp = colDef(name = "SG:APP", width = 67,
                           style = function(value) style_sg(value, round_stat_ranges$sgApp),
                           cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
            ),
            sgOtt = colDef(name = "SG:OTT", width = 67,
                           style = function(value) style_sg(value, round_stat_ranges$sgOtt),
                           cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
            ),
            sgT2G = colDef(name = "SG:T2G", width = 67,
                           style = function(value) style_sg(value, round_stat_ranges$sgT2G),
                           cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
            ),
            sgTot = colDef(name = "SG:TOT", width = 67,
                           style = function(value) style_sg(value, round_stat_ranges$sgTot),
                           cell = function(value) div(title = round(as.numeric(value), 2), round(as.numeric(value), 2))
            ),
            event_key = colDef(show = FALSE)
          ),
          pagination = FALSE,
          bordered = TRUE,
          compact = TRUE,
          highlight = TRUE,
          fullWidth = TRUE,
          theme = reactableTheme(
            style = list(fontSize = "12px"),
            headerStyle = list(display = "none"),
            rowStyle = list(height = "25px")
          ),
        )
      },
      pagination = FALSE,
      searchable = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      theme = reactableTheme(
        style = list(fontSize = "12px"),
        headerStyle = list(fontSize = "12px"),
        rowStyle = list(height = "25px")
      )
    )
    
  })
}

getEventRoundsData <- function(playerName) {
  
  # Develop Last N Data
  lastNData <- data %>% 
    filter(Round == "Event") %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>%
    group_by(player) %>% 
    filter(player == playerName) %>% 
    select(Date, finish, tournament, Round, sgPutt, sgArg, sgApp, sgOtt, sgT2G, sgTot)
  
  return(lastNData)
}

getIndRoundsData <- function(playerName) {
  
  # Develop Last N Data
  lastNData <- data %>% 
    filter(Round != "Event") %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>%
    group_by(player) %>% 
    filter(player == playerName) %>% 
    select(Date, finish, tournament, Round, sgPutt, sgArg, sgApp, sgOtt, sgT2G, sgTot)
  
  return(lastNData)
}