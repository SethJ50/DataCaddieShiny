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


theme <- bs_theme(
  version = 5,
  bootswatch = "lux",
  # Explicitly set navbar colors
  bg = "#ffffff",               # page background
  fg = "rgb(0, 0, 40)",        # default text color (not navbar)
  primary = "#001f3f",          # navy for primary elements (links/buttons)
  "navbar-bg" = "#001f3f",      # navy navbar background
  "navbar-color" = "#ffffff"    # white navbar text
)

library(showtext)

font_add("Almari", "www/Almarai-Regular.TTF")
font_add("Almari-Bold", "www/Almarai-Bold.TTF")
showtext_auto()

#db_url = "mongodb://localhost"
db_url = "mongodb://localhost:27017"

mongo_connection <- mongo(collection = "tournamentrows",
                          db = "data_caddy",
                          url = db_url)

data <- mongo_connection$find('{}')

mongo_fd_conn <- mongo(collection = "salaries",
                       db = "data_caddy",
                       url = db_url)

salaries <- mongo_fd_conn$find('{}')

mongo_pga_conn <- mongo(collection = "pgatours",
                        db = "data_caddy",
                        url = db_url)

pgaData <- mongo_pga_conn$find('{}')

mongo_ch_conn <- mongo(collection = "coursehistories",
                       db = "data_caddy",
                       url = db_url)

courseHistoryData <- mongo_ch_conn$find('{}')

getTenRecentTournaments <- function(data) {
  # Returns Tournament names and dates of 10 most recent tournaments
  
  tenRecTournaments <- data %>%
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(desc(Date)) %>%
    distinct(tournament, Date, .keep_all = TRUE) %>%
    slice_head(n = 10) %>%
    select(tournament, Date)
  
  return(tenRecTournaments)
}

getRecentHistoryDf <- function(playersInTournament, tenRecTournaments, data) {
  # Creates Recent History DF for Cheat Sheet
  
  # Ensure tournaments are ordered by most recent first
  tenRecTournaments <- tenRecTournaments %>%
    arrange(desc(Date))
  
  # Create all player-tournament-date combinations
  playerTournamentGrid <- expand.grid(
    player = playersInTournament,
    tournament = tenRecTournaments$tournament,
    Date = tenRecTournaments$Date,
    stringsAsFactors = FALSE
  ) %>%
    inner_join(tenRecTournaments, by = c("tournament", "Date"))
  
  # Get finishes
  dataWithFinishes <- data %>%
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    filter(player %in% playersInTournament) %>%
    select(player, tournament, Date, finish)
  
  # Merge and pivot
  playerFinishes <- playerTournamentGrid %>%
    left_join(dataWithFinishes, by = c("player", "tournament", "Date")) %>%
    distinct() %>%
    select(player, tournament, finish) %>%
    pivot_wider(names_from = tournament, values_from = finish)
  
  # Reorder tournament columns according to most recent to least recent
  tournament_cols_ordered <- tenRecTournaments$tournament
  
  playerFinishes <- playerFinishes %>%
    select(player, all_of(tournament_cols_ordered))
  
  # Rename tournament columns to rec1, rec2, ..., etc.
  new_col_names <- paste0("rec", seq_along(tournament_cols_ordered))
  colnames(playerFinishes)[-1] <- new_col_names
  
  return(playerFinishes)
}

getCourseHistoryDf <- function(playersInTournament, data) {
  chData <- data %>% 
    filter(player %in% playersInTournament) %>% 
    select(player, minus1, minus2, minus3, minus4, minus5)
  
  return(chData)
}

abbreviate_tourney <- function(name) {
  # Function to generate abbreviation
  
  words <- strsplit(name, "\\s+")[[1]]
  if (tolower(words[1]) == "the") {
    words <- words[-1]
  }
  
  if (length(words) == 0) return("")
  
  # Start with first 3 letters of the first word
  abbrev <- substr(words[1], 1, 3)
  
  # Add 1 letter from each remaining word until we reach 5 characters
  i <- 2
  while (nchar(abbrev) < 5 && i <= length(words)) {
    abbrev <- paste0(abbrev, substr(words[i], 1, 1))
    i <- i + 1
  }
  
  toupper(abbrev)
}

makeRecHistColDefs <- function(recTourneyNames) {
  # Makes column definitions for recent history in Cheat Sheet
  
  # Generate abbreviated names as a list, NOT a named vector
  abbrev_names <- lapply(recTourneyNames, abbreviate_tourney)
  
  # Now build the column definitions as a named list
  tourney_col_defs <- setNames(
    lapply(seq_along(recTourneyNames), function(i) {
      colDef(
        name = abbrev_names[[i]],
        header = function(value) {
          htmltools::tags$div(title = recTourneyNames[[i]], abbrev_names[[i]])
        },
        align = "center",
        width = 70,
        style = function(value) {
          if (is.na(value)) return()
          if (as.numeric(value) <= 10) {
            list(color = "#27ae60", fontWeight = "bold")
          } else if(as.numeric(value >= 100)) {
            list(color = "#c0392b", fontWeight = "bold")
          }
        }
      )
    }),
    paste0("rec", seq_along(recTourneyNames))
  )
}

makeCourseHistoryColDefs <- function() {
  chNames <- c("minus1", "minus2", "minus3", "minus4", "minus5")
  
  ch_col_defs <- setNames(
    lapply(seq_along(chNames), function(i){
      colDef(
        name = paste0("-", i),
        align = "center",
        width = 50,
        style = function(value) {
          if (is.na(value)) return()
          if (as.numeric(value) <= 10) {
            list(color = "#27ae60", fontWeight = "bold")
          } else if(as.numeric(value >= 100)) {
            list(color = "#c0392b", fontWeight = "bold")
          }
        }
      )
    }),
    paste0("minus", seq_along(chNames))
  )
  
  return(ch_col_defs)
}

makePgaColDefs <- function(cheatSheetData, color_mode = "Gradient") {
  # Column metadata: keys and widths
  cols <- data.frame(
    col_key = c(
      "SG Putt PGA", "SG Arg PGA", "SG App PGA", "SG Ott PGA", "SG T2G PGA", "SG Tot PGA",
      "Dr. Dist", "Dr. Acc", "GIR%", "SandSave%", "Scrambling",
      "App. 50-75", "App. 75-100", "App. 100-125", "App. 125-150", "App. 150-175", "App. 175-200", "App. 200_up",
      "BOB%", "Bog. Avd", "Par 3 Score", "Par 4 Score", "Par 5 Score", "Proximity", "Rough Prox.", "Putt BOB%",
      "3 Putt Avd", "Bonus Putt."
    ),
    width = c(
      rep(90, 6), rep(90, 3), 100, 90,
      rep(100, 7),
      90, 90, rep(100, 3),
      rep(100, 2), 90, 90, 90
    ),
    stringsAsFactors = FALSE
  )
  
  lower_is_better_cols <- c(
    "App. 50-75", "App. 75-100", "App. 100-125", "App. 125-150", "App. 150-175", "App. 175-200", "App. 200_up",
    "Par 3 Score", "Par 4 Score", "Par 5 Score", "Proximity", "Rough Prox.", "Bog. Avd", "3 Putt Avd"
  )
  
  # Generate color values or flags
  color_map <- setNames(
    lapply(cols$col_key, function(col_name) {
      if (!is.null(cheatSheetData[[col_name]])) {
        desc_flag <- ifelse(col_name %in% lower_is_better_cols, "FALSE", "TRUE")
        getColumnColorsMedian(cheatSheetData[[col_name]], desc_flag, color_mode)
      } else {
        rep(NA, nrow(cheatSheetData))
      }
    }),
    cols$col_key
  )
  
  # Generate column definitions
  col_defs <- setNames(
    lapply(seq_len(nrow(cols)), function(i) {
      col_key <- cols$col_key[i]
      colDef(
        name = col_key,
        header = function() htmltools::tags$div(
          title = col_key,
          style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
          col_key
        ),
        align = "center",
        width = cols$width[i],
        style = function(value, index, name) {
          if (color_mode == "Gradient") {
            list(background = color_map[[col_key]][index])
          } else {
            list()
          }
        },
        cell = function(value, index) {
          if (color_mode == "Flags") {
            paste0(color_map[[col_key]][index], " ", value)
          } else {
            value
          }
        }
      )
    }),
    cols$col_key
  )
  
  return(col_defs)
}

getColumnColors <- function(data, desc, color_mode = "Gradient") {
  data <- as.numeric(data)
  data[is.na(data)] <- 0
  
  if (color_mode == "Gradient") {
    max_abs <- max(abs(data), na.rm = TRUE)
    data_normalized <- (data + max_abs) / (2 * max_abs)
    ramp_colors <- if (desc == "TRUE") c("#F83E3E", "white", "#4579F1") else c("#4579F1", "white", "#F83E3E")
    colors <- rgb(colorRamp(ramp_colors)(data_normalized), maxColorValue = 255)
    return(colors)
  } else if (color_mode == "Flags") {
    q <- quantile(data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    
    flags <- sapply(data, function(val) {
      if(desc == "TRUE") {
        if (val <= q[1]) {
          "🔴"
        } else if (val <= q[2]) {
          "🟠"
        } else if (val <= q[3]) {
          "🟡"
        } else {
          "🟢"
        }
      } else {
        if (val <= q[1]) {
          "🟢"
        } else if (val <= q[2]) {
          "🟡"
        } else if (val <= q[3]) {
          "🟠"
        } else {
          "🔴"
        }
      }
    })
    
    return(flags)
  } else {
    stop("Invalid color_mode")
  }
}

getColumnColorsMedian <- function(data, desc, color_mode = "Gradient") {
  data <- as.numeric(data)
  data[is.na(data)] <- NA
  
  if (color_mode == "Gradient") {
    median_val <- median(data, na.rm = TRUE)
    max_abs <- max(abs(data - median_val), na.rm = TRUE)
    
    # Normalize relative to median
    data_normalized <- (data - median_val + max_abs) / (2 * max_abs)
    data_normalized[is.na(data_normalized)] <- 0.5
    
    ramp_colors <- if (desc == "TRUE") {
      c("#F83E3E", "white", "#4579F1")
    } else {
      c("#4579F1", "white", "#F83E3E")
    }
    
    colors <- rgb(colorRamp(ramp_colors)(data_normalized), maxColorValue = 255)
    return(colors)
    
  } else if (color_mode == "Flags") {
    q <- quantile(data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    
    flags <- sapply(data, function(val) {
      if (is.na(val)) return("⚪") 
      
      if (desc == "TRUE") {
        if (val <= q[1]) {
          "🔴"
        } else if (val <= q[2]) {
          "🟠"
        } else if (val <= q[3]) {
          "🟡"
        } else {
          "🟢"
        }
      } else {
        if (val <= q[1]) {
          "🟢"
        } else if (val <= q[2]) {
          "🟡"
        } else if (val <= q[3]) {
          "🟠"
        } else {
          "🔴"
        }
      }
    })
    
    return(flags)
  } else {
    stop("Invalid color_mode")
  }
}

getSgColDefs <- function(cheatSheetData, color_mode) {
  cols <- data.frame(
    col_key = c("sgPutt", "sgArg", "sgApp", "sgOtt", "sgT2G", "sgTot"),
    col_name = c("SG: Putt", "SG: Arg", "SG: App", "SG: Ott", "SG: T2G", "SG: Tot"),
    stringsAsFactors = FALSE
  )
  
  color_map <- setNames(
    lapply(cols$col_key, function(col_name) {
      if (!is.null(cheatSheetData[[col_name]])) {
        desc_flag <- "TRUE"
        getColumnColors(cheatSheetData[[col_name]], desc_flag, color_mode)
      } else {
        rep(NA, nrow(cheatSheetData))  # fallback
      }
    }),
    cols$col_key
  )
  
  col_defs <- setNames(
    lapply(seq_len(nrow(cols)), function(i) {
      col_name <- cols$col_name[i]
      colDef(
        name = col_name,
        align = "center",
        width = 90,
        style = function(value, index, name) {
          if(color_mode == "Gradient") {
            list(background = color_map[[cols$col_key[i]]][index])
          } else {
            list()
          }
        },
        cell = function(value, index) {
          if(color_mode == "Flags") {
            paste0(color_map[[cols$col_key[i]]][index], " ", value)
          } else {
            value
          }
        }
      )
    }),
    cols$col_key
  )
  
  return(col_defs)
}

nameToFanduel <- function(name) {
  # Converts a vector of names from PGATOUR naming to Fanduel Naming
  
  TO_FD <- c(
    'Robert MacIntyre' = 'Robert Macintyre',
    'Nicolai Højgaard' = 'Nicolai Hojgaard',
    'S.H. Kim' = 'Seonghyeon Kim',
    'Thorbjørn Olesen' = 'Thorbjorn Olesen',
    'Jordan Smith' = 'Jordan L. Smith',
    'Rasmus Højgaard' = 'Rasmus Hojgaard',
    'Ludvig Åberg' = 'Ludvig Aberg',
    'Nico Echavarria' = 'Nicolas Echavarria',
    'Frankie Capan III' = 'Frankie Capan',
    'Niklas Nørgaard' = 'Niklas Norgaard Moller'
  )
  
  # Use named lookup with fallback to original
  out <- TO_FD[name]
  out[is.na(out)] <- name[is.na(out)]
  return(out)
}

nameFanduelToPga <- function(name) {
  # Converts a vector of names from Fanduel naming to PGATOUR naming
  
  FD_TO_PGA <- c(
    'Robert Macintyre' = 'Robert MacIntyre',
    'Nicolai Hojgaard' = 'Nicolai Højgaard',
    'Seonghyeon Kim' = 'S.H. Kim',
    'Thorbjorn Olesen' = 'Thorbjørn Olesen',
    'Jordan L. Smith' = 'Jordan Smith',
    'Rasmus Hojgaard' = 'Rasmus Højgaard',
    'Ludvig Aberg' = 'Ludvig Åberg',
    'Nicolas Echavarria' = 'Nico Echavarria',
    'Niklas Norgaard Moller' = 'Niklas Nørgaard'
  )
  
  out <- FD_TO_PGA[name]
  out[is.na(out)] <- name[is.na(out)]
  return(out)
}

nameFanduelToTournament <- function(name) {
  # Converts a vector of names from Fanduel naming to PGATOUR Tournament naming
  
  FD_TO_TOURNAMENT <- c(
    'Robert Macintyre' = 'Robert MacIntyre',
    'Seonghyeon Kim' = 'S.H. Kim',
    'Nicolai Hojgaard' = 'Nicolai Højgaard',
    'Thorbjorn Olesen' = 'Thorbjørn Olesen',
    'Jordan L. Smith' = 'Jordan Smith',
    'Rasmus Hojgaard' = 'Rasmus Højgaard',
    'Ludvig Aberg' = 'Ludvig Åberg',
    'Nicolas Echavarria' = 'Nico Echavarria',
    'Frankie Capan' = 'Frankie Capan III',
    'Niklas Norgaard Moller' = 'Niklas Nørgaard'
  )
  
  out <- FD_TO_TOURNAMENT[name]
  out[is.na(out)] <- name[is.na(out)]
  return(out)
}

getLastNSg <- function(data, playersInTournament, N) {
  # Returns a DataFrame with last N Rounds SG averages for players in tournament
  
  # Develop Last N Data
  lastNData <- data %>% 
    filter(player %in% playersInTournament, Round != "Event") %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>%
    group_by(player) %>% 
    slice_head(n = N) %>%
    summarise(
      sgPutt = round(mean(sgPutt), 2),
      sgArg = round(mean(sgArg), 2),
      sgApp = round(mean(sgApp), 2),
      sgOtt = round(mean(sgOtt), 2),
      sgT2G = round(mean(sgT2G), 2),
      sgTot = round(mean(sgTot), 2),
      numRds = n(),
      .groups = "drop"
    )
  
  return(lastNData)
}

getPgaStats <- function(playersInTournament, data){
  pgaData <- data %>% 
    select(player, sgPutt, sgArg, sgApp, sgOtt, sgT2G, sgTot, drDist, drAcc, gir, sandSave, scrambling,
           app50_75, app75_100, app100_125, app125_150, app150_175, app175_200, app200_up,
           bob, bogAvd, par3Scoring, par4Scoring, par5Scoring, prox, roughProx, puttingBob,
           threePuttAvd, bonusPutt)
  
  colnames(pgaData) <- c(
    "player", "SG Putt PGA", "SG Arg PGA", "SG App PGA", "SG Ott PGA", "SG T2G PGA", "SG Tot PGA",
    "Dr. Dist", "Dr. Acc", "GIR%", "SandSave%", "Scrambling",
    "App. 50-75", "App. 75-100", "App. 100-125", "App. 125-150", "App. 150-175", "App. 175-200", "App. 200_up",
    "BOB%", "Bog. Avd", "Par 3 Score", "Par 4 Score", "Par 5 Score", "Proximity", "Rough Prox.", "Putt BOB%",
    "3 Putt Avd", "Bonus Putt."
  )
  
  pgaData[is.na(pgaData)] <- "NULL"
  
  return(pgaData)
}

getBarbellPlotData <- function(player, playersInTournamentTourneyNameConv,
                               playersInTournamentPgaNames, num_rounds, stat_labels, stats, rev_stats) {
  
  baseData <- getLastNSg(data, playersInTournamentTourneyNameConv, 36)
  pgatourStats <- getPgaStats(playersInTournamentPgaNames, pgaData)
  
  finalData <- baseData %>% 
    left_join(pgatourStats, by = c("player" = "player"))
  
  player_row <- finalData[finalData$player == player, ]
  if (nrow(player_row) == 0) return(NULL)
  
  plot_data <- do.call(rbind, lapply(stats, function(st) {
    column_data <- suppressWarnings(as.numeric(finalData[[st]]))
    
    # Check if column_data has any non-NA numeric values
    if (all(is.na(column_data))) return(NULL)
    
    avg_val <- mean(column_data, na.rm = TRUE)
    sd_val <- sd(column_data, na.rm = TRUE)
    player_val <- as.numeric(player_row[[st]])
    
    if (is.na(player_val) || is.na(sd_val) || sd_val == 0) return(NULL)
    
    player_std <- (player_val - avg_val) / sd_val
    avg_std <- (avg_val - avg_val) / sd_val
    
    if(st %in% rev_stats){
      player_std <- -player_std
    }
    
    data.frame(
      stat = stat_labels[st],  # use pretty label
      player = round(player_val, 2),
      player_s = player_std,
      average = round(avg_val, 2),
      avg_s = avg_std
    )
  }))
  
  plot_data$stat <- factor(plot_data$stat, levels = rev(stat_labels[stats]))
  
  return(plot_data)
}

makeBarbellPlot <- function(plot_data) {
  plot_data$y_pos <- as.numeric(plot_data$stat) * 0.6  # numeric y for positioning
  
  bar_thickness <- 0.4
  
  ggplot(plot_data, aes(y = y_pos)) +
    
    geom_rect(aes(xmin = -3, xmax = 3,
                  ymin = y_pos - bar_thickness/2, ymax = y_pos + bar_thickness/2),
              fill = "#d9d9d9", color = NA) +
    
    geom_rect(aes(xmin = pmin(0, player_s), xmax = pmax(0, player_s),
                  ymin = y_pos - bar_thickness/2, ymax = y_pos + bar_thickness/2,
                  fill = player_s)) +
    
    geom_segment(data = plot_data, 
                 aes(x = -3, xend = -3, y = y_pos - bar_thickness / 2, yend = y_pos + bar_thickness / 2), 
                 color = "grey70", linewidth = 2) +
    geom_segment(data = plot_data, 
                 aes(x = 0, xend = 0, y = y_pos - bar_thickness / 2, yend = y_pos + bar_thickness / 2), 
                 color = "grey70", linewidth = 1) +
    geom_segment(data = plot_data, 
                 aes(x = 3, xend = 3, y = y_pos - bar_thickness / 2, yend = y_pos + bar_thickness / 2), 
                 color = "grey70", linewidth = 2) +
    
    geom_text(aes(x = pmax(0, player_s) + 0.1, 
                  label = player),
              hjust = 0, 
              size = 4,
              family = "Almari-Bold"
              ) +
    
    scale_fill_gradient2(
      low = "#F83E3E",
      mid = "white",
      high = "#4579F1",
      midpoint = 0,
      guide = "none"
    ) +
    
    scale_x_continuous(limits = c(-3, 4), expand = c(0, 0)) +
    
    scale_y_continuous(breaks = plot_data$y_pos, labels = rev(levels(plot_data$stat))) +
    
    theme_minimal(base_size = 12) +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(5, 15, 5, 5),
      axis.text.y = element_text(size = 12, margin = margin(r = 10), family = "Almari-Bold", color = "black")
    )
}

render_stat_ui <- function(plot_id, num_stats, title = NULL) {
  height_per <- 35
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

create_stat_plot_output <- function(input, output, id, labels, stats, rev_stats,
                                    playersInTournamentTourneyNameConv,
                                    playersInTournamentPgaNames, tab_name) {
  output[[id]] <- renderPlot({
    req(input$gp_stat_selected == tab_name)
    req(input$gp_player)
    
    plot_data <- getBarbellPlotData(
      input$gp_player,
      playersInTournamentTourneyNameConv,
      playersInTournamentPgaNames,
      36,
      labels,
      stats,
      rev_stats
    )
    
    makeBarbellPlot(plot_data)
  })
}

ui <- page_navbar(
  title = "DataCaddie",
  id = "siteTabs",
  theme = theme,
  
  tabPanel("Home", 
           h4("Welcome"), 
           p("Your golf analytics go here!")
  ),
  
  tabPanel("Cheat Sheet",
           h4("Cheat Sheet"),
           
           tags$head(tags$script(HTML("
            Shiny.addCustomMessageHandler('favoriteClick', function(player) {
              Shiny.setInputValue('favorite_clicked', player, {priority: 'event'});
            });
          "))),
           
           tags$style(HTML("
              .reactable .rt-tbody .rt-tr:hover {
                filter: brightness(80%);
              }
           
              .cheat-controls {
                display: flex;
                align-items: center;
                gap: 20px;
                margin-bottom: 20px;
                padding-left: 20px;
                padding-right: 20px;
                z-index: 10;
                width: fit-content;
                margin-top: -20px;
              }
              
              .cheat-controls .form-group,
              .cheat-controls .shiny-input-container {
                margin-bottom: 0;
              }
            
              .cheat-controls label {
                font-size: 14px;
                font-weight: 600;
                color: #2c3e50;
                margin-bottom: 2px;
                display: block;
              }
            
              .cheat-controls .form-control,
              .cheat-controls .btn,
              .cheat-controls .dropdown-toggle {
                height: 35px !important;
                font-size: 15px;
                padding-top: auto;
                padding-bottom: auto;
                padding-left: 5px;
              }
              
              .btn.dropdown-toggle.btn-light {
                padding-top: 0 !important;
                padding-bottom: 0 !important;
              }
                            
              #sg_sample_size {
                border: 1px solid lightgrey !important;
                font-size: 15px !important;
                background-color: white !important;
              }
              
              .favorite-row {
                background-color: #FFF9C4 !important;
                border-top: 4px solid #FFF9C4;
                border-bottom: 4px solid #FFF9C4;
              }
              
              .navbar-brand {
                color: white !important;
              }
              .nav-link {
                color: #bbb !important;  /* light grey */
              }
              .nav-link:hover, .nav-link:focus {
                color: white !important;
              }
              
           ")),
           
           div(class = "cheat-controls",
               shinyWidgets::pickerInput(
                 inputId = "visible_cols",
                 label = "Choose Stats to Display:",
                 choices = c(
                   "Player Name" = "player",
                   "FD Salary" = "fdSalary",
                   "DK Salary" = "dkSalary",
                   "SG: Putt" = "sgPutt",
                   "SG: Arg" = "sgArg",
                   "SG: App" = "sgApp",
                   "SG: Ott" = "sgOtt",
                   "SG: T2G" = "sgT2G",
                   "SG: Tot" = "sgTot",
                   "numRds" = "numRds",
                   "SG Putt PGA",
                   "SG Arg PGA",
                   "SG App PGA",
                   "SG Ott PGA",
                   "SG T2G PGA",
                   "SG Tot PGA",
                   "Dr. Dist",
                   "Dr. Acc",
                   "GIR%",
                   "SandSave%",
                   "Scrambling",
                   "App. 50-75",
                   "App. 75-100",
                   "App. 100-125",
                   "App. 125-150",
                   "App. 150-175",
                   "App. 175-200",
                   "App. 200_up",
                   "BOB%",
                   "Bog. Avd",
                   "Par 3 Score",
                   "Par 4 Score",
                   "Par 5 Score",
                   "Proximity",
                   "Rough Prox.",
                   "Putt BOB%",
                   "3 Putt Avd",
                   "Bonus Putt.",
                   "Rec. 1" = "rec1",
                   "Rec. 2" = "rec2",
                   "Rec. 3" = "rec3",
                   "Rec. 4" = "rec4",
                   "Rec. 5" = "rec5",
                   "Rec. 6" = "rec6",
                   "Rec. 7" = "rec7",
                   "Rec. 8" = "rec8",
                   "Rec. 9" = "rec9",
                   "Rec. 10" = "rec10",
                   "-1" = "minus1",
                   "-2" = "minus2",
                   "-3" = "minus3",
                   "-4" = "minus4",
                   "-5" = "minus5"
                 ),
                 selected = c("player","fdSalary", "dkSalary", "sgPutt", "sgArg", "sgApp", "sgOtt", "sgT2G", "sgTot", "numRds",
                              "rec1", "rec2", "rec3", "rec4", "rec5", "rec6", "rec7", "rec8", "rec9", "rec10",
                              "minus1", "minus2", "minus3", "minus4", "minus5"),
                 multiple = TRUE,
                 options = list(
                   `actions-box` = TRUE,
                   `live-search` = TRUE,
                   `selected-text-format` = "count > 2",
                   `none-selected-text` = "No columns selected"
                 ),
                 width = "250px"
               ),
               textInput("sg_sample_size", "SG Round Sample", value = 36, width = "150px"),
               shinyWidgets::pickerInput(
                 inputId = "color_mode",
                 label = "Color Mode:",
                 choices = c("Gradient", "Flags"),
                 selected = "Gradient",
                 width = "150px",
                 options = list(
                   style = "btn-light",   # button style
                   size = 5
                 )
               )
           ),
           
           div(
             style = "margin-top: -70px; padding-bottom: 20px; overflow-x: auto; width: calc(100vw - 70px);",
             reactableOutput("cheatSheet")
           )
  ),
  
  tabPanel("Golfer Profiles", 
           
           tags$head(tags$script(HTML("
            Shiny.addCustomMessageHandler('favoriteClick', function(player) {
              Shiny.setInputValue('favorite_clicked', player, {priority: 'event'});
            });
          "))),
           
           tags$style(HTML("
            #gp_player + .dropdown-toggle {
              font-size: 20px !important;
              height: 50px !important;
              padding: 12px 12px !important;
          
              display: flex !important;
              align-items: center !important;
              justify-content: center !important;
              text-align: center !important;
            }
            
            #gp_player {
              font-size: 25px !important;
              margin: auto;
              text-align: center;
            }
            
            .form-group.shiny-input-container {
              margin-bottom: 0 !important;
            }
            
            .dropdown-menu.inner {
              max-height: 200px !important;
              overflow-y: auto;
            }
            
            .bs-searchbox input {
              height: 25px !important;
              font-size: 12px !important;
              padding: 2px 6px !important;
            }
            
            #gp_stat_tabs .nav-pills .nav-link {
              font-size: 11px;
              padding: 4px 4px;
              margin-right: 1px;
              border-radius: 5px;
            }
            
            #gp_stat_tabs .nav-item {
              margin-right: 2px;
              font-size: 5px;
            }
          ")),
           
           layout_columns(
             col_widths = c(3, 3, 6),
             fill = TRUE,
             card(
               div(
                 style = "margin-left: auto; margin-right: auto;",
                 uiOutput("gp_picker_with_star"),
               ),
               uiOutput("gp_salaries"),
               div(
                 style = "margin-top: -15px; margin-left: auto; margin-right: auto;",
                 uiOutput("gp_spider_plot")
               )
             ),
             card(
               div(
                 style = "margin-top: 0px; max-width: 100%;",
                 id = "gp_stat_tabs",
                 navset_card_pill(
                   id = "gp_stat_selected",
                   placement = "above",
                   nav_panel("SUMMARY", uiOutput("gp_stat_summary")),
                   nav_panel("OTT", uiOutput("gp_stat_ott")),
                   nav_panel("APP", uiOutput("gp_stat_app")),
                   nav_panel("ARG", uiOutput("gp_stat_arg")),
                   nav_panel("PUTT", uiOutput("gp_stat_putt")),
                   nav_panel("SCORING", uiOutput("gp_stat_scoring"))
                 )
               )
             ),
             card(
               "Recent Rounds"
             )
           )
           
  )
)

serverHome <- function(input, output, session) {
  
}

serverCheatSheet <- function(input, output, session, favorite_players,
                             playersInTournament, playersInTournamentTourneyNameConv,
                             playersInTournamentPgaNames) {
  
  # Create Cheat Sheet Data
  cheatSheetData <- reactive({
    req(input$color_mode, input$visible_cols, input$sg_sample_size)
    
    selected_cols <- input$visible_cols
    num_rounds <- as.numeric(input$sg_sample_size)
    
    # Get Salary Data for Cheat Sheet
    salaryData <- salaries %>% 
      filter(player %in% playersInTournament) %>% 
      select(player, fdSalary, dkSalary)
    
    tenRecTournaments <- getTenRecentTournaments(data)
    recTourneyNames <- tenRecTournaments$tournament
    playerFinishes <- getRecentHistoryDf(playersInTournamentTourneyNameConv, tenRecTournaments, data)
    pgatourStats <- getPgaStats(playersInTournamentPgaNames, pgaData)
    courseHistoryDf <- getCourseHistoryDf(playersInTournamentPgaNames, courseHistoryData)
    baseData <- getLastNSg(data, playersInTournamentTourneyNameConv, num_rounds)
    
    favs <- favorite_players$names
    
    finalData <- baseData %>% 
      mutate(fanduelName = nameToFanduel(player)) %>%
      left_join(salaryData, by = c("fanduelName" = "player")) %>%
      mutate(tourneyPlayerName = nameFanduelToTournament(fanduelName),
             pgaPlayerName = nameFanduelToPga(fanduelName)) %>%
      left_join(playerFinishes, by = c("tourneyPlayerName" = "player")) %>%
      left_join(pgatourStats, by = c("pgaPlayerName" = "player")) %>%
      left_join(courseHistoryDf, by = c("pgaPlayerName" = "player")) %>% 
      mutate(
        isFavorite = player %in% favs
      ) %>% 
      select(all_of(selected_cols), isFavorite)
    
    finalData$.favorite <- NA
    finalData <- finalData[, c(".favorite", setdiff(names(finalData), ".favorite"))]
    
    list(data = finalData, recTourneyNames = recTourneyNames)
  })
  
  # Create Cheat Sheet Reactable
  output$cheatSheet <- renderReactable({
    color_mode <- input$color_mode
    selected_cols <- input$visible_cols
    favs <- favorite_players$names
    
    cs <- cheatSheetData()
    cheatSheetData <- cs$data
    recTourneyNames <- cs$recTourneyNames
    
    # Setup Cheat Sheet Column Defs
    all_col_defs <- list(
      player = colDef(name = "Player Name", align = "center", width = 200, sticky = "left"),
      fdSalary = colDef("FD Salary", align = "center", width = 100, sticky = "left"),
      dkSalary = colDef("DK Salary", align = "center", width = 100, sticky = "left"),
      numRds = colDef(name = "numRds", align = "center", width = 80)
    )
    
    # Get SG Last N Col Defs, Add to All Col Defs
    sgColDefs <- getSgColDefs(cheatSheetData, color_mode)
    all_col_defs <- c(all_col_defs, sgColDefs)
    
    # Make Recent History Tournament Column Defs, Add to All Col Defs
    if (exists("recTourneyNames") && length(recTourneyNames) > 0) {
      tourney_col_defs <- makeRecHistColDefs(recTourneyNames)
      
      all_col_defs <- c(all_col_defs, tourney_col_defs)
    }
    
    # Make PGATOUR Stats Col Defs
    pgatour_col_defs <- makePgaColDefs(cheatSheetData, color_mode)
    all_col_defs <- c(all_col_defs, pgatour_col_defs)
    
    # Make Course History Col Defs
    ch_col_defs <- makeCourseHistoryColDefs()
    all_col_defs <- c(all_col_defs, ch_col_defs)
    
    col_defs <- lapply(selected_cols, function(col) all_col_defs[[col]])
    names(col_defs) <- selected_cols
    
    # Col Def For 'favorites' column
    favorite_col_def <- list(
      .favorite = colDef(
        name = "",
        width = 28,
        sticky = "left",
        sortable = FALSE,
        resizable = FALSE,
        cell = function(value, index) {
          player_name <- cheatSheetData$player[index]
          is_fav <- player_name %in% favorite_players$names
          star <- if (is_fav) "★" else "☆"
          htmltools::tags$span(
            star,
            style = "cursor:pointer; font-size: 18px; color: gold;",
            onclick = sprintf("Shiny.setInputValue('favorite_clicked', '%s', {priority: 'event'})", player_name)
          )
        }
      )
    )
    
    # Append actual selected columns afterward
    col_defs <- c(
      favorite_col_def,
      setNames(lapply(selected_cols, function(col) all_col_defs[[col]]), selected_cols)
    )
    
    # Create Column 'groups'
    pgatour_stats <- c(
      "SG Putt PGA", "SG Arg PGA", "SG App PGA", "SG Ott PGA", "SG T2G PGA", "SG Tot PGA",
      "Dr. Dist", "Dr. Acc", "GIR%", "SandSave%", "Scrambling",
      "App. 50-75", "App. 75-100", "App. 100-125", "App. 125-150", "App. 150-175", "App. 175-200", "App. 200_up",
      "BOB%", "Bog. Avd", "Par 3 Score", "Par 4 Score", "Par 5 Score", "Proximity", "Rough Prox.", "Putt BOB%",
      "3 Putt Avd", "Bonus Putt."
    )
    
    shown_pga_stats <- intersect(pgatour_stats, colnames(cheatSheetData))
    
    colgroups <- list()
    colgroups <- append(colgroups, list(colGroup(name = "Last N SG", columns = c("sgPutt", "sgArg", "sgApp", "sgOtt", "sgT2G", "sgTot"))))
    colgroups <- append(colgroups, list(colGroup(name = "Recent History", columns = c("rec1", "rec2", "rec3", "rec4", "rec5", "rec6", "rec7", "rec8", "rec9", "rec10"))))
    colgroups <- append(colgroups, list(colGroup(name = "Course History", columns = c("minus1", "minus2", "minus3", "minus4", "minus5"))))
    if(length(shown_pga_stats) > 0) {
      colgroups <- append(colgroups, list(colGroup(name = "PGATOUR Stats", columns = shown_pga_stats)))
    }
    
    col_defs[["isFavorite"]] <- colDef(show = FALSE)
    
    reactable(
      cheatSheetData,
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
      height = 680,
      columns = col_defs,
      columnGroups = colgroups,
      defaultSortOrder = "desc",
      defaultSorted = c("fdSalary"),
      showSortIcon = FALSE,
      theme = reactableTheme(
        style = list(fontSize = "15px"),
        rowStyle = list(height = "25px")
      ),
      defaultColDef = colDef(vAlign = "center"),
      rowClass = JS("
        function(rowInfo, index) {
          if (rowInfo && rowInfo.row && rowInfo.row.isFavorite) {
            return 'favorite-row';
          }
          return null;
        }
      ")
    )
  })
}

serverGolferProfiles <- function(input, output, session, favorite_players,
                                 playersInTournament, playersInTournamentTourneyNameConv,
                                 playersInTournamentPgaNames) {
  
  # Golfer Profiles Player Picker and Star
  output$gp_picker_with_star <- renderUI({
    
    # Use Tournament player names in dropdown
    players <- unique(salaries$player)
    players_tourney_names <- vapply(players, nameFanduelToTournament, character(1))
    
    selected_player <- input$gp_player
    if (is.null(selected_player) || !(selected_player %in% players_tourney_names)) {
      selected_player <- players_tourney_names[1]
    }
    
    favs <- favorite_players$names
    star <- if (selected_player %in% favs) "★" else "☆"
    
    onclick_js <- sprintf(
      "Shiny.setInputValue('favorite_clicked', '%s', {priority: 'event'})",
      selected_player
    )
    
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
  
  # Stat Summary Panel
  output$gp_spider_plot <- renderUI({
    #req(input$gp_stat_selected == "SUMMARY")
    req(input$gp_player)
    
    num_rounds <- 50
    stat_labels <- c(sgPutt = "SG: PUTT", sgArg = "SG: ARG", sgApp = "SG: APP",
                     sgOtt = "SG: OTT", `Dr. Dist` = "DR. DIST", `Dr. Acc` = "DR. ACC")
    stats <- c("sgPutt", "sgArg", "sgApp", "sgOtt", "Dr. Dist", "Dr. Acc")
    rev_stats <- c()
    
    finalData <- getBarbellPlotData(input$gp_player, playersInTournamentTourneyNameConv,
                                    playersInTournamentPgaNames, num_rounds, stat_labels, stats, rev_stats)
    
    r_closed <- c(finalData$player_s, finalData$player_s[1])
    theta_closed <- c(finalData$stat, finalData$stat[1])
    player_vals_closed <- c(finalData$player, finalData$player[1])
    stat_labels_closed <- c(finalData$stat, finalData$stat[1])
    
    r_closedF <- c(finalData$avg_s, finalData$avg_s[1])
    field_vals_closed <- c(finalData$average, finalData$average[1])
    
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
              range = c(-3, 3),
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
  
  stat_config <- list(
    OTT = list(
      title = "OFF-THE-TEE",
      num_stats = 3,
      stat_labels = c(
        sgOtt = "SG: OTT",
        `Dr. Dist` = "DR. DIST",
        `Dr. Acc` = "DR. ACC"
      ),
      stats = c("sgOtt", "Dr. Dist", "Dr. Acc")
    ),
    APP = list(
      title = "APPROACH",
      num_stats = 10,
      stat_labels = c(
        sgApp = "SG: APP", `App. 50-75` = "APP 50-75", `App. 75-100` = "APP 75-100",
        `App. 100-125` = "APP 100-125", `App. 125-150` = "APP 125-150",
        `App. 150-175` = "APP 150-175", `App. 175-200` = "APP 175-200",
        `App. 200_up` = "APP 200+", `GIR%` = "GIR %", Proximity = "PROXIMITY",
        `Rough Prox.` = "ROUGH PROX"
      ),
      stats = c("sgApp", "App. 50-75", "App. 75-100", "App. 100-125", "App. 125-150",
                "App. 150-175", "App. 175-200", "App. 200_up", "GIR%", "Proximity", "Rough Prox."),
      rev_stats = c("App. 50-75", "App. 75-100", "App. 100-125", "App. 125-150", "App. 150-175",
                    "App. 175-200", "App. 200_up", "Proximity", "Rough Prox.")
    ),
    ARG = list(
      title = "AROUND THE GREEN",
      num_stats = 3,
      stat_labels = c(
        sgArg = "SG: ARG",
        Scrambling = "SCRAMBLING",
        `SandSave%` = "SAND SAVE %"
      ),
      stats = stats <- c("sgArg", "Scrambling", "SandSave%")
    ),
    PUTT = list(
      title = "PUTTING",
      num_stats = 4,
      stat_labels = c(
        sgPutt = "SG: PUTT",
        `Putt BOB%` = "PUTT BOB%",
        `3 Putt Avd` = "3 PUTT AVD",
        `Bonus Putt.` = "BONUS PUTT"
      ),
      stats = c("sgPutt", "Putt BOB%", "3 Putt Avd", "Bonus Putt."),
      rev_stats = c("3 Putt Avd")
    ),
    SCORING = list(
      title = "SCORING",
      num_stats = 5,
      stat_labels = c(
        `Par 3 Score` = "PAR 3 AVG",
        `Par 4 Score` = "PAR 4 AVG",
        `Par 5 Score` = "PAR 5 AVG",
        `BOB%` = "BOB %",
        `Bog. Avd` = "BOGEY AVD"
      ),
      stats = c("Par 3 Score", "Par 4 Score", "Par 5 Score", "BOB%", "Bog. Avd"),
      rev_stats = c("Par 3 Score", "Par 4 Score", "Par 5 Score", "Bog. Avd")
    )
  )
  
  for (name in names(stat_config)) {
    
    local({
      n <- name
      config <- stat_config[[n]]
      id_plot <- paste0("gp_", tolower(n), "_multi_plot")
      id_ui <- paste0("gp_stat_", tolower(n))
      
      output[[id_ui]] <- renderUI({
        req(input$gp_stat_selected == n)
        render_stat_ui(id_plot, config$num_stats, config$title)
      })
      
      create_stat_plot_output(
        input = input,
        output = output,
        id = id_plot,
        labels = config$stat_labels,
        stats = config$stats,
        rev_stats = config$rev_stats %||% c(),
        playersInTournamentTourneyNameConv = playersInTournamentTourneyNameConv,
        playersInTournamentPgaNames = playersInTournamentPgaNames,
        tab_name = n
      )
    })
  }
}

server <- function(input, output, session) {
  
  playersInTournament <- unique(salaries$player)
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  playersInTournamentPgaNames <- nameFanduelToPga(playersInTournament)
  
  favorite_players <- reactiveValues(names = character())
  
  observeEvent(input$favorite_clicked, {
    clicked_player <- input$favorite_clicked
    isolate({
      if (clicked_player %in% favorite_players$names) {
        favorite_players$names <- setdiff(favorite_players$names, clicked_player)
      } else {
        favorite_players$names <- unique(c(favorite_players$names, clicked_player))
      }
    })
  })
  
  observe({
    if(input$siteTabs == "Home"){
      serverHome(input, output, session)
    } else if(input$siteTabs == "Cheat Sheet") {
      serverCheatSheet(input, output, session, favorite_players,
                       playersInTournament, playersInTournamentTourneyNameConv,
                       playersInTournamentPgaNames)
    } else if(input$siteTabs == "Golfer Profiles") {
      serverGolferProfiles(input, output, session, favorite_players,
                           playersInTournament, playersInTournamentTourneyNameConv,
                           playersInTournamentPgaNames)
    } else {
      print("Invalid Server")
    }
  })
}

shinyApp(ui, server)