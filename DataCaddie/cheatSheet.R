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

statsDisplayOptions <- c(
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
)

serverCheatSheet <- function(input, output, session, favorite_players, playersInTournament) {
  
  # Create Cheat Sheet Data
  cheatSheetData <- reactive({
    
    # Require color_mode, visible_cols, and sg_sample_size
    req(input$color_mode, input$visible_cols, input$sg_sample_size)
    
    # Grab Selected Columns for Cheat Sheet and Number of Rounds for SG
    selected_cols <- input$visible_cols
    num_rounds <- as.numeric(input$sg_sample_size)
    
    # Get all data for cheat sheet
    dataAndTourneyNames <- get_all_player_data(favorite_players, playersInTournament, num_rounds)
    
    # Filter Cheat Sheet Data to only columns selected
    cheatSheetData <- dataAndTourneyNames$data %>% 
      select(.favorite, all_of(selected_cols), isFavorite)
    
    # Return list containing cheatSheetData and Recent Tournament Names
    list(data = cheatSheetData, recTourneyNames = dataAndTourneyNames$recTourneyNames,
         allData = dataAndTourneyNames$data)
  })
  
  # Create Cheat Sheet Reactable
  output$cheatSheet <- renderReactable({
    color_mode <- input$color_mode
    selected_cols <- input$visible_cols
    favs <- favorite_players$names
    
    cs <- cheatSheetData()
    cheatSheetData <- cs$data
    allData <- cs$allData
    recTourneyNames <- cs$recTourneyNames
    
    # Setup Cheat Sheet Column Defs
    all_col_defs <- list(
      player = colDef(name = "Player Name", align = "center", width = 200, sticky = "left"),
      fdSalary = colDef("FD Salary", align = "center", width = 100, sticky = "left"),
      dkSalary = colDef("DK Salary", align = "center", width = 100, sticky = "left"),
      numRds = colDef(name = "numRds", align = "center", width = 80)
    )
    
    # Get SG Last N Col Defs, Add to All Col Defs
    sgColDefs <- getSgColDefs(allData, color_mode)
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
    
    # Col Def For 'favorites' column - Defines onclick for favorites
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
    
    cheatSheetData$`.favorite` <- ""
    
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



#### Helper Functions----------------------------------------------------------
## Mostly ColDef Creation for Cheat Sheet Reactable

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
        getColumnColors(col_name, cheatSheetData, color_mode)
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


getSgColDefs <- function(cheatSheetData, color_mode) {
  
  # Define SG columns and their display names
  cols <- data.frame(
    col_key = c("sgPutt", "sgArg", "sgApp", "sgOtt", "sgT2G", "sgTot"), # internal names
    col_name = c("SG: Putt", "SG: Arg", "SG: App", "SG: Ott", "SG: T2G", "SG: Tot"), # display names
    stringsAsFactors = FALSE
  )
  
  # For each column, create list of color values
  color_map <- setNames(
    lapply(cols$col_key, function(col_name) {
      if (!is.null(cheatSheetData[[col_name]])) {
        getColumnColors(col_name, cheatSheetData, color_mode)
      } else {
        rep(NA, nrow(cheatSheetData))  # fallback
      }
    }),
    cols$col_key
  )
  
  # Create column definitions for reactable
  col_defs <- setNames(
    lapply(seq_len(nrow(cols)), function(i) {
      col_name <- cols$col_name[i]
      colDef(
        name = col_name,
        align = "center",
        width = 90,
        style = function(value, index, name) { # Change background if in Gradient Mode
          if(color_mode == "Gradient") {
            list(background = color_map[[cols$col_key[i]]][index])
          } else {
            list()
          }
        },
        cell = function(value, index) {
          if(color_mode == "Flags") { # Add Flag character if in 'Flag' Mode
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