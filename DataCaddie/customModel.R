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
library(reactable)
library(reactable.extras)

source("utils.R")
source("playersDataFunctions.R")

modelStatsOptions <- c(
  "Display" = "real",
  "SG: PUTT PGA" = "sgPuttPga",
  "SG: ARG PGA" = "sgArgPga",
  "SG: APP PGA" = "sgAppPga",
  "SG: OTT PGA" = "sgOttPga",
  "SG: T2G PGA" = "sgT2GPga",
  "SG: TOT PGA" = "sgTotPga",
  
  "SG: PUTT L12" = "sgPuttL12",
  "SG: ARG L12" = "sgArgL12",
  "SG: APP L12" = "sgAppL12",
  "SG: OTT L12" = "sgOttL12",
  "SG: T2G L12" = "sgT2GL12",
  "SG: TOT L12" = "sgTotL12",
  
  "SG: PUTT L24" = "sgPuttL24",
  "SG: ARG L24" = "sgArgL24",
  "SG: APP L24" = "sgAppL24",
  "SG: OTT L24" = "sgOttL24",
  "SG: T2G L24" = "sgT2GL24",
  "SG: TOT L24" = "sgTotL24",
  
  "SG: PUTT L36" = "sgPuttL36",
  "SG: ARG L36" = "sgArgL36",
  "SG: APP L36" = "sgAppL36",
  "SG: OTT L36" = "sgOttL36",
  "SG: T2G L36" = "sgT2GL36",
  "SG: TOT L36" = "sgTotL36",
  
  "SG: PUTT L50" = "sgPuttL50",
  "SG: ARG L50" = "sgArgL50",
  "SG: APP L50" = "sgAppL50",
  "SG: OTT L50" = "sgOttL50",
  "SG: T2G L50" = "sgT2GL50",
  "SG: TOT L50" = "sgTotL50",
  
  "DR. DIST" = "drDist",
  "DR. ACC" = "drAcc",
  "BOB%" = "bob%",
  "BOG AVD" = "bogAvd",
  "SAND SAVE %" = "sandSave%",
  "SCRAMBLING" = "scrambling",
  "PAR 3 AVG" = "par3Avg",
  "PAR 4 AVG" = "par4Avg",
  "PAR 5 AVG" = "par5Avg",
  "GIR %" = "gir%",
  "PROXIMITY" = "prox",
  "ROUGH PROX" = "roughProx",
  
  "APP 50-75" = "app50_75",
  "APP 75-100" = "app75_100",
  "APP 100-125" = "app100_125",
  "APP 125-150" = "app125_150",
  "APP 150-175" = "app150_175",
  "APP 175-200" = "app175_200",
  "APP 200+" = "app200",
  
  "BONUS PUTT" = "bonusPutt",
  "3 PUTT AVD" = "threePuttAvd",
  
  "SG EASY FIELD" = "sgEasyField",
  "SG MEDIUM FIELD" = "sgMedField",
  "SG HARD FIELD" = "sgHardField",
  
  "SG EASY COURSE" = "sgEasyCourse",
  "SG MEDIUM COURSE" = "sgMedCourse",
  "SG HARD COURSE" = "sgHardCourse",
  
  "COURSE HISTORY" = "courseHistory"
)

serverCustomModel <- function(input, output, session, favorite_players,
                              playersInTournament, playersInTournamentTourneyNameConv,
                              playersInTournamentPgaNames) {
  
  rv_weights <- reactiveValues()
  
  output$selectedModelStats <- renderUI({
    req(input$modelStatPicker)
    
    tagList(
      lapply(input$modelStatPicker, function(stat) {
        
        weight_val <- if(!is.null(rv_weights[[stat]])) rv_weights[[stat]] else 0
        
        div(
          class = "weight-stat-row",
          fluidRow(
            style = "display: flex; align-items: center; justify-content: space-between;",
            column(8, 
                   div(
                     class = "weight-stat-label",
                     strong(names(modelStatsOptions)[modelStatsOptions == stat])
                   )
            ),
            column(4,
                   div(
                     class = "weight-input",
                     style = "display: flex; align-items: center;",
                     numericInput(
                       inputId = paste0("weight_", stat),
                       label = NULL,
                       value = weight_val,
                       step = 1,
                       width = "80px"
                     ),
                     actionButton(
                       inputId = paste0("remove_", stat),
                       label = "ⓧ",
                       style = "margin-left: 2px; background-color: #fafafa; padding: 2px 2px; min-width: 0; border: none"
                     )
                   )
            )
          )
        )
      })
    )
  })
  
  # On a change, for each stat, if value for stat isn't null, add it to rv_weights to store
  observe({
    req(input$modelStatPicker)
    lapply(input$modelStatPicker, function(stat) {
      local({   # ensures correct binding
        s <- stat
        
        input_id <- paste0("weight_", s)
        observeEvent(input[[input_id]], {
          rv_weights[[s]] <- input[[input_id]]
        }, ignoreNULL = FALSE)
        
        btn_id <- paste0("remove_", s)
        observeEvent(input[[btn_id]], {
          # Remove the stat from pickerInput
          updated <- setdiff(input$modelStatPicker, s)
          updatePickerInput(session, "modelStatPicker", selected = updated)
        }, ignoreInit = TRUE)
      })
    })
  })
}