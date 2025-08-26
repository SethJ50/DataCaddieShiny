library(shiny)
library(bslib)
library(reactable)
library(reactable.extras)
library(mongolite)
library(tidyverse)
library(shinyWidgets)
library(htmltools)
library(ggplot2)
library(ggrepel)
library(plotly)
library(showtext)


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

source("home.R")
source("cheatSheet.R")
source("golferProfiles.R")
source("utils.R")
source("playersDataFunctions.R")
source("customModel.R")

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
                 choices = statsDisplayOptions,
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
               "Recent Rounds",
               div(
                 style = "",
                 reactableOutput("gp_recent_rounds")
               )
             )
           )
           
  ),
  tabPanel("Custom Model",
           tags$style(HTML("
            #modelStatPicker + .dropdown-toggle,
            #modelStatPicker + .dropdown-toggle.btn {
              min-width: 200px;
              max-width: 400px;
              height: 50px;
              line-height: 50px;
              font-size: 16px;
              margin-bottom: -10px;
              background-color: #fafafa;
              border: 1px solid #ddd;
              border-radius: 4px;
            }
            
            .weight-input input.form-control {
              width: 70px !important;   /* fixed width */
              max-width: 70px !important;
              min-width: 70px !important;
              padding: 3px 3px;
              text-align: center;
              background-color: #fafafa;
              border: 1px solid #ddd;
              border-radius: 4px;
            }
            
            .weight-stat-row {
              width: 100%;
              min-width: 280px;
              max-width: 350px;
              border: 1px solid #ddd;
              border-radius: 4px;
              padding: 6px 10px;
              margin-bottom: 6px;
              background-color: #fafafa;
              margin-left: auto;
              margin-right: auto;
            }
            
            .weight-stat-label {
              display: flex;
              align-items: center;
              justify-content: center;
              height: 100%;
            }
            
            
          ")),
           
           layout_columns(
             col_widths = c(3, 9),
             fill = TRUE,
             card(
               div(
                 shinyWidgets::pickerInput( 
                   inputId = "modelStatPicker",
                   label = "Choose Model Stats:",
                   choices = modelStatsOptions,
                   selected = c("sgTotPga"),
                   multiple = TRUE,
                   options = list(
                     `actions-box` = TRUE,
                     `live-search` = TRUE,
                     `selected-text-format` = "count > 2",
                     `none-selected-text` = "No columns selected"
                   ),
                   width = "100%"
                 )
               ),
               div(
                 style = "margin-top: 15px; padding: 0px 10px 0px 10px;",
                 uiOutput("selectedModelStats")
               )
             ),
             card(
               div(
                 
               )
             )
           )         
  )
)

server <- function(input, output, session) {
  
  playersInTournament <- unique(salaries$player)
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  playersInTournamentPgaNames <- nameFanduelToPga(playersInTournament)
  
  favorite_players <- reactiveValues(names = character())
  
  # On Favorite Clicked, add or remove favorite from favorite_players list
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
      serverCheatSheet(input, output, session, favorite_players, playersInTournament)
    } else if(input$siteTabs == "Golfer Profiles") {
      serverGolferProfiles(input, output, session, favorite_players,
                           playersInTournament, playersInTournamentTourneyNameConv,
                           playersInTournamentPgaNames)
    } else if(input$siteTabs == "Custom Model"){
      serverCustomModel(input, output, session, favorite_players,
                           playersInTournament, playersInTournamentTourneyNameConv,
                           playersInTournamentPgaNames)
    } else {
      print("Invalid Server")
    }
  })
}

shinyApp(ui, server)