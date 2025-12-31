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
library(shinyjs)

source("global.R")

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
source("courseOverview.R")
source("customModel.R")
source("generatedLineups.R")
source("multiFilter.R")
source("favorites.R")

ui <- tagList(
  useShinyjs(),   # initialize shinyjs
  tags$head(
    tags$script(HTML("
      // --- FAVORITES LOCAL STORAGE MANAGEMENT ---
      Shiny.addCustomMessageHandler('loadFavorites', function(message) {
        let stored = localStorage.getItem('favorite_players');
        if (stored) {
          let parsed = JSON.parse(stored);
          Shiny.setInputValue('favorite_players_loaded', parsed, {priority: 'event'});
        } else {
          Shiny.setInputValue('favorite_players_loaded', [], {priority: 'event'});
        }
      });
      Shiny.addCustomMessageHandler('saveFavorites', function(players) {
        localStorage.setItem('favorite_players', JSON.stringify(players));
      });
    "))
  ),

  page_navbar(
    title = tags$a(
      href = "#",
      tags$img(
        src = "datacaddie_full_background.png",
        height = "30px"
      )
    ),
    id = "siteTabs",
    theme = theme,
    tabPanel("Home", 
             h4("Welcome"), 
             p("Welcome to DataCaddie, your go-to site for PGA Tour Analytics!")
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
    tabPanel("Course Overview", 
             
             tags$head(tags$script(HTML("
              Shiny.addCustomMessageHandler('favoriteClick', function(player) {
                Shiny.setInputValue('favorite_clicked', player, {priority: 'event'});
              });
            "))),
             
             tags$style(HTML("
                #co_course + .dropdown-toggle {
                  font-size: 20px !important;
                  height: 50px !important;
                  padding: 12px 12px !important;
              
                  display: flex !important;
                  align-items: center !important;
                  justify-content: center !important;
                  text-align: center !important;
                  
                  background-color: #fafafa;
                  border: 1px solid #ddd;
                  border-radius: 8px;
                }
                
                #co_stat_year + .dropdown-toggle {
                  font-size: 10px !important;
                  height: 25px !important;
                  padding: 0px 6px !important;
              
                  display: flex !important;
                  align-items: center !important;
                  justify-content: center !important;
                  text-align: center !important;
                  
                  background-color: #fafafa;
                  border: 1px solid #ddd;
                  border-radius: 8px;
                  margin-top: -15px;
                }
                
                #co_stat_year-label {
                  font-size: 12px !important;
                }
                
                #co_ch_table_year + .dropdown-toggle {
                  font-size: 10px !important;
                  height: 25px !important;
                  padding: 0px 6px !important;
                  width: 80px !important;
              
                  display: flex !important;
                  align-items: center !important;
                  justify-content: center !important;
                  text-align: center !important;
                  
                  background-color: #fafafa;
                  border: 1px solid #ddd;
                  border-radius: 8px;
                  margin-top: -15px;
                }
                
                #co_ch_table_year-label {
                  font-size: 12px !important;
                }
                
                #co_scoring_table_year + .dropdown-toggle {
                  font-size: 10px !important;
                  height: 25px !important;
                  padding: 0px 6px !important;
                  width: 80px !important;
              
                  display: flex !important;
                  align-items: center !important;
                  justify-content: center !important;
                  text-align: center !important;
                  
                  background-color: #fafafa;
                  border: 1px solid #ddd;
                  border-radius: 8px;
                  margin-top: -15px;
                }
                
                #co_scoring_table_year-label {
                  font-size: 12px !important;
                }
                
                #co_fit_viz + .dropdown-toggle {
                  font-size: 10px !important;
                  height: 25px !important;
                  padding: 0px 6px !important;
                  width: 200px !important;
              
                  display: flex !important;
                  align-items: center !important;
                  justify-content: center !important;
                  text-align: center !important;
                  
                  background-color: #fafafa;
                  border: 1px solid #ddd;
                  border-radius: 8px;
                  margin-top: -15px;
                }
                
                #co_fit_viz-label {
                  font-size: 12px !important;
                }
                
                .co_stat_year-label {
                  font-size: 10px !important;
                }
                
                #co_ch_table {
                  margin-top: -45px !important;
                }
                
                #co_proj_fit_tab {
                  margin-top: -45px !important;
                }
                
                #co_sim_course_tab {
                  margin-top: -45px !important;
                }
                
                #co_ott_perf_tab {
                  margin-top: -45px !important;
                }
                
                #co_score_hist_tab {
                  margin-top: -45px !important;
                }
                
                .bootstrap-select.form-control {
                  background-color: white !important;
                }
                
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
              
                #co_course {
                  font-size: 18px !important;
                  margin: auto;
                  text-align: center;
                }
                
                #co_tables_div .nav-pills .nav-link {
                  font-size: 13px;
                  padding: 4px 4px;
                  margin-right: 1px;
                  border-radius: 5px;
                }
                
                /* --- Importance Type Styling --- */
                /* Container background */
                #co_importance_type .btn-group {
                  background-color: #e0e0e0; /* light grey container */
                  border-radius: 6px;
                  padding: 2px;
                }
                
                /* Buttons styling */
                #co_importance_type .btn {
                  background-color: transparent !important;
                  color: #001f3f !important;
                  border: none !important; 
                  min-height: 28px !important;
                  padding: 2px 6px !important;
                  margin: 0 2px; /* small gap between buttons */
                  font-size: 14px; /* adjust text size */
                }
                
                /* Checked button */
                #co_importance_type .btn.active {
                  background-color: #c0c0c0 !important; /* slightly darker grey when active */
                  color: #001f3f !important;
                }
                
                /* Hover effect */
                #co_importance_type .btn:hover {
                  background-color: #d0d0d0 !important;
                  color: #001f3f !important;
                }
                
                /* Remove default shadows */
                #co_importance_type .btn:focus {
                  box-shadow: none !important;
                }
                
                .favorite-row {
                    background-color: #FFF9C4 !important;
                    border-top: 4px solid #FFF9C4;
                    border-bottom: 4px solid #FFF9C4;
                }
                
                #co_ch_table_year.shiny-input-container {
                  width: fit-content !important;
                  margin-bottom: 0 !important;
                }
            ")),
             
             layout_columns(
               col_widths = c(3, 3, 6),
               fill = TRUE,
               card(
                 style = "width: 100%;",
                 div(
                   style = "width: 100%; margin-left: auto; margin-right: auto;",
                   shinyWidgets::pickerInput(
                     inputId = "co_course",
                     label = NULL,
                     choices = unique(courseStatsData$course),
                     selected = unique(courseStatsData$course)[1],
                     options = list(`live-search` = TRUE),
                     width = "100%"
                   ),
                   div(
                     style="margin-top: 5px;",
                     uiOutput("co_year_selection")
                   ),
                   div(
                     style = "margin-top: 0px; overflow-x: hidden;",
                     reactableOutput("coStatsTable", width = "100%")
                   )
                 )
               ),
               card(
                 shinyWidgets::pickerInput(
                   inputId = "co_fit_viz",
                   label = "Course Fit Visualization:",
                   choices = c("Basic Stats", "Course Attributes", "OTT Strategy"),
                   selected = "Basic Stats",
                   width = "100%"
                 ),
                 conditionalPanel(
                   condition = "input.co_fit_viz == 'Basic Stats'",
                   div(
                     style = "display: flex; align-items: center; gap: 10px;",
                     shinyWidgets::radioGroupButtons(
                       inputId = "co_importance_type",
                       label = NULL,
                       choices = c("Scaled" = "scaled", "Raw" = "raw"),
                       selected = "raw",
                       justified = FALSE, # allow horizontal layout
                       checkIcon = list(yes = icon("check")),
                       status = "primary",
                       size = "sm"
                     ),
                     actionButton(
                       inputId = "co_importance_info",
                       label = icon("info-circle"),
                       style = "padding: 4px 8px; font-size: 16px;"
                     )
                   )
                 ),
                 div(
                   style = "width: 100%, max-width: 350px; margin-left: auto; margin-right: auto; text-align: center;",
                   plotlyOutput("co_stat_radar", height = "250px", width = "100%")
                 ),
                 div(
                   plotlyOutput("course_sim_plot", height = "300px")
                 )
               ),
               card(
                 div(
                   style = "margin-top: 0px; max-width: 100%;",
                   id = "co_tables_div",
                   navset_card_pill(
                     id = "co_tables_selected",
                     placement = "above",
                     nav_panel("Course History",
                               div(
                                 style = "display: flex; flex-direction: column; align-items: flex-start; gap: 4px; z-index: 10; width: fit-content;",
                                 actionButton(
                                   inputId = "co_ch_table_info",
                                   label = tagList(
                                     tags$span("Table Info", style = "margin-right: 4px;"),
                                     icon("info-circle")
                                   ),
                                   style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                                 ),
                                 shinyWidgets::pickerInput(
                                   inputId = "co_ch_table_year",
                                   label = "Year:",
                                   choices = NULL,
                                   selected = NULL
                                 )
                               ),
                               
                               reactableOutput("co_ch_table")
                     ),
                     nav_panel("Proj. Course Fit",
                               div(
                                 style = "display: flex; flex-direction: column; align-items: flex-start; gap: 4px; z-index: 10; width: fit-content;",
                                 actionButton(
                                   inputId = "co_proj_fit_table_info",
                                   label = tagList(
                                     tags$span("Table Info", style = "margin-right: 4px;"),
                                     icon("info-circle")
                                   ),
                                   style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                                 )
                               ),
                               uiOutput("co_proj_fit_tab") %>% withSpinner(color = "#0dc5c1")
                               ),
                     nav_panel("Similar Course Perf.",
                               div(
                                 style = "display: flex; flex-direction: column; align-items: flex-start; gap: 4px; z-index: 10; width: fit-content;",
                                 actionButton(
                                   inputId = "co_sim_course_table_info",
                                   label = tagList(
                                     tags$span("Table Info", style = "margin-right: 4px;"),
                                     icon("info-circle")
                                   ),
                                   style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                                 )
                               ),
                               uiOutput("co_sim_course_tab") %>% withSpinner(color = "#0dc5c1")
                               ),
                     nav_panel("OTT Course Perf.",
                               div(
                                 style = "display: flex; flex-direction: column; align-items: flex-start; gap: 4px; z-index: 10; width: fit-content;",
                                 actionButton(
                                   inputId = "co_ott_table_info",
                                   label = tagList(
                                     tags$span("Table Info", style = "margin-right: 4px;"),
                                     icon("info-circle")
                                   ),
                                   style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                                 )
                               ),
                               uiOutput("co_ott_perf_tab") %>% withSpinner(color = "#0dc5c1")
                               ),
                     nav_panel("Scoring History",
                               div(
                                 style = "z-index: 10; width: fit-content;",
                                 actionButton(
                                   inputId = "co_scoring_table_info",
                                   label = tagList(
                                     tags$span("Table Info", style = "margin-right: 4px;"),
                                     icon("info-circle")
                                   ),
                                   style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                                 ),
                                 shinyWidgets::pickerInput(
                                   inputId = "co_scoring_table_year",
                                   label = "Year:",
                                   choices = NULL,
                                   selected = NULL
                                 )
                               ),
                               reactableOutput("co_score_hist_tab")
                     )
                   )
                 )
               )
             )
             
    ),
    tabPanel("Custom Model",
             tags$head(tags$script(HTML("
              Shiny.addCustomMessageHandler('favoriteClick', function(player) {
                Shiny.setInputValue('favorite_clicked', player, {priority: 'event'});
              });
            "))),
             
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
              
              /* Platform label font size */
              label[for='model_platform'] {
                font-size: 12px;  /* increase/decrease as needed */
                font-weight: bold;
                margin-bottom: 0px;
                display: block;
              }
            
              /* Platform pickerInput dropdown size */
              #model_platform + .dropdown-toggle,
              #model_platform + .dropdown-toggle.btn {
                min-width: 150px;   /* adjust width */
                max-width: 150px;
                height: 30px;       /* adjust height */
                line-height: 30px;
                font-size: 14px;    /* dropdown text size */
                background-color: #fafafa;
                border: 1px solid #ddd;
                border-radius: 6px;
                margin-top: -5px;
              }
              
              .favorite-row {
                  background-color: #FFF9C4 !important;
                  border-top: 4px solid #FFF9C4;
                  border-bottom: 4px solid #FFF9C4;
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
                     selected = c("SG Tot PGA"),
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
                 hr(),
                 div(
                   style = "padding: 0px 10px 0px 10px;",
                   uiOutput("selectedModelStats"),
                   hr(),
                   div(
                     style = "display: flex; justify-content: center; align-items: center; margin-top: 20px;",
                     strong("Total Model Weights:"),
                     span(
                       style = "margin-left: 35px;",
                       textOutput("totalWeight", inline = TRUE)
                     )
                   ),
                   div(
                     style = "margin-top: 10px; text-align: center;",
                     actionButton("submitModel", "Submit", class = "btn-primary", style = "padding: 3px 10px; border-radius: 4px;")
                   )
                 )
               ),
               card(
                 div(class = "modelControls",
                     style = "display: flex; align-items: center; gap: 10px; z-index: 10; width: 200px;",
                   shinyWidgets::pickerInput(
                     inputId = "model_platform",
                     label = "Platform:",
                     choices = c("FanDuel", "DraftKings"),
                     selected = "FanDuel",
                     width = "150px",
                     options = list(
                       style = "btn-light",   # button style
                       size = 5
                     )
                   ),
                   div(
                     style = "margin-top: 23px;",
                     actionButton(
                       inputId = "generate_lineups",
                       label = "Generate Lineups",
                       class = "btn-primary",
                       style = "height: 30px; padding: 4px 10px; border-radius: 4px;"
                     )
                   )
                 ),
                 div(
                   style = "margin-top: -50px; z-index: 1 !important;",
                   reactableOutput("model_output")
                 )
               )
             )         
    ),
    
    tabPanel("Multi-Filter", 
             h4("Multi-Filter"),
             
             tags$head(tags$script(HTML("
              Shiny.addCustomMessageHandler('favoriteClick', function(player) {
                Shiny.setInputValue('favorite_clicked', player, {priority: 'event'});
              });
            "))),
             
             tags$style(HTML("
              #course_diff_filter .btn {
                height: 28px;          /* smaller height */
                line-height: 20px;     /* vertically center text */
                font-size: 13px;       /* slightly smaller text */
                padding: 2px 10px;     /* tighter padding */
              }
              
              #field_strength_filter .btn {
                height: 28px;          /* smaller height */
                line-height: 20px;     /* vertically center text */
                font-size: 13px;       /* slightly smaller text */
                padding: 2px 10px;     /* tighter padding */
              }
              
              label[for='stat_view_filter'] {
                font-size: 15px;  /* increase/decrease as needed */
                font-weight: bold;
                margin-bottom: 0px;
                display: block;
              }
            
              #stat_view_filter + .dropdown-toggle,
              #stat_view_filter + .dropdown-toggle.btn {
                min-width: 370px;   /* adjust width */
                max-width: 370px;
                height: 30px;       /* adjust height */
                line-height: 30px;
                font-size: 14px;    /* dropdown text size */
                background-color: #fafafa;
                border: 1px solid #ddd;
                border-radius: 6px;
                margin-top: -5px;
              }
              
              #rec_rds_mf {
                width: 150px !important;   /* fixed width */
                max-width: 150px !important;
                min-width: 150px !important;
                padding: 3px 3px;
                border: 1px solid #ddd;
                border-radius: 4px;
                text-align: center;
                background-color: white !important;
                margin-top: -4px;
              }
              
              #base_rds_mf {
                width: 150px !important;   /* fixed width */
                max-width: 150px !important;
                min-width: 150px !important;
                padding: 3px 3px;
                border: 1px solid #ddd;
                border-radius: 4px;
                text-align: center;
                background-color: white !important;
                margin-top: -4px;
              }
              
              label[for='rec_rds_mf'] {
                font-size: 12px !important;
              }
              label[for='base_rds_mf'] {
                font-size: 12px !important;
              }
              
              .favorite-row {
                  background-color: #FFF9C4 !important;
                  border-top: 4px solid #FFF9C4;
                  border-bottom: 4px solid #FFF9C4;
                }
            ")),
             
             layout_columns(
               col_widths = c(3, 9),
               fill = TRUE,
               card(
                 actionButton(
                   inputId = "mf_tool_info",
                   label = tagList(
                     tags$span("Multi-Filter Tool Info", style = "margin-right: 4px;"),
                     icon("info-circle")
                   ),
                   style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                 ),
                 radioGroupButtons(
                   inputId = "course_diff_filter",
                   label = "Course Difficulty:",
                   choices = c("All", "Easy", "Medium", "Hard"),
                   justified = TRUE
                 ),
                 radioGroupButtons(
                   inputId = "field_strength_filter",
                   label = "Field Strength:",
                   choices = c("All", "Weak", "Medium", "Strong"),
                   justified = TRUE
                 ),
                 shinyWidgets::pickerInput(
                   inputId = "stat_view_filter",
                   label = "Stat View:",
                   choices = c("Strokes Gained", "Off-the-Tee", "Trends", "Floor/Ceiling"),
                   selected = "Strokes Gained",
                   width = "370px",
                   options = list(
                     style = "btn-light",   # button style
                     size = 5
                   )
                 ),
                 conditionalPanel(
                   condition = "input.stat_view_filter == 'Strokes Gained'",
                   actionButton(
                     inputId = "mf_sg_info",
                     label = tagList(
                       tags$span("SG View Info", style = "margin-right: 4px;"),
                       icon("info-circle")
                     ),
                     style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.stat_view_filter == 'Off-the-Tee'",
                   actionButton(
                     inputId = "mf_ott_info",
                     label = tagList(
                       tags$span("OTT View Info", style = "margin-right: 4px;"),
                       icon("info-circle")
                     ),
                     style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.stat_view_filter == 'Trends'",
                   actionButton(
                     inputId = "mf_trends_info",
                     label = tagList(
                       tags$span("Trends View Info", style = "margin-right: 4px;"),
                       icon("info-circle")
                     ),
                     style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.stat_view_filter == 'Floor/Ceiling'",
                   actionButton(
                     inputId = "mf_fc_info",
                     label = tagList(
                       tags$span("Floor/Ceiling View Info", style = "margin-right: 4px;"),
                       icon("info-circle")
                     ),
                     style = "padding: 2px 0px; font-size: 10px; margin-top: -5px;"
                   )
                 )
               ),
               card(
                 div(id = "multifilter_round_inputs",
                     style = "display: flex; align-items: center; gap: 10px; z-index: 10; width: fit-content;",
                     numericInput(
                       inputId = "rec_rds_mf",
                       label = "# Rec. Rds:",
                       value = 50,
                       step = 1,
                       width = "150px"
                     ),
                     numericInput(
                       inputId = "base_rds_mf",
                       label = "# Base Rds:",
                       value = 200,
                       step = 1,
                       width = "150px"
                     )
                 ),
                 div(
                   style = "margin-top: -45px;",
                   reactableOutput("multifilter_results_tbl")
                 )
               )
             )
    ),
    tabPanel("Favorites", 
             h4("Favorites"),
             
             tags$head(tags$script(HTML("
              Shiny.addCustomMessageHandler('favoriteClick', function(player) {
                Shiny.setInputValue('favorite_clicked', player, {priority: 'event'});
              });
            "))),
             
             tags$style(HTML("
              .favorite-row {
                  background-color: #FFF9C4 !important;
                  border-top: 4px solid #FFF9C4;
                  border-bottom: 4px solid #FFF9C4;
                }
            ")),
             layout_columns(
               col_widths = c(4, 8),
               fill = TRUE,
               card(
                 reactableOutput("favorites_table")
               )
             )
    ),
    
    tags$style(HTML("
      .nav-item a[data-value='Generated Lineups'] {
        display: none !important;
        width: 0px !important;
      }
    ")),
    
    tabPanel("Generated Lineups",
             h4("Generated Lineups"),
             tags$style(HTML("
              #num_lineups {
                width: 380px !important;   /* fixed width */
                max-width: 380px !important;
                min-width: 380px !important;
                padding: 3px 3px;
                text-align: center;
                border: 1px solid #ddd;
                border-radius: 4px;
                background-color: white !important;
              }
            ")),
             
             layout_columns(
               col_widths = c(3, 9),
               fill = TRUE,
               card(
                 numericInput(
                   inputId = "num_lineups",
                   label = "Num. Lineups:",
                   value = 10,
                   step = 1,
                   width = "380px"
                 ),
                 shinyWidgets::pickerInput(
                   inputId = "locked_players",
                   label = "Locked in Players:",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(
                     `actions-box` = TRUE,
                     `live-search` = TRUE,
                     `none-selected-text` = "No columns selected",
                     `max-options` = 6
                   ),
                   width = "380px"
                 ),
                 shinyWidgets::pickerInput(
                   inputId = "player_pool",
                   label = "Player Pool:",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   options = list(
                     `actions-box` = TRUE,
                     `live-search` = TRUE,
                     `none-selected-text` = "No columns selected"
                   ),
                   width = "380px"
                 ),
                 actionButton(
                   inputId = "generate_lineups_official",
                   label = "Generate Lineups",
                   class = "btn-primary",
                   style = "height: 30px; padding: 4px 10px; border-radius: 4px;"
                 ),
                 reactableOutput("optimizer_ownership_tbl")
               ),
               card(
                 reactableOutput("optimizer_results_tbl")
               )
             )
    ),
    tags$script(HTML("
      // When browser back/forward is used
      window.onpopstate = function(event) {
        Shiny.setInputValue('url_search_updated', window.location.search);
      };
  "))
  )
)

server <- function(input, output, session) {
  
  playersInTournament <- unique(salaries$player)
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  playersInTournamentPgaNames <- nameFanduelToPga(playersInTournament)
  
  favorite_players <- reactiveValues(names = character())
  session$userData$sessionModelWeights <- reactiveValues()
  
  # ---- Load stored favorites on connect ----
  session$sendCustomMessage("loadFavorites", list())
  
  observeEvent(input$favorite_players_loaded, {
    isolate({
      # populate the reactiveValues with stored favorites
      favorite_players$names <- input$favorite_players_loaded
    })
  })
  
  session$userData$sessionModelWeights <- reactiveValues()
  
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
  
  # ---- Save to localStorage whenever favorites change ----
  observe({
    session$sendCustomMessage("saveFavorites", favorite_players$names)
  })
  
  observe({
    if(input$siteTabs == "Home"){
      serverHome(input, output, session)
    } else if(input$siteTabs == "Cheat Sheet") {
      serverCheatSheet(input, output, session, favorite_players, playersInTournament)
    } else if(input$siteTabs == "Golfer Profiles") {
      serverGolferProfiles(input, output, session, favorite_players,
                           playersInTournament, playersInTournamentTourneyNameConv,
                           playersInTournamentPgaNames, all_player_data)
    } else if(input$siteTabs == "Course Overview"){
      serverCourseOverview(input, output, session, favorite_players,
                        playersInTournament, playersInTournamentTourneyNameConv,
                        playersInTournamentPgaNames)
    } else if(input$siteTabs == "Custom Model"){
      serverCustomModel(input, output, session, favorite_players,
                           playersInTournament, playersInTournamentTourneyNameConv,
                           playersInTournamentPgaNames)
    } else if(input$siteTabs == "Generated Lineups"){
      serverGeneratedLineups(input, output, session, favorite_players,
                             playersInTournament, playersInTournamentTourneyNameConv,
                             playersInTournamentPgaNames, session$userData$optimizerData)
    } else if(input$siteTabs == "Multi-Filter"){
      serverMultiFilter(input, output, session, favorite_players,
                        playersInTournament, playersInTournamentTourneyNameConv,
                        playersInTournamentPgaNames)
    } else if(input$siteTabs == "Favorites") {
      serverFavorites(input, output, session, favorite_players,
                        playersInTournament, playersInTournamentTourneyNameConv,
                        playersInTournamentPgaNames)
    } else {
      print("Invalid Server")
    }
  })
  
  observeEvent(input$siteTabs, {
    # Avoid updating if we haven't fully initialized the session
    if (!is.null(input$siteTabs)) {
      # Update the browser URL when user changes tabs
      updateQueryString(
        paste0("?tab=", URLencode(input$siteTabs, reserved = TRUE)),
        mode = "push",    # "push" adds to browser history (so back button works)
        session = session
      )
    }
  }, ignoreInit = TRUE)
  
  valid_tabs <- c("Home", "Cheat Sheet", "Golfer Profiles", "Custom Model", "Multi-Filter", "Generated Lineups")
  
  observeEvent(input$url_search_updated, {
    query <- parseQueryString(input$url_search_updated)
    if (!is.null(query$tab) && query$tab %in% valid_tabs) {
      if (query$tab != input$siteTabs) {
        updateNavbarPage(session, "siteTabs", selected = query$tab)
      }
    } else if (is.null(query$tab)) {
      # If no tab specified, go to Home (or your default)
      updateNavbarPage(session, "siteTabs", selected = "Home")
    }
  }, ignoreInit = TRUE)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$tab) && query$tab %in% valid_tabs) {
      updateNavbarPage(session, "siteTabs", selected = query$tab)
    }
  })
  
}

shinyApp(ui, server)