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

#### Constants =================================================================
EASY_COURSE_VAL <- -0.9
HARD_COURSE_VAL <- 0.9

SHORT_COURSE_VAL <- 99.5 # yardage / par
LONG_COURSE_VAL <- 101.9

SHORT_AVG_DRIVE <- 283
LONG_AVG_DRIVE <- 293.8

LOW_DRIVE_ACC <- 0.548
HIGH_DRIVE_ACC <- 0.642

SMALL_FW_WIDTH <- 29.2
LARGE_FW_WIDTH <- 36

SMALL_MISS_FW_PEN <- 0.315
HIGH_MISS_FW_PEN <- 0.395

HARD_SG_OTT <- -0.02
EASY_SG_OTT <- 0.014

HARD_SG_APP <- -0.017
EASY_SG_APP <- 0.025

HARD_SG_ARG <- -0.017
EASY_SG_ARG <- 0.027

HARD_SG_PUTT <- -0.0055
EASY_SG_PUTT <- 0.0065

EASY_FIELD_VAL <- -0.15
HARD_FIELD_VAL <- 0.7

#### Name Conversion ----------------------------------------------------------

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
    'Niklas Nørgaard' = 'Niklas Norgaard Moller',
    'Matt McCarty' = 'Matthew McCarty'
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
    'Niklas Norgaard Moller' = 'Niklas Nørgaard',
    'Matthew McCarty' = 'Matt McCarty'
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

#### Get All Data --------------------------------------------------------------

get_all_player_data <- function(favorite_players, playersInTournament, num_rounds) {
  # ALL: Returns DataFrame with Player, Salary Data, Recent History, Base SG, PGATOUR Stats, and Favorites
  # NO LONGER does Field Strength / Course Attrs - Use getAllLevelsData, add norms, and join it on "player" (see customModel)
  
  # Make Name Conversions
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  playersInTournamentPgaNames <- nameFanduelToPga(playersInTournament)
  
  # Get FanDuel and DraftKings Salary Data
  salaryData <- salaries %>% 
    filter(player %in% playersInTournament) %>% 
    select(player, fdSalary, dkSalary)
  
  # Get Recent History Finishes
  tenRecTournaments <- getTenRecentTournaments(data)
  recTourneyNames <- tenRecTournaments$tournament
  playerFinishes <- getRecentHistoryDf(playersInTournamentTourneyNameConv, tenRecTournaments, data)
  
  # Get Course History Finishes
  courseHistoryDf <- getCourseHistoryDf(playersInTournamentPgaNames, courseHistoryData)
  
  # Get RoundByRound Strokes Gained Averages
  baseData <- getLastNSg(data, playersInTournamentTourneyNameConv, num_rounds)
  
  # Get PGA Tour Stats
  pgatourStats <- getPgaStats(playersInTournamentPgaNames, pgaData, baseData)
  
  # Get names of favorite players
  favs <- favorite_players$names
  
  # Combine Data
  finalData <- baseData %>% 
    mutate(fanduelName = nameToFanduel(player)) %>%
    left_join(salaryData, by = c("fanduelName" = "player")) %>%
    mutate(tourneyPlayerName = nameFanduelToTournament(fanduelName),
           pgaPlayerName = nameFanduelToPga(fanduelName)) %>%
    left_join(playerFinishes, by = c("tourneyPlayerName" = "player")) %>%
    left_join(pgatourStats, by = c("pgaPlayerName" = "player")) %>%
    left_join(courseHistoryDf, by = c("pgaPlayerName" = "player")) %>% 
    mutate(`Course History` = rowMeans(select(., minus1:minus5), na.rm = TRUE)) %>%
    mutate(`Course History` = ifelse(is.nan(`Course History`), NA, `Course History`)) %>%
    mutate(
      isFavorite = player %in% favs
    )
  
  # Creates .favorite to hold star, rearranges .favorite to be first column
  finalData$.favorite <- NA
  finalData <- finalData[, c(".favorite", setdiff(names(finalData), ".favorite"))]
  
  # Add Normalized Columns for coloring numerical stats
  finalData <- add_normalized_columns(finalData)
  
  # Return a list containing both finalData, and recent tournament names
  return(list(data = finalData, recTourneyNames = recTourneyNames))
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
      drAcc = round(mean(drAcc, na.rm = TRUE), 2),
      drDist = round(mean(drDist, na.rm = TRUE), 2),
      numRds = n(),
      .groups = "drop"
    )
  
  return(lastNData)
}

getLastNRounds <- function(data, playersInTournament, N) {
  # Returns dataframe full of rows of last <=N rounds for each player in tourney
  
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  
  lastNRows <- data %>% 
    filter(player %in% playersInTournamentTourneyNameConv, Round != "Event") %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y"),
           tournamentyear = paste(tournament, format(Date, "%Y"))) %>% 
    arrange(player, desc(Date)) %>% 
    group_by(player) %>% 
    slice_head(n = N) %>% 
    ungroup()
  
  return(lastNRows)
}

getPgaStats <- function(playersInTournament, data, baseData){
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
  
  # Impute Missing Driving Stats with Round by Round Average
  missing_players <- setdiff(playersInTournament, pgaData$player)
  
  baseData <- baseData %>% 
    mutate(pgaPlayerName = nameFanduelToPga(player))
  
  missing_driving_stats <- baseData %>% 
    filter(pgaPlayerName %in% missing_players) %>% 
    select(player = pgaPlayerName, `Dr. Dist` = drDist, `Dr. Acc` = drAcc)
  
  template <- pgaData[0,]
  new_rows <- template[rep(1, nrow(missing_driving_stats)), ]
  
  new_rows$player <- missing_driving_stats$player
  new_rows$`Dr. Dist` <- missing_driving_stats$`Dr. Dist`
  new_rows$`Dr. Acc` <- missing_driving_stats$`Dr. Acc`
  
  pgaData <- bind_rows(pgaData, new_rows)
  
  return(pgaData)
}

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

getAllLevelsData <- function(playersInTournament, data, N) {
  
  # Get All Data for Players in Tournament
  all_data <- getRoundByRoundData(playersInTournament, data)

  # Get last N Rounds Data
  lastNData <- all_data %>%
    mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
    arrange(player, desc(Date)) %>%
    group_by(player) %>%
    slice_head(n = N) %>%
    ungroup()
  
  # Calculate Baseline SG TOT for each player
  baseline <- lastNData %>% 
    group_by(player) %>% 
    summarise(baseline_sgTot = mean(sgTot, na.rm = TRUE), .groups = "drop")
  
  # Design Object for Each Course Attribute Related Feature
  filter_spec <- list(
    course_diff_filter = list(col = "courseDiff",
                              map = c(Easy = "easy", Medium = "medium", Hard = "hard")),
    
    course_length_filter = list(col = "courseLength",
                                map = c(Short = "short", Medium = "medium", Long = "long")),
    
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
  
  # Function that calculates, for a given course attr, player performance on each level
  calc_filter_adjustments <- function(data, filter_name, filter_info, baseline, k) {
    
    # Grab Current Course Attr 'col'
    col_name <- filter_info$col
    
    # Grab Map of Levels of Attr
    level_map <- filter_info$map
    
    # For each level of this course attr
    results <- lapply(names(level_map), function(level_label) {
      
      # Grab the level value
      level_value <- level_map[[level_label]]
      
      tmp <- data %>% 
        mutate(Date = as.Date(dates, format = "%m/%d/%y")) %>%
        filter(.data[[col_name]] == level_value) %>% 
        group_by(player) %>% 
        slice_head(n = k) %>% 
        summarise(
          avg_sgTot_level = mean(sgTot, na.rm = TRUE),
          n_level = n(),
          .groups = "drop"
        ) %>% 
        left_join(baseline, by = "player") %>% 
        mutate(
          adj_raw = avg_sgTot_level - baseline_sgTot,
          weight = round(n_level / (n_level + k), 2),
          adj_sgTot = round(weight * adj_raw, 2),
          filter = filter_name,
          level = level_label
        )
    })
    
    bind_rows(results)
  }
  
  # k: The number of rounds until we are confident about a measurement (for adjusted stats)
  k <- 50
  
  # Iterate over all course attributes, creating player performance on each level of them
  adjustment_results <- lapply(names(filter_spec), function(fname) {
    calc_filter_adjustments(
      data = all_data,
      filter_name = fname,
      filter_info = filter_spec[[fname]],
      baseline = baseline,
      k = k
    )
  }) %>% 
    bind_rows()
  
  # Pivot to output df of form | player | SG <attr> <level> | SG <attr> <level> Adjusted |
  final_adjustments <- adjustment_results %>%
    select(player, filter, level, avg_sgTot_level, adj_sgTot) %>%
    pivot_wider(
      names_from = c(filter, level),
      values_from = c(avg_sgTot_level, adj_sgTot),
      names_glue = "{filter}_{level}_{.value}"
    ) %>% 
    rename(
      
      # ---- Course Difficulty ----
      "SG Easy Course" = course_diff_filter_Easy_avg_sgTot_level,
      "SG Medium Course" = course_diff_filter_Medium_avg_sgTot_level,
      "SG Hard Course" = course_diff_filter_Hard_avg_sgTot_level,
      "SG Easy Course Adjusted" = course_diff_filter_Easy_adj_sgTot,
      "SG Medium Course Adjusted" = course_diff_filter_Medium_adj_sgTot,
      "SG Hard Course Adjusted" = course_diff_filter_Hard_adj_sgTot,
      
      # ---- Course Length ----
      "SG Short Course" = course_length_filter_Short_avg_sgTot_level,
      "SG Medium Length Course" = course_length_filter_Medium_avg_sgTot_level,
      "SG Long Course" = course_length_filter_Long_avg_sgTot_level,
      "SG Short Course Adjusted" = course_length_filter_Short_adj_sgTot,
      "SG Medium Length Course Adjusted" = course_length_filter_Medium_adj_sgTot,
      "SG Long Course Adjusted" = course_length_filter_Long_adj_sgTot,
      
      # ---- Driving Distance ----
      "SG Short Driver Courses" = avg_dr_dist_filter_Short_avg_sgTot_level,
      "SG Medium Driver Courses" = avg_dr_dist_filter_Medium_avg_sgTot_level,
      "SG Long Driver Courses" = avg_dr_dist_filter_Long_avg_sgTot_level,
      "SG Short Driver Courses Adjusted" = avg_dr_dist_filter_Short_adj_sgTot,
      "SG Medium Driver Courses Adjusted" = avg_dr_dist_filter_Medium_adj_sgTot,
      "SG Long Driver Courses Adjusted" = avg_dr_dist_filter_Long_adj_sgTot,
      
      # ---- Driving Accuracy ----
      "SG Low Accuracy Courses" = avg_dr_acc_filter_Low_avg_sgTot_level,
      "SG Medium Accuracy Courses" = avg_dr_acc_filter_Medium_avg_sgTot_level,
      "SG High Accuracy Courses" = avg_dr_acc_filter_High_avg_sgTot_level,
      "SG Low Accuracy Courses Adjusted" = avg_dr_acc_filter_Low_adj_sgTot,
      "SG Medium Accuracy Courses Adjusted" = avg_dr_acc_filter_Medium_adj_sgTot,
      "SG High Accuracy Courses Adjusted" = avg_dr_acc_filter_High_adj_sgTot,
      
      # ---- Fairway Width ----
      "SG Narrow Fairways" = fw_width_filter_Narrow_avg_sgTot_level,
      "SG Medium Fairways" = fw_width_filter_Medium_avg_sgTot_level,
      "SG Wide Fairways" = fw_width_filter_Wide_avg_sgTot_level,
      "SG Narrow Fairways Adjusted" = fw_width_filter_Narrow_adj_sgTot,
      "SG Medium Fairways Adjusted" = fw_width_filter_Medium_adj_sgTot,
      "SG Wide Fairways Adjusted" = fw_width_filter_Wide_adj_sgTot,
      
      # ---- Missed Fairway Penalty ----
      "SG Low Miss Penalty" = missed_fw_pen_filter_Low_avg_sgTot_level,
      "SG Medium Miss Penalty" = missed_fw_pen_filter_Medium_avg_sgTot_level,
      "SG High Miss Penalty" = missed_fw_pen_filter_High_avg_sgTot_level,
      "SG Low Miss Penalty Adjusted" = missed_fw_pen_filter_Low_adj_sgTot,
      "SG Medium Miss Penalty Adjusted" = missed_fw_pen_filter_Medium_adj_sgTot,
      "SG High Miss Penalty Adjusted" = missed_fw_pen_filter_High_adj_sgTot,
      
      # ---- SG OTT Ease ----
      "SG Hard OTT Courses" = sg_ott_ease_filter_Hard_avg_sgTot_level,
      "SG Medium OTT Courses" = sg_ott_ease_filter_Medium_avg_sgTot_level,
      "SG Easy OTT Courses" = sg_ott_ease_filter_Easy_avg_sgTot_level,
      "SG Hard OTT Courses Adjusted" = sg_ott_ease_filter_Hard_adj_sgTot,
      "SG Medium OTT Courses Adjusted" = sg_ott_ease_filter_Medium_adj_sgTot,
      "SG Easy OTT Courses Adjusted" = sg_ott_ease_filter_Easy_adj_sgTot,
      
      # ---- SG APP Ease ----
      "SG Hard APP Courses" = sg_app_ease_filter_Hard_avg_sgTot_level,
      "SG Medium APP Courses" = sg_app_ease_filter_Medium_avg_sgTot_level,
      "SG Easy APP Courses" = sg_app_ease_filter_Easy_avg_sgTot_level,
      "SG Hard APP Courses Adjusted" = sg_app_ease_filter_Hard_adj_sgTot,
      "SG Medium APP Courses Adjusted" = sg_app_ease_filter_Medium_adj_sgTot,
      "SG Easy APP Courses Adjusted" = sg_app_ease_filter_Easy_adj_sgTot,
      
      # ---- SG ARG Ease ----
      "SG Hard ARG Courses" = sg_arg_ease_filter_Hard_avg_sgTot_level,
      "SG Medium ARG Courses" = sg_arg_ease_filter_Medium_avg_sgTot_level,
      "SG Easy ARG Courses" = sg_arg_ease_filter_Easy_avg_sgTot_level,
      "SG Hard ARG Courses Adjusted" = sg_arg_ease_filter_Hard_adj_sgTot,
      "SG Medium ARG Courses Adjusted" = sg_arg_ease_filter_Medium_adj_sgTot,
      "SG Easy ARG Courses Adjusted" = sg_arg_ease_filter_Easy_adj_sgTot,
      
      # ---- SG PUTT Ease ----
      "SG Hard Putting Courses" = sg_putt_ease_filter_Hard_avg_sgTot_level,
      "SG Medium Putting Courses" = sg_putt_ease_filter_Medium_avg_sgTot_level,
      "SG Easy Putting Courses" = sg_putt_ease_filter_Easy_avg_sgTot_level,
      "SG Hard Putting Courses Adjusted" = sg_putt_ease_filter_Hard_adj_sgTot,
      "SG Medium Putting Courses Adjusted" = sg_putt_ease_filter_Medium_adj_sgTot,
      "SG Easy Putting Courses Adjusted" = sg_putt_ease_filter_Easy_adj_sgTot,
      
      # ---- Field Strength ----
      "SG Weak Fields" = field_strength_filter_Weak_avg_sgTot_level,
      "SG Medium Fields" = field_strength_filter_Medium_avg_sgTot_level,
      "SG Strong Fields" = field_strength_filter_Strong_avg_sgTot_level,
      "SG Easy Field Adjusted" = field_strength_filter_Weak_adj_sgTot,
      "SG Medium Field Adjusted" = field_strength_filter_Medium_adj_sgTot,
      "SG Hard Field Adjusted" = field_strength_filter_Strong_adj_sgTot
    )
  
  return(final_adjustments)
}

getRoundByRoundData <- function(playersInTournament, data) {
  playersInTournamentTourneyNameConv <- nameFanduelToTournament(playersInTournament)
  
  # Get round by round data for players in tournament
  sgRows <- data %>% 
    filter(player %in% playersInTournamentTourneyNameConv, Round != "Event") %>% 
    mutate(Date = as.Date(dates, format = "%m/%d/%y"),
           tournamentyear = paste(tournament, format(Date, "%Y")),
           year = format(Date, "%Y")
    ) %>% 
    arrange(player, desc(Date))
  
  # Grab difficulty by course data
  courseDiffData <- courseStatsData
  # Check for duplicate Course Years
  # d <- courseDiffData %>%
  #   count(course, year) %>%
  #   filter(n > 1)
  # View(d)
  
  # Grab field strength data
  fieldStrengthData <- fieldStrengthData %>% 
    mutate(tournamentyear = paste(tournament, year))
  
  # Get FanDuel and DraftKings Salary Data
  salaryData <- salaries %>% 
    filter(player %in% playersInTournament) %>% 
    select(player, fdSalary, dkSalary)
  
  finalData <- sgRows %>% 
    left_join(
      fieldStrengthData %>% select(tournamentyear, strength),
      by = "tournamentyear"
    ) %>%
    left_join(
      courseDiffData,
      by = c("course", "year")
    ) %>% 
    mutate(fanduelName = nameToFanduel(player)) %>%
    left_join(salaryData, by = c("fanduelName" = "player")) %>% 
    mutate(ydg_per_par = yardage / par)
  
  finalData <- finalData %>% 
    mutate(
      courseDiff = case_when(
        difficulty <= EASY_COURSE_VAL ~ "easy",
        difficulty >= HARD_COURSE_VAL ~ "hard",
        TRUE ~ "medium"
      ),
      courseLength = case_when(
        ydg_per_par <= SHORT_COURSE_VAL ~ "short",
        ydg_per_par >= LONG_COURSE_VAL ~ "long",
        TRUE ~ "medium"
      ),
      avgDriveLength = case_when(
        adj_driving_distance <= SHORT_AVG_DRIVE ~ "short",
        adj_driving_distance >= LONG_AVG_DRIVE ~ "long",
        TRUE ~ "medium"
      ),
      avgDrivingAcc = case_when(
        adj_driving_accuracy <= LOW_DRIVE_ACC ~ "low",
        adj_driving_accuracy >= HIGH_DRIVE_ACC ~ "high",
        TRUE ~ "medium"
      ),
      avgFwWidth = case_when(
        fw_width <= SMALL_FW_WIDTH ~ "narrow",
        fw_width >= LARGE_FW_WIDTH ~ "wide",
        TRUE ~ "medium"
      ),
      missedFwPenalty = case_when(
        fw_diff <= SMALL_MISS_FW_PEN ~ "low",
        fw_diff >= HIGH_MISS_FW_PEN ~ "high",
        TRUE ~ "medium"
      ),
      sgOttEase = case_when(
        ott_sg <= HARD_SG_OTT ~ "hard",
        ott_sg <= EASY_SG_OTT ~ "easy",
        TRUE ~ "medium"
      ),
      sgAppEase = case_when(
        app_sg <= HARD_SG_APP ~ "hard",
        app_sg >= EASY_SG_APP ~ "easy",
        TRUE ~ "medium"
      ),
      sgArgEase = case_when(
        arg_sg <= HARD_SG_ARG ~ "hard",
        arg_sg >= EASY_SG_ARG ~ "easy",
        TRUE ~ "medium"
      ),
      sgPuttEase = case_when(
        putt_sg <= HARD_SG_PUTT ~ "hard",
        putt_sg >= EASY_SG_PUTT ~ "easy",
        TRUE ~ "medium"
      ),
      fieldType = case_when(
        strength < EASY_FIELD_VAL ~ "easy",
        strength > HARD_FIELD_VAL ~ "hard",
        TRUE ~ "medium"
      )
    )
  
  return(finalData)
}

getCourseHistoryDf <- function(playersInTournament, data) {
  
  chData <- data %>% 
    filter(player %in% playersInTournament) %>% 
    select(player, minus1, minus2, minus3, minus4, minus5)
  
  return(chData)
}

add_normalized_columns <- function(data) {
  rev_stats = c("App. 50-75", "App. 75-100", "App. 100-125", "App. 125-150", "App. 150-175",
                "App. 175-200", "App. 200_up", "Proximity", "Rough Prox.", "Par 3 Score", 
                "Par 4 Score", "Par 5 Score", "Bog. Avd", "3 Putt Avd")
  
  numeric_cols <- data %>% 
    select(where(is.numeric)) %>% 
    names()
  
  for (col in numeric_cols) {
    norm_col <- paste0(col, "_norm")
    if(col %in% rev_stats) {
      data[[norm_col]] <- -round(scale(data[[col]], center = TRUE, scale = TRUE)[, 1], 2)
    } else {
      data[[norm_col]] <- round(scale(data[[col]], center = TRUE, scale = TRUE)[, 1], 2)
    }
  }
  
  return(data)
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


get_stat_summary_data <- function(data, pgaData, playersInTournamentTourneyNameConv,
                                  playersInTournamentPgaNames) {
  
  # Get Tournament Row and PGA Tour Data
  baseData <- getLastNSg(data, playersInTournamentTourneyNameConv, 36)
  pgatourStats <- getPgaStats(playersInTournamentPgaNames, pgaData)
  
  # Join Data Together - full data source
  finalData <- baseData %>% 
    left_join(pgatourStats, by = c("player" = "player"))
  
  # Filter to only player's data
  player_row <- finalData[finalData$player == player, ]
  if (nrow(player_row) == 0) return(NULL)
  
  # Create 'Config': Has stat groupings with all stats of interest
  # Create 'reversed stats' list
  
  # Iterate through all stats, creating standardized versions & reversing
  # as in get barbell data...
  
  
  return(player_row)
}

getLastNRoundsPriorTo <- function(curr_player, curr_date, N) {
  
  last_n_data <- data %>%
    mutate(
      date_parsed = as.Date(dates, format = "%m/%d/%y")
    ) %>%
    filter(
      player == curr_player,
      date_parsed < as.Date(curr_date, format = "%m/%d/%y"),
      Round %in% 1:4
    ) %>%
    arrange(desc(date_parsed), desc(Round)) %>%
    slice_head(n = N)
  
  return(last_n_data)
  
}