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

#### Get All Data --------------------------------------------------------------

get_all_player_data <- function(favorite_players, playersInTournament, num_rounds) {
  # ALL: Returns data frame with data for all players combined
  
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