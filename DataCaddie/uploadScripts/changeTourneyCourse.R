library(mongolite)
library(tidyverse)
library(readxl)
library(dplyr)
source("uploadUtils/db_connection.R")

changeCourseOfTourney <- function(tourney_name, dates_val, new_course) {
  
  col <- get_mongo_collection("tournamentrows")
  
  filter_query <- list(
    tournament = tourney_name,
    dates = dates_val
  )
  
  update_query <- list(
    '$set' = list(course = new_course)
  )
  
  col$update(
    query = jsonlite::toJSON(filter_query, auto_unbox = TRUE),
    update = jsonlite::toJSON(update_query, auto_unbox = TRUE),
    multiple = TRUE
  )
}