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

if (file.exists("www/Almarai-Regular.ttf")) {
  # Local development
  regular_path <- "www/Almarai-Regular.ttf"
  bold_path <- "www/Almarai-Bold.ttf"
} else {
  # Docker container
  print("HELLOOOOOO")
  regular_path <- "/srv/shiny-server/www/Almarai-Regular.ttf"
  bold_path <- "/srv/shiny-server/www/Almarai-Bold.ttf"
}

font_add("Almari", regular_path)
font_add("Almari-Bold", bold_path)
showtext_auto()

#### Helper Functions

getColumnColors <- function(col_name, data, color_mode = "Gradient") {
  
  norm_col_name <- paste0(col_name, "_norm")
  if (!norm_col_name %in% names(data)) {
    stop(paste("Normalized column", norm_col_name, "not found in data"))
  }
  
  col_data <- as.numeric(data[[norm_col_name]])
  #col_data[is.na(col_data)] <- 0
  
  if (color_mode == "Gradient") {
    
    # Set data to be within [0, 1] for gradient
    data_normalized <- (col_data + 3) / 6
    data_normalized <- pmin(pmax(data_normalized, 0), 1)
    
    ramp_colors <- c("#F83E3E", "white", "#4579F1")
    
    colors <- rep("white", length(col_data))  # gray for NA
    
    # Only compute gradient for non-NA values
    non_na_idx <- !is.na(data_normalized)
    if (any(non_na_idx)) {
      colors[non_na_idx] <- rgb(colorRamp(ramp_colors)(data_normalized[non_na_idx]), maxColorValue = 255)
    }
    return(colors)
  } else if (color_mode == "Flags") {
    q <- quantile(col_data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    
    flags <- sapply(col_data, function(val) {
      if (val <= q[1]) {
        "🔴"
      } else if (val <= q[2]) {
        "🟠"
      } else if (val <= q[3]) {
        "🟡"
      } else {
        "🟢"
      }
    })
    
    return(flags)
  } else {
    stop("Invalid color_mode")
  }
}

#### Random Long Stuff ---------------------------------------------------------
# Stat configuration for 'bar' plots in golfer profiles
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