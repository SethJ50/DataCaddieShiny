library(reactable)
library(tidyverse)


makeColorFunc <- function(palette = c("#F83E3E", "white", "#4579F1"),
                          min_val = 0, max_val = 1, reverse_colors = FALSE) {
  
  function(value) {
    if (is.na(value)) return("white")
    if (max_val == min_val) return("white")
    
    val <- pmax(pmin(value, max_val), min_val)
    val_scaled <- (val - min_val) / (max_val - min_val)
    val_scaled <- pmax(pmin(val_scaled, 1), 0)
    if(reverse_colors) val_scaled <- 1 - val_scaled
    
    rgb(colorRamp(palette)(val_scaled), maxColorValue = 255)
  }
}

makeColDef <- function(col_name, display_name = NULL, width = NULL,
                       sticky = NULL, color_func = NULL, norm_data = NULL,
                       font_size = NULL, header_size = NULL) {
  
  style_func <- NULL
  
  if (!is.null(color_func) || !is.null(font_size)) {
    
    norm_data <- norm_data %||% rep(NA_real_, 1)
    
    style_func <- function(value, index){
      styles <- list()
      
      # Background Color
      if(!is.null(color_func)) {
        bg <- "white"
        
        # Safely convert to numeric if possible
        value_num <- suppressWarnings(as.numeric(value))
        norm_num <- suppressWarnings(as.numeric(norm_data[index]))
        
        if(!is.na(norm_num)) {
          bg <- color_func(norm_num)
        } else if(!is.na(value_num)) {
          bg <- color_func(value_num)
        } else {
          bg <- "white"
        }
        
        styles$background <- bg
      }
      
      # Cell Font Size
      if(!is.null(font_size)) {
        styles$fontSize <- font_size
      }
      
      styles
    }
  }
  
  if(!is.null(header_size)) {
    h_size <- paste0(header_size, "px")
  } else {
    h_size = "12px";
  }
  
  colDef(
    width = width,
    align = "center",
    vAlign = "center",
    sticky = sticky,
    header = function(value) htmltools::tags$div(title = display_name, display_name),
    headerStyle = list(fontSize = h_size),
    style = style_func
  )
}

makeBasicTable <- function(data, col_defs, default_sorted, favorite_players, hasFavorites = FALSE,
                           searchable = TRUE, font_size = 15, row_height = 25) {
  View(data)
  if(hasFavorites) {
    favorite_col_def <- list(
      .favorite = colDef(
        name = "",
        width = 28,
        sticky = "left",
        sortable = FALSE,
        resizable = FALSE,
        cell = function(value, index) {
          player_name <- data$Player[index]
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
    
    col_defs <- c(favorite_col_def, col_defs)
    
    col_defs[["isFavorite"]] <- colDef(show = FALSE)
    
    row_class = JS("
        function(rowInfo, index) {
          if (rowInfo && rowInfo.row && rowInfo.row.isFavorite) {
            return 'favorite-row';
          }
          return null;
        }
      ")
  } else {
    row_class = NULL
  }
  
  table <- reactable(
    data,
    searchable = searchable,
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
    defaultSortOrder = "desc",
    defaultSorted = default_sorted,
    showSortIcon = FALSE,
    theme = reactableTheme(
      style = list(fontSize = paste0(font_size, "px")),
      rowStyle = list(height = paste0(row_height, "px"))
    ),
    defaultColDef = colDef(vAlign = "center", align = "center", width = 100),
    columns = col_defs,
    rowClass = row_class
  )
  
  return(table)
}