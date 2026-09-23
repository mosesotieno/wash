library(tidyverse)
library(openxlsx)
library(readxl)

# Import -------------------------------------------------------------------


results_hpv <- read_xlsx("data/participants.xlsx")
wash <- read_rds("data/wash_main_clean.rds")
wash <- wash |> filter(consent == "Yes")

results_hpv <- results_hpv |> 
  mutate(sample_id = str_replace(sample_id, "^1", "R"))


# Functions ---------------------------------------------------------------

create_excel <- function(exportdata, fname, addcomment = TRUE){
  wb <- createWorkbook()
  
  # Define styles
  headerStyle <- createStyle(fontSize = 12, fontColour = "white", halign = "center",
                             fgFill = "#4F81BD", border = "Bottom", textDecoration = "bold")
  
  bodyStyle <- createStyle(halign = "center", valign = "center", wrapText = FALSE)
  
  
  # Define outside border styles
  border_top    <- createStyle(border = "top",    borderStyle = "medium")
  border_bottom <- createStyle(border = "bottom", borderStyle = "medium")
  border_left   <- createStyle(border = "left",   borderStyle = "medium")
  border_right  <- createStyle(border = "right",  borderStyle = "medium")
  
  # Write data with formatting using walk2
  walk2(names(exportdata), exportdata, ~ {
    addWorksheet(wb, .x)
    
    nrows <- nrow(.y) + 1  # +1 for header
    ncols <- ncol(.y) 
    
    
    # Write data
    writeData(wb, sheet = .x, x = .y, headerStyle = headerStyle)
    
    # Apply body style
    addStyle(wb, sheet = .x, style = bodyStyle,
             rows = 2:(nrow(.y)+1), cols = 1:ncol(.y), gridExpand = TRUE)
    
    # Add thick border ONLY on outer edges
    addStyle(wb, .x, border_top,    rows = 1,        cols = 1:ncols, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, .x, border_bottom, rows = nrows,    cols = 1:ncols, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, .x, border_left,   rows = 1:nrows,  cols = 1,       gridExpand = TRUE, stack = TRUE)
    addStyle(wb, .x, border_right,  rows = 1:nrows,  cols = ncols,   gridExpand = TRUE, stack = TRUE)
    
    
    # Set column widths
    setColWidths(wb, sheet = .x, cols = 1:ncol(.y), widths = "auto")
    
    nextrows <- nrows + 2
    
    if (addcomment == TRUE){
      writeComment(wb, sheet = .x, col = 1, row = nextrows,
                   comment = createComment(comment = "Key \n -5 Not Done \n -6 TND \n -7 Not applicable \n -8 Result Pending\n -9 NA",
                                           author = "Moses", visible = FALSE)
      )
    }
    
    
  })
  
  filename <- paste0("./reports/", fname, ".xlsx")
  
  saveWorkbook(wb, file = filename, overwrite = TRUE)
  
}



# Merge -------------------------------------------------------------------

miss_wash <- wash |> 
  anti_join(results_hpv, by = c("respondent_id"="sample_id")) |> 
  select(respondent_id, sample_id)


true_missing <- wash |>  
  anti_join(results_hpv, by = "sample_id") |> 
  select(respondent_id)
  
hanging_results <- results_hpv |>  
  anti_join(wash, by = "sample_id") |> 
  select(sample_id)
  
  
datas <- list(
  "miss_wash"=miss_wash,
  "wash_missing_cleaned"=true_missing,
  "hanging_results"=hanging_results
)
create_excel(datas, "missing", FALSE)


