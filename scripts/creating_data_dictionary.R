library(tidyverse)
library(labelled)
library(openxlsx)



# ---- 1. Build the data dictionary ----


create_detailed_dictionary <- function(df) {
  
  map_dfr(names(df), function(var_name) {
    var <- df[[var_name]]
    
    # Value labels ONLY for factor variables
    value_labels <- if (is.factor(var)) {
      levels_formatted <- paste0(seq_along(levels(var)), " = ", levels(var))
      paste(levels_formatted, collapse = "\n")
    } else {
      "Not applicable"
    }
    
    # Statistics: numeric stats OR factor frequencies
    if (is.numeric(var) && !inherits(var, "Date")) {
      stats_summary <- paste0(
        "Mean = ", round(mean(var, na.rm = TRUE), 2), "\n",
        "SD = ", round(sd(var, na.rm = TRUE), 2), "\n",
        "Range = ", paste(round(range(var, na.rm = TRUE), 2), collapse = " to ")
      )
    } else if (is.factor(var)) {
      freq_table <- table(var, useNA = "ifany")
      stats_summary <- paste0(
        names(freq_table), ": n = ", freq_table, " (",
        round(prop.table(freq_table) * 100, 1), "%)",
        collapse = "\n"
      )
    } else {
      stats_summary <- "Not applicable"
    }
    
    tibble(
      `Variable Name`  = var_name,
      `Description`    = attr(var, "label") %||% "",
      `Data Type`      = class(var)[1],
      `N`              = length(var),
      `Missing`        = sum(is.na(var)),
      `Missing %`      = round(mean(is.na(var)) * 100, 1),
      `Unique Values`  = length(unique(na.omit(var))),
      `Value Labels`   = value_labels,
      `Statistics`     = stats_summary
    )
  })
}

# ---- 2. Export to Excel with formatting ----
export_data_dictionary <- function(df, file_path = "data_dictionary.xlsx") {
  
  dict <- create_detailed_dictionary(df)
  
  wb <- createWorkbook()
  addWorksheet(wb, "Data Dictionary")
  
  writeData(wb, "Data Dictionary", dict)
  
  # ---- Styles ----
  header_style <- createStyle(
    textDecoration = "bold",
    fgFill         = "#D9E1F2",
    halign         = "center",
    valign         = "center",
    border         = "TopBottomLeftRight",
    wrapText       = TRUE
  )
  
  body_style <- createStyle(
    valign   = "top",
    wrapText = TRUE
  )
  
  # ---- Apply header style ----
  addStyle(
    wb, "Data Dictionary", header_style,
    rows = 1, cols = 1:ncol(dict), gridExpand = TRUE
  )
  
  # ---- Apply body style (wrap text so \n renders on separate lines) ----
  addStyle(
    wb, "Data Dictionary", body_style,
    rows = 2:(nrow(dict) + 1), cols = 1:ncol(dict), gridExpand = TRUE
  )
  
  # ---- Column widths ----
  setColWidths(
    wb, "Data Dictionary",
    cols   = 1:ncol(dict),
    widths = c(22, 45, 12, 8, 10, 12, 15, 50, 55)
  )
  
  # ---- Row heights (auto-ish: taller for wrapped content) ----
  setRowHeights(wb, "Data Dictionary", rows = 1, heights = 30)
  
  # ---- Freeze header row ----
  freezePane(wb, "Data Dictionary", firstRow = TRUE)
  
  # ---- Filter ----
  addFilter(wb, "Data Dictionary", rows = 1, cols = 1:ncol(dict))
  
  saveWorkbook(wb, file_path, overwrite = TRUE)
  message("Data dictionary saved to: ", file_path)
  
  invisible(dict)
}

# ---- 3. Usage ----
dict <- export_data_dictionary(wash_subset, "washdata_dictionary.xlsx")






