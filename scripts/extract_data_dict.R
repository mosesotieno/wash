library(tidyverse)
library(labelled)


# Functions ---------------------------------------------------------------

create_detailed_dictionary <- function(df) {
  
  map_dfr(names(df), function(var_name) {
    var <- df[[var_name]]
    
    # Get value labels ONLY for factor variables (unchanged)
    value_labels <- if(is.factor(var)) {
      # Create formatted levels with line breaks that work in Excel
      levels_formatted <- paste0(seq_along(levels(var)), " = ", levels(var))
      # Use CHAR(10) for Excel line breaks
      paste(levels_formatted, collapse = "\n")
    } else {
      "Not applicable"
    }
    
    # Get descriptive statistics for numeric variables
    # AND frequencies for factor variables
    if(is.numeric(var)) {
      stats_summary <- paste0(
        "Mean = ", round(mean(var, na.rm = TRUE), 2), "\n",
        "SD = ", round(sd(var, na.rm = TRUE), 2), "\n", 
        "Range = ", paste(round(range(var, na.rm = TRUE), 2), collapse = " to ")
      )
    } else if(is.factor(var)) {
      # Add frequency table for factor variables
      freq_table <- table(var, useNA = "ifany")
      stats_summary <- paste0(
        "Level frequencies:\n",
        paste(
          names(freq_table), ": n = ", freq_table, " (", 
          round(prop.table(freq_table) * 100, 1), "%)",
          collapse = "\n"
        )
      )
    } else {
      stats_summary <- "Not applicable"
    }
    
    tibble(
      `Variable Name` = var_name,
      `Description` = attr(var, "label") %||% "",
      `Data Type` = class(var)[1],
      `N` = length(var),
      `Missing` = sum(is.na(var)),
      `Missing %` = round(mean(is.na(var)) * 100, 1),
      `Unique Values` = length(unique(na.omit(var))),
      `Value Labels` = value_labels,
      `Statistics` = stats_summary
    )
  })
}



wash <- read_rds("data/wash_main_labelled.rds")
water_data <- read_rds("data/water_data_clean.rds")

wash <- wash |> 
  mutate(sd01 = as.Date(sd01),
         mid_month = 15,
         mid_year = 06, 
         dob_approx = make_date(year_birth, mid_year, mid_month),
         sd01 = ifelse(is.na(sd01), dob_approx, sd01),
         sd01 = as.Date(sd01),
         interview_date = as.Date(interview_date),
         age = as.numeric(interview_date-sd01)/365.25,
         age = floor(age)) |> 
  filter(consent == "Yes")



wash_water <- wash |> 
  left_join(water_data, by = "respondent_id")


demos <- c("age", "age_cat", "marital_status", "education_level",
           "hhsize", "hhold_income" )


wash_vars <- c("wsp01", "wsp03", "wsp04", "wsp07", "wsp08", "wsp09", 
               "wsp10", "wsp11", "sfp01","shares_latrine","sfp03",
               "diaper_napkin","dispose_menstprod",
               "sfp10","dispose_solidwaste", "hp01")





demos <- c("e_coli","age", "sd02", "sd03", "sd07_2", "sd06")


wash_vars <- c("wsp01", "wsp03", "wsp04", "wsp07", "wsp08", "wsp09", 
               "wsp10", "wsp11", "sfp01","sfp02","sfp03",
               "sfp05","sfp09","sfp10","sfp11", "hp01")



all_vars <- c(demos, wash_vars)


wash_subset <- wash_water |> 
  select(all_of(all_vars))


dict_wash <- create_detailed_dictionary(wash_subset)


