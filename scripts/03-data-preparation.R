# Header ------------------------------------------------------------------

# Name: 03-data-preparation

# Purpose: Prepare the datasets for analysis 

# Author: Moses Otieno

# Contacts: mosotieno25@gmail.com

# Date: 09 June 2026


# Pakcages ----------------------------------------------------------------


library(tidyverse)
library(dmngt)


# Imports -----------------------------------------------------------------


wash <- read_rds("data/wash_main_labelled.rds")


wash_labels <- labels_keep(wash)



# Functions ---------------------------------------------------------------

get_binary_vars <- function(data) {
  result <- sapply(data, function(col) {
    unique_vals <- unique(na.omit(col))
    all(unique_vals %in% c(0, 1)) && length(unique_vals) <= 2
  })
  return(names(data)[result])
}



# Data Preparation --------------------------------------------------------

less_5k <- c("<1000", "1001-2000","2001-3000","3001-4000","4001-5000")

sexvars <- c("wsr01", "wsr02", "wsr04", "wsr05")

wash <- wash |> 
  mutate(sd01 = as.Date(sd01),
         mid_month = 15,
         mid_year = 06, 
         dob_approx = make_date(year_birth, mid_year, mid_month),
         sd01 = ifelse(is.na(sd01), dob_approx, sd01),
         sd01 = as.Date(sd01),
         interview_date = as.Date(interview_date),
         age = as.numeric(interview_date-sd01)/365.25,
         age = floor(age),
         hhold_income = case_when(sd06 %in% less_5k ~ "<=5000",
                                  sd06 == "5001-10000" ~ "5001-10000",
                                  sd06 %in% c("10001-20000", "20001-50000") ~ ">10000",
                                  sd06 %in% c("Don't know", "Prefer not to say") ~ "Dont know/not say"),
         hhold_income = factor(hhold_income, levels = c("<=5000", "5001-10000", ">10000", "Dont know/not say")),
         household_size = as.numeric(household_size),
         wsp01 = str_remove( wsp01, " \\(.*\\)"),
         wsp01 = ifelse(str_detect(wsp01, "Borehole"), "Borehole", wsp01),
         wsp01 = ifelse(str_detect(wsp01, "Rain"), "Rainwater", wsp01),
         wsp01 = as.factor(wsp01),
         wsp01 = fct_infreq(wsp01),
         across(c("wsp01", "wsp04","wsp08", "wsp11"), as_factor),
         across(sexvars, as_factor),
         
         )

# Multiselect --------------------------------------------------------------


bin_vars <- get_binary_vars(wash)
bin_vars <- bin_vars[!bin_vars%in% "refusal"]


wash <- wash |> 
  mutate(across(bin_vars, ~factor(., levels = c(0, 1), labels = c("No", "Yes"))))

wash <- labels_restore(wash, wash_labels)

# Label variables ---------------------------------------------------------

attr(wash[['age']], 'label') <- "Age"
attr(wash[['hhold_income']], 'label') <- "Monthly household income"


write_rds(wash, "data/wash_main_clean.rds")





