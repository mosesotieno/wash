library(tidyverse)
library(dmngt)


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

sexvars <- c("wsr01", "wsr02", "wsr04", "wsr05", "wsr01")

wash <- wash |> 
  mutate(sd01 = as.Date(sd01),
         # sd03b = case_when(sd03b == "Don't know/can't remember" ~ 6,
         #                   sd03b == "August" ~ 8,
         #                   sd03b == "December" ~ 12),
         # sd03b = as.numeric(sd03b),
         # sd03c = as.numeric(sd03c),
         mid_month = 15,
         
         dob_approx = make_date(year_birth, 06, mid_month),
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
         #sd09_3 = as.factor(sd09_3),
         wsp01 = str_remove( wsp01, " \\(.*\\)"),
         wsp01 = ifelse(str_detect(wsp01, "Borehole"), "Borehole", wsp01),
         wsp01 = ifelse(str_detect(wsp01, "Rain"), "Rainwater", wsp01),
         wsp01 = as.factor(wsp01),
         wsp01 = fct_infreq(wsp01),
         across(c("wsp01", "wsp04","wsp08", "wsp11"), as_factor),
         #across(c("wqa01", "wqa02", "wqa03", "wqa04", "wqa05", "wqa06"), as_factor),
         across(sexvars, as_factor),
         across(where(is.character), ~str_remove(., "\\(.+\\)"))
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
attr(wash[['wsp01']], 'label') <- "Primary source of water general use"
attr(wash[['wsp04']], 'label') <- "Time taking to fetch water"
attr(wash[['wsp08']], 'label') <- "Believe primary source of water is clean"
attr(wash[['wsp10']], 'label') <- "Treats drinking water"
attr(wash[['wsp11']], 'label') <- "Availability of drinking water"

attr(wash[['wsr01']], 'label') <- "Currently sexually active"
attr(wash[['wsr02']], 'label') <- "Frequency of washing genital area"
#attr(wash[['wsr03']], 'label') <- "Bathe genital area after sex"
attr(wash[['wsr04']], 'label') <- "Partner bathes genital area after sex"
attr(wash[['wsr05']], 'label') <- "Partner is circumcised"
#attr(wash[['wsr06']], 'label') <- "What used in bathing genital area"
#attr(wash[['wsr08']], 'label') <- "Consistent use of condm with partner"
#attr(wash[['wsr09']], 'label') <- "Number of sexual partners last 2 years"



write_rds(wash, "data/wash_main_clean.rds")





