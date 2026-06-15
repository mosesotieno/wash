# Header ------------------------------------------------------------------

# Name: 03-data-preparation

# Purpose: Prepare the datasets for analysis 

# Author: Moses Otieno

# Contacts: mosotieno25@gmail.com

# Date: 09 June 2026


# Pakcages ----------------------------------------------------------------

library(janitor)
library(tidyverse)
library(dmngt)


# Imports -----------------------------------------------------------------


wash <- read_rds("data/wash_main_labelled.rds")
water_types <- read_rds("data/water_types_labelled.rds")
watertypes_dict <- read_rds("metadata/watertypes_dict.rds")
water_data <- read_rds("data/water_data_labelled.rds")



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



# Clean dob ---------------------------------------------------------------


# R128 did not know her dob and year of birth recorded as 999

# She was single and in Primary school. We can get the median dob and age of 
# of women almost similar to her and assign that dob and age to her 

wash |> 
  summarise(med_age = median(age), 
            med_dob = median(sd01),
            .by = c(sd02, sd03)) |> 
  filter(sd03 == "Primary school")

sprima_dob = as.Date("1992-04-04")
sprima_age = 34



wash <- wash |> 
  mutate(age = ifelse(respondent_id == "R128", sprima_age, age),
         sd01 = ifelse(respondent_id == "R128", sprima_dob, sd01),
         sd01 = as.Date(sd01),
         )




# Create age bands --------------------------------------------------------

wash <- wash |> 
  mutate(age_cat = case_when(between(age, 15, 19)~"15-19",
                             between(age, 20, 24)~"20-24",
                             between(age, 25, 34)~"25-34",
                             between(age, 35, 44)~"35-44",
                             between(age, 45, 64)~"45-64",
                             ),
         age_cat = as.factor(age_cat))





# Recode variables --------------------------------------------------------


wash <- wash |> 
  mutate(marital_status = ifelse(str_detect(sd02, "Married"), "Married", as.character(sd02)),
         marital_status = factor(marital_status, levels = c("Single", "Married", "Divorced/Separated", "Widowed")),
         education_level = fct_collapse(sd03, "None/Primary" = c("Primary school", "No formal education"),
                                      "University/Tertiary" = c("University/Higher education", "Tertiary/Vocational training"),
                                      "Secondary" = "Secondary school"),
         education_level = fct_relevel(education_level, "None/Primary"),
         occupation = fct_collapse(sd04, "Farming" = "Farming/Agriculture",
                                   "Housewife" = "Housewife/Homemaker",
                                   "Small business owner" = "Small business owner/Trader/Kiosk",
                                   "Other" = c("Other", "Artisan/Mining/Carpentry", "Sales person", "Formal employment",
                                               "Fishing")),
         hhsize = case_when(between(sd07_2, 1, 3 ) ~ "1-3",
                            between(sd07_2, 4, 6 ) ~ "4-6",
                            between(sd07_2, 7, 9 ) ~ "7-9",
                            between(sd07_2, 10, 15 ) ~ "10+",
                            ),
         hhsize = factor(hhsize, levels = c("1-3", "4-6", "7-9", "10+")),
         wsp04 = fct_relevel(wsp04, c("< 5 minutes", "Less than 30 minutes", "30 minutes - 1 hour")),
         wsp04 = fct_recode(wsp04, "< 30 minutes"  = "Less than 30 minutes"),
         sfp08 = str_remove(sfp08, "N/A"),
         sfp08 = str_squish(sfp08),
         sfp08 = na_if(sfp08, ""),
         pad_category = case_when(
           sfp08 == "Sanitary towels" ~ "Sanitary towels only",
           str_detect(sfp08, "Sanitary towels") & sfp08 != "Sanitary towels" ~ "Sanitary towels with other products",
           !str_detect(sfp08, "Sanitary towels") & !is.na(sfp08) ~ "Other combinations (no sanitary towels)",
           TRUE ~ NA_character_
         ),
         pad_category = factor(pad_category, levels = c("Sanitary towels only", "Sanitary towels with other products", "Other combinations (no sanitary towels)")),
         shares_latrine = ifelse(sfp02 == "It is shared with other households", "Yes", "No"),
         shares_latrine = ifelse(sfp01 == "No toilet/latrine facility", "No", shares_latrine),
         num_sharing_latrine = sfp02_1,
         num_sharing_latrine = ifelse(shares_latrine == "No", 0, num_sharing_latrine),
         shares_latrine = ifelse(num_sharing_latrine == 0, "No", shares_latrine),
         shares_latrine = as.factor(shares_latrine),
         ) 


wash |> count(num_sharing_latrine, shares_latrine)


# Reorer factor levels ----------------------------------------------------

wash <- wash |> 
  mutate(sd08 = fct_relevel(sd08, "No"),
         across(where(is.factor), ~fct_relevel(., "Other",after = Inf)),
         across(where(is.factor), ~fct_recode(., NULL = "N/A")))


# Label variables ---------------------------------------------------------

attr(wash[['age']], 'label') <- "Age"
attr(wash[['age_cat']], 'label') <- "Age category"
attr(wash[['hhold_income']], 'label') <- "Monthly household income"
attr(wash[['marital_status']], 'label') <- "Marital status"
attr(wash[['education_level']], 'label') <- "Highest level of education"
attr(wash[['occupation']], 'label') <- "Primary occupation"
attr(wash[['hhsize']], 'label') <- "Number of people living in the household"
attr(wash[['pad_category']], 'label') <- "Menstrual hygiene product used"
attr(wash[['shares_latrine']], 'label') <- "Shares latrine/toilet facilities with other households"
attr(wash[['num_sharing_latrine']], 'label') <- "Number of households sharing latrine/toilet facilities"




# Merge water types with main dataset -------------------------------------


water_types_labels <- labels_keep(water_types)

water_types <- water_types |> 
  select(water_source_sample, starts_with("wqa"), submission_id) |> 
  select(!c(wqa08, wqa08a))

dups_water_types <- water_types |> 
  get_dupes(submission_id) 


distinct_water_types <- water_types |> 
  anti_join(dups_water_types, by = "submission_id")



dups_water_types <- dups_water_types |> 
  distinct() 



dups_water_types |> 
  filter(water_source_sample %in% c("Rain", "Rain water", "Rainwater")) |> 
  get_dupes(submission_id)


dups_water_types <- dups_water_types |> 
  filter(! water_source_sample %in% c("Rain", "Rain water", "Rainwater") | submission_id == "718228739") # n = 51


dups_water_types <- dups_water_types |> 
  filter(!c(submission_id == "723818710" & wqa07 == 6.4)) |> 
  filter(!c(submission_id == "720561982" & wqa07 == 6.4)) |> 
  filter(!c(submission_id == "718479896" & water_source_sample == "Pond"),
         !c(submission_id == "720524261" & water_source_sample == "Pond"),
         !c(submission_id == "720530222" & water_source_sample == "Water Pan"),
         !c(submission_id == "721381218" & water_source_sample == "River"),
         !c(submission_id == "721381576" & water_source_sample == "River"),
         !c(submission_id == "722596386" & water_source_sample == "Piped water"))


water_types <- distinct_water_types |> 
  bind_rows(dups_water_types) |> 
  select(!c(dupe_count))

water_types <- labels_restore(water_types, water_types_labels)


# water_types <- water_types |> 
#   group_by(submission_id) |> 
#   mutate(index = row_number()) |> 
#   ungroup()
# 
# 
# water_types <- water_types |> 
#   pivot_wider(id_cols = submission_id, names_from = index, values_from = starts_with("wqa") )


wash <- wash |> 
  left_join(water_types, by = c("id"="submission_id"))



wash <- wash |> 
  mutate(
    ph_group = case_when(
      wqa07 < 6.5 ~ "Acidic (<6.5)",
      wqa07 >= 6.5 & wqa07 < 7.5 ~ "Neutral (6.5-7.5)",
      wqa07 >= 7.5 & wqa07 < 8.5 ~ "Alkaline (7.5-8.5)",
      wqa07 >= 8.5 ~ "Highly Alkaline (≥8.5)",
      TRUE ~ "Missing"
    ),
    ph_group = factor(ph_group, levels = c("Acidic (<6.5)", "Neutral (6.5-7.5)", "Alkaline (7.5-8.5)",
                                           "Highly Alkaline (≥8.5)"
                                           )))




wash <- wash |> 
  mutate(ph_group = fct_drop(ph_group))



attr(wash[['ph_group']], 'label') <- "pH Level"





# Water Data --------------------------------------------------------------


dups_waterdata <- water_data |> 
  get_dupes(respondent_id) 


distinct_waterdata <- water_data |> 
  anti_join(dups_waterdata, by = "respondent_id")

dups_waterdata <- dups_waterdata |> 
  distinct() 

dups_waterdata <- dups_waterdata |> 
  filter(! water_type %in% c("Rain", "Rain water", "Rainwater")) # n = 51


dups_waterdata <- dups_waterdata |> 
  filter(! c(respondent_id == "R185" & water_type == "Piped water")) |> 
  filter(! c(respondent_id == "R22" & water_type == "Water pan")) |> 
  filter(! c(respondent_id == "R265" & water_type == "Piped water")) |> 
  filter(! c(respondent_id == "R270" & water_type == "River")) |> 
  filter(! c(respondent_id == "R275" & water_type == "Lake")) |> 
  filter(! c(respondent_id == "R284" & water_type == "Piped water"))


water_data <- distinct_waterdata |> 
  bind_rows(dups_waterdata)



wash <- wash |> 
  left_join(water_data, by = "respondent_id")


water_data |> anti_join(wash, by = "respondent_id")



write_rds(wash, "data/wash_main_clean.rds")





