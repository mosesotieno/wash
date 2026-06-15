# Header ------------------------------------------------------------------

# Name: 01-import-data.R

# Purpose: Importing the datasets and data dictionary for WASH analysis

# Author: Moses Otieno

# Contacts: mosotieno25@gmail.com

# Date: 09 June 2026

# Packages ----------------------------------------------------------------

library(janitor)
library(writexl)
library(readxl)
library(tidyverse)


# Import datasets ---------------------------------------------------------


sanitation <- read_xlsx("data/WASH HPV MAIN STUDY DATASET.xlsx", sheet = 1)
water_types <- read_xlsx("data/WASH HPV MAIN STUDY DATASET.xlsx", sheet = 3)
dict_pilot <- read_xlsx("metadata/wash_dict_pilot.xlsx")
wash_hpv_quiz <- read_xlsx("metadata/WASH_HPV_QUESTIONNAIRE.xlsx")
water_data <- read_xlsx("data/Water Data- Main Study.xlsx")

sanitation <- sanitation |> 
  remove_empty(which = c("cols"))

dict_sat <- labelled::generate_dictionary(sanitation)


dict_sat <- dict_sat |> 
  select(pos, variable, col_type) |> 
  mutate(new_var = str_extract(variable, "\\w+.+\\. "),
         new_var = str_remove(new_var, "\\. $"))


dict_pilot <- dict_pilot |> 
  select(variable, new_label)


dict_sat <- dict_sat |> 
  left_join(dict_pilot, by = "variable")

write_xlsx(dict_sat, "metadata/dict_sat_main.xlsx")


write_rds(sanitation, "data/wash_main.rds")



# Questionnaire -----------------------------------------------------------

wash_hpv_quiz <- wash_hpv_quiz |> 
  select(type, name, `label::English (en)`, relevant, constraint) |> 
  rename(quiz_text = `label::English (en)`)



write_rds(wash_hpv_quiz, "metadata/wash_hpv_quiz.rds")




# Water types -------------------------------------------------------------

water_types <- water_types |> 
  mutate(
    across(where(is.character), ~str_remove(., "\\(.+\\)")),
    across(where(is.character), ~str_remove_all(., "\\n|\\r")),
    across(where(is.character), ~str_squish(.))
  )


write_rds(water_types, "data/water_types.rds")


dict_types <- labelled::generate_dictionary(water_types)


dict_types <- dict_types |> 
  select(pos, variable, col_type) |> 
  mutate(new_var = str_extract(variable, "\\w+.+\\. "),
         new_var = str_remove(new_var, "\\. $"))


write_xlsx(dict_types, "metadata/dict_water_types.xlsx")



# Water Data Results ------------------------------------------------------

water_data <- water_data |> 
  clean_names() |> 
  rename(respondent_id = enter_the_respondent_id)

write_rds(water_data, "data/water_data.rds")


