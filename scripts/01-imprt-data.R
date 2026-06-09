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
dict_pilot <- read_xlsx("metadata/wash_dict_pilot.xlsx")
wash_hpv_quiz <- read_xlsx("metadata/WASH_HPV_QUESTIONNAIRE.xlsx")

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


