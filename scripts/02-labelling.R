# Header ------------------------------------------------------------------

# Name: 02-labelling

# Purpose: Prepare the variable labels and value labels of the datasets

# Author: Moses Otieno

# Contacts: mosotieno25@gmail.com

# Date: 09 June 2026


# Packages ----------------------------------------------------------------


library(tidyverse)
library(labelled)
library(readxl)
library(writexl)
library(janitor)


# Imports -----------------------------------------------------------------


wash <- read_rds("data/wash_main.rds")
wash_dict <- read_xlsx("metadata/wash_main_dict.xlsx")


# Clean dictionary --------------------------------------------------------

symps <- c("Have you ever experienced any of the following symptoms in the past six months\\?/")
menses <- "What menstrual hygiene products do you use/did you use\\?/"
trt_water <- "If Yes, what method do you primarily use to treat your drinking water\\?/"
sti3 <- "If yes, which sexually transmitted infections have you had in the past six months\\?/"
  
  
wash_dict <- wash_dict |> 
  mutate(new_label = ifelse(is.na(new_label), variable, new_label),
         new_label = str_remove(new_label, "\\w+.+\\. "),
         new_label = str_replace(new_label, "How do you store your household water at home\\?/", "Stores water: "),
         new_label = str_replace(new_label, "In  the past 1 year  which family  planning method have  you used \\?/",
                                 "Family planning past year:"),
         new_label = str_replace(new_label, symps, "Experienced symptoms past 6 months: "),
         new_label = str_replace(new_label, menses, "Menstrual hygiene products used: "),
         new_label = str_replace(new_label, trt_water, "Primary method used to treat driniking water: "),
         new_label = str_replace(new_label, sti3, "STI past 3 months: "),
         new_label = str_remove(new_label, "\\(.+\\)"))


write_xlsx(wash_dict, "metadata/wash_main_dict.xlsx")


# Rename variables --------------------------------------------------------

# Create named vector for renaming
rename_vector <- setNames(wash_dict$new_var, wash_dict$variable)

# Rename variables
wash <- wash %>%
  rename_with(~ rename_vector[.x], .cols = any_of(names(rename_vector))) |> 
  clean_names()



wash_dict <- wash_dict |> 
  mutate(new_var = make_clean_names(new_var))


# Apply variable labels ---------------------------------------------------

var_label(wash) <- setNames(wash_dict$new_label, wash_dict$new_var)


write_rds(wash, "data/wash_main_labelled.rds")
