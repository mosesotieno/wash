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
watertypes_dict <- read_xlsx("metadata/dict_water_types_main.xlsx")
water_types <- read_rds("data/water_types.rds")
water_data <- read_rds("data/water_data.rds")

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





# Cleaning character variables --------------------------------------------

wash <- wash |> 
  mutate(
    across(where(is.character), ~str_remove(., "\\(.+\\)")),
    across(where(is.character), ~str_remove_all(., "\\n|\\r")),
    across(where(is.character), ~str_squish(.))
  )
  


# Create factor variables -------------------------------------------------

fct_vars <- wash_dict |> 
  filter(col_type == "fct") |> 
  pull(new_var)


wash <- wash |> 
  mutate(across(fct_vars, as_factor))


# Apply variable labels ---------------------------------------------------

var_label(wash) <- setNames(wash_dict$new_label, wash_dict$new_var)

write_rds(wash, "data/wash_main_labelled.rds")




# Water types -------------------------------------------------------------

# Create named vector for renaming
rename_vector <- setNames(watertypes_dict$new_var, watertypes_dict$variable)

# Rename variables
water_types <- water_types %>%
  rename_with(~ rename_vector[.x], .cols = any_of(names(rename_vector))) |> 
  clean_names()



watertypes_dict <- watertypes_dict |> 
  mutate(new_var = make_clean_names(new_var))


# Apply variable labels for water types-----------------------------------------

var_label(water_types) <- setNames(watertypes_dict$new_label, watertypes_dict$new_var)


write_rds(water_types, "data/water_types_labelled.rds")
write_rds(watertypes_dict, "metadata/watertypes_dict.rds")



# Water Data --------------------------------------------------------------


attr(water_data[['water_type']], 'label') <- "Water type"
attr(water_data[['sample_source']], 'label') <- "Sample source"
attr(water_data[['date']], 'label') <- "Date"
attr(water_data[['e_coli']], 'label') <- "E-coli"
attr(water_data[['e_coli_100']], 'label') <- "E-coli * 100"
attr(water_data[['total_coliforms']], 'label') <- "Total Coliforms"
attr(water_data[['total_coliforms_100']], 'label') <- "Total Coliforms *100"

write_rds(water_data, "data/water_data_labelled.rds")


