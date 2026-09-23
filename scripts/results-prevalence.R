library(readxl)
library(tidyverse)
library(janitor)
library(labelled)

# Import data -------------------------------------------------------------

results_hpv <- read_xlsx("data/HPV Results.xlsx")
results_hpv <- read_xlsx("data/Compiled- Updated.xlsx")

# Data Preparation --------------------------------------------------------

varlabels <- names(results_hpv)
varlabels <- str_replace_all(varlabels, "_", " ")

results_hpv <- results_hpv |> 
  clean_names()



lgl_vars <- c("is_invalid", "is_inconclusive", "is_retest")


results_hpv <- results_hpv |> 
  mutate(across(c(lgl_vars), ~ifelse(.==1, "Yes", "No"))) |> 
  mutate(across(lgl_vars, ~as.factor(.)))


var_label(results_hpv) <- setNames(varlabels, names(results_hpv))

results_hpv <- results_hpv |> 
  mutate(respondent_id = str_replace(sample_id, "^1", "R"))



# Save  -------------------------------------------------------------------

write_rds(results_hpv, "data/results_hpv.rds")
