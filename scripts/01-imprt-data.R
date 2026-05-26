library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

sanitation <- read_xlsx("data/Water-Sanitation-HPV.xlsx", sheet = 2, skip = 1)



sanitation <- sanitation |> 
  remove_empty(which = c("cols"))

dict_sat <- labelled::generate_dictionary(sanitation)


dict_sat <- dict_sat |> 
  select(pos, variable, col_type) |> 
  mutate(new_var = str_extract(variable, "\\w+.+\\. "),
         new_var = str_remove(new_var, "\\. $"))


write_xlsx(dict_sat, "metadata/dict_sat.xlsx")



write_rds(sanitation, "data/wash.rds")
