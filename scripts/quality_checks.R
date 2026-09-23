

wash <- read_rds("data/wash_main_clean.rds")

wash_consented <- wash |> filter(consent == "Yes")
  

# Missing ecolli ----------------------------------------------------------



missing_ecolli <- wash_consented |> 
  filter(is.na(e_coli)) |> 
  select(respondent_id, e_coli, total_coliforms)


write_xlsx(missing_ecolli, "reports/missing_ecolli_info.xlsx")
