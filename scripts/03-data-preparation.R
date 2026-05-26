library(tidyverse)



wash <- read_rds("data/wash_labelled.rds")




# Data Preparation --------------------------------------------------------

less_5k <- c("<1000", "1001-2000","2001-3000","3001-4000","4001-5000")



wash <- wash |> 
  mutate(sd03 = as.Date(sd03),
         interview_date = as.Date(interview_date),
         age = as.numeric(interview_date-sd03)/365.25,
         age = floor(age),
         hhold_income = case_when(sd08 %in% less_5k ~ "<=5000",
                                  sd08 == "5001-10000" ~ "5001-10000",
                                  sd08 %in% c("10001-20000", "20001-50000") ~ ">10000",
                                  sd08 %in% c("Don't know", "Prefer not to say") ~ "Dont know/not say"),
         hhold_income = factor(hhold_income, levels = c("<=5000", "5001-10000", ">10000", "Dont know/not say")))



# Label variables ---------------------------------------------------------

attr(wash[['age']], 'label') <- "Age"
attr(wash[['hhold_income']], 'label') <- "Household income"



write_rds(wash, "data/wash_clean.rds")
