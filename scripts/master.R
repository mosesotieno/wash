source("scripts/01-imprt-data.R")


source("scripts/02-labelling.R")


source("scripts/03-data-preparation.R")


wash_labels[names(wash_labels) %in% wash_vars] |> View()
