library(readxl)
library(tidyverse)

# Load the data
data <- read_csv("Rate_of_Women_on_Medical_Assistance__MA__Diagnosed_with_Opioid_Use_Disorder__OUD__during_Pregnancy_CY_2016-Current_Statewide_Department_of_Human_Services__DHS__20250624.csv")
unique(data$`Type of Rate`) ### Only "rate per 1,000 deliveries"

# Filter
allegheny_county <- data |>
  filter(`Geographic Name` == "Allegheny County")