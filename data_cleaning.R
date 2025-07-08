library(readxl)
library(tidyverse)
library(janitor)

# Load the data

### Data from the US Census ###
census_data_PA <- read_csv("data/cc-est2024-alldata-42.csv")

census_data_cleaned <- census_data_PA |>
  mutate(
    YEAR = case_when(
      YEAR == 1 ~ as.Date("2020-04-01"),
      YEAR == 2 ~ as.Date("2020-07-01"),
      YEAR == 3 ~ as.Date("2021-07-01"),
      YEAR == 4 ~ as.Date("2022-07-01"),
      YEAR == 5 ~ as.Date("2023-07-01"),
      YEAR == 6 ~ as.Date("2024-07-01"),
      TRUE ~ as.Date(NA)
    ),
    AGEGRP = case_when(
      AGEGRP == 0 ~ "Total",
      AGEGRP == 1 ~ "0-4",
      AGEGRP == 2 ~ "5-9",
      AGEGRP == 3 ~ "10-14",
      AGEGRP == 4 ~ "15-19",
      AGEGRP == 5 ~ "20-24",
      AGEGRP == 6 ~ "25-29",
      AGEGRP == 7 ~ "30-34",
      AGEGRP == 8 ~ "35-39",
      AGEGRP == 9 ~ "40-44",
      AGEGRP == 10 ~ "45-49",
      AGEGRP == 11 ~ "50-54",
      AGEGRP == 12 ~ "55-59",
      AGEGRP == 13 ~ "60-64",
      AGEGRP == 14 ~ "65-69",
      AGEGRP == 15 ~ "70-74",
      AGEGRP == 16 ~ "75-79",
      AGEGRP == 17 ~ "80-84",
      AGEGRP == 18 ~ "85+",
      TRUE ~ NA_character_
    )
  ) |>
  clean_names() |>
  rename(date = year,
         age_group = agegrp) |>
  select(-c(state, sumlev, stname)) |>
  filter(date != "2020-04-01")

### Data from the US Department of Agriculture ###
poverty_data <- read_xlsx("data/PovertyAreaMeasures2023.xlsx", sheet = "COUNTY PUBLIC DATA")

poverty_data_cleaned <- poverty_data |>
  clean_names() |>
  filter(stusab == "PA") |>
  mutate(county_name = str_remove(county_name, ", Pennsylvania")) |>
  select(-c(stusab, fips)) |>

### Medicaid data ###
medicaid_data <- read_csv("data/Rate_of_Women_on_Medical_Assistance__MA__Diagnosed_with_Opioid_Use_Disorder__OUD__during_Pregnancy_CY_2016-Current_Statewide_Department_of_Human_Services__DHS__20250624.csv")

medicaid_data_cleaned <- medicaid_data |>
  clean_names() |>
  filter(geographic_name != "Commonwealth") |>
  filter(grepl("Annual", time_period)) |>
  mutate(year = as.Date(paste0(year, "-07-01")))

### Join data sets ###
joined_data <- medicaid_data_cleaned |>
  left_join(census_data_cleaned, by = c("geographic_name" = "ctyname"), relationship = "many-to-many") |>
  left_join(poverty_data_cleaned, by = c("geographic_name" = "county_name"), relationship = "many-to-many") |>
  select(!geographic_area)
            
