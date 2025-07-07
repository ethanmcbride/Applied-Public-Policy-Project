library(readxl)
library(tidyverse)
library(janitor)

# Load the data

census_data_PA <- read_csv("data/cc-est2024-alldata-42.csv")

census_data_cleaned_v2 <- census_data_PA |>
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
  select(-c(state, sumlev, stname))

poverty_data <- read_xlsx("data/PovertyAreaMeasures2023.xlsx", sheet = "COUNTY PUBLIC DATA")

poverty_data_cleaned <- poverty_data |>
  clean_names() |>
  filter(stusab == "PA")

