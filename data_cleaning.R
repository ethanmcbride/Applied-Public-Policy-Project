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
  select(-c(state, sumlev, stname, county)) |>
  filter(date != "2020-04-01") |>
  mutate(
    ctyname = str_remove(ctyname, " County"),
    ctyname = str_trim(ctyname),
  ) |>
  filter(age_group == "Total",
         date == "2020-07-01") |>
  select(-age_group) |>
  rename(county_name = ctyname,
         census_date = date)

### Data from the US Department of Agriculture ###
# poverty_data <- read_xlsx("data/PovertyAreaMeasures2023.xlsx", sheet = "COUNTY PUBLIC DATA")

more_poverty_data <- read_csv("data/HDPulse_data_export.csv", skip = 4)

more_poverty_data_cleaned <- more_poverty_data |>
  clean_names() |>
  rename(county_name = county,
         poverty_rate = value_percent) |>
  filter(str_detect(county_name, "County")) |>
  mutate(county_name = str_remove(county_name, " County"),
         county_name = str_trim(county_name))

# poverty_data_cleaned <- poverty_data |>
#   clean_names() |>
#   filter(stusab == "PA") |>
#   mutate(county_name = str_remove(county_name, " County, Pennsylvania"),
#          county_name = str_trim(county_name)) |>
#   select(-c(stusab, fips, geo_id_ct)) |>
#   select(county_name, everything())

### Medicaid data ###
medicaid_data <- read_csv("data/Rate_of_Women_on_Medical_Assistance__MA__Diagnosed_with_Opioid_Use_Disorder__OUD__during_Pregnancy_CY_2016-Current_Statewide_Department_of_Human_Services__DHS__20250624.csv")

medicaid_data_cleaned <- medicaid_data |>
  clean_names() |>
  filter(geographic_name != "Commonwealth") |>
  filter(grepl("Annual", time_period)) |>
  mutate(year = as.Date(paste0(year, "-07-01"))) |>
  mutate(geographic_name = str_remove(geographic_name, " County")) |>
  select(!c(geographic_area, gender, age, time_period, time_period_dates)) |>
  filter(year == "2019-07-01") |>
  rename(county_name = geographic_name,
         medicaid_date = year)

joint_data <- census_data_cleaned |>
  left_join(medicaid_data_cleaned,
            by = "county_name")

joint_data <- joint_data |>
  left_join(more_poverty_data_cleaned,
            by = "county_name")

joint_data_with_names <- joint_data |>
  rename(
    total_population = tot_pop,
    total_male = tot_male,
    total_female = tot_female,
    
    white_alone_male = wa_male,
    white_alone_female = wa_female,
    black_alone_male = ba_male,
    black_alone_female = ba_female,
    american_indian_alone_male = ia_male,
    american_indian_alone_female = ia_female,
    asian_alone_male = aa_male,
    asian_alone_female = aa_female,
    pacific_islander_alone_male = na_male,
    pacific_islander_alone_female = na_female,
    two_or_more_races_male = tom_male,
    two_or_more_races_female = tom_female,
    
    white_combination_male = wac_male,
    white_combination_female = wac_female,
    black_combination_male = bac_male,
    black_combination_female = bac_female,
    american_indian_combination_male = iac_male,
    american_indian_combination_female = iac_female,
    asian_combination_male = aac_male,
    asian_combination_female = aac_female,
    pacific_islander_combination_male = nac_male,
    pacific_islander_combination_female = nac_female,
    
    non_hispanic_male = nh_male,
    non_hispanic_female = nh_female,
    non_hispanic_white_alone_male = nhwa_male,
    non_hispanic_white_alone_female = nhwa_female,
    non_hispanic_black_alone_male = nhba_male,
    non_hispanic_black_alone_female = nhba_female,
    non_hispanic_american_indian_alone_male = nhia_male,
    non_hispanic_american_indian_alone_female = nhia_female,
    non_hispanic_asian_alone_male = nhaa_male,
    non_hispanic_asian_alone_female = nhaa_female,
    non_hispanic_pacific_islander_alone_male = nhna_male,
    non_hispanic_pacific_islander_alone_female = nhna_female,
    non_hispanic_two_or_more_races_male = nhtom_male,
    non_hispanic_two_or_more_races_female = nhtom_female,
    non_hispanic_white_combination_male = nhwac_male,
    non_hispanic_white_combination_female = nhwac_female,
    non_hispanic_black_combination_male = nhbac_male,
    non_hispanic_black_combination_female = nhbac_female,
    non_hispanic_american_indian_combination_male = nhiac_male,
    non_hispanic_american_indian_combination_female = nhiac_female,
    non_hispanic_asian_combination_male = nhaac_male,
    non_hispanic_asian_combination_female = nhaac_female,
    non_hispanic_pacific_islander_combination_male = nhnac_male,
    non_hispanic_pacific_islander_combination_female = nhnac_female,
    
    hispanic_male = h_male,
    hispanic_female = h_female,
    hispanic_white_alone_male = hwa_male,
    hispanic_white_alone_female = hwa_female,
    hispanic_black_alone_male = hba_male,
    hispanic_black_alone_female = hba_female,
    hispanic_american_indian_alone_male = hia_male,
    hispanic_american_indian_alone_female = hia_female,
    hispanic_asian_alone_male = haa_male,
    hispanic_asian_alone_female = haa_female,
    hispanic_pacific_islander_alone_male = hna_male,
    hispanic_pacific_islander_alone_female = hna_female,
    hispanic_two_or_more_races_male = htom_male,
    hispanic_two_or_more_races_female = htom_female,
    hispanic_white_combination_male = hwac_male,
    hispanic_white_combination_female = hwac_female,
    hispanic_black_combination_male = hbac_male,
    hispanic_black_combination_female = hbac_female,
    hispanic_american_indian_combination_male = hiac_male,
    hispanic_american_indian_combination_female = hiac_female,
    hispanic_asian_combination_male = haac_male,
    hispanic_asian_combination_female = haac_female,
    hispanic_pacific_islander_combination_male = hnac_male,
    hispanic_pacific_islander_combination_female = hnac_female
    ) |>
  mutate(
    ## Basic demographics ##
    pct_female = (total_female / total_population) * 100,
    pct_male = (total_male / total_population) * 100,
    ## Racial categories ##
    white_pop = white_alone_male + white_alone_female,
    black_pop = black_alone_male + black_alone_female,
    american_indian_pop = american_indian_alone_male + american_indian_alone_female,
    asian_pop = asian_alone_male + asian_alone_female,
    pacific_islander_pop = pacific_islander_alone_male + pacific_islander_alone_female,
    two_or_more_races_pop = two_or_more_races_male + two_or_more_races_female,
    ## Hispanic & hon-hispanic categories ##
    hispanic_pop = hispanic_male + hispanic_female,
    non_hispanic_pop = non_hispanic_male + non_hispanic_female,
    ## Race percentages
    pct_white = (white_pop / total_population) * 100,
    pct_black = (black_pop / total_population) * 100,
    pct_american_indian = (american_indian_pop / total_population) * 100,
    pct_asian = (asian_pop / total_population) * 100,
    pct_pacific_islander = (pacific_islander_pop / total_population) * 100,
    pct_two_or_more_races = (two_or_more_races_pop / total_population) * 100,
    ## Hispanic & non-hispanic percentages ##
    pct_hispanic = (hispanic_pop / total_population) * 100,
    pct_non_hispanic = (non_hispanic_pop / total_population) * 100
  )

unemployment_data <- read_xlsx("data/Unemployment2023.xlsx", 
                               sheet = "Unemployment Med HH Income",
                               skip = 4)
unemployment_data_cleaned <- unemployment_data |>
  clean_names() |>
  filter(state == "PA") |>
  filter(area_name != "Pennsylvania") |>
  mutate(area_name = str_remove(area_name, " County, PA"),
         area_name = str_trim(area_name)) |>
  rename(county_name = area_name)

joint_data_with_names <- joint_data_with_names |>
  left_join(unemployment_data_cleaned,
            by = "county_name") |>
  select(county_name, census_date, medicaid_date, everything())

### Load data from the Pennsylvania Department of Health ###
pa_stats <- read_csv("data/Stats_All.csv")

pa_stats_cleaned <- pa_stats |>
  clean_names() |>
  filter(measure %in% c("Percent cesarean section births", "Percent low birth weight births",
         "Percent prenatal care in 1st trimester births", "Percent preterm births", 
         "General fertility rate - ages 15 to 44", "Percent fair or poor general health")) |>
  select(measure, county_name, rate) |>
  mutate(
    measure = case_when(
      measure == "Percent cesarean section births" ~ "pct_cesarean_section",
      measure == "Percent low birth weight births" ~ "pct_low_birth_weight",
      measure == "Percent prenatal care in 1st trimester births" ~ "pct_prenatal_care_first_trimester",
      measure == "Percent preterm births" ~ "pct_preterm_births",
      measure == "General fertility rate - ages 15 to 44" ~ "fertility_rate",
      measure == "Percent fair or poor general health" ~ "pct_fair_poor_health",
      TRUE ~ measure
    )
  ) |>
  pivot_wider(
    names_from = measure,
    values_from = rate
  )

joint_data_with_names <- joint_data_with_names |>
  left_join(pa_stats_cleaned,
            by = "county_name")


# Write new file for data analysis #
write_csv(joint_data_with_names, "data/APPP_dataset.csv")

