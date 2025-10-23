### Data Cleaning and EDA
## Data set up and initial check

# load packages
library(tidyverse)
library(here)
library(dplyr)
library(lubridate)

# loading in data
global_trust <- read_csv(here("data/global_trust_rate.csv")) |> 
  janitor::clean_names() 

global_covid <- read_csv(here("data/compact.csv")) |> 
  janitor::clean_names() |> 
  # filtered for latest date with data (3/16/2025)
  filter(date == "2024-12-31") |> 
  select(country, 
         total_cases, 
         total_cases_per_million,
         total_deaths, 
         total_deaths_per_million)

# global_excess_deaths <- read_csv(here("data/all_monthly_excess_deaths.csv")) |> 
  # janitor::clean_names() |> 
  # filter(end_date == "2024-12-31") |> 
  # select(country, expected_deaths, excess_deaths)

# make a global dataset
# add type of political regime 
political_regime <- read_csv(here("data/political_regime.csv")) |> 
  janitor::clean_names() |> 
  filter(year == 2024) |> 
  rename(country = entity) |> 
  mutate(political_regime = factor(political_regime, 
                                      levels = c(0, 1, 2, 3),
                                      labels = c("close autocracy",
                                                 "electoral autocracy", 
                                                 "electoral democracy", 
                                                 "liberal democracy"))) |> 
  select(country, political_regime)

global <- global_trust |> left_join(global_covid) |> 
  left_join(political_regime)

global_trust_ed <- global |> left_join(global_excess_deaths) |> 
  filter(country %in% c("Armenia", 
                      "Azerbaijan", 
                      "Bosnia and Herzegovina",
                      "Egypt", 
                      "Faroe Islands",
                      "Japan",
                      "Kazakhstan",
                      "Kosovo",
                      "Kyrgyzstan",
                      "Macao",
                      "Malaysia",
                      "Mongolia",
                      "North Macedonia",
                      "Oman", 
                      "Russia", 
                      "San Marino",
                      "Serbia", 
                      "Singapore",
                      "Taiwan", 
                      "Thailand"))

# Saving out new data
save(global, file  = here("data/global.rda"))
save(global_trust_ed, file  = here("data/global_trust_ed.rda"))


# Skim and data distribution
## the id variable is country

skim_global <- skimr::skim_without_charts(global)

save(skim_global, file = here("figures_tables/skim_global.rda"))

