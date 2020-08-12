# COVID-19 vulnerabilities for GM districts #

library(tidyverse) ; library(httr) ; library(rvest) ; library(readxl)

gm <- c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")

# Mid-2019 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327937...1820328318&date=latest&gender=0&c_age=200&measures=20100&select=geography_name,obs_value") %>% 
  filter(GEOGRAPHY_NAME %in% gm) %>% 
  rename(area_name = GEOGRAPHY_NAME, population = OBS_VALUE) 

# COVID-9 deaths
# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard

tmp <- tempfile(fileext = ".xlsx")

ext <- read_html("https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard") %>% 
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\.xlsx") %>% 
  .[[1]]

GET(url = paste0("https://www.ons.gov.uk", ext),
    write_disk(tmp))

covid_deaths <- read_xlsx(tmp, sheet = 4, skip = 3) %>%
  filter(`Area name` %in% gm , `Cause of death` == "COVID 19") %>% 
  group_by(`Area name`) %>% 
  summarise(value = sum(`Number of deaths`)) %>% 
  rename(area_name = `Area name`) %>% 
  left_join(population, by = "area_name") %>% 
  mutate(covid_deaths = round(value/population*100000,1)) %>% 
  select(area_name, covid_deaths)

# Black, Asian and Minority Ethnic (BAME)
# Source: 2011 Census
# URL: https://www.nomisweb.co.uk/census/2011/ks201ew

bame <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_608_1.data.csv?date=latest&geography=1946157081...1946157090&rural_urban=0&cell=200,300,400,500&measures=20301&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name") %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  summarise(bame = sum(OBS_VALUE)/100) %>% 
  rename(area_name = GEOGRAPHY_NAME)

# Income deprivation
# Source: IoD 2019, MHCLG
# URL: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
# Notes: rank of average score

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833995/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx",
    write_disk(tmp))

income <- read_xlsx(tmp, sheet = 3) %>% 
  filter(`Local Authority District name (2019)` %in% gm) %>% 
  select(area_name = `Local Authority District name (2019)`, income = `Income - Rank of average score`)

# Over 80s
# Source: ONS
# URL: https://www.nomisweb.co.uk/datasets/pestnew

over80 <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_31_1.data.csv?geography=1811939355...1811939364&date=latest&sex=7&age=18,19&measures=20100,20301&select=date_name,geography_name,geography_code,sex_name,age_name,measures_name,obs_value,obs_status_name") %>% 
  filter(MEASURES_NAME == "Percent") %>% 
  group_by(GEOGRAPHY_NAME) %>% 
  summarise(over80 = sum(OBS_VALUE)/100) %>% 
  rename(area_name = GEOGRAPHY_NAME)

# Overcrowded households
# 2011 Census
# URL: https://www.nomisweb.co.uk/census/2011/qs412ew
# Notes: with an occupancy rating of -1 or below

overcrowded <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_544_1.data.csv?date=latest&geography=1946157081...1946157090&rural_urban=0&occratbroom=0,4,5&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,occratbroom_name,measures_name,obs_value,obs_status_name") %>% 
  mutate(OCCRATBROOM_NAME = case_when(OCCRATBROOM_NAME == "All categories: Occupancy rating bedrooms" ~ "Total", TRUE ~ "Overcrowded")) %>% 
  group_by(GEOGRAPHY_NAME, OCCRATBROOM_NAME) %>% 
  summarise(OBS_VALUE = sum(OBS_VALUE)) %>%
  pivot_wider(names_from = OCCRATBROOM_NAME, values_from = OBS_VALUE) %>% 
  mutate(overcrowded = Overcrowded/Total) %>% 
  select(area_name = GEOGRAPHY_NAME, overcrowded)

# join datasets

df <- left_join(covid_deaths, bame) %>% 
  left_join(over80) %>% 
  left_join(income) %>% 
  left_join(overcrowded)

# create interactive table
library(reactable) ; library(htmltools)

with_tooltip <- function(value, tooltip) {
  span(style = "text-decoration: underline; text-decoration-style: dotted;", title = tooltip, value)
}

reactable(
  df,
  columns = list(
    area_name = colDef(name = "Local authority"),
    covid_deaths = colDef(header = with_tooltip("COVID-19 deaths", "Deaths per 100,000 population up to 31 July 2020 | Source: ONS")),
    bame = colDef(header = with_tooltip("BAME", "Black, Asian and Minority Ethnic group | Source: 2011 Census"), format = colFormat(percent = TRUE, digits = 1)),
    over80 = colDef(header = with_tooltip("Over 80s", "Residents aged 80 or over | Source: Mid-2019 population estimates"), format = colFormat(percent = TRUE, digits = 1)),
    income = colDef(header = with_tooltip("Income deprivation", "Rank of average score | Source: IoD 2019")),
    overcrowded = colDef(header = with_tooltip("Overcrowded housing", "Households with an occupancy rating of -1 or below | Source: 2011 Census"), format = colFormat(percent = TRUE, digits = 1))
  )
)
