# CUSUM Control Chart #

library(tidyverse) ; library(lubridate) ; library(qcc)

local_authority <- "Leicester"
period_since <- "2020-06-01"

# Daily confirmed coronavirus cases
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk

cases <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
  mutate(`Specimen date` = as.Date(`Specimen date`, format = "%Y-%m-%d")) %>% 
  filter(`Area type` == "Lower tier local authority") %>%
  select(date = `Specimen date`,
         area_code = `Area code`,
         area_name = `Area name`,
         new_cases = `Daily lab-confirmed cases`) %>% 
  arrange(date) %>% 
  group_by(area_code, area_name) %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(new_cases = replace_na(new_cases, 0),
         cum_cases = cumsum(new_cases)) %>% 
  ungroup() %>% 
  fill(area_name) %>% 
  filter(area_name == local_authority, 
         # include only 'complete' cases
         date >= period_since & date <= max(date)-days(5))

# Cusum chart
# Parameters derive from Montgomery (2009), Chapter 9
xbar <- mean(cases$new_cases)
sigma <- sd(cases$new_cases)
h <- 4 # decision limit
k <- 0.5 # reference value

cusum_chart <- cusum(cases$new_cases, 
               center = xbar, 
               std.dev = sigma,
               se.shift = k,
               decision.interval = h, 
               labels = cases$date,
               plot = FALSE)

summary(cusum_chart)

plot(cusum_chart,
     title = paste0("Daily confirmed COVID-19 cases in ", local_authority, " since ", 
                    format(as.Date(period_since), '%d %B %Y')),
     
     add.stats = FALSE)
