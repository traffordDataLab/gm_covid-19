#COVID-19 Reproduction number for GM

# Packages -----------------------------------------------------------------
require(EpiNow, quietly = TRUE)
require(NCoVUtils, quietly = TRUE)
require(furrr, quietly = TRUE)
require(future, quietly = TRUE)
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(readr, quietly = TRUE)
require(magrittr, quietly = TRUE)
require(forecastHybrid, quietly = TRUE)

# Confirmed cases
# Source: Public Health England
# URL: https://coronavirus.data.gov.uk

path_eng <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
gm_cases<- suppressMessages(readr::read_csv(file = path_eng)) %>%
  filter(`Area type` == "Lower tier local authority") %>%
  filter(`Area name` %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", "Stockport", "Tameside", "Trafford", "Wigan")) %>%
  mutate(date = lubridate::ymd(`Specimen date`)) %>%
  group_by(date) %>%
  summarise(confirm=sum(`Daily lab-confirmed cases`)) %>%
  select(date, confirm) %>%
  mutate(region="Greater Manchester", import_status="local") %>%
  arrange(date) %>%
  filter(!confirm==0)
      
# Shared delay ------------------------------------------------------------

delay_defs <- readRDS("delays.rds")

# Set up cores -----------------------------------------------------
if (!interactive()){
  options(future.fork.enable = TRUE)
}

future::plan("multiprocess", workers = round(future::availableCores() / 3))

# Run pipeline ----------------------------------------------------

EpiNow::regional_rt_pipeline(
  cases = gm_cases,
  delay_defs = delay_defs,
  target_folder = "gm",
  horizon = 14,
  nowcast_lag = 10,
  approx_delay = TRUE,
  report_forecast = TRUE,
  forecast_model = function(y, ...){EpiSoon::forecastHybrid_model(
    y = y[max(1, length(y) - 21):length(y)],
    model_params = list(models = "aefz", weights = "equal"),
    forecast_params = list(PI.combination = "mean"), ...)}
)

summarise_to_csv(
  results_dir = "gm",
  summary_dir = "csv",
  type = "area_name",
  date = "latest"
)

summary <-readRDS("gm/Greater Manchester/latest/region_summary.rds")
write_csv(summary,"csv/summary_2020-06-14.csv")
