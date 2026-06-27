# Clean Terna actual generation data and build the daily modelling dataset

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)
library(stringr)
library(readr)

raw_dir <- "data/raw"

files <- c(
  "terna_actual_generation_2021.xlsx",
  "terna_actual_generation_2022.xlsx",
  "terna_actual_generation_2023.xlsx",
  "terna_actual_generation_2024.xlsx",
  "terna_actual_generation_2025.xlsx"
)

file_paths <- file.path(raw_dir, files)

missing_files <- file_paths[!file.exists(file_paths)]
if (length(missing_files) > 0) {
  stop("Missing files:\n", paste(missing_files, collapse = "\n"))
}

parse_terna_datetime <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.POSIXct(x))
  }
  
  if (inherits(x, "Date")) {
    return(as.POSIXct(x))
  }
  
  if (is.numeric(x)) {
    return(as.POSIXct(x * 86400, origin = "1899-12-30", tz = "UTC"))
  }
  
  dmy_hms(as.character(x), tz = "UTC")
}

read_terna_file <- function(path) {
  suppressWarnings(
    read_excel(path)
  ) %>%
    clean_names() %>%
    mutate(source_file = basename(path))
}

raw_generation <- bind_rows(lapply(file_paths, read_terna_file))

required_cols <- c("date", "actual_generation", "primary_source")
if (!all(required_cols %in% names(raw_generation))) {
  stop("Unexpected columns: ", paste(names(raw_generation), collapse = ", "))
}

generation <- raw_generation %>%
  transmute(
    datetime = parse_terna_datetime(date),
    source = str_trim(primary_source),
    generation_mw = as.numeric(actual_generation),
    source_file
  ) %>%
  filter(
    !is.na(datetime),
    !is.na(source),
    !is.na(generation_mw)
  )

print(range(generation$datetime, na.rm = TRUE))
print(sort(unique(generation$source)))
print(generation %>% count(source, source_file))
print(colSums(is.na(generation)))

generation_energy <- generation %>%
  mutate(
    interval_hours = if_else(
      str_detect(source_file, "2025"),
      0.25,
      1
    ),
    energy_mwh = generation_mw * interval_hours,
    date = as.Date(datetime)
  )

interval_check <- generation_energy %>%
  group_by(source_file) %>%
  summarise(
    interval_hours = unique(interval_hours),
    .groups = "drop"
  )

print(interval_check)

daily_generation <- generation_energy %>%
  group_by(date, source) %>%
  summarise(
    energy_mwh = sum(energy_mwh, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

daily_wide <- daily_generation %>%
  select(date, source, energy_mwh) %>%
  pivot_wider(
    names_from = source,
    values_from = energy_mwh
  ) %>%
  clean_names()

source_cols <- c(
  "thermal",
  "hydro",
  "geothermal",
  "photovoltaic",
  "wind",
  "self_consumption"
)

missing_source_cols <- setdiff(source_cols, names(daily_wide))
if (length(missing_source_cols) > 0) {
  warning("Missing source columns: ", paste(missing_source_cols, collapse = ", "))
  for (col in missing_source_cols) {
    daily_wide[[col]] <- 0
  }
}

daily_clean <- daily_wide %>%
  arrange(date) %>%
  mutate(
    renewable_generation_mwh = hydro + geothermal + photovoltaic + wind,
    total_generation_excl_sc_mwh = thermal + hydro + geothermal + photovoltaic + wind,
    total_generation_incl_sc_mwh = total_generation_excl_sc_mwh + self_consumption,
    renewable_share_excl_sc = 100 * renewable_generation_mwh / total_generation_excl_sc_mwh,
    renewable_share_incl_sc = 100 * renewable_generation_mwh / total_generation_incl_sc_mwh,
    trend = row_number(),
    year = year(date),
    month = month(date),
    month_label = month(date, label = TRUE, abbr = TRUE),
    weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    day_of_year = yday(date),
    lag_1 = lag(renewable_share_excl_sc, 1),
    lag_7 = lag(renewable_share_excl_sc, 7),
    lag_14 = lag(renewable_share_excl_sc, 14),
    lag_30 = lag(renewable_share_excl_sc, 30)
  )

all_dates <- tibble(date = seq(min(daily_clean$date), max(daily_clean$date), by = "day"))

missing_dates <- anti_join(all_dates, daily_clean, by = "date")

validation_summary <- list(
  date_range = range(daily_clean$date),
  n_days = nrow(daily_clean),
  missing_dates = missing_dates,
  renewable_share_range_excl_sc = range(daily_clean$renewable_share_excl_sc, na.rm = TRUE),
  renewable_share_range_incl_sc = range(daily_clean$renewable_share_incl_sc, na.rm = TRUE),
  missing_values = colSums(is.na(daily_clean))
)

print(validation_summary)

invalid_share <- daily_clean %>%
  filter(
    renewable_share_excl_sc < 0 |
      renewable_share_excl_sc > 100 |
      renewable_share_incl_sc < 0 |
      renewable_share_incl_sc > 100
  )

if (nrow(invalid_share) > 0) {
  warning("Invalid renewable share values detected.")
  print(invalid_share)
}

readr::write_csv(daily_clean, "data/processed/renewable_daily_clean.csv")