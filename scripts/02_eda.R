library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(zoo)
library(scales)
library(forecast)

daily_data <- readr::read_csv(
  "data/processed/renewable_daily_clean.csv",
  show_col_types = FALSE
)

dir.create("figures", showWarnings = FALSE)
unlink("figures/*.png")

theme_project <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, colour = "grey35"),
      plot.caption = element_text(size = 9, colour = "grey45", hjust = 0),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10, colour = "grey25"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "grey90"),
      panel.grid.major.y = element_line(colour = "grey88"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.position = "right"
    )
}

source_labels <- c(
  thermal = "Thermal",
  hydro = "Hydro",
  geothermal = "Geothermal",
  photovoltaic = "Photovoltaic",
  wind = "Wind"
)

renewable_labels <- c(
  hydro = "Hydro",
  geothermal = "Geothermal",
  photovoltaic = "Photovoltaic",
  wind = "Wind"
)

daily_data <- daily_data %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    month_name = factor(month.abb[month], levels = month.abb),
    weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    split = if_else(date < as.Date("2025-01-01"), "Training period", "Test period")
  )

summary_stats <- daily_data %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    n_days = n(),
    mean_renewable_share = mean(renewable_share_excl_sc, na.rm = TRUE),
    sd_renewable_share = sd(renewable_share_excl_sc, na.rm = TRUE),
    min_renewable_share = min(renewable_share_excl_sc, na.rm = TRUE),
    max_renewable_share = max(renewable_share_excl_sc, na.rm = TRUE)
  )

print(summary_stats)

p1_data <- daily_data %>%
  arrange(date) %>%
  mutate(
    renewable_share_30d = zoo::rollmean(
      renewable_share_excl_sc,
      k = 30,
      fill = NA,
      align = "right"
    )
  )

p1 <- ggplot(p1_data, aes(x = date)) +
  geom_line(aes(y = renewable_share_excl_sc), linewidth = 0.25, alpha = 0.35) +
  geom_line(
    data = p1_data %>% filter(!is.na(renewable_share_30d)),
    aes(y = renewable_share_30d),
    linewidth = 0.9
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Daily renewable electricity share in Italy",
    subtitle = "Daily values with 30-day rolling average, 2021–2025",
    x = NULL,
    y = "Renewable share",
    caption = "Renewable share excludes self-consumption from the denominator."
  ) +
  theme_project()

ggsave("figures/01_renewable_share_trend.png", p1, width = 11, height = 5.8, dpi = 320)

p2_data <- daily_data %>%
  group_by(year, month, month_name) %>%
  summarise(
    renewable_share = mean(renewable_share_excl_sc, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(p2_data, aes(x = month_name, y = renewable_share, group = factor(year), colour = factor(year))) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Monthly seasonality of renewable electricity share",
    subtitle = "Monthly averages reveal recurring seasonal patterns and differences across years",
    x = NULL,
    y = "Average renewable share",
    colour = "Year"
  ) +
  theme_project()

ggsave("figures/02_monthly_seasonality_by_year.png", p2, width = 11, height = 5.8, dpi = 320)

p3_data <- daily_data %>%
  group_by(month, month_name) %>%
  summarise(
    mean_share = mean(renewable_share_excl_sc, na.rm = TRUE),
    p25 = quantile(renewable_share_excl_sc, 0.25, na.rm = TRUE),
    p75 = quantile(renewable_share_excl_sc, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p3 <- ggplot(p3_data, aes(x = month, y = mean_share)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.22) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Typical seasonal profile of renewable share",
    subtitle = "Line shows monthly mean; shaded band shows interquartile range across 2021–2025",
    x = NULL,
    y = "Renewable share"
  ) +
  theme_project()

ggsave("figures/03_monthly_seasonal_profile.png", p3, width = 10, height = 5.6, dpi = 320)

p4 <- ggplot(daily_data, aes(x = weekday, y = renewable_share_excl_sc)) +
  geom_boxplot(width = 0.65, outlier.alpha = 0.25, linewidth = 0.35) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Weekday distribution of renewable share",
    subtitle = "Weekend days show a different renewable-share distribution from working days",
    x = NULL,
    y = "Renewable share"
  ) +
  theme_project() +
  theme(legend.position = "none")

ggsave("figures/04_weekday_distribution.png", p4, width = 9, height = 5.6, dpi = 320)

p5 <- ggplot(daily_data, aes(x = factor(year), y = renewable_share_excl_sc)) +
  geom_boxplot(width = 0.65, outlier.alpha = 0.22, linewidth = 0.35) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Yearly distribution of daily renewable share",
    subtitle = "The distribution shows shifts in level and volatility across years",
    x = NULL,
    y = "Renewable share"
  ) +
  theme_project() +
  theme(legend.position = "none")

ggsave("figures/05_yearly_distribution.png", p5, width = 9, height = 5.6, dpi = 320)

monthly_mix_share <- daily_data %>%
  mutate(month_date = as.Date(sprintf("%d-%02d-01", year, month))) %>%
  group_by(month_date) %>%
  summarise(
    thermal = sum(thermal, na.rm = TRUE),
    hydro = sum(hydro, na.rm = TRUE),
    geothermal = sum(geothermal, na.rm = TRUE),
    photovoltaic = sum(photovoltaic, na.rm = TRUE),
    wind = sum(wind, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(total_generation_gwh = thermal + hydro + geothermal + photovoltaic + wind) %>%
  pivot_longer(
    cols = c(thermal, hydro, geothermal, photovoltaic, wind),
    names_to = "source",
    values_to = "generation_gwh"
  ) %>%
  mutate(
    generation_share = 100 * generation_gwh / total_generation_gwh,
    source = factor(source, levels = names(source_labels), labels = source_labels)
  )

p6 <- ggplot(monthly_mix_share, aes(x = month_date, y = generation_share, fill = source)) +
  geom_area(linewidth = 0.1, alpha = 0.95) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%"), expand = c(0, 0)) +
  labs(
    title = "Monthly electricity generation mix",
    subtitle = "Share of total generation by primary source",
    x = NULL,
    y = "Generation share",
    fill = "Source"
  ) +
  theme_project()

ggsave("figures/06_monthly_generation_mix_share.png", p6, width = 11, height = 5.8, dpi = 320)

renewable_composition <- daily_data %>%
  mutate(month_date = as.Date(sprintf("%d-%02d-01", year, month))) %>%
  group_by(month_date) %>%
  summarise(
    hydro = sum(hydro, na.rm = TRUE),
    geothermal = sum(geothermal, na.rm = TRUE),
    photovoltaic = sum(photovoltaic, na.rm = TRUE),
    wind = sum(wind, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(renewable_total_gwh = hydro + geothermal + photovoltaic + wind) %>%
  pivot_longer(
    cols = c(hydro, geothermal, photovoltaic, wind),
    names_to = "source",
    values_to = "generation_gwh"
  ) %>%
  mutate(
    renewable_source_share = 100 * generation_gwh / renewable_total_gwh,
    source = factor(source, levels = names(renewable_labels), labels = renewable_labels)
  )

p7 <- ggplot(renewable_composition, aes(x = month_date, y = renewable_source_share, fill = source)) +
  geom_area(linewidth = 0.1, alpha = 0.95) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%"), expand = c(0, 0)) +
  labs(
    title = "Composition of renewable generation",
    subtitle = "Monthly source shares within renewable generation only",
    x = NULL,
    y = "Renewable generation share",
    fill = "Renewable source"
  ) +
  theme_project()

ggsave("figures/07_renewable_source_composition.png", p7, width = 11, height = 5.8, dpi = 320)

acf_data <- forecast::Acf(
  daily_data$renewable_share_excl_sc,
  lag.max = 42,
  plot = FALSE
)

acf_tbl <- tibble(
  lag = as.numeric(acf_data$lag),
  acf = as.numeric(acf_data$acf)
) %>%
  filter(lag > 0)

ci <- 1.96 / sqrt(nrow(daily_data))

p8 <- ggplot(acf_tbl, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, linewidth = 0.35) +
  geom_hline(yintercept = c(-ci, ci), linetype = "dashed", linewidth = 0.35) +
  geom_col(width = 0.55) +
  scale_x_continuous(breaks = seq(0, 42, by = 7)) +
  labs(
    title = "Autocorrelation of daily renewable share",
    subtitle = "Strong weekly dependence supports lag-based and seasonal forecasting models",
    x = "Lag in days",
    y = "ACF"
  ) +
  theme_project() +
  theme(legend.position = "none")

ggsave("figures/08_acf_renewable_share.png", p8, width = 10, height = 5.6, dpi = 320)

p9_data <- p1_data %>%
  mutate(split = if_else(date < as.Date("2025-01-01"), "Training period", "Test period"))

p9 <- ggplot(p9_data, aes(x = date, y = renewable_share_excl_sc)) +
  annotate(
    "rect",
    xmin = as.Date("2021-01-01"),
    xmax = as.Date("2024-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.08
  ) +
  annotate(
    "rect",
    xmin = as.Date("2025-01-01"),
    xmax = as.Date("2025-12-31"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.16
  ) +
  geom_line(linewidth = 0.25, alpha = 0.4) +
  geom_line(
    data = p9_data %>% filter(!is.na(renewable_share_30d)),
    aes(y = renewable_share_30d),
    linewidth = 0.85
  ) +
  geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = as.Date("2022-12-31"), y = 77, label = "Training: 2021–2024", size = 4) +
  annotate("text", x = as.Date("2025-07-01"), y = 77, label = "Test: 2025", size = 4) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Forecast evaluation design",
    subtitle = "Models are trained on 2021–2024 and evaluated on the held-out 2025 period",
    x = NULL,
    y = "Renewable share"
  ) +
  theme_project()

ggsave("figures/09_train_test_split.png", p9, width = 11, height = 5.8, dpi = 320)

print(list.files("figures"))