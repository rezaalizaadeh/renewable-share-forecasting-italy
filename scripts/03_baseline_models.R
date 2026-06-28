library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(forecast)
library(scales)

daily_data <- readr::read_csv(
  "data/processed/renewable_daily_clean.csv",
  show_col_types = FALSE
)

dir.create("figures", showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE)

theme_project <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 17, colour = "#111827"),
      plot.subtitle = element_text(size = 11.5, colour = "#4B5563"),
      plot.caption = element_text(size = 9, colour = "#6B7280", hjust = 0),
      axis.title = element_text(size = 11, colour = "#374151"),
      axis.text = element_text(size = 10, colour = "#374151"),
      strip.text = element_text(face = "bold", size = 11, colour = "#111827"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = "#E5E7EB", linewidth = 0.35),
      panel.grid.major.y = element_line(colour = "#E5E7EB", linewidth = 0.35),
      legend.title = element_text(size = 10, colour = "#374151"),
      legend.text = element_text(size = 10, colour = "#374151"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

target_col <- "renewable_share_excl_sc"

model_data <- daily_data %>%
  arrange(date) %>%
  mutate(
    year = as.integer(year),
    month = factor(month, levels = 1:12, labels = month.abb),
    weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

train_data <- model_data %>%
  filter(date < as.Date("2025-01-01"))

test_data <- model_data %>%
  filter(date >= as.Date("2025-01-01"), date <= as.Date("2025-12-31"))

h <- nrow(test_data)

cat("Training observations:", nrow(train_data), "\n")
cat("Test observations:", nrow(test_data), "\n")

train_ts <- ts(
  train_data[[target_col]],
  frequency = 7
)

mean_fc <- forecast::meanf(train_ts, h = h)
naive_fc <- forecast::naive(train_ts, h = h)
weekly_snaive_fc <- forecast::snaive(train_ts, h = h)

previous_year_forecast <- test_data %>%
  mutate(previous_year_date = date - years(1)) %>%
  left_join(
    model_data %>%
      select(previous_year_date = date, previous_year_value = renewable_share_excl_sc),
    by = "previous_year_date"
  ) %>%
  pull(previous_year_value)

tslm_model <- lm(
  renewable_share_excl_sc ~ trend + month + weekday,
  data = train_data
)

tslm_fc <- predict(tslm_model, newdata = test_data)

baseline_forecasts <- bind_rows(
  tibble(
    date = test_data$date,
    actual = test_data[[target_col]],
    model = "Mean",
    forecast = as.numeric(mean_fc$mean)
  ),
  tibble(
    date = test_data$date,
    actual = test_data[[target_col]],
    model = "Naive",
    forecast = as.numeric(naive_fc$mean)
  ),
  tibble(
    date = test_data$date,
    actual = test_data[[target_col]],
    model = "Weekly seasonal naive",
    forecast = as.numeric(weekly_snaive_fc$mean)
  ),
  tibble(
    date = test_data$date,
    actual = test_data[[target_col]],
    model = "Previous-year naive",
    forecast = as.numeric(previous_year_forecast)
  ),
  tibble(
    date = test_data$date,
    actual = test_data[[target_col]],
    model = "TSLM",
    forecast = as.numeric(tslm_fc)
  )
) %>%
  mutate(
    error = actual - forecast,
    abs_error = abs(error),
    pct_error = 100 * abs_error / actual
  )

accuracy_metrics <- baseline_forecasts %>%
  group_by(model) %>%
  summarise(
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAE = mean(abs_error, na.rm = TRUE),
    MAPE = mean(pct_error, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(RMSE) %>%
  mutate(rank = row_number())

print(accuracy_metrics)

residual_diagnostics <- baseline_forecasts %>%
  group_by(model) %>%
  summarise(
    mean_error = mean(error, na.rm = TRUE),
    sd_error = sd(error, na.rm = TRUE),
    ljung_box_statistic = as.numeric(Box.test(error, lag = 14, type = "Ljung-Box")$statistic),
    ljung_box_p_value = as.numeric(Box.test(error, lag = 14, type = "Ljung-Box")$p.value),
    .groups = "drop"
  ) %>%
  arrange(ljung_box_p_value)

print(residual_diagnostics)

readr::write_csv(
  baseline_forecasts,
  "data/processed/baseline_forecasts.csv"
)

readr::write_csv(
  accuracy_metrics,
  "data/processed/baseline_model_accuracy.csv"
)

readr::write_csv(
  residual_diagnostics,
  "data/processed/baseline_residual_diagnostics.csv"
)

forecast_plot_data <- baseline_forecasts %>%
  select(date, model, actual, forecast) %>%
  pivot_longer(
    cols = c(actual, forecast),
    names_to = "series",
    values_to = "renewable_share"
  ) %>%
  mutate(
    series = recode(
      series,
      actual = "Actual",
      forecast = "Forecast"
    ),
    series = factor(series, levels = c("Actual", "Forecast")),
    model = factor(model, levels = accuracy_metrics$model)
  )

p10 <- ggplot(forecast_plot_data, aes(x = date, y = renewable_share)) +
  geom_line(
    aes(colour = series, linewidth = series, alpha = series, linetype = series)
  ) +
  facet_wrap(~ model, ncol = 1) +
  scale_colour_manual(
    values = c("Actual" = "#111827", "Forecast" = "#2563EB")
  ) +
  scale_linewidth_manual(
    values = c("Actual" = 0.5, "Forecast" = 0.65)
  ) +
  scale_alpha_manual(
    values = c("Actual" = 0.9, "Forecast" = 0.9)
  ) +
  scale_linetype_manual(
    values = c("Actual" = "solid", "Forecast" = "longdash")
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = "Baseline forecasts for renewable electricity share",
    subtitle = "Actual 2025 values compared with simple benchmark forecasts",
    x = NULL,
    y = "Renewable share",
    colour = NULL,
    linewidth = NULL,
    alpha = NULL,
    linetype = NULL
  ) +
  guides(
    linewidth = "none",
    alpha = "none"
  ) +
  theme_project()

ggsave(
  "figures/10_baseline_forecast_comparison.png",
  p10,
  width = 11,
  height = 10,
  dpi = 320
)

accuracy_long <- accuracy_metrics %>%
  pivot_longer(
    cols = c(RMSE, MAE, MAPE),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    model = factor(model, levels = rev(accuracy_metrics$model)),
    metric = factor(metric, levels = c("RMSE", "MAE", "MAPE")),
    is_best = if_else(model == accuracy_metrics$model[1], "Best baseline", "Other baseline"),
    value_label = case_when(
      metric %in% c("RMSE", "MAE") ~ paste0(round(value, 2), " pp"),
      metric == "MAPE" ~ paste0(round(value, 1), "%"),
      TRUE ~ as.character(round(value, 2))
    )
  )

p11 <- ggplot(accuracy_long, aes(x = value, y = model, fill = is_best)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = value_label),
    hjust = -0.08,
    size = 3.4,
    colour = "#111827"
  ) +
  facet_wrap(~ metric, scales = "free_x") +
  scale_fill_manual(
    values = c("Best baseline" = "#2563EB", "Other baseline" = "#9CA3AF")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Baseline model accuracy on the 2025 test set",
    subtitle = "TSLM is highlighted because it achieves the lowest RMSE among baseline models",
    x = "Error value",
    y = NULL,
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/11_baseline_accuracy_ranking.png",
  p11,
  width = 11,
  height = 6.4,
  dpi = 320
)

best_model <- accuracy_metrics$model[1]

best_residuals <- baseline_forecasts %>%
  filter(model == best_model) %>%
  mutate(
    residual_direction = if_else(error >= 0, "Actual above forecast", "Actual below forecast")
  )

p12 <- ggplot(best_residuals, aes(x = date, y = error, fill = residual_direction)) +
  geom_hline(yintercept = 0, linewidth = 0.45, colour = "#111827") +
  geom_col(width = 1.0, alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "Actual above forecast" = "#16A34A",
      "Actual below forecast" = "#DC2626"
    )
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(
    title = paste("Test residuals for best baseline model:", best_model),
    subtitle = "Residuals are actual minus forecast; persistent blocks indicate remaining time structure",
    x = NULL,
    y = "Forecast error",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/12_best_baseline_residuals.png",
  p12,
  width = 11,
  height = 5.8,
  dpi = 320
)

best_acf <- forecast::Acf(
  best_residuals$error,
  lag.max = 42,
  plot = FALSE
)

best_acf_tbl <- tibble(
  lag = as.numeric(best_acf$lag),
  acf = as.numeric(best_acf$acf)
) %>%
  filter(lag > 0)

ci <- 1.96 / sqrt(nrow(best_residuals))

best_acf_tbl <- best_acf_tbl %>%
  mutate(
    significance = if_else(abs(acf) > ci, "Significant", "Not significant")
  )

p13 <- ggplot(best_acf_tbl, aes(x = lag, y = acf, fill = significance)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "#111827") +
  geom_hline(
    yintercept = c(-ci, ci),
    linetype = "dashed",
    linewidth = 0.4,
    colour = "#6B7280"
  ) +
  geom_col(width = 0.58) +
  scale_fill_manual(
    values = c("Significant" = "#2563EB", "Not significant" = "#CBD5E1")
  ) +
  scale_x_continuous(breaks = seq(0, 42, by = 7)) +
  labs(
    title = paste("Residual autocorrelation for best baseline model:", best_model),
    subtitle = "Significant residual autocorrelation motivates ARIMA and dynamic regression models",
    x = "Lag in days",
    y = "ACF",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/13_best_baseline_residual_acf.png",
  p13,
  width = 10,
  height = 5.8,
  dpi = 320
)

cat("Best baseline model:", best_model, "\n")
cat("Saved baseline forecasts, accuracy metrics, and residual diagnostics.\n")
print(list.files("figures", pattern = "^1[0-3]_"))