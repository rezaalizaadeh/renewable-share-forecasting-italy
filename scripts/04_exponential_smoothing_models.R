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
      strip.text = element_text(face = "bold", size = 10.5, colour = "#111827"),
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
  mutate(date = as.Date(date))

train_data <- model_data %>%
  filter(date < as.Date("2025-01-01"))

test_data <- model_data %>%
  filter(date >= as.Date("2025-01-01"), date <= as.Date("2025-12-31"))

cat("ETS train observations:", nrow(train_data), "\n")
cat("ETS test observations:", nrow(test_data), "\n")

safe_aicc <- function(model) {
  if (!is.null(model$aicc)) {
    return(as.numeric(model$aicc))
  }
  as.numeric(AIC(model))
}

model_summary_text <- function(model) {
  paste(capture.output(model), collapse = " ")
}

accuracy_summary <- function(forecast_data) {
  forecast_data %>%
    group_by(model) %>%
    summarise(
      RMSE = sqrt(mean(error^2, na.rm = TRUE)),
      MAE = mean(abs_error, na.rm = TRUE),
      MAPE = mean(pct_error, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(RMSE) %>%
    mutate(rank = row_number())
}

ljung_box_stat <- function(x, lag_value = 14) {
  x <- x[!is.na(x)]
  lag_value <- min(lag_value, length(x) - 1)
  as.numeric(Box.test(x, lag = lag_value, type = "Ljung-Box")$statistic)
}

ljung_box_p <- function(x, lag_value = 14) {
  x <- x[!is.na(x)]
  lag_value <- min(lag_value, length(x) - 1)
  as.numeric(Box.test(x, lag = lag_value, type = "Ljung-Box")$p.value)
}

forecast_to_tbl <- function(fc, eval_df, actual, model_name) {
  tibble(
    date = eval_df$date,
    actual = actual,
    model = model_name,
    forecast = as.numeric(fc$mean),
    lower_80 = as.numeric(fc$lower[, 1]),
    upper_80 = as.numeric(fc$upper[, 1]),
    lower_95 = as.numeric(fc$lower[, 2]),
    upper_95 = as.numeric(fc$upper[, 2])
  ) %>%
    mutate(
      error = actual - forecast,
      abs_error = abs(error),
      pct_error = 100 * abs_error / actual
    )
}

train_y <- train_data[[target_col]]
test_y <- test_data[[target_col]]
h_test <- nrow(test_data)

train_weekly_ts <- ts(
  train_y,
  frequency = 7
)

fit_ses <- forecast::ses(
  train_weekly_ts,
  h = h_test,
  level = c(80, 95)
)

fit_holt <- forecast::holt(
  train_weekly_ts,
  h = h_test,
  damped = FALSE,
  level = c(80, 95)
)

fit_damped_holt <- forecast::holt(
  train_weekly_ts,
  h = h_test,
  damped = TRUE,
  level = c(80, 95)
)

fit_weekly_ets_model <- forecast::ets(
  train_weekly_ts,
  model = "ZZZ"
)

fit_weekly_ets <- forecast::forecast(
  fit_weekly_ets_model,
  h = h_test,
  level = c(80, 95)
)

ets_forecasts <- bind_rows(
  forecast_to_tbl(
    fc = fit_ses,
    eval_df = test_data,
    actual = test_y,
    model_name = "Simple exponential smoothing"
  ),
  forecast_to_tbl(
    fc = fit_holt,
    eval_df = test_data,
    actual = test_y,
    model_name = "Holt linear trend"
  ),
  forecast_to_tbl(
    fc = fit_damped_holt,
    eval_df = test_data,
    actual = test_y,
    model_name = "Damped Holt trend"
  ),
  forecast_to_tbl(
    fc = fit_weekly_ets,
    eval_df = test_data,
    actual = test_y,
    model_name = "Weekly ETS"
  )
)

ets_accuracy <- accuracy_summary(ets_forecasts)

print(ets_accuracy)

ets_residual_diagnostics <- ets_forecasts %>%
  group_by(model) %>%
  summarise(
    mean_error = mean(error, na.rm = TRUE),
    sd_error = sd(error, na.rm = TRUE),
    ljung_box_statistic = ljung_box_stat(error),
    ljung_box_p_value = ljung_box_p(error),
    .groups = "drop"
  ) %>%
  arrange(ljung_box_p_value)

print(ets_residual_diagnostics)

ets_interval_accuracy <- ets_forecasts %>%
  group_by(model) %>%
  summarise(
    coverage_80 = mean(actual >= lower_80 & actual <= upper_80, na.rm = TRUE) * 100,
    coverage_95 = mean(actual >= lower_95 & actual <= upper_95, na.rm = TRUE) * 100,
    avg_width_80 = mean(upper_80 - lower_80, na.rm = TRUE),
    avg_width_95 = mean(upper_95 - lower_95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(coverage_95), avg_width_95)

print(ets_interval_accuracy)

ets_bounds_check <- ets_forecasts %>%
  group_by(model) %>%
  summarise(
    forecasts_below_0 = sum(forecast < 0, na.rm = TRUE),
    forecasts_above_100 = sum(forecast > 100, na.rm = TRUE),
    lower_95_below_0 = sum(lower_95 < 0, na.rm = TRUE),
    upper_95_above_100 = sum(upper_95 > 100, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(forecasts_below_0 + forecasts_above_100))

print(ets_bounds_check)

ets_model_specifications <- tibble(
  model = c(
    "Simple exponential smoothing",
    "Holt linear trend",
    "Damped Holt trend",
    "Weekly ETS"
  ),
  AICc = c(
    safe_aicc(fit_ses$model),
    safe_aicc(fit_holt$model),
    safe_aicc(fit_damped_holt$model),
    safe_aicc(fit_weekly_ets_model)
  ),
  specification = c(
    model_summary_text(fit_ses$model),
    model_summary_text(fit_holt$model),
    model_summary_text(fit_damped_holt$model),
    model_summary_text(fit_weekly_ets_model)
  )
)

print(ets_model_specifications)

readr::write_csv(
  ets_forecasts,
  "data/processed/ets_forecasts.csv"
)

readr::write_csv(
  ets_accuracy,
  "data/processed/ets_model_accuracy.csv"
)

readr::write_csv(
  ets_residual_diagnostics,
  "data/processed/ets_residual_diagnostics.csv"
)

readr::write_csv(
  ets_interval_accuracy,
  "data/processed/ets_interval_accuracy.csv"
)

readr::write_csv(
  ets_bounds_check,
  "data/processed/ets_bounds_check.csv"
)

readr::write_csv(
  ets_model_specifications,
  "data/processed/ets_model_specifications.csv"
)

forecast_plot_data <- ets_forecasts %>%
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
    model = factor(model, levels = ets_accuracy$model)
  )

p31 <- ggplot(forecast_plot_data, aes(x = date, y = renewable_share)) +
  geom_line(
    aes(colour = series, linewidth = series, alpha = series, linetype = series)
  ) +
  facet_wrap(~ model, ncol = 1) +
  scale_colour_manual(
    values = c("Actual" = "#111827", "Forecast" = "#EA580C")
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
    title = "Exponential smoothing forecasts for renewable electricity share",
    subtitle = "Models are trained on 2021-2024 and evaluated on the held-out 2025 test set",
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
  "figures/31_ets_forecast_comparison.png",
  p31,
  width = 11,
  height = 8.2,
  dpi = 320
)

accuracy_long <- ets_accuracy %>%
  pivot_longer(
    cols = c(RMSE, MAE, MAPE),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    model = factor(model, levels = rev(ets_accuracy$model)),
    metric = factor(metric, levels = c("RMSE", "MAE", "MAPE")),
    is_best = if_else(model == ets_accuracy$model[1], "Best ETS model", "Other ETS model"),
    value_label = case_when(
      metric %in% c("RMSE", "MAE") ~ paste0(round(value, 2), " pp"),
      metric == "MAPE" ~ paste0(round(value, 1), "%"),
      TRUE ~ as.character(round(value, 2))
    )
  )

p32 <- ggplot(accuracy_long, aes(x = value, y = model, fill = is_best)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = value_label),
    hjust = -0.08,
    size = 3.4,
    colour = "#111827"
  ) +
  facet_wrap(~ metric, scales = "free_x") +
  scale_fill_manual(
    values = c("Best ETS model" = "#EA580C", "Other ETS model" = "#9CA3AF")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.34))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Exponential smoothing model accuracy on the 2025 test set",
    subtitle = "Lower values indicate better full test-year forecasting performance",
    x = "Error value",
    y = NULL,
    fill = NULL
  ) +
  theme_project() +
  theme(
    plot.margin = margin(10, 50, 10, 10)
  )

ggsave(
  "figures/32_ets_accuracy_ranking.png",
  p32,
  width = 11,
  height = 5.8,
  dpi = 320
)

best_ets_model <- ets_accuracy$model[1]

best_ets_residuals <- ets_forecasts %>%
  filter(model == best_ets_model) %>%
  mutate(
    residual_direction = if_else(error >= 0, "Actual above forecast", "Actual below forecast")
  )

p33 <- ggplot(best_ets_residuals, aes(x = date, y = error, fill = residual_direction)) +
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
    title = paste("Test residuals for best exponential smoothing model:", best_ets_model),
    subtitle = "Residuals are actual minus forecast, measured in percentage points",
    x = NULL,
    y = "Forecast error",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/33_best_ets_residuals.png",
  p33,
  width = 11,
  height = 5.8,
  dpi = 320
)

best_ets_acf <- forecast::Acf(
  best_ets_residuals$error,
  lag.max = 42,
  plot = FALSE
)

best_ets_acf_tbl <- tibble(
  lag = as.numeric(best_ets_acf$lag),
  acf = as.numeric(best_ets_acf$acf)
) %>%
  filter(lag > 0)

ci <- 1.96 / sqrt(nrow(best_ets_residuals))

best_ets_acf_tbl <- best_ets_acf_tbl %>%
  mutate(
    significance = if_else(abs(acf) > ci, "Significant", "Not significant")
  )

p34 <- ggplot(best_ets_acf_tbl, aes(x = lag, y = acf, fill = significance)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "#111827") +
  geom_hline(
    yintercept = c(-ci, ci),
    linetype = "dashed",
    linewidth = 0.4,
    colour = "#6B7280"
  ) +
  geom_col(width = 0.58) +
  scale_fill_manual(
    values = c("Significant" = "#EA580C", "Not significant" = "#CBD5E1")
  ) +
  scale_x_continuous(breaks = seq(0, 42, by = 7)) +
  labs(
    title = paste("Residual autocorrelation for best exponential smoothing model:", best_ets_model),
    subtitle = "Residual ACF checks remaining dependence after exponential smoothing",
    x = "Lag in days",
    y = "ACF",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/34_best_ets_residual_acf.png",
  p34,
  width = 10,
  height = 5.8,
  dpi = 320
)

cat("Best exponential smoothing model on 2025 test set:", best_ets_model, "\n")
cat("Saved ETS forecasts, accuracy metrics, interval metrics, residual diagnostics, model specifications, and figures 31-34.\n")
print(list.files("figures", pattern = "^3[1-4]_"))