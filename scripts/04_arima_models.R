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
  mutate(
    year = as.integer(year),
    month = factor(month, levels = 1:12, labels = month.abb),
    weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

train_data <- model_data %>%
  filter(date < as.Date("2025-01-01"))

test_data <- model_data %>%
  filter(date >= as.Date("2025-01-01"), date <= as.Date("2025-12-31"))

rolling_folds <- list(
  list(
    fold = "Train 2021, validate 2022",
    train_start = as.Date("2021-01-01"),
    train_end = as.Date("2021-12-31"),
    validation_start = as.Date("2022-01-01"),
    validation_end = as.Date("2022-12-31")
  ),
  list(
    fold = "Train 2021-2022, validate 2023",
    train_start = as.Date("2021-01-01"),
    train_end = as.Date("2022-12-31"),
    validation_start = as.Date("2023-01-01"),
    validation_end = as.Date("2023-12-31")
  ),
  list(
    fold = "Train 2021-2023, validate 2024",
    train_start = as.Date("2021-01-01"),
    train_end = as.Date("2023-12-31"),
    validation_start = as.Date("2024-01-01"),
    validation_end = as.Date("2024-12-31")
  )
)

cat("Final train observations:", nrow(train_data), "\n")
cat("Final test observations:", nrow(test_data), "\n")

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

logit_share <- function(x) {
  p <- x / 100
  p <- pmin(pmax(p, 0.001), 0.999)
  qlogis(p)
}

inv_logit_share <- function(x) {
  100 * plogis(x)
}

build_calendar_matrix <- function(data) {
  model.matrix(
    ~ trend + month + weekday,
    data = data
  )[, -1, drop = FALSE]
}

make_full_rank_xreg <- function(train_xreg, eval_xreg) {
  train_xreg <- as.matrix(train_xreg)
  eval_xreg <- as.matrix(eval_xreg)
  
  if (ncol(train_xreg) == 0) {
    return(
      list(
        train = train_xreg,
        eval = eval_xreg,
        dropped_columns = character(0)
      )
    )
  }
  
  qr_fit <- qr(train_xreg)
  rank_value <- qr_fit$rank
  
  if (rank_value == ncol(train_xreg)) {
    return(
      list(
        train = train_xreg,
        eval = eval_xreg,
        dropped_columns = character(0)
      )
    )
  }
  
  keep_index <- sort(qr_fit$pivot[seq_len(rank_value)])
  dropped_columns <- colnames(train_xreg)[-keep_index]
  
  list(
    train = train_xreg[, keep_index, drop = FALSE],
    eval = eval_xreg[, keep_index, drop = FALSE],
    dropped_columns = dropped_columns
  )
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

forecast_to_tbl <- function(fc, eval_df, actual, model_name, transform_back = function(x) x) {
  tibble(
    date = eval_df$date,
    actual = actual,
    model = model_name,
    forecast = transform_back(as.numeric(fc$mean)),
    lower_80 = transform_back(as.numeric(fc$lower[, 1])),
    upper_80 = transform_back(as.numeric(fc$upper[, 1])),
    lower_95 = transform_back(as.numeric(fc$lower[, 2])),
    upper_95 = transform_back(as.numeric(fc$upper[, 2]))
  ) %>%
    mutate(
      error = actual - forecast,
      abs_error = abs(error),
      pct_error = 100 * abs_error / actual
    )
}

make_fourier_terms <- function(y, h = NULL, fourier_k) {
  y_msts <- forecast::msts(
    y,
    seasonal.periods = c(7, 365.25)
  )
  
  if (is.null(h)) {
    terms <- forecast::fourier(y_msts, K = fourier_k)
  } else {
    terms <- forecast::fourier(y_msts, K = fourier_k, h = h)
  }
  
  colnames(terms) <- paste0("fourier_", seq_len(ncol(terms)))
  terms
}

evaluate_fourier_candidate <- function(fold_info, weekly_k, yearly_k) {
  fold_train <- model_data %>%
    filter(date >= fold_info$train_start, date <= fold_info$train_end)
  
  fold_validation <- model_data %>%
    filter(date >= fold_info$validation_start, date <= fold_info$validation_end)
  
  train_y <- fold_train[[target_col]]
  validation_y <- fold_validation[[target_col]]
  h_validation <- nrow(fold_validation)
  
  result <- tryCatch(
    {
      fourier_train <- make_fourier_terms(
        y = train_y,
        fourier_k = c(weekly_k, yearly_k)
      )
      
      fourier_validation <- make_fourier_terms(
        y = train_y,
        h = h_validation,
        fourier_k = c(weekly_k, yearly_k)
      )
      
      fit <- forecast::auto.arima(
        train_y,
        xreg = fourier_train,
        seasonal = FALSE,
        stepwise = TRUE,
        approximation = FALSE
      )
      
      fc <- forecast::forecast(
        fit,
        xreg = fourier_validation,
        h = h_validation,
        level = c(80, 95)
      )
      
      errors <- validation_y - as.numeric(fc$mean)
      
      tibble(
        fold = fold_info$fold,
        weekly_k = weekly_k,
        yearly_k = yearly_k,
        n_fourier_terms = 2 * weekly_k + 2 * yearly_k,
        validation_RMSE = sqrt(mean(errors^2, na.rm = TRUE)),
        validation_MAE = mean(abs(errors), na.rm = TRUE),
        validation_MAPE = mean(100 * abs(errors) / validation_y, na.rm = TRUE),
        AICc = safe_aicc(fit),
        specification = model_summary_text(fit),
        status = "ok"
      )
    },
    error = function(e) {
      tibble(
        fold = fold_info$fold,
        weekly_k = weekly_k,
        yearly_k = yearly_k,
        n_fourier_terms = 2 * weekly_k + 2 * yearly_k,
        validation_RMSE = NA_real_,
        validation_MAE = NA_real_,
        validation_MAPE = NA_real_,
        AICc = NA_real_,
        specification = NA_character_,
        status = paste("error:", e$message)
      )
    }
  )
  
  result
}

fourier_grid <- expand.grid(
  weekly_k = c(1, 2, 3),
  yearly_k = c(2, 4, 6, 8)
)

fourier_tuning_by_fold <- bind_rows(
  lapply(
    rolling_folds,
    function(fold_info) {
      bind_rows(
        lapply(
          seq_len(nrow(fourier_grid)),
          function(i) {
            evaluate_fourier_candidate(
              fold_info = fold_info,
              weekly_k = fourier_grid$weekly_k[i],
              yearly_k = fourier_grid$yearly_k[i]
            )
          }
        )
      )
    }
  )
)

fourier_tuning_results <- fourier_tuning_by_fold %>%
  filter(status == "ok") %>%
  group_by(weekly_k, yearly_k, n_fourier_terms) %>%
  summarise(
    avg_validation_RMSE = mean(validation_RMSE, na.rm = TRUE),
    sd_validation_RMSE = sd(validation_RMSE, na.rm = TRUE),
    avg_validation_MAE = mean(validation_MAE, na.rm = TRUE),
    avg_validation_MAPE = mean(validation_MAPE, na.rm = TRUE),
    avg_AICc = mean(AICc, na.rm = TRUE),
    n_successful_folds = n(),
    .groups = "drop"
  ) %>%
  arrange(avg_validation_RMSE, avg_AICc)

print(fourier_tuning_results)

best_fourier_row <- fourier_tuning_results %>%
  slice(1)

if (nrow(best_fourier_row) == 0) {
  stop("No valid Fourier ARIMA model was selected.")
}

best_fourier_k <- c(
  best_fourier_row$weekly_k,
  best_fourier_row$yearly_k
)

cat("Selected Fourier K:", best_fourier_k[1], best_fourier_k[2], "\n")

fit_arima_family <- function(train_df, eval_df, fourier_k) {
  train_y <- train_df[[target_col]]
  eval_y <- eval_df[[target_col]]
  h_eval <- nrow(eval_df)
  
  train_ts <- ts(train_y, frequency = 1)
  train_weekly_ts <- ts(train_y, frequency = 7)
  
  fourier_train <- make_fourier_terms(
    y = train_y,
    fourier_k = fourier_k
  )
  
  fourier_eval <- make_fourier_terms(
    y = train_y,
    h = h_eval,
    fourier_k = fourier_k
  )
  
  calendar_train <- build_calendar_matrix(train_df)
  calendar_eval <- build_calendar_matrix(eval_df)
  
  trend_train <- as.matrix(train_df %>% select(trend))
  trend_eval <- as.matrix(eval_df %>% select(trend))
  
  trend_fourier_train <- cbind(trend_train, fourier_train)
  trend_fourier_eval <- cbind(trend_eval, fourier_eval)
  
  train_logit_y <- logit_share(train_y)
  
  fit_arima <- forecast::auto.arima(
    train_ts,
    seasonal = FALSE,
    stepwise = TRUE,
    approximation = FALSE
  )
  
  fit_weekly_sarima <- forecast::auto.arima(
    train_weekly_ts,
    seasonal = TRUE,
    stepwise = TRUE,
    approximation = FALSE
  )
  
  fit_fourier <- forecast::auto.arima(
    train_y,
    xreg = fourier_train,
    seasonal = FALSE,
    stepwise = TRUE,
    approximation = FALSE
  )
  
  fit_calendar <- forecast::auto.arima(
    train_y,
    xreg = calendar_train,
    seasonal = FALSE,
    stepwise = TRUE,
    approximation = FALSE
  )
  
  fit_trend_fourier <- forecast::auto.arima(
    train_y,
    xreg = trend_fourier_train,
    seasonal = FALSE,
    stepwise = TRUE,
    approximation = FALSE
  )
  
  fit_logit_fourier <- forecast::auto.arima(
    train_logit_y,
    xreg = fourier_train,
    seasonal = FALSE,
    stepwise = TRUE,
    approximation = FALSE
  )
  
  fit_logit_trend_fourier <- forecast::auto.arima(
    train_logit_y,
    xreg = trend_fourier_train,
    seasonal = FALSE,
    stepwise = TRUE,
    approximation = FALSE
  )
  
  fc_arima <- forecast::forecast(
    fit_arima,
    h = h_eval,
    level = c(80, 95)
  )
  
  fc_weekly_sarima <- forecast::forecast(
    fit_weekly_sarima,
    h = h_eval,
    level = c(80, 95)
  )
  
  fc_fourier <- forecast::forecast(
    fit_fourier,
    xreg = fourier_eval,
    h = h_eval,
    level = c(80, 95)
  )
  
  fc_calendar <- forecast::forecast(
    fit_calendar,
    xreg = calendar_eval,
    h = h_eval,
    level = c(80, 95)
  )
  
  fc_trend_fourier <- forecast::forecast(
    fit_trend_fourier,
    xreg = trend_fourier_eval,
    h = h_eval,
    level = c(80, 95)
  )
  
  fc_logit_fourier <- forecast::forecast(
    fit_logit_fourier,
    xreg = fourier_eval,
    h = h_eval,
    level = c(80, 95)
  )
  
  fc_logit_trend_fourier <- forecast::forecast(
    fit_logit_trend_fourier,
    xreg = trend_fourier_eval,
    h = h_eval,
    level = c(80, 95)
  )
  
  forecasts <- bind_rows(
    forecast_to_tbl(
      fc = fc_arima,
      eval_df = eval_df,
      actual = eval_y,
      model_name = "ARIMA"
    ),
    forecast_to_tbl(
      fc = fc_weekly_sarima,
      eval_df = eval_df,
      actual = eval_y,
      model_name = "Weekly SARIMA"
    ),
    forecast_to_tbl(
      fc = fc_fourier,
      eval_df = eval_df,
      actual = eval_y,
      model_name = "Tuned Fourier ARIMA"
    ),
    forecast_to_tbl(
      fc = fc_calendar,
      eval_df = eval_df,
      actual = eval_y,
      model_name = "Calendar ARIMA errors"
    ),
    forecast_to_tbl(
      fc = fc_trend_fourier,
      eval_df = eval_df,
      actual = eval_y,
      model_name = "Trend Fourier ARIMA errors"
    ),
    forecast_to_tbl(
      fc = fc_logit_fourier,
      eval_df = eval_df,
      actual = eval_y,
      model_name = "Logit Fourier ARIMA",
      transform_back = inv_logit_share
    ),
    forecast_to_tbl(
      fc = fc_logit_trend_fourier,
      eval_df = eval_df,
      actual = eval_y,
      model_name = "Logit Trend Fourier ARIMA errors",
      transform_back = inv_logit_share
    )
  )
  
  specifications <- tibble(
    model = c(
      "ARIMA",
      "Weekly SARIMA",
      "Tuned Fourier ARIMA",
      "Calendar ARIMA errors",
      "Trend Fourier ARIMA errors",
      "Logit Fourier ARIMA",
      "Logit Trend Fourier ARIMA errors"
    ),
    AICc = c(
      safe_aicc(fit_arima),
      safe_aicc(fit_weekly_sarima),
      safe_aicc(fit_fourier),
      safe_aicc(fit_calendar),
      safe_aicc(fit_trend_fourier),
      safe_aicc(fit_logit_fourier),
      safe_aicc(fit_logit_trend_fourier)
    ),
    specification = c(
      model_summary_text(fit_arima),
      model_summary_text(fit_weekly_sarima),
      model_summary_text(fit_fourier),
      model_summary_text(fit_calendar),
      model_summary_text(fit_trend_fourier),
      model_summary_text(fit_logit_fourier),
      model_summary_text(fit_logit_trend_fourier)
    )
  )
  
  list(
    forecasts = forecasts,
    specifications = specifications
  )
}

validation_model_results <- bind_rows(
  lapply(
    rolling_folds,
    function(fold_info) {
      fold_train <- model_data %>%
        filter(date >= fold_info$train_start, date <= fold_info$train_end)
      
      fold_validation <- model_data %>%
        filter(date >= fold_info$validation_start, date <= fold_info$validation_end)
      
      fit_arima_family(
        train_df = fold_train,
        eval_df = fold_validation,
        fourier_k = best_fourier_k
      )$forecasts %>%
        mutate(fold = fold_info$fold)
    }
  )
)

validation_accuracy <- accuracy_summary(validation_model_results) %>%
  select(
    model,
    validation_RMSE = RMSE,
    validation_MAE = MAE,
    validation_MAPE = MAPE
  )

final_fit <- fit_arima_family(
  train_df = train_data,
  eval_df = test_data,
  fourier_k = best_fourier_k
)

arima_forecasts <- final_fit$forecasts

accuracy_metrics <- accuracy_summary(arima_forecasts)

print(accuracy_metrics)

generalization_check <- accuracy_metrics %>%
  select(model, test_RMSE = RMSE, test_MAE = MAE, test_MAPE = MAPE) %>%
  left_join(validation_accuracy, by = "model") %>%
  mutate(
    RMSE_gap_test_minus_validation = test_RMSE - validation_RMSE,
    generalization_flag = case_when(
      RMSE_gap_test_minus_validation > 3 ~ "Possible overfitting or distribution shift",
      RMSE_gap_test_minus_validation < -3 ~ "Test period easier than validation",
      TRUE ~ "Stable validation-test performance"
    )
  ) %>%
  arrange(test_RMSE)

print(generalization_check)

residual_diagnostics <- arima_forecasts %>%
  group_by(model) %>%
  summarise(
    mean_error = mean(error, na.rm = TRUE),
    sd_error = sd(error, na.rm = TRUE),
    ljung_box_statistic = ljung_box_stat(error),
    ljung_box_p_value = ljung_box_p(error),
    .groups = "drop"
  ) %>%
  arrange(ljung_box_p_value)

print(residual_diagnostics)

interval_accuracy <- arima_forecasts %>%
  group_by(model) %>%
  summarise(
    coverage_80 = mean(actual >= lower_80 & actual <= upper_80, na.rm = TRUE) * 100,
    coverage_95 = mean(actual >= lower_95 & actual <= upper_95, na.rm = TRUE) * 100,
    avg_width_80 = mean(upper_80 - lower_80, na.rm = TRUE),
    avg_width_95 = mean(upper_95 - lower_95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(coverage_95), avg_width_95)

print(interval_accuracy)

bounds_check <- arima_forecasts %>%
  group_by(model) %>%
  summarise(
    forecasts_below_0 = sum(forecast < 0, na.rm = TRUE),
    forecasts_above_100 = sum(forecast > 100, na.rm = TRUE),
    lower_95_below_0 = sum(lower_95 < 0, na.rm = TRUE),
    upper_95_above_100 = sum(upper_95 > 100, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(forecasts_below_0 + forecasts_above_100))

print(bounds_check)

stationarity_summary <- tibble(
  sample = "Final training set 2021-2024",
  nonseasonal_differences_recommended = forecast::ndiffs(train_data[[target_col]]),
  weekly_seasonal_differences_recommended = forecast::nsdiffs(
    ts(train_data[[target_col]], frequency = 7)
  ),
  selected_weekly_fourier_k = best_fourier_k[1],
  selected_yearly_fourier_k = best_fourier_k[2],
  selected_fourier_terms = 2 * best_fourier_k[1] + 2 * best_fourier_k[2]
)

print(stationarity_summary)

model_specifications <- final_fit$specifications %>%
  mutate(
    selected_weekly_fourier_k = best_fourier_k[1],
    selected_yearly_fourier_k = best_fourier_k[2]
  )

print(model_specifications)

readr::write_csv(
  fourier_tuning_by_fold,
  "data/processed/arima_fourier_tuning_by_fold.csv"
)

readr::write_csv(
  fourier_tuning_results,
  "data/processed/arima_fourier_tuning_results.csv"
)

readr::write_csv(
  validation_model_results,
  "data/processed/arima_validation_forecasts.csv"
)

readr::write_csv(
  arima_forecasts,
  "data/processed/arima_forecasts.csv"
)

readr::write_csv(
  accuracy_metrics,
  "data/processed/arima_model_accuracy.csv"
)

readr::write_csv(
  generalization_check,
  "data/processed/arima_generalization_check.csv"
)

readr::write_csv(
  residual_diagnostics,
  "data/processed/arima_residual_diagnostics.csv"
)

readr::write_csv(
  interval_accuracy,
  "data/processed/arima_interval_accuracy.csv"
)

readr::write_csv(
  bounds_check,
  "data/processed/arima_bounds_check.csv"
)

readr::write_csv(
  stationarity_summary,
  "data/processed/arima_stationarity_summary.csv"
)

readr::write_csv(
  model_specifications,
  "data/processed/arima_model_specifications.csv"
)

p14 <- ggplot(
  fourier_tuning_results,
  aes(
    x = yearly_k,
    y = avg_validation_RMSE,
    colour = factor(weekly_k),
    group = factor(weekly_k)
  )
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.4) +
  geom_point(
    data = best_fourier_row,
    aes(x = yearly_k, y = avg_validation_RMSE),
    inherit.aes = FALSE,
    size = 4.2,
    colour = "#7C3AED"
  ) +
  scale_x_continuous(breaks = sort(unique(fourier_tuning_results$yearly_k))) +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  scale_colour_manual(
    values = c("1" = "#93C5FD", "2" = "#2563EB", "3" = "#1E3A8A")
  ) +
  labs(
    title = "Fourier seasonal complexity tuning",
    subtitle = "Rolling-origin validation is used to select seasonal complexity before the 2025 test set",
    x = "Yearly Fourier K",
    y = "Average validation RMSE",
    colour = "Weekly Fourier K",
    caption = "Low K can underfit seasonality; high K can overfit seasonal noise."
  ) +
  theme_project()

ggsave(
  "figures/14_fourier_tuning_validation.png",
  p14,
  width = 10.5,
  height = 5.8,
  dpi = 320
)

forecast_plot_data <- arima_forecasts %>%
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

p15 <- ggplot(forecast_plot_data, aes(x = date, y = renewable_share)) +
  geom_line(
    aes(colour = series, linewidth = series, alpha = series, linetype = series)
  ) +
  facet_wrap(~ model, ncol = 1) +
  scale_colour_manual(
    values = c("Actual" = "#111827", "Forecast" = "#7C3AED")
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
    title = "ARIMA-family forecasts for renewable electricity share",
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
  "figures/15_arima_forecast_comparison.png",
  p15,
  width = 11,
  height = 12.5,
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
    is_best = if_else(model == accuracy_metrics$model[1], "Best ARIMA-family model", "Other ARIMA-family model"),
    value_label = case_when(
      metric %in% c("RMSE", "MAE") ~ paste0(round(value, 2), " pp"),
      metric == "MAPE" ~ paste0(round(value, 1), "%"),
      TRUE ~ as.character(round(value, 2))
    )
  )

p16 <- ggplot(accuracy_long, aes(x = value, y = model, fill = is_best)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = value_label),
    hjust = -0.08,
    size = 3.4,
    colour = "#111827"
  ) +
  facet_wrap(~ metric, scales = "free_x") +
  scale_fill_manual(
    values = c("Best ARIMA-family model" = "#7C3AED", "Other ARIMA-family model" = "#9CA3AF")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.34))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "ARIMA-family model accuracy on the 2025 test set",
    subtitle = "Lower values indicate better point forecast performance",
    x = "Error value",
    y = NULL,
    fill = NULL
  ) +
  theme_project() +
  theme(
    plot.margin = margin(10, 50, 10, 10)
  )

ggsave(
  "figures/16_arima_accuracy_ranking.png",
  p16,
  width = 11,
  height = 6.6,
  dpi = 320
)

best_model <- accuracy_metrics$model[1]

best_residuals <- arima_forecasts %>%
  filter(model == best_model) %>%
  mutate(
    residual_direction = if_else(error >= 0, "Actual above forecast", "Actual below forecast")
  )

p17 <- ggplot(best_residuals, aes(x = date, y = error, fill = residual_direction)) +
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
    title = paste("Test residuals for best ARIMA-family model:", best_model),
    subtitle = "Residuals are actual minus forecast; persistent blocks indicate remaining time structure",
    x = NULL,
    y = "Forecast error",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/17_best_arima_residuals.png",
  p17,
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

p18 <- ggplot(best_acf_tbl, aes(x = lag, y = acf, fill = significance)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "#111827") +
  geom_hline(
    yintercept = c(-ci, ci),
    linetype = "dashed",
    linewidth = 0.4,
    colour = "#6B7280"
  ) +
  geom_col(width = 0.58) +
  scale_fill_manual(
    values = c("Significant" = "#7C3AED", "Not significant" = "#CBD5E1")
  ) +
  scale_x_continuous(breaks = seq(0, 42, by = 7)) +
  labs(
    title = paste("Residual autocorrelation for best ARIMA-family model:", best_model),
    subtitle = "Significant residual autocorrelation indicates remaining dependence after modelling",
    x = "Lag in days",
    y = "ACF",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/18_best_arima_residual_acf.png",
  p18,
  width = 10,
  height = 5.8,
  dpi = 320
)

best_interval_data <- arima_forecasts %>%
  filter(model == best_model)

p19 <- ggplot(best_interval_data, aes(x = date)) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95),
    fill = "#C4B5FD",
    alpha = 0.35
  ) +
  geom_ribbon(
    aes(ymin = lower_80, ymax = upper_80),
    fill = "#7C3AED",
    alpha = 0.28
  ) +
  geom_line(aes(y = actual, colour = "Actual"), linewidth = 0.55) +
  geom_line(aes(y = forecast, colour = "Forecast"), linewidth = 0.7, linetype = "longdash") +
  scale_colour_manual(
    values = c("Actual" = "#111827", "Forecast" = "#7C3AED")
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(
    title = paste("Prediction intervals for best ARIMA-family model:", best_model),
    subtitle = "Shaded bands show 80% and 95% forecast intervals on the 2025 test period",
    x = NULL,
    y = "Renewable share",
    colour = NULL
  ) +
  theme_project()

ggsave(
  "figures/19_best_arima_prediction_interval.png",
  p19,
  width = 11,
  height = 5.8,
  dpi = 320
)

cat("Best ARIMA-family model on 2025 test set:", best_model, "\n")
cat("Selected Fourier K:", best_fourier_k[1], best_fourier_k[2], "\n")
cat("Saved ARIMA forecasts, tuning results, accuracy metrics, generalization checks, interval metrics, residual diagnostics, and model specifications.\n")
print(list.files("figures", pattern = "^1[4-9]_"))