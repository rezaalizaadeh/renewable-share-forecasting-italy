required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2",
  "lubridate",
  "zoo",
  "glmnet",
  "mgcv",
  "ranger",
  "forecast",
  "scales"
)

missing_packages <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them with install.packages()."
  )
}

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(zoo)
library(glmnet)
library(mgcv)
library(ranger)
library(forecast)
library(scales)

set.seed(123)

daily_data <- readr::read_csv(
  "data/processed/renewable_daily_clean.csv",
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date))

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

make_forecast_tbl <- function(data, prediction, model_name, period_name) {
  tibble(
    date = data$date,
    actual = data[[target_col]],
    model = model_name,
    period = period_name,
    forecast = as.numeric(prediction)
  ) %>%
    mutate(
      error = actual - forecast,
      abs_error = abs(error),
      pct_error = 100 * abs_error / actual
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

model_data <- daily_data %>%
  arrange(date) %>%
  mutate(
    year = year(date),
    month = factor(month(date), levels = 1:12, labels = month.abb),
    weekday = factor(
      wday(date, label = TRUE, abbr = TRUE, week_start = 1),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    day_of_year = yday(date),
    sin_day = sin(2 * pi * day_of_year / 365.25),
    cos_day = cos(2 * pi * day_of_year / 365.25),
    y = .data[[target_col]],
    lag_1 = lag(y, 1),
    lag_2 = lag(y, 2),
    lag_7 = lag(y, 7),
    lag_14 = lag(y, 14),
    lag_30 = lag(y, 30),
    rolling_7_mean = zoo::rollapply(lag_1, width = 7, FUN = mean, fill = NA, align = "right", na.rm = TRUE),
    rolling_14_mean = zoo::rollapply(lag_1, width = 14, FUN = mean, fill = NA, align = "right", na.rm = TRUE),
    rolling_30_mean = zoo::rollapply(lag_1, width = 30, FUN = mean, fill = NA, align = "right", na.rm = TRUE),
    rolling_7_sd = zoo::rollapply(lag_1, width = 7, FUN = sd, fill = NA, align = "right", na.rm = TRUE),
    rolling_30_sd = zoo::rollapply(lag_1, width = 30, FUN = sd, fill = NA, align = "right", na.rm = TRUE)
  ) %>%
  filter(!is.na(lag_30), !is.na(rolling_30_mean), !is.na(rolling_30_sd))

feature_cols <- c(
  "trend",
  "month",
  "weekday",
  "day_of_year",
  "sin_day",
  "cos_day",
  "lag_1",
  "lag_2",
  "lag_7",
  "lag_14",
  "lag_30",
  "rolling_7_mean",
  "rolling_14_mean",
  "rolling_30_mean",
  "rolling_7_sd",
  "rolling_30_sd"
)

train_validation_data <- model_data %>%
  filter(date >= as.Date("2021-01-01"), date <= as.Date("2023-12-31"))

validation_data <- model_data %>%
  filter(date >= as.Date("2024-01-01"), date <= as.Date("2024-12-31"))

final_train_data <- model_data %>%
  filter(date >= as.Date("2021-01-01"), date <= as.Date("2024-12-31"))

test_data <- model_data %>%
  filter(date >= as.Date("2025-01-01"), date <= as.Date("2025-12-31"))

cat("Tuning train observations:", nrow(train_validation_data), "\n")
cat("Validation observations:", nrow(validation_data), "\n")
cat("Final train observations:", nrow(final_train_data), "\n")
cat("Test observations:", nrow(test_data), "\n")

cat(
  "Forecasting setup: one-step-ahead prediction using lagged values available up to the previous day.\n"
)

model_formula <- as.formula(
  paste(target_col, "~", paste(feature_cols, collapse = " + "))
)

make_glmnet_matrix <- function(data) {
  model.matrix(model_formula, data = data)[, -1, drop = FALSE]
}

x_train_validation <- make_glmnet_matrix(train_validation_data)
y_train_validation <- train_validation_data[[target_col]]

x_validation <- make_glmnet_matrix(validation_data)
y_validation <- validation_data[[target_col]]

x_final_train <- make_glmnet_matrix(final_train_data)
y_final_train <- final_train_data[[target_col]]

x_test <- make_glmnet_matrix(test_data)
y_test <- test_data[[target_col]]

linear_validation_fit <- lm(model_formula, data = train_validation_data)
linear_validation_pred <- predict(linear_validation_fit, newdata = validation_data)

linear_final_fit <- lm(model_formula, data = final_train_data)
linear_test_pred <- predict(linear_final_fit, newdata = test_data)

linear_validation_forecast <- make_forecast_tbl(
  validation_data,
  linear_validation_pred,
  "Lagged linear regression",
  "Validation"
)

linear_test_forecast <- make_forecast_tbl(
  test_data,
  linear_test_pred,
  "Lagged linear regression",
  "Test"
)

regularization_grid <- expand.grid(
  alpha = c(0, 0.25, 0.5, 0.75, 1),
  stringsAsFactors = FALSE
)

regularization_tuning_results <- bind_rows(
  lapply(
    regularization_grid$alpha,
    function(alpha_value) {
      fit <- glmnet::glmnet(
        x = x_train_validation,
        y = y_train_validation,
        alpha = alpha_value,
        standardize = TRUE
      )
      
      validation_predictions <- predict(
        fit,
        newx = x_validation,
        s = fit$lambda
      )
      
      lambda_results <- tibble(
        alpha = alpha_value,
        lambda = fit$lambda,
        validation_RMSE = apply(
          validation_predictions,
          2,
          function(pred) sqrt(mean((y_validation - pred)^2, na.rm = TRUE))
        ),
        validation_MAE = apply(
          validation_predictions,
          2,
          function(pred) mean(abs(y_validation - pred), na.rm = TRUE)
        ),
        validation_MAPE = apply(
          validation_predictions,
          2,
          function(pred) mean(100 * abs(y_validation - pred) / y_validation, na.rm = TRUE)
        )
      )
      
      lambda_results
    }
  )
) %>%
  mutate(
    model = case_when(
      alpha == 0 ~ "Ridge regression",
      alpha == 1 ~ "Lasso regression",
      TRUE ~ "Elastic Net"
    )
  )

best_ridge <- regularization_tuning_results %>%
  filter(alpha == 0) %>%
  arrange(validation_RMSE) %>%
  slice(1)

best_lasso <- regularization_tuning_results %>%
  filter(alpha == 1) %>%
  arrange(validation_RMSE) %>%
  slice(1)

best_elastic_net <- regularization_tuning_results %>%
  filter(alpha %in% c(0.25, 0.5, 0.75)) %>%
  arrange(validation_RMSE) %>%
  slice(1)

selected_regularized_models <- bind_rows(
  best_ridge,
  best_lasso,
  best_elastic_net
)

print(selected_regularized_models)

fit_glmnet_selected <- function(alpha_value, lambda_value, train_x, train_y, eval_x) {
  fit <- glmnet::glmnet(
    x = train_x,
    y = train_y,
    alpha = alpha_value,
    lambda = lambda_value,
    standardize = TRUE
  )
  
  list(
    fit = fit,
    prediction = as.numeric(predict(fit, newx = eval_x, s = lambda_value))
  )
}

ridge_validation <- fit_glmnet_selected(
  best_ridge$alpha,
  best_ridge$lambda,
  x_train_validation,
  y_train_validation,
  x_validation
)

lasso_validation <- fit_glmnet_selected(
  best_lasso$alpha,
  best_lasso$lambda,
  x_train_validation,
  y_train_validation,
  x_validation
)

elastic_net_validation <- fit_glmnet_selected(
  best_elastic_net$alpha,
  best_elastic_net$lambda,
  x_train_validation,
  y_train_validation,
  x_validation
)

ridge_test <- fit_glmnet_selected(
  best_ridge$alpha,
  best_ridge$lambda,
  x_final_train,
  y_final_train,
  x_test
)

lasso_test <- fit_glmnet_selected(
  best_lasso$alpha,
  best_lasso$lambda,
  x_final_train,
  y_final_train,
  x_test
)

elastic_net_test <- fit_glmnet_selected(
  best_elastic_net$alpha,
  best_elastic_net$lambda,
  x_final_train,
  y_final_train,
  x_test
)

regularized_validation_forecasts <- bind_rows(
  make_forecast_tbl(validation_data, ridge_validation$prediction, "Ridge regression", "Validation"),
  make_forecast_tbl(validation_data, lasso_validation$prediction, "Lasso regression", "Validation"),
  make_forecast_tbl(validation_data, elastic_net_validation$prediction, "Elastic Net", "Validation")
)

regularized_test_forecasts <- bind_rows(
  make_forecast_tbl(test_data, ridge_test$prediction, "Ridge regression", "Test"),
  make_forecast_tbl(test_data, lasso_test$prediction, "Lasso regression", "Test"),
  make_forecast_tbl(test_data, elastic_net_test$prediction, "Elastic Net", "Test")
)

gam_formula <- as.formula(
  paste(
    target_col,
    "~",
    paste(
      c(
        "s(trend, k = 20)",
        "s(day_of_year, bs = 'cc', k = 20)",
        "weekday",
        "s(lag_1, k = 10)",
        "s(lag_7, k = 10)",
        "s(lag_14, k = 10)",
        "s(lag_30, k = 10)",
        "s(rolling_7_mean, k = 10)",
        "s(rolling_30_mean, k = 10)",
        "s(rolling_7_sd, k = 8)"
      ),
      collapse = " + "
    )
  )
)

gam_validation_fit <- mgcv::gam(
  gam_formula,
  data = train_validation_data,
  method = "REML",
  knots = list(day_of_year = c(0.5, 366.5))
)

gam_validation_pred <- predict(gam_validation_fit, newdata = validation_data)

gam_final_fit <- mgcv::gam(
  gam_formula,
  data = final_train_data,
  method = "REML",
  knots = list(day_of_year = c(0.5, 366.5))
)

gam_test_pred <- predict(gam_final_fit, newdata = test_data)

gam_validation_forecast <- make_forecast_tbl(
  validation_data,
  gam_validation_pred,
  "GAM",
  "Validation"
)

gam_test_forecast <- make_forecast_tbl(
  test_data,
  gam_test_pred,
  "GAM",
  "Test"
)

rf_formula <- model_formula

rf_mtry_grid <- unique(
  pmax(
    2,
    pmin(
      length(feature_cols),
      c(3, 5, 8, floor(sqrt(length(feature_cols))), floor(length(feature_cols) / 2))
    )
  )
)

rf_grid <- expand.grid(
  mtry = rf_mtry_grid,
  min_node_size = c(5, 10, 20),
  stringsAsFactors = FALSE
)

rf_tuning_results <- bind_rows(
  lapply(
    seq_len(nrow(rf_grid)),
    function(i) {
      fit <- ranger::ranger(
        formula = rf_formula,
        data = train_validation_data %>% select(all_of(c(target_col, feature_cols))),
        num.trees = 500,
        mtry = rf_grid$mtry[i],
        min.node.size = rf_grid$min_node_size[i],
        importance = "permutation",
        seed = 123
      )
      
      pred <- predict(
        fit,
        data = validation_data %>% select(all_of(feature_cols))
      )$predictions
      
      tibble(
        model = "Random Forest",
        mtry = rf_grid$mtry[i],
        min_node_size = rf_grid$min_node_size[i],
        num_trees = 500,
        validation_RMSE = sqrt(mean((y_validation - pred)^2, na.rm = TRUE)),
        validation_MAE = mean(abs(y_validation - pred), na.rm = TRUE),
        validation_MAPE = mean(100 * abs(y_validation - pred) / y_validation, na.rm = TRUE)
      )
    }
  )
) %>%
  arrange(validation_RMSE)

best_rf <- rf_tuning_results %>%
  slice(1)

print(best_rf)

rf_validation_fit <- ranger::ranger(
  formula = rf_formula,
  data = train_validation_data %>% select(all_of(c(target_col, feature_cols))),
  num.trees = best_rf$num_trees,
  mtry = best_rf$mtry,
  min.node.size = best_rf$min_node_size,
  importance = "permutation",
  seed = 123
)

rf_validation_pred <- predict(
  rf_validation_fit,
  data = validation_data %>% select(all_of(feature_cols))
)$predictions

rf_final_fit <- ranger::ranger(
  formula = rf_formula,
  data = final_train_data %>% select(all_of(c(target_col, feature_cols))),
  num.trees = best_rf$num_trees,
  mtry = best_rf$mtry,
  min.node.size = best_rf$min_node_size,
  importance = "permutation",
  seed = 123
)

rf_test_pred <- predict(
  rf_final_fit,
  data = test_data %>% select(all_of(feature_cols))
)$predictions

rf_validation_forecast <- make_forecast_tbl(
  validation_data,
  rf_validation_pred,
  "Random Forest",
  "Validation"
)

rf_test_forecast <- make_forecast_tbl(
  test_data,
  rf_test_pred,
  "Random Forest",
  "Test"
)

validation_forecasts <- bind_rows(
  linear_validation_forecast,
  regularized_validation_forecasts,
  gam_validation_forecast,
  rf_validation_forecast
)

ml_forecasts <- bind_rows(
  linear_test_forecast,
  regularized_test_forecasts,
  gam_test_forecast,
  rf_test_forecast
)

accuracy_metrics <- accuracy_summary(ml_forecasts)

print(accuracy_metrics)

validation_accuracy <- accuracy_summary(validation_forecasts) %>%
  select(
    model,
    validation_RMSE = RMSE,
    validation_MAE = MAE,
    validation_MAPE = MAPE
  )

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

residual_diagnostics <- ml_forecasts %>%
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

regularization_tuning_summary <- selected_regularized_models %>%
  select(
    model,
    alpha,
    lambda,
    validation_RMSE,
    validation_MAE,
    validation_MAPE
  )

rf_tuning_summary <- rf_tuning_results %>%
  mutate(
    alpha = NA_real_,
    lambda = NA_real_
  ) %>%
  select(
    model,
    mtry,
    min_node_size,
    num_trees,
    validation_RMSE,
    validation_MAE,
    validation_MAPE
  )

ml_tuning_results <- bind_rows(
  regularization_tuning_summary %>%
    mutate(
      mtry = NA_real_,
      min_node_size = NA_real_,
      num_trees = NA_real_
    ) %>%
    select(
      model,
      alpha,
      lambda,
      mtry,
      min_node_size,
      num_trees,
      validation_RMSE,
      validation_MAE,
      validation_MAPE
    ),
  rf_tuning_summary %>%
    mutate(
      alpha = NA_real_,
      lambda = NA_real_
    ) %>%
    select(
      model,
      alpha,
      lambda,
      mtry,
      min_node_size,
      num_trees,
      validation_RMSE,
      validation_MAE,
      validation_MAPE
    )
)

print(ml_tuning_results)

rf_importance <- tibble(
  variable = names(rf_final_fit$variable.importance),
  importance = as.numeric(rf_final_fit$variable.importance),
  source = "Random Forest permutation importance"
) %>%
  arrange(desc(importance))

elastic_net_coefficients <- as.matrix(
  coef(elastic_net_test$fit, s = best_elastic_net$lambda)
)

elastic_net_importance <- tibble(
  variable = rownames(elastic_net_coefficients),
  coefficient = as.numeric(elastic_net_coefficients[, 1]),
  abs_coefficient = abs(coefficient),
  source = "Elastic Net coefficient"
) %>%
  filter(variable != "(Intercept)", abs_coefficient > 0) %>%
  arrange(desc(abs_coefficient))

readr::write_csv(
  ml_forecasts,
  "data/processed/ml_forecasts.csv"
)

readr::write_csv(
  validation_forecasts,
  "data/processed/ml_validation_forecasts.csv"
)

readr::write_csv(
  accuracy_metrics,
  "data/processed/ml_model_accuracy.csv"
)

readr::write_csv(
  generalization_check,
  "data/processed/ml_generalization_check.csv"
)

readr::write_csv(
  residual_diagnostics,
  "data/processed/ml_residual_diagnostics.csv"
)

readr::write_csv(
  regularization_tuning_results,
  "data/processed/ml_regularization_tuning_results.csv"
)

readr::write_csv(
  rf_tuning_results,
  "data/processed/ml_random_forest_tuning_results.csv"
)

readr::write_csv(
  ml_tuning_results,
  "data/processed/ml_tuning_results.csv"
)

readr::write_csv(
  rf_importance,
  "data/processed/ml_variable_importance.csv"
)

readr::write_csv(
  elastic_net_importance,
  "data/processed/ml_elastic_net_coefficients.csv"
)

p20_data <- regularization_tuning_results %>%
  mutate(
    alpha_label = paste0("alpha = ", alpha)
  )

p20 <- ggplot(
  p20_data,
  aes(x = log(lambda), y = validation_RMSE, colour = alpha_label)
) +
  geom_line(linewidth = 0.75) +
  geom_point(
    data = selected_regularized_models %>%
      mutate(alpha_label = paste0("alpha = ", alpha)),
    aes(x = log(lambda), y = validation_RMSE),
    size = 2.7
  ) +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(
    title = "Regularization tuning",
    subtitle = "Validation RMSE is used to select shrinkage strength before the 2025 test set",
    x = "log(lambda)",
    y = "Validation RMSE",
    colour = "Mixing parameter",
    caption = "Higher lambda increases regularization; alpha = 0 is Ridge, alpha = 1 is Lasso."
  ) +
  theme_project()

ggsave(
  "figures/20_regularization_tuning_validation.png",
  p20,
  width = 10.5,
  height = 5.8,
  dpi = 320
)

forecast_plot_data <- ml_forecasts %>%
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

p21 <- ggplot(forecast_plot_data, aes(x = date, y = renewable_share)) +
  geom_line(
    aes(colour = series, linewidth = series, alpha = series, linetype = series)
  ) +
  facet_wrap(~ model, ncol = 1) +
  scale_colour_manual(
    values = c("Actual" = "#111827", "Forecast" = "#059669")
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
    title = "Regression and machine-learning forecasts",
    subtitle = "One-step-ahead predictions for the 2025 test set using lagged and calendar features",
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
  "figures/21_ml_forecast_comparison.png",
  p21,
  width = 11,
  height = 10.5,
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
    is_best = if_else(model == accuracy_metrics$model[1], "Best ML model", "Other ML model"),
    value_label = case_when(
      metric %in% c("RMSE", "MAE") ~ paste0(round(value, 2), " pp"),
      metric == "MAPE" ~ paste0(round(value, 1), "%"),
      TRUE ~ as.character(round(value, 2))
    )
  )

p22 <- ggplot(accuracy_long, aes(x = value, y = model, fill = is_best)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = value_label),
    hjust = -0.08,
    size = 3.4,
    colour = "#111827"
  ) +
  facet_wrap(~ metric, scales = "free_x") +
  scale_fill_manual(
    values = c("Best ML model" = "#059669", "Other ML model" = "#9CA3AF")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.34))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Regression and ML model accuracy on the 2025 test set",
    subtitle = "Lower values indicate better one-step-ahead forecasting performance",
    x = "Error value",
    y = NULL,
    fill = NULL
  ) +
  theme_project() +
  theme(
    plot.margin = margin(10, 50, 10, 10)
  )

ggsave(
  "figures/22_ml_accuracy_ranking.png",
  p22,
  width = 11,
  height = 6.6,
  dpi = 320
)

top_importance <- rf_importance %>%
  slice_head(n = 15) %>%
  mutate(variable = factor(variable, levels = rev(variable)))

p23 <- ggplot(top_importance, aes(x = importance, y = variable)) +
  geom_col(fill = "#059669", width = 0.65) +
  labs(
    title = "Random Forest variable importance",
    subtitle = "Permutation importance shows which lagged and calendar features contribute most",
    x = "Permutation importance",
    y = NULL
  ) +
  theme_project() +
  theme(legend.position = "none")

ggsave(
  "figures/23_ml_variable_importance.png",
  p23,
  width = 10,
  height = 6.2,
  dpi = 320
)

best_model <- accuracy_metrics$model[1]

best_residuals <- ml_forecasts %>%
  filter(model == best_model) %>%
  mutate(
    residual_direction = if_else(error >= 0, "Actual above forecast", "Actual below forecast")
  )

p24 <- ggplot(best_residuals, aes(x = date, y = error, fill = residual_direction)) +
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
    title = paste("Test residuals for best ML model:", best_model),
    subtitle = "Residuals are actual minus forecast, measured in percentage points",
    x = NULL,
    y = "Forecast error",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/24_best_ml_residuals.png",
  p24,
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

p25 <- ggplot(best_acf_tbl, aes(x = lag, y = acf, fill = significance)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "#111827") +
  geom_hline(
    yintercept = c(-ci, ci),
    linetype = "dashed",
    linewidth = 0.4,
    colour = "#6B7280"
  ) +
  geom_col(width = 0.58) +
  scale_fill_manual(
    values = c("Significant" = "#059669", "Not significant" = "#CBD5E1")
  ) +
  scale_x_continuous(breaks = seq(0, 42, by = 7)) +
  labs(
    title = paste("Residual autocorrelation for best ML model:", best_model),
    subtitle = "Residual ACF checks whether lag-based ML reduced remaining serial dependence",
    x = "Lag in days",
    y = "ACF",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/25_best_ml_residual_acf.png",
  p25,
  width = 10,
  height = 5.8,
  dpi = 320
)

cat("Best ML model on 2025 test set:", best_model, "\n")
cat("Saved ML forecasts, tuning results, accuracy metrics, diagnostics, and variable importance.\n")
print(list.files("figures", pattern = "^2[0-5]_"))