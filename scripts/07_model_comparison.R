required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "ggplot2",
  "forecast",
  "scales",
  "grid"
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
library(forecast)
library(scales)
library(grid)

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

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Required file not found: ", path)
  }
  
  readr::read_csv(path, show_col_types = FALSE)
}

accuracy_summary <- function(forecast_data) {
  forecast_data %>%
    group_by(model_family, forecasting_setup, model) %>%
    summarise(
      RMSE = sqrt(mean(error^2, na.rm = TRUE)),
      MAE = mean(abs_error, na.rm = TRUE),
      MAPE = mean(pct_error, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(RMSE)
}

make_acf_tbl <- function(data, max_lag = 42) {
  data %>%
    group_by(model_family, forecasting_setup, model) %>%
    group_modify(
      ~ {
        acf_obj <- forecast::Acf(
          .x$error,
          lag.max = max_lag,
          plot = FALSE
        )
        
        tibble(
          lag = as.numeric(acf_obj$lag),
          acf = as.numeric(acf_obj$acf),
          n = sum(!is.na(.x$error))
        ) %>%
          filter(lag > 0) %>%
          mutate(
            ci = 1.96 / sqrt(n),
            significance = if_else(abs(acf) > ci, "Significant", "Not significant")
          )
      }
    ) %>%
    ungroup()
}

family_levels <- c(
  "Baseline",
  "Exponential smoothing",
  "ARIMA-family",
  "Regression/ML"
)

family_colours <- c(
  "Baseline" = "#6B7280",
  "Exponential smoothing" = "#EA580C",
  "ARIMA-family" = "#7C3AED",
  "Regression/ML" = "#059669"
)

baseline_accuracy <- safe_read_csv(
  "data/processed/baseline_model_accuracy.csv"
) %>%
  mutate(
    model_family = "Baseline",
    forecasting_setup = "Full test-year benchmark"
  )

ets_accuracy <- safe_read_csv(
  "data/processed/ets_model_accuracy.csv"
) %>%
  mutate(
    model_family = "Exponential smoothing",
    forecasting_setup = "Full test-year forecast"
  )

arima_accuracy <- safe_read_csv(
  "data/processed/arima_model_accuracy.csv"
) %>%
  mutate(
    model_family = "ARIMA-family",
    forecasting_setup = "Full test-year forecast"
  )

ml_accuracy <- safe_read_csv(
  "data/processed/ml_model_accuracy.csv"
) %>%
  mutate(
    model_family = "Regression/ML",
    forecasting_setup = "One-step-ahead operational forecast"
  )

final_model_comparison <- bind_rows(
  baseline_accuracy,
  ets_accuracy,
  arima_accuracy,
  ml_accuracy
) %>%
  select(
    model_family,
    forecasting_setup,
    model,
    RMSE,
    MAE,
    MAPE
  ) %>%
  group_by(model_family) %>%
  arrange(RMSE, .by_group = TRUE) %>%
  mutate(rank_within_family = row_number()) %>%
  ungroup() %>%
  arrange(RMSE)

print(final_model_comparison)

best_models_by_family <- final_model_comparison %>%
  group_by(model_family) %>%
  slice_min(order_by = RMSE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    model_family = factor(
      model_family,
      levels = family_levels
    )
  ) %>%
  arrange(model_family)

print(best_models_by_family)

best_baseline_rmse <- best_models_by_family %>%
  filter(model_family == "Baseline") %>%
  pull(RMSE)

best_ets_rmse <- best_models_by_family %>%
  filter(model_family == "Exponential smoothing") %>%
  pull(RMSE)

best_arima_rmse <- best_models_by_family %>%
  filter(model_family == "ARIMA-family") %>%
  pull(RMSE)

best_ml_rmse <- best_models_by_family %>%
  filter(model_family == "Regression/ML") %>%
  pull(RMSE)

improvement_summary <- best_models_by_family %>%
  mutate(
    reference_model = "Best baseline model",
    baseline_RMSE = best_baseline_rmse,
    RMSE_reduction_vs_baseline = baseline_RMSE - RMSE,
    percent_RMSE_reduction_vs_baseline = 100 * RMSE_reduction_vs_baseline / baseline_RMSE,
    comparison_note = case_when(
      model_family == "Baseline" ~ "Reference benchmark",
      model_family == "Exponential smoothing" ~ "Classical full-horizon smoothing benchmark",
      model_family == "ARIMA-family" ~ "Full-horizon improvement over baseline",
      model_family == "Regression/ML" ~ "Operational one-step-ahead improvement over baseline",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    model_family,
    forecasting_setup,
    model,
    RMSE,
    baseline_RMSE,
    RMSE_reduction_vs_baseline,
    percent_RMSE_reduction_vs_baseline,
    comparison_note
  )

ets_vs_baseline <- tibble(
  comparison = "Best Exponential smoothing vs best baseline",
  setup_note = "Full-horizon classical forecasting comparison",
  reference_model = best_models_by_family %>%
    filter(model_family == "Baseline") %>%
    pull(model),
  challenger_model = best_models_by_family %>%
    filter(model_family == "Exponential smoothing") %>%
    pull(model),
  reference_RMSE = best_baseline_rmse,
  challenger_RMSE = best_ets_rmse,
  RMSE_reduction = best_baseline_rmse - best_ets_rmse,
  percent_RMSE_reduction = 100 * (best_baseline_rmse - best_ets_rmse) / best_baseline_rmse
)

arima_vs_baseline <- tibble(
  comparison = "Best ARIMA-family vs best baseline",
  setup_note = "Full-horizon statistical comparison",
  reference_model = best_models_by_family %>%
    filter(model_family == "Baseline") %>%
    pull(model),
  challenger_model = best_models_by_family %>%
    filter(model_family == "ARIMA-family") %>%
    pull(model),
  reference_RMSE = best_baseline_rmse,
  challenger_RMSE = best_arima_rmse,
  RMSE_reduction = best_baseline_rmse - best_arima_rmse,
  percent_RMSE_reduction = 100 * (best_baseline_rmse - best_arima_rmse) / best_baseline_rmse
)

ml_vs_baseline <- tibble(
  comparison = "Best Regression/ML vs best baseline",
  setup_note = "Operational one-step-ahead comparison",
  reference_model = best_models_by_family %>%
    filter(model_family == "Baseline") %>%
    pull(model),
  challenger_model = best_models_by_family %>%
    filter(model_family == "Regression/ML") %>%
    pull(model),
  reference_RMSE = best_baseline_rmse,
  challenger_RMSE = best_ml_rmse,
  RMSE_reduction = best_baseline_rmse - best_ml_rmse,
  percent_RMSE_reduction = 100 * (best_baseline_rmse - best_ml_rmse) / best_baseline_rmse
)

ml_vs_arima <- tibble(
  comparison = "Best Regression/ML vs best ARIMA-family",
  setup_note = "Different information setting: one-step-ahead ML vs full-horizon ARIMA",
  reference_model = best_models_by_family %>%
    filter(model_family == "ARIMA-family") %>%
    pull(model),
  challenger_model = best_models_by_family %>%
    filter(model_family == "Regression/ML") %>%
    pull(model),
  reference_RMSE = best_arima_rmse,
  challenger_RMSE = best_ml_rmse,
  RMSE_reduction = best_arima_rmse - best_ml_rmse,
  percent_RMSE_reduction = 100 * (best_arima_rmse - best_ml_rmse) / best_arima_rmse
)

pairwise_improvement_summary <- bind_rows(
  ets_vs_baseline,
  arima_vs_baseline,
  ml_vs_baseline,
  ml_vs_arima
)

print(improvement_summary)
print(pairwise_improvement_summary)

baseline_forecasts <- safe_read_csv(
  "data/processed/baseline_forecasts.csv"
) %>%
  mutate(
    date = as.Date(date),
    model_family = "Baseline",
    forecasting_setup = "Full test-year benchmark"
  )

ets_forecasts <- safe_read_csv(
  "data/processed/ets_forecasts.csv"
) %>%
  mutate(
    date = as.Date(date),
    model_family = "Exponential smoothing",
    forecasting_setup = "Full test-year forecast"
  )

arima_forecasts <- safe_read_csv(
  "data/processed/arima_forecasts.csv"
) %>%
  mutate(
    date = as.Date(date),
    model_family = "ARIMA-family",
    forecasting_setup = "Full test-year forecast"
  )

ml_forecasts <- safe_read_csv(
  "data/processed/ml_forecasts.csv"
) %>%
  mutate(
    date = as.Date(date),
    model_family = "Regression/ML",
    forecasting_setup = "One-step-ahead operational forecast"
  )

all_forecasts <- bind_rows(
  baseline_forecasts,
  ets_forecasts,
  arima_forecasts,
  ml_forecasts
) %>%
  select(
    date,
    model_family,
    forecasting_setup,
    model,
    actual,
    forecast,
    error,
    abs_error,
    pct_error
  )

best_model_keys <- best_models_by_family %>%
  mutate(model_family = as.character(model_family)) %>%
  select(model_family, forecasting_setup, model)

final_best_model_forecasts <- all_forecasts %>%
  inner_join(
    best_model_keys,
    by = c("model_family", "forecasting_setup", "model")
  ) %>%
  mutate(
    model_family = factor(
      model_family,
      levels = family_levels
    ),
    model_label = paste0(model_family, ": ", model)
  ) %>%
  arrange(model_family, date)

final_best_model_accuracy_check <- accuracy_summary(final_best_model_forecasts)

print(final_best_model_accuracy_check)

final_residual_acf_comparison <- make_acf_tbl(
  final_best_model_forecasts,
  max_lag = 42
)

arima_ml_residual_acf <- final_residual_acf_comparison %>%
  filter(model_family %in% c("ARIMA-family", "Regression/ML"))

final_diagnostic_summary <- bind_rows(
  safe_read_csv("data/processed/ets_residual_diagnostics.csv") %>%
    mutate(model_family = "Exponential smoothing"),
  safe_read_csv("data/processed/arima_residual_diagnostics.csv") %>%
    mutate(model_family = "ARIMA-family"),
  safe_read_csv("data/processed/ml_residual_diagnostics.csv") %>%
    mutate(model_family = "Regression/ML")
) %>%
  inner_join(
    best_model_keys %>% select(model_family, model),
    by = c("model_family", "model")
  ) %>%
  select(
    model_family,
    model,
    mean_error,
    sd_error,
    ljung_box_statistic,
    ljung_box_p_value
  )

print(final_diagnostic_summary)

if (file.exists("data/processed/ml_variable_importance.csv")) {
  final_variable_importance <- safe_read_csv(
    "data/processed/ml_variable_importance.csv"
  ) %>%
    slice_head(n = 15)
} else {
  final_variable_importance <- tibble()
}

readr::write_csv(
  final_model_comparison,
  "data/processed/final_model_comparison.csv"
)

readr::write_csv(
  best_models_by_family,
  "data/processed/final_best_models_by_family.csv"
)

readr::write_csv(
  improvement_summary,
  "data/processed/final_improvement_summary.csv"
)

readr::write_csv(
  pairwise_improvement_summary,
  "data/processed/final_pairwise_improvement_summary.csv"
)

readr::write_csv(
  final_best_model_forecasts,
  "data/processed/final_best_model_forecasts.csv"
)

readr::write_csv(
  final_residual_acf_comparison,
  "data/processed/final_residual_acf_comparison.csv"
)

readr::write_csv(
  final_diagnostic_summary,
  "data/processed/final_diagnostic_summary.csv"
)

if (nrow(final_variable_importance) > 0) {
  readr::write_csv(
    final_variable_importance,
    "data/processed/final_top_variable_importance.csv"
  )
}

best_accuracy_long <- best_models_by_family %>%
  mutate(
    model_label = paste0(model_family, "\n", model),
    model_label = factor(model_label, levels = paste0(model_family, "\n", model))
  ) %>%
  pivot_longer(
    cols = c(RMSE, MAE, MAPE),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, levels = c("RMSE", "MAE", "MAPE")),
    value_label = case_when(
      metric %in% c("RMSE", "MAE") ~ paste0(round(value, 2), " pp"),
      metric == "MAPE" ~ paste0(round(value, 1), "%"),
      TRUE ~ as.character(round(value, 2))
    )
  )

p26 <- ggplot(
  best_accuracy_long,
  aes(x = value, y = model_label, fill = model_family)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = value_label),
    hjust = -0.08,
    size = 3.4,
    colour = "#111827"
  ) +
  facet_wrap(~ metric, scales = "free_x") +
  scale_fill_manual(values = family_colours) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.36))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Final accuracy comparison by model family",
    subtitle = "Best model from each family evaluated on the 2025 test period",
    x = "Error value",
    y = NULL,
    fill = "Model family",
    caption = "Elastic Net is evaluated in a one-step-ahead operational setup; baseline, ETS, and ARIMA-family models are full test-year forecasts."
  ) +
  theme_project() +
  theme(
    plot.margin = margin(10, 80, 10, 10),
    panel.spacing.x = unit(1.4, "lines")
  )

ggsave(
  "figures/26_final_best_family_accuracy.png",
  p26,
  width = 13.8,
  height = 7.2,
  dpi = 320
)

forecast_plot_data <- final_best_model_forecasts %>%
  select(date, model_family, forecasting_setup, model_label, actual, forecast) %>%
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
    model_label = factor(model_label, levels = unique(final_best_model_forecasts$model_label))
  )

p27 <- ggplot(forecast_plot_data, aes(x = date, y = renewable_share)) +
  geom_line(
    aes(colour = series, linewidth = series, alpha = series, linetype = series)
  ) +
  facet_wrap(~ model_label, ncol = 1) +
  scale_colour_manual(
    values = c("Actual" = "#111827", "Forecast" = "#059669")
  ) +
  scale_linewidth_manual(
    values = c("Actual" = 0.5, "Forecast" = 0.7)
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
    title = "Forecast comparison for the best model in each family",
    subtitle = "Elastic Net is one-step-ahead; Baseline, ETS, and ARIMA-family models are full test-year forecasts",
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
  "figures/27_final_best_forecast_comparison.png",
  p27,
  width = 11.5,
  height = 10.2,
  dpi = 320
)

error_distribution_data <- final_best_model_forecasts %>%
  mutate(
    model_label = factor(model_label, levels = unique(model_label))
  )

p28 <- ggplot(
  error_distribution_data,
  aes(x = model_label, y = abs_error, fill = model_family)
) +
  geom_boxplot(width = 0.55, alpha = 0.85, outlier.alpha = 0.45) +
  scale_fill_manual(values = family_colours) +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(
    title = "Absolute error distribution for the best models",
    subtitle = "Lower and tighter boxes indicate more consistent forecast performance",
    x = NULL,
    y = "Absolute error",
    fill = "Model family"
  ) +
  theme_project() +
  theme(
    axis.text.x = element_text(angle = 18, hjust = 1)
  )

ggsave(
  "figures/28_final_absolute_error_distribution.png",
  p28,
  width = 11.2,
  height = 6.5,
  dpi = 320
)

acf_plot_data <- arima_ml_residual_acf %>%
  mutate(
    model_label = paste0(model_family, ": ", model),
    model_label = factor(model_label, levels = unique(model_label))
  )

p29 <- ggplot(acf_plot_data, aes(x = lag, y = acf, fill = significance)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "#111827") +
  geom_hline(
    aes(yintercept = ci),
    linetype = "dashed",
    linewidth = 0.35,
    colour = "#6B7280"
  ) +
  geom_hline(
    aes(yintercept = -ci),
    linetype = "dashed",
    linewidth = 0.35,
    colour = "#6B7280"
  ) +
  geom_col(width = 0.58) +
  facet_wrap(~ model_label, ncol = 1) +
  scale_fill_manual(
    values = c(
      "Significant" = "#059669",
      "Not significant" = "#CBD5E1"
    )
  ) +
  scale_x_continuous(breaks = seq(0, 42, by = 7)) +
  labs(
    title = "Residual autocorrelation comparison",
    subtitle = "Lag-based Elastic Net reduces residual dependence relative to the best ARIMA-family model",
    x = "Lag in days",
    y = "ACF",
    fill = NULL
  ) +
  theme_project()

ggsave(
  "figures/29_final_residual_acf_comparison.png",
  p29,
  width = 10.5,
  height = 7.5,
  dpi = 320
)

improvement_plot_data <- improvement_summary %>%
  filter(model_family %in% c("ARIMA-family", "Regression/ML")) %>%
  mutate(
    model_label = paste0(model_family, "\n", model),
    model_label = factor(model_label, levels = model_label),
    improvement_label = paste0(
      round(percent_RMSE_reduction_vs_baseline, 1),
      "% lower RMSE"
    )
  )

p30 <- ggplot(
  improvement_plot_data,
  aes(
    x = percent_RMSE_reduction_vs_baseline,
    y = model_label,
    fill = model_family
  )
) +
  geom_col(width = 0.55) +
  geom_text(
    aes(label = improvement_label),
    hjust = -0.08,
    size = 3.8,
    colour = "#111827"
  ) +
  scale_fill_manual(
    values = c(
      "ARIMA-family" = "#7C3AED",
      "Regression/ML" = "#059669"
    )
  ) +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.25))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "RMSE improvement over the best baseline model",
    subtitle = "ARIMA improves the full-horizon forecast; Elastic Net gives the largest operational one-step-ahead improvement",
    x = "RMSE reduction vs best baseline",
    y = NULL,
    fill = "Model family",
    caption = "ML is one-step-ahead; ETS is omitted because it underperforms the best baseline."
  ) +
  theme_project() +
  theme(
    plot.margin = margin(10, 70, 10, 10)
  )

ggsave(
  "figures/30_final_improvement_over_baseline.png",
  p30,
  width = 10.5,
  height = 5.8,
  dpi = 320
)

cat("\nFinal best models by family:\n")
print(best_models_by_family)

cat("\nPairwise improvement summary:\n")
print(pairwise_improvement_summary)

cat("\nFinal diagnostic summary:\n")
print(final_diagnostic_summary)

cat("\nSaved final comparison tables and figures 26-30.\n")
print(list.files("figures", pattern = "^2[6-9]_|^30_"))