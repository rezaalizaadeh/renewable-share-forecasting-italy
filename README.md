Short-Term Forecasting of Italy’s Renewable Electricity Share

This project forecasts Italy’s daily renewable electricity share using official generation-mix data from Terna. The dataset is built from actual generation files, converted to daily energy values, and used to compare simple baselines, ARIMA-family time-series models, and lag-based regression/ML models.

The goal is not only to find the lowest error, but to keep the forecasting setup clear. In particular, the project separates full test-year forecasts from one-step-ahead operational forecasts, because the information available to the models is different.

⸻

Research question

Can Italy’s daily renewable electricity share be forecast from historical generation-mix data, and which modelling approach performs best under a time-aware evaluation setup?

⸻

Data

The raw data come from Terna’s public Transparency Report / Download Center, using the Actual Generation dataset.

The raw Excel files are not stored in this repository. They are excluded through .gitignore, while the cleaned daily dataset and model outputs are stored under data/processed/.

The final cleaned dataset covers:

* Daily observations from 2021-01-01 to 2025-12-31
* 1,826 complete daily records
* Generation sources including thermal, hydro, geothermal, photovoltaic, wind, and self-consumption
* A held-out 2025 test period for final evaluation

⸻

Target variable

The target is the daily renewable electricity share, excluding self-consumption from the denominator:

renewable_share_excl_sc =
(Hydro + Geothermal + Photovoltaic + Wind)
/
(Thermal + Hydro + Geothermal + Photovoltaic + Wind)
* 100

The raw Terna values were converted to daily energy values before aggregation. The final target is a percentage, so it is interpreted in percentage points.

⸻

Methodology

The pipeline is organized in six scripts.

Script	Purpose
01_clean_data.R	Reads raw Terna Excel files, cleans source names and dates, converts generation to daily energy, and creates the final daily dataset
02_eda.R	Explores trend, seasonality, weekday effects, generation mix, renewable composition, and autocorrelation
03_baseline_models.R	Fits benchmark models: mean, naive, weekly seasonal naive, previous-year naive, and TSLM
04_arima_models.R	Fits ARIMA-family models, including SARIMA, Fourier ARIMA, dynamic regression with ARIMA errors, and logit-transformed bounded models
05_regression_ml_models.R	Fits lag-based regression and ML models: linear regression, Ridge, Lasso, Elastic Net, GAM, and Random Forest
06_model_comparison.R	Combines all model families, compares final results, calculates improvements, and creates the final summary figures

⸻

Forecasting setups

The project uses two forecasting setups.

Full test-year forecast

The baseline and ARIMA-family models are trained on 2021-2024 and evaluated on the full 2025 test year.

This setup is used for:

* Baseline models
* ARIMA
* Weekly SARIMA
* Fourier ARIMA
* Logit Fourier ARIMA
* Dynamic regression with ARIMA errors

One-step-ahead operational forecast

The regression/ML models predict each day in 2025 using lagged information available up to the previous day.

This setup is used for:

* Lagged linear regression
* Ridge regression
* Lasso regression
* Elastic Net
* GAM
* Random Forest

This distinction is important. The Elastic Net result should be interpreted as an operational day-ahead forecast, not as a direct full-horizon replacement for ARIMA.

⸻

Main results

Models are ranked primarily by RMSE. MAE and MAPE are reported as complementary error measures.

Model family	Best model	Forecasting setup	RMSE	MAE	MAPE
Baseline	TSLM	Full test-year benchmark	9.52	7.83	22.3
ARIMA-family	Logit Fourier ARIMA	Full test-year forecast	8.93	7.30	17.5
Regression/ML	Elastic Net	One-step-ahead operational forecast	5.15	4.07	10.1

The best full-horizon statistical model is Logit Fourier ARIMA. It improves on the best baseline by modelling weekly and annual seasonality through Fourier terms, while the logit transformation keeps the target within its natural 0-100% range.

The best operational one-step-ahead model is Elastic Net. Its performance is mainly driven by recent lagged renewable-share information, especially the previous day’s value and short-term rolling features.

⸻

Final comparison

The final comparison shows a clear progression:

Best baseline RMSE:       9.52
Best ARIMA-family RMSE:   8.93
Best operational ML RMSE: 5.15

The ARIMA-family stage improves the full-horizon statistical forecast by capturing seasonal structure. The lag-based regression/ML stage gives the strongest operational result because recent observations contain useful short-term information.

⸻

Residual diagnostics

Forecast accuracy alone is not enough. I also checked whether the residuals still contained serial dependence.

The best ARIMA-family model still leaves strong residual autocorrelation on the 2025 test period. The Elastic Net residuals are much closer to white noise:

Model	Ljung-Box p-value
Logit Fourier ARIMA	0.000
Elastic Net	0.109

This supports the main modelling result: lag-based features reduce the short-term dependence that remains after ARIMA-family modelling.

⸻

Improvement over baseline

Relative to the best baseline model:

* Logit Fourier ARIMA reduces RMSE by about 6.2%
* Elastic Net reduces RMSE by about 45.9% in the one-step-ahead operational setup

The second comparison uses more recent information, so it should be read as an operational forecasting result rather than a like-for-like full-horizon comparison.

⸻

Feature importance

The Random Forest importance analysis confirms that the short-term lag structure is the main signal.

The most important predictor is lag_1, followed by weekly and short-term rolling features. This is consistent with the strong performance of the lag-based regression models.

⸻

Repository structure

renewable-share-forecasting-italy/
├── data/
│   ├── raw/
│   │   └── .gitkeep
│   └── processed/
│       ├── renewable_daily_clean.csv
│       ├── baseline_model_accuracy.csv
│       ├── arima_model_accuracy.csv
│       ├── ml_model_accuracy.csv
│       └── final_model_comparison.csv
├── figures/
│   ├── 01_renewable_share_trend.png
│   ├── ...
│   └── 30_final_improvement_over_baseline.png
├── scripts/
│   ├── 01_clean_data.R
│   ├── 02_eda.R
│   ├── 03_baseline_models.R
│   ├── 04_arima_models.R
│   ├── 05_regression_ml_models.R
│   └── 06_model_comparison.R
├── report/
├── README.md
├── .gitignore
└── renewable-share-forecasting-italy.Rproj

⸻

Reproducibility

The scripts are designed to be run from the project root.

Install the required R packages:

install.packages(c(
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "janitor",
  "ggplot2",
  "lubridate",
  "forecast",
  "scales",
  "zoo",
  "glmnet",
  "mgcv",
  "ranger"
))

Then run the pipeline in order:

source("scripts/01_clean_data.R")
source("scripts/02_eda.R")
source("scripts/03_baseline_models.R")
source("scripts/04_arima_models.R")
source("scripts/05_regression_ml_models.R")
source("scripts/06_model_comparison.R")

The raw Terna Excel files must be placed locally under data/raw/ before running the cleaning script. They are intentionally not committed to the repository.

Expected raw file names:

data/raw/terna_actual_generation_2021.xlsx
data/raw/terna_actual_generation_2022.xlsx
data/raw/terna_actual_generation_2023.xlsx
data/raw/terna_actual_generation_2024.xlsx
data/raw/terna_actual_generation_2025.xlsx

⸻

Limitations

This project uses only historical generation-mix data. It does not include weather forecasts, electricity demand, market prices, holidays, or cross-border exchange variables.

The ML models are evaluated in a one-step-ahead setup, using lagged values available up to the previous day. This is useful for operational short-term forecasting, but it is not the same task as forecasting the entire 2025 test year in one step.

The target is renewable electricity share, not absolute renewable generation. This makes the forecast useful for studying generation-mix dynamics, but it does not directly forecast total renewable output.

⸻

References

* Terna, Actual Generation data, Transparency Report / Download Center
    https://developer.terna.it/docs/read/apis_catalog/generation/Actual_Generation
* Hyndman, R. J. and Athanasopoulos, G., Forecasting: Principles and Practice — Time series cross-validation
    https://otexts.com/fpp3/tscv.html
* Hyndman, R. J. and Athanasopoulos, G., Forecasting: Principles and Practice — Residual diagnostics
    https://otexts.com/fpp3/diagnostics.html
* Zou, H. and Hastie, T. (2005), Regularization and Variable Selection via the Elastic Net
    https://academic.oup.com/jrsssb/article/67/2/301/7109482
