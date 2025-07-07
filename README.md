# US Wind Energy Production Analysis

Time series analysis of US wind electricity generation using ARIMA models to understand trends, seasonality, and forecasting performance.

## Overview

This project analyzes monthly US wind energy production data from the Energy Information Administration (EIA). The analysis applies econometric time series techniques to model wind electricity generation patterns and provide forecasts.

## Repository Structure

```
us-wind-energy-analysis/
├── README.md                              # Project documentation
├── code/
│   ├── wind_energy_analysis.R             # Main analysis script
│   ├── TSA-Predict-Student-Functions.R    # Time series analysis functions
│   ├── TSA-Useful-Functions.R             # Additional utility functions
│   └── CalendarEffects-Student-Functions.R # Calendar effects functions
├── data/
│   └── US-Wind-MWh_1000.txt              # Wind energy production data
└── report/
    └── analysis_report.pdf                # Detailed report (if available)
```

## Data Information

- **Source:** EIA (US Energy Information Administration) - Electricity Data Browser
- **Variable:** Monthly electricity generation from wind
- **Unit:** GWh (MWh × 1,000)
- **Frequency:** Monthly
- **Context:** The US is the world's 2nd largest wind electricity producer (21% of global production in 2021)

## How to Run

1. **Install required R packages:**
```r
install.packages(c("forecast", "lmtest", "tsoutliers", "FinTS", "urca"))
```

2. **Set up the analysis:**
   - Clone this repository
   - Ensure all files in `code/` are in the same directory
   - Set working directory to `code/` folder
   - Place data file in `../data/` relative to the code directory

3. **Run the analysis:**
```r
source("wind_energy_analysis.R")
```

## Analysis Overview

### 1. Data Exploration
- **Time series decomposition** (multiplicative)
- **Trend and seasonality analysis**
- **Visual inspection** of the data patterns

### 2. Stationarity Testing
- **Augmented Dickey-Fuller (ADF) tests** for unit roots
- **KPSS tests** for stationarity confirmation
- **Differencing operations** (d=1, D=1) for achieving stationarity

### 3. Model Identification and Estimation
- **ARIMA specification testing** with various orders
- **Best model:** ARIMA(0,1,1) × (0,1,1)[12] on log-transformed data
- **Parameter estimation** using maximum likelihood

### 4. Model Diagnostics
- **Residual analysis** (ACF, PACF plots)
- **Normality testing** (Shapiro-Wilk test)
- **Homoskedasticity testing** (ARCH tests)
- **Outlier detection** procedures

### 5. Forecasting
- **Ex-ante forecasting** with confidence bands
- **Ex-post validation** using rolling forecasts
- **Naive benchmark** comparison
- **Error measure calculation** (RMSE, MAE, etc.)

## Key Findings

| Aspect | Result |
|--------|--------|
| **Best Model** | ARIMA(0,1,1) × (0,1,1)[12] with log transformation |
| **AIC** | -343.43 |
| **Seasonality** | Strong seasonal pattern (12-month cycle) |
| **Trend** | Significant upward trend in wind energy production |
| **Residuals** | Well-behaved, approximately normal |
| **Forecasting** | Good performance vs. naive benchmarks |

## Model Specifications

### Final ARIMA Model
```
log(Yt) = ARIMA(0,1,1) × (0,1,1)[12]
```

Where:
- `Yt` = Wind energy production at time t
- `(0,1,1)` = Non-seasonal component: MA(1) with one difference
- `(0,1,1)[12]` = Seasonal component: seasonal MA(1) with seasonal difference

### Model Equation
```
(1-B)(1-B^12) log(Yt) = (1 + θ₁B)(1 + Θ₁B^12) εt
```

## Statistical Tests Results

- **Unit Root Tests:** Confirmed I(1) behavior requiring first differencing
- **Seasonal Unit Root:** Confirmed seasonal I(1) requiring seasonal differencing  
- **ARCH Tests:** No evidence of heteroskedasticity in residuals
- **Normality:** Residuals approximately follow normal distribution

## Technical Implementation

- **Software:** R
- **Key Packages:** forecast, lmtest, urca, tsoutliers
- **Estimation:** Maximum likelihood estimation
- **Validation:** Cross-validation with rolling forecasts
- **Transformation:** Log transformation for variance stabilization

## Usage Notes

- Data file must be placed in `../data/` directory relative to code
- All helper function files must be in the same directory as main script
- Analysis generates multiple diagnostic plots automatically
- Forecast results include both point estimates and prediction intervals
