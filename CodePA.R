# ----install packages--------------------------------------------------------------
install.packages("ggcorrplot")
install.packages("strucchange")
install.packages("fpp3")

# ----import libraries--------------------------------------------------------------
library(tidyverse)   
library(lubridate)
library(tsibble)
library(fpp3)        
library(forecast)   
library(strucchange) 
library(readxl)
library(ggcorrplot)
library(tseries)
library(fable)

# ----load data--------------------------------------------------------------
data <- read_excel("/Users/dariopatzi/Desktop/PA_Paper/energiestatistik_ch.xlsx")
head(data)

weather_data <- read_delim("/Users/dariopatzi/Desktop/PA_Paper/wetterbern.csv", delim = ";")
head(weather_data)

reservoir_data <- read_csv("/Users/dariopatzi/Desktop/PA_Paper/speicherseen.csv")
head(reservoir_data)

heating_data <- read_csv("/Users/dariopatzi/Desktop/PA_Paper/heizgradtage.csv")
head(heating_data)

# ----data processing--------------------------------------------------------------

# Energy Data
# convert to date format
data$Datum <- as.Date(data$Datum)
data <- data %>%
  select(Datum, Wasserkraftwerke, Landesverbrauch) %>%
  rename(
    hydro_production       = Wasserkraftwerke,
    total_consumption_ch   = Landesverbrauch
  ) %>% 
  mutate(Datum = yearmonth(Datum))

# Weather Data 
weather_data<- weather_data %>%
  mutate(reference_timestamp = dmy_hm(reference_timestamp)) %>%
  filter(year(reference_timestamp) >= 2000) %>%
  mutate(Datum = yearmonth(reference_timestamp)) %>%
  group_by(Datum) %>%
  summarise(
    air_temp_mean       = mean(tre200m0, na.rm = TRUE),
    solar_radiation     = mean(pva200m0, na.rm = TRUE),
    rel_humidity        = mean(ure200m0, na.rm = TRUE),
    surface_pressure    = mean(prestam0, na.rm = TRUE),
    wind_speed          = mean(fu3010m0, na.rm = TRUE),
    WaterBalance        = sum(rreetsm0, na.rm = TRUE),
    evaporation         = mean(erefaom0, na.rm = TRUE),
    rain                = sum(rre150m0, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(Datum, air_temp_mean, solar_radiation, rel_humidity, surface_pressure, wind_speed,WaterBalance, evaporation, rain)

# reservoir data 
reservoir_data <- reservoir_data %>%
  mutate(Monat = yearmonth(Datum)) %>%     
  arrange(Datum) %>%
  group_by(Monat) %>%
  slice_tail(n=1) %>%                              
  ungroup() %>%
  rename(
    reservoir_storage_gwh  = TotalCH_speicherinhalt_gwh,
    reservoir_capacity_gwh = TotalCH_max_speicherinhalt_gwh
  ) %>%
  mutate(Datum = as.Date(Monat)) %>%
  select(Datum, reservoir_storage_gwh, reservoir_capacity_gwh)

# heating data 
heating_data <- heating_data %>%
  slice(-1) %>% 
  mutate(Datum = yearmonth(paste(Jahr, Monat, sep = "-"))) %>%
  rename(HeatingDegreeDays = Heizgradtage) %>%  
  select(Datum, HeatingDegreeDays)

# merging
all_data <- list(data, weather_data, reservoir_data, heating_data)
final_data <- reduce(all_data, full_join, by = "Datum") %>%
  filter(yearmonth(Datum) >= yearmonth("2000-01") & 
           yearmonth(Datum) <= yearmonth("2024-12"))

# add log
final_data <- final_data %>%
  mutate(log_hydro = log(hydro_production))

# creating reservoir_fill_ratio with lags, WaterBalance with lags
final_data <- final_data %>%
  mutate(
    reservoir_fill_ratio = reservoir_storage_gwh / reservoir_capacity_gwh,
    reservoir_fill_ratio_lag1 = lag(reservoir_fill_ratio, n = 1),
    reservoir_fill_ratio_lag2 = lag(reservoir_fill_ratio, n = 2)
  ) %>%
  select(-reservoir_storage_gwh)

# ----EDA before Split--------------------------------------------------------------

# missing values
missing_values <- final_data %>%
  filter(if_any(everything(), is.na)) %>%
  select(Datum)
print(unique(missing_values))

# Evaporation average per month
seasonal_avg_evap <- final_data %>%
  mutate(Month = month(Datum)) %>%
  group_by(Month) %>%
  summarize(avg_evap = mean(evaporation, na.rm = TRUE)) %>%
  ungroup()

# Imputation
final_data <- final_data %>%
  mutate(Month = month(Datum)) %>%
  left_join(seasonal_avg_evap, by = "Month") %>%
  mutate(
    evaporation = if_else(is.na(evaporation), avg_evap, evaporation)
  ) %>%
  select(-Month, -avg_evap)

# plot production
ggplot(final_data, aes(x = Datum, y = hydro_production)) +
  geom_line() +
  labs(title = "Hydropower Production Over Time", y = "GWh", x = "Date")

# plot production vs. temp
ggplot(final_data, aes(x = Datum, y = total_consumption_ch)) +
  geom_line() +
  geom_smooth(method = "lm") +
  labs(title = "Total Consumption CH", x = "Date", y = "Hydro Production (GWh)")

# plot fill ratio
monthly_avg_fill_ratio <- final_data %>%
  mutate(Month = month(Datum, label = TRUE, abbr = TRUE)) %>%
  group_by(Month) %>%
  summarize(mean_ratio = mean(reservoir_fill_ratio, na.rm = TRUE)) %>%
  ungroup()

ggplot(monthly_avg_fill_ratio, aes(x = Month, y = mean_ratio, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Reservoir Fill Ratio by Month",
    x     = "Month",
    y     = "Average Fill Ratio"
  )

# plot rain
monthly_avg_rain<- final_data %>%
  mutate(Month = month(Datum, label = TRUE, abbr = TRUE)) %>%
  group_by(Month) %>%
  summarize(mean_rain = mean(rain, na.rm = TRUE)) %>%
  ungroup()

ggplot(monthly_avg_rain, aes(x = Month, y = mean_rain, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Rain by Month",
    x     = "Month",
    y     = "Average Rain"
  )

# Step 1: Create monthly averages for hydro production and WaterBalance
monthly_avg_hydro <- final_data %>%
  mutate(Month = month(Datum, label = TRUE, abbr = TRUE)) %>%
  group_by(Month) %>%
  summarise(Hydro = mean(hydro_production, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Combine all variables
monthly_combined <- monthly_avg_fill_ratio %>%
  rename(Fill_Ratio = mean_ratio) %>%
  left_join(monthly_avg_rain %>% rename(Rain = mean_rain), by = "Month") %>%
  left_join(monthly_avg_hydro, by = "Month") %>%
  mutate(
    Fill_Ratio_scaled = (Fill_Ratio - min(Fill_Ratio)) / (max(Fill_Ratio) - min(Fill_Ratio)),
    Rain_scaled        = (Rain - min(Rain)) / (max(Rain) - min(Rain)),
    Hydro_scaled       = (Hydro - min(Hydro)) / (max(Hydro) - min(Hydro)),
  ) %>%
  pivot_longer(
    cols = c(Fill_Ratio_scaled, Rain_scaled, Hydro_scaled),
    names_to = "Variable",
    values_to = "Value"
  )

# Step 3: Plot all four
ggplot(monthly_combined, aes(x = Month, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "Fill_Ratio_scaled" = "gray",
      "Rain_scaled"       = "lightgray",
      "Hydro_scaled"      = "black"
    ),
    labels = c(
      "Fill_Ratio_scaled" = "Reservoir Fill Ratio",
      "Rain_scaled"       = "Rainfall",
      "Hydro_scaled"      = "Hydropower Production"
    )
  ) +
  labs(
    title = "Seasonal Patterns: Hydro Production, Fill Ratio and Rainfall",
    x = "Month",
    y = "Normalized Value",
    color = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# plot total consumption
monthly_avg_consumption<- final_data %>%
  mutate(Month = month(Datum, label = TRUE, abbr = TRUE)) %>%
  group_by(Month) %>%
  summarize(mean_consumption = mean(total_consumption_ch, na.rm = TRUE)) %>%
  ungroup()

ggplot(monthly_avg_consumption, aes(x = Month, y = mean_consumption, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Consumption by Month",
    x     = "Month",
    y     = "Average Consumption"
  )

# plot total hydro production
monthly_avg_hydro_production<- final_data %>%
  mutate(Month = month(Datum, label = TRUE, abbr = TRUE)) %>%
  group_by(Month) %>%
  summarize(mean_production= mean(hydro_production, na.rm = TRUE)) %>%
  ungroup()

ggplot(monthly_avg_hydro_production, aes(x = Month, y = mean_production, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Hydro Production by Month",
    x     = "Month",
    y     = "Average Production"
  )

# plot total
ggplot(final_data, aes(x = total_consumption_ch, y = hydro_production)) +
  geom_line() +
  labs(y = "Production in GWh", x = "Total Consumption in GWh")

# plot log production
ggplot(final_data, aes(x = Datum, y = log_hydro)) +
  geom_line() +
  labs(title = "Log Hydropower Production Over Time", y = "GWh", x = "Date")

# plot production vs. reservoir
ggplot(final_data, aes(x = reservoir_fill_ratio, y = hydro_production)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Hydro Production vs. Reservoir Fill Ratio", x = "%", y = "Hydro Production")

# plot production vs. heating
ggplot(final_data, aes(x = HeatingDegreeDays, y = hydro_production)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Heating Demand vs. Hydro Production", x = "Heating Degree Days", y = "Hydro Production")

# plot production vs. rain
ggplot(final_data, aes(x = rain, y = hydro_production)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Monthly Rainfall in mm", y = "Hydro Production in GWh")

# plot production vs. WaterBalance
ggplot(final_data, aes(x = WaterBalance, y = hydro_production)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "WaterBalance vs. Hydro Production", x = "Monthly WaterBalance", y = "Hydro Production (GWh)")

# plot production vs. temp
ggplot(final_data, aes(x = air_temp_mean, y = hydro_production)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Temp. vs. Hydro Production", x = "°C", y = "Hydro Production (GWh)")

# correlation matrix
cor_matrix <- cor(final_data %>% select(-Datum) %>% drop_na(), use = "complete.obs")

# Prepare the correlation matrix
cor_matrix <- final_data %>%
  select(-Datum) %>%
  drop_na() %>%
  cor(use = "complete.obs")

# Plot the correlation matrix
ggcorrplot(cor_matrix,
           method = "circle",         # or "square"
           type = "lower",            # only lower triangle
           lab = TRUE,                # show values
           lab_size = 3,              # smaller label size
           tl.cex = 10,               # text label size
           tl.srt = 45,               # rotate labels 45° to avoid overlap
           colors = c("red", "white", "blue"),
           ggtheme = theme_minimal())

# create time series tsibble
final_tsibble <- final_data %>%
  mutate(Datum = yearmonth(Datum)) %>%
  as_tsibble(index = Datum)

# Seasonal Plot
final_tsibble %>%
  gg_season(hydro_production) +
  labs(title = "Seasonal Plot of Hydropower Production")

# Subseries Plot
final_tsibble %>%
  gg_subseries(hydro_production) +
  labs(
    x= "Date",
    y= "Production in GWh"
  )

# ----preparing for training--------------------------------------------------------------

# Train set (2000-2022)
train_data <- final_tsibble %>% filter(Datum <= yearmonth("2022 Dec"))

# test set (2023–2024)
test_data <- final_tsibble %>% filter(Datum >= yearmonth("2023 Jan"))

# ---- decompose --------------------------------------------------------------

# structural break test
train_ts <- ts(train_data$hydro_production, start = c(2000, 1), frequency = 12)

# Run F-statistic for every possible breakpoint
fs <- Fstats(train_ts ~ 1)

# Plot the F-statistics over time
plot(fs, main = "QLR-type SupF Test for Structural Breaks")

# Add significance threshold (e.g., 5% level)
abline(h = boundary(fs, 0.05), col = "red", lty = 2)

bp <- breakpoints(train_ts ~ 1)
summary(bp)

# decompose into trend, cycle and season
ts_decomp <- decompose(train_ts) 
plot(ts_decomp)

# trend plot
ggsubseriesplot(train_ts)

# ---- stationary check --------------------------------------------------------------

# Box-Cox transformation using Guerrero method
lambda_raw <- BoxCox.lambda(train_data$hydro_production, method = "guerrero")
print(lambda_raw)

train_data <- train_data %>%
  mutate(hydro_production_boxc = BoxCox(hydro_production, lambda_raw))
test_data <- test_data %>%
  mutate(hydro_production_boxc = BoxCox(hydro_production, lambda_raw))

train_ts_boxc <- ts(train_data$hydro_production_boxc, start = c(2000, 1), frequency = 12)
test_ts_boxc <- ts(test_data$hydro_production_boxc, start = c(2023, 1), frequency = 12)

# Decompose the transformed series
train_ts_boxc_decomp <- decompose(train_ts_boxc)
plot(train_ts_boxc_decomp)

# Plot
ggplot(train_data, aes(x = Datum, y = hydro_production_boxc)) +
  geom_line() +
  labs(
    x = "Date",
    y = "BoxCox-Transformed Production"
  ) +
  theme_minimal()

# plot autocorrelation
acf_plot <- acf(train_data$hydro_production_boxc, main= "Autocorrelation Function (ACF)")

# ADF — H0: non-stationary. Outcome:is stationary p=0.01167 (reject)
k_optimal = floor(12 * (nrow(train_data)/100)^(1/4))
adf.test(train_ts_boxc, alternative = "stationary", k = k_optimal)

# KPSS — H0: stationary Outcome:is trend stationary p=0.1 (reject)
kpss.test(train_ts_boxc, null = "Trend")

# How many non‐seasonal differences to apply
nd <- ndiffs(train_data$hydro_production_boxc)
print(nd)
# nd = 0 → already stationary (no regular diff needed)
# nd = 1 → take one regular difference

# How many seasonal differences to apply
sd <- nsdiffs(train_data$hydro_production_boxc, m = 12)
print(sd)

# ----seasonality check--------------------------------------------------------------

# Plot ACF and PACF side by side
par(mfrow = c(1, 2))  # 1 row, 2 columns layout
acf(train_ts_boxc, lag.max = 48,
    main = "ACF ")
pacf(train_ts_boxc, lag.max = 48,
     main = "PACF")

# Create month variable
train_data$month <- factor(month(train_data$Datum))

# Run an ANOVA (F-test) Outcome: Seasonality F high p value small
summary(aov(train_data$hydro_production ~ train_data$month))

# ----seasonal differencing--------------------------------------------------------------

# Apply seasonal differencing
train_ts_boxc_diff <- diff(train_data$hydro_production_boxc, lag = 12)

# ADF — H0: non-stationary. Outcome:is stationary p=0.02236 (reject)
adf.test(train_ts_boxc_diff, alternative = "stationary", k = k_optimal)

# KPSS — H0: stationary Outcome:is trend stationary p=0.1 (reject)
kpss.test(train_ts_boxc_diff, null = "Trend")

# acf and pacf
acf(train_ts_boxc_diff, lag.max = 48,
    main = "ACF")
pacf(train_ts_boxc_diff, lag.max = 48,
     main = "PACF")
par(mfrow = c(1, 1))  


# -----------------------------------------------------------------------
# ---- Modelling --------------------------------------------------------
# -----------------------------------------------------------------------

# Cross validation
cv_splits <- train_data %>%
  filter(Datum < yearmonth("2023 Jan")) %>%
  stretch_tsibble(.init = 120, .step = 12)

# ---- ARIMA + Seasonal Naive --------------------------------------------------------------

# Training and Validation 2000-2023

# models
models <- cv_splits %>%
  model(
    snaive_model = SNAIVE(box_cox(hydro_production,lambda_raw)),
    
    auto_ets = ETS(box_cox(hydro_production,lambda_raw)),
    ets_ANA = ETS(box_cox(hydro_production,lambda_raw) ~ error("A") + trend("N") + season("A")),
    ets_AAA = ETS(box_cox(hydro_production,lambda_raw) ~ error("A") + trend("A") + season("A")),
    ets_AAN = ETS(box_cox(hydro_production,lambda_raw) ~ error("A") + trend("A") + season("N")),
    
    auto_arima = ARIMA(box_cox(hydro_production,lambda_raw)),
    SARIMA_101_011 = ARIMA(box_cox(hydro_production, lambda_raw) ~ 0 + pdq(1,0,1) + PDQ(0,1,1)),
    SARIMA_101_111 = ARIMA(box_cox(hydro_production, lambda_raw) ~ 0 + pdq(1,0,1) + PDQ(1,1,1)),
    SARIMA_102_111 = ARIMA(box_cox(hydro_production, lambda_raw) ~ 0 + pdq(1,0,2) + PDQ(1,1,1)),
    SARIMA_101_211 = ARIMA(box_cox(hydro_production, lambda_raw) ~ 0 + pdq(1,0,1) + PDQ(2,1,1))
    )

# AICc train results

# glance(models) %>% arrange(AICc)

glance(models) %>%
  group_by(.model) %>%
  summarise(
    mean_sigma2 = mean(sigma2, na.rm = TRUE),
    mean_loglik = mean(log_lik, na.rm = TRUE),
    mean_AIC    = mean(AIC, na.rm = TRUE),
    mean_AICc   = mean(AICc, na.rm = TRUE),
    mean_BIC    = mean(BIC, na.rm = TRUE)
  ) %>%
  arrange(mean_AICc)

# Accuracy
average_accuracy <- accuracy(models) %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    MAE  = mean(MAE, na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE),
    ACF1 = mean(ACF1, na.rm = TRUE)
  ) %>%
  arrange(RMSE)  
print(average_accuracy, n=10)

# ---- ARIMAX --------------------------------------------------------------

# Add ARIMAX separately
arimax_models <- cv_splits %>%
  model(
    
    # SARIMA_101_011 structure + Regressor
    arimax_model_101_011_a = ARIMA(
      box_cox(hydro_production, lambda_raw) ~ 0 +
        reservoir_fill_ratio_lag1 +
        pdq(1,0,1) + PDQ(0,1,1)
    ),
    
    arimax_model_101_011_b = ARIMA(
      box_cox(hydro_production, lambda_raw) ~ 0 +
        solar_radiation + 
        pdq(1,0,1) + PDQ(0,1,1)
    ),
    
    arimax_model_101_011_c = ARIMA(
      box_cox(hydro_production, lambda_raw) ~ 0 +
        reservoir_fill_ratio_lag1 +
        solar_radiation + 
        pdq(1,0,1) + PDQ(0,1,1)
    ),

    # SARIMA_101_111 structure + Regressor
    arimax_model_101_111_a = ARIMA(
      box_cox(hydro_production, lambda_raw) ~ 0 +
        reservoir_fill_ratio_lag1 +
        pdq(1,0,1) + PDQ(1,1,1)
    ),
    
    arimax_model_101_111_b = ARIMA(
      box_cox(hydro_production, lambda_raw) ~ 0 +
        solar_radiation + 
        pdq(1,0,1) + PDQ(1,1,1)
    ),
    
    arimax_model_101_111_c = ARIMA(
      box_cox(hydro_production, lambda_raw) ~ 0 +
        reservoir_fill_ratio_lag1 +
        solar_radiation + 
        pdq(1,0,1) + PDQ(1,1,1)
    ),
    
  )

# AICc train results
# glance(arimax_models) %>% arrange(AICc)

glance(arimax_models) %>%
  group_by(.model) %>%
  summarise(
    mean_sigma2 = mean(sigma2, na.rm = TRUE),
    mean_loglik = mean(log_lik, na.rm = TRUE),
    mean_AIC    = mean(AIC, na.rm = TRUE),
    mean_AICc   = mean(AICc, na.rm = TRUE),
    mean_BIC    = mean(BIC, na.rm = TRUE)
  ) %>%
  arrange(mean_AICc)

# Accuracy
average_accuracy_ARIMAX <- accuracy(arimax_models) %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    MAE  = mean(MAE, na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE),
    ACF1 = mean(ACF1, na.rm = TRUE)
  ) %>%
  arrange(RMSE)
print(average_accuracy_ARIMAX)

# Coefficient ARIMAX 101_011
arimax_models %>%
  filter(.id == 1) %>%
  select(arimax_model_101_011_a) %>%
  tidy()

arimax_models %>%
  filter(.id == 1) %>%
  select(arimax_model_101_011_b) %>%
  tidy()

arimax_models %>%
  filter(.id == 1) %>%
  select(arimax_model_101_011_c) %>%
  tidy()

# Coefficient ARIMAX 101_111
arimax_models %>%
  filter(.id == 1) %>%
  select(arimax_model_101_111_a) %>%
  tidy()

arimax_models %>%
  filter(.id == 1) %>%
  select(arimax_model_101_111_b) %>%
  tidy()

arimax_models %>%
  filter(.id == 1) %>%
  select(arimax_model_101_111_c) %>%
  tidy()

# ----Residual Diagnostic--------------------------------------------------------------

best_model <- train_data %>%
  model(
    best_ets = ETS(box_cox(hydro_production,lambda_raw)~ error("A") + trend("N") + season("A")),
    best_arima = ARIMA(box_cox(hydro_production,lambda_raw) ~ 0 + pdq(1,0,1) + PDQ(2,1,1)),
    best_arimax = ARIMA(box_cox(hydro_production, lambda_raw) ~ 0 + reservoir_fill_ratio_lag1 + solar_radiation + pdq(1,0,1) + PDQ(0,1,1))
  )

best_model %>%
  select(best_ets) %>%
  report()

best_model %>%
  select(best_arima) %>%
  report()

best_model %>%
  select(best_arimax) %>%
  report()

# ETS: Resiudals + Ljung-Box
gg_tsresiduals(best_model %>% select(best_ets))
augment(best_model %>% select(best_ets)) %>%
  features(.innov, ljung_box, lag = 12, dof = 0)

# ARIMA: Resiudals + Ljung-Box
gg_tsresiduals(best_model %>% select(best_arima))
augment(best_model %>% select(best_arima)) %>%
  features(.innov, ljung_box, lag = 12, dof = 5)

# ARIMAX: Resiudals + Ljung-Box
gg_tsresiduals(best_model %>% select(best_arimax))
augment(best_model %>% select(best_arimax)) %>%
  features(.innov, ljung_box, lag = 12, dof = 3)


# ----Test Data--------------------------------------------------------------

# Testing on 2023-2024

# Forecast 
forecast_models <- best_model %>%
  forecast(new_data = test_data)

forecast_models %>%
  autoplot(test_data, level = 95) +
  labs(
    x = "Date",
    y = "Hydropower Production (GWh)"
  ) +
  facet_wrap(~.model, ncol = 1) +
  theme_minimal()

# Accuracy 
accuracy(forecast_models, test_data)

# ------------------------------------------------------------------------------
# ---- Forecasting 2025 --------------------------------------------------------
# ------------------------------------------------------------------------------

final_tsibble_full <- final_data %>%
  mutate(Datum = yearmonth(Datum)) %>% 
  as_tsibble(index = Datum)

lambda_full <- BoxCox.lambda(final_tsibble_full$hydro_production, method = "guerrero")

forecasting2025_model <- final_tsibble_full %>%
  model(
    best_ets = ETS(box_cox(hydro_production,lambda_full)~ error("A") + trend("N") + season("A")),
    best_arima = ARIMA(box_cox(hydro_production,lambda_full) ~ 0 + pdq(1,0,1) + PDQ(2,1,1)),
    best_arimax = ARIMA(box_cox(hydro_production, lambda_full) ~ 0 + reservoir_fill_ratio_lag1 + solar_radiation + pdq(1,0,1) + PDQ(0,1,1))
  )


# Creating table for mean and sd of exogenous variables
exo_vars <- c(
  "reservoir_fill_ratio_lag1",
  "solar_radiation"
)
exo_vars_monthly_stats <- train_data %>%
  as_tibble() %>%  
  mutate(
    Month = month(as.Date(Datum), label = TRUE, abbr = TRUE)
  ) %>%
  group_by(Month) %>%
  summarise(
    across(
      all_of(exo_vars),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  mutate(Month = factor(as.character(Month), levels = month.abb)) %>%
  arrange(Month)

# Dates for 2025 
future_dates <- yearmonth(seq(as.Date("2025-01-01"), by = "month", length.out = 12))

# simulate
set.seed(42)  

sim_exog_data <- tibble(
  Datum           = future_dates,
  solar_radiation = rnorm(12, mean = exo_vars_monthly_stats$solar_radiation_mean, sd = exo_vars_monthly_stats$solar_radiation_sd),
  reservoir_fill_ratio_lag1 = rnorm(12, mean = exo_vars_monthly_stats$reservoir_fill_ratio_lag1_mean, sd = exo_vars_monthly_stats$reservoir_fill_ratio_lag1_sd),
) %>%
  
  as_tsibble(index = Datum)

# Forecast with simulated exogenous variables
sim_forecast <- forecast(forecasting2025_model, new_data = sim_exog_data)

# Plot forecast
autoplot(sim_forecast) +
  labs(
    x = "Date", y = "Hydropower Production (GWh)"
  ) +
  facet_wrap(~.model, ncol = 1) +
  theme_minimal()

# Monte Carlo --------------------------------------------

# Number of simulations
n_sim <- 1000

# Create list to store simulated forecasts
simulated_forecasts <- vector("list", n_sim)

for (i in 1:n_sim) {
  # Simulate exogenous input
  sim_exog_data <- tibble(
    Datum = future_dates,
    solar_radiation = rnorm(12, mean = exo_vars_monthly_stats$solar_radiation_mean, sd = exo_vars_monthly_stats$solar_radiation_sd),
    reservoir_fill_ratio_lag1 = rnorm(12, mean = exo_vars_monthly_stats$reservoir_fill_ratio_lag1_mean, sd = exo_vars_monthly_stats$reservoir_fill_ratio_lag1_sd),
  ) %>%
    as_tsibble(index = Datum)
  
  # Forecast using ARIMAX model
  sim_fc <- forecast(forecasting2025_model %>% select(best_arimax), new_data = sim_exog_data)
  
  # Store mean forecasts
  simulated_forecasts[[i]] <- sim_fc %>% 
    as_tibble() %>%
    select(Datum, .mean) %>%
    mutate(sim_id = i)
}

# Combine all simulations
forecast_distribution <- bind_rows(simulated_forecasts)

# Summarize forecasts (mean and 95% interval per month)
summary_forecast <- forecast_distribution %>%
  group_by(Datum) %>%   
  summarise(
    mean_forecast = mean(.mean, na.rm = TRUE),
    lower_95 = quantile(.mean, 0.025, na.rm = TRUE),
    upper_95 = quantile(.mean, 0.975, na.rm = TRUE)
  )

ggplot(summary_forecast, aes(x = Datum, y = mean_forecast)) +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = "lightgreen", alpha = 0.4) +
  labs(
    x = "Date",
    y = "Hydropower Production (GWh)"
  ) +
  theme_minimal()