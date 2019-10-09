# load Libraries --------------------------------------------------------------
library(forecast)
library(tseries)
library(zoo)
library(bizdays)
library(lubridate)
library(plotly)

# load auxiliary functions ----------------------------------------------------
source("./src/util/auxiliary_functions.R")

# executing data preparation steps --------------------------------------------
source("./src/datapreparation/step_01_config_environment.R")
#py_run_file("./src/datapreparation/step_02_data_download.py")
source("./src/datapreparation/step_03_data_ingestion.R")
source("./src/datapreparation/step_04_data_cleaning.R")
source("./src/datapreparation/step_05_data_enhancement.R")
source("./src/datapreparation/step_06_dataset_preparation.R")

# Create Time Series ----------------------------------------------------------

df <- filter(bond_data, titulo == "NTN-B Princ 2024-08-15") %>% 
  select(dia, taxa_venda) %>% 
  arrange(dia)

dates <- seq(min(df$dia), max(df$dia), by = "day") %>% as_tibble
colnames(dates) <- c('dia')

df <- left_join(dates, df, by = 'dia')
df$isWeekend <- ifelse(wday(df$dia) %in% c(7, 1), 1, 0)
df$isHoliday <- ifelse(df$dia %in% holidaysANBIMA, 1, 0)
df$taxa_venda = na.locf(df$taxa_venda, na.rm = TRUE) * 100

rm(dates)

time_serie <- ts(df$taxa_venda, start = decimal_date(min(df$dia)), 
                 frequency = 365)

# getting time series componente ----
ts_decom <- decompose(time_serie, type = 'additive')

plot(ts_decom)
stl(time_serie, s.window = 'periodic')

hist(ts_decom[["random"]], main = 'Random Noise - Histogram')
hist(ts_decom[["seasonal"]], main = 'Seasonal - Histogram')
hist(ts_decom[["trend"]], main = 'Trend - Histogram')

# getting autocorrelation metrics ----
auto_correl <- acf(time_serie)
parc_correl <- pacf(time_serie)

adf.test(time_serie)
kpss.test(time_serie)

# experimenting with auto ARIMA ----
auto_ARIMA <- auto.arima(time_serie)

auto_ARIMA

plot(forecast(auto_ARIMA, h = 30))

forecast_ts <- forecast(auto_ARIMA, h = 30)

# exploring results ----

result <- tibble(dia = df$dia,
                actual = df$taxa_venda,
                predict = forecast_ts$fitted,
                residual = forecast_ts$residuals,
                error = abs(forecast_ts$residuals))

result <- filter(result, dia >= today() - 180)

p <- ggplot(data = result) +
  geom_line(aes(x = dia, y = actual), color = 'grey') +
  geom_line(aes(x = dia, y = predict), color = 'blue') +
  theme(panel.background = element_blank())

p <- ggplotly(p)

p

tail(result, n = 15)
