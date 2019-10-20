# load Libraries --------------------------------------------------------------
library(forecast)
library(tseries)
library(zoo)
library(bizdays)
library(lubridate)
library(plotly)
library(urca)

setwd('D:/rodri/My GIT Projects/Time-Series-Analysis/markdown')

# load auxiliary functions ----------------------------------------------------
source("../src/util/auxiliary_functions.R")

# executing data preparation steps --------------------------------------------
source("../src/datapreparation/step_01_config_environment.R")
setwd('D:/rodri/My GIT Projects/Time-Series-Analysis/markdown')

#py_run_file("./src/datapreparation/step_02_data_download.py")
source("../src/datapreparation/step_03_data_ingestion.R")
source("../src/datapreparation/step_04_data_cleaning.R")
source("../src/datapreparation/step_05_data_enhancement.R")
source("../src/datapreparation/step_06_dataset_preparation.R")

# reading dataset ----
target_data <- readRDS('../data/processed/target_dataset.rds')
parameters <- GetTimeSeriesDateParameters(target_data$date, 0.70)

# creating time series object ----
target_ts <- ts(target_data$taxa_venda, 
                start = parameters$start, 
                end = parameters$end, 
                frequency = 12)

# differentiating target ts ----
target_ts_diff <- diff(target_ts, lag = 1)

# testing autocorrelation ----

summary(ur.kpss(target_ts))
summary(ur.kpss(target_ts_diff))

summary(ur.df(target_ts))
summary(ur.df(target_ts_diff))

checkresiduals(target_ts, test = 'LB')
Box.test(target_ts, type = "Ljung")

checkresiduals(target_ts_diff, test = 'LB')
Box.test(target_ts_diff, type = "Ljung")

target_ts <- target_ts
# creating train and test sets ----
GenerateTrainTestDatasets(target_ts,
                          parameters$train_start,
                          parameters$train_end,
                          parameters$test_start,
                          parameters$test_end)

test_sample_size = parameters$test_sample_size

train_ts <- readRDS('../data/processed/train_ts.rds')
test_ts  <- readRDS('../data/processed/test_ts.rds')

# plot of the training and testing temporal series ----
plot(train_ts, 
     xlab = "Tempo", 
     ylab = "Indices", 
     xaxt = "n",
     xlim = c(parameters$start_year, parameters$end_year),
     ylim = c(0, 16),
     bty = "l")

axis(1, at = seq(parameters$start_year, parameters$end_year, 1), 
     labels = format(seq(parameters$start_year, parameters$end_year, 1)))

lines(test_ts, bty = "l", col = "red")

# function to facilitate projection ----
plot_projection <- function(model_proj, test_ts) {
  
  plot(model_proj, 
       ylab = "index", 
       xlab = "Tempo", 
       bty = "l", 
       xaxt = "n", 
       flty = 2)
  
  lines(Modelo_ARIMA$fitted, lwd = 2, col = "blue")
  
  lines(test_ts)
}

# Modelo Arima
Modelo_ARIMA <- Arima(train_ts, order = c(2, 1, 1))
summary(Modelo_ARIMA)
modelo_ARIMA_proj <- forecast(Modelo_ARIMA, h = test_sample_size, level = 0.95)
plot_projection(modelo_ARIMA_proj, test_ts)

# Modelo Arima
Modelo_Auto_ARIMA <- auto.arima(train_ts, stepwise = FALSE, approximation  =  FALSE)
summary(Modelo_Auto_ARIMA)
modelo_Auto_ARIMA_proj <- forecast(Modelo_Auto_ARIMA, h = test_sample_size, level = 0.95)
plot_projection(modelo_Auto_ARIMA_proj, test_ts)

