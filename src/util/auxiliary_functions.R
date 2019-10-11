# general functions -----------------------------------------------------------

# clear environment, memory, console screen and plots
ClearEnvironment <- function () {
  rm(list=ls())
  invisible(gc())
  cat("\014")
  
  while (!is.null(dev.list()))
    dev.off()
}

# model functions -------------------------------------------------------------

# this function will define the all the required dates for the time series 
# train and test periods based on the percentage desired to split these datasets
GetTimeSeriesDateParameters <- function (target_base_date, percentage){

  split_index <- round(length(target_base_date) * percentage)
  
  parameters <- list()
  parameters$start       <- c(as.numeric(format(as.Date(min(target_base_date), format="%d/%m/%Y"),"%Y")),
                              as.numeric(format(as.Date(min(target_base_date), format="%d/%m/%Y"),"%m")))
  parameters$end         <- c(as.numeric(format(as.Date(max(target_base_date), format="%d/%m/%Y"),"%Y")),
                              as.numeric(format(as.Date(max(target_base_date), format="%d/%m/%Y"),"%m")))
  parameters$start_year  <- as.numeric(format(as.Date(min(target_base_date), format="%d/%m/%Y"),"%Y"))
  parameters$end_year    <- as.numeric(format(as.Date(max(target_base_date), format="%d/%m/%Y"),"%Y"))
  parameters$train_start <- c(as.numeric(format(as.Date(min(target_base_date), format="%d/%m/%Y"),"%Y")),
                              as.numeric(format(as.Date(min(target_base_date), format="%d/%m/%Y"),"%m")))
  parameters$train_end   <- c(as.numeric(format(as.Date(target_base_date[split_index], format="%d/%m/%Y"),"%Y")),
                              as.numeric(format(as.Date(target_base_date[split_index], format="%d/%m/%Y"),"%m")))
  parameters$test_start  <- c(as.numeric(format(as.Date(target_base_date[split_index + 1], format="%d/%m/%Y"),"%Y")),
                              as.numeric(format(as.Date(target_base_date[split_index + 1], format="%d/%m/%Y"),"%m")))
  parameters$test_end    <- c(as.numeric(format(as.Date(max(target_base_date), format="%d/%m/%Y"),"%Y")),
                              as.numeric(format(as.Date(max(target_base_date), format="%d/%m/%Y"),"%m")))
  parameters$test_sample_size <- length(target_base_date) - split_index
  
  return(parameters)
}

# this function will creates the training and testing datasets and 
# save them to the data/processed directory for later consuming
GenerateTrainTestDatasets <- function (target_ts,
                                       train_start,
                                       train_end,
                                       test_start,
                                       test_end) {
  train_ts <- window(target_ts, start = train_start, end = train_end)
  test_ts  <- window(target_ts, start = test_start,  end = test_end)
  
  saveRDS(train_ts, '../data/processed/train_ts.rds')
  saveRDS(test_ts, '../data/processed/test_ts.rds')
}

# naive model functions -------------------------------------------------------

# this function is capable of running the naive time series model
# given a certain formula and based in a train/test pair datasets
RunNaiveModel <- function (train_ts, 
                           test_ts, 
                           test_sample_size) {

  model <- naive(train_ts, level = 0, h = test_sample_size)
  model_accuracy <- accuracy(model, test_ts)
  
  modelresults <- list()
  modelresults$title <- "Naive"
  modelresults$model <- model
  modelresults$accuracy <- model_accuracy

  return(modelresults)
}

# this function generates and saves the naive time series models to the
# model's directory for later consuming
GenerateNaiveTimeSeriesModel <- function (train_ts, test_ts, test_sample_size) {
  
  model_naive <- RunNaiveModel(train_ts,
                               test_ts,
                               test_sample_size)
  saveRDS(model_naive, '../models/ts_naive_model.rds')
  
  # performing result's consolidation
  consolidation <- tibble(Model = character(), MAPE = numeric())
  
  consolidation <-  add_row(consolidation, 
                            Model = model_naive$title, 
                            MAPE = model_naive$accuracy["Test set",'MAPE'])

  return(consolidation)
}

# linear model functions ------------------------------------------------------

# this function is capable of running a linear time series model
# given a certain formula and based in a train/test pair datasets
RunLinearTimeSeriesModel <- function (train_ts, 
                                      test_ts, 
                                      formula_train,
                                      formula_final,
                                      test_sample_size,
                                      number_of_periods_for_forecasting,
                                      title) {
  
  model <- tslm(as.formula(formula_train))
  model_projected <- forecast(model, h = test_sample_size, level = 0.95)
  model_accuracy <- accuracy(model_projected, test_ts)
  model_final <- tslm(as.formula(formula_final))
  model_final_projected <- forecast(model_final, h = number_of_periods_for_forecasting, level = 0.95)
  
  modelresults <- list()
  modelresults$title <- title
  modelresults$model <- model
  modelresults$model_projected <- model_projected
  modelresults$accuracy <- model_accuracy
  modelresults$model_final <- model_final
  modelresults$model_final_projected <- model_final_projected
  
  return(modelresults)
}

# this function generates and saves all linear time series models to the
# model's directory for later consuming
GenerateLinearTimeSeriesModels <- function (train_ts, test_ts, test_sample_size) {
  # Tendência Linear
  model_trend <- RunLinearTimeSeriesModel(train_ts,
                                          test_ts,
                                          formula_train = "train_ts ~ trend",
                                          formula_final = "target_ts ~ trend", 
                                          test_sample_size,
                                          number_of_periods_for_forecasting = 36,
                                          title = "Tendência Linear")
  saveRDS(model_trend, '../models/ts_linear_model_trend.rds')
  
  # Tendência Quadrática
  model_trend_square <- RunLinearTimeSeriesModel(train_ts,
                                                 test_ts,
                                                 formula_train = "train_ts ~ trend + I(trend^2)", 
                                                 formula_final = "target_ts ~ trend + I(trend^2)", 
                                                 test_sample_size,
                                                 number_of_periods_for_forecasting = 36,
                                                 title = "Tendência Quadrática")
  saveRDS(model_trend_square, '../models/ts_linear_model_trend_square.rds')

  # Sazonalidade
  model_season <- RunLinearTimeSeriesModel(train_ts,
                                           test_ts,
                                           formula_train = "train_ts ~ season", 
                                           formula_final = "target_ts ~ season", 
                                           test_sample_size,
                                           number_of_periods_for_forecasting = 36,
                                           title = "Sazonalidade")
  saveRDS(model_season, '../models/ts_linear_model_season.rds')
  
  # Tendência Linear com Sazonalidade
  model_trend_season <- RunLinearTimeSeriesModel(train_ts,
                                                 test_ts,
                                                 formula_train = "train_ts ~ trend + season", 
                                                 formula_final = "target_ts ~ trend + season", 
                                                 test_sample_size,
                                                 number_of_periods_for_forecasting = 36,
                                                 title = "Tendência Linear com Sazonalidade")
  saveRDS(model_trend_season, '../models/ts_linear_model_trend_season.rds')
  
  # Tendência Quadrática com Sazonalidade
  model_trend_square_season <- RunLinearTimeSeriesModel(train_ts,
                                                        test_ts,
                                                        formula_train = "train_ts ~ season + trend + I(trend^2)", 
                                                        formula_final = "target_ts ~ season + trend + I(trend^2)", 
                                                        test_sample_size,
                                                        number_of_periods_for_forecasting = 36,
                                                        title = "Tendência Quadrática com Sazonalidade")
  saveRDS(model_trend_square_season, '../models/ts_linear_model_trend_square_season.rds')
  
  # performing result's consolidation
  consolidation <- tibble(Model = character(), MAPE = numeric())
  
  consolidation <-  add_row(consolidation, 
                            Model = model_trend$title, 
                            MAPE = model_trend$accuracy["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = model_trend_square$title, 
                            MAPE = model_trend_square$accuracy["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = model_season$title, 
                            MAPE = model_season$accuracy["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = model_trend_season$title, 
                            MAPE = model_trend_season$accuracy["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = model_trend_square_season$title, 
                            MAPE = model_trend_square_season$accuracy["Test set",'MAPE'])
  
  return(consolidation)
}

# moving average (MA) model functions ---------------------------------------

# this function is capable of running the moving average (MA) time series 
# model given a certain method and based in a train/test pair datasets
RunMovingAverageTimeSeriesModel <- function (train_ts, 
                                             test_ts, 
                                             test_sample_size,
                                             train_sample_size,
                                             k_value,
                                             order_value,
                                             start_date,
                                             end_date,
                                             frequency) {
  
  simple_ma <- rollmean(train_ts, k = k_value, align = "right")
  centered_ma <- ma(train_ts, order = order_value)
  last_ma <- tail(simple_ma, 1)
  simple_ma_projection <- ts(rep(last_ma, test_sample_size), 
                             start=c(start_date, train_sample_size + 1), 
                             end = c(end_date, train_sample_size + test_sample_size), 
                             freq = frequency)
  
  model_train_accuracy <- accuracy(simple_ma, train_ts)
  model_test_accuracy <- accuracy(simple_ma_projection, test_ts)
  
  
  modelresults <- list()
  modelresults$title <- "Moving Average (MA)"
  modelresults$simple_ma <- simple_ma
  modelresults$centered_ma <- centered_ma
  modelresults$simple_ma_projection <- simple_ma_projection
  modelresults$train_accuracy <- model_train_accuracy
  modelresults$test_accuracy <- model_test_accuracy
  
  return(modelresults)
}

# this function generates and saves the moving average (MA) time series models 
# to the model's directory for later consuming
GenerateMovingAverageTimeSeriesModel <- function (target_ts, train_ts, test_ts, test_sample_size) {
  
  model_ma <- RunMovingAverageTimeSeriesModel(train_ts, 
                                              test_ts, 
                                              test_sample_size,
                                              train_sample_size = length(target_ts) - test_sample_size,
                                              k_value = 12,
                                              order_value = 12,
                                              start_date = 2005,
                                              end_date = 2019,
                                              frequency = 12)
  saveRDS(model_ma, '../models/ts_moving_average_model.rds')
  
  # performing result's consolidation
  consolidation <- tibble(Model = character(), MAPE = numeric())
  
  consolidation <-  add_row(consolidation, 
                            Model = model_ma$title, 
                            MAPE = model_ma$test_accuracy["Test set",'MAPE'])
  
  return(consolidation)
} 


# exponential smoothing model functions ---------------------------------------

# this function is capable of running a exponential smooghing time series 
# model given a certain method and based in a train/test pair datasets
RunExponentialsmoothingStateTimeSeriesModel <- function (target_ts,
                                                         train_ts, 
                                                         test_ts, 
                                                         method,
                                                         test_sample_size,
                                                         number_of_periods_for_forecasting) {
  
  model <- ets(train_ts, model = method)
  model_projected <- forecast(model, h = test_sample_size, level = 0.95)
  model_accuracy <- accuracy(model_projected, test_ts)
  model_final <- ets(target_ts, model = method)
  model_final_projected <- forecast(model_final, h = number_of_periods_for_forecasting, level = 0.95)
  
  modelresults <- list()
  modelresults$title <- method
  modelresults$model <- model
  modelresults$model_projected <- model_projected
  modelresults$accuracy <- model_accuracy
  modelresults$model_final <- model_final
  modelresults$model_final_projected <- model_final_projected
  
  return(modelresults)
}

# this function generates and saves all linear time series models to the
# model's directory for later consuming
GenerateExponentialsmoothingStateTimeSeriesModel <- function (target_ts, train_ts, test_ts, test_sample_size) {

  methods_list <- c("ANN", "AAN", "ANA", "AAA", "MNN", 
                    "MAN", "MMN", "MNM", "MAM", "MMM", 
                    "MNA", "MAA", "ZZZ")
  consolidation <- tibble(Model = character(), MAPE = numeric())
  
  for (method_item in methods_list) {
    model <- RunExponentialsmoothingStateTimeSeriesModel(target_ts,
                                                         train_ts, 
                                                         test_ts, 
                                                         method = method_item,
                                                         test_sample_size,
                                                         number_of_periods_for_forecasting = 36)
    model_file_name <- paste0('../models/ts_exponential_smoothing_model_', method_item,'.rds')
    saveRDS(model, model_file_name)
    
    consolidation <-  add_row(consolidation,
                              Model = model$title, 
                              MAPE = model$accuracy["Test set",'MAPE'])
  }
  
  return(consolidation)
}
