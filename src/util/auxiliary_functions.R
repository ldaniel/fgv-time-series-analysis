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

# linear model functions ------------------------------------------------------

# this function is capable of running a linear time series model
# given a certain formula and based in a train/test pair datasets
RunLinearTimeSeriesModel <- function (train_ts, 
                                      test_ts, 
                                      formula_train,
                                      formula_final,
                                      test_sample_size,
                                      number_of_periods_for_forecasing,
                                      title) {

  model <- tslm(as.formula(formula_train))
  model_projected <- forecast(model, h = test_sample_size, level = 0.95)
  model_projected_analisys <- accuracy(model_projected, test_ts)
  model_final <- tslm(as.formula(formula_final))
  model_final_projected <- forecast(model_final, h = number_of_periods_for_forecasing, level = 0.95)

  modelresults <- list()
  modelresults$model <- model
  modelresults$Acf <- Acf(model$residuals)
  modelresults$checkresiduals <- checkresiduals(model, test = "LB")
  modelresults$model_projected <- model_projected
  modelresults$model_projected_analisys <- model_projected_analisys
  modelresults$model_final_projected <- model_final_projected

  return(modelresults)
}

# this function generates and saves all linear time series models to the
# models directory for later consuming
GenerateLinearTimeSeriesModels <- function (train_ts, test_ts) {
  # Tendência Linear  
  model_trend <- RunLinearTimeSeriesModel(train_ts,
                                          test_ts,
                                          formula_train = "train_ts ~ trend",
                                          formula_final = "target_ts ~ trend", 
                                          test_sample_size = 4,
                                          number_of_periods_for_forecasing = 36,
                                          title = "Tendência Linear")
  saveRDS(model_trend, '../models/ts_linear_model_trend.rds')
  
  # Tendência Quadrática
  model_trend_square <- RunLinearTimeSeriesModel(train_ts,
                                                 test_ts,
                                                 formula_train = "train_ts ~ trend + I(trend^2)", 
                                                 formula_final = "target_ts ~ trend + I(trend^2)", 
                                                 test_sample_size = 4,
                                                 number_of_periods_for_forecasing = 36,
                                                 title = "Tendência Quadrática")
  saveRDS(model_trend_square, '../models/ts_linear_model_trend_square.rds')
  
  # Tendência Linear com Sazonalidade
  model_trend_season <- RunLinearTimeSeriesModel(train_ts,
                                                 test_ts,
                                                 formula_train = "train_ts ~ trend + season", 
                                                 formula_final = "target_ts ~ trend + season", 
                                                 test_sample_size = 4,
                                                 number_of_periods_for_forecasing = 36,
                                                 title = "Tendência Linear com Sazonalidade")
  saveRDS(model_trend_season, '../models/ts_linear_model_trend_season.rds')
  
  # Tendência Quadrática com Sazonalidade
  model_trend_square_season <- RunLinearTimeSeriesModel(train_ts,
                                                        test_ts,
                                                        formula_train = "train_ts ~ season + trend + I(trend^2)", 
                                                        formula_final = "target_ts ~ season + trend + I(trend^2)", 
                                                        test_sample_size = 4,
                                                        number_of_periods_for_forecasing = 36,
                                                        title = "Tendência Quadrática com Sazonalidade")
  saveRDS(model_trend_square_season, '../models/ts_linear_model_trend_square_season.rds')
  
  # performing result's consolidation
  consolidation <- tibble(Model = character(), MAPE = numeric())
  
  consolidation <-  add_row(consolidation, 
                            Model = "model_trend", 
                            MAPE = model_trend$model_projected_analisys["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = "model_trend_square", 
                            MAPE = model_trend_square$model_projected_analisys["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = "model_trend_season", 
                            MAPE = model_trend_season$model_projected_analisys["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = "model_trend_square_season", 
                            MAPE = model_trend_square_season$model_projected_analisys["Test set",'MAPE'])
  
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
                                                         number_of_periods_for_forecasing,
                                                         title) {
  
  model <- ets(train_ts, model = method)
  model_projected <- forecast(model, h = test_sample_size, level = 0.95)
  model_projected_analisys <- accuracy(model_projected, test_ts)
  model_final <- ets(target_ts, model = method)
  model_final_projected <- forecast(model_final, h = number_of_periods_for_forecasing, level = 0.95)
  
  modelresults <- list()
  modelresults$model <- model
  modelresults$Acf <- Acf(model$residuals)
  modelresults$checkresiduals <- checkresiduals(model, test = "LB")
  modelresults$model_projected <- model_projected
  modelresults$model_projected_analisys <- model_projected_analisys
  modelresults$model_final_projected <- model_final_projected
  
  return(modelresults)
}

# this function generates and saves all linear time series models to the
# models directory for later consuming
GenerateExponentialsmoothingStateTimeSeriesModel <- function (target_ts, train_ts, test_ts) {

  methods_list <- c("ANN", "AAN", "ANA", "AAA", "MNN", 
                    "MAN", "MMN", "MNM", "MAM", "MMM", 
                    "MNA", "MAA", "ZZZ")
  consolidation <- tibble(Model = character(), MAPE = numeric())
  
  for (method_item in methods_list) {
    model <- RunExponentialsmoothingStateTimeSeriesModel(target_ts,
                                                         train_ts, 
                                                         test_ts, 
                                                         method = method_item,
                                                         test_sample_size = 4,
                                                         number_of_periods_for_forecasing = 36,
                                                         title = method)
    model_file_name <- paste0('../models/ts_exponential_smoothing_model_', method_item,'.rds')
    saveRDS(model, model_file_name)
    
    consolidation <-  add_row(consolidation,
                              Model = method_item, 
                              MAPE = model$model_projected_analisys["Test set",'MAPE'])
  }
  
  return(consolidation)
}