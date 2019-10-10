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

# to-do: include description for this function  
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

# to-do: include description for this function  
RunLinearTimeSeriesModel <- function (train_ts, 
                                      test_ts, 
                                      formula,
                                      test_sample_size,
                                      number_of_periods_for_forecasing,
                                      title) {

  model <- tslm(as.formula(formula))
  model_projected <- forecast(model, h = test_sample_size, level = 0.95)
  model_projected_analisys <- accuracy(model_projected, test_ts)
  model_final <- tslm(as.formula(formula))
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

# to-do: include description for this function
GenerateLinearTimeSeriesModels <- function (train_ts, test_ts) {
  # Tendência Linear  
  model_trend <- RunLinearTimeSeriesModel(train_ts,
                                          test_ts,
                                          formula = "train_ts ~ trend", 
                                          test_sample_size = 4,
                                          number_of_periods_for_forecasing = 36,
                                          title = "Tendência Linear")
  saveRDS(model_trend, '../models/ts_linear_model_trend.rds')
  
  # Tendência Quadrática
  model_trend_square <- RunLinearTimeSeriesModel(train_ts,
                                                 test_ts,
                                                 formula = "train_ts ~ trend + I(trend^2)", 
                                                 test_sample_size = 4,
                                                 number_of_periods_for_forecasing = 36,
                                                 title = "Tendência Quadrática")
  saveRDS(model_trend_square, '../models/ts_linear_model_trend_square.rds')
  
  # Tendência Linear com Sazonalidade
  model_trend_season <- RunLinearTimeSeriesModel(train_ts,
                                                 test_ts,
                                                 formula = "train_ts ~ trend + season", 
                                                 test_sample_size = 4,
                                                 number_of_periods_for_forecasing = 36,
                                                 title = "Tendência Linear com Sazonalidade")
  saveRDS(model_trend_season, '../models/ts_linear_model_trend_season.rds')
  
  # Tendência Quadrática com Sazonalidade
  model_trend_square_season <- RunLinearTimeSeriesModel(train_ts,
                                                        test_ts,
                                                        formula = "train_ts ~ season + trend + I(trend^2)", 
                                                        test_sample_size = 4,
                                                        number_of_periods_for_forecasing = 36,
                                                        title = "Tendência Quadrática com Sazonalidade")
  saveRDS(model_trend_square_season, '../models/ts_linear_model_trend_square_season.rds')
  
  # performing results consolidation
  consolidation <- tibble(model = character(), mape = numeric())
  
  consolidation <-  add_row(consolidation, 
                            model = "model_trend", 
                            mape = model_trend$model_projected_analisys["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            model = "model_trend_square", 
                            mape = model_trend_square$model_projected_analisys["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            model = "model_trend_season", 
                            mape = model_trend_season$model_projected_analisys["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            model = "model_trend_square_season", 
                            mape = model_trend_square_season$model_projected_analisys["Test set",'MAPE'])
  
  return(consolidation)
}