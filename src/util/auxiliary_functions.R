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

# this function will define all required dates for the time series, including 
# train and test periods based on the percentage desired to split these datasets
GetTimeSeriesDateParameters <- function (target_base_date, percentage){

  split_index <- round(length(target_base_date) * percentage)
  
  parameters <- list()
  parameters$start       <- c(as.numeric(format(as.Date(min(target_base_date), format="%d/%m/%Y"),"%Y")),
                              as.numeric(format(as.Date(min(target_base_date), format="%d/%m/%Y"),"%m")))
  parameters$end         <- c(as.numeric(format(as.Date(max(target_base_date), format="%d/%m/%Y"),"%Y")),
                              as.numeric(format(as.Date(max(target_base_date), format="%d/%m/%Y"),"%m")))
  parameters$start_year  <-   as.numeric(format(as.Date(min(target_base_date), format="%d/%m/%Y"),"%Y"))
  parameters$end_year    <-   as.numeric(format(as.Date(max(target_base_date), format="%d/%m/%Y"),"%Y"))
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
  modelresults$title <- "ts_naive_model"
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
  saveRDS(model_naive, paste0('../models/', model_naive$title,'.rds'))
  
  # performing result's consolidation
  consolidation <- tibble(Model = character(), MAPE_Train = numeric(), MAPE_Test = numeric())
  
  consolidation <-  add_row(consolidation, 
                            Model = model_naive$title, 
                            MAPE_Train = model_naive$accuracy["Training set",'MAPE'],
                            MAPE_Test = model_naive$accuracy["Test set",'MAPE'])

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
GenerateLinearTimeSeriesModels <- function (train_ts, test_ts, test_sample_size, number_of_periods_for_forecasting = 6) {
  max_ylim <-  max(train_ts, test_ts)
  
  # Tendência Linear
  model_trend <- RunLinearTimeSeriesModel(train_ts,
                                          test_ts,
                                          formula_train = "train_ts ~ trend",
                                          formula_final = "target_ts ~ trend", 
                                          test_sample_size,
                                          number_of_periods_for_forecasting = number_of_periods_for_forecasting,
                                          title = "ts_linear_model_trend")
  saveRDS(model_trend, paste0('../models/', model_trend$title,'.rds'))
  SaveFitPlot(model_trend, max_ylim)
  
  # Tendência Quadrática
  model_trend_square <- RunLinearTimeSeriesModel(train_ts,
                                                 test_ts,
                                                 formula_train = "train_ts ~ trend + I(trend^2)", 
                                                 formula_final = "target_ts ~ trend + I(trend^2)", 
                                                 test_sample_size,
                                                 number_of_periods_for_forecasting = number_of_periods_for_forecasting,
                                                 title = "ts_linear_model_trend_square")
  saveRDS(model_trend_square, paste0('../models/', model_trend_square$title,'.rds'))
  SaveFitPlot(model_trend_square, max_ylim)

  # Sazonalidade
  model_season <- RunLinearTimeSeriesModel(train_ts,
                                           test_ts,
                                           formula_train = "train_ts ~ season", 
                                           formula_final = "target_ts ~ season", 
                                           test_sample_size,
                                           number_of_periods_for_forecasting = number_of_periods_for_forecasting,
                                           title = "ts_linear_model_season")
  saveRDS(model_season, paste0('../models/', model_season$title,'.rds'))
  SaveFitPlot(model_season, max_ylim)
  
  # Tendência Linear com Sazonalidade
  model_trend_season <- RunLinearTimeSeriesModel(train_ts,
                                                 test_ts,
                                                 formula_train = "train_ts ~ trend + season", 
                                                 formula_final = "target_ts ~ trend + season", 
                                                 test_sample_size,
                                                 number_of_periods_for_forecasting = number_of_periods_for_forecasting,
                                                 title = "ts_linear_model_trend_season")
  saveRDS(model_trend_season, paste0('../models/', model_trend_season$title,'.rds'))
  SaveFitPlot(model_trend_season, max_ylim)
  
  # Tendência Quadrática com Sazonalidade
  model_trend_square_season <- RunLinearTimeSeriesModel(train_ts,
                                                        test_ts,
                                                        formula_train = "train_ts ~ season + trend + I(trend^2)", 
                                                        formula_final = "target_ts ~ season + trend + I(trend^2)", 
                                                        test_sample_size,
                                                        number_of_periods_for_forecasting = number_of_periods_for_forecasting,
                                                        title = "ts_linear_model_trend_square_season")
  saveRDS(model_trend_square_season, paste0('../models/', model_trend_square_season$title,'.rds'))
  SaveFitPlot(model_trend_square_season, max_ylim)
  
  # performing result's consolidation
  consolidation <- tibble(Model = character(), MAPE_Train = numeric(), MAPE_Test = numeric())
  
  consolidation <-  add_row(consolidation, 
                            Model = model_trend$title, 
                            MAPE_Train = model_trend$accuracy["Training set",'MAPE'],
                            MAPE_Test = model_trend$accuracy["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = model_trend_square$title, 
                            MAPE_Train = model_trend_square$accuracy["Training set",'MAPE'],
                            MAPE_Test = model_trend_square$accuracy["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = model_season$title, 
                            MAPE_Train = model_season$accuracy["Training set",'MAPE'],
                            MAPE_Test = model_season$accuracy["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = model_trend_season$title, 
                            MAPE_Train = model_trend_season$accuracy["Training set",'MAPE'],
                            MAPE_Test = model_trend_season$accuracy["Test set",'MAPE'])
  
  consolidation <-  add_row(consolidation,
                            Model = model_trend_square_season$title, 
                            MAPE_Train = model_trend_square_season$accuracy["Training set",'MAPE'],
                            MAPE_Test = model_trend_square_season$accuracy["Test set",'MAPE'])
  
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
  modelresults$title <- "ts_moving_average_model"
  modelresults$simple_ma <- simple_ma
  modelresults$centered_ma <- centered_ma
  modelresults$simple_ma_projection <- simple_ma_projection
  modelresults$train_accuracy <- model_train_accuracy
  modelresults$test_accuracy <- model_test_accuracy
  
  return(modelresults)
}

# this function generates and saves the moving average (MA) time series models 
# to the model's directory for later consuming
GenerateMovingAverageTimeSeriesModel <- function (target_ts, 
                                                  train_ts, 
                                                  test_ts, 
                                                  test_sample_size,
                                                  start_date_year,
                                                  end_date_year) {
  
  model_ma <- RunMovingAverageTimeSeriesModel(train_ts, 
                                              test_ts, 
                                              test_sample_size,
                                              train_sample_size = length(target_ts) - test_sample_size,
                                              k_value = 12,
                                              order_value = 12,
                                              start_date = start_date_year,
                                              end_date = end_date_year,
                                              frequency = 12)
  
  saveRDS(model_ma, paste0('../models/', model_ma$title,'.rds'))
  
  # performing result's consolidation
  consolidation <- tibble(Model = character(), MAPE_Train = numeric(), MAPE_Test = numeric())
  
  consolidation <-  add_row(consolidation, 
                            Model = model_ma$title, 
                            MAPE_Train = model_ma$train_accuracy["Test set",'MAPE'],
                            MAPE_Test = model_ma$test_accuracy["Test set",'MAPE'])

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
  modelresults$title <- paste0("ts_exponential_smoothing_model_", method)
  modelresults$model <- model
  modelresults$model_projected <- model_projected
  modelresults$accuracy <- model_accuracy
  modelresults$model_final <- model_final
  modelresults$model_final_projected <- model_final_projected
  
  return(modelresults)
}

# this function generates and saves all linear time series models to the
# model's directory for later consuming
GenerateExponentialsmoothingStateTimeSeriesModel <- function (target_ts, train_ts, test_ts, test_sample_size, number_of_periods_for_forecasting = 6) {

  methods_list <- c("ANN", "AAN", "ANA", "AAA", "MNN", 
                    "MAN", "MMN", "MNM", "MAM", "MMM", 
                    "MNA", "MAA", "ZZZ")

  consolidation <- tibble(Model = character(), MAPE_Train = numeric(), MAPE_Test = numeric())
  
  max_ylim <- max(target_ts, train_ts)
  
  for (method_item in methods_list) {
    model <- RunExponentialsmoothingStateTimeSeriesModel(target_ts,
                                                         train_ts, 
                                                         test_ts, 
                                                         method = method_item,
                                                         test_sample_size,
                                                         number_of_periods_for_forecasting = number_of_periods_for_forecasting)
    model_file_name <- paste0('../models/', model$title,'.rds')
    
    saveRDS(model, model_file_name)
    SaveFitPlot(model, max_ylim)
    
    consolidation <-  add_row(consolidation,
                              Model = model$title,
                              MAPE_Train = model$accuracy["Training set",'MAPE'],
                              MAPE_Test = model$accuracy["Test set",'MAPE'])
    }
  
  return(consolidation)
}

# plot model fit ----
SaveFitPlot <- function(model, max_ylim) {

  png(paste('../images/', model$title, '.png'),
      width = 640, 
      height = 640)
  
  plot(model$model_projected, 
       bty = "l",
       ylim = c(0, max_ylim), 
       flty = 2,
       main = str_to_upper(str_replace_all(model$title,'ts|_',' ')))
  
  title(sub = paste('MAPE: ', round(model$accuracy['Test set', 'MAPE'], 5)), 
        col.sub = 'blue',
        xlab = "Tempo",
        ylab = "Indice")
  
  title( 
    col.sub = 'blue',
    xlab = "Tempo",
    ylab = "Indice")
  
  lines(model$model_projected$fitted, lwd=2, col = "blue")
  lines(test_ts, col = 'red')

}

GenerateFitPlotGIF <- function() {
  png_files <- list.files('../images/', full.names = TRUE)
  gif_file <- tempfile(fileext = ".gif")
  gif_file <- '../gifs/model_fit.gif'
  
  gifski(png_files, gif_file)
}
