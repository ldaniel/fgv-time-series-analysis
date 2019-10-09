# functions -----------------------------------------------------------

# clear environment, memory, console screen and plots
ClearEnvironment <- function () {
  rm(list=ls())
  invisible(gc())
  cat("\014")
  
  while (!is.null(dev.list()))
    dev.off()
}
  
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

# work in progress
RunSmoothingTimeSeriesModel <- function () {
  #to-do
}

# work in progress
GetBetterTimeSeriesLinearModel <- function () {
  model_trend <- RunLinearTimeSeriesModel("train_ts ~ trend", 
                                "Tendência Linear")
  model_trend_square <- RunLinearTimeSeriesModel("train_ts ~ trend + I(trend^2)", 
                                       "Tendência Quadrática")
  model_trend_season <- RunLinearTimeSeriesModel("train_ts ~ trend + season", 
                                       "Tendência Linear com Sazonalidade")
  model_trend_square_season <- RunLinearTimeSeriesModel("train_ts ~ season + trend + I(trend^2)", 
                                              "Tendência Quadrática com Sazonalidade")
  
  rownames(model_trend)[1] <- "Tendência Linear Training Set" 
  rownames(model_trend)[2] <- "Tendência Linear Test Set" 
  
  rownames(model_trend_square)[1] <- "Tendência Quadrática Training Set" 
  rownames(model_trend_square)[2] <- "Tendência Quadrática Test Set" 
  
  rownames(model_trend_season)[1] <- "Tendência Linear com Sazonalidade Training Set" 
  rownames(model_trend_season)[2] <- "Tendência Linear com Sazonalidade Test Set" 
  
  rownames(model_trend_square_season)[1] <- "Tendência Quadrática com Sazonalidade Training Set" 
  rownames(model_trend_square_season)[2] <- "Tendência Quadrática com Sazonalidade Test Set" 
  
  comparison <-  rbind(model_trend, 
                       model_trend_square,
                       model_trend_season,
                       model_trend_square_season)
}

# work in progress
GetBetterTimeSeriesSmoothingModel <- function () {
  #to-do
}