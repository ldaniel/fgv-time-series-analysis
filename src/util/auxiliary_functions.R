# functions -----------------------------------------------------------
clearEnv <- function() {
  
  # clear environment and memory
  rm(list=ls())
  invisible(gc())
  
  # clear console screen
  cat("\014")
  
  # clear plots
  while (!is.null(dev.list()))  
    dev.off()
}