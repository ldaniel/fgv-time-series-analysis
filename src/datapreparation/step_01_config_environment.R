# loading required libraries --------------------------------------------------
# data collection libraries
library(Quandl)

# data prep libraries
library(tibble)
library(magrittr)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(reticulate)

# plotting libraries
library(ggplot2)
library(plotly)
library(ggthemes)

# clearing everything before starting -----------------------------------------
ClearEnvironment()

# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

info.username  <- Sys.info()[["user"]]
info.sysname   <- Sys.info()[["sysname"]]
info.machine   <- Sys.info()[["machine"]]
info.encoding  <- getOption("encoding")
directoryPath  <- dirname(rstudioapi::getSourceEditorContext()$path)
directoryPath  <- stringr::str_replace(directoryPath, "/src/datapreparation", "")
directoryPath  <- stringr::str_replace(directoryPath, "/src/playground", "")
directoryPath  <- stringr::str_replace(directoryPath, "/markdown", "")

setwd(directoryPath)
getwd()