# loading required libraries --------------------------------------------------

# libraries for data prep
library(dplyr)
library(stringr)

# libraries for spatial data manipulation
library(rgdal)     

# load auxiliary functions ----------------------------------------------------
source("./src/util/auxiliary_functions.R")

# executing data preparation steps --------------------------------------------
source("./src/datapreparation/step_01_config_environment.R")
source("./src/datapreparation/step_02_data_ingestion.R")
source("./src/datapreparation/step_03_data_cleaning.R")
source("./src/datapreparation/step_04_label_translation.R")
source("./src/datapreparation/step_05_data_enhancement.R")
source("./src/datapreparation/step_06_dataset_preparation.R")