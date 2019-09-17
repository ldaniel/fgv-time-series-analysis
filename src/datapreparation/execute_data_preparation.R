
# load auxiliary functions ----------------------------------------------------
source("./src/util/auxiliary_functions.R")

# executing data preparation steps --------------------------------------------

directoryPath  <- dirname(rstudioapi::getSourceEditorContext()$path)
directoryPath  <- stringr::str_replace(directoryPath, "/src/datapreparation", "")
directoryPath  <- stringr::str_replace(directoryPath, "/src/playground", "")
directoryPath  <- stringr::str_replace(directoryPath, "/markdown", "")

setwd(directoryPath)

source("./src/datapreparation/step_01_config_environment.R")
py_run_file("./src/datapreparation/step_02_data_download.py")
source("./src/datapreparation/step_03_data_ingestion.R")
source("./src/datapreparation/step_04_data_cleaning.R")
source("./src/datapreparation/step_05_data_enhancement.R")
source("./src/datapreparation/step_06_dataset_preparation.R")
