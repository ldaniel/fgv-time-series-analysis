# load auxiliary functions ----------------------------------------------------
source("./src/util/auxiliary_functions.R")

# executing data preparation steps --------------------------------------------
source("./src/datapreparation/step_01_config_environment.R")
if (info.sysname == "Linux" & info.username == 'jovyan') {
  use_condaenv('notebook')
}
py_run_file("./src/datapreparation/step_02_data_download.py")
source("./src/datapreparation/step_03_data_ingestion.R")
source("./src/datapreparation/step_04_data_cleaning.R")
source("./src/datapreparation/step_05_data_enhancement.R")
source("./src/datapreparation/step_06_dataset_preparation.R")
