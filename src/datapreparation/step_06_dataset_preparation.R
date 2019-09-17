# dataset preparation ---------------------------------------------------------

# The objective of this step is to return a DataFrame to be used in the
# predictive modeling.

# defining the final dataset to evaluate
source_dataset <- mytable

# calling function to split and create train and test databases
# this function will split the dataset into train and test data and save the sampling in disk
# to resample just delete './models/source_train_test_dataset.rds' file and rerun this script
if (file.exists('./models/source_train_test_dataset.rds')) {
  source_train_test_dataset <- readRDS('./models/source_train_test_dataset.rds')
} else {
  source_train_test_dataset <- fgvr::createTestAndTrainSamples(source_dataset, "x_importance", 12345, 0.7)
  saveRDS(source_train_test_dataset, './models/source_train_test_dataset.rds')  
}