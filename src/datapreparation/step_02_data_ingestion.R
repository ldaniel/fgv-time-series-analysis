# performing data loading -----------------------------------------------------
dataDirectory <- "data/raw/"

# The sample dataset is intended for demostration purpose only
# Please, remove this whole comment section and the sample dataset
# before starting your final project version.
#
# About the sample data set:
# https://github.com/washingtonpost/data-game-of-thrones-deaths

mytable <- read.csv2(paste(dataDirectory, "game-of-thrones-deaths-data.txt", sep = ""), sep = ",", stringsAsFactors = TRUE)

# special case for (big) tables to speedup data ingestion
if(!file.exists('data/processed/bigtable.feather')) {
  mybigtable   <- read.csv2(paste(dataDirectory, "game-of-thrones-deaths-data.txt", sep = ""), stringsAsFactors = TRUE)
  write_feather(mybigtable, 'data/processed/bigtable.feather')
} else {
  mybigtable   <- read_feather('data/processed/bigtable.feather')
}

# performing data casting, column renaming and small touch-ups ----------------

# renaming columns in district table 
names(mytable)[names(mytable) == "order"] <- "x_order"
names(mytable)[names(mytable) == "season"] <- "x_season"
names(mytable)[names(mytable) == "episode"] <- "x_episode"
names(mytable)[names(mytable) == "order"] <- "x_var_1"
names(mytable)[names(mytable) == "character_killed"] <- "x_character_killed"
names(mytable)[names(mytable) == "killer"] <- "x_killer"
names(mytable)[names(mytable) == "method"] <- "x_method"
names(mytable)[names(mytable) == "method_cat"] <- "x_method_cat"
names(mytable)[names(mytable) == "reason"] <- "x_reason"
names(mytable)[names(mytable) == "location"] <- "x_location"
names(mytable)[names(mytable) == "allegiance"] <- "x_allegiance"
names(mytable)[names(mytable) == "importance"] <- "x_importance"
#names(mytable)[names(mytable) == "location"] <- "y_var"


