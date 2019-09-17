# analysing missing values and other strange conditions -----------------------

# looking for NAs
mytable_na_cols <- sapply(mytable, function(x) sum(is.na(x)))

# looking for empty values
mytable_empty_cols <-  sapply(mytable, function(x) table(as.character(x) == "" | as.character(x) == " ")["TRUE"])

# fixing NAs and missing values
mytable <- mutate(mytable, x_importance = ifelse(is.na(x_importance), "1", x_importance))