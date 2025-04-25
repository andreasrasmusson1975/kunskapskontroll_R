library(glue)

# A function for doing checks on the car data before and data cleaning
# has been performed.
do_checks <- function(df) {
  result <- new.env()
  
  result$num_duplicates <- sum(duplicated(df))
  result$num_missing <- sum(is.na(df))
  for(col in names(df)){
    result[[col]] <- check_column(df,col)
  }
  
  return(result)
  
}

# A function for checking a specific column of the car data. The following
# actions are performed:
#
# 1. The unique values of the coloumn are collected for visual inspection
#    of duplicates, typos etc.
# 2. The number of missing values in the column is collected
# 3. If the column is one of the columns that are supposed to be
#    numeric in the data, a conversion attempt is made to convert
#    it to numeric. If there is a conversion error, that means further
#    cleaning is needed.
check_column <- function(df,col) {
  result <- new.env()
  
  result$unique <- df |>
    pull(!!sym(col)) |>
    unique() |>
    sort() |>
    paste(collapse = ', ')
  
  result$num_missing <- sum(is.na(df[[col]]))
  
  numerics = c('Försäljningspris','Miltal','Modellår','Hästkrafter (HK)')
  check = all(!is.na(suppressWarnings(as.numeric(df[[col]]))))
  if (col %in% numerics) {
    if (check) {
      result$numeric_conversion_error <- FALSE
    }
    else{
      result$numeric_conversion_error <- TRUE  
    }
    
  }
  else {
    result$numeric_conversion_error <- FALSE
  }
  
  return(result)
}

# A function for printing a summary of the checks for the car data
print_result <- function(result) {
  print(glue('Number of duplicate rows: {result$num_duplicates}'))
  print(glue('Total number of missing values: {result$num_missing}'))
  num_conversion_errors <- 0
  numerics = c('Försäljningspris','Miltal','Modellår','Hästkrafter (HK)')
  factors = c('Säljare ','Bränsle','Växellåda','Biltyp','Drivning','Färg','Modell','Region')
  conversion_errors <- 0
  for(col in numerics) {
    conversion_errors <- conversion_errors + result[[col]]$numeric_conversion_error
  }
  print(glue('Total number of conversion errors: {conversion_errors}'))
  print(glue('\nSäljare unique values:\n{result[["Säljare"]]$unique}\n\n'))
  print(glue('\nBränsle unique values:\n{result[["Bränsle"]]$unique}\n\n'))
  print(glue('\nVäxellåda unique values:\n{result[["Växellåda"]]$unique}\n\n'))
  print(glue('\nBiltyp unique values:\n{result[["Biltyp"]]$unique}\n\n'))
  print(glue('\nDrivning unique values:\n{result[["Drivning"]]$unique}\n\n'))
  print(glue('\nFärg unique values:\n{result[["Färg"]]$unique}\n\n'))
  print(glue('\nModell unique values:\n{result[["Modell"]]$unique}\n\n'))
  print(glue('\nRegion unique values:\n{result[["Region"]]$unique}\n\n'))
}

# A function for printing the checking results for a specific column
print_column <- function(result,col) {
  print(glue('Number of missing values: {result[[col]]$num_missing}'))
  print(glue('Numeric conversion error: {result[[col]]$numeric_conversion_error}'))
  print(glue(result[[col]]$unique))
}



