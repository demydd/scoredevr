#install.packages("data.table")
#library(data.table)

#old_dir <- getwd()
#new_dir <- "D:\\Demyd\\Personal\\R"
#setwd(new_dir)

#file_output <- fread("source.txt")
#str(file_output)

#as.Date(file_output$BIRTHDAY, format = "%d %m %Y")

#as.Date(file_output$BIRTHDAY, format = "%d %m %Y")

#install.packages("stringr")
#library(stringr)

#mmm <- str_replace(file_output$BIRTHDAY, ".", "/")

#file_output$BIRTHDAY <- as.Date(chartr(".", "/", file_output$BIRTHDAY), format = "%d/%m/ %Y")

##############################################################################################################

library(data.table)
library(smbinning)

data(chileancredit)
old_dir <- getwd()
new_dir <- "D:\\Demyd\\Personal\\R"
setwd(new_dir)

#the table to collect aggregated info about interval distribution of each variable
initial_intervals_summary <- data.frame( variable = as.character()
                                         ,interval_type = as.character()
                                         ,interval_number = as.integer()
                                         ,start = as.numeric()
                                         ,end = as.numeric()
                                         ,total = as.integer()
                                         ,good = as.integer()
)

names(chileancredit)[1:3]

initial_data <- as.data.table(chileancredit)
#qty of attributes to bin
interval_qty <- 20
#the length of the predictor for binning
column_length <- dim(initial_data)[1]
#GOOD/BAD column name
good_bad <- "fgood"
#GOOD/BAD vector
gb <- as.vector(initial_data[, ..good_bad])
#selected variables to bin
selected_vars <- selectVars(initial_data, c("fgood #cbs1 cbs2 cbs3 dep cbnew cbdpd"), good_bad)
#the final output table
binned_table <- data.table(matrix(nrow = column_length, ncol = length(selected_vars)))
#data set to be processed
initial_data_updated <- as.data.table(chileancredit[,c(names(chileancredit) %in% selected_vars)])
#row_unique_identifier
initial_data_updated[, row_num := .I]
#initial_data_updated <- cbind(initial_data_updated, gb)
binAll(initial_data_updated, interval_qty, selected_vars)



############################################################################################################

#select the necessary variable and reduce the data table
selectVars <- function(initial_data,  column_names, good_bad){

  #remove end of line
  x_var <- gsub("[\n]", "", column_names)
  #convert single string value into character vector
  x_var <- unlist(strsplit(x_var, " "))
  #m <- x_var != ""
  #index <- which(m %in% c(TRUE))
  x_var <- x_var[x_var != ""]
  #remove commented fields
  commented <- grepl("#",x_var)
  #index <- which((commented) %in% c(FALSE))
  #purified vector with fields to model
  x_var <- x_var[(commented) %in% c(FALSE)]

  # fields to be used in binning (numerics only)
  x_var <- x_var[x_var != good_bad]

  print(paste("predictors selected:",length(x_var)))

  return (x_var)

}

compilePreSummary <- function(){

}

processFactor <- function(initial_data_updated, selected_vars, factor_type){

  #vector of column classes
  column_classes <- sapply(initial_data_updated, class)
  factors_selected_index  <- which(column_classes == "factor")
  if (length(factors_selected_index) == 0) break

  #vector of column names
  column_names <- names(initial_data_updated)
  column_names_factor <- column_names[factors_selected_index]
  column_names_factor[length(column_names_factor)+1] <- "row_num"
  
  i <-1
  #column_classes == column_names
  xxx <- data.table(nrows = dim(initial_data_updated)[1])
  
  for (i in length(column_names_factor)-1){

    #initial_data_updated[, ..column_names_factor]
    
    

    formula_string <- paste("row_num ~ ", column_names_factor[i]) 
    
    m <- dcast.data.table(initial_data_updated[, ..column_names_factor], formula(formula_string), fun.aggregate = length, drop = FALSE)
    xxx <- cbind(xxx, m[,-1])
    
  }

}


#the function to preprocess data
processData <- function(initial_data, selected_vars){

}


#the function to bin vector and factor data
binAll <- function(initial_data_updated, interval_qty, selected_vars){
  browser()
  #vector of column classes
  column_classes <- sapply(initial_data_updated, class)
  #vector of column names
  column_names <- names(initial_data_updated)
  attribute_qty <- length(column_names)

  if (interval_qty > column_length) {

    stop ('The function execution is interrupted: The number of intervals > column length!')

  } else {
    #indecies to find values for each interval
    vector_index <- round(quantile(c(1:column_length), c(seq(0, 1, 1/interval_qty))), 0)

  }

  for (j in 1:attribute_qty){
    #order the vector in ascendency
    sorted_vector <- sort(as.vector(unlist(initial_data_updated[, ..j])), na.last = TRUE)
    #numbers of NA items in the vector
    NA_values_qty <- sum(is.na(sorted_vector))
    #share of NA items in the vector
    NA_values_qty_share <- round(sum(is.na(sorted_vector))/length(sorted_vector), 2)

    #interval value distribution before preprocessing
    initial_vector <- sorted_vector[vector_index]
    #numbers of NA intervals in the vector
    NA_intervals_qty <- sum(is.na(initial_vector))
    #share of NA intervals in the vector
    NA_intervals_qty_share <- sum(is.na(initial_vector))/(length(initial_vector)-1)

    #output of NA values and share
    print(paste("NA % in vector: ", NA_values_qty_share * 100, "% (", NA_values_qty, ") of (", column_length, ")", sep = "", collapse = ""))
    print(paste("NA % in intervals: ", NA_intervals_qty_share * 100, "% (", NA_intervals_qty, ") of (", interval_qty, ")", sep = "", collapse = ""))

    #initial vector with intervals without NA
    initial_vector_updated <- initial_vector[!is.na(initial_vector)]
    #remove NAs fron the vector
    sorted_vector_updated <- sorted_vector[!is.na(sorted_vector)]

    #matrix of start and end of intervals (1- star, 2 - end)
    actual_vector_intervals <- rbind(initial_vector_updated[-length(initial_vector_updated)], initial_vector_updated[-1])
    #rename columns: Vx -> 1, 2, 3 ...
    colnames(actual_vector_intervals) <- as.character(c(1:dim(actual_vector_intervals)[2]))
    rownames(actual_vector_intervals) <- c("start", "end")
    #actual interval q-ty
    actual_vector_intervals_qty <- dim(actual_vector_intervals)[2]

    #make data table for binned intervals

    setnames(binned_table, colnames(binned_table), column_names)

    t(actual_vector_intervals)

    binned_table[, j] <<- binVector(
      sorted_vector_updated
      ,actual_vector_intervals
      ,actual_vector_intervals_qty
    )

  }
}


#function to bin factor data
binFactor <- function(vector_to_be_binned){

  binned_table[, j] <<- as.integer(vector_to_be_binned)

}

#function to bin vector data
binVector <- function( vector_to_be_binned
                       ,actual_vector_intervals
                       ,actual_vector_intervals_qty

){

  #make temporary vector for binning (intervals are marked as integer values)
  mapping_vector <- rep(0, column_length)

  #loop to check all intervals and paste the order number of intervals
  for (i in 1:actual_vector_intervals_qty){

    #check the first interval
    if(i == 1 && sum(vector_to_be_binned < actual_vector_intervals[2, i]) > 0){

      mapping_vector[vector_to_be_binned < actual_vector_intervals[2, i]] <- i

    }

    #check the last interval
    if(i == actual_vector_intervals_qty && sum(vector_to_be_binned > actual_vector_intervals[2, i]) > 0){

      mapping_vector[vector_to_be_binned > actual_vector_intervals[2, actual_vector_intervals_qty]] <- i

    }

    #check the rest of items
    if (i != 1 && i < actual_vector_intervals_qty){

      mapping_vector[vector_to_be_binned >= actual_vector_intervals[1, i] & vector_to_be_binned < actual_vector_intervals[2, i]] <- i

    }


  }

  return(mapping_vector)
  #binned_table[, j] <<- mapping_vector

}


rm(list=ls())
rm()


binned_table[, c(names(binned_table)[j])]


length(unique(mapping_vector))

sum(mapping_vector==27)


class(actual_vector_intervals[2, 1])

sorted_vector_updated > 514

class(as.vector(actual_vector_intervals[2, 1]))

str(chileancredit)

as.integer(levels(chileancredit$home))[chileancredit$home]

as.integer(chileancredit$home)
as.integer(chileancredit$inc)

rm(list = ls())
gc()



