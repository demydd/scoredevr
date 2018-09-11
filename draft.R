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
library(stringr)
library(smbinning)

data(chileancredit)
#old_dir <- getwd()
#new_dir <- "D:\\Demyd\\Personal\\R"
#setwd(new_dir)

#the table to collect aggregated info about interval distribution of each variable
initial_intervals_summary <- data.frame(  variable = as.character()
                                         ,variable_factor = as.character()  
                                         ,interval_type = as.character()
                                         ,interval_number = as.integer()
                                         ,interval_str = as.character()
                                         ,start = as.numeric()
                                         ,end = as.numeric()
                                         ,total = as.integer()
                                         ,good = as.integer()
                                         ,bad = as.integer()
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
gb <- as.vector(unlist(initial_data[, ..good_bad]))
#selected variables to bin
selected_vars <- selectVars(initial_data, c("fgood #cbs1 cbs2 #cbs3 #dep #cbnew #cbdpd #pmt"), good_bad)
#data set to be processed
initial_data_updated <- as.data.table(chileancredit[,c(names(chileancredit) %in% selected_vars)])
names(initial_data_updated) <- selected_vars
#processed factor table (ready for binning as a vector)
binned_factor_table <- binFactor(initial_data_updated, selected_vars, factor_type = 3, gb = gb)

#sum(is.na(initial_data_updated$cbs2))
#sum(is.na(initial_data_updated$cbs2))

#unique(initial_data_updated$cbs2)

test <- binVector(initial_data_updated, interval_qty, selected_vars, gb)

summary <- test[[2]]
data <- test[[1]]
data <- cbind(data, gb, 1)

data <- data[ , .N, by = cbs2]
  
sum(data$N)
cbind(data, summary$total)


data[,sum(V3), by = cbs2]

sum(is.na(data$dep))
sum(summary$total)

sum(is.na(data$dep))
unique(data$cbs2)

xxx <- binFactor(initial_data_updated, selected_vars, factor_type = 3, gb = gb, rounding = 6)


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

binFactor <- function(  initial_data_updated
                      , column_classes = NA
                      , column_names = NA
                      , selected_vars = NULL
                      , factor_type = 1
                      , gb
                      , rounding = 4){

    #the table to collect aggregated info about interval distribution of each variable
    initial_intervals_summary <- data.frame(  variable = as.character()
                                              ,variable_factor = as.character()  
                                              ,interval_type = as.character()
                                              ,interval_number = as.integer()
                                              ,interval_str = as.character()
                                              ,start = as.numeric()
                                              ,end = as.numeric()
                                              ,total = as.integer()
                                              ,good = as.integer()
                                              ,bad = as.integer()
                                           )  
  
      #vector of column classes
      column_classes <- sapply(initial_data_updated, class)
      #define factor column
      factors_selected_index  <- which(column_classes == "factor")
      #vector of column names for factors
      column_names <- names(initial_data_updated)
      if (is.null(selected_vars)){
        column_names_factor <- column_names[factors_selected_index]
      } else {
        column_names_factor <- column_names[column_names[factors_selected_index] %in% selected_vars]
      }

  #temporary table to contain transposed vectors
  nrows <- dim(initial_data_updated)[1]
  #temporary table for all options
  tmp_table <- data.table(nrows = nrows)
  #temporary table for option
  tmp_level_table <- data.table(nrows = nrows)
  tmp_vector <- c(1:nrows)
  
  # OPTION1 - Dummy varuables. FOR loop to process all factors in vector per each level
  if (factor_type == 1){
    for (step in column_names_factor){
      #define factor levels in the selected column
      cycle <- levels(unlist(initial_data_updated[,..step]))
      #FOR loop to process factor levels
      interval_number <- 1
        
      for(j in cycle){
        #define the vector with 1 and 0 per each level
        condition <- as.integer(unlist(initial_data_updated[,..step]) == j)
        #populate the temporary table
        tmp_table <- cbind(tmp_table, condition)
        #put names to new columns
        names(tmp_table)[dim(tmp_table)[2]] <- paste(step, "_", j, sep = "") 

        #put data into interval summary table
        unique_intervals <- unique(condition)
        for (inter in 1:length(unique_intervals)){
          #check for NA items
          if (is.na(unique_intervals[inter])){
              initial_intervals_summary <- rbind(initial_intervals_summary, 
                                                  data.frame(  variable = as.character(step)
                                                              ,variable_factor = as.character(paste(step, "_", j, sep = "")) #variable <- 
                                                              ,interval_type = as.character("factor") #interval_type <- 
                                                              ,interval_number = as.integer(inter) #interval_number <- 
                                                              ,interval_str = as.character("NA = NA")  #interval_str <-       
                                                              ,start = NA #start <- 
                                                              ,end = NA #end <- 
                                                              ,total = sum(is.na(condition)) #total <- 
                                                              ,good = sum(gb[is.na(condition)] == 1) #good <- 
                                                              ,bad = sum(is.na(condition)) - sum(gb[is.na(condition)]) #bad <- 
                                                            )
                                                )
                                               
            
                  
          } else {
          #check non-NA items
              initial_intervals_summary <- rbind(initial_intervals_summary, 
                                                 data.frame( variable = as.character(step)
                                                            ,variable_factor = as.character(paste(step, "_", j, sep = "")) #variable <- 
                                                            ,interval_type = as.character("factor") #interval_type <- 
                                                            ,interval_number = as.integer(inter) #interval_number <- 
                                                            ,interval_str = as.character(paste(inter-1,"=", inter - 1))  #interval_str <-       
                                                            ,start = as.numeric(inter - 1) #start <- 
                                                            ,end = as.numeric(inter - 1) #end <- 
                                                            ,total = as.numeric(sum(condition == inter - 1)) #total <- 
                                                            ,good = as.numeric(sum(gb[condition == inter - 1])) #good <- 
                                                            ,bad = as.numeric(sum(condition == inter - 1) - sum(gb[condition == inter - 1])) #bad <- 
                                                          )
                                                )
          }
          
      }
        
        
     }
      
   }
    
  }

  # OPTION2 - FOR loop to process all factors in integer per each level
  if (factor_type == 2){
    
    for (step in column_names_factor){
      #add the integer vector
      selection <- as.integer(unlist(initial_data_updated[,..step])) 
      tmp_table <- cbind(tmp_table,  as.integer(unlist(initial_data_updated[,..step])))
      names(tmp_table)[dim(tmp_table)[2]] <- step
      
      #put data into interval summary table
      unique_intervals <- levels(unlist(initial_data_updated[,..step]))
      for (inter in 1:length(unique_intervals)){
        
        #check for NA items
        if (is.na(unique_intervals[inter])){
          initial_intervals_summary <- rbind(initial_intervals_summary, 
                                              data.frame(   variable = as.character(step)
                                                           ,variable_factor = as.character(unique_intervals[inter]) #variable <- 
                                                           ,interval_type = as.character("factor") #interval_type <- 
                                                           ,interval_number = as.integer(inter) #interval_number <- 
                                                           ,interval_str = as.character("NA = NA")  #interval_str <-       
                                                           ,start = NA #start <- 
                                                           ,end = NA #end <- 
                                                           ,total = sum(is.na(selection)) #total <- 
                                                           ,good = sum(gb[is.na(selection)] == 1) #good <- 
                                                           ,bad = sum(is.na(selection)) - sum(gb[is.na(selection)]) #bad <- 
                                              )
                                            )
          
          
          
        } else {
          #check non-NA items
          initial_intervals_summary <- rbind(initial_intervals_summary, 
                                              data.frame(  variable = as.character(step)
                                                          ,variable_factor = as.character(unique_intervals[inter]) #variable <- 
                                                          ,interval_type = as.character("factor") #interval_type <- 
                                                          ,interval_number = as.integer(inter) #interval_number <- 
                                                          ,interval_str = as.character(paste(inter,"=", inter))  #interval_str <-       
                                                          ,start = as.numeric(inter) #start <- 
                                                          ,end = as.numeric(inter) #end <- 
                                                          ,total = as.numeric(sum(selection == inter)) #total <- 
                                                          ,good = as.numeric(sum(gb[selection == inter])) #good <- 
                                                          ,bad = as.numeric(sum(selection == inter) - sum(gb[selection == inter])) #bad <- 
                                                        )
                                            )
        }
        
      }    
      
    }
        
    
    
  }
  
    # OPTION3 - FOR loop to process all factors as mean per each level
  if (factor_type == 3){
    for (step in column_names_factor){
      #define factor levels in the selected column
      cycle <- levels(unlist(initial_data_updated[,..step]))
      #FOR loop to process factor levels
      for(j in cycle){
        #define the vector with 1 and 0 per each level
        condition <- unlist(initial_data_updated[,..step]) == j
        #calculate mean per each level 
        mean_level <- round(mean(unlist(gb[condition]), na.rm = FALSE), rounding)
        #populate the temporary vector with level mean
        tmp_vector[condition] <- mean_level
        
        #put data into interval summary table
        inter <- which(cycle %in% j)
          #check for NA items
          if (is.na(j)){
            initial_intervals_summary <- rbind(initial_intervals_summary, 
                                                data.frame(   variable = as.character(step)
                                                              ,variable_factor = as.character(j) #variable <- 
                                                              ,interval_type = as.character("factor") #interval_type <- 
                                                              ,interval_number = as.integer(inter) #interval_number <- 
                                                              ,interval_str = as.character(paste(mean_level,"=",mean_level))  #interval_str <-       
                                                              ,start = mean_level #start <- 
                                                              ,end = mean_level #end <- 
                                                              ,total = sum(is.na(condition)) #total <- 
                                                              ,good = sum(gb[is.na(condition)] == 1) #good <- 
                                                              ,bad = sum(is.na(condition)) - sum(gb[is.na(condition)] == 1) #bad <- 
                                                )
                                              )
            
            
            
          } else {
            #check non-NA items
            initial_intervals_summary <- rbind(initial_intervals_summary, 
                                                data.frame(  variable = as.character(step)
                                                             ,variable_factor = as.character(j) #variable <- 
                                                             ,interval_type = as.character("factor") #interval_type <- 
                                                             ,interval_number = as.integer(inter) #interval_number <- 
                                                             ,interval_str = as.character(paste(mean_level,"=",mean_level))  #interval_str <-       
                                                             ,start = mean_level #start <- 
                                                             ,end = mean_level #end <- 
                                                             ,total = sum(condition) #total <- 
                                                             ,good = sum(gb[condition == 1]) #good <- 
                                                             ,bad = sum(condition) - sum(gb[condition == 1]) #bad <- 
                                                          )
                                               )
          }
          
        #}    
        

      }
      #populate the temporary table
      tmp_table <- cbind(tmp_table, tmp_vector)
      #put names to new columns
      names(tmp_table)[dim(tmp_table)[2]] <- step  
      
    }
    
  }

  #return binned factor portfolio and interval summary.  
  return(list(tmp_table[, -1], initial_intervals_summary))
}

#the function to preprocess data
processData <- function(initial_data, selected_vars){

}

#the function to bin vector and factor data
binVector <- function(initial_data_updated, interval_qty, selected_vars, gb){
  browser()
  
  initial_intervals_summary <- data.frame(   variable = as.character()
                                            ,variable_factor = as.character()  
                                            ,interval_type = as.character()
                                            ,interval_number = as.integer()
                                            ,interval_str = as.character()
                                            ,start = as.numeric()
                                            ,end = as.numeric()
                                            ,total = as.integer()
                                            ,good = as.integer()
                                            ,bad = as.integer()
                                        )  
  #vector of column classes
  column_classes <- sapply(initial_data_updated, class)
  #reduce the input data by factor columns
  index <- which(column_classes %in% c("integer", "numeric", "complex"))
  column_classes <- column_classes[index]

  #vector of column names[index]
  column_names <- names(initial_data_updated)[index]
  if (is.null(selected_vars)) selected_vars <- column_names
  column_names <- column_names[column_names %in% selected_vars]
  initial_data_updated <- initial_data_updated[ , ..column_names]
  attribute_qty <- length(column_names)
  
  #the final output table
  binned_table <- data.table(matrix(nrow = column_length, ncol = length(column_names)))

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
    
    if (sum(is.na(unique(sorted_vector))) > 0) actual_vector_intervals <- cbind(actual_vector_intervals, c(NA, NA))
    
    #rename columns: Vx -> 1, 2, 3 ...
    colnames(actual_vector_intervals) <- as.character(c(1:dim(actual_vector_intervals)[2]))
    rownames(actual_vector_intervals) <- c("start", "end")
    #actual interval q-ty
    actual_vector_intervals_qty <- dim(actual_vector_intervals)[2]

    #make data table for binned intervals

    setnames(binned_table, colnames(binned_table), column_names)

    tmp_tbl <- binColumn(
                          vector_to_be_binned = sorted_vector
                          ,actual_vector_intervals = actual_vector_intervals
                          ,actual_vector_intervals_qty = actual_vector_intervals_qty
                          ,gb = gb
                          ,column_classes = column_classes[j]
                          ,column_names = column_names[j]
                        )
    
    binned_table[, j] <- tmp_tbl[[1]]
    
    initial_intervals_summary <- rbind(initial_intervals_summary, tmp_tbl[[2]])

  }
  
  return(list(binned_table, initial_intervals_summary))
}


#function to bin vector data
binColumn <- function(  vector_to_be_binned
                       ,actual_vector_intervals
                       ,actual_vector_intervals_qty
                       ,gb
                       ,column_classes
                       ,column_names
                       ,env = parent.frame()

                     ){
  
  #temporary nterval summary
  initial_intervals_summary <- data.frame(   variable = as.character()
                                             ,variable_factor = as.character()  
                                             ,interval_type = as.character()
                                             ,interval_number = as.integer()
                                             ,interval_str = as.character()
                                             ,start = as.numeric()
                                             ,end = as.numeric()
                                             ,total = as.integer()
                                             ,good = as.integer()
                                             ,bad = as.integer()
                                          )  
  
  #make temporary vector for binning (intervals are marked as integer values)
  mapping_vector <- rep(0, column_length)

  #loop to check all intervals and paste the order number of intervals
  for (i in 1:actual_vector_intervals_qty){

    #check the first interval
    if(i == 1 && sum(vector_to_be_binned[!is.na(vector_to_be_binned)] < actual_vector_intervals[2, i]) > 0){

      mapping_vector[vector_to_be_binned[!is.na(vector_to_be_binned)] < actual_vector_intervals[2, i]] <- i

      total <- sum(vector_to_be_binned[!is.na(vector_to_be_binned)] < actual_vector_intervals[2, i])
      good <- sum(gb[vector_to_be_binned[!is.na(vector_to_be_binned)] < actual_vector_intervals[2, i]] == 1)
      initial_intervals_summary <- rbind(initial_intervals_summary, 
                                         data.frame(   variable = column_names
                                                      ,variable_factor = NA #variable <- 
                                                      ,interval_type = column_classes #interval_type <- 
                                                      ,interval_number = i #interval_number <- 
                                                      ,interval_str = paste("<", actual_vector_intervals[2, i])  #interval_str <-       
                                                      ,start = actual_vector_intervals[1, i] #start <- 
                                                      ,end = actual_vector_intervals[2, i] #end <- 
                                                      ,total =  total #total <- 
                                                      ,good = good #good <- 
                                                      ,bad = total - good #bad <- 
                                                  )
                                        )             

    }

    #check the last interval
    if(i == actual_vector_intervals_qty - 1 && sum(vector_to_be_binned[!is.na(vector_to_be_binned)] >= actual_vector_intervals[1, i]) > 0){

      mapping_vector[vector_to_be_binned[!is.na(vector_to_be_binned)] >= actual_vector_intervals[1, actual_vector_intervals_qty - 1]] <- i
      
      total <- sum(vector_to_be_binned[!is.na(vector_to_be_binned)] >= actual_vector_intervals[1, actual_vector_intervals_qty - 1])
      good <- sum(gb[vector_to_be_binned[!is.na(vector_to_be_binned)] >= actual_vector_intervals[1, actual_vector_intervals_qty - 1]] == 1)
      initial_intervals_summary <- rbind(initial_intervals_summary, 
                                         data.frame(    variable = column_names
                                                       ,variable_factor = NA #variable <- 
                                                       ,interval_type = column_classes #interval_type <- 
                                                       ,interval_number = i #interval_number <- 
                                                       ,interval_str = paste(">=", actual_vector_intervals[1, i])  #interval_str <-       
                                                       ,start = actual_vector_intervals[1, i] #start <- 
                                                       ,end = actual_vector_intervals[2, i] #end <- 
                                                       ,total =  total #total <- 
                                                       ,good = good #good <- 
                                                       ,bad = total - good #bad <- 
                                                  )
                                        )       


    }

    #check the rest of items
    if (i != 1 && i < actual_vector_intervals_qty - 1){

      mapping_vector[vector_to_be_binned[!is.na(vector_to_be_binned)] >= actual_vector_intervals[1, i] & vector_to_be_binned[!is.na(vector_to_be_binned)] < actual_vector_intervals[2, i]] <- i

      total <- sum(vector_to_be_binned[!is.na(vector_to_be_binned)] >= actual_vector_intervals[1, i] & vector_to_be_binned[!is.na(vector_to_be_binned)] < actual_vector_intervals[2, i])
      good <- sum(gb[vector_to_be_binned[!is.na(vector_to_be_binned)] >= actual_vector_intervals[1, i] & vector_to_be_binned[!is.na(vector_to_be_binned)] < actual_vector_intervals[2, i]] == 1)
      initial_intervals_summary <- rbind(initial_intervals_summary, 
                                         data.frame(   variable = column_names
                                                       ,variable_factor = NA #variable <- 
                                                       ,interval_type = column_classes #interval_type <- 
                                                       ,interval_number = i #interval_number <- 
                                                       ,interval_str = paste(">=", actual_vector_intervals[1, i], "<", actual_vector_intervals[2, i])  #interval_str <-       
                                                       ,start = actual_vector_intervals[1, i] #start <- 
                                                       ,end = actual_vector_intervals[2, i] #end <- 
                                                       ,total =  total #total <- 
                                                       ,good = good #good <- 
                                                       ,bad = total - good #bad <- 
                                                  )
                                        )       
      
    }
    
    #check NA values
    if(sum(is.na(vector_to_be_binned)) > 0 & i == actual_vector_intervals_qty){
      
      mapping_vector[is.na(vector_to_be_binned)] <- i
      
      total <- sum(is.na(vector_to_be_binned))
      good <- sum(gb[is.na(vector_to_be_binned)] == 1)
      initial_intervals_summary <- rbind(initial_intervals_summary, 
                                         data.frame(     variable = column_names
                                                         ,variable_factor = NA #variable <- 
                                                         ,interval_type = column_classes #interval_type <- 
                                                         ,interval_number = i #interval_number <- 
                                                         ,interval_str = "NA = NA"  #interval_str <-       
                                                         ,start = NA #start <- 
                                                         ,end = NA #end <- 
                                                         ,total =  total #total <- 
                                                         ,good = good #good <- 
                                                         ,bad = total - good #bad <- 
                                                  )
                                        )       
      
      
    }  
    
    
  }
  browser()  
 

  return(list(mapping_vector, initial_intervals_summary))
  #binned_table[, j] <<- mapping_vector

}


#rm(list=ls())
#gc()




