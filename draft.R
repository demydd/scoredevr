
library(data.table)
library(stringr)
library(smbinning)
library(caret)

data(chileancredit)
#old_dir <- getwd()
#new_dir <- "D:\\Demyd\\Personal\\R"
#setwd(new_dir)

#the table to collect aggregated info about interval distribution of each variable
initial_intervals_summary <- data.frame(  variable = as.character()
                                         ,variable_factor = as.character()
                                         ,column_final = as.character()
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
selected_vars <- selectVars(initial_data, c("fgood cbs1 cbs2 cbs3 dep cbnew cbdpd pmt"), good_bad)
#data set to be processed
initial_data_updated <- as.data.table(chileancredit[,c(names(chileancredit) %in% selected_vars)])
names(initial_data_updated) <- selected_vars
#processed factor table (ready for binning as a vector)
binned_factor_table <- binFactor(initial_data_updated, selected_vars, factor_type = 1, gb = gb)
binned_vectors <- binVector(initial_data_updated, interval_qty, selected_vars, gb)

#overall interval summary
interval_summary <- rbind(binned_factor_table[[2]], binned_vectors[[2]])
#overall binned portfolio
binned_portfolio <- cbind(binned_factor_table[[1]], binned_vectors[[1]])
#add WOE and IV values to interval summary
interval_summary_WOE_IV <- calcWOEIV(interval_summary)
#bin portfolio with WOE values
binned_portfolio_WOE <- binPortfolioWoe(binned_portfolio, interval_summary_WOE_IV)
#calculate correlation
corSummary <- calcCorrelation(binned_portfolio_WOE, cut_off_cor = 0.75)
#add good/bad vector (1 and/or 0)
binned_portfolio_WOE <- cbind(binned_portfolio_WOE, gb)
names(binned_portfolio_WOE)[dim(binned_portfolio_WOE)[2]] <- good_bad 
#calculate model
modelOutput <- calcModel(binned_portfolio_WOE, selected_vars, good_bad, gb)
#select variables by p-value
p_value <- 0.99
selectedModelVars <- rownames(modelOutput[[2]])[modelOutput[[2]][ , 4] < p_value]  
  


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
                                              ,column_final = as.character()
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
        condition[condition == 1] <- 2
        condition[condition == 0] <- 1
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
                                                              ,variable_factor = paste(step, "_", j, sep = "") #variable <- 
                                                              ,column_final = paste(step, "_", j, sep = "")
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
                                                            ,column_final = paste(step, "_", j, sep = "")
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
                                                           ,column_final = as.character(step)
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
                                                          ,column_final = as.character(step)
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
        #populate the temporary vector with interval numberlevel mean
        inter <- which(cycle %in% j)
        tmp_vector[condition] <- inter 

        #put data into interval summary table
        inter <- which(cycle %in% j)
          #check for NA items
          if (is.na(j)){
            initial_intervals_summary <- rbind(initial_intervals_summary, 
                                                data.frame(    variable = as.character(step)
                                                              ,variable_factor = as.character(j) #variable <- 
                                                              ,column_final = as.character(step)
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
                                                             ,column_final = as.character(step)
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

#the function to bin vector and factor data
binVector <- function(initial_data_updated, interval_qty, selected_vars, gb){

  initial_intervals_summary <- data.frame(   variable = as.character()
                                            ,variable_factor = as.character()
                                            ,column_final = as.character()
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
  index <- which(column_classes %in% c("integer", "numeric", "complex", "double"))
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
  initial_intervals_summary <- data.frame(    variable = as.character()
                                             ,variable_factor = as.character()
                                             ,column_final = as.character()
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
                                                      ,column_final = column_names
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
                                                       ,column_final = column_names
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
                                                       ,column_final = column_names
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
                                                         ,column_final = column_names
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

  return(list(mapping_vector, initial_intervals_summary))
  #binned_table[, j] <<- mapping_vector
}

#calculate WOE and IV
calcWOEIV <- function(interval_summary, rounding = 4){
  #convert to data.table
  interval_summary <- as.data.table(interval_summary)
  #calculate basic values - part 1
  interval_summary[ , `:=`(  total_cum = round(cumsum(total), rounding) 
                             ,good_cum = round(cumsum(good), rounding)
                             ,bad_cum = round(cumsum(bad), rounding)
                             ,good_rate = ifelse(total == 0, 0, round(good/total, rounding))
                             ,bad_rate = ifelse(total == 0, 0, round(bad/total, rounding))
                           )
                     , by = .(variable)
                  ]
  #calculate basic values (cumulative) - part 2
  interval_summary[ , `:=`(   good_rate_cum = ifelse(total_cum == 0, 0, round(good_cum/total_cum, rounding))
                             ,bad_rate_cum = ifelse(total_cum, 0, round(bad_cum/total_cum, rounding))
                             ,good_odds = ifelse(bad == 0, 0, round(good/bad, rounding))
                             
                           )
                     , by = .(variable)
                  ]
  #calculate WOE
  interval_summary[ , `:=`(woe = ifelse(is.infinite(log(good_odds)), 0, round(log(good_odds), rounding)))
                     , by = .(variable)
                  ]
  #calculate IV per interval
  interval_summary[ , `:=`(IV = round(ifelse(is.infinite(woe * (good_rate - bad_rate)), 0, woe * (good_rate - bad_rate)), rounding))
                     , by = .(variable)
                  ]
  #calculate IV cumulative 
  interval_summary[ , `:=`(IV_cum = round(ifelse(is.infinite(woe * (good_rate - bad_rate)), 0, sum(woe * (good_rate - bad_rate))), rounding))
                     , by = .(variable)
                  ]
  
  return (interval_summary)
  
}


binPortfolioWoe <- function(binned_portfolio, interval_summary_WOE_IV ){
  
  binned_portfolio_WOE <- copy(binned_portfolio)
  column_names <- names(binned_portfolio_WOE)
  
  interval_summary <- as.data.table(interval_summary_WOE_IV)
  #binWOE factor columns of option1 (1 or 0) - paste proper WOE values
  if(sum(column_names %in% interval_summary$variable_factor) > 0){
    
    
    for(j in column_names[column_names %in% interval_summary$variable_factor]){
      #temporary data table per a column 
      interval_summary_tmp <- interval_summary[variable_factor == eval(j), ]
      #set keys
      setkeyv(interval_summary_tmp, c("interval_number"))
      setkeyv(binned_portfolio_WOE, eval(j))
      #transfer selected data to temporary var
      tmp <- binned_portfolio_WOE[, ..j][interval_summary_tmp[variable_factor == eval(j), ], eval(j) := i.woe]
      binned_portfolio_WOE[, eval(j)] <- tmp
        
    }
  } 
  #binWOE non-factor columns (option1)- paste proper WOE values
  if(sum(!(column_names %in% interval_summary$variable_factor)) > 0){
      #binWOE from variable 
      for(j in column_names[!(column_names %in% interval_summary$variable_factor)]){
        
        interval_summary_tmp <- interval_summary[variable == eval(j), ]
        setkeyv(interval_summary_tmp, c("interval_number"))
        setkeyv(binned_portfolio_WOE, eval(j))
        
        tmp <- binned_portfolio_WOE[, ..j][interval_summary_tmp[variable == eval(j), ], eval(j) := i.woe]
        binned_portfolio_WOE[, eval(j)] <- tmp     
      
      }
    
  }
    
    return(binned_portfolio_WOE)
}


calcCorrelation <- function(binned_portfolio_WOE, cut_off_cor = 0.75){

  #calculate the initial correlation matrix (with NA)
  df2 <- cor(binned_portfolio_WOE)
  print("Correlation calculated.")
  #to remove NA from the correlation matrix
  i <- 1
  for (i in 1:ncol(df2)){
    m <- is.na(df2[,i])
    index <- which(m %in% c(TRUE))
    df2[index,i] <- 0
  }
  
  df2 <- as.matrix(df2)
  #to remove zero columns (factors)
  x <- apply(df2, 2, sum)<=1
  index <- which(x %in% c(TRUE))
  df2 <- as.data.frame(df2)
  
  if (sum(x)!=0){
    df3 <- df2[-index,-index]
  }else{
    df3 <- df2
  }

  #to define factors(columns) to be removed due to cut off defined
  hc <- findCorrelation(as.matrix(df3), cutoff=cut_off_cor) # putt any value as a "cutoff"
  hc <- sort(hc)
  #return the output (variables with accepted correlation)
  if (length(hc)==0){
  
    return(df3)

  }else{
    
    return(df3[-hc,-hc])
  
  }
  
}
 
calcModel <- function(data, x_vars, y_vars, ...){
  
  #pick up the existing columns in data
  data <- as.data.frame(data)
  column_names <- names(data)
  ifelse(is.null(x_vars), column_names <- names(data), column_names <- column_names[column_names %in% x_vars])
  #compile formula string to inset into model
  y_factor <- paste(y_vars,"~", collapse="")
  #compileformula string
  formula_string <- paste(column_names, collapse = "+")
  formula_string <- paste(y_factor, formula_string)
  print(formula_string)
  #convert formula string into formula object
  z <- formula(formula_string)
  #model calculation
  fullmodel <- glm(z, family = binomial(logit), data = data)
  #save the model output to a file
  #writeLines(capture.output(summary(fullmodel)),con=full_model_to_file)
  #print (proc.time()-ptm)
  
  #to compile model.csv
  coefficients <- coef(summary(fullmodel))
  predictors <- rownames(coefficients)
  predictors[1] <- "C"
  #to derive coefficients from model summary
  coef_values <- as.data.frame(coefficients[1:nrow(coefficients),1])
  coef_values <- print(coef_values, row.names = FALSE)
  # to write coefficients to file
  model_vars <- cbind(predictors,coef_values)
  colnames(model_vars) <- c("predictor","value")
  #write.csv2(to_file,model_to_file,row.names=FALSE)
  #env$model_coefs_global<-to_file
  #print("coefs calculated")
  return(list(model_vars, coef(summary(fullmodel)), summary(fullmodel)))
  
}


calcScore <- function(binned_portfolio_WOE, good_bad){
  
  
  rowQty<-nrow(attrs)
  colQty<-ncol(attrs)
  #read model coefficients
  model<-read.csv2(model_from_file,sep=";")
  #read the couln with Good_Bad criterium
  #GB1<-read.csv2("xxx.csv",sep=";")
  
  GB<-attrs[,(colQty-1)]
  
  #vector with column names from binned sample
  field_names<-unique(colnames(attrs))
  
  #vector to transfer final data
  new_vector<-1:rowQty
  #variable to calculate tech_score
  multiplication<-0
  #calculated tech_score to transfer to file
  tech_score_final<-0
  
  #calculation and data transferare performed by vector approach - separate vectors are created and merged in the final step
  #binned vector of selected predictors
  attribute_vector<-matrix(nrow=rowQty,ncol=length(field_names)-2)
  attribute_vector<-as.data.frame(attribute_vector)
  #vector of model coeffitients
  coefficient_vector<-matrix(nrow=rowQty,ncol=length(field_names)-2)
  coefficient_vector<-as.data.frame(coefficient_vector)
  #vector of woe calculated
  woe_vector<-matrix(nrow=rowQty,ncol=(length(field_names)-2))
  woe_vector<-as.data.frame(woe_vector)
  #vector of final score result
  result_vector<-matrix(nrow=rowQty,ncol=1)
  result_vector<-as.data.frame(result_vector)
  
  dim(woe_vector)
  
  #to loop all predictors selected
  for(i in 1:(length(field_names)-2)){
    #to select the vector of the specific binned column
    field_selected<-field_names==field_names[i]
    index<-which(field_selected %in% c(TRUE))
    column_selected<-attrs[,index]
    
    #check for NA cells in the column
    index<-which(is.na(column_selected) %in% c(TRUE))
    if (is.na(index[1])){
      column_selected[index]<-0
    }
    #to make the attribute vector
    attribute_vector[,i]<-column_selected
    
    
    #to select coefficients from model
    coefficient_selected<-model[,1]==field_names[i]
    index<-which(coefficient_selected %in% c(TRUE))
    if (is.na(index[1])){
      coefficient_selected[index]<-0
    }else{
      coefficient_selected<-model[index,2]
    }
    
    coefficients<-1:length(column_selected)
    coefficients[1:length(column_selected)]<-coefficient_selected
    
    #to make coefficient vector
    coefficient_vector[,i]<-coefficients
    
    #to select WOEs for the specified predictor
    WOE_predictor<-subset(WOE,predictor==field_names[i],c(predictor,attributes,WOE))
    #print(WOE_predictor)
    
    #to define the transfer vector
    new_vector<-1:rowQty
    #check for NA cells
    if (is.na(WOE_predictor[1,1])){
      new_vector[1:rowQty]<-0
    }else{
      #to fill up the tranfer vector
      for(n in 1:length(WOE_predictor[,1])){
        check<-column_selected==as.vector(WOE_predictor[n,2])
        index<-which(check %in% c(TRUE))
        new_vector[index]<-as.vector(WOE_predictor[n,3])
        
      }
    }
    
    woe_vector[,i]<-new_vector
    
    if (field_names[i]!="C"){
      multiplication<-coefficients*new_vector
    }else{
      multiplication<-coefficients
    }
    #to make the score result vector
    tech_score_final<-tech_score_final+multiplication
  }
  
  #to add Intercept vector
  c<-unlist(subset(model,predictor=="C"))[2]
  c_final<-1:rowQty
  c_final[1:rowQty]<-c
  
  tech_score_final<-round(tech_score_final+c_final,10)
  result_vector<-tech_score_final
  
  #to make the final sample to record into file
  to_file<-cbind(attribute_vector,coefficient_vector,woe_vector, c_final,tech_score_final,GB)
  #names of predictors binned
  column_names<-field_names[1:(length(field_names)-2)]
  #names of predictors model coefficients
  x<-paste(field_names[1:(length(field_names)-2)],"_coef",sep="")
  #names of predictors woes
  y<-paste(field_names[1:(length(field_names)-2)],"_woe",sep="")
  #to make the finale vector of field names
  column_names<-c(column_names,x,y,"C_intercept","tech_score_final","GB")
  #to add names to
  colnames(to_file)<-c(column_names)
  #to record the final tech score results
  #write.csv2(to_file,tech_score_to_file,row.names=FALSE)
  if (memory_boost_in==FALSE){
    #fwrite_score(to_file,tech_score_to_file)
    #fast_write(to_file,tech_score_to_file,sep=";",header=TRUE)
    write.csv2(to_file,tech_score_to_file,row.names=FALSE)
    print("tech. score saved to file.")
    print (proc.time()-ptm)
  }else{
    env$tech_score_global<-to_file
    print("tech. score saved to RAM.")
    print (proc.time()-ptm)
  }
  
  
  
  
}



#rm(list = ls())
#gc()

