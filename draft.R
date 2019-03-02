library(data.table)
library(stringr)
library(smbinning)
library(caret)

#install.packages("smbinning")
#data(chileancredit)
#old_dir <- getwd()
#new_dir <- "D:\\Demyd\\Personal\\R"
#setwd(new_dir)

file_path <- 'D:\\Demyd\\Personal\\R\\kaggle\\'
file_path2 <- 'C:\\Users\\Demyd_Dzyuban\\Documents\\Demyd\\Personal\\Kaggle\\'
file_name <- 'application_train.csv'

data <- fread(paste(file_path, file_name, sep=""))
classSummary <- readColNamesClasses(data)
sum(classSummary[,2] == 'factor')
vars_to_convert <- c('NAME_CONTRACT_TYPE', 'CODE_GENDER', 'FLAG_OWN_CAR')
data_type <- c('factor', 'factor', 'factor')
data_converted <- convertToDataType(data, vars_to_convert, data_type)

#read column names and their classes  
#vars <- readColNamesClasses(data_converted)

#readColNamesClasses(data_converted)


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

#names(chileancredit)[1:3]

#initial_data <- as.data.table(chileancredit)
initial_data <- data
#qty of attributes to bin
interval_qty <- 20
#the length of the predictor for binning
column_length <- dim(initial_data)[1]
#GOOD/BAD column name
good_bad <- "TARGET"
#GOOD/BAD vector
gb <- as.vector(unlist(initial_data[, ..good_bad]))
#selected variables to bin
selected_vars <- selectVars(initial_data, good_bad, all.columns = TRUE)
                            
selected_vars <- selectVars(initial_data, good_bad,
                            c("TARGET 
                               SK_ID_CURR 
                               NAME_CONTRACT_TYPE 
                               CODE_GENDER
                               FLAG_OWN_CAR 
                               AMT_REQ_CREDIT_BUREAU_DAY 
                               AMT_REQ_CREDIT_BUREAU_WEEK 
                               AMT_REQ_CREDIT_BUREAU_MON 
                               AMT_REQ_CREDIT_BUREAU_QRT 
                               AMT_REQ_CREDIT_BUREAU_YEAR"
                             )
                          )

#check the selected vars with existing col names
selected_vars <- names(data)[names(data) %in% selected_vars]

#data set to be processed
initial_data_updated <- as.data.table(data[ , ..selected_vars])

#processed factor table (ready for binning as a vector)
binned_factor_table <- binFactor(initial_data_updated, selected_vars, factor_type = 3, gb = gb)
binned_vectors <- binVector(initial_data_updated, interval_qty, selected_vars, gb)

summary_and_binned_portfolio <- binPortfolioAndSummary(binned_factor_table, binned_vectors)

debugonce(calcDescStat)
data_converted <- convertToDataType(data, 'NAME_CONTRACT_TYPE', 'factor')
descriptiveStatistics <- calcDescStat(data_converted, selected_vars)

#add WOE and IV values to interval summary
interval_summary_WOE_IV <- calcWOEIV(summary_and_binned_portfolio[[1]])

#interval_summary_WOE_IV[interval_summary_WOE_IV$variable == 'AMT_REQ_CREDIT_BUREAU_YEAR']
 
#bin portfolio with WOE values
binned_portfolio_WOE <- binPortfolioWoe(summary_and_binned_portfolio[[2]], interval_summary_WOE_IV)
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
#calculate score
scored_portfolio <- calcScore(binned_portfolio_WOE, interval_summary_WOE_IV, modelOutput, selected_vars, good_bad)  
#make score distribution over intervals
scoreDist <- binVector(scored_portfolio, interval_qty, "score", gb)
#add WOE and IV values to interval summary
scoreDistSummary <- calcWOEIV(scoreDist[[2]])
#Gini calculation
debugonce(calcGini)
gini_square <- calcGini(scoreDistSummary)
gini <- 1 - sum(gini_square$gini_square)

gini_square$good

############################################################################################################
#select the necessary variable and reduce the data table
selectVars <- function( initial_data
                       ,good_bad
                       ,column_names = NULL
                       ,all.columns = FALSE
                      ){
  
  if(all.columns == TRUE){
    column_names <- readColNamesClasses(initial_data)
    x_var <- column_names$column_names
  } else {
  
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
  }
  
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
      
      index_not_na <- which(!is.na(vector_to_be_binned))
      index_total <- which(vector_to_be_binned[index_not_na] < actual_vector_intervals[2, i])
      
      mapping_vector[index_total] <- i
      
      total <- length(index_total)
      good <- sum(gb[index_not_na][index_total] == 1)
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
      
      index_not_na <- which(!is.na(vector_to_be_binned))
      index_total <- which(vector_to_be_binned[index_not_na] >= actual_vector_intervals[1, actual_vector_intervals_qty - 1])
      
      mapping_vector[index_total] <- i
      
      total <- length(index_total)
      good <- sum(gb[index_not_na][index_total] == 1)
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
      
      index_not_na <- which(!is.na(vector_to_be_binned))
      index_total <- which(vector_to_be_binned[!is.na(vector_to_be_binned)] >= actual_vector_intervals[1, i] & vector_to_be_binned[!is.na(vector_to_be_binned)] < actual_vector_intervals[2, i])
      
      mapping_vector[index_total] <- i
      
      total <- length(index_total)
      good <- sum(gb[index_not_na][index_total] == 1)
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
      
      total <- length(is.na(vector_to_be_binned))
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
  #browser()
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
  interval_summary[ , `:=`(    good_rate_cum = ifelse(total_cum == 0, 0, round(good_cum/max(good_cum), rounding))
                              ,bad_rate_cum = ifelse(total_cum == 0, 0, round(bad_cum/max(bad_cum), rounding))
                              ,good_odds = ifelse(bad == 0, 0, round(good_rate/bad_rate, rounding))
                              
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
  
  return(list(model_vars, coef(summary(fullmodel)), summary(fullmodel)))
  
}


calcScore <- function(data, summaryWOE, modelOutput, x_vars, good_bad){
  
  #browser()
  #pick up the existing columns in data  
  ifelse(is.null(x_vars), column_names <- names(data), column_names <- names(data)[names(data) %in% x_vars])
  #pick up the data table with coefficients per variable
  model <- as.data.table(modelOutput[[1]])
  #score calculation for all variables selected
  for(j in column_names){
    #pick up WOE extract per variable 
    woe <- summaryWOE[column_final == j][ , .(column_final, interval_number, woe)]
    #FOR loop to rush through all woe intervals per variable selected 
    for(i in woe$interval_number){
      #pick up model coefficient per variable
      model_selected <- model[model$predictor == j]$value[i]
      #selection vector for IF statement
      select <- data[, ..j] == i 
      #if any items are in selection -> perform calculation (WOE & var coefficient) per variable
      if(sum(select) != 0){
        
        data[select, ..j] <- woe$woe[i] * model_selected
        #print(woe$woe[i] * model_selected)        
      }
      
    }
    
  }
  #pick up intercept 
  C <- model[model$predictor == 'C']$value
  #calculate total score per row
  score <- apply(data, 1, sum)
  #add intercept
  score <- score + C
  #add score to the final output
  data <- cbind(data, score)
  
  return (data) 
  
}


calcGini <- function(scoreDistSummary, rounding = 10){
  #browser()
  #make initial values for gini calculation
  scoreDistSummary[ ,`:=`(KS_diff = bad_rate_cum - good_rate_cum)
                 ]
  
  #to make vector of Bads diff (Bi-B(i-1))
  BS_start = scoreDistSummary$bad_rate_cum[1:length(scoreDistSummary$bad_rate_cum) - 1]
  BS_end = scoreDistSummary$bad_rate_cum[2:length(scoreDistSummary$bad_rate_cum)]
  
  #to make vector of Goods diff (Gi-G(i-1))
  GS_start = scoreDistSummary$good_rate_cum[1:length(scoreDistSummary$good_rate_cum) - 1]
  GS_end = scoreDistSummary$good_rate_cum[2:length(scoreDistSummary$good_rate_cum)]
  
  #BS and GS total
  scoreDistSummary[, `:=`(    BS_final = c(bad_rate_cum[1], BS_end - BS_start) 
                             ,GS_final = c(good_rate_cum[1], GS_end + GS_start)
  )
  ]
  
  #Square GINI calculation
  scoreDistSummary[, `:=`(gini_square = BS_final * GS_final) ]  
  
  return(scoreDistSummary)
  
}


readColNamesClasses <- function(data){
  
  column_classes <- sapply(data, class)
  column_names <- names(data)
  
  return(as.data.table(cbind(column_names, column_classes)))
  
}

data_type <- "factor"

convertToDataType <- function(data, vars_to_convert, data_type){
  
  #read column names and their classes  
  vars <- readColNamesClasses(data)
  #check whether we need to perfrom further stepas
  data_type <- data_type[vars_to_convert %in% vars$column_names]
  vars_to_convert <- vars_to_convert[vars_to_convert %in% vars$column_names]
  
  
  if (length(vars_to_convert) != 0){
    
    for (col in vars_to_convert){
      
      check <- vars_to_convert %in% col
      
      if(data_type[check] == 'factor'){
        #conversion (it is taken from https://stackoverflow.com/questions/16943939/elegantly-assigning-multiple-columns-in-data-table-with-lapply/33000778#33000778)
        data[, (col) := lapply(col, function(x) {as.factor(data[[x]])})]
      }
      
      if(data_type[check] == 'character'){
        data[, (selection) := lapply(col, function(x) {as.character(data[[x]])})]
      }
      
      if(data_type[check] == 'integer'){
        data[, (col) := lapply(col, function(x) {as.integer(data[[x]])})]
      }    
      
      if(data_type[check] == 'numeric'){
        data[, (col) := lapply(col, function(x) {as.numeric(data[[x]])})]
      }
      
      if(data_type[check] == 'date'){
        data[, (col) := lapply(col, function(x) {as.Date(data[[x]])})]
      }  
      
    }
  }
  return (data)
}

binPortfolioAndSummary <- function(binned_factor_table, binned_vector_table){
  
  #overall interval summary
  interval_summary <- rbind(binned_factor_table[[2]], binned_vectors[[2]])
  
  
  #overall binned portfolio
  if(nrow(binned_factor_table[[1]]) != 0 & nrow(binned_vectors[[1]]) != 0){
    
    binned_portfolio <- cbind(binned_factor_table[[1]], binned_vectors[[1]]) 
    
  }
  
  if (nrow(binned_factor_table[[1]]) == 0){
    
    binned_portfolio <- binned_vectors[[1]] 
    
  }
  
  if (nrow(binned_vectors[[1]]) == 0){
    
    binned_portfolio <- binned_factor_table[[1]] 
    
  } 
  
  
  return(list(interval_summary, binned_portfolio))
  
}


calcDescStat <- function(data, selected_vars, rounding = 5){
 
  #vector of column names[index]
  column_names <- names(data)
  if (is.null(selected_vars)) selected_vars <- column_names
  column_names <- column_names[column_names %in% selected_vars]
  col <- column_names[1]

  #data table to store the statistic output  
  statSummary <- data.table( variable = NA_character_
                            ,data_type = NA_character_
                            ,qty_total = NA_integer_
                            ,qty_NA = NA_integer_
                            ,qty_level = NA_integer_
                            ,factor_levels = NA_character_
                            #,quants = NA_integer_
                            ,minVal = NA_integer_
                            ,firstQuantile = NA_integer_
                            ,medianVal = NA_integer_
                            ,meanVal = NA_integer_
                            ,modeVal = NA_integer_
                            ,thirdQuantile = NA_integer_
                            ,maxVal = NA_integer_
                            ,stdDev = NA_integer_
                           )
  
  #loop all integer, numeric columns to collect descriptive statistics
  for(col in column_names){
    print(col)
    classVal <- class(unlist(data[, ..col]))
    #check data for factor and character classes
    if (classVal %in% c('character', 'factor')){
      #the vector to process
      vector_desc <- unlist(data[, ..col])
      #qty of records 
      qty_total <- length(vector_desc)
      #qty of NA items
      qty_NA <- sum(is.na(vector_desc))
      #level qty
      qty_level <- ifelse(classVal == 'factor', length(levels(vector_desc)), 0)
      factor_levels <- ifelse(classVal == 'factor', length(levels(vector_desc)), NA)
      #quantiles (median is the upper limit of the 2nd quantile)
      quants <- NA 
      
      #descriptive statistics
      minVal <- NA
      firstQuantile <- NA
      medianVal <- NA
      meanVal <- NA
      
      ux <- NA
      modeVal <- NA
      
      thirdQuantile <- NA
      maxVal <- NA 
      stdDev <- NA
      
      #collect the output per each column
      row <- data.frame( variable = col
                        ,data_type = classVal
                        ,qty_total
                        ,qty_NA
                        ,qty_level
                        ,factor_levels
                        #,quants
                        ,minVal
                        ,firstQuantile
                        ,medianVal
                        ,meanVal
                        ,modeVal
                        ,thirdQuantile
                        ,maxVal
                        ,stdDev
                       )
      statSummary <- rbind(statSummary, row)
    }  
    #check data for 'integer', 'numeric', 'float' classes  
    if (classVal %in% c('integer', 'numeric', 'float')){
      #the vector to process
      vector_desc <- unlist(data[, ..col])
      #qty of records 
      qty_total <- length(vector_desc)
      #qty of NA items
      qty_NA <- sum(is.na(vector_desc))
      #qty of levels
      qty_level <- 0
      factor_levels <- NA
      #quantiles (median is the upper limit of the 2nd quantile)
      quants <- quantile(vector_desc, probs = c(0, 0.25, 0.50, .75, 1), na.rm = TRUE)
      
      #descriptive statistics
      minVal <- round(min(vector_desc, na.rm = TRUE), rounding)
      firstQuantile <- round(quants[2], rounding)
      medianVal <- round(median(vector_desc, na.rm = TRUE), rounding)
      meanVal <- round(mean(vector_desc, na.rm = TRUE), rounding)
      
      ux <- unique(vector_desc[!is.na(vector_desc)])
      modeVal <- round(ux[which.max(tabulate(match(vector_desc[!is.na(vector_desc)], ux)))], rounding)
      
      thirdQuantile <- round(quants[4], rounding)
      maxVal <- round(max(vector_desc, na.rm = TRUE), rounding) 
      stdDev <- round(sd(vector_desc, na.rm = TRUE), rounding)
      
      #collect the output per each column
      row <- data.frame( variable = col
                        ,data_type = classVal
                        ,qty_total
                        ,qty_NA
                        ,qty_level
                        ,factor_levels
                        #,quants
                        ,minVal
                        ,firstQuantile
                        ,medianVal
                        ,meanVal
                        ,modeVal
                        ,thirdQuantile
                        ,maxVal
                        ,stdDev
                       )
      
      statSummary <- rbind(statSummary, row)
  
    }
        
  }
  
  return(statSummary)  
  
}


scaleData <- function(data, ...){
  
  scale(data, ... )
  
}


scaleData(x, center = FALSE, scale = TRUE)



summary(data)

#rm(list = ls())
#gc()