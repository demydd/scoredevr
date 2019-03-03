# create vars for NaN observations
create_nan_vars = function(tr_te, start_pos){
  col_num = ncol(tr_te)
  for (i in start_pos:col_num) {
    colname = names(tr_te)[i]
    tr_te[is.na(eval(as.name(colname)))|is.nan(eval(as.name(colname)))|is.null(eval(as.name(colname)))|is.infinite(eval(as.name(colname))), 
          paste0(colname, '_nulls') := 1]
    tr_te[is.na(eval(as.name(paste0(colname, '_nulls')))), paste0(colname, '_nulls') := 0]
  }
  return(tr_te)
}

# outliers marking
outliers_remove = function(dt,col_from,col_to) {
  for (i in col_from:col_to) {
    colname = names(dt)[i]
    qnt <- quantile(dt[,eval(as.name(colname))], probs=c(.25, .75), na.rm = T)
    H <- 1.5 * (qnt[2]-qnt[1])
    dt[eval(as.name(colname)) < (qnt[1] - H), paste0(colname, '_outliers') := -1]
    dt[eval(as.name(colname)) > (qnt[2] + H), paste0(colname, '_outliers') := 1]
    dt[is.na(eval(as.name(paste0(colname, '_outliers')))), paste0(colname, '_outliers') := 0]
  }
  return(as.data.table(dt))
}

create_factor_by_model = function(tr_te, y, vect_fla, list_params){
# Examples of input:
# vect_fla = c('y ~ CNT_PAYMENT_max + NAME_CONTRACT_STATUS_sum.y', 
#              'y ~ CREDIT_ACTIVE_BOOL_sum + DAYS_CREDIT_mean'
#             )
# list_params = list(c('CNT_PAYMENT_max', 'NAME_CONTRACT_STATUS_sum.y'), 
#                    c('CREDIT_ACTIVE_BOOL_sum', 'DAYS_CREDIT_mean')
#                   )
  for (i in 1:length(vect_fla)) {
    fla = vect_fla[i]
    params = list_params[[i]]
    # apply model
    dt_mod = as.data.table(cbind(y, tr_te[1:length(y), params, with = FALSE]))
    mod = lm(data=dt_mod, formula=as.formula(fla)) #to do: add random model here
    tr_te[, paste0('newcol','_', sub('y ~ ', '', fla)) := predict(mod, tr_te[, params, with = FALSE])]
  }
  return(tr_te)
}

load_chunk_data = function(n, data_dir, temp_names){
# n = number of parts to split
  for (i in 1:n) {
    cat("Loading ", i, "th part.\n", sep = "")
    train_data_temp <- fread(input = paste0(data_dir, "//Calculation//input_bigmatrix.csv"),
                             select = (1+round((i-1)*nrow(temp_names)/n, 0)):round(i*nrow(temp_names)/n, 0),
                             header = TRUE,
                             sep = ",",
                             stringsAsFactors = FALSE,
                             colClasses = rep("numeric", nrow(temp_names)),
                             data.table = TRUE)
    
    gc(verbose = FALSE)
    if (i > 1) {
      cat("Coercing to matrix.\n", sep = "")
      tr_te_temp <- as.matrix(train_data_temp)
      rm(train_data_temp)
      gc(verbose = FALSE)
      cat("Coercing into dgCMatrix with NA as blank.\n", sep = "")
      tr_te_temp <- dropNA(tr_te_temp)
      gc(verbose = FALSE)
      cat("Column binding the full matrix with the newly created matrix.\n", sep = "")
      tr_te <- cbind(tr_te, tr_te_temp)
      rm(tr_te_temp)
      gc(verbose = FALSE)
    } else {
      cat("Coercing to matrix.\n", sep = "")
      tr_te_temp <- as.matrix(train_data_temp)
      rm(train_data_temp)
      gc(verbose = FALSE)
      cat("Coercing into dgCMatrix with NA as blank.\n", sep = "")
      tr_te <- dropNA(tr_te_temp)
      gc(verbose = FALSE)
    }
  }
  return(tr_te)
}

replace_commas_cirilic = function(train_sample){
  indx <- sapply(train_sample, is.factor)
  cirillic_letters = c('é','ö','ó','ê','å','í','ã','ø','ù','ç','õ','ú','ô','û','â','à','ï','ð','î','ë','ä','æ','ý','ÿ','÷','ñ','ì','è','ò','ü','á','þ','¸')
  train_sample[indx] <- lapply(train_sample[indx], function(x) ifelse(grepl(paste(cirillic_letters, collapse = "|"), x), x, as.numeric(sub(",", ".", as.character(x), fixed = TRUE))))
  return(train_sample)
}

dummy_transform = function(overall_sample) {
  #Transform text data in dummy variables
  nums = sapply(overall_sample, is.numeric)
  not_nums = overall_sample[, !nums]
  dummy_transform=dummyVars(~. , data = not_nums)
  dummy_data = predict(dummy_transform, not_nums)
  #make a training data set
  overall_sample_num = data.frame(overall_sample[,c(1,ncol(overall_sample))], overall_sample[, nums][, c(-1,-ncol(overall_sample[, nums]))], dummy_data)
  return(overall_sample_num)
}

replace_nan = function(overall_sample_num) {
#make data replacement (for NA, NAN, NULL, INF elements)
overall_sample_mutated <- overall_sample_num %>% 
  mutate_if(is.numeric, 
            .funs = funs(
              ifelse(is.na(.)|is.nan(.)|is.null(.)|is.infinite(.), 
                     median(., na.rm = TRUE),
                     .))) %>%
  mutate_if(is.character, 
            .funs = funs(
              ifelse(is.na(.)|is.nan(.)|is.null(.)|is.infinite(.), 
                     my_mode(.),
                     .)))
return(overall_sample_mutated)
}

# outliers removing (non-standard approach)
outliers_remove = function(x,col_from,col_to) {
  for (i in col_from:col_to) {
    qnt <- quantile(x[i], probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(x[,i], na.rm = T)
    x[i][x[i] < (qnt[1] - H)] <- (qnt[1] - H)
    x[i][x[i] > (qnt[2] + H)] <- (qnt[2] + H)
  }
  return(x)
}

scale_custom = function(overall_sample_outliers_remove, start){
#scaling of input data
overall_sample_nan = as.data.frame(apply(overall_sample_outliers_remove[,start:ncol(overall_sample_outliers_remove)],  2, function (x) (x - mean(x)) / sd(x)))
return(overall_sample_nan)
}

cluster_determine = function(test_sample_outliers_remove) {
  # Determine number of clusters
  # wss[1] <- (nrow(test_sample_outliers_remove)-1)*sum(apply(test_sample_outliers_remove[,-c(1,2,ncol(test_sample_outliers_remove))],2,var))
  # http://www.bagualu.net/wordpress/wp-content/uploads/2015/10/A_Handbook_of_Statistical_Analyses_Using_R__Second_Edition.pdf
  # page 329
  wss = rep(0,15)
  for (i in 1:15) wss[i] <- sum(kmeans(test_sample_outliers_remove[,-c(1,2,ncol(test_sample_outliers_remove))],
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  
  #choose the optimal cluster number (when margin weighted squares effect higher than margin growth of cluster count)
  for (i in 1:length(wss)) {
    if (-diff(wss, lag = 1)[i]/max(wss) > 1/length(wss)) {
      count_claster = i+1;
      print(c(count_claster, wss_result,diff(wss, lag = 1)[i]/max(wss)));
    }
    else {break}
  }
  return(count_claster) #return evaluated optimal count of clusters
}