# fns_glam.R

# Purpose: Various functions for the main earth-glm runs


# Functions in this script:

# fn_extractModelData
# fn_downsample
# fn_earthReduceSize
# fn_modelMatrix
# fn_score

# devtools::use_package("caret")


#--------------------------------------------------------------------------------
#' Create dataset to store predictions (if it does not already exist)
#' 
#' Create dataset (dt_preds) to store predictions (if it does not already exist)
#' an index into dt_all that is 1:1 into dt_preds is also created
#' (e.g. prefix is m1 and both m1a, m1b = freq-sev are stored in it).
#' Variable names in the file need to allow for modelname + varname
#' dt_preds contains all data satisfying the where statement.
#' Not all this data need be used for training.  In particular, earth
#' tends to have a maximum of 2m records and even glmnet training
#' might not use all these records.  Therefore n_train should not be 
#' defined here.  Rather we will define n_train as the number of records
#' used in the glm.
#'  
#' @export
#' 
#' @param model_prefix model prefix - included  defines name of stored RData file
#'                     eg '04a_dt_preds_m1'.RData'
#' @param fname_data filename of the RData file where dt_all is stored
#' @param fname_vars filename of the RData file where the various vars_ 
#'    variables are stored; i.e. vars_uniqueID, vars_weightAll, vars_fold,
#'    vars_neverToUse, vars_indNotToUse, vars_targetAll, vars_missingInds
#' @param whereStatement a statement that returns TRUE / FALSE for 
#'    each example, using any columns in dt_all.  For example:
#'    lossLimit >= 225000
#' 
#' @return dt_
#' 
fn_create_dt_preds <- function(model_prefix,
                               fname_dt_all,
                               fname_vars,
                               whereStatement,
                               verbose = FALSE){

  if(verbose) {print('fn_create_dt_preds: start')}
  
  if(!(file.exists(file.path(dirRData, 
                             paste0('04a_dt_preds_', model_prefix, '.RData'))))
  ){
    if (verbose){print("Creating dt_preds")}
    load(file = file.path(dirRData, paste0(fname_dt_all, '.RData')))
    load(file = file.path(dirRData, paste0(fname_vars, '.RData')))
      
    dt_preds <- dt_all[, unique(c(vars_uniqueID,
                                  vars_fold,
                                  vars_targetAll,
                                  vars_weightAll)),
                       with = FALSE]
    idx_allpreds <- NULL
    if(!is.null(whereStatement)){
      idx_allpreds <- which(dt_preds[, eval(parse(text = whereStatement))])
      dt_preds <- dt_preds[idx_allpreds]
    }
    
    save(dt_preds, idx_allpreds, 
         file = file.path(dirRData, 
                          paste0('04a_dt_preds_', model_prefix, '.RData'))
         )
    
    rm(dt_all, dt_preds); gc()
  } else {
    if (verbose){print("dt_preds already exists")}
  }
  if(verbose) {print('fn_create_dt_preds: end')}
  
  }


#--------------------------------------------------------------------------------
#' Extract data for modelling or scoring
#' 
#' Extracts data needed for modelling, sampling the rows and limiting columns
#' as required.  Saves details of variables used to paste0(model_ref ,'_varsUsed.RData')
#'  
#' @export
#' 
#' @param fname_data filename of the RData file where dt_all is stored
#' @param fname_vars filename of the RData file where the various vars_ 
#'    variables are stored; i.e. vars_uniqueID, vars_weightAll, vars_fold,
#'    vars_neverToUse, vars_indNotToUse, vars_targetAll, vars_missingInds
#' @param vars_notToUseAdditional any additional variables not to use besides
#'    those defined in: vars_neverToUse, vars_indNotToUse
#' @param folds folds to extract.  If 'all' then all
#' @param n_max (e.g. 1:7, 'all') maximum number of examples.  
#'    Bites if n_max is less than the number of examples in folds.
#'    Expected use is to limit size of training data when doing mars.
#'    If want to use all folds set n_max = 'all'
#' @param whereStatement a statement that returns TRUE / FALSE for 
#'    each example, using any columns in dt_all.  For example:
#'    lossLimit >= 225000
#' 
#' @return dt_
#' 
fn_extractModelData <- 
    function(fname_data, 
             fname_vars, 
             model_ref,
             vars_notToUseAdditional = NULL,
             folds, 
             n_max, 
             whereStatement){

    # load dt_all
    load(file = file.path(dirRData, paste0(fname_data, '.RData')))
    cnames_dt_all <- colnames(dt_all)

    # load vars: vars_weight, vars_fold, vars_neverToUse, vars_indNotToUse
    # vars_targetAll
    load(file = file.path(dirRData, paste0(fname_vars, '.RData')))
    
    vars_indNotToUse <- c(vars_indNotToUse, vars_notToUseAdditional)
    
    # independent variables to use
    vars_indToUse <- 
        setdiff(colnames(dt_all), c(vars_targetAll, 
                                    vars_weightAll, 
                                    vars_fold,
                                    vars_neverToUse,
                                    vars_indNotToUse))
    
    vars_toUseAll <- c(vars_indToUse, 
                       vars_targetAll,
                       vars_weightAll,
                       vars_fold)
    
    # identify numeric variables
    vars_indToUseNumeric <-
        vars_indToUse[sapply(dt_all[1, vars_indToUse, with = FALSE], is.numeric)]
    
    # identify non numeric variables 
    vars_indToUseNonNumeric <- 
        setdiff(vars_indToUse, c(vars_indToUseNumeric, vars_missingInds))
    
    vars_toDelete <- setdiff(cnames_dt_all, vars_toUseAll)
    
    # save variable choices for future reference
    fn_resave(
      vars_indToUse, 
      vars_indToUseNumeric, 
      vars_indToUseNonNumeric, 
      file = file.path(dirRData, paste0(model_ref ,'_varsUsed.RData')))
    
    if (identical(folds, 'all')){
      idx_ <- 1:nrow(dt_all)
    } else {
      idx_ <- which(dt_all$fold %in% folds)
    }
    
    if (!is.null(n_max)){
        if (length(idx_) > n_max){
            idx_ <- idx_train[sample(length(idx_), n_max)]
        }  
    }
  
    dt_ <- dt_all[idx_]
    
    rm(dt_all); gc()
    
    "R code for slice of data to include (ie where statement)"
    if(!is.null(whereStatement)){
        dt_ <- dt_[eval(parse(text = whereStatement))]
    }
    
    dt_[, (vars_toDelete) := NULL]
    
    return(dt_)
    
    }


#--------------------------------------------------------------------------------
#' Downsample a data table returning selected columns only
#'
#' Downsample a data table returning selected columns only.  The targetVar
#' parameter defines the variable which will be used for downsampling.
#' targetVar is assumed to be a binomial 0-1 response or an integer (count)
#' which is 0 or greater.  If the latter, all responses > 0 are grouped to
#' form class 1 and the response is then treated as 0-1. 
#'
#' @export
#'
#' @param var_target the target variable.  The target variable must be 0-1
#' @param vars_toUse a vector of variables to use
#'    (excluding the missing indicators)
#' @param vars_toAdd variables to add to varsToUse, typically the
#'    missingInds - the missing indicators
#' @param dt_ a (training) data table to downsample
#' @param method one of (caret - now removed,) balanced, imbalanced, reduce.
#'    'balanced' is used to downsample the majority class 
#'    to the minority class.  'imbalanced' downsamples but allows the 
#'    the majority class to contain up to upsampleMult time as many examples
#'    as the minority class. 'reduce' reduces both
#'    classes in the same proportion leaving n_max observations.
#' @param n_max the maximum number of examples left by 'reduce' or 
#'    'imbalanced' downsampling
#' @param upsampleMult maxmimum size of major class as multiple of minor class
#' @param seed seed set random before sample is used
#' 
#' @return a downsampled dataset with targetVarFac, a 0-1 factor which is 0 
#'    when targetVar == 0 and 1 where targetVar > 0 
#'
fn_downsample <- function(var_target, vars_toUse, vars_toAdd,
                          dt_,
                          method,
                          n_max,
                          upsampleMult = 50,
                          seed = 2018){

  # caret method removed after checking output from balanced and speed
  # tests.  'balanced' does the same as caret with slightly different
  # code.  It does not check for matrices etc and assumes dt because this function is 
  # written to be used with mlslib only.  caret uses dplyr and does not return
  # a data table.
  if (!(method %in% c(#'caret', 
                      'balanced',
                      'imbalanced',
                      'reduce'))){
    stop('fn_downsample: method must be one of balanced, imbalanced, reduce')
  }

  set.seed(2018)
  vars_toUse <- c(vars_toUse, vars_toAdd)
  
  x = copy(dt_[, vars_toUse, with = FALSE])
  
  targetVarFac <- 
      ifelse(dt_[,var_target, with = FALSE] > 0, 1, 0) %>%
      as.factor()

  switch(method,
         
         # caret = {
           # for speed test vs balanced
             # dt_ds <- as.data.table(
             #   caret::downSample(x = x,
             #                     y = targetVarFac)
             # )
             # setnames(dt_ds, "Class", "targetVarFac")
         # },
         balanced = {
           # If want same number as min class
             table_targetVarFac <- unname(table(targetVarFac))
             # minClass is 0 or 1 (positions in table_targetVarFac being 1 or 2)
             minClass <- which.min(table_targetVarFac) - 1
             n_minClass <- table_targetVarFac[minClass + 1]
             
             x[, targetVarFac := targetVarFac]
             
             idx_0 <- which(targetVarFac == 0)
             idx_1 <- which(targetVarFac == 1)
             
             if (minClass == 0){
                 idx_ <- c(idx_0, sample(idx_1, size = n_minClass))
             } else {
                 idx_ <- c(sample(idx_0, size = n_minClass), idx_1)
             }
             dt_ds <- x[idx_]
         },
         imbalanced = {
             if (is.null(n_max)) n_max <- nrow(dt_)
             
             table_targetVarFac <- unname(table(targetVarFac))
             # minClass is 0 or 1 (positions in table_targetVarFac being 1 or 2)
             minClass <- which.min(table_targetVarFac) - 1
             majClass <- 1 - minClass
             n_minClass <- table_targetVarFac[minClass + 1]
             n_majClass <- table_targetVarFac[majClass + 1]
             
             if (n_max <= n_minClass) stop(
                 'fn_downsample: In imbalanced downsampling, n_max must be 
                  more than the number of examples in the minimum class')
             
             n_majClassSample <- min(upsampleMult * n_minClass, 
                                     n_majClass,
                                     n_max - n_minClass)

             x[, targetVarFac := targetVarFac]
             
             idx_0 <- which(targetVarFac == 0)
             idx_1 <- which(targetVarFac == 1)
             
             if (minClass == 0){
                 idx_ <- c(idx_0, sample(idx_1, size = n_majClassSample))
             } else {
                 idx_ <- c(sample(idx_0, size = n_majClassSample), idx_1)
             }
             dt_ds <- x[idx_]
         },
         reduce = {
           # Simply downsample because dataset is big
           if (is.null(n_max)) stop(
               'fn_downsample: for reduce downsampling, n_max must be set')
             
           x[, targetVarFac := targetVarFac]
           if(n_max >= nrow(x)){
               dt_ds <- x
           } else {
               dt_ds <- x[sample(.N, min(nrow(x), n_max))]
           }
         }
         )

  return(dt_ds)

}


#--------------------------------------------------------------------------------
#' Reduce size of earth object
#' 
#' Reduce size of earth object by dropping the slots we do not need, especially
#' the bx (we will create our own on the full data) and any predictions etc
#' 
#' @export
#' 
#' @param x an earth object
#' 
#' @return a reduced size earth object
#' 
fn_earthReduceSize <- function(x){
    for (idx_name in names(x)) {
        if(idx_name %in% c('bx', 'prune.terms', 'fitted.values',
                           'residuals', 'leverages', 'weights'
                           )) 
            x[[idx_name]] <- NULL
    }
    return(x)
    
}


#--------------------------------------------------------------------------------
#' Create model matrix
#' 
#' Create model matrix based on saved earth object and categorical
#' variables chosen by IV
#' 
#' @export
#' 
#' @param model_ref the model reference
#' @param dt_ the data table from which to create the model matrix
#' @param bln_create_checkncols (TRUE or FALSE) save the number of columns
#'        in the model matrix from numerics and from categoricals
#' @param bln_test_ncols (TRUE or FALSE) test the number of columns
#'        in the model matrix based on previously expected number
#'  
#' @return a reduced size earth object
#' 
fn_modelMatrix <- function(model_ref,
                           dt_,
                           bln_create_checkncols,
                           bln_test_ncols){
  
  # load the variable  definitions:
  # var_target, var_weight,
  # vars_indToUse, 
  # vars_indToUseNumeric, vars_indToUseNonNumeric, vars_missingInds,
  # filteredNames
  load(file = file.path(dirRData, paste0(model_ref, '_varsUsed.RData')))
  
  # load the earth object: earth_all
  load(file = file.path(dirRData,
                        paste0(model_ref, '_', 'earth_all.RData')))
  
  mars_ <- 
    model.matrix(earth_all,  
                 x = dt_[, 
                         c(vars_indToUseNumeric, vars_missingInds),
                         with = FALSE])[,-1] 
  
  # We need to figure out how there can be duplicated colums and then
  # stop that from happening before here
  duplicated_columns <- duplicated(t(as.matrix(mars_)))
  mars_ <- mars_[, !duplicated_columns]
  
  mars_ <- Matrix(mars_, sparse = TRUE)
  
  # Categorical variables: Dummy coding
  cat_ <- model.Matrix(~.,
                       data = dt_[, filteredNames, with = FALSE],
                       sparse = TRUE,
                       drop.unused.levels = FALSE)[,-1, drop = FALSE]
  
  if (bln_create_checkncols){
    # initialise check ncols
    lst_checkncols <- list()
    lst_checkncols$ncol_mars <- ncol(mars_)
    lst_checkncols$ncol_cat <- ncol(cat_)
    save(lst_checkncols,
         file =  file.path(dirRData, paste0(model_ref, '_', 'lst_checkncols.RData')))
  }
  
  if (bln_test_ncols){
    load(file =  file.path(dirRData, paste0(model_ref, '_', 'lst_checkncols.RData')))
    if (ncol(mars_) != lst_checkncols$ncol_mars){
      stop("fn_modelMatrix: number of columns from mars is not as expected")
    }
    if (ncol(cat_) != lst_checkncols$ncol_cat){
      stop("fn_modelMatrix: number of columns from mars is not as expected")
      }
  }
  
  # design matrix
  X_ = cbind(mars_, cat_)
  
  return(X_)
}


#--------------------------------------------------------------------------------
#' Scoring / predictions
#' 
#' Create scores / predictions, given a glmnet model and the underlying
#' mars and categorical variables chosen by IV
#' 
#' @export
#' 
#' @param model_ref the model reference
#' @param model_prefix the model prefix
#' @param fname_data the name of the RData file storing dt_all 
#' @param folds_train training folds (for rebasing if used)
#' @param whereStatement the where statement used during training
#' @param rebase whether or not to rebase predictions based on 
#'        difference between mean actual and predictions on train
#'  
#' @return nothing is returned.  dt_preds is updated and saved
#' 
fn_score <- function(model_ref,
                     model_prefix,
                     fname_data,
                     folds_train,
                     whereStatement,
                     rebase = FALSE,
                     verbose = FALSE){
  
  if(verbose){print('fn_score: start')}
  # load the variable  definitions:
  load(file = file.path(dirRData, paste0(model_ref, '_varsUsed.RData')))

  # load the glm (m_)
  load(file = file.path(dirRData, paste0(model_ref, '_', 'glmnet.RData')))
  
  # create model matrix (X_all) over all data that needs to be scored  
  if(verbose){print('Creating model matrix...')}
  dt_score <- 
    mlslib::fn_extractModelData(fname_data, 
                                fname_vars, 
                                model_ref,
                                vars_notToUseAdditional,
                                folds = 'all', 
                                n_max = NULL, 
                                whereStatement)
  
  idx_train <- dt_score$fold %in% folds_train
  
  # Prepare model matrix
  X_all <- fn_modelMatrix(model_ref,
                          dt_ = dt_score,
                          bln_create_checkncols = FALSE,
                          bln_test_ncols = TRUE)
  
  # response variable
  y_all <- dt_score[, var_target, with = FALSE][[1]]
  
  # weight - log for offset
  logweight_all <- log(dt_score[, var_weight, with = FALSE][[1]])
  
  # clear non essential objects from memory
  rm(dt_score); gc()
  
  # predict.glmnet produces predicted counts, not frequencies? offset??
  if(verbose){print('fn_score: Creating predictions...')}
  pred_all <- predict(object = m_,
                      newx = X_all,
                      newoffset = logweight_all,
                      type = "response",
                      s = "lambda.min") %>% 
    as.numeric()
  
  # Check for bias in train intercepts and adjust if needed
  train_bias <-  sum(pred_all[idx_train]) / sum(y_all[idx_train])
  print(paste0('The mean prediction over the mean actual
               on the train data is: ', train_bias))
  if (rebase & train_bias != 1){
    pred_all <- pred_all * train_bias
    print(paste0('Predictions have been rebased by: ', train_bias))
  }
  
  # load dt_preds
  load(file = file.path(dirRData, 
                        paste0('04a_dt_preds_', model_prefix, '.RData'))
  )
  
  if (!(identical(nrow(dt_preds), length(pred_all)))){
    stop("Number of predictions is different to number of 
         examples in dt_preds")
  }
  dt_preds[, eval(model_ref) := pred_all]
  
  # need model prefix
  if(verbose){print('fn_score: Saving predictions')}
  save(dt_preds, 
       file = file.path(dirRData, 
                        paste0('04a_dt_preds_', model_prefix, '.RData'))
  )
  rm(dt_preds); gc()
  if(verbose){print('fn_score: end')}
  
}


#--------------------------------------------------------------------------------
#' Output results
#' 
#' Outputs main results to text file and run paramters to RData file
#' 
#' @export
#' 
#' @param model_ref
#' @param model_prefix
#'  
#' @return nothing is returned.  .csv and .RData are saved
#' 
fn_outputResults <- function(model_ref
                             ){
  
  load(file = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData')))
                  
  # load: m_, n_train, time_glmnet
  load(file = file.path(dirRData, paste0(model_ref, '_', 'glmnet.RData')))
  
  # load: dt_results, n_test
  load(file = file.path(dirRData, paste0(model_ref, '_', 'dt_results.RData')))
  
  # load the variable  definitions:
  # var_target, var_weight,
  # vars_indToUse, 
  # vars_indToUseNumeric, vars_indToUseNonNumeric, vars_missingInds,
  # filteredNames
  load(file = file.path(dirRData, paste0(model_ref, '_varsUsed.RData')))
  
  # load lst_checkncols$ncol_mars/ncol_cat
  load(file =  file.path(dirRData, paste0(model_ref, '_', 'lst_checkncols.RData')))
  
  dt_results[, design_test := 'design']
  dt_results[fold %in% folds_test, design_test := 'test']
  dt_results_summary <- dt_results[, lapply(.SD, mean), 
                                   by = 'design_test',
                                   .SDcols = c('m1a_deviance', 'm1a_gini', 'm1a_ngini')]
  
  preTrainedEarth <- ifelse(is.null(preTrainedEarth), 'no', preTrainedEarth)
  
  model_info_current <- data.frame(model_prefix,
                                   model_ref,
                                   model_desc = model_desc,
                                   current_time = current_time,
                                   folds_design = paste(folds_design, collapse = ' '),
                                   folds_test = paste(folds_test, collapse = ' '),
                                   n_design = n_design,
                                   n_test = n_test,
                                   n_numeric_features = length(vars_indToUseNumeric),
                                   n_categorical_features = length(vars_indToUseNonNumeric),
                                   preTrainedEarth = preTrainedEarth,
                                   nk = nk,
                                   thresh = thresh,
                                   penalty = penalty,
                                   degree = degree,
                                   IV_threshold = IV_threshold,
                                   n_cat = lst_checkncols$ncol_cat,
                                   n_mars = lst_checkncols$ncol_mars,
                                   n_manInt = n_manInt,
                                   alpha = alpha,
                                   constrained = constrained,
                                   dev_design   = dt_results_summary[1, 2, with = FALSE],
                                   gini_design  = dt_results_summary[1, 3, with = FALSE],
                                   ngini_design = dt_results_summary[1, 4, with = FALSE],
                                   dev_test     = dt_results_summary[2, 2, with = FALSE],
                                   gini_test    = dt_results_summary[2, 3, with = FALSE],
                                   ngini_test   = dt_results_summary[2, 4, with = FALSE],
                                   time_glmnet_mins = time_glmnet[[3]]/60,
                                   workers = workers) 
  
  # append results to results.csv
  write.table(model_info_current,
              file = file.path(dirROutput,'04__results.csv'),
              row.names = FALSE,
              col.names = FALSE,
              append = TRUE,
              sep = ",")
  
  # save run parameters
  # idea is that it should be possible to replicate all results based
  # on this file and the data it refers to
  save(# Model reference and description
    model_prefix,
    model_ref,
    model_desc,
    # specific variable definitions for this model
    var_target,
    var_weight,
    m_family,
    vars_notToUseAdditional,
    whereStatement,
    workers,
    folds_design,
    folds_test,
    # control variables: earth
    preTrainedEarth,
    nk,
    thresh,
    penalty,
    degree,
    # control variables: IV
    IV_threshold,
    # control variables: glm 
    alpha,
    # control variables: interactions
    n_manInt,
    # other variables
    current_time,
    n_max_earth,
    n_max_glm,
    seed,
    file = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData'))
  )
}