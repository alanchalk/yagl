# fns_glam.R

# Purpose: Various functions for the main earth-glm runs


# Functions in this script:

# fn_extractModelData
# fn_downsample
# fn_earthReduceSize
# fn_earth
# fn_IVscreening
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
#'    variables are stored; i.e. vars_uniqueID, vars_weightAll, vars_fold,
#'    vars_neverToUse, vars_indNotToUse, vars_targetAll, vars_missingInds
#' 
#' @return nothing.  A datatable is saved to
#'         '04a_dt_preds_', model_prefix, '.RData')
#'         containing a small number of key variables  
#'         and limited by the where statement (if there is one)
#' 
fn_create_dt_preds <- function(model_prefix,
                               fname_dt_all,
                               verbose = FALSE){

  if(verbose) {print('fn_create_dt_preds: start')}
  
  if(!(file.exists(file.path(dirRData, 
                             paste0('04a_dt_preds_', model_prefix, '.RData'))))
  ){
    if (verbose){print("Creating dt_preds")}
    if(!exists('dt_all')){
      load(file = file.path(dirRData, paste0(fname_dt_all, '.RData')))
    }
    load(file = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData')))
    load(file = file.path(dirRData, paste0(model_ref, '_varsUsed.RData')))
      
    # Do where statement first so that where can depend on variables
    # in dt_all which will do be used in model (and therefore dropped)
    
    # NB: dt_preds excludes ONLY where_mprefix so that sub models, even
    # if trained over less data, will predict over all data.  In 
    # particular, severity models trained over nucl > 0 will be used to
    # predict over all data
    idx_allpreds <- NULL
    if(!is.null(where_mprefix)){
      idx_allpreds <- which(dt_all[, eval(parse(text = where_mprefix))])
      dt_preds <- dt_all[idx_allpreds]
    }

    # Now only keep variables we will use
    # (might be quicker to drop vars we will not use - in place)
    dt_preds <- dt_preds[, unique(c(vars_uniqueID,
                                  vars_fold,
                                  vars_targetAll,
                                  vars_weightAll)),
                       with = FALSE]
    
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
#' @param model_ref reference for model
#' @param folds (if NULL passed then folds_design is used) 
#' @param bln_n_max (default is TRUE) limit number of examples to
#'              max(n_max_earth, n_max_glm)
#' @param verbose (default FALSE) print progress to console
#' 
#' @return dt_
#' 
fn_extractModelData <- 
  function(fname_data, 
           model_ref,
           folds = NULL, 
           bln_n_max = TRUE,
           bln_verbose = FALSE){
    
    if(bln_verbose) {print('fn_extractModelData: Start')}
    
    # load dt_all
    if(bln_verbose) {print('Loading data')}
    if(!exists('dt_all')){
      load(file = file.path(dirRData, paste0(fname_data, '.RData')))
    }
    
    # load variable definitions
    load(file = file.path(dirRData, paste0(fname_vars ,'.RData')))
    
    # load run parameters
    load(file = file.path(dirRData, paste0(model_ref, '_run_params.RData')))
    
    cnames_dt_all <- colnames(dt_all)
    vars_indNotToUse <- c(vars_indNotToUse, vars_notToUseAdditional)
    
    if (is.null(folds)){
      folds <- folds_design 
      }
    
    # independent variables to use
    # Can be set directly in run parameters or alternatively
    # defined as all columns excluding those specifically excluded
    if (is.null(vars_indToUse)){
      vars_indToUse <- 
        setdiff(colnames(dt_all), c(vars_targetAll, 
                                    vars_weightAll, 
                                    vars_fold,
                                    vars_neverToUse,
                                    vars_indNotToUse))
    }
    
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
      file_in = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData')))
    
    if (identical(folds, 'all')){
      idx_ <- 1:nrow(dt_all)
    } else {
      idx_ <- which(dt_all$fold %in% folds)
    }
    
    # n_max_toUse bites if it is less than the number of examples in folds.
    # Expected use is to limit size of training data when doing mars.
    if (bln_n_max){
      n_max_toUse <- max(n_max_earth, n_max_glm)
      if (length(idx_) > n_max_toUse){
        idx_ <- idx_train[sample(length(idx_), n_max_toUse)]
      }  
    }
    
    if(bln_verbose) {print('Extracting training data')}
    dt_ <- dt_all[idx_]
    
    rm(dt_all); gc()
    
    # where statement
    if(bln_verbose) {print('Applying where statements')}
    # add where statements for prefix and model ref together
    # tests
    # w1 = NULL; w2 = NULL; 
    # w1 = NULL; w2 = "ex > 0"; 
    # w1 = "ex > 0"; w2 = NULL; 
    # w1 = "ex > 0"; w2 = "ex > 0"; 
    w1 <- where_mprefix; w2 <- where_mref
    whereStatement <- NULL
    if(!is.null(w1)) {w1 <- paste0("(", w1, ")" )}
    if(!is.null(w2)) {w2 <- paste0("(", w2, ")" )}
    if (!(is.null(w1) & is.null(w2))){
      whereStatement <- paste0(c(w1, w2), collapse = " & ")}
    if(!is.null(whereStatement)){
      dt_ <- dt_[eval(parse(text = whereStatement))]
    }
    
    if(bln_verbose) {print('Removing unused columns')}
    dt_[, (vars_toDelete) := NULL]
    
    if(bln_verbose) {print('fn_extractModelData: End')}
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
#' @return a downsampled dataset, var_target and 
#'    targetVarFac, a 0-1 numeric which is 0 
#'    when var_target == 0 and 1 where var_target > 0 
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
  
  x = copy(dt_[, c(var_target, vars_toUse), with = FALSE])
  
  if(length(unique(dt_[[var_target]])) > 2){
    warning('The target variable is not binary.  All values > 0 set to 1')
  }
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
             dt_ds <- x[sort(idx_)]
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
             dt_ds <- x[sort(idx_)]
         },
         reduce = {
           # Simply downsample because dataset is big
           if (is.null(n_max)) stop(
               'fn_downsample: for reduce downsampling, n_max must be set')
             
           x[, targetVarFac := targetVarFac]
           if(n_max >= nrow(x)){
               dt_ds <- x
           } else {
               dt_ds <- x[sort(sample(.N, min(nrow(x), n_max)))]
           }
         }
         )
  
  dt_ds[, targetVarFac := as.numeric(levels(targetVarFac)[targetVarFac])]
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
#' Numeric variables preparation with earth

#' Numeric variables preparation with earth.  An earth object is created
#' and stored in earth_all.  Contains hinges or linear for variables
#' passed in a slimmed down earth object
#' Currently requires requires dt_train to be loaded (we pass it)
#' 
#' @export
#' 
#' @param dt_train training dataset
#' @param model_ref reference for model
#' 
#' @return nothing is returned.  earth_all is saved for this model_ref 
#' 
fn_earth <- function(dt_train,
                     model_ref){
  
  # load run parameters
  load(file = file.path(dirRData, paste0(model_ref, '_run_params.RData')))

  # load variable definitions
  load(file = file.path(dirRData, paste0(fname_vars ,'.RData')))
  
  # downsampling function should do downsampling only
  # not removing unwanted columns - should change the function
  # and remove the columns first here
  
  if (is.null(preTrainedEarth)){
    # downsample numeric data
    if (bln_ds){
      dt_train_ds_num <- 
        mlslib::fn_downsample(var_target = var_target,
                              vars_toUse = vars_indToUseNumeric, 
                              vars_toAdd = vars_missingInds,
                              dt_train,
                              method = "imbalanced",
                              n_max = n_max_earth, 
                              upsampleMult = upsampleMult)
      
      # downsample returns a 0-1 version of the target variable
      # called targetVarFac.  This is needed for IV but not 
      # needed here 
      dt_train_ds_cat[, "targetVarFac" := NULL]
      
    } else {
      vars_toUse <- c(var_target, vars_indToUseNumeric, vars_missingInds)
      dt_train_ds_num <- copy(dt_train[, vars_toUse, with = FALSE])
      rm(vars_toUse)
    }
    
    # initialise list to hold earth models
    earth_list = vector("list", degree + 1)
    
    # include factors as linear predictors first (linpreds = TRUE)
    earth_list[[1]] <- 
      earth(
        x = dt_train_ds_num[, -var_target, with = FALSE],
        y = dt_train_ds_num[[var_target]],
        pmethod = "none",
        thresh = thresh,
        nk = nk,
        penalty = -1,
        degree = 1,
        trace = 2,
        fast.k = 0,
        linpreds = TRUE)
    
    earth_list[[1]] <- mlslib::fn_earthReduceSize(earth_list[[1]])
    
    # fit earth models with hinges
    for (deg in seq(1:degree)){ # deg = 1
      earth_list[[deg + 1]] <- 
        earth(
          x = dt_train_ds_num[, -var_target, with = FALSE],
          y = dt_train_ds_num[[var_target]],
          pmethod = "none",
          thresh = thresh,
          nk = nk,
          penalty = penalty,
          degree = deg,
          trace = 2,
          fast.k = 0)
      
      earth_list[[deg]] <- fn_earthReduceSize(earth_list[[deg]])
    }
    
    rm(dt_train_ds_num)
  }
  
  e_cuts <- do.call(rbind, lapply(earth_list, function(x) x$cuts))
  e_dirs <- do.call(rbind, lapply(earth_list, function(x) x$dirs))
  
  # note - I got an error (but I could non replicate it) when running
  # with only one variable - since output was not a matrix
  
  # Now remove duplicate splines - duplicate cut point AND direction
  e_cutsdirs <- cbind(e_cuts, e_dirs)
  idx_nonDuplicate <- !duplicated(e_cutsdirs)
  e_cuts <- e_cuts[idx_nonDuplicate, ]
  e_dirs <- e_dirs[idx_nonDuplicate, ]
  rm(e_cutsdirs)
  e_selected.terms <- 1:nrow(e_dirs)
  e_namesx <- earth_list[[1]]$namesx
  
  earth_all <- list()
  earth_all$cuts <- e_cuts
  earth_all$dirs <- e_dirs
  earth_all$namesx <- e_namesx
  earth_all$selected.terms <- e_selected.terms
  earth_all$pmethod <- "none"
  class(earth_all) <- "earth"
  
  # save earth
  save(earth_all,
       file = file.path(dirRData,
                        paste0(model_ref, '_', 'earth_all.RData')))
}


#--------------------------------------------------------------------------------
#' Categorical variables IV screening

#' Categorical variables IV screening
#' IV package works for binary classification tasks only
#' Functions needs to allow for two cases
#' 1. there are no categorical variables at all in the data
#' 2. there are some but none pass the threshold test
#' Also note, eg gender - might be same frequency male-female overall
#' but could still be very important in an interaction - hence could fail
#' here and then not be available later.  Possible is to use gbm or rf
#' and to include all variables of a given importance - this will allow
#' inclusion of those important due to interactions as opposed to other
#' forms of variable screening
#' 
#' @export
#' 
#' @param model_ref reference for model
#' @param IV_threshold on intial run this takes a default value.
#'        IV should be rerun after inspecting graph output
#'  
#' @return nothing.  filteredNames is added to _varsUsed.RData
#' 
fn_IVscreening <- function(model_ref,
                           IV_threshold = 0.0005
                           ){
  
  # load variable definitions
  load(file = file.path(dirRData, paste0(fname_vars ,'.RData')))
  
  #load run parameters
  load(file = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData')))
  
  # 3a. Categorical variables: Information Value
  # Note. missing inds are treated with numerics
  dt_train_ds_cat <- 
    mlslib::fn_downsample(var_target = var_target,
                          vars_toUse = vars_indToUseNonNumeric, 
                          vars_toAdd = NULL,
                          dt_train,
                          method = "reduce",
                          n_max = n_max_IV)
  
  # remove var_target as IV needs 0-1 response and targetVarFac is 0-1
  dt_train_ds_cat[, (var_target) := NULL]
  
  # Information Value
  IV <- dt_train_ds_cat %>%
    Information::create_infotables(data = ., 
                                   y = "targetVarFac", 
                                   parallel = TRUE)
  
  rm(dt_train_ds_cat)
  
  # Plot to see fall off
  plot(x = 1:nrow(IV$Summary), y = log(IV$Summary$IV))
  
  # identify categorical variables that pass the threshold
  filteredNames <- IV$Summary[IV$Summary$IV > IV_threshold, ]$Variable
  
  # update the vars file with filteredNames
  mlslib::fn_resave(filteredNames,
                    file_in = file.path(dirRData, paste0(model_ref ,'_varsUsed.RData')))
}


#--------------------------------------------------------------------------------
#' Penalised regression
#' 
#' Penalised regression
#' Requires in memory: dt_train
#' 
#' @export
#' 
#' @param model_ref the model reference
#' @param dt_train the training dataset
#' @param bln_constrained whether or not to use a previously 
#'        created constraints file
#' @param bln_verbose whether or not to print progress
#' 
#' @return nothing.  The model (and n_design, time_glmnet) is saved
#'         (to model_ref, '_', 'glmnet.RData')
#' 
fn_penreg <- function(model_ref,
                      dt_train,
                      bln_constrained = FALSE,
                      bln_verbose = FALSE){
  
  if(bln_verbose){message('fn_penreg: Start')}
  
  # load variable definitions
  load(file = file.path(dirRData, paste0(fname_vars ,'.RData')))
  
  #load run parameters
  load(file = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData')))
  
  # Prepare model matrix
  if(bln_verbose){message('Preparing model matrix')}
  X_train <- mlslib::fn_modelMatrix(model_ref,
                                    dt_ = dt_train,
                                    bln_create_checkncols = TRUE,
                                    bln_test_ncols = FALSE)
  
  # This seems the best place to define n_design as X_train defines it
  n_design <- nrow(X_train)
  
  # response variable
  y_train = dt_train[, var_target, with = FALSE][[1]]
  
  # exposure - log for offset - not needed for binom - but if ex = 1 then just = 0 
  logweight_train = log(dt_train[, var_weight, with = FALSE][[1]])
  
  # clear non essential objects from memory - not sure if this
  # will work when called from this function's environment
  rm(dt_train); gc()
  
  # predictor names
  predictors <- colnames(X_train)
  
  # cross validation
  foldid <- sample(1:10, size = length(y_train), replace = TRUE)
  
  # parallel processing
  library(doSNOW)
  cl <- makeCluster(workers, type = "SOCK")
  registerDoSNOW(cl)
  getDoParWorkers()
  
  # glmnet: Note family is currently manual 
  # AC note: For the binomial task all is  1 and so far 
  #          no summarisation of data so all rows 1
  #          so no offset needed

  if(bln_verbose){message('Running model')}
  if (bln_constrained){
    
    dt_limits <- fread(file.path(dirRawData, 'limits.csv'))
    
    time_glmnet <-
      system.time(
        m_ <- 
          cv.glmnet(x = X_train,
                    y = y_train,
                    family = m_family,
                    lower.limits = dt_limits$lower,
                    upper.limits = dt_limits$upper,
                    penalty.factor = dt_limits$penalty,
                    offset = logweight_train,
                    standardize = TRUE,#standardise if variables are on a different scale
                    parallel = TRUE,
                    alpha = alpha,
                    foldid = foldid))
  } else {
    time_glmnet <- 
      system.time(
        m_ <- 
          cv.glmnet(x = X_train,
                    y = y_train,
                    family = m_family,
                    offset = logweight_train,
                    standardize = TRUE,
                    parallel = TRUE,
                    alpha = alpha,
                    foldid = foldid))
  }
  
  stopCluster(cl)
  registerDoSEQ()
  
  plot(m_) 
  
  # save glmnet and related variables
  if(bln_verbose){message('Saving model')}
  save(m_, n_design, time_glmnet,
       file = file.path(dirRData, paste0(model_ref, '_', 'glmnet.RData')))
  
  if(bln_verbose){message('fn_penreg: End')}
  
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
  load(file = file.path(dirRData, paste0(model_ref, '_varsUsed.RData')))
  
  # load the earth object: earth_all
  if(!is.null(preTrainedEarth)){
    load(file = file.path(dirRData,
                          paste0(preTrainedEarth, '.RData')))
  } else {
    load(file = file.path(dirRData,
                          paste0(model_ref, '_', 'earth_all.RData')))
  }
  
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
#' mars and categorical variables chosen by IV.  Currently works
#' for glmnet only
#' 
#' @export
#' 
#' @param model_ref the model reference
#' @param model_prefix the model prefix
#' @param fname_dt_all the name of the RData file storing dt_all 
#' @param s lambda.min or lambda.1se (currently works for glmnet only)
#' @param rebase whether or not to rebase predictions based on 
#'        difference between mean actual and predictions on train
#' @param verbose print progress to console
#'   
#' @return nothing is returned.  dt_preds is updated and saved
#' 
fn_score <- function(model_ref,
                     model_prefix,
                     fname_dt_all,
                     s = "lambda.min",
                     rebase = FALSE,
                     verbose = FALSE){
  
  if(verbose){print('fn_score: start')}
  
  # load the variable  definitions:
  load(file = file.path(dirRData, paste0(model_ref, '_varsUsed.RData')))
  # load run parameters
  load(file = file.path(dirRData, paste0(model_ref, '_', 'run_params.RData')))
  # load the glm (m_)
  load(file = file.path(dirRData, paste0(model_ref, '_', 'glmnet.RData')))
  
  # create model matrix (X_all) over all data that needs to be scored  
  if(verbose){print('Creating model matrix...')}
  dt_score <- 
    mlslib::fn_extractModelData(fname_dt_all, 
                                model_ref,
                                folds = 'all', 
                                n_max = FALSE 
                                )
  
  idx_train <- dt_score$fold %in% folds_design
  
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
                      s = s) %>% 
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
  # resave to avoid deleting other objects (idx_allpreds)
  fn_resave(dt_preds, 
            file_in = file.path(dirRData, 
                             paste0('04a_dt_preds_', model_prefix, '.RData'))
  )
  rm(dt_preds); gc()
  if(verbose){print('fn_score: end')}
  
}


#--------------------------------------------------------------------------------
#' Output runtime statistics and performance
#' 
#' Output runtime statistics and performance
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
  
  # resave run parameters if needed
  # currently not needed as they are all saved perviously in pipeline
}
