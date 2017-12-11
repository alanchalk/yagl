# fns_glam.R

# Purpose: Various functions for the main earth-glm runs


# Functions in this script:

# fn_prepareTrainingData
# fn_downsample
# fn_earthReduceSize

# devtools::use_package("caret")


#--------------------------------------------------------------------------------
#' Prepare training data
#' 
#' Prepares the training data, sampling the rows and limiting columns
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
#' @param train_folds training folds - typically 1:7
#' @param n_train_max maximum number of training examples.  Bites if 
#'    n_train_max is less than the number of examples in train_folds
#' @param whereStatement_train a statement that returns TRUE / FALSE for 
#'    each example, using any columns in dt_all.  For example:
#'    lossLimit >= 225000
#' 
#' @return dt_train
#' 
fn_prepareTrainingData <- 
    function(fname_data, fname_vars, model_ref,
             vars_notToUseAdditional = NULL,
             train_folds, n_train_max, whereStatement_train){

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
                                    weightVar, 
                                    vars_fold,
                                    vars_neverToUse,
                                    vars_indNotToUse))
    
    vars_toUseAll <- c(vars_indToUse, 
                       targetVar,
                       weightVar,
                       vars_fold)
    
    vars_toDelete <- setdiff(colnames(dt_all), vars_toUseAll)
    
    # identify numeric variables
    vars_indToUseNumeric <-
        vars_indToUse[sapply(dt_all[1, vars_indToUse, with = FALSE], is.numeric)]
    
    # identify non numeric variables 
    vars_indToUseNonNumeric <- 
        setdiff(vars_indToUse, c(vars_indToUseNumeric, vars_missingInds))
    
    varsToDelete <- setdiff(cnames_dt_all, vars_toUseAll)
    
    # save variable choices for future reference
    save(vars_indToUse, 
         vars_indToUseNumeric, vars_indToUseNonNumeric, vars_missingInds,
         targetVar, weightVar,
         file = file.path(dirRData, paste0(model_ref ,'_varsUsed.RData')))
    
    idx_train  <- which(dt_all$fold %in% train_folds)
    
    if (!is.null(n_train_max)){
        if (length(idx_train) > n_train_max){
            idx_train <- idx_train[sample(length(idx_train), n_train_max)]
        }  
    }
    
    dt_train = dt_all[idx_train]
    rm(dt_all); gc()
    
    n_train = nrow(dt_train)
    
    "R code for slice of data to include (ie where statement)"
    if(!is.null(whereStatement_train)){
        dt_train <- dt_train[eval(parse(text = whereStatement_train))]
    }
    
    dt_train[, (vars_toDelete) := NULL]
    
    return(dt_train)
    
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
#' @param targetVar the target variable.  The target variable must be 0-1
#' @param varsToUse a vector of variables to use
#'    (excluding the missing indicators)
#' @param varsToAdd variables to add to varsToUse, typically the
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
#' @param upsampleMult
#' @param seed seed set random before sample is used
#' @return a downsampled dataset with targetVarFac, a 0-1 factor which is 0 
#'    when targetVar == 0 and 1 where targetVar > 0 
#'
fn_downsample <- function(targetVar, varsToUse, varsToAdd,
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
  varsToUse <- c(varsToUse, varsToAdd)
  
  x = copy(dt_[, varsToUse, with = FALSE])
  
  targetVarFac <- 
      ifelse(dt_[,targetVar, with = FALSE] > 0, 1, 0) %>%
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