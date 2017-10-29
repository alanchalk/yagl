# fns_glam.R

# Purpose: Various functions for the main earth-glm runs


# Functions in this script:

# fn_downsample

# devtools::use_package("caret")


#--------------------------------------------------------------------------------
#' Downsample a data table returning selected columns only
#'
#' Downsample a data table returning selected columns only
#'
#' @export
#'
#' @param targetVar the target variable.  The target variable must be 0-1
#' @param varsToUse a vector of variables to use
#'    (excluding the missing indicators)
#' @param varsToAdd variables to add to varsToUse, typically the
#'    missingInds - the missing indicators
#' @param dt_ a (training) data table to downsample
#' @param method one of caret, imbalanced, reduce.  caret is used to downsample
#'    the majority class (currently assumed to be the negative-0 class)
#'    to the minority class.  Imbalanced downsamples but maintains a
#'    minimum number of the majority class. Reduce simply reduced both
#'    classes in the same proportion leaving n_max observations
#' @param n_max the maximum number of examples left by 'reduce', downsampling
#'
#' @return a downsampled dataset
#'
fn_downsample <- function(targetVar, varsToUse, varsToAdd,
                          dt_,
                          method,
                          n_max){

  if (!(method %in% c('caret', 'imbalanced', 'reduce'))){
    stop('fn_downsample: method must be one of caret, imbalanced, reduce')
  }

  varsToUse <- c(varsToUse, varsToAdd)

  switch(method,
         caret = {
           # If want same number as min class - use caret downSample
           # indicator target variable for downsampling
           train_ind <- ifelse(dt_[,targetVar, with = FALSE] > 0, 1, 0) %>%
             as.factor()

           # Downsample the data
           dt_ds <- as.data.table(
             caret::downSample(x = dt_[, varsToUse, with = FALSE],
                               y = train_ind)
           )

         },

         imbalanced = {
           # If want to maintain a minimum number of the larger class
           set.seed(2018)
           train_ind = dt_[[targetVar]] > 0
           minNegativeClass = min(50 * sum(train_ind), min(nrow(dt_), 100000))
           down_train_pve = dt_[train_ind, c(targetVar, varsToUse), with = FALSE]
           train_ind_nve = which(train_ind == 0)
           train_ind_nve_ds = sample(train_ind_nve,
                                     size = minNegativeClass,
                                     replace = FALSE)
           down_train_nve = dt_[train_ind_nve_ds, c(targetVar, varsToUse), with = FALSE]
           dt_ds = rbind(down_train_pve, down_train_nve)
           rm(train_ind, minNegativeClass, down_train_pve,
              train_ind_nve, train_ind_nve_ds, down_train_nve)
           # to match with Max Kuhn code used above
           setnames(dt_ds, targetVar, "Class")
         },

         reduce = {
           # Simply downsample because dataset is big
           set.seed(2018)
           dt_ds = dt_[sample(.N, n_max), c(varsToUse, targetVar), with = FALSE]
           # to match with Max Kuhn code used above
           setnames(dt_ds, targetVar, "Class")

         })

  return(dt_ds)

}
