#fns_dataManipulation.R

# Purpose: Various functions for data manupilation / pre-processing


# Functions in this script:

# fn_colNamesToDataDict
# fn_zeroVariance
# fn_removeRedundantCols
# fn_explainsTooMuch
# fn_pctMissing
# fn_checkForMAR

# heavy use of data.table, also add to namespace since in any case we need to do this
# for in place operations
# devtools::use_package("data.table")


#--------------------------------------------------------------------------------
#' Change column names to data dictionary names
#' @export
#'
fn_colNamesToDataDict <- function(dt_, tbl_dataDict){
  colNames_old <- colnames(dt_)
  idx_rename <- match(colNames_old, tbl_dataDict$name)
  idx_missing <- which(is.na(idx_rename))

  colNames_new <- tbl_dataDict$nameInR[idx_rename]
  colNames_new[idx_missing] <- colNames_old[idx_missing]
  print(c('The following column names are not in the data dictionary and will be left as is', colNames_old[idx_missing]))
  data.table::setnames(dt_, colNames_old, colNames_new)

  # A data table setnames is carried out in place so no need to return dt_
}


#--------------------------------------------------------------------------------

#' Remove zero variance columns
#' @export
#'
fn_zeroVariance <-

  function(dt_){

    # ensure we are working with a data.table
    # Do not change the class - let user see the error and change if desired
    if (!data.table::is.data.table(dt_)){
      stop("fn_zeroVariance function requires data.table input")
    }

    # identify zeroVariance predictior names
    zeroVariancePositions = sapply(dt_, function(x) length(unique(x))==1)

    if (sum(zeroVariancePositions) > 0){

      zeroVarianceNames = names(zeroVariancePositions [zeroVariancePositions == TRUE])

      print ("The following variables have zero variance and have been removed")
      print (zeroVarianceNames)

      dt_[,(zeroVarianceNames):=NULL]

    }
    else {
      print ("There were no columns with zero Variance")
    }
  }


#--------------------------------------------------------------------------------

#' Remove duplicate columns
#' @export
#'
fn_removeRedundantCols <-

  function(dt_){

    # ensure we are working with a data.table
    # Do not change the class - let user see the error and change if desired
    if (!data.table::is.data.table(dt_)){
      stop("fn_zeroVariance function requires data.table input")
    }

    # remove redundant columns
    # http://stackoverflow.com/questions/38222318/how-to-remove-duplicate-columns-content-in-data-table-r#comment63867686_38222379
    colNamesOriginal = colnames(dt_)
    colNamesRedundant = colNamesOriginal[duplicated(as.list(dt_))]

    if (length(colNamesRedundant)>0) {
      dt_ = dt_[, (colNamesRedundant):=NULL]
      warning ("the above columns have been removed since they are redundant")
      print(colNamesRedundant)
    }
    else {
      warning ("no columns were found to be redundant")
    }

  }


#--------------------------------------------------------------------------------

#' Check which features explain too much
#'
#' Returns a dataframe of variables which explain too much.
#'
#' For a binomial response this function goes through each feature and checks if
#' any values of the feature perfectly classifies 25 or more examples.  In practice
#' we have found that when this is the case, we are some times dealing with feature
#' which is actually a proxy for the dependent variable or should not be used in the
#' analysis for some other reason (such as operational).  Such variables can be an
#' example of what is called ``leakage''.
#'
#' @export
#' 
#' @param dt_ A data table.
#' @param varsToCheck A vector of features for this function to check.
#' @param targetVar The target variable (currently binomial 0-1 response only).
#' @param problemValues The means values of the response for which perfect classification
#'   are deemed problematic.
#'   For binomial response this may be c(0, 1).  But if the data is highly imbalanced
#'   it is possible that only seeing a perfect response of the minority class 
#'   is an issue.
#' @param thresh_n there must be >= this number of cases
#' @param thesh_p thresh_p % or more of cases must take the problem value
#'
#' @return A data table with two columns; varName, the feature names and n_problemObs,
#'   the number of obervations which perfectly pedict the target variable.
#'
#' @import data.table
#'
fn_explainsTooMuch <- function(dt_, 
                               varsToCheck = NULL,
                               targetVar = NULL,
                               problemValues = NULL,
                               thresh_n = 25, 
                               thresh_p = 1){
  # colName = "qtLag"

  if (is.null(varsToCheck)) stop("varsToCheck must be passed.")
  if (is.null(targetVar)) stop("A target variable must be passed.")
  if (is.null(problemValues)) stop("problemValues must be passed.")
  
  dt_explainsToMuch <- data.table::data.table(varName = character(),
                                  n_problemObs = numeric()
                                  )
  dt_problemCases <- data.table::data.table(varName = character(),
                                featureValue = character(),
                                n_obs = numeric()
                                )

  for (idx_name in varsToCheck){
    # idx_name = varsToCheck[1]
    dt_temp <- data.table::data.table(varName = idx_name,
                          target = dt_[[targetVar]],
                          featureValue = dt_[[idx_name]]
                          )

    dt_temp <- dt_temp[, list(mean = mean(target),
                              n_obs = length(target)),
                       by = list(varName, featureValue)]
    dt_temp <- dt_temp[n_obs > 25]
    n_problemObs <- sum(dt_temp[mean %in% problemValues, n_obs])

    if (n_problemObs > 0){
      dt_temp <- dt_temp[mean %in% problemValues]
      # coercion of factors is handled internally to rbind
      if (class(dt_temp$featureValue) != 'factor'){
        dt_temp[, featureValue := as.character(featureValue)]
      }
      dt_problemCases <-
        rbind(dt_problemCases,
              dt_temp[, list(varName, featureValue, n_obs)])
      dt_explainsToMuch <-
        rbind(dt_explainsToMuch,
              data.table::data.table(varName = idx_name, n_problemObs = n_problemObs)
        )

    }
  }
  data.table::setorder(dt_explainsToMuch, - n_problemObs)

  return(list(dt_explainsToMuch = dt_explainsToMuch,
              dt_problemCases = dt_problemCases))
}


#--------------------------------------------------------------------------------

#' Check percent missing
#'
#' Check percent missing.  The percent missing is round to 0.01%.
#'
#' @export
#'
#' @param dt_ a data table with columns to be checked for missings
#'
#' @return a data table with the following rows: varName, class,
#'     nMissing and pctMissing.  Only outputs rows for variables with
#'     any missing and outputs in order of descending number missing.
#'
fn_percentMissing <- function(dt_){

  vec_nMissing <- sapply(dt_, function(x) sum(is.na(x)))
  vec_class <- unname(sapply(dt_, class))
  dt_pctMissing <- data.table(varName = names(vec_nMissing),
                              class = vec_class,
                              nMissing = unname(vec_nMissing))
  nObs <- nrow(dt_)
  dt_pctMissing[, pctMissing := round(100 * nMissing / nObs, 2)]
  setorder(dt_pctMissing, -pctMissing)
  dt_pctMissing <- dt_pctMissing[nMissing > 0, ]

  return(dt_pctMissing)

}


#--------------------------------------------------------------------------------

#' Check for missing at random
#'
#' @export
#'
fn_checkForMAR <- function(dt_, dt_pctMissing){

  # colName = "busTms"
  dt_checkForMAR <- data.table(varName = character(),
                               p_value = numeric(),
                               mean_missing = numeric(),
                               mean_nonmissing = numeric(),
                               CI = character()
  )

  for (colName in dt_pctMissing$varName){
    # colName = dt_pctMissing$varName[1]

    dt_temp <- data.table(response = dt_[[targetVar]],
                          feature = dt_[[colName]])
    dt_temp[!is.na(feature), varTestFac := 'NotMissing']
    dt_temp[is.na(feature),  varTestFac := 'Missing']
    ttest_result <- t.test(x = dt_temp$response[dt_temp$varTestFac =='NotMissing'],
                           y = dt_temp$response[dt_temp$varTestFac =='Missing'])

    dt_checkForMAR <-
      rbind(dt_checkForMAR,
            data.table(varName = colName,
                       p_value = ttest_result$p.value,
                       mean_missing = round(unname(ttest_result$estimate[2]),3),
                       mean_nonmissing = round(unname(ttest_result$estimate[1]), 3),
                       CI = paste("lower:", round(ttest_result$conf.int[1],3),
                                  "upper ", round(ttest_result$conf.int[2],3)                                   )
            )
      )
  }
  return(dt_checkForMAR)
}

#--------------------------------------------------------------------------------

#' Impute missing values
#'
#' @export
#'
#' @param dt_ a data.table over which to impute missing values for
#'    some given numeric or logical variables
#' @param meanValues a named vector of variables and mean non-missing
#'    values
#'
#' @return a data.table with missing values imputed - modified inplace
#'
fn_missingValuesImpute <-

  function(dt_, meanValues){

    # ensure we are working with a data.table
    if (!is.data.table(dt_)){
      stop("fn_missingValuesImpute: data.table input required")
    }

    # Impute Missing Values
    for (name in names(meanValues)){
      # name = colnames(impute_Values)[1]
      dt_[is.na(eval(parse(text=name))), (name) := meanValues[[name]]]
    }

  }

